;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

#+gencgc
(macrolet ((define-alloc-tramp (lisp-name c-name)
 `(define-assembly-routine (,lisp-name (:return-style :none))
    ((:temp nl0 unsigned-reg nl0-offset)
     (:temp nl1 unsigned-reg nl1-offset))
  (flet ((reg (offset sc)
           (make-random-tn
            :kind :normal
            :sc (sc-or-lose sc)
            :offset offset))
         (reverse-pairs (list)
           (let (result)
             (loop for (a b) on list by #'cddr
                   do
                   (push b result)
                   (push a result))
             result)))
    (let ((nl-registers (loop for i to 9
                              collect (reg i 'unsigned-reg)))
          (lisp-registers (loop for i from r0-offset to r9-offset
                                collect (reg i 'unsigned-reg)))
          (float-registers (loop for i below 32
                                 collect (reg i 'complex-double-reg))))
      (macrolet  ((map-pairs (op base start offsets
                              &key pre-index post-index
                                   (delta 16))
                    `(let ((offsets ,(if (eq op 'ldp)
                                         `(reverse-pairs ,offsets)
                                         offsets)))
                       (loop with offset = ,start
                             for first = t then nil
                             for (a b . next) on offsets by #'cddr
                             for last = (not next)
                             do (inst ,op a b
                                      (@ ,base (or (and first ,pre-index)
                                                   (and last ,post-index)
                                                   offset)
                                               (cond ((and last ,post-index)
                                                      :post-index)
                                                     ((and first ,pre-index)
                                                      :pre-index)
                                                     (t
                                                      :offset))))
                                (incf offset ,delta)))))
        (map-pairs stp nsp-tn 0 nl-registers :pre-index -80)
        (inst mov nl0 tmp-tn) ;; size
        #+sb-thread
        (progn
          (inst stp cfp-tn csp-tn (@ thread-tn (* thread-control-frame-pointer-slot n-word-bytes)))
          (inst str csp-tn (@ thread-tn (* thread-foreign-function-call-active-slot n-word-bytes))))
        #-sb-thread
        (progn
          ;; Each of these loads of a fixup loads the address of a linkage table entry,
          ;; that is, if LINKAGE-TABLE-SPACE-START is #xF0200000, then the first load puts
          ;; #xF0200000+something, not the address of current_control_frame_pointer,
          ;; into register nl1.
          ;; This is kinda dumb because first of all we could have calculated the address
          ;; of the linkage entry instead of loading it. Second, it's unnecessarily
          ;; double-indirect. I wonder if there's a way to remove double-indirection.
          ;; The ohly way I can see doing that is for os_link_runtime() to poke bytes
          ;; into this assembly code in the manner of the system's dynamic loader.
          (load-inline-constant nl1 '(:fixup "current_control_frame_pointer" :foreign-dataref))
          (inst ldr nl1 (@ nl1)) ; now load the address of the C variable
          (inst str cfp-tn (@ nl1))
          (load-inline-constant nl1 '(:fixup "current_control_stack_pointer" :foreign-dataref))
          (inst ldr nl1 (@ nl1)) ; "
          (inst str csp-tn (@ nl1))
          (load-inline-constant nl1 '(:fixup "foreign_function_call_active" :foreign-dataref))
          (inst ldr nl1 (@ nl1)) ; "
          ;; storing NULL ensures at least 1 set bit somewhere in the low 4 bytes
          (inst str (32-bit-reg null-tn) (@ nl1))) ; (alien variable is 4 bytes, not 8)
        ;; Create a new frame
        (inst add csp-tn csp-tn (+ 32 80))
        (inst stp cfp-tn null-tn (@ csp-tn -112))
        (inst stp code-tn lr-tn (@ csp-tn -96))

        (map-pairs stp csp-tn -80 lisp-registers)
        (map-pairs stp nsp-tn 0 float-registers :pre-index -512 :delta 32)

        (load-inline-constant nl1 '(:fixup ,c-name :foreign))
        (inst blr nl1)

        (map-pairs ldp nsp-tn 480 float-registers :post-index 512 :delta -32)
        (map-pairs ldp csp-tn -16 lisp-registers :delta -16)

        (inst ldr lr-tn (@ csp-tn -88))

        (inst sub csp-tn csp-tn (+ 32 80)) ;; deallocate the frame
        #+sb-thread
        (inst str zr-tn (@ thread-tn (* thread-foreign-function-call-active-slot n-word-bytes)))
        #-sb-thread
        (progn
          ;; We haven't restored NL1 yet, so it's ok to use as a scratch register here.
          ;; It gets restored just prior to the RET instruction.
          (load-inline-constant nl1 '(:fixup "foreign_function_call_active" :foreign-dataref))
          (inst ldr nl1 (@ nl1))
          (inst str (32-bit-reg zr-tn) (@ nl1)))

        (inst mov tmp-tn nl0) ;; result

        (map-pairs ldp nsp-tn 64 nl-registers :post-index 80 :delta -16)
        (inst ret)))))))
  (define-alloc-tramp alloc-tramp "alloc")
  (define-alloc-tramp list-alloc-tramp "alloc_list"))

(define-assembly-routine
    (xundefined-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-tramp
                               (undefined-tramp-tagged
                                (+ xundefined-tramp
                                   fun-pointer-lowtag))))
    ()
  HEADER
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'undefined-tramp-tagged
                          :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  UNDEFINED-TRAMP
  (inst adr code-tn header fun-pointer-lowtag)
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error)
                    (list lexenv-tn))
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))

  (inst br lr-tn))

(define-assembly-routine
    (xundefined-alien-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export undefined-alien-tramp
                               (undefined-alien-tramp-tagged
                                (+ xundefined-alien-tramp
                                   fun-pointer-lowtag))))
    ((:temp r8-tn unsigned-reg r8-offset))
  HEADER
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'undefined-alien-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  UNDEFINED-ALIEN-TRAMP
  (inst adr code-tn header fun-pointer-lowtag)
  (error-call nil 'undefined-alien-fun-error r8-tn))

(define-assembly-routine
    (xclosure-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export closure-tramp
                               (closure-tramp-tagged
                                (+ xclosure-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'closure-tramp-tagged
                         :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  CLOSURE-TRAMP
  (loadw lexenv-tn lexenv-tn fdefn-fun-slot other-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))
  (inst br lr-tn))

(define-assembly-routine
    (xfuncallable-instance-tramp (:return-style :none)
                      (:align n-lowtag-bits)
                      (:export (funcallable-instance-tramp
                                (+ xfuncallable-instance-tramp
                                   fun-pointer-lowtag))))
    ()
  (inst dword simple-fun-widetag)
  (inst dword (make-fixup 'funcallable-instance-tramp :assembly-routine))
  (dotimes (i (- simple-fun-insts-offset 2))
    (inst dword nil-value))

  (loadw lexenv-tn lexenv-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (loadw code-tn lexenv-tn closure-fun-slot fun-pointer-lowtag)
  (inst add lr-tn code-tn (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))
  (inst br lr-tn))
