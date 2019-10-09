;;;; a bunch of handy macros for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mr ,n-dst ,n-src))))

(macrolet
    ((def (op inst inst-indexed)
       `(defun ,op (object base &optional (offset 0) (lowtag 0))
          (let ((displacement (- (ash offset word-shift) lowtag)))
            (cond ((logtest displacement #b11) ; not directly encodable in ld/std
                   (inst li temp-reg-tn displacement)
                   (inst ,inst-indexed object base temp-reg-tn))
                  (t
                   (inst ,inst object base displacement)))))))
  (def loadw ld ldx)
  (def storew std stdx))

(defmacro load-symbol (reg symbol)
  `(inst addi ,reg null-tn (static-symbol-offset ,symbol)))

#+sb-thread
(progn
  (defun load-tls-index (reg symbol)
    (inst lwz reg symbol (- #+little-endian 4 other-pointer-lowtag)))
  (defun store-tls-index (reg symbol)
    (inst stw reg symbol (- #+little-endian 4 other-pointer-lowtag))))

(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
                                          "LOAD-SYMBOL-"
                                          (string slot))))
             (storer (intern (concatenate 'simple-string
                                          "STORE-SYMBOL-"
                                          (string slot))))
             (offset (intern (concatenate 'simple-string
                                          "SYMBOL-"
                                          (string slot)
                                          "-SLOT")
                             (find-package "SB-VM"))))
         `(progn
            (defmacro ,loader (reg symbol)
              `(progn
                 ;; Work around the usual lowtag subtraction problem.
                 (inst li temp-reg-tn
                       (+ (static-symbol-offset ',symbol)
                          (ash ,',offset word-shift)
                          (- other-pointer-lowtag)))
                 (inst ldx ,reg null-tn temp-reg-tn)))
            (defmacro ,storer (reg symbol)
              `(progn
                 (inst li temp-reg-tn
                       (+ (static-symbol-offset ',symbol)
                          (ash ,',offset word-shift)
                          (- other-pointer-lowtag)))
                 (inst stdx ,reg null-tn temp-reg-tn)))))))
  (frob value)
  (frob function)

  ;; FIXME: These are only good for static-symbols, so why not
  ;; statically-allocate the static-symbol TLS slot indices at
  ;; cross-compile time so we can just use a fixed offset within the
  ;; TLS block instead of mucking about with the extra memory access
  ;; (and temp register, for stores)?
  #+sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(progn
       (inst lwz ,reg null-tn
             (+ (static-symbol-offset ',symbol)
                (- #+little-endian 4 other-pointer-lowtag)))
       (inst ldx ,reg thread-base-tn ,reg)))
  #-sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(load-symbol-value ,reg ,symbol))

  #+sb-thread
  (defmacro store-tl-symbol-value (reg symbol temp)
    `(progn
       (inst lwz ,temp null-tn
             (+ (static-symbol-offset ',symbol)
                (- #+little-endian 4 other-pointer-lowtag)))
       (inst stdx ,reg thread-base-tn ,temp)))
  #-sb-thread
  (defmacro store-tl-symbol-value (reg symbol temp)
    (declare (ignore temp))
    `(store-symbol-value ,reg ,symbol)))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst lbz ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst lbz ,n-target ,n-source (+ ,n-offset (1- n-word-bytes)))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
    ;; something is deeply bogus.  look at this
    ;; (loadw ,lip ,function function-code-offset function-pointer-type)
    (inst addi ,lip ,function (- (* n-word-bytes simple-fun-insts-offset) fun-pointer-lowtag))
    (inst mtctr ,lip)
    (inst bctr)))

(defmacro lisp-return (return-pc lip &key (offset 0))
  "Return to RETURN-PC."
  `(progn
     (inst addi ,lip ,return-pc
           (+ (- other-pointer-lowtag) n-word-bytes (* ,offset 4)))
     (inst mtlr ,lip)
     (inst blr)))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (loadw reg cfp-tn offset))))))
(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (storew reg cfp-tn offset))))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

;;; This is the main mechanism for allocating memory in the lisp heap.
;;;
;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;; must be a multiple of the lisp object size).
;;;
;;; On other platforms (Non-PPC), if STACK-P is given, then allocation
;;; occurs on the control stack (for dynamic-extent).  In this case,
;;; you MUST also specify NODE, so that the appropriate compiler
;;; policy can be used, and TEMP-TN, which is needed for work-space.
;;; TEMP-TN MUST be a non-descriptor reg. FIXME: This is not yet
;;; implemented on PPC. We should implement this and replace the
;;; inline stack-based allocation that presently occurs in the
;;; VOPs. The stack-p argument is ignored on PPC.
;;;
;;; If generational GC is enabled, you MUST supply a value for TEMP-TN
;;; because a temp register is needed to do inline allocation.
;;; TEMP-TN, in this case, can be any register, since it holds a
;;; double-word aligned address (essentially a fixnum).
;;;
;;; Using trap instructions for not-very-exceptional situations, such as
;;; allocating, is clever but not very convenient when using gdb to debug.
;;; So at the expense of speed and code size, we can use a call/return.
;;; In such case, we never actually try to perform allocations inline.
;;;
(defun allocation (result-tn size lowtag &key stack-p node temp-tn flag-tn)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.  If the lowtag also has a 1 bit in the same position, we're all
  ;; set.  Otherwise, we need to zap out the lowtag from alloc-tn, and
  ;; then or in the lowtag.
  ;; Normal allocation to the heap.
  (declare (ignore stack-p node))

  ;; if sigtrap is making you suffer, enable out-of-line allocator for everything
  ;;  #-alloc-use-sigtrap
  #+nil
  (let ((lip (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                             :offset lip-offset)))
    (inst addi lip null-tn (make-fixup 'alloc-tramp :asm-routine-nil-offset))
    (if (numberp size)
        (inst lr temp-tn size)
        (move temp-tn size))
    (inst std temp-tn lip -8)
    ;; the asm routine is not allowed to mess up the link register - it has to
    ;; be truly "invisible", like the trap signal. Obviously it's impossible
    ;; to avoid messing up LR, which means we need to save it now because we don't
    ;; know whether we're inside a context that needs it saved.
    (inst mflr temp-tn)
    (inst std temp-tn lip -16)
    (inst mtctr lip)
    (inst bctrl)
    ;; LIP once again points to the static spill area from which we can
    ;; recover the LR
    (inst ld temp-tn lip -16)
    (inst mtlr temp-tn)
    ;; asm routine returns its result in the count register.
    (inst mfctr result-tn)
    (inst ori result-tn result-tn lowtag)
    (return-from allocation))

  (let ((imm-size (typep size '(unsigned-byte 15)))
        (region (+ #+sb-thread (* thread-alloc-region-slot n-word-bytes))))

    (unless imm-size ; Make temp-tn be the size
      (if (numberp size)
          (inst lr temp-tn size)
          (move temp-tn size)))

    ;; thread-base-tn will point directly to the C variable if no thread
    #-sb-thread
    (progn (inst lr thread-base-tn (make-fixup "gc_alloc_region" :foreign-dataref))
           (inst ld thread-base-tn thread-base-tn 0)) ; because sb-dynamic-core

    (inst ld result-tn thread-base-tn region)
    (inst ld flag-tn thread-base-tn (+ region n-word-bytes)) ; region->end_addr

    (without-scheduling ()
         ;; CAUTION: The C code depends on the exact order of
         ;; instructions here.  In particular, immediately before the
         ;; TW instruction must be an ADD or ADDI instruction, so it
         ;; can figure out the size of the desired allocation and
         ;; storing the new base pointer back to the allocation region
         ;; must take two instructions (one on threaded targets).

         ;; Now make result-tn point at the end of the object, to
         ;; figure out if we overflowed the current region.
         (if imm-size
             (inst addi result-tn result-tn size)
             (inst add result-tn result-tn temp-tn))

         ;; result-tn points to the new end of the region.  Did we go past
         ;; the actual end of the region?  If so, we need a full alloc.
         ;; The C code depends on this exact form of instruction.  If
         ;; either changes, you have to change the other appropriately!
         (inst tw :lgt result-tn flag-tn)

         ;; The C code depends on this instruction sequence taking up
         ;; one machine instruction.
         (inst std result-tn thread-base-tn region))

       ;; Should the allocation trap above have fired, the runtime
       ;; arranges for execution to resume here, just after where we
       ;; would have updated the free pointer in the alloc region.

       ;; At this point, result-tn points at the end of the object.
       ;; Adjust to point to the beginning.
    (cond (imm-size
           (inst addi result-tn result-tn (+ (- size) lowtag)))
          (t
           (inst sub result-tn result-tn temp-tn)
           ;; Set the lowtag appropriately
           (inst ori result-tn result-tn lowtag)))))

(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size
                                            &key (lowtag other-pointer-lowtag)
                                                 stack-allocate-p)
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag))
    `(pseudo-atomic (,flag-tn)
       (if ,stack-allocate-p
           (progn
             (align-csp ,temp-tn)
             (inst ori ,result-tn csp-tn ,lowtag)
             (inst addi csp-tn csp-tn (pad-data-block ,size)))
         (allocation ,result-tn (pad-data-block ,size) ,lowtag
                     :temp-tn ,temp-tn
                     :flag-tn ,flag-tn))
       (when ,type-code
         (inst lr ,temp-tn (compute-object-header ,size ,type-code))
         (storew ,temp-tn ,result-tn 0 ,lowtag))
       ,@body)))

(defun align-csp (temp)
  ;; is used for stack allocation of dynamic-extent objects
  (let ((aligned (gen-label)))
    (inst andi. temp csp-tn lowtag-mask)
    (inst beq aligned)
    (inst addi csp-tn csp-tn n-word-bytes)
    (inst li temp 0)
    (storew temp csp-tn -1)
    (emit-label aligned)))


;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (emit-internal-error kind code values
                         :trap-emitter (lambda (tramp-number)
                                         (inst unimp tramp-number)))
    (emit-alignment word-shift)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC

;;; handy macro for making sequences look atomic
;;;
;;; FLAG-TN must be wired to NL3. If a deferred interrupt happens
;;; while we have the low bits of ALLOC-TN set, we add a "large"
;;; constant to FLAG-TN. On exit, we add FLAG-TN to ALLOC-TN which (a)
;;; aligns ALLOC-TN again and (b) makes ALLOC-TN go negative. We then
;;; trap if ALLOC-TN's negative (handling the deferred interrupt) and
;;; using FLAG-TN - minus the large constant - to correct ALLOC-TN.
(defmacro pseudo-atomic ((flag-tn) &body forms)
  #+sb-safepoint-strictly
  `(progn ,flag-tn ,@forms (emit-safepoint))
  #-sb-safepoint-strictly
  `(progn
     (without-scheduling ()
       ;; Extra debugging stuff:
       #+debug
       (progn
         (inst andi. ,flag-tn alloc-tn lowtag-mask)
         (inst twi :ne ,flag-tn 0))
       (inst ori alloc-tn alloc-tn pseudo-atomic-flag))
     ,@forms
     (inst sync)
     (without-scheduling ()
       (inst subi alloc-tn alloc-tn pseudo-atomic-flag)
       ;; Now test to see if the pseudo-atomic interrupted bit is set.
       (inst andi. ,flag-tn alloc-tn pseudo-atomic-interrupted-flag)
       (inst twi :ne ,flag-tn 0))
     #+debug
     (progn
       (inst andi. ,flag-tn alloc-tn lowtag-mask)
       (inst twi :ne ,flag-tn 0))
     #+sb-safepoint
     (emit-safepoint)))

#+sb-safepoint
(defun emit-safepoint ()
  (inst lwz temp-reg-tn null-tn
        (- (+ gc-safepoint-trap-offset n-word-bytes other-pointer-lowtag))))
