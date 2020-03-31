;;; Allocation profiler

;;; Quick start: To enable this code, you must DECLAIM or DECLARE
;;;  (OPTIMIZE SB-C::INSTRUMENT-CONSING)
;;; to affect functions in which instrumentation should be inserted.
;;; After that, the simplest way to run the function under test is to pass
;;; it to APROF-RUN. Alternatively, collection can be toggled with APROF-START
;;; and APROF-STOP, but some setup is needed (as done by APROF-RUN).
;;;
;;; Note: This is x86-64 only at the moment.

;;; Profiling works via compile-time instrumentation.
;;; Each point in code emitted by ALLOCATION adds a few instructions
;;; prior to the actual inline allocation sequence. The extra instructions
;;; are normally disabled (there is a jump around them).  When the profiler
;;; is switched on, the instructions are modified to atomically increment
;;; a counter (or two). If the allocation is a compile-time fixed size,
;;; only one counter is needed. If the size is variable, then two counters
;;; are used: hits and total bytes.
;;; The array of counters is prepared by the profiler when first started.
;;; The index into the array for any particular counter is determined
;;; the first time that counter is hit.  Subsequent hits are cheap.

;;; On x86-64, instrumented code initially looks like this:

;;;  9E98:       4D8B5D18         MOV R11, [R13+24]      ; thread.profile-data
;;;  9E9C:       4D85DB           TEST R11, R11
;;;  9E9F:       EB17             JMP L21
;;;  9EA1:       0F1F8000000000   NOP
;;;  9EA8:       E8D366FDFF       CALL #x21B00580        ; ENABLE-SIZED-ALLOC-COUNTER
;;;  9EAD:       0F1F00           NOP
;;;  9EB0:       4885C9           TEST RCX, RCX
;;;  9EB3:       0F1F440000       NOP
;;;  9EB8: L21:  4D8B5D20         MOV R11, [R13+32]      ; thread.alloc-region
;;;
;;; The first TEST instruction's result is ignored.
;;; The second TEST conveys the register holding the size to allocate in bytes.

;;; After enabling profiling, the first JMP becomes a JEQ so that
;;; if the data buffer exists, the CALL is performed.
;;; If this code is actually reached, and the call performed,
;;; then the call instruction is modified to (something like):
;;;
;;;  9E98:       4D8B5D18         MOV R11, [R13+24]      ; thread.profile-data
;;;  9E9C:       4D85DB           TEST R11, R11
;;;  9E9F:       7417             JEQ L21
;;;  9EA1:       0F1F8000000000   NOP
;;;  9EA8:       F049FF83C8020000 LOCK INC QWORD PTR [R11+712]
;;;  9EB0:       F049018BD0020000 LOCK ADD [R11+720], RCX
;;;  9EB8: L21:  4D8B5D20         MOV R11, [R13+32]      ; thread.alloc-region

;;; All of the modifications to running code can be made safely
;;; even if multiple threads are in that code at the same time, because:
;;;
;;; * changing a JMP to a JNE requires writing only 1 byte.
;;;   Instruction fetch will either see the new byte or the old,
;;;   and it doesn't matter which.
;;;
;;; * changing CALL to LOCK INC writes one naturally aligned octword.
;;;   Moreover, RIP gets decremented so that the next fetch won't read
;;;   the NOP or only part of the bytes of the LOCK INC.

;;; See the example run at the bottom of this file.

(defpackage #:sb-aprof
  (:use #:cl #:sb-ext #:sb-alien #:sb-sys #:sb-int #:sb-kernel)
  (:export #:aprof-run #:aprof-show)
  (:import-from #:sb-di #:valid-lisp-pointer-p)
  (:import-from #:sb-vm #:rbp-offset)
  (:import-from #:sb-x86-64-asm
                #:get-gpr #:reg #:reg-num
                #:machine-ea #:machine-ea-p
                #:machine-ea-disp #:machine-ea-base #:machine-ea-index
                #:inc #:add #:mov))

(in-package #:sb-aprof)
(setf (system-package-p *package*) t)

(defstruct (alloc (:constructor make-alloc (bytes count type pc)))
  bytes count type pc)
(declaim (freeze-type alloc))

(defvar *allocation-profile-metadata* nil)

(defvar *allocation-fixups-installed*
  (make-hash-table :test 'eq :weakness :key :synchronized t))

(define-alien-variable alloc-profile-buffer system-area-pointer)
(defun aprof-reset ()
  (alien-funcall (extern-alien "memset" (function void system-area-pointer int size-t))
                 alloc-profile-buffer
                 0
                 (* (/ (length *allocation-profile-metadata*) 2)
                    sb-vm:n-word-bytes)))

(defun patch-fixups ()
  (let ((n-fixups 0)
        (n-patched 0)
        (from-ht sb-c::*allocation-point-fixups*)
        (to-ht *allocation-fixups-installed*))
    (when (plusp (hash-table-count from-ht))
      (dohash ((code fixups) from-ht)
        (do-packed-varints (loc fixups)
          (incf n-fixups)
          (let ((byte (sap-ref-8 (code-instructions code) loc)))
            (when (eql byte #xEB)
              (setf (sap-ref-8 (code-instructions code) loc) #x74) ; JEQ
              (incf n-patched))))
        (setf (gethash code to-ht) fixups)
        (remhash code from-ht)))
    (values n-fixups n-patched)))

(defun aprof-start ()
  (let ((v *allocation-profile-metadata*))
    (unless v
      (setq v (make-array 100000) *allocation-profile-metadata* v)
      (with-pinned-objects (v)
        (setf (extern-alien "alloc_profile_data" unsigned)
              (sb-kernel:get-lisp-obj-address v)))))
  (alien-funcall (extern-alien "allocation_profiler_start" (function void))))

(defun aprof-stop ()
  (alien-funcall (extern-alien "allocation_profiler_stop" (function void))))

(defglobal *tag-to-type*
  (map 'vector
       (lambda (x)
        (cond ((sb-vm::specialized-array-element-type-properties-p x)
               (let ((et (sb-vm:saetp-specifier x)))
                 (sb-kernel:type-specifier
                  (sb-kernel:specifier-type `(simple-array ,et 1)))))
              (x
               (sb-vm::room-info-name x))))
       sb-vm::*room-info*))

(defun layout-name (ptr)
  (if (eql (valid-lisp-pointer-p (int-sap ptr)) 0)
      'structure
      (layout-classoid-name (make-lisp-obj ptr))))

;;; These EAs are s-expressions, not instances of EA or MACHINE-EA.
(defconstant-eqx p-a-flag `(ea ,(ash sb-vm::thread-pseudo-atomic-bits-slot sb-vm:word-shift)
                               ,(get-gpr :qword (sb-c:tn-offset sb-vm::thread-base-tn)))
  #'equal)
(defconstant-eqx region-ptr
    `(ea ,(ash sb-vm::thread-alloc-region-slot sb-vm:word-shift)
         ,(get-gpr :qword (sb-c:tn-offset sb-vm::thread-base-tn)))
  #'equal)
(defconstant-eqx region-end
    `(ea ,(ash (1+ sb-vm::thread-alloc-region-slot) sb-vm:word-shift)
         ,(get-gpr :qword (sb-c:tn-offset sb-vm::thread-base-tn)))
  #'equal)

(defun header-word-store-p (inst bindings)
  ;; a byte-sized or word-sized store at displacement 8 through 10 is OK
  (let ((ea (caddr inst))
        (freeptr (cdr (assoc '$free bindings))))
    (and (eq (car inst) 'mov)
         (typep ea 'machine-ea)
         (typep freeptr 'reg)
         (eql (reg-num freeptr) (machine-ea-base ea))
         (or (and (member (cadr inst) '(:byte :word))
                  (typep (machine-ea-disp ea) '(integer 8 10)))
             (and (eq (cadr inst) :qword)
                  (eql (machine-ea-disp ea) 8)
                  (typep (cadddr inst) 'reg))))))

;;; Templates to try in order.  The one for unknown headered objects should be last
;;; so that we try to match a store to the header word if possible.
(defglobal *allocation-templates* nil)
;;; Running sb-aprof with #+sb-show is not an important concern,
;;; and I don't care to fix it. It gets an error here:
;;; "don't know how to dump R13 (default MAKE-LOAD-FORM method called)."
#-sb-show
(setq *allocation-templates*
      `((array ;; also array-header
               (xadd $free $size)
               (cmp :qword $free ,region-end)
               (jmp :nbe $_)
               (mov :qword ,region-ptr $free)
               (mov $_ (ea 0 $size) $header); after XADD, size is the old free ptr
               (:optional (mov $_ (ea 8 $size) $vector-len))
               (:or (or $size ,sb-vm:other-pointer-lowtag)
                    (lea :qword $result (ea ,sb-vm:other-pointer-lowtag $size))))

        (any (:or (lea :qword $end (ea $nbytes $free $nbytes-var))
                  ;; LEA with scale=1 can have base and index swapped
                  (lea :qword $end (ea 0 $nbytes-var $free))
                  (add $end $free)) ; $end originally holds the size in bytes
             (cmp :qword $end ,region-end)
             (jmp :nbe $_)
             (mov :qword ,region-ptr $end)
             (mov $_ (ea 0 $free) $header)
             (:optional (:if header-word-store-p (mov $_ (ea $_ $free) $vector-len)))
             (:or (or $free $lowtag)
                  (lea :qword $result (ea $lowtag $free))))

        ;; either non-headered object (cons) or unknown header or unknown nbytes
        (unknown-header (:or (lea :qword $end (ea $nbytes $free $nbytes-var))
                             (lea :qword $end (ea 0 $nbytes-var $free))
                             (add $end $free))
                        (cmp :qword $end ,region-end)
                        (jmp :nbe $_)
                        (mov :qword ,region-ptr $end)
                        (:repeat (:or (mov . ignore) (lea . ignore)))
                        (:or (or $free $lowtag)
                             (lea :qword $result (ea $lowtag $free))))))

(defglobal *allocation-templates-large* nil)
(setq *allocation-templates-large*
      `((array (push $nbytes)
               (call . ignore)
               (pop $result)
               (mov $_ (ea 0 $result) $header)
               (mov $_ (ea $_ $result) $vector-len)
               (or $result $lowtag))))

(defun iterator-begin (iterator pc code)
  (let ((segment (sb-disassem:make-code-segment
                  code
                  (sb-sys:sap- (sb-sys:int-sap pc) (sb-kernel:code-instructions code))
                  1000)) ; arbitrary
        (dstate (cddr iterator)))
    (setf (sb-disassem:dstate-segment dstate) segment
          (sb-disassem:dstate-segment-sap dstate) (funcall (sb-disassem:seg-sap-maker segment))
          (sb-disassem:dstate-cur-offs dstate) 0)))

(defun get-instruction (iterator)
  (destructuring-bind (pos vector . dstate) iterator
    (if (< pos (length vector))
        (aref vector pos)
        (let ((inst (sb-disassem:disassemble-instruction dstate)))
          ;; FIXME: this drops any LOCK prefix, but that seems to be ok
          (do ((tail (cdr inst) (cdr tail)))
              ((null tail))
            (when (typep (car tail) '(cons machine-ea))
              (setf inst (list* (car inst) (cdar tail) (cdr inst))
                    (car tail) (caar tail))
              (return)))
          (vector-push-extend inst vector)
          inst))))

(defparameter *debug-deduce-type* nil)
(eval-when (:compile-toplevel)
  (defmacro note (&rest args)
    `(when *debug-deduce-type*
       (let ((*print-pretty* nil))
         (format t ,@args)))))

;;; If INPUT matches PATTERN then return a potentially amended
;;; list of BINDINGS, otherwise return :FAIL.
(defun %matchp (input pattern bindings)
  (when (eq (car pattern) :or)
    ;; Match the first thing possible. There is no backtracking.
    (dolist (choice (cdr pattern) (return-from %matchp :fail))
      (let ((new-bindings (%matchp input choice bindings)))
        (unless (eq new-bindings :fail)
          (return-from %matchp new-bindings)))))
  (let ((inst (get-instruction input)))
    (note "Pat=~S Inst=~S bindings=~S~%" pattern inst bindings)
    (when (eq (car pattern) :if)
      (unless (funcall (cadr pattern) inst bindings)
        (return-from %matchp :fail))
      (setq pattern (caddr pattern)))
    (unless (string= (car inst) (car pattern))
      (return-from %matchp :fail))
    (pop pattern)
    (pop inst)
    (when (eq pattern 'ignore) ; don't parse operands
      (return-from %matchp bindings))
    (labels ((match-atom (pattern input &optional ea-reg-p)
               (note "   match-atom ~s ~s ~s ~s~%" pattern input ea-reg-p bindings)
               (when (and (integerp input) ea-reg-p)
                 (setq input (get-gpr :qword input)))
               (cond ((eq pattern '$_) t) ; match and ignore anything
                     ((and (symbolp pattern) (char= (char (string pattern) 0) #\$))
                      ;; free variable binds to input, otherwise binding must match
                      (let* ((cell (assq pattern bindings))
                             (binding (cdr cell)))
                        (cond (cell
                               (if (and (typep input 'reg) (typep binding 'reg))
                                   ;; ignore the operand size I guess?
                                   (= (reg-num input) (reg-num binding))
                                   (eql input binding)))
                              ((ok-binding pattern input)
                               (note "   binding ~s~%" pattern)
                               (push (cons pattern input) bindings)))))
                     (t (eql pattern input))))
             (ok-binding (pattern input)
               (case pattern
                ($lowtag
                 (memq input `(,sb-vm:instance-pointer-lowtag ,sb-vm:list-pointer-lowtag
                               ,sb-vm:fun-pointer-lowtag ,sb-vm:other-pointer-lowtag)))
                (t t))))
      (loop
       (let* ((pattern-operand (pop pattern))
              (inst-operand (pop inst))
              (matchp
               (etypecase pattern-operand
                (symbol
                 (match-atom pattern-operand inst-operand))
                ((cons (eql ea))
                 (and (typep inst-operand 'machine-ea)
                      (destructuring-bind (disp base &optional index) (cdr pattern-operand)
                        (and (match-atom disp (or (machine-ea-disp inst-operand) 0))
                             (match-atom base (machine-ea-base inst-operand) t)
                             (match-atom index (machine-ea-index inst-operand) t)))))
                ((or (integer 1 15) ; looks like a widetag
                     reg)
                 (eql pattern-operand inst-operand)))))
         (unless matchp
           (return-from %matchp :fail)))
       (when (null pattern)
         (return (if (null inst) bindings)))))))

(defun matchp (input template bindings &aux (start (car input)))
  (macrolet ((inst-matchp (input pattern)
               `(let ((new-bindings (%matchp ,input ,pattern bindings)))
                  (note "bindings <- ~s~%" new-bindings)
                  (cond ((eq new-bindings :fail) nil)
                        (t (setq bindings new-bindings)
                           (incf (car input))))))
             (fail ()
               `(progn (setf (car input) start) ; rewind the iterator
                       (return-from matchp :fail))))
    (loop (when (endp template) (return bindings))
          (let ((pattern (pop template)))
            (case (car pattern)
             ((:optional :repeat)
              (let ((count (if (eq (car pattern) :repeat) most-positive-fixnum 1)))
                ;; :REPEAT matches zero or more instructions, but as few as possible.
                ;; :OPTIONAL matches zero or one, similarly.
                (let ((next-pattern (pop template)))
                  (loop (when (inst-matchp input next-pattern) (return))
                        (unless (and (>= (decf count) 0)
                                     (inst-matchp input (cadr pattern)))
                          (fail))))))
             (t
              (unless (inst-matchp input pattern)
                (fail))))))))

(defun deduce-layout (iterator bindings)
  (unless (assq '$result bindings)
    (push `($result . ,(cdr (assq '$free bindings))) bindings))
  (let ((bindings
         (matchp iterator
                 (load-time-value
                  `((xor :qword ,p-a-flag ,(get-gpr :qword rbp-offset))
                    (jmp :eq $_)
                    (break . ignore)
                    (mov :dword (ea 1 $result) $layout)) t)
                 bindings)))
    (if (eq bindings :fail)
        'instance
        (layout-name (cdr (assq '$layout bindings))))))

(defun deduce-fun-subtype (iterator bindings)
  (declare (ignorable iterator bindings))
  #-immobile-space 'function
  #+immobile-space
  (let* ((bindings
          (matchp iterator
                  (load-time-value
                   `((mov $scratch $header)
                     (or :qword $scratch
                         (ea ,(ash sb-vm::thread-function-layout-slot sb-vm:word-shift)
                             ,(get-gpr :qword (sb-c:tn-offset sb-vm::thread-base-tn))))
                     (mov :qword (ea ,(- sb-vm:fun-pointer-lowtag) $result) $scratch))
                   t)
                  bindings))
         (header (and (listp bindings) (cdr (assoc '$header bindings)))))
    (if (and (integerp header) (eq (logand header #xFF) sb-vm:closure-widetag))
        'closure
        'function)))

(defun deduce-type (pc dstate code &optional (*debug-deduce-type* *debug-deduce-type*)
                                             template-name)
  (dx-let ((iterator (list* 0 (make-array 16 :fill-pointer 0) dstate)))
    (iterator-begin iterator pc code)

    ;; Expect an increment of the allocation point hit counter
    (let* ((inst (get-instruction iterator))
           (ea (third inst))) ; (INC :qword EA)
      (when (and (eq (car inst) 'inc)
                 (eql (machine-ea-base ea) (sb-c:tn-offset sb-vm::temp-reg-tn))
                 (null (machine-ea-index ea)))
        (incf (car iterator))
        (let ((profiler-index (machine-ea-disp ea)))
          ;; Optional: the total number of bytes at the allocation point
          (let* ((inst (get-instruction iterator))
                 (ea (third inst)))
            (when (and (eq (car inst) 'add)
                       (machine-ea-p ea)
                       (eql (machine-ea-base ea) (sb-c:tn-offset sb-vm::temp-reg-tn))
                       (null (machine-ea-index ea))
                       (eql (machine-ea-disp ea) (+ profiler-index sb-vm:n-word-bytes)))
              (incf (car iterator)))))))

    ;; Expect a store to the pseudo-atomic flag
    (when (eq (matchp iterator
                      (load-time-value `((mov :qword ,p-a-flag ,(get-gpr :qword rbp-offset))) t)
                      nil) :fail)
      (return-from deduce-type (values nil nil)))

    (let* ((type)
           (bindings
            (matchp iterator
                    ;; compiler bug? If expressed with "`" then the compiler tries to
                    ;; refer to the undumpable constant REGION-PTR by its value, not by its
                    ;; name, then complaining that it can't be dumped.
                    ;; Why is ",P-A-FLAG" acceptable but this not?
                    (load-time-value (list (list 'mov :qword '$free region-ptr)) t)
                    nil))
           (templates
            (cond (template-name
                   (list (find template-name *allocation-templates* :key 'car)))
                  ((eq bindings :fail)
                   (setq bindings nil)
                   *allocation-templates-large*)
                  (t *allocation-templates*))))
      (dolist (allocator templates
                         (error "Unrecognized allocator at ~x in ~s:~{~%~S~}"
                                pc code (coerce (second iterator) 'list)))
        (note "Trying ~a~%" (car allocator))
        (let ((new-bindings (matchp iterator (cdr allocator) bindings)))
          (unless (eq new-bindings :fail)
            (setq bindings new-bindings type (car allocator))
            (return))))
      (if (eq bindings :fail)
          (values nil nil)
          (let ((nbytes (cdr (assoc '$nbytes bindings)))
                (header (cdr (assoc '$header bindings)))
                (lowtag (cdr (assoc '$lowtag bindings))))
            ;; The low bit might signify something to the allocator. Clear it.
            (when (integerp nbytes)
              (setq nbytes (logandc2 nbytes 1)))
            ;; matchp converts NIL in a machine-ea to 0.  The disassembler uses
            ;; NIL to signify that there was no displacement, which makes sense
            ;; when register indirect mode is used without a SIB byte.
            (when (eq nbytes 0)
              (setq nbytes nil))
            (cond ((and (member type '(array any))
                        (typep header '(or sb-vm:word sb-vm:signed-word)))
                   (setq type (aref *tag-to-type* (logand header #xFF)))
                   (when (eq type 'instance)
                     (setq type (deduce-layout iterator bindings))))
                  ((member type '(any unknown-header))
                   (setq type (case lowtag
                                (#.sb-vm:list-pointer-lowtag 'list)
                                (#.sb-vm:instance-pointer-lowtag 'instance)
                                (#.sb-vm:fun-pointer-lowtag
                                 (deduce-fun-subtype iterator bindings))
                                (t '#:|unknown|)))))
            (values type nbytes))))))

;;; Return a name for PC-OFFS in CODE. PC-OFFSET is relative
;;; to CODE-INSTRUCTIONS.
(defun pc-offs-to-fun-name (pc-offs code &aux (di (%code-debug-info code)))
  (if (consp di) ; assembler routines
      (block nil
       (maphash (lambda (k v) ; FIXME: OAOO violation, at least twice over
                  (when (<= (car v) pc-offs (cadr v))
                    (return k)))
                (car di)))
      (let* ((funmap (sb-c::compiled-debug-info-fun-map di)))
        (unless (sb-c::compiled-debug-fun-next funmap)
          (aver (typep funmap 'sb-c::compiled-debug-fun-toplevel))
          (return-from pc-offs-to-fun-name :toplevel))
        (loop for fun = funmap then next
              for next = (sb-c::compiled-debug-fun-next fun)
              when (or (not next)
                       (< (sb-c::compiled-debug-fun-offset next) pc-offs))
              return (sb-c::compiled-debug-fun-name fun)))))

(defun aprof-collect (stream)
  (sb-disassem:get-inst-space) ; for effect
  (let* ((metadata *allocation-profile-metadata*)
         (n-hit (extern-alien "alloc_profile_n_counters" int))
         (metadata-len (/ (length metadata) 2))
         (n-counters (min metadata-len n-hit))
         (sap (extern-alien "alloc_profile_buffer" system-area-pointer))
         (index 3)
         (dstate (sb-disassem:make-dstate nil))
         (collection (make-hash-table :test 'equal)))
    (when stream
      (format stream "~&~d (of ~d max) profile entries consumed~2%"
              n-hit metadata-len))
    (loop
     (when (>= index n-counters)
       (return collection))
     (let ((count (sap-ref-word sap (* index 8))))
       (multiple-value-bind (code pc-offset total-bytes)
           (if (null (aref metadata (ash index 1))) ; sized alloc
               (values (aref metadata (+ (ash index 1) 2))
                       (aref metadata (+ (ash index 1) 3))
                       (sap-ref-word sap (* (1+ index) 8)))
               (values (aref metadata (+ (ash index 1) 0))
                       (aref metadata (+ (ash index 1) 1))))
         (incf index (if total-bytes 2 1)) ; <count,bytes> or just a count
         (unless (eql count 0)
           (with-pinned-objects (code)
            (let ((pc (+ (sb-kernel:get-lisp-obj-address code)
                          (- sb-vm:other-pointer-lowtag)
                          pc-offset))
                  (name (pc-offs-to-fun-name
                         ;; Relativize to CODE-INSTRUCTIONS, not the base address
                         (- pc-offset (ash (code-header-words code) sb-vm:word-shift))
                         code)))
              (multiple-value-bind (type size) (deduce-type pc dstate code)
                (cond (size ; fixed-size allocation
                       (aver (not total-bytes))
                       (push (make-alloc (* count size) count type pc)
                             (gethash name collection)))
                      (t ; variable-size
                       (aver total-bytes)
                       ;; The only allocator that determines TYPE at run-time
                       ;; is ALLOCATE-VECTOR-ON-HEAP. VAR-ALLOC uses a codegen
                       ;; arg, not a TN, for the widetag, but it looks like
                       ;; the type is dynamic because it kind of is: the
                       ;; header words is computed into a register and written
                       ;; in a single store.
                       (push (make-alloc total-bytes count type pc)
                             (gethash name collection)))))))))))))

(defun collapse-by-type (data &aux new)
  (dolist (datum data new)
    (let ((found (find (alloc-type datum) new :key #'alloc-type
                       :test #'equal)))
      (cond (found
             (incf (alloc-bytes found) (alloc-bytes datum))
             (incf (alloc-count found) (alloc-count datum)))
            (t
             (push datum new))))))

;; DETAIL NIL shows just function name and percent space consumption
;; DETAIL T shows function, bytes, percent,
;;    and unless there is only one detail line, the detail lines
;;
(defun aprof-show (&key (top-n 20) (detail t) (collapse t) (stream *standard-output*))
  (unless top-n
    (setq top-n 1000))
  (let* ((collection (%hash-table-alist (aprof-collect stream)))
         (summary
          (mapcar (lambda (x)
                    (list* (car x)
                           (reduce #'+ (cdr x) :key #'alloc-bytes)
                           (cdr x)))
                  collection))
         (sorted (sort (copy-list summary) #'> :key #'second))
         (total-bytes (reduce #'+ sorted :key #'second))
         (*print-pretty* nil)
         (i 0)
         (sum-pct 0)
         (sum-bytes 0))
    (when (eq stream nil)
      (setq stream (make-broadcast-stream))) ; lazy's person's approach
    (cond ((not detail)
           (format stream "~&       %    Sum %        Bytes    Allocations   Function~%")
           (format stream "~& -------  -------  -----------    -----------   --------~%"))
          (t
           (format stream "~&       %        Bytes        Count    ~:[~;    PC        ~]Function~%"
                   (not collapse))
           (format stream "~& -------  -----------    ---------    ~:[~;----------    ~]--------~%"
                   (not collapse))))

    ;; In detailed view, each function takes up either one line or
    ;; more than one line. In the interest of saving space, newlines are
    ;; omitted between consecutive functions each of whose detail consists
    ;; of a single line. But to avoid ambiguity, esure that there is always
    ;; a blank line before and after each function having multiline detail.
    (let ((emitted-newline t))
      (dolist (x sorted)
        (destructuring-bind (name bytes . data) x
          (when detail
            (when collapse
              (setq data (collapse-by-type data)))
            (setq data (sort data #'> :key #'alloc-bytes)))
          (assert (eq bytes (reduce #'+ data :key #'alloc-bytes)))
          (when (and detail (cdr data) (not emitted-newline))
            (terpri stream))
          (incf sum-pct (float (/ bytes total-bytes)))
          ;; Show summary for the function
          (cond ((not detail)
                 (format stream " ~5,1,2f      ~5,1,2f ~12d~15d   ~s~%"
                         (/ bytes total-bytes)
                         sum-pct
                         bytes
                         (reduce #'+ data :key #'alloc-count)
                         name))
                (t
                 (format stream " ~5,1,2f   ~12d   ~:[~10@t~;~:*~10d~]~@[~14@a~]    ~s~@[ - ~s~]~%"
                         (/ bytes total-bytes)
                         bytes
                         (if (cdr data) nil (alloc-count (car data)))
                         (cond (collapse nil)
                               ((cdr data) "")
                               (t (write-to-string (alloc-pc (car data)) :base 16)))
                         name
                         (if (cdr data) nil (alloc-type (car data)))
                         )))
          (when (and detail (cdr data))
            (dolist (point data)
              (format stream "     ~5,1,2f ~12d ~10d~@[~14x~]~@[        ~s~]~%"
                        (/ (alloc-bytes point) bytes) ; fraction within function
                        (alloc-bytes point)
                        (alloc-count point)
                        (if collapse nil (alloc-pc point))
                        (alloc-type point))))
          (incf sum-bytes bytes)
          (when (and detail
                     (setq emitted-newline (not (null (cdr data)))))
            (terpri stream)))
        (incf i)
        (if (and (neq top-n :all) (>= i top-n)) (return))))
;    (assert (= sum-bytes total-bytes))
    (cond ((not detail)
           (format stream "~19@t===========~%~19@t~11d~%" sum-bytes))
          (t
           (format stream " =======  ===========~%~6,1,2f   ~12d~%"
                   sum-pct sum-bytes)))
    sum-bytes))

;;; Call FUN and return the exact number of bytes it (an all descendant
;;; calls) allocated, provided that they were instrumented for precise
;;; cons profiling.
;;; STREAM is where to report, defaulting to *standard-output*.
;;; The convention is that of map-segment-instructions, meaning NIL is a sink.
(defun aprof-run (fun &key (stream *standard-output*))
  (aprof-reset)
  (patch-fixups)
  (let (nbytes)
    (unwind-protect
         (progn (aprof-start) (funcall fun))
      (aprof-stop)
      (setq nbytes (aprof-show :stream stream))
      (when stream (terpri stream)))
    nbytes))

;;;;

;;; Example:
;;;   The default output is a report showing the top 20 allocators
;;;   (by function name) with a line of detail for each distinct
;;;   type of object allocated within the function.
;;;
;;; * (aprof-run (lambda () (compile-file "~/aprof")))
;;;        %        Bytes        Count    Function
;;;  -------  -----------    ---------    --------
;;;    6.6        1980160                 INIT-SB-VECTORS
;;;       79.1      1565824      48880        SIMPLE-BIT-VECTOR
;;;       20.9       414336       1872        SIMPLE-VECTOR
;;;
;;;    4.6        1368960                 MAKE-TN
;;;       83.3      1140800       7130        TN
;;;       16.7       228160       7130        SIMPLE-BIT-VECTOR

;;; The report can be made more detailed by not combining lines
;;; for the same object type within a function:
;;;
;;; * (aprof-show :detail  t :collapse nil)
;;;
;;;        %        Bytes        Count        PC        Function
;;;  -------  -----------    ---------    ----------    --------
;;;    6.6        1980160                               INIT-SB-VECTORS
;;;       76.0      1504256      47008      21D07E88        SIMPLE-BIT-VECTOR
;;;       20.9       414336       1872      21D07D28        SIMPLE-VECTOR
;;;        3.1        61568       1872      21D07F30        SIMPLE-BIT-VECTOR
;;;
;;;    4.6        1368960                               MAKE-TN
;;;       83.3      1140800       7130      21C98638        TN
;;;       16.7       228160       7130      21C985D8        SIMPLE-BIT-VECTOR
;;;
;;;    4.0        1204160                               MAKE-HASH-TABLE
;;;       39.3       473472       1644      21B2A008        SIMPLE-VECTOR
;;;       37.1       447168       1644      21B29F50        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
;;;       21.8       263040       1644      21B29FA8        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
;;;        1.7        20480        128      21B2A098        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))

;;; Or less detailed by combining all lines within a function:
;;;
;;; * (sb-aprof::aprof-show :detail nil :top-n 1000) ; or anything
;;;        %    Sum %        Bytes    Allocations   Function
;;;  -------  -------  -----------    -----------   --------
;;;    6.6        6.6      1980160          50752   INIT-SB-VECTORS
;;;    4.6       11.1      1368960          14260   MAKE-TN
;;;    4.0       15.1      1204160           5060   MAKE-HASH-TABLE
;;;    3.8       19.0      1154384          28727   CONSTRAIN-REF-TYPE
;;;    3.8       22.8      1152160          13084   COPY-CONSET
;;;    3.6       26.5      1094240          13678   MAKE-TN-REF
;;; ... many more lines ...
;;;   00.0      100.0           16              1   %ENTER-NEW-NICKNAMES
;;;   00.0      100.0           16              1   UNIX-LSTAT
;;;                    ===========
;;;                       30054816
