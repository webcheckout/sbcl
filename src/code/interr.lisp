;;;; functions and macros to define and deal with internal errors
;;;; (i.e. problems that can be signaled from assembler code)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; internal errors

(macrolet ((def-it ()
             (let ((n (1+ (position-if 'stringp sb-c:+backend-internal-errors+
                                       :key #'car :from-end t))))
               `(progn
                  (defconstant n-internal-error-handlers ,n)
                  (declaim ((simple-vector ,n) **internal-error-handlers**))
                  (!define-load-time-global **internal-error-handlers**
                              ,(make-array n :initial-element 0))))))
  (def-it))

(defmacro deferr (name args &rest body)
  (multiple-value-bind (llks required optional rest) (parse-lambda-list args)
    (declare (ignore llks))
    (aver (not rest))
    (let ((max (+ (length required) (length optional))))
      (unless (<= max 3)
        (error "Update (DEFUN INTERNAL-ERROR) for ~D error arguments" max))))
  `(setf (svref **internal-error-handlers** ,(error-number-or-lose name))
         (named-lambda ,(string name) (,@args)
           (declare (optimize (sb-c::verify-arg-count 0)))
           ,@body)))

;;; Backtrace code may want to know the error that caused
;;; interruption, but there are other means to get code interrupted
;;; and inspecting code around PC for the error number may yield wrong
;;; results.
(defvar *current-internal-error* nil)
(defvar *current-internal-trap-number*)
(defvar *current-internal-error-args*)

#+undefined-fun-restarts
(defun restart-undefined (name condition fdefn-or-symbol context)
  (multiple-value-bind (tn-offset pc-offset)
      (if context
          (sb-c::decode-restart-location context)
          (car *current-internal-error-args*))
    (labels ((retry-value (value)
               (or (typecase value
                     (fdefn (fdefn-fun value))
                     (symbol
                      (let ((fdefn (symbol-fdefn value)))
                        (and fdefn
                             (fdefn-fun fdefn))))
                     (function value)
                     (t
                      (if (valid-function-name-p value)
                          (let ((fdefn (find-fdefn value)))
                            (and fdefn
                                 (fdefn-fun fdefn)))
                          (still-bad "Bad value when restarting ~s: ~s"
                                     name value))))
                   (still-bad (if (fdefn-p value)
                                  "~S is still undefined"
                                  "Can't replace ~s with ~s because it is undefined")
                              name value)))
             (set-value (function retrying)
               (if retrying
                   (retry-value function)
                   (sb-di::sub-set-debug-var-slot
                    nil tn-offset
                    (retry-value function)
                    *current-internal-error-context*)))
             (still-bad (format-control &rest format-arguments)
               (try (make-condition 'retry-undefined-function
                                    :name name
                                    :format-control format-control
                                    :format-arguments format-arguments)
                    t))
             (try (condition &optional retrying)
               (cond (context
                      ;; The #'abc case from SAFE-FDEFN-FUN, CONTEXT
                      ;; specifies the offset from the error location
                      ;; where it can retry checking the FDEFN
                      (prog1
                          (restart-case (error condition)
                            (continue ()
                              :report (lambda (stream)
                                        (format stream "Retry using ~s." name))
                              (set-value fdefn-or-symbol retrying))
                            (use-value (value)
                              :report (lambda (stream)
                                        (format stream "Use specified function."))
                              :interactive read-evaluated-form
                              (set-value value retrying)))
                        (unless retrying
                          (sb-vm::incf-context-pc *current-internal-error-context*
                                                  pc-offset))))
                     (t
                      (restart-case (error condition)
                        (continue ()
                          :report (lambda (stream)
                                    (format stream "Retry calling ~s." name))
                          (set-value fdefn-or-symbol retrying))
                        (use-value (value)
                          :report (lambda (stream)
                                    (format stream "Call specified function."))
                          :interactive read-evaluated-form
                          (set-value value retrying))
                        (return-value (&rest values)
                          :report (lambda (stream)
                                    (format stream "Return specified values."))
                          :interactive mv-read-evaluated-form
                          (set-value (lambda (&rest args)
                                       (declare (ignore args))
                                       (values-list values))
                                     retrying))
                        (return-nothing ()
                          :report (lambda (stream)
                                    (format stream "Return zero values."))
                          (set-value (lambda (&rest args)
                                       (declare (ignore args))
                                       (values))
                                     retrying)))))))
      (try condition))))

(deferr undefined-fun-error (fdefn-or-symbol)
  (let* ((name (etypecase fdefn-or-symbol
                 (symbol fdefn-or-symbol)
                 (fdefn (fdefn-name fdefn-or-symbol))))
         (condition
           ;; Depending whether NAME is a special operator we signal a different
           ;; condition class. Similar logic appears in SB-C::INSTALL-GUARD-FUNCTION.
           (make-condition (if (and (symbolp name) (special-operator-p name))
                               'special-form-function
                               'undefined-function)
                           :name name
                           :not-yet-loaded
                           (cond ((and (boundp 'sb-c:*compilation*)
                                       (member name (sb-c::fun-names-in-this-file
                                                     sb-c:*compilation*)
                                               :test #'equal))
                                  t)
                                 ((and (boundp 'sb-c:*lexenv*)
                                       (sb-c::fun-locally-defined-p
                                        name sb-c:*lexenv*))
                                  :local))))
         #+undefined-fun-restarts
         context)
    (cond #+undefined-fun-restarts
          ((or (= *current-internal-trap-number* sb-vm:cerror-trap)
               (integerp (setf context (sb-di:error-context))))
           (restart-undefined name condition fdefn-or-symbol context))
          (t
           (error condition)))))

#+(or arm arm64 x86-64)
(deferr undefined-alien-fun-error (address)
  (error 'undefined-alien-function-error
         :name
         (or (sb-di:error-context
              (sb-di:frame-down sb-debug:*stack-top-hint*))
             (and (integerp address)
                  (sap-foreign-symbol (int-sap address))))))

#-(or arm arm64 x86-64)
(defun undefined-alien-fun-error ()
  (error 'undefined-alien-function-error))

(deferr invalid-arg-count-error (nargs)
  (let* ((frame (find-interrupted-frame))
         (name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))))
    (when (typep name '(cons (eql sb-pcl::fast-method)))
      (decf nargs 2)))
  (restart-case
      (%program-error "invalid number of arguments: ~S" nargs)
    #+(or x86-64 arm64)
    (replace-function (value)
      :report (lambda (stream)
                (format stream "Call a different function with the same arguments"))
      :interactive read-evaluated-form
      (sb-vm::context-call-function *current-internal-error-context*
                                    (fdefinition value)))
    #+(or x86-64 arm64)
    (call-form (form)
      :report (lambda (stream)
                (format stream "Call a different form"))
      :interactive read-evaluated-form
      (sb-vm::context-call-function *current-internal-error-context*
                                    (lambda ()
                                      ;; Don't invoke the compiler in
                                      ;; case it's dealing with an
                                      ;; error within the compiler
                                      (let (#+(or sb-eval sb-fasteval)
                                            (*evaluator-mode* :interpret))
                                        (eval form)))
                                    0))))

(deferr local-invalid-arg-count-error (nargs name)
  (%program-error "~S called with invalid number of arguments: ~S"
                  name nargs))

(deferr bogus-arg-to-values-list-error (list)
  (with-simple-restart (continue "Ignore the last CDR")
    (error 'values-list-argument-error :datum list :expected-type 'list)))

(defun restart-unbound (symbol condition context)
  (multiple-value-bind (tn-offset pc-offset)
      (sb-c::decode-restart-location context)
    (labels ((retry-value (value)
               (multiple-value-bind (type defined)
                   (info :variable :type symbol)
                 (if (and defined
                          (not (ctypep value type)))
                     (still-bad (sb-format:tokens
                                 "Type mismatch when restarting unbound symbol error:~@
                                 ~s is not of type ~/sb-impl:print-type/")
                                value type)
                     value)))
             (set-value (value &optional set-symbol)
               (sb-di::sub-set-debug-var-slot
                nil tn-offset (retry-value value)
                *current-internal-error-context*)
               (sb-vm::incf-context-pc *current-internal-error-context*
                                       pc-offset)
               (when set-symbol
                 (set symbol value))
               (return-from restart-unbound))
             (retry-evaluation ()
               (if (boundp symbol)
                   (set-value (symbol-value symbol))
                   (still-bad "~s is still unbound" symbol)))
             (still-bad (format-control &rest format-arguments)
               (try (make-condition 'retry-unbound-variable
                                    :name symbol
                                    :format-control format-control
                                    :format-arguments format-arguments)))
             (try (condition)
               (restart-case (error condition)
                 (continue ()
                   :report (lambda (stream)
                             (format stream "Retry using ~s." symbol))
                   (retry-evaluation))
                 (use-value (value)
                   :report (lambda (stream)
                             (format stream "Use specified value."))
                   :interactive read-evaluated-form
                   (set-value value))
                 (store-value (value)
                   :report (lambda (stream)
                             (format stream "Set specified value and use it."))
                   :interactive read-evaluated-form
                   (set-value value t)))))
      (try condition))))

(deferr unbound-symbol-error (symbol)
  (let* ((context (sb-di:error-context))
         (condition (make-condition 'unbound-variable
                                    :name symbol
                                    :not-yet-loaded
                                    (cond ((and (boundp 'sb-c:*lexenv*)
                                                (sb-c:lexenv-find symbol vars))
                                           :local)))))
    (if context
        (restart-unbound symbol condition context)
        (error condition))))

(deferr invalid-unwind-error ()
  (error 'simple-control-error
         :format-control
         "attempt to RETURN-FROM a block or GO to a tag that no longer exists"))

(deferr unseen-throw-tag-error (tag)
  (let ((text "attempt to THROW to a tag that does not exist: ~S"))
    #+sb-fasteval
    (when (listp tag)
      (binding* ((frame (find-interrupted-frame))
                 (name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))
                 (down (and (eq name 'throw) ; is this tautological ?
                            (sb-di:frame-down frame)) :exit-if-null))
        (case (sb-di:debug-fun-name (sb-di:frame-debug-fun down))
         ((return-from)
          (setq text "attempt to RETURN-FROM an exited block: ~S"
                     ;; block name was wrapped in a cons
                tag (car tag)))
         ((go)
                     ;; FIXME: can we reverse-engineer the tag name from
                     ;; the object that was thrown, for a better diagnostic?
          (setq text "attempt to GO into an exited tagbody")))))
    (error 'simple-control-error
           :format-control text :format-arguments (list tag))))

(deferr nil-fun-returned-error (function)
  (error 'simple-control-error
         :format-control
         "A function with declared result type NIL returned:~%  ~S"
         :format-arguments (list function)))

(deferr nil-array-accessed-error (array)
  (error 'nil-array-accessed-error
         :datum array :expected-type '(not (array nil))))

(deferr division-by-zero-error (this that)
  (error 'division-by-zero
         :operation '/
         :operands (list this that)))

(macrolet ((def (errname fun-name)
             `(setf (svref **internal-error-handlers**
                           ,(error-number-or-lose errname))
                    (fdefinition ',fun-name))))
  (def etypecase-failure-error etypecase-failure)
  (def ecase-failure-error ecase-failure))

(deferr object-not-type-error (object type)
  (if (invalid-array-p object)
      (invalid-array-error object)
      (error (if (and (%instancep object)
                      (layout-invalid (%instance-layout object)))
                 'layout-invalid
                 'type-error)
             :datum object
             :expected-type (typecase type
                              (classoid-cell
                               (classoid-cell-name type))
                              (layout
                               (layout-proper-name type))
                              (t
                               type))
             :context (sb-di:error-context))))

(deferr layout-invalid-error (object layout)
  (error 'layout-invalid
         :datum object
         :expected-type (layout-classoid layout)))

(deferr odd-key-args-error ()
  (%program-error "odd number of &KEY arguments"))

(deferr unknown-key-arg-error (key-name)
  (let ((context (sb-di:error-context)))
    (if (integerp context)
        (restart-case
            (error 'unknown-keyword-argument :name key-name)
          (continue ()
            :report (lambda (stream)
                      (format stream "Ignore all unknown keywords"))
            (sb-vm::incf-context-pc *current-internal-error-context*
                                    context)))
        (error 'unknown-keyword-argument :name key-name))))

;; TODO: make the arguments (ARRAY INDEX &optional BOUND)
;; and don't need the bound for vectors. Just read it.
(deferr invalid-array-index-error (array bound index)
  (invalid-array-index-error array index bound))

(deferr tls-exhausted-error ()
  ;; There is nothing we can do about it. A number of entries in the
  ;; tls could be reserved and made available for recovery but since
  ;; tls indices are never reused it would be kind of silly and
  ;; without it signalling an error is more than likely to end in a
  ;; recursive error.
  (%primitive print "Thread local storage exhausted.")
  (sb-impl::%halt))

(deferr uninitialized-memory-error (address nbytes value)
  (declare (type sb-vm:word address))
  ;; Ignore sanitizer errors from reading the C stack.
  ;; These occur because foreign code typically marks shadow words as valid/invalid
  ;; as it consumes parts of the stack for each new frame; but Lisp does not mark words
  ;; as valid when storing to the stack, so reading via sap-ref-n needs to disregard
  ;; the sanitizer error.  This was especially noticeable in our 'callback.impure' test.
  ;; Obviously it would be more efficient to annotate all the pertinent code with
  ;; a safety 0 declaration to avoid a detour through the trap handler, but that was
  ;; more intrusive than I'd have liked. At minimum, these functions need some help:
  ;;   SB-DI::SUB-ACCESS-DEBUG-VAR-SLOT
  ;;   SB-DI::X86-CALL-CONTEXT
  ;;   SB-VM::BOXED-CONTEXT-REGISTER
  (let ((stackp (and (>= address (get-lisp-obj-address sb-vm:*control-stack-start*))
                     (< address (get-lisp-obj-address sb-vm:*control-stack-end*)))))
    (unless stackp
      (let ((pc (sap-int (sb-vm:context-pc *current-internal-error-context*))))
        (cerror "Treat the value #x~*~x as valid."
                'sanitizer-error
                :value value
                :address address
                :size nbytes
                :format-control "Read of uninitialized memory: ~D byte~:P at #x~x = #x~x (PC=#x~x)."
                :format-arguments (list nbytes address value pc))))))

(deferr failed-aver-error (form)
  (bug "~@<failed AVER: ~2I~_~S~:>" form))
(deferr unreachable-error ()
  (bug "Unreachable code reached"))

;;;; INTERNAL-ERROR signal handler

;;; This is needed for restarting XEPs, which do not bind anything but
;;; also do not save their own BSP, and we need to discard the
;;; bindings made by the error handling machinery.
#+unwind-to-frame-and-call-vop
(defvar *interr-current-bsp* nil)

(defun internal-error (context continuable)
  (declare (type system-area-pointer context))
  (declare (ignore continuable))
  (let (#+unwind-to-frame-and-call-vop
        (*interr-current-bsp*
          ;; Needs to be done before anything is bound
          (%primitive sb-c:current-binding-pointer)))
    (infinite-error-protect
     (let ((alien-context (sap-alien context (* os-context-t))))
       (multiple-value-bind (error-number arguments
                             *current-internal-trap-number*)
           (sb-vm::with-pinned-context-code-object (alien-context)
             (sb-vm:internal-error-args alien-context))
         (with-interrupt-bindings
           (let ((sb-debug:*stack-top-hint* (find-interrupted-frame))
                 (*current-internal-error* error-number)
                 (*current-internal-error-args* arguments)
                 (*current-internal-error-context* alien-context)
                 (fp (int-sap (sb-vm:context-register alien-context
                                                      sb-vm::cfp-offset))))
             (if (and (>= error-number (length **internal-error-handlers**))
                      (< error-number (length sb-c:+backend-internal-errors+)))
                 (let ((context (sb-di:error-context)))
                   (if (typep context '(cons (eql :struct-read)))
                       ;; This was shoehorned into being a "type error"
                       ;; which isn't the best way to explain it to the user.
                       ;; However, from an API stance, it makes some sense to signal
                       ;; a TYPE-ERROR since there may be existing code that catches
                       ;; unbound slots errors as type-errors. Our tests certainly do,
                       ;; but perhaps only as an artifact of the implementation.
                       (destructuring-bind (struct-name . slot-name) (cdr context)
                         ;; Infer the slot type, but fail safely. The message is enough,
                         ;; and the required type is pretty much irrelevant.
                         (let* ((dd (find-defstruct-description struct-name))
                                (dsd (and dd (find slot-name (dd-slots dd) :key #'dsd-name))))
                           (error 'simple-type-error
                                  :format-control "Accessed uninitialized slot ~S of structure ~S"
                                  :format-arguments (list slot-name struct-name)
                                  :datum (make-unbound-marker)
                                  :expected-type (if dsd (dsd-type dsd) 't))))
                       (error 'type-error
                              :datum (sb-di::sub-access-debug-var-slot
                                      fp (first arguments) alien-context)
                              :expected-type
                              (car (svref sb-c:+backend-internal-errors+
                                          error-number))
                              :context context)))
                 (let ((handler
                         (and (typep error-number `(mod ,n-internal-error-handlers))
                              (svref **internal-error-handlers** error-number))))
                   (cond
                     ((functionp handler)
                      (if (eq (car arguments) :raw) ; pass args as they are
                          (apply handler (cdr arguments))
                          ;; Otherwise decode the SC+OFFSETs
                          ;; INTERNAL-ERROR-ARGS supplies the right amount of arguments
                          (macrolet ((arg (n)
                                       `(sb-di::sub-access-debug-var-slot
                                         fp (nth ,n arguments) alien-context)))
                            (ecase (length arguments)
                              (0 (funcall handler))
                              (1 (funcall handler (arg 0)))
                              (2 (funcall handler (arg 0) (arg 1)))
                              (3 (funcall handler (arg 0) (arg 1) (arg 2)))))))
                     ((eql handler 0) ; if (DEFERR x) was inadvertently omitted
                      (error 'simple-error
                             :format-control
                             "unknown internal error, ~D, args=~S"
                             :format-arguments
                             (list error-number
                                   (mapcar (lambda (sc+offset)
                                             (sb-di::sub-access-debug-var-slot
                                              fp sc+offset alien-context))
                                           arguments))))
                     (t                 ; wtf?
                      (error 'simple-error
                             :format-control "internal error ~D: ~A; args=~S"
                             :format-arguments
                             (list error-number
                                   handler
                                   (mapcar (lambda (sc+offset)
                                             (sb-di::sub-access-debug-var-slot
                                              fp sc+offset alien-context))
                                           arguments))))))))))))))

(defun control-stack-exhausted-error ()
  (let ((sb-debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Control stack guard page temporarily disabled: proceed with caution~%")
     (error 'control-stack-exhausted))))

(defun binding-stack-exhausted-error ()
  (let ((sb-debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Binding stack guard page temporarily disabled: proceed with caution~%")
     (error 'binding-stack-exhausted))))

(defun alien-stack-exhausted-error ()
  (let ((sb-debug:*stack-top-hint* nil))
    (infinite-error-protect
     (format *error-output*
             "Alien stack guard page temporarily disabled: proceed with caution~%")
     (error 'alien-stack-exhausted))))

;;; KLUDGE: we keep a single HEAP-EXHAUSTED-ERROR object around, so
;;; that we don't need to allocate it when running out of
;;; memory. Similarly we pass the amounts in special variables as
;;; there may be multiple threads running into trouble at the same
;;; time. The condition is created by GC-REINIT.
(define-load-time-global *heap-exhausted-error-condition*
  (make-condition 'heap-exhausted-error))
(defvar *heap-exhausted-error-available-bytes*)
(defvar *heap-exhausted-error-requested-bytes*)

(defun heap-exhausted-error (available requested)
  ;; Double word aligned bytes, can be passed as fixnums to avoid
  ;; allocating bignums on the C side.
  (declare (fixnum available requested))
  (infinite-error-protect
   (let ((*heap-exhausted-error-available-bytes*
           (ash available sb-vm:n-fixnum-tag-bits))
         (*heap-exhausted-error-requested-bytes*
           (ash requested sb-vm:n-fixnum-tag-bits)))
     (error *heap-exhausted-error-condition*))))

(defun undefined-alien-variable-error ()
  (declare (optimize allow-non-returning-tail-call))
  (error 'undefined-alien-variable-error))

#-win32
(defun memory-fault-error (context-sap address-sap)
  (declare (ignore context-sap))
  (let ((sb-debug:*stack-top-hint* (find-interrupted-frame)))
    (error 'memory-fault-error
           :address (sap-int address-sap))))

;;; This is SIGTRAP / EXCEPTION_BREAKPOINT that runtime could not deal
;;; with. Prior to Windows we just had a Lisp side handler for
;;; SIGTRAP, but now we need to deal with this portably.
(defun unhandled-trap-error (context-sap)
  (declare (type system-area-pointer context-sap))
  (infinite-error-protect
   (let ((context (sap-alien context-sap (* os-context-t))))
     (error 'breakpoint-error
            :context context
            :address (sap-int (sb-vm:context-pc context))))))
