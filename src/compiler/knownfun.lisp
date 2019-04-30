;;;; This file contains stuff for maintaining a database of special
;;;; information about functions known to the compiler. This includes
;;;; semantic information such as side effects and type inference
;;;; functions as well as transforms and IR2 translators.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(/show0 "knownfun.lisp 17")

;;;; interfaces to defining macros

;;; an IR1 transform
(defstruct (transform (:copier nil))
  ;; the function type which enables this transform.
  ;;
  ;; (Note that declaring this :TYPE FUN-TYPE probably wouldn't
  ;; work because some function types, like (SPECIFIER-TYPE 'FUNCTION0
  ;; itself, are represented as BUILT-IN-TYPE, and at least as of
  ;; sbcl-0.pre7.54 or so, that's inconsistent with being a
  ;; FUN-TYPE.)
  (type (missing-arg) :type ctype)
  ;; the transformation function. Takes the COMBINATION node and
  ;; returns a lambda expression, or throws out.
  (function (missing-arg) :type function)
  ;; string used in efficiency notes
  (note (missing-arg) :type string)
  ;; T if we should emit a failure note even if SPEED=INHIBIT-WARNINGS.
  (important nil :type (member nil :slightly t))
  ;; A function with NODE as an argument that checks wheteher the
  ;; transform applies in its policy.
  ;; It used to be checked in the FUNCTION body but it would produce
  ;; notes about failed transformation due to types even though it
  ;; wouldn't have been applied with the right types anyway,
  ;; or if another transform could be applied with the right policy.
  (policy nil :type (or null function))
  (extra-type nil))

(defprinter (transform) type note important)

;;; Grab the FUN-INFO and enter the function, replacing any old
;;; one with the same type and note.
(defun %deftransform (name type fun &optional note important policy)
  (let* ((ctype (specifier-type type))
         (note (or note "optimize"))
         (info (fun-info-or-lose name))
         (old (find ctype (fun-info-transforms info)
                    :test #'type=
                    :key #'transform-type)))
    (cond (old
           (style-warn 'redefinition-with-deftransform
                       :transform old)
           (setf (transform-function old) fun
                 (transform-note old) note
                 (transform-important old) important
                 (transform-policy old) policy))
          (t
           (push (make-transform :type ctype :function fun :note note
                                 :important important
                                 :policy policy)
                 (fun-info-transforms info))))
    name))

;;; Make a FUN-INFO structure with the specified type, attributes
;;; and optimizers.
(defun %defknown (names type attributes location
                  &key derive-type optimizer result-arg
                       overwrite-fndb-silently
                       call-type-deriver
                       annotation)
  (let* ((ctype (specifier-type type))
         (type-to-store (if (contains-unknown-type-p ctype)
                            ;; unparse it, so SFUNCTION -> FUNCTION
                            (type-specifier ctype)
                            ctype)))
    (dolist (name names)
      (unless overwrite-fndb-silently
        (let ((old-fun-info (info :function :info name)))
          (when old-fun-info
            ;; This is handled as an error because it's generally a bad
            ;; thing to blow away all the old optimization stuff. It's
            ;; also a potential source of sneaky bugs:
            ;;    DEFKNOWN FOO
            ;;    DEFTRANSFORM FOO
            ;;    DEFKNOWN FOO ; possibly hidden inside some macroexpansion
            ;;    ; Now the DEFTRANSFORM doesn't exist in the target Lisp.
            ;; However, it's continuable because it might be useful to do
            ;; it when testing new optimization stuff interactively.
            (cerror "Go ahead, overwrite it."
                    "~@<overwriting old FUN-INFO ~2I~_~S ~I~_for ~S~:>"
                    old-fun-info name))))
      (setf (info :function :type name) type-to-store)
      (setf (info :function :where-from name) :declared)
      (setf (info :function :kind name) :function)
      (setf (info :function :info name)
            (make-fun-info :attributes attributes
                           :derive-type derive-type
                           :optimizer optimizer
                           :result-arg result-arg
                           :call-type-deriver call-type-deriver
                           :annotation annotation))
      (if location
          (setf (getf (info :source-location :declaration name) 'defknown)
                location)
          (remf (info :source-location :declaration name) 'defknown))))
  names)


;;; This macro should be the way that all implementation independent
;;; information about functions is made known to the compiler.
;;;
;;; FIXME: The comment above suggests that perhaps some of my added
;;; FTYPE declarations are in poor taste. Should I change my
;;; declarations, or change the comment, or what?
;;;
;;; FIXME: DEFKNOWN is needed only at build-the-system time. Figure
;;; out some way to keep it from appearing in the target system.
;;;
;;; Declare the function NAME to be a known function. We construct a
;;; type specifier for the function by wrapping (FUNCTION ...) around
;;; the ARG-TYPES and RESULT-TYPE. ATTRIBUTES is an unevaluated list
;;; of boolean attributes of the function. See their description in
;;; (!DEF-BOOLEAN-ATTRIBUTE IR1). NAME may also be a list of names, in
;;; which case the same information is given to all the names. The
;;; keywords specify the initial values for various optimizers that
;;; the function might have.
(defmacro defknown (name arg-types result-type &optional (attributes '(any))
                    &body keys)
  #-sb-xc-host
  (when (member 'unsafe attributes)
    (style-warn "Ignoring legacy attribute UNSAFE. Replaced by its inverse: DX-SAFE.")
    (setf attributes (remove 'unsafe attributes)))
  (when (and (intersection attributes '(any call unwind))
             (intersection attributes '(movable)))
    (error "function cannot have both good and bad attributes: ~S" attributes))

  (when (member 'any attributes)
    (setq attributes (union '(unwind) attributes)))
  (when (member 'flushable attributes)
    (pushnew 'unsafely-flushable attributes))
  (multiple-value-bind (type annotation)
      (split-type-info arg-types result-type)
    `(%defknown ',(if (and (consp name)
                           (not (legal-fun-name-p name)))
                      name
                      (list name))
                ',type
                (ir1-attributes ,@attributes)
                (source-location)
                :annotation ,annotation
                ,@keys)))

(defstruct (fun-type-annotation
            (:copier nil))
  positional ;; required and &optional
  rest
  key
  returns)

(defun split-type-info (arg-types result-type)
  (if (eq arg-types '*)
      `(sfunction ,arg-types ,result-type)
      (multiple-value-bind (llks required optional rest keys)
          (parse-lambda-list
           arg-types
           :context :function-type
           :accept (lambda-list-keyword-mask
                    '(&optional &rest &key &allow-other-keys))
           :silent t)
        (let ((i -1)
              positional-annotation
              rest-annotation
              key-annotation
              return-annotation)
          (labels ((annotation-p (x)
                     (typep x '(or (cons (member function function-designator modifying
                                          inhibit-flushing))
                                (member type-specifier proper-sequence proper-list
                                 proper-or-dotted-list proper-or-circular-list))))
                   (strip-annotation (x)
                     (if (consp x)
                         (ecase (car x)
                           ((function function-designator) (car x))
                           ((modifying inhibit-flushing) (cadr x)))
                         (case x
                           (proper-sequence 'sequence)
                           ((proper-list proper-or-dotted-list proper-or-circular-list) 'list)
                           (t x))))
                   (process-positional (type)
                     (incf i)
                     (cond ((annotation-p type)
                            (push (cons i (ensure-list type)) positional-annotation)
                            (strip-annotation type))
                           (t
                            type)))
                   (process-key (pair)
                     (cond ((annotation-p (cadr pair))
                            (destructuring-bind (key value) pair
                              (setf (getf key-annotation key) (ensure-list value))
                              (list key (strip-annotation value))))
                           (t
                            pair)))
                   (process-rest (type)
                     (cond ((annotation-p type)
                            (setf rest-annotation (ensure-list type))
                            (strip-annotation type))
                           (t
                            type)))
                   (process-return (type)
                     (cond ((annotation-p type)
                            (setf return-annotation (ensure-list type))
                            (strip-annotation type))
                           (t
                            type))))
            (let ((required (mapcar #'process-positional required))
                  (optional (mapcar #'process-positional optional))
                  (rest (process-rest (car rest)))
                  (key (mapcar #'process-key keys))
                  (return (process-return result-type)))
              (values
               `(sfunction
                 (,@required
                  ,@(and optional `(&optional ,@optional))
                  ,@(and (ll-kwds-restp llks) `(&rest ,rest))
                  ,@(and (ll-kwds-keyp llks) `(&key ,@key))
                  ,@(and (ll-kwds-allowp llks) '(&allow-other-keys)))
                 ,return)
               (when (or positional-annotation rest-annotation
                         key-annotation return-annotation)
                 `(make-fun-type-annotation :positional ',positional-annotation
                                            :rest ',rest-annotation
                                            :key ',key-annotation
                                            :returns ',return-annotation)))))))))

;;; Return the FUN-INFO for NAME or die trying.
(declaim (ftype (sfunction (t) fun-info) fun-info-or-lose))
(defun fun-info-or-lose (name)
  (or (info :function :info name) (error "~S is not a known function." name)))

;;;; generic type inference methods

(defun symeval-derive-type (node &aux (args (basic-combination-args node))
                                      (lvar (pop args)))
  (unless (and lvar (endp args))
    (return-from symeval-derive-type))
  (if (constant-lvar-p lvar)
      (let* ((sym (lvar-value lvar))
             (var (maybe-find-free-var sym))
             (local-type (when var
                           (let ((*lexenv* (node-lexenv node)))
                             (lexenv-find var type-restrictions))))
             (global-type (info :variable :type sym)))
        (if local-type
            (type-intersection local-type global-type)
            global-type))
      *universal-type*))

;;; Derive the type to be the type of the xxx'th arg. This can normally
;;; only be done when the result value is that argument.
(defun result-type-first-arg (call)
  (declare (type combination call))
  (let ((lvar (first (combination-args call))))
    (when lvar (lvar-type lvar))))
(defun result-type-last-arg (call)
  (declare (type combination call))
  (let ((lvar (car (last (combination-args call)))))
    (when lvar (lvar-type lvar))))

;;; Derive the result type according to the float contagion rules, but
;;; always return a float. This is used for irrational functions that
;;; preserve realness of their arguments.
(defun result-type-float-contagion (call)
  (declare (type combination call))
  (reduce #'numeric-contagion (combination-args call)
          :key #'lvar-type
          :initial-value (specifier-type 'single-float)))

(defun simplify-list-type (type &key preserve-dimensions)
  ;; Preserve all the list types without dragging
  ;; (cons (eql 10)) stuff in.
  (let ((cons-type (specifier-type 'cons))
        (list-type (specifier-type 'list))
        (null-type (specifier-type 'null)))
    (cond ((and preserve-dimensions
                (csubtypep type cons-type))
           cons-type)
          ((and preserve-dimensions
                (csubtypep type null-type))
           null-type)
          ((csubtypep type list-type)
           list-type))))

;;; Return a closure usable as a derive-type method for accessing the
;;; N'th argument. If arg is a list, result is a list. If arg is a
;;; vector, result is a vector with the same element type.
(defun sequence-result-nth-arg (n &key preserve-dimensions
                                       preserve-vector-type)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when lvar
        (let ((type (lvar-type lvar)))
          (cond ((simplify-list-type type
                                     :preserve-dimensions preserve-dimensions))
                ((not (csubtypep type (specifier-type 'vector)))
                 nil)
                (preserve-vector-type
                 type)
                (t
                 (let ((simplified (simplify-vector-type type)))
                   (if (and preserve-dimensions
                            (csubtypep simplified (specifier-type 'simple-array)))
                       (type-intersection (specifier-type
                                           `(simple-array * ,(ctype-array-dimensions type)))
                                          simplified)
                       simplified)))))))))

;;; Derive the type to be the type specifier which is the Nth arg.
(defun result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (careful-specifier-type (lvar-value lvar))))))

;;; Derive the type to be the type specifier which is the Nth arg,
;;; with the additional restriptions noted in the CLHS for STRING and
;;; SIMPLE-STRING, defined to specialize on CHARACTER, and for VECTOR
;;; (under the page for MAKE-SEQUENCE).
;;; At present this is used to derive the output type of CONCATENATE,
;;; MAKE-SEQUENCE, and MERGE. Two things seem slightly amiss:
;;; 1. The sequence type actually produced might not be exactly that specified.
;;;    (TYPE-OF (MAKE-SEQUENCE '(AND (NOT SIMPLE-ARRAY) (VECTOR BIT)) 9))
;;;    => (SIMPLE-BIT-VECTOR 9)
;;; 2. Because we *know* that a hairy array won't be produced,
;;;    why does derivation preserve the non-simpleness, if so specified?
(defun creation-result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (let* ((specifier (lvar-value lvar))
               (lspecifier (if (atom specifier) (list specifier) specifier)))
          (cond
            ((eq (car lspecifier) 'string)
             (destructuring-bind (string &rest size)
                 lspecifier
               (declare (ignore string))
               (careful-specifier-type
                `(vector character ,@(when size size)))))
            ((eq (car lspecifier) 'simple-string)
             (destructuring-bind (simple-string &rest size)
                 lspecifier
               (declare (ignore simple-string))
               (careful-specifier-type
                `(simple-array character ,@(if size (list size) '((*)))))))
            (t
             (let ((ctype (careful-specifier-type specifier)))
               (cond ((not (array-type-p ctype))
                      ctype)
                     ((unknown-type-p (array-type-element-type ctype))
                      (make-array-type (array-type-dimensions ctype)
                                       :complexp (array-type-complexp ctype)
                                       :element-type *wild-type*
                                       :specialized-element-type *wild-type*))
                     ((eq (array-type-specialized-element-type ctype)
                          *wild-type*)
                      (make-array-type (array-type-dimensions ctype)
                                       :complexp (array-type-complexp ctype)
                                       :element-type *universal-type*
                                       :specialized-element-type *universal-type*))
                     (t
                      ctype))))))))))

(defun read-elt-type-deriver (skip-arg-p element-type-spec no-hang)
  (lambda (call)
    (let* ((element-type (specifier-type element-type-spec))
           (null-type (specifier-type 'null))
           (err-args (if skip-arg-p ; for PEEK-CHAR, skip 'peek-type' + 'stream'
                         (cddr (combination-args call))
                         (cdr (combination-args call)))) ; else just 'stream'
           (eof-error-p (first err-args))
           (eof-value (second err-args))
           (unexceptional-type ; the normally returned thing
            (if (and eof-error-p
                     (types-equal-or-intersect (lvar-type eof-error-p)
                                               null-type))
                ;; (READ-elt stream nil <x>) returns (OR (EQL <x>) elt-type)
                (type-union (if eof-value (lvar-type eof-value) null-type)
                            element-type)
                ;; If eof-error is unsupplied, or was but couldn't be nil
                element-type)))
      (if no-hang
          (type-union unexceptional-type null-type)
          unexceptional-type))))

;;; Return MAX MIN
(defun sequence-lvar-dimensions (lvar)
  (if (constant-lvar-p lvar)
      (let ((value (lvar-value lvar)))
        (and (proper-sequence-p value)
             (let ((length (length value)))
               (values length length))))
      (let ((max 0) (min sb-xc:array-total-size-limit))
        (block nil
          (labels ((max-dim (type)
                     ;; This can deal with just enough hair to handle type STRING,
                     ;; but might be made to use GENERIC-ABSTRACT-TYPE-FUNCTION
                     ;; if we really want to be more clever.
                     (typecase type
                       (union-type
                        (mapc #'max-dim (union-type-types type)))
                       (array-type (if (array-type-complexp type)
                                       (return '*)
                                       (process-dim (array-type-dimensions type))))
                       (t (return '*))))
                   (process-dim (dim)
                     (let ((length (car dim)))
                       (if (and (singleton-p dim)
                                (integerp length))
                           (setf max (max max length)
                                 min (min min length))
                           (return '*)))))
            ;; If type derivation were able to notice that non-simple arrays can
            ;; be mutated (changing the type), we could safely use LVAR-TYPE on
            ;; any vector type. But it doesn't notice.
            ;; We could use LVAR-CONSERVATIVE-TYPE to get a conservative answer.
            ;; However that's probably not an important use, so the above
            ;; logic restricts itself to simple arrays.
            (max-dim (lvar-type lvar))
            (values max min))))))

(defun position-derive-type (call)
  (let ((dim (sequence-lvar-dimensions (second (combination-args call)))))
    (when (integerp dim)
      (specifier-type `(or (integer 0 (,dim)) null)))))

(defun count-derive-type (call)
  (let ((dim (sequence-lvar-dimensions (second (combination-args call)))))
    (when (integerp dim)
      (specifier-type `(integer 0 ,dim)))))

;;; This used to be done in DEFOPTIMIZER DERIVE-TYPE, but
;;; ASSERT-CALL-TYPE already asserts the ARRAY type, so it gets an extra
;;; assertion that may not get eliminated and requires extra work.
(defun array-call-type-deriver (call trusted &optional set row-major-aref)
  (let* ((fun (combination-fun call))
         (type (lvar-fun-type fun))
         (policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call)))
    (when (fun-type-p type)
      (flet ((assert-type (arg type &optional set index)
               (when (cond (index
                            (assert-array-index-lvar-type arg type policy))
                           (t
                            (when set
                              (add-annotation arg
                                              (make-lvar-modified-annotation :caller (lvar-fun-name fun))))
                            (assert-lvar-type arg type policy)))
                 (unless trusted (reoptimize-lvar arg)))))
        (let ((required (fun-type-required type)))
          (when set
            (assert-type (pop args)
                         (pop required)))
          (assert-type (pop args)
                       (if row-major-aref
                           (pop required)
                           (type-intersection
                            (pop required)
                            (specifier-type `(array * ,(length args)))))
                       set)
          (loop for type in required
                do
                (assert-type (pop args) type nil (or (not (and set row-major-aref))
                                                     args)))
          (loop for type in (fun-type-optional type)
                do (assert-type (pop args) type nil t))
          (loop for subscript in args
                do (assert-type subscript (fun-type-rest type) nil t)))))))

(defun append-call-type-deriver (call trusted)
  (let* ((policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call))
         (list-type (specifier-type 'list)))
    ;; All but the last argument should be proper lists
    (loop for (arg next) on args
          while next
          do
          (add-annotation
           arg
           (make-lvar-proper-sequence-annotation
            :kind 'proper-list))
          (when (and (assert-lvar-type arg list-type policy)
                     (not trusted))
            (reoptimize-lvar arg)))))
