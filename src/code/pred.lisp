;;;; predicate functions (EQUAL and friends, and type predicates)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; miscellaneous non-primitive predicates

#-sb-fluid (declaim (inline streamp))
(defun streamp (stream)
  (typep stream 'stream))

;;; various (VECTOR FOO) type predicates, not implemented as simple
;;; widetag tests
(macrolet
    ((def ()
       `(progn
          ,@(loop for (name spec) in *vector-without-complex-typecode-infos*
                  collect `(defun ,name (x)
                             (or (typep x '(simple-array ,spec (*)))
                                 (and (complex-vector-p x)
                                      (do ((data (%array-data x) (%array-data data)))
                                          ((not (array-header-p data)) (typep data '(simple-array ,spec (*))))))))))))
  (def))

;;; Is X an extended sequence?
;; This is like the "hierarchical layout depths for other things"
;; case of the TYPEP transform (cf 'typetran'). Ideally it would
;; be preferable to share TYPEP's code rather than repeat it here.
(declaim (maybe-inline extended-sequence-p))
(defun extended-sequence-p (x)
  (let* ((slayout #.(info :type :compiler-layout 'sequence))
         (depthoid #.(layout-depthoid (info :type :compiler-layout 'sequence)))
         (layout
          ;; It is not an error to define a class that is both SEQUENCE and
          ;; FUNCALLABLE-INSTANCE with metaclass FUNCALLABLE-STANDARD-CLASS
          (cond ((%instancep x) (%instance-layout x))
                ((funcallable-instance-p x) (%funcallable-instance-layout x))
                (t (return-from extended-sequence-p nil)))))
    (when (layout-invalid layout)
      (setq layout (update-object-layout-or-invalid x slayout)))
    ;; It's _nearly_ impossible to create an instance which is exactly
    ;; of type SEQUENCE. To wit: (make-instance 'sequence) =>
    ;;   "Cannot allocate an instance of #<BUILT-IN-CLASS SEQUENCE>."
    ;; We should not need to check for that, just the 'inherits' vector.
    ;; However, bootstrap code does a sleazy thing, making an instance of
    ;; the abstract base type which is impossible for user code to do.
    ;; Preferably the prototype instance for SEQUENCE would be one that could
    ;; exist, so it would be a STANDARD-OBJECT and SEQUENCE. But it's not.
    ;; Hence we have to check for a layout that no code using the documented
    ;; sequence API would ever see, just to get the boundary case right.
    ;; Note also:
    ;; - Some builtins use a prototype object that is strictly deeper than
    ;;   layout of the named class because it is indeed the case that no
    ;;   object's layout can ever be EQ to that of the ancestor.
    ;;   e.g. a fixnum as representative of class REAL
    ;; - Some builtins actually fail (TYPEP (CLASS-PROTOTYPE X) X)
    ;;   but that's not an excuse for getting SEQUENCE wrong:
    ;;    (CLASS-PROTOTYPE (FIND-CLASS 'FLOAT)) => 42
    ;;    (CLASS-PROTOTYPE (FIND-CLASS 'VECTOR)) => 42
    ;;    (CLASS-PROTOTYPE (FIND-CLASS 'LIST)) => 42
    ;;    (CLASS-PROTOTYPE (FIND-CLASS 'STRING)) => 42
    (let ((inherits (layout-inherits (truly-the layout layout))))
      (declare (optimize (safety 0)))
      (eq (if (> (length inherits) depthoid) (svref inherits depthoid) layout)
          slayout))))

;;; Is X a SEQUENCE?  Harder than just (OR VECTOR LIST)
(defun sequencep (x)
  (declare (inline extended-sequence-p))
  (or (listp x) (vectorp x) (extended-sequence-p x)))

;;;; primitive predicates. These must be supported directly by the
;;;; compiler.

(defun not (object)
  "Return T if X is NIL, otherwise return NIL."
  (not object))

;;; All the primitive type predicate wrappers share a parallel form..
(macrolet ((def-type-predicate-wrapper (pred)
             (let* ((name (symbol-name pred))
                    (stem (string-left-trim "%" (string-right-trim "P-" name)))
                    (article (if (position (schar name 0) "AEIOU") "an" "a")))
               `(defun ,pred (object)
                  ;; Document the standardized predicates and not the internal ones.
                  ,@(when (eql (sb-xc:symbol-package pred) *cl-package*)
                      (list (format nil
                                    "Return true if OBJECT is ~A ~A, and NIL otherwise."
                                    article
                                    stem)))
                  ;; (falling through to low-level implementation)
                  (,pred object)))))
  (def-type-predicate-wrapper array-header-p)
  (def-type-predicate-wrapper simple-array-header-p)
  (def-type-predicate-wrapper arrayp)
  (def-type-predicate-wrapper atom)
  ;; Testing for BASE-CHAR-P is usually redundant on #-sb-unicode,
  ;; remove it there completely so that #-sb-unicode build will
  ;; break when it's used.
  #+sb-unicode (def-type-predicate-wrapper base-char-p)
  (def-type-predicate-wrapper base-string-p)
  #+sb-unicode (def-type-predicate-wrapper character-string-p)
  (def-type-predicate-wrapper bignump)
  (def-type-predicate-wrapper bit-vector-p)
  (def-type-predicate-wrapper characterp)
  (def-type-predicate-wrapper code-component-p)
  (def-type-predicate-wrapper consp)
  (def-type-predicate-wrapper compiled-function-p)
  (def-type-predicate-wrapper complexp)
  (def-type-predicate-wrapper complex-double-float-p)
  (def-type-predicate-wrapper complex-float-p)
  #+long-float (def-type-predicate-wrapper complex-long-float-p)
  (def-type-predicate-wrapper complex-rational-p)
  (def-type-predicate-wrapper complex-single-float-p)
  ;; (COMPLEX-VECTOR-P is not included here since it's awkward to express
  ;; the type it tests for in the Common Lisp type system, and since it's
  ;; only used in the implementation of a few specialized things.)
  (def-type-predicate-wrapper double-float-p)
  (def-type-predicate-wrapper extended-char-p)
  (def-type-predicate-wrapper fdefn-p)
  (def-type-predicate-wrapper fixnump)
  (def-type-predicate-wrapper floatp)
  (def-type-predicate-wrapper functionp)
  (def-type-predicate-wrapper integerp)
  (def-type-predicate-wrapper listp)
  (def-type-predicate-wrapper long-float-p)
  #-(or x86 x86-64) (def-type-predicate-wrapper lra-p)
  (def-type-predicate-wrapper null)
  (def-type-predicate-wrapper numberp)
  (def-type-predicate-wrapper rationalp)
  (def-type-predicate-wrapper ratiop)
  (def-type-predicate-wrapper realp)
  (def-type-predicate-wrapper short-float-p)
  (def-type-predicate-wrapper single-float-p)
  #+sb-simd-pack (def-type-predicate-wrapper simd-pack-p)
  #+sb-simd-pack-256 (def-type-predicate-wrapper simd-pack-256-p)
  (def-type-predicate-wrapper %instancep)
  (def-type-predicate-wrapper funcallable-instance-p)
  (def-type-predicate-wrapper symbolp)
  ;; The interpreter needs this because it assumes that any type spec
  ;; in SB-C::*BACKEND-TYPE-PREDICATES* has a callable predicate.
  (def-type-predicate-wrapper non-null-symbol-p)
  (def-type-predicate-wrapper %other-pointer-p)
  (def-type-predicate-wrapper system-area-pointer-p)
  (def-type-predicate-wrapper unbound-marker-p)
  (def-type-predicate-wrapper weak-pointer-p)
  #-64-bit
  (progn
    (def-type-predicate-wrapper unsigned-byte-32-p)
    (def-type-predicate-wrapper signed-byte-32-p))
  #+64-bit
  (progn
    (def-type-predicate-wrapper unsigned-byte-64-p)
    (def-type-predicate-wrapper signed-byte-64-p))
  ;; Specialized array types
  (macrolet ((saetp-defs ()
               `(progn
                  ,@(map 'list
                         (lambda (saetp)
                           `(def-type-predicate-wrapper
                                ,(symbolicate (sb-vm:saetp-primitive-type-name saetp) "-P")))
                         sb-vm:*specialized-array-element-type-properties*))))
    (saetp-defs))
  ;; Other array types
  (def-type-predicate-wrapper simple-array-p)
  (def-type-predicate-wrapper simple-rank-1-array-*-p)
  (def-type-predicate-wrapper simple-string-p)
  (def-type-predicate-wrapper stringp)
  (def-type-predicate-wrapper vectorp)
  (def-type-predicate-wrapper vector-nil-p))

#+(or x86 x86-64 arm arm64)
(defun fixnum-mod-p (x limit)
  (and (fixnump x)
       (<= 0 x limit)))

;;; a vector that maps widetags to layouts, used for quickly finding
;;; the layouts of built-in classes
(define-load-time-global **primitive-object-layouts** nil)
(declaim (type simple-vector **primitive-object-layouts**))
(defun !pred-cold-init ()
  ;; This vector is allocated in immobile space when possible. There isn't
  ;; a way to do that from lisp, so it's special-cased in genesis.
  #-immobile-space (setq **primitive-object-layouts** (make-array 256))
  (map-into **primitive-object-layouts**
            (lambda (name) (classoid-layout (find-classoid name)))
            #.(let ((table (make-array 256 :initial-element 'sb-kernel::random-class)))
                (dolist (x sb-kernel::+!built-in-classes+)
                  (destructuring-bind (name &key codes &allow-other-keys) x
                    (dolist (code codes)
                      (setf (svref table code) name))))
                (loop for i from sb-vm:list-pointer-lowtag by (* 2 sb-vm:n-word-bytes)
                      below 256
                      do (setf (aref table i) 'cons))
                (loop for i from sb-vm:even-fixnum-lowtag by (ash 1 sb-vm:n-fixnum-tag-bits)
                      below 256
                      do (setf (aref table i) 'fixnum))
                table)))

;;; Return the layout for an object. This is the basic operation for
;;; finding out the "type" of an object, and is used for generic
;;; function dispatch. The standard doesn't seem to say as much as it
;;; should about what this returns for built-in objects. For example,
;;; it seems that we must return NULL rather than LIST when X is NIL
;;; so that GF's can specialize on NULL.
;;; x86-64 has a vop that implements this without even needing to place
;;; the vector of layouts in the constant pool of the containing code.
#-(and compact-instance-header x86-64)
(progn
(declaim (inline layout-of))
(defun layout-of (x)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((%instancep x) (%instance-layout x))
        ((funcallable-instance-p x) (%funcallable-instance-layout x))
        ;; Compiler can dump literal layouts, which handily sidesteps
        ;; the question of when cold-init runs L-T-V forms.
        ((null x) #.(find-layout 'null))
        (t
         ;; Note that WIDETAG-OF is slightly suboptimal here and could be
         ;; improved - we've already ruled out some of the lowtags.
         (svref (load-time-value **primitive-object-layouts** t)
                (widetag-of x))))))

(declaim (inline classoid-of))
#-sb-xc-host
(defun classoid-of (object)
  "Return the class of the supplied object, which may be any Lisp object, not
   just a CLOS STANDARD-OBJECT."
  (layout-classoid (layout-of object)))

;;; Return the specifier for the type of object. This is not simply
;;; (TYPE-SPECIFIER (CTYPE-OF OBJECT)) because CTYPE-OF has different
;;; goals than TYPE-OF. In particular, speed is more important than
;;; precision here, and it is not permitted to return member types,
;;; negation, union, or intersection types.
(defun type-of (object)
  "Return the type of OBJECT."
  (declare (explicit-check))
  ;; We have special logic for everything except arrays.
  ;; Arrays use CTYPE-OF and then convert the answer to a specifier.
  (typecase object
    (fixnum
     (cond
       ((<= 0 object 1) 'bit)
       ((< object 0) 'fixnum)
       (t `(integer 0 ,sb-xc:most-positive-fixnum))))
    (integer
     (if (>= object 0)
         `(integer ,(1+ sb-xc:most-positive-fixnum))
         'bignum))
    (character
     (typecase object
       (standard-char 'standard-char)
       (base-char 'base-char)
       (extended-char 'extended-char)))
    ;; We "have to" (or have chosen to) pick off KEYWORD and BOOLEAN,
    ;; so we may as well have a branch that returns early for any SYMBOL
    ;; rather than falling into the CLASSOID-based test. But then since we
    ;; do that, we also have to pick off NIL so that it doesn't say SYMBOL.
    (symbol
     (cond ((eq object t) 'boolean)
           ((eq object nil) 'null)
           ((eq (sb-xc:symbol-package object) *keyword-package*) 'keyword)
           (t 'symbol)))
    ((or array complex #+sb-simd-pack simd-pack #+sb-simd-pack-256 simd-pack-256)
     (let ((sb-kernel::*unparse-allow-negation* nil))
       (declare (special sb-kernel::*unparse-allow-negation*)) ; forward ref
       (type-specifier (ctype-of object))))
    (t
     (let* ((classoid (classoid-of object))
            (name (classoid-name classoid)))
       (if (%instancep object)
           (case name
             (sb-alien-internals:alien-value
              `(alien
                ,(sb-alien-internals:unparse-alien-type
                  (sb-alien-internals:alien-value-type object))))
             (t
              (let ((pname (classoid-proper-name classoid)))
                (if (classoid-p pname)
                    (classoid-pcl-class pname)
                    pname))))
           name)))))

;;;; equality predicates

;;; This is real simple, 'cause the compiler takes care of it.
(defun eq (obj1 obj2)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq obj1 obj2))
;;; and this too, but it's only needed for backends on which
;;; IR1 might potentially transform EQL into %EQL/INTEGER.
#+integer-eql-vop
(defun %eql/integer (obj1 obj2)
  ;; This is just for constant folding, there's no real need to transform
  ;; into the %EQL/INTEGER VOP. But it's confusing if it becomes identical to
  ;; EQL, and then due to ICF we find that #'EQL => #<FUNCTION %EQL/INTEGER>.
  ;; Type declarations don't suffice because we don't know which arg is an integer
  ;; (if not both). We could ensure selection of the vop by using %PRIMITIVE.
  ;; Anyway it suffices to disable type checking and pretend its the always
  ;; the first arg that's an integer, but that won't work on the host because
  ;; it might enforce the type since we can't portably unenforce after declaring.
  #-sb-xc-host (declare (optimize (sb-c::type-check 0)))
  (eql #+sb-xc-host obj1 #-sb-xc-host (the integer obj1) obj2))

(declaim (inline %eql))
(defun %eql (obj1 obj2)
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  #+x86-64 (eql obj1 obj2) ; vop fully implements all cases of EQL
  #-x86-64 ; else this is the only full implementation of EQL
  (or (eq obj1 obj2)
      (if (or (typep obj2 'fixnum)
              (not (typep obj2 'number)))
          nil
          (macrolet ((foo (&rest stuff)
                       `(typecase obj2
                          ,@(mapcar (lambda (foo)
                                      (let ((type (car foo))
                                            (fn (cadr foo)))
                                        `(,type
                                          (and (typep obj1 ',type)
                                               (,fn obj1 obj2)))))
                                    stuff))))
            (foo
             (single-float eql)
             (double-float eql)
             #+long-float
             (long-float eql)
             (bignum
              #-integer-eql-vop (lambda (x y) (zerop (bignum-compare x y)))
              #+integer-eql-vop eql) ; will become %eql/integer
             (ratio
              (lambda (x y)
                (and (eql (numerator x) (numerator y))
                     (eql (denominator x) (denominator y)))))
             ((complex single-float)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y)))))
             ((complex double-float)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y)))))
             ((complex rational)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y))))))))))

(defun eql (x y)
  ;; On x86-64, EQL is just an interpreter stub for a vop.
  ;; For others it's a call to the implementation of generic EQL.
  (#+x86-64 eql #-x86-64 %eql x y))

(defun bit-vector-= (x y)
  (declare (type bit-vector x y))
  (cond ((eq x y))
        ((and (simple-bit-vector-p x)
              (simple-bit-vector-p y))
         (bit-vector-= x y))            ; DEFTRANSFORM
        (t
         (and (= (length x) (length y))
              (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                                        :check-fill-pointer t)
                (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                          :check-fill-pointer t)
                  (declare (ignore end-y))
                  (loop for x-i fixnum from start-x below end-x
                        for y-i fixnum from start-y
                        always (or (= (sbit x x-i)
                                      (sbit y y-i))))))))))

(defun equal (x y)
  "Return T if X and Y are EQL or if they are structured components whose
elements are EQUAL. Strings and bit-vectors are EQUAL if they are the same
length and have identical components. Other arrays must be EQ to be EQUAL."
  ;; Non-tail self-recursion implemented with a local auxiliary function
  ;; is a lot faster than doing it the straightforward way (at least
  ;; on x86oids) due to calling convention differences. -- JES, 2005-12-30
  (labels ((equal-aux (x y)
             (cond ((%eql x y)
                    t)
                   ((consp x)
                    (and (consp y)
                         (equal-aux (car x) (car y))
                         (equal-aux (cdr x) (cdr y))))
                   ((stringp x)
                    (and (stringp y) (string= x y)))
                   ((pathnamep x)
                    (and (pathnamep y) (pathname= x y)))
                   ((bit-vector-p x)
                    (and (bit-vector-p y)
                         (bit-vector-= x y)))
                   (t nil))))
    ;; Use MAYBE-INLINE to get the inline expansion only once (instead
    ;; of 200 times with INLINE). -- JES, 2005-12-30
    (declare (maybe-inline equal-aux))
    (equal-aux x y)))

;;; Like EQUAL, but any two gensyms whose names are STRING= are equalish.
(defun fun-names-equalish (x y)
  (named-let recurse ((x x) (y y))
    (cond ((eql x y) t) ; not performance-critical: don't inline %EQL here
          ((consp x) (and (consp y)
                          (recurse (car x) (car y))
                          (recurse (cdr x) (cdr y))))
          ((and (symbolp x) (not (sb-xc:symbol-package x)))
           (and (symbolp y) (not (sb-xc:symbol-package y)) (string= x y)))
          (t
           (equal x y)))))

;;; EQUALP comparison of HASH-TABLE values
(defun hash-table-equalp (x y)
  (declare (type hash-table x y))
  (or (eq x y)
      (and (hash-table-p y)
           (eql (hash-table-count x) (hash-table-count y))
           (eql (hash-table-test x) (hash-table-test y))
           (block comparison-of-entries
             (maphash (lambda (key x-value)
                        (multiple-value-bind (y-value y-value-p)
                            (gethash key y)
                          (unless (and y-value-p (equalp x-value y-value))
                            (return-from comparison-of-entries nil))))
                      x)
             t))))

(declaim (inline instance-equalp))
(defun instance-equalp (x y)
  (let ((layout-x (%instance-layout x)))
    (and
     (eq layout-x (%instance-layout y))
     (logtest +structure-layout-flag+ (layout-%bits layout-x))
     (macrolet ((slot-ref-equalp ()
                  `(let ((x-el (%instance-ref x i))
                         (y-el (%instance-ref y i)))
                     (or (eq x-el y-el) (equalp x-el y-el)))))
       (let ((n (%instance-length x)))
         (if (eql (layout-bitmap layout-x) sb-kernel:+layout-all-tagged+)
             (loop for i downfrom (1- n) to sb-vm:instance-data-start
                   always (slot-ref-equalp))
             (let ((comparators (layout-equalp-tests layout-x)))
               (unless (= (length comparators) (- n sb-vm:instance-data-start))
                 (bug "EQUALP got incomplete instance layout"))
               ;; See remark at the source code for %TARGET-DEFSTRUCT
               ;; explaining how to use the vector of comparators.
               (loop for i downfrom (1- n) to sb-vm:instance-data-start
                     for test = (data-vector-ref
                                 comparators (- i sb-vm:instance-data-start))
                     always (cond ((eql test 0) (slot-ref-equalp))
                                  ((functionp test) (funcall test i x y))
                                  (t))))))))))

;;; Doesn't work on simple vectors
(defun array-equalp (x y)
  (declare (array x y))
  (let ((rank (array-rank x)))
    (and
     (= rank (array-rank y))
     (dotimes (axis rank t)
       (unless (= (%array-dimension x axis)
                  (%array-dimension y axis))
         (return nil)))
     (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                               :array-header-p t)
       (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                 :array-header-p t)
         (declare (ignore end-y))
         (let* ((reffers %%data-vector-reffers%%)
                (getter-x (truly-the function (svref reffers (%other-pointer-widetag x))))
                (getter-y (truly-the function (svref reffers (%other-pointer-widetag y)))))
           (loop for x-i fixnum from start-x below end-x
                 for y-i fixnum from start-y
                 for x-el = (funcall getter-x x x-i)
                 for y-el = (funcall getter-y y y-i)
                 always (or (eq x-el y-el)
                            (equalp x-el y-el)))))))))

(defun vector-equalp (x y)
  (declare (vector x y))
  (let ((length (length x)))
    (and (= length (length y))
         (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                                   :check-fill-pointer t)
           (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                     :check-fill-pointer t)
             (declare (ignore end-y))
             (let* ((reffers %%data-vector-reffers%%)
                    (getter-x (truly-the function (svref reffers (%other-pointer-widetag x))))
                    (getter-y (truly-the function (svref reffers (%other-pointer-widetag y)))))
               (loop for x-i fixnum from start-x below end-x
                     for y-i fixnum from start-y
                     for x-el = (funcall getter-x x x-i)
                     for y-el = (funcall getter-y y y-i)
                     always (or (eq x-el y-el)
                                (equalp x-el y-el)))))))))

(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion. Characters may differ in alphabetic case. Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction. The elements of structured components
  are compared with EQUALP."
  (cond ((eq x y) t)
        ((characterp x) (and (characterp y) (char-equal x y)))
        ((numberp x) (and (numberp y) (= x y)))
        ((consp x)
         (and (consp y)
              (equalp (car x) (car y))
              (equalp (cdr x) (cdr y))))
        ((pathnamep x)
         (and (pathnamep y) (pathname= x y)))
        ((hash-table-p x)
         (and (hash-table-p y)
              (hash-table-equalp x y)))
        ((%instancep x)
         (and (%instancep y)
              (instance-equalp x y)))
        ((and (simple-vector-p x) (simple-vector-p y))
         (let ((len (length x)))
           (and (= len (length y))
                (loop for i below len ; somewhat faster than the generic loop
                      always (let ((a (svref x i)) (b (svref y i)))
                               (or (eq a b) (equalp a b)))))))
        ((and (bit-vector-p x) (bit-vector-p y))
         (bit-vector-= x y))
        ((vectorp x)
         (and (vectorp y)
              (vector-equalp x y)))
        ((arrayp x)
         (and (arrayp y)
              (array-equalp x y)))
        (t nil)))

(let ((test-cases `(($0.0 $-0.0 t)
                    ($0.0 $1.0 nil)
                    ;; There is no cross-compiler #C reader macro.
                    ;; SB-XC:COMPLEX does not want uncanonical input, i.e. imagpart
                    ;; of rational 0 which downgrades the result to just an integer.
                    (1 ,(complex $1.0 $0.0) t)
                    (,(complex 0 1) ,(complex $0.0 $1.0) t)
                    ;; 11/10 is unequal to real 1.1 due to roundoff error.
                    ;; COMPLEX here is a red herring
                    (,(complex $1.1 $0.0) 11/10 nil)
                    ("Hello" "hello" t)
                    ("Hello" #(#\h #\E #\l #\l #\o) t)
                    ("Hello" "goodbye" nil))))
  (dolist (test-case test-cases)
    (destructuring-bind (x y expected-result) test-case
      (let* ((result (equalp x y))
             (bresult (if result 1 0))
             (expected-bresult (if expected-result 1 0)))
        (unless (= bresult expected-bresult)
          (error "failed test (EQUALP ~S ~S)" x y))))))
