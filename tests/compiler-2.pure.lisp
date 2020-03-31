;;;; various compiler tests without side effects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;;; This file of tests was added because the tests in 'compiler.pure.lisp'
;;;; are a total hodgepodge- there is often no hugely compelling reason for
;;;; their being tests of the compiler per se, such as whether
;;;; INPUT-ERROR-IN-COMPILED-FILE is a subclass of SERIOUS-CONDITION;
;;;; in addition to which it is near impossible to wade through the
;;;; ton of nameless, slow, and noisy tests.

;;;; This file strives to do better on all fronts:
;;;; the tests should be fast, named, and not noisy.

(enable-test-parallelism)

(defun compiles-with-warning (lambda)
  (assert (nth-value 2 (checked-compile lambda :allow-warnings t))))

(with-test (:name (position :derive-type))
  (checked-compile '(lambda (x)
                      (ash 1 (position (the (member a b c) x) #(a b c )))))
  (checked-compile '(lambda (x)
                      (ash 1 (position x #(a b c ))))
                   :allow-style-warnings t)
  ;; The sequence must contain a mixture of symbols and non-symbols
  ;; to call %FIND-POSITION. If only symbols, it makes no calls.
  (let ((f (checked-compile '(lambda (x)
                              (position x '(1 2 3 a b c 4 5 6 d e f g))))))
    ;; test should be EQ, not EQL
    (assert (or (find (symbol-function 'eq)
                      (ctu:find-code-constants f :type 'sb-kernel:simple-fun))
                (ctu:find-named-callees f :name 'eq))))
  (checked-compile-and-assert ()
      '(lambda (x)
        (position x '(a b c d e d c b a) :from-end t))
    (('a) 8)
    (('b) 7)))

(with-test (:name (ldb :recognize-local-macros))
  ;; Should not call %LDB
  (assert (not (ctu:find-named-callees
                (checked-compile
                 '(lambda (x)
                   (declare (optimize speed))
                   (macrolet ((b () '(byte 2 2)))
                     (ldb (b) (the fixnum x)))))))))

(with-test (:name (dpb :eval-order :lp-1458190))
  (sb-int:collect ((calls))
    (flet ((f (new old)
             (dpb (progn (calls 'eval-new) new)
                  (progn (calls 'eval-byte) (byte 10 10))
                  (progn (calls 'eval-old) old))))
      (f 20 0)
      (assert (equal (calls)
                     '(eval-new eval-byte eval-old))))))

;; Best practice treats TRULY-THE as a special operator, not a macro,
;; in a context such as (DPB X (TRULY-THE SB-KERNEL:BYTE-SPECIFIER ...) Y).
;; DPB used to expand its second argument using MACROEXPAND and lose
;; the nuance of TRULY-THE. Strictly speaking, byte-specifier is not a
;; type specifier that users are supposed to know about, so portable code
;; should not care, but this might affect internal code.
(with-test (:name (dpb :inner-macro))
  (flet ((source-xform (sexpr)
           (funcall (sb-int:info :function :source-transform (car sexpr))
                    sexpr (sb-kernel:make-null-lexenv))))
    (assert (equal-mod-gensyms
             (source-xform
              '(dpb (new) (truly-the sb-kernel:byte-specifier bspec) (old)))
             '(let ((new (new))
                    (byte (truly-the sb-kernel:byte-specifier bspec)))
               (sb-kernel:%dpb new (byte-size byte) (byte-position byte)
                               (old)))))))

(with-test (:name :inline-satisfies-predicate)
  ;; If we remove the indirections in these functions,
  ;; this test should visibly break so that we can write a new test
  ;; that asserts that inlining F works in (THE (SATISFIES F) obj).
  (assert (equal (sb-ext:typexpand 'sb-impl::function-name)
                 '(satisfies sb-int:legal-fun-name-p)))
  (let ((f (checked-compile '(lambda (x) (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:valid-function-name-p))
                   (ctu:find-named-callees f))))
  (let ((f (checked-compile '(lambda (x)
                               (declare (notinline sb-int:legal-fun-name-p))
                               (the sb-impl::function-name x)))))
    (assert (equal (list (symbol-function 'sb-int:legal-fun-name-p))
                   (ctu:find-named-callees f)))))

(with-test (:name (make-array :untestable-type :no-warning))
  (checked-compile `(lambda () (make-array '(2 2)
                                           :element-type `(satisfies foofa)))))

(with-test (:name (make-array nil :no-warning))
  (checked-compile '(lambda () (make-array '(2 2) :element-type nil))))

(with-test (:name (nth-value :huge-n :works))
  (flet ((return-a-ton-of-values ()
           (values-list (loop for i below 5000 collect i))))
    (assert (= (nth-value 1 (return-a-ton-of-values)) 1))
    (assert (= (nth-value 4000 (return-a-ton-of-values)) 4000))))

(with-test (:name :internal-name-p :skipped-on :sb-xref-for-internals)
  (assert (sb-c::internal-name-p 'sb-int:neq)))

(with-test (:name (:coerce-callable-to-fun :note))
  (flet ((try (form what)
           (multiple-value-bind (fun failure-p warnings style-warnings notes)
               (checked-compile `(lambda (x)
                                  (declare (optimize speed))
                                  (funcall ,form)))
             (declare (ignore fun failure-p warnings style-warnings))
             (assert (search (format nil "~A is not known to be" what)
                             (princ-to-string (first notes)))))))

    (try '(eval `(work-with ,x)) "callable expression")
    (try 'x "X")
    ;; For this I'd accept either Z or X in the message.
    (try '(progn (let ((z x)) (identity z))) "X")))

(with-test (:name (princ-to-string :unflushable))
  ;; Ordinary we'll flush it
  (let ((f (checked-compile '(lambda (x) (princ-to-string x) x))))
    (assert (not (ctu:find-named-callees f :name 'princ-to-string))))
  ;; But in high safety it should be called for effect
  (let ((f (checked-compile '(lambda (x)
                               (declare (optimize safety)) (princ-to-string x) x))))
    (assert (ctu:find-named-callees f :name 'princ-to-string))))

(with-test (:name :space-bounds-no-consing
            :serial t
            :skipped-on :interpreter)
  ;; Asking for the size of a heap space should not cost anything!
  (ctu:assert-no-consing (sb-vm:%space-bounds :static))
  (ctu:assert-no-consing (sb-vm:space-bytes :static)))

(with-test (:name (sb-vm:map-allocated-objects :no-consing)
            :serial t
            :fails-on :cheneygc
            :skipped-on :interpreter)
  (let ((n 0))
    (sb-int:dx-flet ((f (obj type size)
                       (declare (ignore obj type size))
                       (incf n)))
      (ctu:assert-no-consing
       (sb-vm:map-allocated-objects #'f :dynamic)
       5))))

(with-test (:name :pack-varints-as-bignum)
  (dotimes (i 500) ; do some random testing this many times
    (let* ((random-numbers (loop repeat (+ (random 20) 3)
                                 collect (1+ (random 4000))))
           (test-list (sort (delete-duplicates random-numbers) #'<))
           (packed-int (sb-c::pack-code-fixup-locs test-list nil))
           (result (make-array 1 :element-type 'sb-ext:word)))
      ;; The packer intrinsically self-checks the packing
      ;; so we don't need to assert anything about that.
      (sb-sys:with-pinned-objects (packed-int result)
        ;; Now exercise the C unpacker.
        ;; This hack of allocating 4 longs is terrible, but whatever.
        (let ((unpacker (make-alien long 4))
              (prev-loc 0))
          (alien-funcall (extern-alien "varint_unpacker_init"
                                       (function void (* long) unsigned))
                         unpacker
                         (sb-kernel:get-lisp-obj-address packed-int))
          (sb-int:collect ((unpacked))
            (loop
             (let ((status
                    (alien-funcall
                     (extern-alien "varint_unpack"
                                   (function int (* long) system-area-pointer))
                     unpacker (sb-sys:vector-sap result))))
               (let ((val (aref result 0)))
                 ;; status of 0 is EOF, val = 0 means a decoded value was 0,
                 ;; which can't happen, so it's effectively EOF.
                 (when (or (eql status 0) (eql val 0)) (return))
                 (let ((loc (+ prev-loc val)))
                   (unpacked loc)
                   (setq prev-loc loc)))))
            (assert (equal (unpacked) test-list))))))))

(with-test (:name (symbol-value symbol-global-value :quoted-constant))
  (let ((f (checked-compile '(lambda () (symbol-value 'char-code-limit)))))
    (assert (not (ctu:find-code-constants f :type 'symbol))))
  (let ((f (checked-compile '(lambda () (symbol-global-value 'char-code-limit)))))
    (assert (not (ctu:find-code-constants f :type 'symbol)))))

(with-test (:name (:set symbol-value :of defglobal))
  (let ((s 'sb-c::*recognized-declarations*))
    (assert (eq (sb-int:info :variable :kind s) :global)) ; verify precondition
    (let ((f (checked-compile `(lambda () (setf (symbol-value ',s) nil)))))
      ;; Should not have a call to SET-SYMBOL-GLOBAL-VALUE>
      (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn))))))

(with-test (:name :layout-constants
                  :skipped-on (not (and :x86-64 :immobile-space)))
  (let ((addr-of-pathname-layout
         (write-to-string
          (sb-kernel:get-lisp-obj-address
           (sb-kernel:find-layout 'hash-table))
          :base 16 :radix t))
        (count 0))
    ;; The constant should appear in two CMP instructions
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (let ((sb-disassem:*disassem-location-column-width* 0))
                       (disassemble 'hash-table-p :stream s)))
                   #\newline))
      (when (and (search "CMP" line) (search addr-of-pathname-layout line))
        (incf count)))
    (assert (= count 2))))

;; Whoever deals with the Alpha and/or HPPA ports can make this test pass for them
(with-test (:name :linkage-table-bogosity)
  (let ((strings (map 'list (lambda (x) (if (consp x) (car x) x))
                      sb-vm::+required-foreign-symbols+)))
    (assert (= (length (remove-duplicates strings :test 'string=))
               (length strings)))))

(with-test (:name (:no style-warning :for inline :cl-fun))
  (checked-compile '(lambda (x)
                      (declare (optimize (speed 3)) (inline length)
                      (muffle-conditions compiler-note))
                      (length x))))

(with-test (:name :deleted-return-use)
  (checked-compile-and-assert ()
      `(lambda ()
         (block nil
           (return 345)
           (let ((a (catch 'x)))
             (flet ((%f (a &optional b)
                      a))
               (%f 0 (%f 123))))))
    (() 345)))

(with-test (:name :shift-right-transform-nil-type)
  (checked-compile-and-assert (:optimize nil)
      `(lambda (b c)
         (declare (type (integer -10 -6) c)
                  (optimize (debug 2)))
         (catch 'c
           (flet ((f1 (a &optional (b (shiftf b 0)) c d)
                    (declare (ignore a b c d))
                    (throw 'c 780)))
             (flet ((f2 (a b)
                      (f1 a b 0)))
               (ash
                (f1 (if t
                        c
                        (f1 (f2 1 0) 0))
                    b)
                (+ c))))))
      ((-3 -7) 780)))

(with-test (:name :move-lvar-result-through-unused-cast)
  (checked-compile-and-assert (:optimize nil)
      `(lambda ()
         (declare (optimize (debug 0)))
         (labels ((f (a b)
                    a b)
                  (x ()
                    (apply #'f (list 2 3))))
           (declare (notinline f))
           (the integer (x)))
         132)
    (() 132)))

(with-test (:name (:type-conflict funcall :external-lambda))
  (compiles-with-warning `(lambda ()
                            (let ((x (lambda (x) (declare (fixnum x)) x)))
                              (funcall x 'a)))))

(with-test (:name (:type-conflict :callable :external-lambda))
  (compiles-with-warning `(lambda ()
                            (let ((x (lambda (x) (declare (fixnum x)) x)))
                              (find-if x "abca")))))

(with-test (:name (:type-conflict map :result-type))
  (compiles-with-warning `(lambda (str)
                            (map 'string (lambda (x) (declare (ignore x)) nil)
                                 str))))

(with-test (:name (:type-conflict :by-name))
  (compiles-with-warning `(lambda (str)
                            (map 'string 'evenp str))))

(with-test (:name (:type-conflict :callable :reporting))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda (x) (map-into (make-string 10) #'evenp x))
                       :allow-warnings 'warning)
    (declare (ignore fun))
    (assert failure-p)
    (assert (= (length warnings) 1))
    (search "Derived type of EVENP is"
            (princ-to-string (first warnings)))))

(with-test (:name (:type-conflict string :union-type))
  (compiles-with-warning `(lambda (x)
                            (find-if #'evenp (the string x)))))

(with-test (:name (:type-conflict map-into :let))
  (compiles-with-warning `(lambda (z)
                            (let ((x "abc"))
                              (map-into z #'evenp x)))))

(with-test (:name (:type-conflict map-into :result))
  (compiles-with-warning `(lambda (z)
                            (map-into (make-string 10) #'evenp z))))

(with-test (:name (:type-conflict complement))
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (z)
                         (find z "l" :test (complement #'=)))
                      :allow-style-warnings t))))

(with-test (:name :type-across-hairy-lambda-transforms)
  (assert (subtypep (sb-kernel:%simple-fun-type
                     (checked-compile `(lambda (x) (find 1 (the vector x)))))
                    '(function * (values (or (integer 1 1) null) &optional)))))

(with-test (:name :lea-type-derivation)
  (checked-compile-and-assert ()
      `(lambda (b)
         (declare ((integer -3755795408964870057 -3391381516052960895)
                   b))
         (ldb (byte 22 10) (* b 9)))
    ((-3391381516052980893) 2826685)))

(with-test (:name (:unused &optional :and &key))
  (checked-compile-and-assert (:allow-style-warnings t)
      `(lambda (&optional x &key)
         (declare (ignore x))
         10)
    (() 10)))

(with-test (:name (:unknown values :coercion))
  (checked-compile-and-assert ()
      `(lambda (a)
         (declare (notinline values typep))
         (the integer (values a 2305843009213693946 a -207)))
    ((123) (values 123 2305843009213693946 123 -207))))

(with-test (:name :deleted-block-during-generate-type-checks)
  (checked-compile-and-assert (:allow-warnings t)
      `(lambda (a b)
         (declare (notinline min ash conjugate oddp >=))
         (if (and (or t (>= a)) (oddp 0))
             (prog2 0
                 0
               (labels ((f (a b c &key)
                          (declare (ignore a b c))
                          6965670824543402))
                 (f a 0 b)))
             (conjugate
              (dotimes (i 0 0)
                (catch 'c
                  (ash
                   (the integer
                        (ignore-errors
                          (ignore-errors (throw 'c 1))))
                   (min a)))))))
    ((1 2) 0)))

(with-test (:name :block-delete-twice)
  (checked-compile-and-assert ()
      `(lambda ()
         (declare (notinline >=))
         (block nil
           (lambda (x &key (key (if (>= 0 1)
                                    (return (catch 'ct5 0)))))
             (declare (ignore key))
             x)))
    (() 123 :test (lambda (values expected)
                    (equal (multiple-value-list
                            (funcall (first values) (first expected)))
                           expected)))))

(with-test (:name :dead-lvars-and-stack-analysis)
  (checked-compile-and-assert ()
    `(lambda (b)
       (catch 'ct2
         (block b5
           (return-from b5
             (multiple-value-prog1 19
               (if (or b t)
                   (return-from b5 333)))))))
    ((11) 333)))

(with-test (:name :mv-call-more-values)
  (checked-compile-and-assert ()
    `(lambda (z)
       (multiple-value-call (lambda (&optional x y &rest args)
                              (declare (ignore args))
                              (+ y x))
         2 (truncate z 30)))
    ((2345) 80)))

(with-test (:name :unused-casts-at-ir2-convert)
  (checked-compile-and-assert ()
    `(lambda ()
       (unwind-protect 123
         (the integer
              (labels ((%f (x &key)
                         (declare (ignore x))
                         (svref #(46 32) 0)))
                (unwind-protect (%f (%f 0)))))))
    (() 123)))

(with-test (:name :cmov-constants-different-primitive-type)
  (checked-compile-and-assert ()
    `(lambda (b)
       (case b
         ((2030) 4611686018427387908)
         ((572) b)
         (t 0)))
    ((572) 572)
    ((123) 0)
    ((2030) 4611686018427387908)))

(with-test (:name :mv-bind-skipping-vars-on-reoptimize)
  (checked-compile-and-assert ()
    `(lambda ()
       (let (lv1)
         (apply (lambda (&rest args)
                  (declare (ignore args)))
                0
                (list 3 lv1))
         (setf lv1 10)))
    (() 10)))

(with-test (:name :transform-on-a-nil-arg)
  (checked-compile-and-assert ()
   `(lambda ()
      (block nil
        (logtest
         (multiple-value-prog1
             (unwind-protect (return 32))
           (catch 'tag (return 33)))
         1)
        34))
   (() 32)))

(with-test (:name :nesteted-dx-deleted-uses)
  (checked-compile-and-assert ()
    `(lambda (a)
       (block b2
         (let* ((v1 (make-array nil :initial-element
                                (let ((a a))
                                  (return-from b2 a)))))
           (declare (dynamic-extent v1))
           (aref v1))))
    ((342) 342)))

(with-test (:name :deleted-during-locall-analyze-fun-1)
  (checked-compile-and-assert (:allow-warnings t)
    `(lambda ()
       (flet ((a ()))
         (a 1)
         (a 2)))
    (() (condition 'program-error))))

(with-test (:name :delete-return-without-flush-dest)
  (assert (eql
           (catch 'c
             (funcall (checked-compile
                       '(lambda ()
                         (labels ((%f () 40))
                           (multiple-value-prog1 *
                             (throw 'c (%f))
                             (%f)
                             30))))))
           40)))

(with-test (:name :let-conversion-inside-deleted-lambda.1)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (catch 'c)
         (flet ((f (x &key)
                  (when x
                    (progv '(*) '(0)
                      (return)))))
           (f (return 123))
           (f 0))))
    (() 123)))

(with-test (:name :let-conversion-inside-deleted-lambda.2)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (block nil
           (lambda () (return)))
         (labels ((l () (l))
                  (%f (a &key)
                    (l)
                    (return a)))
           (%f (return 321))
           (%f 1))))
    (() 321)))

(with-test (:name :unconvert-tail-calls)
  (checked-compile-and-assert ()
    `(lambda ()
       (block nil
         (labels ((f (&optional (a (return))
                                (b (if t (return)))
                                c
                      &rest args)
                    (declare (ignore a b c args))
                    (return 0)))
           (let (x)
             (equal 10 (f 0 3))
             (f 123 0 0)
             (f 0)
             x))))
    (() 0)))

(with-test (:name :deleting-exits-with-multiple-users)
  (checked-compile-and-assert ()
    `(lambda (a b)
       (block nil
         (multiple-value-prog1 b
           (tagbody (return (multiple-value-prog1 3
                              (if a (go z)))) z))))
    ((nil :good) 3)
    ((t :good) :good)))

(with-test (:name :merge-tail-sets-deleted-functional)
  (checked-compile-and-assert ()
    `(lambda (a)
       (block nil
         (tagbody
            (go g549)
          g549
            (return-from nil
              (block b3
                (let ((x (progn (lambda (&optional (x a)) x)
                                (unwind-protect 10)
                                (return-from b3 a))))
                  (unwind-protect x)))))))
    ((321) 321)))

(with-test (:name :interval-div-zero)
  (checked-compile-and-assert (:optimize :safe)
    `(lambda (x y)
       (truncate (the (integer 0 0) x)
                 (the (rational (1) (2)) y)))
   ((0 3/2) (values 0 0))))

(with-test (:name :float-remainders-rounding-errors)
  (loop for fun in '(ceiling truncate floor
                     fceiling ftruncate ffloor
                     round fround)
        do
        (assert (member (second
                         (third (sb-kernel:%simple-fun-type
                                 (checked-compile
                                  `(lambda (x)
                                     (nth-value 1 (,fun (the double-float x) 1/2)))))))
                        '(double-float real)))))

(with-test (:name :float-quotient-rounding-errors)
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (floor -114658225103614 84619.58))
    (() (values -1354984705 8473228.0)))
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (floor -302254842 50510.5))
    (() (eval '(floor -302254842 50510.5))))
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (ceiling 114658225103614 84619.58))
    (() (values 1354984705 -8473228.0)))
  (checked-compile-and-assert (:optimize :safe)
   `(lambda ()
      (ceiling 285493348393 94189.93))
    (() (values 3031039 0.0))))

(with-test (:name :complex-float-contagion)
  (checked-compile-and-assert ()
    `(lambda (p1)
       (declare (type (or double-float integer) p1))
       (complex p1 2.0))
    ((1d0) #c(1d0 2d0))))

(with-test (:name :equal-transform-member-types)
  (let* ((s1 "abc")
         (s2 (copy-seq s1)))
    (checked-compile-and-assert ()
      `(lambda (p1 p2)
         (declare (type (member ,s1) p1)
                  (type (member ,s2 #*10) p2))
         (equal p1 p2))
      ((s1 s2) t))))

(with-test (:name :equalp-transform-numeric-types)
  (checked-compile-and-assert ()
    `(lambda (p1 p2)
       (declare (type (or fixnum list) p1)
                (type double-float p2))
       (equalp p1 p2))
    ((1 1d0) t)))

(with-test (:name :equalp-transform-zero-array)
  (checked-compile-and-assert ()
    `(lambda (a b)
       (declare (simple-string a)
                (simple-bit-vector b))
       (equalp a b))
    (("" #*) t)))

(with-test (:name :equalp-transform-zero-string)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (equalp "" a))
   ((#*) t)
   ((#()) t)))

(with-test (:name :fill-transform-returning-array-data)
  (let ((vector (make-array 10 :fill-pointer 2)))
    (checked-compile-and-assert ()
      `(lambda (v)
           (declare (type (vector t) v))
           (fill v nil))
      ((vector) vector))))

(with-test (:name :missing-error-context)
  (flet ((run ()
          (let ((string
                 (with-output-to-string (*error-output*)
                   (compile nil '(sb-int:named-lambda bob () (otherfun) 3)))))
            (assert (search "in: SB-INT:NAMED-LAMBDA BOB" string)))))
    (run)
    ;; Unrepeatability is confusing:
    ;; The first compiler invocation used to leave *last-format-string*
    ;; with a toplevel value, so the second would not print enough context
    ;; because the format control and args were the same.
    (run)))

(with-test (:name :cast-deletion-notes)
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (m)
         (setf m (list 1 2 3))
         (the simple-vector
              (coerce m 'vector)))
    ((nil) #(1 2 3) :test #'equalp)))

(with-test (:name :cast-deletion-notes.2)
  (multiple-value-bind (fun fail warn style notes)
      (checked-compile
       `(lambda (m)
          (setf m (list 1 2 3))
          (the simple-vector
               (if (vectorp m)
                   m
                   #(1)))))
    (declare (ignore fail warn style))
    (assert (equalp (funcall fun nil)
                    #(1)))
    (assert (= (length notes) 1))
    (assert (typep (car notes) 'code-deletion-note))))

(with-test (:name :array-call-type-deriver)
  (checked-compile-and-assert
      ()
      `(lambda (vector)
         (funcall (the (function (t t)) #'aref)
                  vector
                  0))
    (((vector 333)) 333)))

(with-test (:name :function-designator-cast-removal)
  (let ((fun (checked-compile
              `(lambda (vectors x)
                 (declare (list vectors x))
                 (map 'list #'svref vectors x)))))
    (assert (notany (lambda (c)
                      (typecase c
                        (sb-kernel:fdefn
                         (eq (sb-c::fdefn-name c) 'svref))
                        (function
                         (eq c #'svref))))
                    (ctu:find-code-constants fun)))
    (assert (equal (funcall fun '(#(44)) '(0)) '(44)))))

(with-test (:name :zombie-casts)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (flet ((f (a b)
                  (declare (ignore a))
                  b))
           (multiple-value-call #'f
             (values (the integer (unwind-protect (f 10 20)))
                     322))))
    (() 322)))

(with-test (:name :zombie-casts.2)
  (let ((sb-c::*max-optimize-iterations* 1))
    (checked-compile-and-assert
        ()
        `(lambda (a b)
           (declare (type fixnum a b))
           (elt '(167992664 119771479)
                (max 0
                     (catch 'ct2
                       (if (typep b '(integer -52))
                           a
                           0)))))
      ((1 2) 119771479))))


(with-test (:name :find-dfo-on-deleted-lambda)
  (assert (= (funcall
              (funcall (checked-compile
                        `(lambda ()
                           (declare (notinline <))
                           (block nil
                             (lambda (&key (key
                                            (unwind-protect
                                                 (if (< 0)
                                                     34
                                                     (return (catch 'c))))))
                               key))))))
             34)))

(with-test (:name :ir1-ir2-dead-code-consistency)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (loop for x below 2
               count (zerop (min x x x x x x x x x x))))
    (() 1)))

(with-test (:name :ir1-ir2-dead-code-consistency)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (loop for x below 2
               count (zerop (min x x x x x x x x x x))))
    (() 1)))

(with-test (:name (setf svref :constant-modification))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (setf (svref #(a b c) 1) x))
                          :allow-warnings 'sb-int:constant-modified)))
            1)))

(with-test (:name (debug :constant-modification))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (declare (optimize (debug 2)))
                             (let ((m "abc"))
                               (delete x m)))
                          :allow-warnings 'sb-int:constant-modified)))
      1)))

(with-test (:name (debug :constant-modification.2))
  (assert
   (= (length (nth-value 2
                         (checked-compile
                          `(lambda (x)
                             (declare (optimize (debug 2)))
                             (let ((m (if x
                                          "abc"
                                          "fgh")))
                               (delete x m)))
                          :allow-warnings 'sb-int:constant-modified)))
      1)))

(with-test (:name (debug :unused-tn-long-arglist))
  (checked-compile-and-assert
      ()
      `(lambda (n x)
         (declare (sb-vm:word n))
         (log (float n))
         (nth-value 33 (funcall x . #.(loop for i to 35 collect i))))
    ((10 (lambda (&rest args) (values-list args))) 33)))

(with-test (:name (debug :unused-tn-very-long-arglist))
  (checked-compile-and-assert
      ()
      `(lambda (n x)
         (declare (sb-vm:word n))
         (log (float n))
         (nth-value 33 (funcall x . #.(loop for i to 350 collect i))))
    ((10 (lambda (&rest args) (values-list args))) 33)))

(with-test (:name (dynamic-extent :recursive-local-functions))
  (checked-compile
   `(lambda ()
      (let ((s (labels ((%f () (%f)))
                 (%f))))
        (declare (dynamic-extent s))
        (car s)))))

(with-test (:name (:ctypep :hairy-types))
  (checked-compile
   `(lambda ()
      (the (cons (satisfies error)) '("a"))))
  (assert
   (nth-value 3
              (checked-compile
               `(lambda () (the (array abc) #()))
               :allow-style-warnings t))))

(with-test (:name (catch :evaluate-tag-before-%catch))
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (z)
         (catch (multiple-value-call #'+
                  (if z 1 (values 1 2)))
           :done))
    ((t) :done)
      ((nil) :done)))

(with-test (:name :fewer-cast-conversions)
  (multiple-value-bind (fun failed)
      (checked-compile
       `(lambda ()
          (let* ((v (cons 0 (catch 'ct (the integer nil)))))
            (declare (dynamic-extent v))
            (flet ((%f (x) x))
              (%f (cdr v)))))
       :allow-warnings t)
    (assert failed)
    (handler-bind ((error (lambda (c) c (throw 'ct 33))))
      (assert (= (funcall fun) 33)))))

(with-test (:name :constant-folding-with-callable-args)
  (checked-compile '(lambda () (count #'%f '(a)))
                   :allow-style-warnings t))

(with-test (:name :flushable-with-callable-args)
  (let ((fun (checked-compile '(lambda (y) (let ((x (count y '(1 2 3))))
                                             (declare (ignore x)))))))
    (assert (not (ctu:find-named-callees fun)))))

(with-test (:name (remove :count))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (remove x "aaa" :count 2))
   ((#\a) "a"))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (remove-if (lambda (y) (eql y x)) "aaa" :count 2))
   ((#\a) "a")))

(with-test (:name (:constant-fold :allow-other-keys))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (reduce #'+ '(1 2 3)  :allow-other-keys t :bad x))
   ((1) 6)))

(with-test (:name (:constant-fold :allow-other-keys.2))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (reduce #'+ '(1 2 3)  :allow-other-keys x))
   ((1) 6)))

(with-test (:name (:constant-fold :repeat-keys))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (member nil '(1 2 3) :key #'evenp :key x))
   ((1) '(1 2 3) :test #'equal)))



(with-test (:name :function-and-instance-primitive-type)
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (declare (function f))
         (the standard-object f)
         (funcall f #'list t))
    ((#'documentation) (documentation #'list t))))

(with-test (:name :mv-call-safety-0)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (flet ((%f1 (x y) (+ x y)))
           (apply #'%f1 a (list 0))))
    ((3) 3)))

(with-test (:name :cast-type-check-external)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (declare (notinline +))
         (gcd
          (loop for lv2 below 1
                count (logbitp 0
                               (if x
                                   (return x)
                                   1)))
          0))
    ((334) 334)))

(with-test (:name :flush-combination-non-fun-type)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (rassoc-if-not #'values '((1 . a)) :allow-other-keys t)
         1)
    (() 1)))

(with-test (:name :symeval-nil)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (sb-kernel:symeval nil))
    (() nil)))

(with-test (:name (:physenv-analyze :deleted-lambda))
  (checked-compile-and-assert
      ()
      `(lambda (log)
         (loop for str in nil
               for i from 0
               do
               (ignore-errors (format log ""))))
    ((t) nil)))

(with-test (:name (:ensure-lvar-fun-form :lvar-uses))
  (checked-compile-and-assert
      ()
      `(lambda (op) (funcall (case op (equal '=) (t '=)) 1 2))
    (('equal) nil)
    ((t) nil)))

(with-test (:name :substitute-let-funargs-during-find-initial-dfo)
  (checked-compile
   `(lambda ()
      (labels ((%r (f)
                 (loop)
                 (%r f)))
        (%r (lambda ()))))))

(with-test (:name :split-ir2-blocks-cmov)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((v (list 0)))
           (if (block nil
                 (eq v (cdr v)))
               1
               2)))
    (() 2)))

(with-test (:name :=-rational-complex-rational-fold)
  (let ((fun (checked-compile '(lambda (x)
                                (declare ((complex integer) x))
                                (= x 10))))
        (fun2 (checked-compile '(lambda (x)
                                (declare ((complex rational) x))
                                (= x 10d0)))))
    (assert (equal (sb-kernel:%simple-fun-type fun)
                   '(function ((complex integer)) (values null &optional))))
    (assert (not (funcall fun #C(10 10))))
    (assert (equal (sb-kernel:%simple-fun-type fun2)
                   '(function ((complex rational)) (values null &optional))))
    (assert (not (funcall fun2 #C(10 10))))))

(with-test (:name :find-type-deriver)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (find 1 x :key #'values))
    (('(1)) 1)))

(with-test (:name :tail-call-ltn-annotation)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (labels ((ff1 ()
                    (multiple-value-call #'print
                      (if x
                          (values t t)
                          nil))
                    (ff1)))
           (identity (ff1))))))

(with-test (:name (:substitute-lvar-uses :deleted-code-and-dx-lvars))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (let ((v (values
                                   (the integer
                                        (flet ((%f5 (x) x))
                                          (%f5)))
                                   (unwind-protect 1))))
                           (declare (dynamic-extent v))
                           v))
                      :allow-warnings t))))

(with-test (:name (restart-case :declaration-processing))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (restart-case (list)
           (my-restart (x) "foo" "bar" x)))
    (() ()))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (restart-case (list)
           (my-restart () (declare))))
    (() ())))

(with-test (:name (handler-case :declaration-processing))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (handler-case (list 1 2) (error (e) "foo" "bar" e)))
    (() '(1 2)))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (handler-case (declare)))
                      :allow-failure t))))

(with-test (:name (:unconvert-tail-calls :deleted-call))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (labels ((%f (&optional (x (* 2 nil (%f)))) x))
                          (%f)
                          (%f 1)))
                      :allow-warnings t))))

(with-test (:name (:equal-transform :nil-types))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (loop for y below 3
                              count (or
                                     (not (or (>= y y) (equal y -787357528)))
                                     (the integer (or (>= y y) (equal y -787357528))))))
                      :allow-warnings t))))



(with-test (:name (:delete-recursive-optional))
  (checked-compile '(lambda (x)
                     (lambda ()
                       (labels ((f (&optional a) (values x a #'f))))))))

(with-test (:name (:combination-args-flow-cleanly-p :unused-result))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((v (flet ((%f (x)
                           (list x)
                           (list 1)))
                    (%f 2))))
           (declare (dynamic-extent v))
           (car v)))
    (() 1)))

(with-test (:name (:delete-ref :maintain-lambda-calls-or-closes))
  (checked-compile `(lambda (c y)
                      (labels ((f1 ()
                                 (if y
                                     (f3 2)))
                               (l () (loop))
                               (f2 ()
                                 (l)
                                 (f3 3))
                               (f3 (x)
                                 (f3 x))
                               (f4 ()
                                 (f1)
                                 (f2)))
                        (f4)
                        c))))

(with-test (:name (the :nil-type))
  (checked-compile
   `(lambda ()
      (flet ((f () (the nil 0)))
        (oddp (f))))))

(with-test (:name :concatenate-transform-hairy-type)
  (checked-compile
      '(lambda (x)
        (concatenate '(and string (satisfies eval)) x))))

(with-test (:name :make-array-transform-deletion-notes)
  (checked-compile
   `(lambda (vector)
      (let* ((length (length vector))
             (new (make-array length :adjustable t
                                     :fill-pointer length)))
        new))
   :allow-notes nil))

(with-test (:name :ltn-analyze-cast-unlink)
  (assert (nth-value 1 (checked-compile
                        `(lambda (n)
                           (* 2 n)
                           (let ((p (make-array n :element-type 'double-float)))
                             (dotimes (i n)
                               (setf (aref p i)
                                     (ignore-errors i)))))
                        :allow-warnings t))))

(with-test (:name :call-type-validation)
  (checked-compile
   `(lambda ()
      (funcall (the (or cons function) *debugger-hook*)))))

(with-test (:name :setf-schar-hairy-types)
  (checked-compile-and-assert
      ()
      `(lambda (s v)
         (setf (schar (the (satisfies eval) s) 0) v)
         s)
    (((copy-seq "abc") #\m) "mbc" :test #'equal)))

(with-test (:name :check-function-designator-cast-key-lambda-var)
  (checked-compile-and-assert
      (:optimize '(:speed 3 :space 0))
      `(lambda (p1 p4)
         (declare (vector p1)
                  ((member ,#'car "x" cdr) p4))
         (stable-sort p1 #'<= :key p4))
    (((vector '(2) '(3) '(1)) #'car) #((1) (2) (3)) :test #'equalp)))

(with-test (:name :replace-zero-elements)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (declare ((simple-vector 2) x))
        (replace x x :start1 2))
    (((vector 1 2)) #(1 2) :test #'equalp))
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (replace x x :start1 2))
    (((vector 1 2)) #(1 2) :test #'equalp)))

(with-test (:name :error-in-xep)
  (checked-compile-and-assert
      (:optimize :safe)
      '(lambda (x)
        (declare (type (satisfies error) x))
        x)
    (("") (condition 'error))))

(with-test (:name :lifetime-analyze-tn-overflow-unused-tns)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (multiple-value-bind (a b c)
          (funcall x 1 2 3 ,@(make-list 58))
        (declare (ignore b))
        (values a c)))
   ((#'values) (values 1 3))))

(with-test (:name :constraints-not-enough-args)
  (checked-compile-and-assert
   ()
   `(lambda (list)
      (delete-if #'> (the list list)))
   (((list 1)) nil)))

(with-test (:name :%coerce-callable-for-call-removal-order-mv-call)
  (checked-compile-and-assert
      ()
      `(lambda (fun args)
         (loop
          (let ((result (apply fun args)))
            (when result
              (return result))
            (setf args result))))
    (('list '(1)) '(1) :test #'equal)))

(with-test (:name :constraint-loop)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (check-type a list)
         (when a
           (mapcar #'identity a)
           (loop for c from 0 do (loop for d in b do
                                       (loop for e in a)))))))

(with-test (:name :primitive-type-fun-designator)
  (checked-compile-and-assert
      ()
      `(lambda  (fun)
         (map 'vector fun '(1 2 3)))
    (('1+) #(2 3 4) :test #'equalp)))

(with-test (:name :mv-call-lambda-type-derivation)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (multiple-value-call
                  (lambda () 133)
                (funcall x)))))
          '(function (t) (values (integer 133 133) &optional)))))

(with-test (:name :mv-call-lambda-type-derivation.closure)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (multiple-value-call
                  (lambda () (print x) 133)
                (funcall x)))))
          '(function (t) (values (integer 133 133) &optional)))))

(with-test (:name :constant-folding-and-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda ()
        (> 0 (the (satisfies eval) (- 1))))
    (() t)))

(with-test (:name :type-approximate-interval-and-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (declare (fixnum x))
        (<= (the (satisfies eval) 65) x))
    ((66) t)))

(with-test (:name :remove-equivalent-blocks-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (c)
         (declare (integer c))
         (= (case c
              ((-10) (abs c))
              (t c))
            -1))
    ((-1) t)))

(with-test (:name :typep-singleton-intersect-types)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (keywordp t))
    (() nil)))

(with-test (:name :constants-and-cmp)
  (checked-compile-and-assert
      ()
      '(lambda (l)
        (declare (fixnum l))
        (let ((v 0))
          (labels ((change ()
                     (setf v 10)
                     #'change))
            (> v l))))
    ((1) nil))
  (checked-compile-and-assert
      ()
      '(lambda (l)
        (declare (fixnum l))
        (let ((v 0))
          (labels ((change ()
                     (setf v 10)
                     #'change))
            (> l v))))
    ((1) t)))

(with-test (:name :inlining-and-substituted-block-lvars)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((z (block nil
                    (labels ((f (x)
                               (return x)))
                      (declare (inline f))
                      (funcall (the function #'f) t)
                      (funcall (the function #'f) t)))))
           (and z
                1)))
    (() 1)))

(with-test (:name :inlining-reanlyzing-optionals)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((w (x)
                    x
                    #'s)
                  (fun2 (f x)
                    (funcall f x))
                  (s (&optional x)
                    (fun2 #'w x)))
           (declare (inline w))
           (s)))))

(with-test (:name :vector-fill/t-fast-safe)
  (let ((sb-c::*policy-min* sb-c::*policy-min*))
    (sb-ext:restrict-compiler-policy 'safety 1)
    (checked-compile-and-assert
     ()
     '(lambda ()
       (make-array 2 :initial-element 10))
     (() #(10 10) :test #'equalp))))

(with-test (:name :deleted-tail-sets)
  (checked-compile-and-assert
   ()
   '(lambda ()
     (labels ((f (&optional (a (catch t 6))
                            (b (error ""))
                            (c (unwind-protect 1)))
                (+ a b c)))
       (unwind-protect (f 4))))
   (() (condition 'error))))

;;; The SLEEP source transform barfed on float positive infinity
;;; values.
(with-test (:name (compile sleep float :infinity :lp-1754081))
  (checked-compile '(lambda () (sleep single-float-positive-infinity)))
  (checked-compile '(lambda () (sleep double-float-positive-infinity))))
;;; And it didn't work at all after the fix for aforementioned
(with-test (:name :sleep-float-transform
                  :skipped-on (and :win32 (not :sb-thread)))
  (let* ((xform (car (sb-c::fun-info-transforms (sb-int:info :function :info 'sleep))))
         (type (car (sb-kernel:fun-type-required (sb-c::transform-type xform)))))
    (assert (sb-kernel:constant-type-p type))
    ;; CONSTANT-TYPE isn't actually testable through CTYPEP.
    ;; So pull out the actual type as the compiler would do.
    (assert (sb-kernel:ctypep 1.5 (sb-kernel:constant-type-type type)))))

(with-test (:name :atanh-type-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (atanh (coerce x '(double-float * (0.0d0)))))))

(with-test (:name :ir1-optimize-combination-unknown-keys)
  (checked-compile-and-assert
      ()
      '(lambda (p x y)
        (let ((f (when p #'string-equal)))
          (when f
            (funcall f "a" "b" x y))))
    ((t :start1 0) nil)))

(with-test (:name :member-transform)
  (let ((list '(2 1 3)))
    (checked-compile-and-assert
     ()
     '(lambda (list &key key)
       (member 1 list :key key))
     ((list) (cdr list)))))

(with-test (:name :note-no-stack-allocation-casts)
  (checked-compile-and-assert
   ()
   `(lambda ()
     (let ((*s* (the integer (catch 'ct1 0))))
       (declare (dynamic-extent *s*)
                (special *s*))))))

(with-test (:name :dxify-downward-funargs-variable-name)
  (checked-compile-and-assert
      ()
      '(lambda () ((lambda (map) (funcall map)) #'list))))

(with-test (:name :dxify-downward-funargs-malformed)
  (checked-compile
      '(lambda () (sb-debug::map-backtrace))
      :allow-style-warnings t))

(with-test (:name :dxify-downward-funargs-casts)
  (checked-compile-and-assert
   ()
   '(lambda (f x)
     (flet ((f (y) (funcall f y)))
       (funcall (the (satisfies eval) #'every) #'f x)))
   ((#'evenp '(2 2 4)) t)))

(with-test (:name :array-call-type-deriver-non-fun-type)
  (checked-compile-and-assert
      ()
      '(lambda (x) (funcall (the compiled-function #'aref) x))
    ((#0A123) 123)))

(with-test (:name :nth-&rest-overflow)
  (checked-compile-and-assert
      ()
      '(lambda (&rest s) (nth 536870908 s))
    (() nil)))


(with-test (:name :array-in-bounds-p-transform-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda ()
        (let ((a (the (satisfies eval) (make-array 4 :fill-pointer 0))))
          (and (array-in-bounds-p a 0)
               (array-in-bounds-p a 1))))
    (() t)))

(with-test (:name :array-type-dimensions-or-give-up-hairy-types)
  (checked-compile-and-assert
      ()
      '(lambda (a i)
        (declare ((or (array * (1)) (satisfies eval)) a))
        (array-row-major-index a i))
    ((#(a b) 1) 1)))

(with-test (:name :array-type-dimensions-0-rank)
  (checked-compile-and-assert
      ()
      '(lambda (p1)
        (declare ((or (array bit 1) (array * 0)) p1))
        (array-total-size p1))
    ((#0a3) 1)))

(with-test (:name :type-derivation-hairy-types)
  (checked-compile-and-assert
   ()
   `(lambda (n s)
      (declare (fixnum n))
      (ash (the (satisfies eval) n)
           (the (integer * 0) s)))
   ((1234 -4) 77))
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (declare (type (member #c(0.5d0 4.0d0) #c(0 -1)) p))
      (/ (the (satisfies eval) p)))
   ((#c(0 -1)) #C(0 1))))

(with-test (:name :assert-lvar-type-intersection)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (write-sequence nil (the standard-object x) y nil))))

(with-test (:name :or-bignum-single-float-no-notes
            :skipped-on (not (or :arm64 ppc :x86 :x86-64)))
  (checked-compile
   '(lambda (x) (declare (optimize speed)) (typep x '(or bignum single-float)))
   :allow-notes nil))


(with-test (:name :vertices-best-color/general-default-value)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (declare ((simple-array (complex double-float)) a))
         (* (aref a 0)
            (let ((z (aref a 0)))
              (complex (realpart z) (imagpart z)))))))

(with-test (:name :copy-list-inlined)
  (let ((f (checked-compile
            `(lambda (x) (declare (optimize speed)) (copy-list x)))))
    ;; Should not have a call to COPY-LIST (or anything)
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn)))))

(with-test (:name :move-from-fixnum+-1)
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (x)
      (declare (fixnum x))
      (1- x))
   ((0) -1)
   ((most-positive-fixnum) (1- most-positive-fixnum))
   ((most-negative-fixnum) (1- most-negative-fixnum)))
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (x)
      (declare (fixnum x))
      (1+ x))
   ((0) 1)
   ((most-positive-fixnum) (1+ most-positive-fixnum))
   ((most-negative-fixnum) (1+ most-negative-fixnum)))
  (checked-compile-and-assert
   (:allow-notes nil)
   `(lambda (a x)
      (declare (fixnum x))
      (if a
          10
          (1+ x)))
   ((nil 0) 1)
   ((t 0) 10)
   ((nil most-positive-fixnum) (1+ most-positive-fixnum))
   ((nil most-negative-fixnum) (1+ most-negative-fixnum))))

(with-test (:name :coalesce-more-ltn-numbers-constants)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (list 1 1 ,@(make-list 100 :initial-element 'x)))
    ((1) (make-list 102 :initial-element 1) :test #'equal)))

(with-test (:name (:lambda-var-ref-lvar :multiple-refs))
  (checked-compile-and-assert
   ()
   `(lambda (vector index)
      (labels ((update (index)
                 (let ((old (svref vector index)))
                   (if (eq old 10)
                       (update index)
                       old)))
               (wrap (index)
                 (update index)))
        (wrap index)))
   ((#(1 2 3) 1) 2)))

(with-test (:name :string-type-unparsing)
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (declare (type (string 1) s))
         (the (or simple-array (member 1/2 "ba" 0 #\3)) s))
    ((#1="a") #1#)))

(with-test (:name :primitive-type-function)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (funcall (the (and atom (not null)) x))
         )
    ((#'list) nil)
    (('list) nil)))

(with-test (:name :copyprop-sc-mismatch-between-moves
            :serial t
            :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (f x)
              (let ((x (the double-float x)))
                (values (funcall f x) (> x 1d0)))))))
    (ctu:assert-no-consing (funcall f #'identity 1d0))))

(with-test (:name :infer-iteration-var-type)
  (let ((f (checked-compile
            '(lambda (s)
              (declare ((integer 1 2) s))
              (let ((r 16))
                (loop for i from 16 below 32 by s
                      do (setf r i))
                r)))))
    (assert (equal (sb-impl::%simple-fun-type f)
                   '(function ((integer 1 2)) (values (integer 16 31) &optional))))))

(with-test (:name :delay-transform-until-constraint-loop)
  (checked-compile-and-assert
      ()
      `(lambda (str)
         (declare (string str))
         (when (plusp (length str))
           (make-array (1- (length str))
                       :element-type (array-element-type str)
                       :displaced-to str)))
    (("abc") "ab" :test #'equal)))

(with-test (:name :lambda-var-ref-lvar-loop)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (labels ((z (a)
                    (when (>= 0 (the integer a))
                      (values #'z a))))))
    (() nil)))

(with-test (:name :vector-length-fill-pointer-type-derivation)
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (= (length (the (string 1) s)) 1))
    (((make-array 1 :element-type 'character :fill-pointer 0)) nil)))

(with-test (:name :function-designator-loop)
  (checked-compile-and-assert
      ()
      `(lambda (p1 p3 p4)
         (declare (type (or (eql #.#'oddp)
                            (satisfies eval)) p1))
         (find-if-not p1 nil p3 p4))
       ((#'oddp :from-end t) nil)))

(with-test (:name :lvar-constants-nested-funs)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (labels ((f (x &optional result)
                    (if x
                        (f x result)
                        (nreverse result))))
           (f x)))
      ((nil) nil)))

(with-test (:name :nested-indirect-var-fp-coalescence)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (let ((x 1)
            (j 2))
        (labels ((m ()
                   (incf x 32)
                   (incf j 44)
                   (let ((z 1))
                     (labels ((m ()
                                (incf x 32)
                                (incf z)))
                       (declare (notinline m))
                       (m)
                       (incf j z)))))
          (declare (notinline m))
          (m)
          (values x j))))
    (() (values 65 48))))

(with-test (:name :non-returning-functions-conflict)
  (checked-compile-and-assert
   ()
   `(lambda (x) (map nil #'error x))
    ((nil) nil)))

(with-test (:name :array-typep-other-pointer-widetag)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (typep x '(and base-string (not simple-array))))
    ((10) nil)
    (((make-array 10 :element-type 'base-char :adjustable t)) t)))

(with-test (:name :constraint-after-checkgen)
  (let ((v #(10 20)))
    (checked-compile-and-assert
        ()
        `(lambda (p1 p2 p3 p4)
           (declare (type (satisfies eval) p2)
                    (type (member :from-end 2) p3))
           (position p1
                     (the (member ,v 3) p2)
                     (the (member 1 :from-end) p3) nil
                     :test-not p4))
      ((20 v :from-end #'/=) 1))))

(with-test (:name :cast-multiple-uses-no-dest)
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (x)
         (the integer
              (when x
                (if (> x 0) 1 2)))
         (the integer x))
    ((23) 23))
  (checked-compile-and-assert
      (:allow-style-warnings t)
      `(lambda (a b)
         (declare (optimize (debug 1)))
         (logand
          (when b
            (if (> (the integer a) 0) 10 20))
          0)
         (the integer b))
    ((24 23) 23)))


(with-test (:name :maybe-delete-exit-after-let-conversion)
  (checked-compile-and-assert
      ()
      `(lambda (m)
         (flet ((out ()
                  (flet ((in (a)
                           (dotimes (i 3 a)
                             (if m
                                 (return-from out)
                                 (return-from out)))
                           (labels ((f (&optional (a m))
                                      a
                                      m)))))
                    (in (in 10)))))

           (out)
           33))
    ((t) 33))
  (checked-compile-and-assert
      ()
      `(lambda ()
         (unwind-protect
              (flet ((f (a b &optional c)
                       (values a b c)))
                (f 1 2 (f 0 0)))))
    (() (values 1 2 0))))

(with-test (:name :make-array-hairy-cons)
  (checked-compile-and-assert
      ()
      `(lambda (type)
         (make-array 4 :element-type type))
    (('(or (cons (satisfies eval)) atom)) #(0 0 0 0) :test #'equalp)))

(with-test (:name :substitute-single-use-lvar-exit-cleanups)
  (checked-compile-and-assert
      ()
      `(lambda (z)
         (block nil
           (let ((b (1+ (funcall z))))
             (catch 'c (return b)))))
    (((constantly 33)) 34)))

(with-test (:name :substitute-single-use-lvar-unknown-exits)
  (checked-compile-and-assert
      ()
      `(lambda (f)
         (block nil
           (let ((x (evenp (funcall f)))
                 (y (catch 'c
                      (return (catch 'c (block nil 11))))))
             (declare (ignore y))
             x)))
    (((constantly 33)) 11)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.2)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (block nil
           (if (catch 'c 0)
               (return
                 (let ((x (the real b)))
                   (let ((* (list 1)))
                     (declare (dynamic-extent *))
                     (catch 'ct5
                       (if t (return 34))))
                   x))
               (catch 'c 0))))
    ((1) 34)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.3)
  (checked-compile-and-assert
      ()
      `(lambda (b)
         (let ((a b))
           (block nil
             (let ((* (list 1)))
               (declare (dynamic-extent *))
               (if b
                   (let ((j a))
                     (let ((* (list 1)))
                       (declare (dynamic-extent *))
                       (if b (return 44))
                       (setf a nil))
                     (let ((z j)) z))
                   (eval 2))))))
      ((33) 44)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.4)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (block nil
        (flet ((f ()
                 (let ((p (1+ a)))
                   (let ((* (list 1)))
                     (declare (dynamic-extent *))
                     (if a
                         (return 45)))
                   p)))
          (let ((* (lambda ()
                     (return (eval a)))))
            (f)))))
   ((33) 45)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.5)
  (checked-compile-and-assert
   ()
   `(lambda (b c)
      (block nil
        (flet ((f ()
                 (return (catch 'c (block b b)))))
          (return
            (block b5
              (let ((o c))
                (setf c
                      (catch 'c
                        (flet ((g ()
                                 (return)))
                          (f))))
                (let ((x o)) x)))))))
   ((10 20) 10)))

(with-test (:name :substitute-single-use-lvar-unknown-exits.6)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (block b
        (return-from b
          (let ((lv3 (random 10))
                *)
            (boole boole-1 lv3
                   (the integer
                        (catch 'ct4
                          (let ((x (list '*)))
                            (declare (dynamic-extent x))
                            (return-from b (eval x))))))))))
   (() 1)))

(with-test (:name :lambda-let-inline)
  (let ((fun (checked-compile
              `(lambda ()
                 (let ((x (lambda () 1)))
                   (funcall x))))))
    (assert (null (ctu:find-anonymous-callees fun)))
    (assert (= (funcall fun) 1))))

(with-test (:name :external-cast-deletion)
  (checked-compile-and-assert
   ()
   `(lambda (a c)
      (declare (notinline elt logior))
      (logior
       (if c
           (the integer (elt '(10 20) a))
           (let ((v1 (loop repeat 3 count t)))
             (declare (dynamic-extent v1))
             v1))))
   ((0 t) 10)
   ((1 nil) 3)))

(with-test (:name :fixnump-instance-ref-immediately-used)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (let (z)
        (and
         (typep
          (let ((y (let ((s (cons a b)))
                     (declare (dynamic-extent s))
                     (cdr s))))
            (unwind-protect
                 (let ((s (list c)))
                   (declare (dynamic-extent s))
                   (setf z (car s))))
            y)
          'fixnum)
         z)))
   ((1 2 'a) 'a)))

(with-test (:name :fixnump-instance-ref-immediately-used.2)
  (checked-compile-and-assert
   ()
   `(lambda (a b c)
      (let* ((l (cons a b))
             (cdr (cdr l)))
        (setf (cdr l) c)
        (typep cdr 'fixnum)))
   ((1 2 'a) t)))

(with-test (:name :round-numeric-bound)
  (checked-compile-and-assert
   ()
   `(lambda (a c f)
      (declare (type (integer -1111868182375 1874303539234) a))
      (- (rem (funcall f) (max 23 (* 45092832376540563 a -4469591966)))
         (signum c)))
   ((1874303539234 2 (constantly 123)) 7)))

(with-test (:name :ir2-optimize-jumps-to-nowhere)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type fixnum a))
      (if (< a 0 a)
          (block a (shiftf a 1))
          0))
   ((0) 0)))

(with-test (:name :double-float-bits-stub)
  (checked-compile-and-assert
   ()
   `(lambda (x)
     (float-sign 5.0d0 (the double-float x)))
   ((3d0) 3d0)))

(with-test (:name :typep-word)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (typep x 'sb-vm:word))
   ((1) t)
   (((1- (expt 2 sb-vm:n-word-bits))) t)
   (((expt 2 sb-vm:n-word-bits)) nil)
   ((-1) nil)
   (('a) nil)
   ((()) nil)
   (((1- most-negative-fixnum)) nil)))

(with-test (:name :fixnum-mod-p-word-descriptor)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare (type sb-vm:signed-word a))
      (typep a '(integer 0 ,(1- most-positive-fixnum))))
   (((1- most-positive-fixnum)) t)
   ((0) t)
   ((1) t)
   ((most-positive-fixnum) nil)
   (((1+ most-positive-fixnum)) nil)
   ((most-negative-fixnum) nil)
   (((1+ most-negative-fixnum)) nil)
   (((1- (expt 2 (1- sb-vm:n-word-bits)))) nil)
   ((-1) nil)))

(with-test (:name :check-bound-zero-safety-notes)
  (checked-compile-and-assert
      (:allow-notes nil
       :optimize '(:speed 3 :safety 0))
      `(lambda (a x y z)
         (declare (fixnum x y z)
                  ((simple-array t (*)) a)
                  (optimize (speed 3) (safety 0)))
         (aref a (+ x (- y z))))
    ((#(1 2 3) 1 0 0) 2)))

(with-test (:name :convert-mv-bind-to-let-multiple-uses)
  (checked-compile-and-assert
   ()
   `(lambda (f)
      (let* ((a (eval 1))
             (b (eval 2)))
        (multiple-value-bind (x y) (if f
                                       (values a 1)
                                       (values b 2))
          (values x y))))
   ((t) (values 1 1))
   ((nil) (values 2 2))))

(with-test (:name :substitute-single-use-lvar-multiple-uses)
  (checked-compile-and-assert
   ()
   `(lambda (f)
      (let* ((a (eval 1))
             (b (eval 2))
             (m (if f
                    (values a)
                    (values b))))
        m))
   ((t) 1)
   ((nil) 2)))

(with-test (:name :tn-ref-type-multiple-moves)
  (checked-compile-and-assert
   ()
   `(lambda  (a c)
      (declare (type (integer 546181490258163 937632934000433) c))
      (let ((v8 c))
        (multiple-value-bind (v9 v6)
            (if (/= a v8)
                (values 0 10983313414045189807)
                (values 0 c))
          (declare (ignore v9))
          (loop repeat 2
                do (eval v6))
          v6)))
   ((0 571816791704489) 10983313414045189807)))

(with-test (:name :substitute-single-use-lvar-cast-chains)
  (checked-compile-and-assert
   ()
   `(lambda (f a b)
      (labels ((fun (z)
                 (let ((m z))
                   ;; delays type derivation of FUN as FIXNUM until constraint propagation
                   ;; making sure SUBSTITUTE-SINGLE-USE-LVAR runs first.
                   (if (typep m 'fixnum)
                       m
                       0))))
        (declare (inline fun))
        (let* ((a (fun a))
               (b (fun b)))
          (let ((m
                  (if f
                      (the fixnum (the integer a))
                      (the fixnum (the integer b)))))
            m))))
   ((t 1 2) 1)
   ((nil 1 2) 2)))

(with-test (:name :m-v-bind-multi-use-unused-values.1)
  (let ((f (checked-compile
            '(lambda (z m)
              (multiple-value-bind (a b)
                  (if z
                      10
                      (values (sxhash m) m))
                (declare (ignore a))
                b)))))
    (assert (eql (funcall f t 33) nil))
    (assert (eql (funcall f nil 33) 33))
    (assert (not (ctu:find-named-callees f)))))

(with-test (:name :m-v-bind-multi-use-unused-values.2)
  (let ((f (checked-compile
            '(lambda (z m)
              (multiple-value-bind (a b c)
                  (if z
                      (values 10)
                      (values (sxhash m) m))
                (declare (ignore a))
                (list b c))))))
    (assert (equal (funcall f t 33) '(nil nil)))
    (assert (equal (funcall f nil 33) '(33 nil)))
    (assert (not (ctu:find-named-callees f)))))

(with-test (:name :m-v-bind-multi-use-unused-values.3)
  (let ((f (checked-compile
            '(lambda (z m)
              (multiple-value-bind (a b)
                  (if z
                      10
                      (values m (sxhash m)))
                (declare (ignore b))
                a)))))
    (assert (eql (funcall f t 33) 10))
    (assert (eql (funcall f nil 33) 33))
    (assert (not (ctu:find-named-callees f)))))

(with-test (:name :m-v-bind-multi-use-unused-values.4
            :skipped-on :sbcl)
  (let ((f (checked-compile
            '(lambda (z m)
              (nth-value 1
               (if z
                   (funcall (the function z))
                   (values (sxhash m) m)))))))
    (assert (eql (funcall f (lambda () (values 1 22)) 33) 22))
    (assert (eql (funcall f nil 34) 34))
    (assert (not (ctu:find-named-callees f)))))

(with-test (:name :m-v-bind-multi-use-unused-values.5
            :skipped-on :sbcl)
  (let ((f (checked-compile
            '(lambda (z m)
              (nth-value 1
               (if z
                   (funcall (the function z))
                   (sxhash m)))))))
    (assert (eql (funcall f (lambda () (values 1 22)) 33) 22))
    (assert (eql (funcall f nil 34) nil))
    (assert (not (ctu:find-named-callees f)))))

(with-test (:name :m-v-bind-multi-use-variable-type-change)
  (checked-compile-and-assert
   ()
   '(lambda (p)
     (when (position #\a (the (or (simple-string 1) (simple-string 2)) p))
       nil))
   (("a") nil)
   (("ab") nil)))

(with-test (:name :array-element-type-cons.1)
  (checked-compile-and-assert
   (:allow-notes nil)
   '(lambda (vector)
     (declare ((or (simple-array (unsigned-byte 32) (2))
                   (simple-array (unsigned-byte 32) (4))) vector))
     (make-array 10 :element-type (array-element-type vector)))
   (((make-array 2 :element-type '(unsigned-byte 32)))
    '(unsigned-byte 32) :test (lambda (x y)
                                (equal (array-element-type (car x)) (car y))))))

(with-test (:name :array-element-type-cons.2)
  (checked-compile-and-assert
   (:allow-notes nil)
   '(lambda (vector)
     (declare ((and (simple-array (unsigned-byte 32) (2))
                (satisfies eval)) vector))
     (make-array 10 :element-type (array-element-type vector)))
   (((make-array 2 :element-type '(unsigned-byte 32)))
    '(unsigned-byte 32) :test (lambda (x y)
                                (equal (array-element-type (car x)) (car y))))))

(with-test (:name :about-to-modify-symbol-value-relax-fun-type)
  (let* ((compiled-lambda (compile nil '(lambda (&rest x) x 'hi)))
         (sb-c::*compiler-error-bailout*
          (lambda (&optional c) (error c))))
    (declare (notinline set))
    (set 'sb-c::*compiler-error-bailout* compiled-lambda)))

(with-test (:name :self-evaluating-p-not)
  (let ((s (gensym)))
    (set s 9)
    (unintern s)
    (import s 'keyword)
    (assert (not (sb-int:self-evaluating-p s)))))

(with-test (:name :lea-modfx-constant-folding)
  (checked-compile-and-assert
      ()
      '(lambda (c)
        (if (if c
                c
                (if 444
                    nil
                    99))
            11
            (logand 3
                    (logxor
                     (* 5
                        (if c
                            0
                            (ash most-positive-fixnum -2)))
                     3))))
    ((t) 11)
    ((nil) 0)))

(with-test (:name :setup-environment-tn-conflicts)
  (checked-compile-and-assert
      ()
      '(lambda (z)
        (let ((c 0))
          (flet ((bar ()
                   (let ((m (eval :bad)))
                     (eval m)
                     (and m c))))
            (declare (notinline bar))
            (cond (z
                   (setf c 10)
                   (bar))
                  (44)))))
    ((t) 10)
    ((nil) 44)))

(with-test (:name :setup-environment-tn-conflicts.2)
  (checked-compile-and-assert
      ()
      '(lambda (z)
        (let ((c 0)
              (b 0)
              (a 0)
              (d 0))
          (labels ((bar ()
                     (let ((m (eval :bad)))
                       (eval m)
                       (if m
                           (values a b c d))))
                   (jam ()
                     (multiple-value-list (bar))))
            (declare (notinline bar
                                jam))
            (cond (z
                   (setf a 10
                         c 10
                         b 10
                         d 10)
                   (jam))
                  (44)))))
    ((t) '(10 10 10 10) :test #'equal)
    ((nil) 44)))

(with-test (:name :setup-environment-tn-conflicts.3)
  (checked-compile-and-assert
      ()
      '(lambda (b)
        (flet ((%f7 ()
                 (flet ((%f10 ()
                          (setf b b)))
                   (declare (dynamic-extent #'%f10))
                   (funcall (eval #'%f10)))))
          (declare (notinline %f7))
          (%f7)))
    ((10) 10)))

(with-test (:name :dead-sets)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (logtest
          ((lambda (v &rest args)
             (declare (ignore args))
             (setf v
                   ((lambda (&rest args) (declare (ignore args)) (error "")) v)))
           1)
          1))
    (() (condition 'simple-error))))

(with-test (:name :functional-may-escape-p)
  (checked-compile-and-assert
      (:optimize :safe)
      '(lambda ()
        (let (x)
          (block nil
            (flet ((x () (let (*)
                           (return 33))))
              (setf x #'x)))
          (funcall x)))
    (() (condition 'control-error))))

(with-test (:name :inlining-multiple-refs)
  (checked-compile
   `(lambda (x)
      (labels ((%s (y &rest r)
                 (some
                  (lambda (r) (apply #'%s (1+ y) r))
                  (apply #'eql x r))))
        (%s 1)))))

(with-test (:name :update-lvar-dependencies-delete-lvar)
  (checked-compile-and-assert
      ()
      '(lambda (x y)
        (let ((x x))
          (block nil
            (flet ((proc (thing)
                     (when thing
                       (return (eval thing)))))
              (declare (inline proc))
              (if x
                  (proc y)
                  (proc y)))))
        t)
    ((1 2) t)))

(with-test (:name :car-type-on-or-null)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (declare (type (or null (cons fixnum)) x))
              (if x
                  (car x)
                  0))))
          '(function ((or null (cons fixnum t))) (values fixnum &optional)))))

(with-test (:name :nlx-entry-zero-values)
  (checked-compile-and-assert
      ()
      '(lambda (x)
        (multiple-value-call (lambda (&optional x) x)
          (block nil
            (funcall (eval (lambda ()
                             (return (if x
                                         (values)
                                         10))))))))
    ((t) nil)
    ((nil) 10)))

(with-test (:name :find-test-to-eq-with-key)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (position (1- (expt x 64)) '((#xFFFFFFFFFFFFFFFF)) :key #'car))
   ((2) 0)
   ((1) nil)))

(with-test (:name :maybe-infer-iteration-var-type-on-union)
  (checked-compile-and-assert
      (:allow-notes nil :optimize '(:speed 3 :compilation-speed 1 :space 1))
      `(lambda (a)
         (loop repeat (if a 2 0) count 1))
    ((t) 2)
    ((nil) 0)))

(with-test (:name :maybe-infer-iteration-var-type-on-union.2)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (let ((v4 (the (or (single-float (1.0) (3.0)) (single-float 4.0 5.0)) a)))
           (incf v4 1.0)))
      ((4.0) 5.0)))

(with-test (:name :derive-array-rank-negation)
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (simple-array * (* *))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2) :adjustable t)) t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (simple-array fixnum (* *))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2))) t))
  (checked-compile-and-assert
   ()
   `(lambda (a)
      (declare ((not (and (array * (* *)) (not simple-array))) a))
      (eql (array-rank a) 2))
   (((make-array '(2 2))) t)))

(with-test (:name :derive-array-rank-negation.2)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (declare ((and simple-array
                         (not (simple-array * (* *))))
                        x))
              (eql (array-rank x) 2))))
          '(function ((and simple-array (not (simple-array * (* *)))))
            (values null &optional)))))

(with-test (:name :known-fun-no-fdefn)
  (assert (not (ctu:find-named-callees
                (checked-compile
                 '(lambda () #'+))))))

(with-test (:name :double-float-p-weakening)
  (checked-compile-and-assert
   (:optimize '(:speed 3 :safety 1))
   '(lambda (x)
     (declare (double-float x))
     x)
   ((0.0) (condition 'type-error))
      ((1d0) 1d0)))

(declaim (inline inline-fun-arg-mismatch))
(defun inline-fun-arg-mismatch (x)
  (declare (optimize (debug 0)))
  x)

(with-test (:name :inline-fun-arg-mismatch)
  (checked-compile-and-assert
      (:allow-warnings '(or sb-int:local-argument-mismatch
                         #+interpreter simple-warning)) ;; why?
      '(lambda ()
        (multiple-value-call #'inline-fun-arg-mismatch 1 2))
    (() (condition 'program-error))))

(with-test (:name :principal-lvar-ref-use-loop)
  (checked-compile-and-assert ()
   '(lambda (vector)
     (labels ((f (count)
                (when (< (aref vector 0) count)
                  (f count))))))
   ((1) nil)))

(with-test (:name (:mv-call :more-arg))
  (checked-compile-and-assert
   ()
   '(lambda (&rest rest)
     (multiple-value-bind (a b c) (values-list rest)
       (declare (ignore c))
       (list a b)))
   ((1 3) '(1 3) :test #'equal)))

(with-test (:name (:mv-call :more-arg-unused)
            ;; needs SB-VM::MORE-ARG-OR-NIL VOP
            :broken-on (not (or :x86-64 :x86 :ppc :arm :arm64 :riscv)))
  (checked-compile-and-assert
   ()
   '(lambda (&rest rest)
     (multiple-value-bind (a b) (values-list rest)
       (list a b)))
   (() '(nil nil) :test #'equal)
   ((1) '(1 nil) :test #'equal)
   ((1 3) '(1 3) :test #'equal)))

(with-test (:name :truncate-deriver-on-number-type)
  (checked-compile-and-assert
   ()
   '(lambda (i)
     (truncate
      (labels ((f (&optional (o i))
                 (declare (ignore o))
                 (complex 0 0)))
        (declare (dynamic-extent (function f)))
        (the integer
             (multiple-value-call #'f (values))))
      3))
   ((0) (values 0 0))))

(with-test (:name :signum-type-deriver)
  (checked-compile-and-assert
   ()
   '(lambda (n)
     (typep (signum n) 'complex))
   ((#C(1 2)) t)
   ((1d0) nil)
   ((10) nil)))

(with-test (:name :array-header-p-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (q)
     (and (typep q '(not simple-array))
      (sb-kernel:array-header-p q)))
   ((10) nil)
   (((make-array 10 :adjustable t)) t)))

(with-test (:name :phase-type-derivation)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (= (phase (the (integer -1 0) x))
      (coerce pi 'single-float)))
   ((-1) t)
   ((0) nil)))

(with-test (:name :maybe-negate-check-fun-type)
  (checked-compile-and-assert
   ()
   '(lambda (m)
     (declare ((or (function (number)) (eql #.#'symbolp)) m))
     (the (member 3/4 4/5 1/2 #.#'symbolp) m))
   ((#'symbolp) #'symbolp)))

(with-test (:name :lvar-fun-type-on-literal-funs)
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (declare (type (or null string) p))
      (locally (declare (optimize (space 0)))
        (stable-sort p ,#'string<)))
   (((copy-seq "acb")) "abc" :test #'equal)))

(with-test (:name :equal-to-eql)
  (let ((f (checked-compile
            `(lambda (x y)
               (equal (the hash-table x) y)))))
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn))))
  (let ((f (checked-compile
            `(lambda (x y)
               (equalp (the function x) y)))))
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:fdefn)))))

(with-test (:name :multiway-branch-duplicate-case)
  (let ((f (checked-compile '(lambda (b)
                              (case b
                                ((1 2) :good)
                                ((3 2) :bad)))
                            :allow-style-warnings t)))
    (assert (eq (funcall f 2) :good))))

(with-test (:name :symbol-case-as-jump-table
                  :skipped-on (not (or :x86 :x86-64)))
  ;; Assert that a prototypical example of (CASE symbol ...)
  ;; was converted to a jump table.
  (let ((c (sb-kernel:fun-code-header #'sb-debug::parse-trace-options)))
    (assert (>= (sb-kernel:code-jump-table-words c) 17))))

(with-test (:name :modular-arith-type-derivers)
  (let ((f (checked-compile
            `(lambda (x)
               (declare ((and fixnum
                              unsigned-byte) x)
                        (optimize speed))
               (rem x 10)))))
        (assert (not (ctu:find-code-constants f :type 'bignum)))))

(with-test (:name :deduplicated-fdefns :fails-on (not :64-bit))
  (flet ((scan-range (c start end)
           (let (dup-fdefns names)
             (loop for i from start below end
                   do (let ((obj (sb-kernel:code-header-ref c i)))
                        (when (sb-kernel:fdefn-p obj)
                          (let ((name (sb-kernel:fdefn-name obj)))
                            (when (member name names)
                              (push obj dup-fdefns))
                            (push name names)))))
             (assert (not dup-fdefns)))))
    (dolist (c (sb-vm::list-allocated-objects :all :type sb-vm:code-header-widetag))
      (let* ((start (+ sb-vm:code-constants-offset
                       (* (sb-kernel:code-n-entries c)
                          sb-vm:code-slots-per-simple-fun)))
             (end (+ start (sb-kernel:code-n-named-calls c))))
        ;; Within each subset of FDEFNs there should be no duplicates
        ;; by name. But there could be an fdefn that is in the union of
        ;; the ranges twice, if used for named call and a global ref.
        (scan-range c start end)
        (scan-range c end (sb-kernel:code-header-words c))))))

(with-test (:name :map-all-lvar-dests)
  (checked-compile-and-assert
   ()
   `(lambda (&key (pred (constantly 44)))
      (declare (type function pred))
      (funcall pred))
   (() 44)))

(with-test (:name (:lvar-fun-name :constant-leaf-not-constant-lvar-p))
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (funcall
                          (the (function (t) t)
                               ,(checked-compile '(lambda ())))))
                      :allow-warnings t
                      :allow-style-warnings t))))

(with-test (:name (:%logbitp :signed-and-unsigned))
  (checked-compile-and-assert
      ()
      `(lambda (p2)
         (declare (type (integer ,(expt -2 (1- sb-vm:n-word-bits))
                                 ,(1- (expt 2 sb-vm:n-word-bits))) p2))
         (logbitp 26 p2))
    ((3) nil)
    (((ash 1 26)) t)))

(with-test (:name :vop-return-constant-boxing)
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (setf (aref (the (simple-array double-float (*)) x) 0)
            10d0))
   :allow-notes nil)
  (checked-compile
   `(lambda (x)
      (declare (optimize speed))
      (setf (aref (the (simple-array sb-vm:word (*)) x) 0)
            (1- (expt 2 sb-vm:n-word-bits))))
   :allow-notes nil)
  (checked-compile
   `(lambda (x y)
      (declare (optimize speed))
      (setf (svref y 0)
            (setf (aref (the (simple-array double-float (*)) x) 0)
                  10d0)))
   :allow-notes nil)
  (checked-compile
   `(lambda (f a)
      (declare (optimize speed))
      (funcall (the function f)
               1 2 3 4 5 6 7 8 9 10
               (setf (aref (the (simple-array double-float (*)) a) 0)
                     10d0)))
   :allow-notes nil))

(with-test (:name :make-constant-tn-force-boxed)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (declare (type character c))
      (list 1 1 1 1 1 1 1 1 1 1 1 (the (eql #\() c)))
   ((#\() '(1 1 1 1 1 1 1 1 1 1 1 #\() :test #'equal)))

(with-test (:name :jump-over-move-coercion
            :serial t
            :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (number)
              (declare ((or fixnum double-float single-float) number))
              (cond ((typep number 'double-float)
                     number)
                    ((typep number 'single-float)
                     (coerce number 'double-float))
                    ((typep number 'fixnum)
                     (coerce number 'double-float)))))))
    (ctu:assert-no-consing (funcall f 1d0)))
  (let ((f (checked-compile
            '(lambda (v number)
              (declare ((or fixnum double-float single-float) number))
              (setf (svref v 0)
               (cond ((typep number 'double-float)
                      number)
                     ((typep number 'single-float)
                      (coerce number 'double-float))
                     ((typep number 'fixnum)
                      (coerce number 'double-float))))))))
    (let ((v (vector 0)))
      (ctu:assert-no-consing (funcall f v 1d0)))))

(with-test (:name :jump-over-move-coercion-match-type)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (type (or sb-vm:word sb-vm:signed-word) a))
         (declare (type (and fixnum unsigned-byte) b))
         (lognand (max 0 a) b))
    (((expt 2 (1- sb-vm:n-word-bits)) #xFFFFFF) -1)
    (((1- (expt 2 (1- sb-vm:n-word-bits))) #xFFFFFF) -16777216)))

#+#.(cl:if (cl:gethash 'sb-c:multiway-branch-if-eq sb-c::*backend-template-names*)
           '(:and)
           '(:or))
(with-test (:name :typecase-to-case-preserves-type)
  (let ((f (checked-compile
            '(lambda (x)
              ;; This illustrates another possible improvement-
              ;; there are not actually 6 different slot indices
              ;; that we might load. Some of them are the same
              (typecase x
                (sb-pretty:pprint-dispatch-table (sb-pretty::pp-dispatch-entries x))
                (sb-impl::comma (sb-impl::comma-expr x))
                (sb-vm:primitive-object (sb-vm:primitive-object-slots x))
                (sb-kernel:defstruct-description (sb-kernel::dd-name x))
                (sb-kernel:lexenv (sb-c::lexenv-vars x))
                (broadcast-stream (broadcast-stream-streams x))
                (t :none))))))
    ;; There should be no #<layout> referenced directly from the code header.
    ;; There is of course a vector of layouts in there to compare against.
    (assert (not (ctu:find-code-constants f :type 'sb-kernel:layout)))
    ;; The function had better work.
    (assert (eq (funcall f 'wat) :none))
    (assert (equal (funcall f (make-broadcast-stream *error-output*))
                   (list *error-output*)))))


(with-test (:name :=-interval-derivation-and-complex)
  (checked-compile-and-assert
      ()
      `(lambda (p1)
         (declare ((complex (integer -1 -1)) p1))
         (= -1 p1))
    ((#C(-1 -1)) nil)))

(with-test (:name :cmov-move-hoisting)
  (checked-compile-and-assert
      ()
      `(lambda (p)
         (declare ((or (eql 0.0)
                       sb-vm:word) p))
         (if (> p 51250)
             p
             1))
    ((0.0) 1)
      ((#1=(1- (expt 2 sb-vm:n-word-bits))) #1#))
  (checked-compile-and-assert
   ()
   `(lambda (p)
      (declare (type (member 4801112936349103672 -9474680540642044437) p))
      (max 0 p -1.0))
   ((4801112936349103672) 4801112936349103672)
   ((-9474680540642044437) 0)))

(with-test (:name :logior-derive-type-widening-tail-set-types)
  (checked-compile-and-assert
      ()
      `(lambda (a b c)
         (labels ((q (x y)
                    (let ((* (lambda () x y)))
                      (the integer a)))
                  (p ()
                    (logior (apply #'q (list a b))
                            (if b
                                (return-from p (q b c))
                                1))))
           (if c
               0.0
               (p))))
    ((44 nil nil) 45)
    ((3 2 1) 0.0)
    ((30 2 nil) 30)))

(with-test (:name :if-eq-optimization-consistency)
  (let ((sb-c::*check-consistency* t))
    (checked-compile-and-assert
     ()
     `(lambda ()
        (eval (and (if (eval 0) (eval 0) (eval 0)) t)))
     (() t))))

(with-test (:name :make-array-half-finished-transform)
  (checked-compile-and-assert
      (:allow-warnings t)
      `(lambda ()
         (make-array 6 :fill-pointer 33))
    (() (condition '(not program-error)))))

(with-test (:name :nested-if+let)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let (x)
           (when x
             (setq x 1))
           (let ((y (if x
                        t
                        nil)))
             (if y
                 y
                 (let ((x x))
                   x)))))
      (() nil)))

(with-test (:name :let-var-immediately-used-p-deleted-lambda)
  (checked-compile-and-assert
   ()
   `(lambda (c)
      (if (and nil
               (or
                (zerop (count (unwind-protect 1) '(1)))
                c))
          1
          0))
   ((2) 0)))

(with-test (:name :dce-local-functions)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (block out
           (labels ((mmm (z vars)
                      (when vars
                        (mmm z vars))))
             (mmm 1 (progn
                      (dotimes (a 1) (return-from out 10))
                      (dotimes (b 3) (catch 'b))))
             (dotimes (c 3) (catch 'c)))))
    (() 10)))

(with-test (:name :dce-more-often)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (+ 1
            (if t
                0
                (progn
                  (tagbody
                   p
                     (tagbody
                        (let ((a (lambda () (go o))))
                          (declare (special a)))
                      o)
                     (when (< a 1)
                       (go p)))
                  2))))
    ((1) 1)))

(with-test (:name :ir1-optimize-constant-fold-before-giving-up)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (+ 2 (- (let ((sum 0))
                   (declare (type fixnum sum))
                   (block nil
                     (tagbody
                      next
                        (cond ((>= sum '0)
                               (go end))
                              (a
                               (ceiling 1 (unwind-protect 0))
                               (incf sum)))
                        (go next)
                      end))
                   sum))))
    ((1) 2)))

(with-test (:name :position-case-otherwise)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (position x '(a otherwise b t nil)))
    (('a) 0)
    (('otherwise) 1)
    ((nil) 4)
    ((t) 3)))

(with-test (:name :unreachable-component-propagate-let-args)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (let ((p 0))
           (flet ((f (&key)
                    (flet ((g (&optional
                                 (z
                                  (return-from f (+ (dotimes (i 0 0)) p))))
                             p))))))
           p))
    (() 0)))

(with-test (:name :dce-through-optional-dispatch)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (flet ((z (&optional a)
                  (declare (ignore a))
                  123))
           (let ((z #'z))
             (when x
               (unless x
                 (setf z 10)))
                   (funcall z))))
    ((nil) 123)
    ((t) 123)))

(with-test (:name :values-list+cons)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            `(lambda ()
               (values-list (cons 1 nil)))))
          '(function () (values (integer 1 1) &optional)))))

(with-test (:name :xeps-and-inlining)
  (checked-compile-and-assert
   ()
   `(lambda (args)
      (flet ((fun () args))
        (declare (inline fun))
        (multiple-value-call #'fun (values-list args))
        #'fun))))

(with-test (:name :split-let-ctran-kind)
  (checked-compile-and-assert
   ()
   `(lambda (a b)
      (let ((a-n (null a))
            (b-n (null b)))
        (cond (b-n 1)
              (a-n a)
              (t a))))
   ((nil nil) 1)
   ((nil t) nil)))
