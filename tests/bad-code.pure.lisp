(enable-test-parallelism)

(with-test (:name (make-array :bad-initial-contents))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (make-array '(1) :initial-contents 'foo))
               :allow-warnings t))))

(with-test (:name (make-string-output-stream :bad-element-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-string-output-stream :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (coerce :bad-type-specifier))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (coerce (list 2) 1))
               :allow-warnings t))))

(with-test (:name :zombie-entry-point-reference)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (labels ((%f ())) (%f #'%f)))
               :allow-warnings t))))
(with-test (:name :ir1-optimize-combination-dead-node)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f2 (x) x))
                    (list (%f2 1)
                          (multiple-value-call #'%f2 (values)))))
               :allow-warnings t))))

(with-test (:name (:bogus-block &key))
  (assert
   (nth-value 1
              (checked-compile `(lambda (&key (x (block 1 10))) x)
                               :allow-failure t))))

(with-test (:name :type-error-reporting)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (lambda ()
                                    (let ((v3 0))
                                      (cdr (1- (block b5 (if nil v3 0)))))))
                               :allow-warnings t))))

(with-test (:name :dx-on-deleted-nodes)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (restart-bind ((1 3))))
                               :allow-warnings t))))

(with-test (:name :transform-call-dfo-consistency)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f (&optional x) x))
                    (%f)
                    ;; Two of the %f calls are erroneous, with an extra argument
                    (flet ((%f6 (&key (k (%f (%f -1 (%f -2 -3))))) 0))
                      5)))
               :allow-warnings t))))

(with-test (:name :&aux-check-variable-names)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&aux (nil 10))
                  nil)
               :allow-failure t))))

(with-test (:name :mv-call-too-many-values)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (a)
                  (flet ((%f1 (x) x))
                    (apply #'%f1 a 2 (list 0))))
               :allow-warnings t))))

(with-test (:name :mv-call-too-many-values.closure)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (a b)
                  (flet ((%f1 () b))
                    (apply #'%f1 a 2 (list 0))))
               :allow-warnings t))))

(with-test (:name (map :values-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (map '* #'+ #(1) #(2)))
               :allow-warnings t))))


(with-test (:name :bad-type-specifier)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array 10 :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(x)))
               :allow-warnings t)))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(-10)))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.2))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(0 . 2)))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.3))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(0 . 2)
                              :element-type 'fixnum
                              :adjustable t))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.4))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array (list 'x)))
               :allow-warnings t))))

(with-test (:name (make-array :initial-contents :bad-macro))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(10) :initial-contents (do)))
               :allow-failure t))))

(with-test (:name (make-array :dimensions :bad-macro))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array (do)))
               :allow-failure t))))

(with-test (:name (make-array :dimensions :bad-propagated-value))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (let ((x '(("foo"))))
                    (make-array (list x) :fill-pointer 0)))
               :allow-warnings t))))

(with-test (:name (make-array :dimensions :unraveling-list))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x)
                  (make-array (list (list 10)) :adjustable x))
               :allow-warnings t))))

(with-test (:name :&rest-ref-bad-n)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&rest a) (lambda () (nth nil a)))
               :allow-warnings t))))

(with-test (:name :bad-type-specifier-handling)
  (multiple-value-bind (fun failure warnings)
      (checked-compile
       `(lambda (v) (typep v '(unsigned-byte 8 x (error ~s v))))
       :allow-warnings t)
    (declare (ignore fun))
    (assert failure)
    (mapcar #'princ-to-string warnings)))

(with-test (:name :ldb-transform-macroexpand)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (ldb (do) 0))
               :allow-failure t))))

(with-test (:name :bad-values-ftype)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (declare (values 0)))
               :allow-warnings t))))

(with-test (:name :bad-progv)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x) (progv x 1))
               :allow-warnings t)))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x) (progv 1 x))
               :allow-warnings t))))

(with-test (:name :coerce-to-nil)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (coerce (list t) nil))
               :allow-warnings t))))

(with-test (:name :unknown-vector-type-conflict)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (the (vector nonsense-type) nil))
               :allow-warnings t
               :allow-style-warnings t))))

(with-test (:name :subseq-unknown-vector-type)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (subseq (the (vector nonsense-type) :x) 0 1))
               :allow-warnings t
               :allow-style-warnings t))))
(with-test (:name :derive-node-type-unknown-type)
  (assert
   (nth-value 3
              (checked-compile
               '(lambda (x)
                 (let ((k (make-array 8 :element-type '(unsigned-byte 8))))
                   (setf (aref k 0) (the unknown-type (the integer x)))
                   (setf k (subseq "y" 0))))
               :allow-warnings t
               :allow-style-warnings t))))

(with-test (:name :highly-nested-type-error)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (macrolet ((macro ()
                                      `((lambda (x)
                                          (declare (number x))
                                          ',@ (loop repeat 10000
                                                    for cons = (list 1) then (list cons)
                                                    finally (return cons)))
                                        t)))
                           (macro)))
                      :allow-warnings t))))

(with-test (:name :complex-member-type)
  (assert (= (length (nth-value 2
                                (checked-compile
                                 `(lambda (x)
                                    (typep x '(complex (eql t))))
                                 :allow-warnings t)))
             1)))

(with-test (:name :bad-optionals)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (z)
                        (lambda (&optional (a nil x))
                          (declare (type integer x))
                          z))
                      :allow-warnings t))))

(with-test (:name :recursive-delete-lambda)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (flet ((%f ()
                                 (lambda ())))
                          (%f :a)
                          (%f :b)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (flet ((%f ()
                                 (lambda (&optional m) m)))
                          (%f :a)
                          (%f :b)))
                      :allow-warnings t))))

(with-test (:name :complex-number)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (typep 1 x))
   (('(complex number)) (condition 'error))))

(with-test (:name :aref-type-mismatch)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda (x)
                         (svref x *break-on-signals*))
                      :allow-warnings t))))

(with-test (:name :unknown-keys-propagation-error-checking.1)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda (x)
                         (let ((a :tests))
                           (find 1 x a #'eql)))
                      :allow-warnings t))))

(with-test (:name :unknown-keys-propagation-error-checking.2)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (apply 'find '(3 (1 2 3) :bad t)))
                      :allow-warnings t))))


(with-test (:name :sequence-lvar-dimensions-dotted-list)
  (assert (nth-value 3
                     (checked-compile
                      '(lambda () (position 0 '(1 2 0 5 . 5)))
                      :allow-style-warnings t
                      :allow-warnings t))))

(with-test (:name :source-form-context-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (y) `(defines ,@ (and x) . ,y))
                      :allow-warnings t))))

(with-test (:name :typep-transform-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (typep x (quote . z)))
                      :allow-failure t))))

(with-test (:name :member-transform-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (member x '(a . b)))
                      :allow-warnings t))))

(with-test (:name :encode-universal-time)
  (assert (nth-value 3
                     (checked-compile
                      '(lambda () (encode-universal-time 0 0 0 1 1 1900 -1))
                      :allow-style-warnings t))))

(with-test (:name :search-transform-bad-index)
  (checked-compile
   '(lambda (a)
     (search '(0 1 0 2) a :start1 4 :end1 5))))

(with-test (:name :bound-mismatch-union-types)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x)
                        (declare ((or (simple-string 10) (simple-string 15)) x))
                        (aref x 100))
                      :allow-warnings t))))

(with-test (:name :uses-with-bad-types)
  (assert (nth-value 3
                     (checked-compile
                      '(lambda (x)
                        (the integer (if x 10)))
                      :allow-style-warnings t))))

(with-test (:name :constant-modification-local-function)
  (assert (= (length (nth-value 2
                                (checked-compile
                                 '(lambda ()
                                   (flet ((z (a)
                                            (setf (aref a 0) 10)))
                                     (z #(10))
                                     (z #(a))))
                                 :allow-warnings t)))
             2)))

(with-test (:name :improper-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'string x '(#\a . #\b)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'list x '(1 2 . 3)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'vector x '(1 2 . 3)))
                      :allow-warnings t))))

(with-test (:name :improper-list.2)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (member-if #'(lambda (x) (evenp x)) '(1 2 3 . 4)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x)
                        (search '(a . b) x))
                      :allow-warnings t))))

(with-test (:name :call-nil)
  (checked-compile-and-assert
   ()
   `(lambda ()
      (funcall nil))
   (() (condition 'undefined-function)))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (if x
          10
          (funcall x)))
   ((nil) (condition 'undefined-function))))

(with-test (:name (:valid-callable-argument :toplevel-xep))
  (assert (nth-value 2 (checked-compile `(lambda (l) (find-if (lambda ()) l))
                                        :allow-warnings t))))

(with-test (:name (:valid-callable-argument :handler-bind))
  (assert (nth-value 2 (checked-compile
                        `(lambda (l) (handler-bind ((error (lambda ()))) (funcall l)))
                        :allow-warnings t))))

(with-test (:name (:valid-callable-argument :closure))
  (assert (nth-value 2 (checked-compile
                        `(lambda (l) (the (function (t)) (lambda () l)))
                        :allow-warnings t))))

(with-test (:name :bad-macros)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (coerce 'integer (restart-bind foo)))
               :allow-failure t))))

(with-test (:name :bad-funcall-macros)
  (assert
   (nth-value 1
              (checked-compile
              `(lambda () (funcall (lambda)))
               :allow-failure t))))

(with-test (:name :inlining-bad-code)
  (assert
   (nth-value 2
              (checked-compile
               `(lambda (x &rest args)
                  (unless
                      (if (eq x :tud)
                          (zerop (first args))
                          (every #'identity args (every #'identity args)))
                    args))
               :allow-style-warnings t
               :allow-warnings t))))

(with-test (:name :keyword-type-checking)
  (assert
   (nth-value 2
              (checked-compile
               `(lambda (x)
                  (make-array 10 (list x) x))
               :allow-warnings t))))
