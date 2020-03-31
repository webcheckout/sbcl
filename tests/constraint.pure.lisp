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

#+sb-unicode
(with-test (:name :base-char-p)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (if (sb-kernel:base-char-p x)
                  (characterp x)
                  t))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-eql)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x) (let (y) (setq y x) (eql y x)))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-lvar-substition)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare ((integer 0 10) a)
                  (fixnum b))
         (let ((c b))
           (setq b a)
           (eql c b)))
    ((0 2) nil)
    ((0 0) t)))

(with-test (:name :number-comparisons)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (< a 0)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional))))
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (= a 30)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional)))))

(with-test (:name :=-constraint-complex-no-bounds)
  (checked-compile-and-assert
      ()
      `(lambda (p)
        (let ((x #c(1 2)))
          (when (= x p)
            x)))
    ((#c(1 2)) #c(1 2))
    ((#c(2 1)) nil)))
