#!/bin/sh

mkdir -p /var/tmp/junk /var/tmp/sbcl-test-logs
TEST_DIRECTORY=/var/tmp/junk SBCL_HOME=../obj/sbcl-home exec ../src/runtime/sbcl \
  --noinform --core ../output/sbcl.core --no-userinit --no-sysinit --noprint --disable-debugger << EOF
(require :sb-posix)
(let ((*evaluator-mode* :compile))
  (with-compilation-unit () (load"run-tests")))
(in-package run-tests)
(setq *summarize-test-times* t)
(defun parallel-execute-tests (max-jobs)
  (format t "Using ~D processes~%" max-jobs)
  (let ((files (mapcar #'pathname-name
                       (append (pure-load-files)
                               (pure-cload-files)
                               (impure-load-files)
                               (impure-cload-files))))
        (subprocess-count 0)
        (subprocess-list nil)
        (losing))
    (flet ((wait ()
             (multiple-value-bind (pid status) (sb-posix:wait)
               (decf subprocess-count)
               (let ((process (assoc pid subprocess-list)))
                 (setq subprocess-list (delete process subprocess-list))
                 (let* ((code (ash status -8))
                        (filename (cdr process)))
                   (cond ((eq code 104)
                          (format t "~A: success~%" filename))
                         (t
                          (format t "~A: status ~D~%" filename code)
                          (push filename losing))))))))
      (dolist (file files)
        (when (>= subprocess-count max-jobs)
          (wait))
        (let ((pid (sb-posix:fork)))
          (when (zerop pid)
            (with-open-file (stream (format nil "/var/tmp/sbcl-test-logs/~a" file)
                                    :direction :output :if-exists :supersede)
              sb-alien::(alien-funcall (extern-alien "dup2" (function int int int))
                                       (sb-sys:fd-stream-fd stream) 1)
              sb-alien::(alien-funcall (extern-alien "dup2" (function int int int)) 1 2))
            ;; Send this to the log file, not the terminal
            (setq *debug-io* (make-two-way-stream (make-concatenated-stream)
                                                  *error-output*))
            (pure-runner (list (concatenate 'string file ".lisp"))
                         (if (search "-cload" file) 'cload-test 'load-test)
                         (make-broadcast-stream))
            (exit :code (if (unexpected-failures) 1 104)))
          (format t "~A: pid ~d~%" file pid)
          (incf subprocess-count)
          (push (cons pid file) subprocess-list)))
      (loop (if (plusp subprocess-count) (wait) (return)))
      (when losing
        (terpri)
        (format t "Failing files:~%")
        (dolist (filename losing)
          (format t "~A~%" filename))
        (exit :code 1)))))
(parallel-execute-tests $1)
EOF