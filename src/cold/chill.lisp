;;;; This file is not used at cold load time. Instead, it can be
;;;; loaded into an initialized SBCL to get it into a nostalgic frame
;;;; of mind, remembering the way things were in cold init, so that it
;;;; can READ code which is ordinarily read only when bootstrapping.
;;;; (This can be useful when debugging the system, since the debugger
;;;; likes to be able to read the source for the code. It can also be
;;;; useful when experimenting with patches on a running system.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage "SB-COLD"
  (:use "CL"))
(in-package "SB-COLD")

(sb-ext:unlock-package "CL")
(rename-package "COMMON-LISP" "COMMON-LISP"
                (cons "SB-XC" (package-nicknames "CL")))
;; Unlock all other SB- packages
(dolist (package (list-all-packages))
  (let ((name (package-name package)))
    (when (sb-int:system-package-p name)
      (sb-ext:unlock-package package))))

;;; Define this first to avoid a style-warning from 'shebang'
(defun read-target-float (stream char)
  (declare (ignore stream char))
  (values)) ; ignore the $ as if it weren't there
(compile 'read-target-float)
;;; We need the #! readtable modifications.
(load (merge-pathnames "shebang.lisp" *load-truename*))
;;; ... applied to the default readtable
(set-dispatch-macro-character #\# #\+ #'read-targ-feature-expr)
(set-dispatch-macro-character #\# #\- #'read-targ-feature-expr)
(set-macro-character #\$ #'read-target-float t)

;;; Just in case we want to play with the initial value of
;;; backend-subfeatures
(setf sb-cold:*shebang-backend-subfeatures* sb-c:*backend-subfeatures*)

;; Restore !DEFINE-LOAD-TIME-GLOBAL macro
(export 'sb-int::!define-load-time-global 'sb-int)
(setf (macro-function 'sb-int::!define-load-time-global)
      (macro-function 'sb-ext:define-load-time-global))

(export '(sb-int::!cold-init-forms
          sb-int::/show sb-int::/noshow sb-int::/show0 sb-int::/noshow0)
        'sb-int)

(defmacro sb-int:!cold-init-forms (&rest forms) `(progn ,@forms))

;; If :sb-show is present, then these symbols are fboundp.
;; Otherwise define them as no-ops.
(unless (fboundp 'sb-int:/show)
  (defmacro sb-int:/show (&rest junk) (declare (ignore junk)))
  (setf (macro-function 'sb-int:/noshow) (macro-function 'sb-int:/show)
        (macro-function 'sb-int:/show0) (macro-function 'sb-int:/show)
        (macro-function 'sb-int:/noshow0) (macro-function 'sb-int:/show)))
