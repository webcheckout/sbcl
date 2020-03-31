;;;; This file contains definitions of VOPs used as internal markers by
;;;; the compiler. Since they don't emit any code, they should be
;;;; portable.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; This notes the place at which the environment is properly
;;; initialized, for debug-info purposes.
(define-vop (note-environment-start)
  (:info start-lab)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (emit-label start-lab)
    (note-debug-location vop start-lab :non-local-entry)))

;;; Call a move function. Used for register save/restore and spilling.
(define-vop (move-operand)
  (:args (x))
  (:results (y))
  (:info name)
  (:vop-var vop)
  (:generator 0
    (funcall (symbol-function name) vop x y)))

;;; Emit a label for sub-block (or sub-node) reference.
(define-vop (emit-label)
  (:info the-label)
  (:vop-var vop)
  (:generator 0
     (emit-label the-label)))

(define-vop (mark-covered)
  (:info path)
  (:generator 0
    (sb-assem:inst* 'sb-assem:.coverage-mark path)))
