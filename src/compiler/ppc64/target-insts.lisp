;;;; target-only parts of the instruction set definition for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PPC64-ASM")

(defun current-instruction (dstate &optional (offset 0))
  (sap-ref-int (dstate-segment-sap dstate)
               (+ (dstate-cur-offs dstate) offset)
               4
               (dstate-byte-order dstate)))

(defun maybe-add-notes (regno dstate)
  (let ((inst (current-instruction dstate))
        (symbolic-inst (sb-disassem::dstate-inst dstate)))
    (case (sb-disassem::inst-name symbolic-inst)
      (lwz
       (when (= regno (ldb (byte 5 16) inst)) ; only for the second
         (case (ldb (byte 5 16) inst)
           ;; reg_CODE
           (#.sb-vm::code-offset
            (note-code-constant (ldb (byte 16 0) inst) dstate)))))
      (ld
       (when (= regno (ldb (byte 5 16) inst) sb-vm::code-offset)
         ;; FIXME: should be sign-extended, but we only access words
         ;; at positive displacement from reg_CODE.
         (let ((ofs (ash (ldb (byte 14 2) inst) 2))) ; D scaled form
           (note-code-constant ofs dstate))))
      (ldx
       (when (= regno (ldb (byte 5 16) inst) sb-vm::code-offset)
         (let ((ra (ldb (byte 5 11) inst))
               (prev (current-instruction dstate -4)))
           (when (and (= (ldb (byte 6 26) prev) 14) ;; addi
                      (= (ldb (byte 5 16) prev) 0)
                      (= (ldb (byte 5 21) prev) ra))
             (note-code-constant (ldb (byte 16 0) prev) dstate)))))
      (addi
       (when (= regno null-offset)
         (maybe-note-nil-indexed-object (ldb (byte 16 0) inst) dstate))))))

(defun unimp-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (xinstr-data chunk dstate)))
     (case trap
       (#.breakpoint-trap
        (nt "Breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "Pending interrupt trap"))
       (#.halt-trap
        (nt "Halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "Function end breakpoint trap"))
       (t
        (when (or (and (= trap cerror-trap) (nt "cerror trap"))
                  (>= trap error-trap))
          (handle-break-args #'snarf-error-junk trap stream dstate)))))))
