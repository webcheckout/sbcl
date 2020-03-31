;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; the number of bits per byte, where a byte is the smallest
;;; addressable object
(defconstant n-byte-bits 8)

;;; the number of bits at the low end of a pointer used for type
;;; information
(defconstant n-lowtag-bits
  (integer-length (1- (/ (* 2 n-word-bits) n-byte-bits))))
;;; a mask to extract the low tag bits from a pointer
(defconstant lowtag-mask (1- (ash 1 n-lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(defconstant lowtag-limit (ash 1 n-lowtag-bits))
;;; the number of tag bits used for a fixnum
(defconstant n-fixnum-tag-bits
  ;; On 64-bit targets, this may be as low as 1 (for 63-bit
  ;; fixnums) and as high as 3 (for 61-bit fixnums).  The
  ;; constraint on the low end is that we need at least one bit
  ;; to determine if a value is a fixnum or not, and the
  ;; constraint on the high end is that it must not exceed
  ;; WORD-SHIFT (defined below) due to the use of unboxed
  ;; word-aligned byte pointers as boxed values in various
  ;; places.
  #+64-bit (or #+ppc64 3 #-ppc64 1)
  ;; On 32-bit targets, this may be as low as 2 (for 30-bit
  ;; fixnums) and as high as 2 (for 30-bit fixnums).  The
  ;; constraint on the low end is simple overcrowding of the
  ;; lowtag space, and the constraint on the high end is that it
  ;; must not exceed WORD-SHIFT.
  #-64-bit (1- n-lowtag-bits))
;;; the fixnum tag mask
(defconstant fixnum-tag-mask (1- (ash 1 n-fixnum-tag-bits)))
;;; the bit width of fixnums
(defconstant n-fixnum-bits (- n-word-bits n-fixnum-tag-bits))
;;; the bit width of positive fixnums
(defconstant n-positive-fixnum-bits (1- n-fixnum-bits))

;;; the number of bits to shift between word addresses and byte addresses
(defconstant word-shift (1- (integer-length (/ n-word-bits n-byte-bits))))

;;; the number of bytes in a word
(defconstant n-word-bytes (/ n-word-bits n-byte-bits))

;;; the number of bytes in a machine word
(defconstant n-machine-word-bytes (/ n-machine-word-bits n-byte-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(defconstant n-widetag-bits 8)
;;; a mask to extract the type from a data block header word
(defconstant widetag-mask (1- (ash 1 n-widetag-bits)))

(defconstant sb-xc:most-positive-fixnum
    (1- (ash 1 n-positive-fixnum-bits))
  "the fixnum closest in value to positive infinity")
(defconstant sb-xc:most-negative-fixnum
    (ash -1 n-positive-fixnum-bits)
  "the fixnum closest in value to negative infinity")

(defconstant most-positive-word (1- (expt 2 n-word-bits))
  "The most positive integer that is of type SB-EXT:WORD.")

(defconstant maximum-bignum-length
  ;; Compute number of bits in the maximum length's representation
  ;; leaving one bit for a GC mark bit.
  (ldb (byte (- n-word-bits n-widetag-bits 1) 0) -1))

(defconstant sb-xc:char-code-limit #-sb-unicode 256 #+sb-unicode #x110000
  "the upper exclusive bound on values produced by CHAR-CODE")

(defconstant base-char-code-limit #-sb-unicode 256 #+sb-unicode 128)

;;; the size of the chunks returned by RANDOM-CHUNK
(defconstant n-random-chunk-bits 32)

;;; Internal time format.
;;; 61 bits should give
;;; seventy-three million one hundred seventeen thousand eight hundred two years of runtime
;;; It's dangerous to run SBCL for that long without updating.
;;; And it'll be a fixnum on 64-bit targets.
;;; The result from querying get-internal-run-time with multiple cores
;;; running full tilt will exhaust this faster, but it's still plenty enough.
(defconstant sb-kernel::internal-time-bits 61)

(defconstant most-positive-exactly-single-float-fixnum
  (min (expt 2 single-float-digits) sb-xc:most-positive-fixnum))
(defconstant most-negative-exactly-single-float-fixnum
  (max (- (expt 2 single-float-digits)) sb-xc:most-negative-fixnum))
(defconstant most-positive-exactly-double-float-fixnum
  (min (expt 2 double-float-digits) sb-xc:most-positive-fixnum))
(defconstant most-negative-exactly-double-float-fixnum
  (max (- (expt 2 double-float-digits)) sb-xc:most-negative-fixnum))

;;;; Point where continuous area starting at dynamic-space-start bumps into
;;;; next space. Computed for genesis/constants.h, not used in Lisp.
#+(and gencgc sb-xc-host)
(defconstant max-dynamic-space-end
    (let ((stop (1- (ash 1 n-word-bits)))
          (start dynamic-space-start))
      (dolist (other-start (list read-only-space-start static-space-start
                                 #+linkage-table
                                 linkage-table-space-start))
        (declare (notinline <)) ; avoid dead code note
        (when (< start other-start)
          (setf stop (min stop other-start))))
      stop))

;; The lowest index that you can pass to %INSTANCE-REF accessing
;; a slot of data that is not the instance-layout.
;; To get a layout, you must call %INSTANCE-LAYOUT - don't assume index 0.
(defconstant instance-data-start (+ #-compact-instance-header 1))

;; The largest number that may appear in the header-data for an instance,
;; and some other mostly-boxed objects, such as FDEFNs.
;; This constraint exists because for objects managed by the immobile GC,
;; their generation number is stored in the header, so we have to know
;; how much to mask off to obtain the payload size.
;; Objects whose payload gets capped to this limit are considered
;; "short_boxed" objects in the sizetab[] array in 'gc-common'.
;; Additionally there are "tiny_boxed" objects, the payload length of
;; which can be expressed in 8 bits.
(defconstant short-header-max-words #x7fff)

;;; Is X a fixnum in the target Lisp?
#+sb-xc-host
(defun fixnump (x)
  (and (integerp x)
       (<= sb-xc:most-negative-fixnum x sb-xc:most-positive-fixnum)))

;;; Helper macro for defining FIXUP-CODE-OBJECT so that its body
;;; can be the same between the host and target.
;;; In the target, the byte offset supplied is relative to CODE-INSTRUCTIONS.
;;; Genesis works differently - it adjusts the offset so that it is relative
;;; to the containing gspace since that's what bvref requires.
(defmacro !with-bigvec-or-sap (&body body)
  `(macrolet #-sb-xc-host ()
             #+sb-xc-host
             ((code-instructions (code) `(sb-fasl::descriptor-mem ,code))
              (sap-int (sap)
                ;; KLUDGE: SAP is a bigvec; it doesn't know its address.
                ;; Note that this shadows the uncallable stub function for SAP-INT
                ;; that placates the host when compiling 'compiler/*/move.lisp'.
                (declare (ignore sap))
                `(locally
                     (declare (notinline sb-fasl::descriptor-gspace)) ; fwd ref
                   (sb-fasl::gspace-byte-address
                    (sb-fasl::descriptor-gspace code)))) ; use CODE, not SAP
              (sap-ref-8 (sap offset) `(sb-fasl::bvref-8 ,sap ,offset))
              (sap-ref-32 (sap offset) `(sb-fasl::bvref-32 ,sap ,offset))
              (sap-ref-64 (sap offset) `(sb-fasl::bvref-64 ,sap ,offset))
              (sap-ref-word (sap offset) `(sb-fasl::bvref-word ,sap ,offset)))
     ,@body))

#+sb-safepoint
;;; The offset from the fault address reported to the runtime to the
;;; END of the global safepoint page.
(defconstant gc-safepoint-trap-offset n-word-bytes)

#+sb-xc-host (deftype sb-xc:fixnum () `(signed-byte ,n-fixnum-bits))
