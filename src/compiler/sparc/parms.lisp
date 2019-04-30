;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant sb-assem:assem-scheduler-p t)
(defconstant sb-assem:+inst-alignment-bytes+ 4)
(defconstant sb-assem:+assem-max-locations+ 100)

(defconstant +backend-fasl-file-implementation+ :sparc)
(defconstant +backend-page-bytes+ 8192)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(defconstant gencgc-card-bytes +backend-page-bytes+)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(defconstant gencgc-release-granularity +backend-page-bytes+)

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

;;; flags for the generational garbage collector
(defconstant pseudo-atomic-interrupted-flag 1)
(defconstant pseudo-atomic-flag
    ;; Must be (ash 1 (1- sb-vm:n-lowtag-bits)) for cheneygc ALLOCATION.
    4)

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

;;; CMUCL COMMENT:
;;;   X These values are for the x86 80 bit format and are no doubt
;;;   incorrect for the sparc.
;;; FIXME
(defconstant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

;;; This looks wrong - CSR
(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 30) #'equalp)        ; RD
(defconstant-eqx float-sticky-bits (byte 5 5) #'equalp)   ; aexc
(defconstant-eqx float-traps-byte (byte 5 23) #'equalp)   ; TEM
(defconstant-eqx float-exceptions-byte (byte 5 0) #'equalp)       ; cexc

;;; According to the SPARC doc (as opposed to FPU doc), the fast mode
;;; bit (EFM) is "reserved", and should always be zero.  However, for
;;; sparc-V8 and sparc-V9, it appears to work, causing denormals to
;;; be truncated to 0 silently.
(defconstant float-fast-bit (ash 1 22))

); eval-when


;;;; Description of the target address space.

#+gencgc ; sensibly small read-only and static spaces
(!gencgc-space-setup #x0f800000 :dynamic-space-start #x30000000)

;;; Where to put the different spaces.  Must match the C code!
#+(and linux cheneygc)
(progn
  (defconstant linkage-table-space-start #x0f800000)
  (defconstant linkage-table-space-end   #x10000000)

  (defconstant read-only-space-start     #x11000000)
  (defconstant read-only-space-end       #x15000000)

  (defconstant static-space-start        #x28000000)
  (defconstant static-space-end          #x2c000000)

  (defparameter dynamic-0-space-start #x30000000)
  (defparameter dynamic-0-space-end   #x38000000))

#+(and sunos cheneygc) ; might as well start by trying the same numbers
(progn
  (defconstant linkage-table-space-start #x0f800000)
  (defconstant linkage-table-space-end   #x10000000)

  (defconstant read-only-space-start     #x11000000)
  (defconstant read-only-space-end       #x15000000)

  (defconstant static-space-start        #x28000000)
  (defconstant static-space-end          #x2c000000)

  (defparameter dynamic-0-space-start    #x30000000)
  (defparameter dynamic-0-space-end      #x38000000))

#+(and netbsd cheneygc) ; Need a gap at 0x4000000 for shared libraries
(progn
  (defconstant linkage-table-space-start #x0f800000)
  (defconstant linkage-table-space-end   #x10000000)

  (defconstant read-only-space-start     #x11000000)
  (defconstant read-only-space-end       #x15000000)

  (defconstant static-space-start        #x18000000)
  (defconstant static-space-end          #x1c000000)

  (defparameter dynamic-0-space-start    #x48000000)
  (defparameter dynamic-0-space-end      #x5ffff000))

;; Size of one linkage-table entry in bytes. See comment in
;; src/runtime/sparc-arch.c
(defconstant linkage-table-entry-size 16)


(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  after-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  #+gencgc allocation-trap
  error-trap)

;;;; static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defconstant-eqx +static-symbols+
  `#(,@+common-static-symbols+)
  #'equalp)

(defconstant-eqx +static-fdefns+
  #(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor two-arg-eqv
    two-arg-gcd two-arg-lcm)
  #'equalp)

;;;; Pseudo-atomic trap number

;;; KLUDGE: Linux on the SPARC doesn't seem to conform to any kind of
;;; standards at all. So we use an explicitly undefined trap, because
;;; that currently does the right thing. Expect this to break
;;; eventually (but with luck, at that point we'll be able to revert
;;; to the compliant trap number...
;;;
;;; KLUDGE: Maybe this should be called pseudo-atomic-magic-number,
;;; allowing other architectures (which don't necessarily use traps
;;; for pseudo-atomic) to propagate a magic number to C land via
;;; sbcl.h.
#-linux
(defconstant pseudo-atomic-trap #x10)
#+linux
(defconstant pseudo-atomic-trap #x40)
