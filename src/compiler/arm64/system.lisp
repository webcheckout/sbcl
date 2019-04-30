;;;; ARM VM definitions of various system hacking operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Type frobbing VOPs

(define-vop (widetag-of)
  (:translate widetag-of)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types positive-fixnum)
  (:generator 6
    ;; First, pick off the immediate types, starting with FIXNUM.
    (inst ands result object fixnum-tag-mask)
    (inst b :eq done)
    ;; If it wasn't a fixnum, start with the full widetag.
    (inst and result object widetag-mask)

    ;; Now, we have our result for an immediate type, but we might
    ;; have a pointer object instead, in which case we need to do more
    ;; work.  Check for a pointer type, set two low-bits.

    (inst tbz object 1 done)
    ;; If we have a pointer type, we need to compute a different
    ;; answer.  For lists and instances, we just need the lowtag.  For
    ;; functions and "other", we need to load the widetag from the
    ;; object header.  In both cases, having just the widetag
    ;; available is handy.
    (inst and result object lowtag-mask)

    ;; We now have the correct answer for list-pointer-lowtag and
    ;; instance-pointer-lowtag, but need to pick off the case for the
    ;; other two pointer types.  KLUDGE: FUN-POINTER-LOWTAG and
    ;; OTHER-POINTER-LOWTAG are both in the upper half of the lowtag
    ;; space, while LIST-POINTER-LOWTAG and INSTANCE-POINTER-LOWTAG
    ;; are in the lower half, so we distinguish with a bit test.
    (inst tbz object 3 done)

    ;; We can't use both register and immediate offsets in the same
    ;; load/store instruction, so we need to bias our register offset
    ;; on big-endian systems.
    #+big-endian
    (inst sub result result (1- n-word-bytes))

    ;; And, finally, pick out the widetag from the header.
    (inst neg result result)
    (inst ldrb result (@ object result))
    done))

(define-vop (layout-depthoid)
  (:translate layout-depthoid)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (any-reg)))
  (:result-types fixnum)
  (:generator 1
   ;; I have not tested this for big-endian aarch but it looks right.
   (inst ldrsw value
         (@ object
            (load-store-offset
             (- (+ #+little-endian 4
                   (ash (+ instance-slots-offset
                           (get-dsd-index layout sb-kernel::%bits))
                        word-shift))
                instance-pointer-lowtag))))))

(define-vop (%other-pointer-widetag)
  (:translate %other-pointer-widetag)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result object (- other-pointer-lowtag))))

(define-vop (fun-subtype)
  (:translate fun-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- fun-pointer-lowtag))))

(define-vop (fun-header-data)
  (:translate fun-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 fun-pointer-lowtag)
    (inst lsr res res n-widetag-bits)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-lowtag)
    (inst lsr res res n-widetag-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
         (data :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1)
  (:generator 6
    (load-type t1 x (- other-pointer-lowtag))
    (sc-case data
      (any-reg
       (inst orr t1 t1 (lsl data (- n-widetag-bits n-fixnum-tag-bits))))
      (immediate
       (inst orr t1 t1 (logical-mask (ash (tn-value data) n-widetag-bits)))))
    (storew t1 x 0 other-pointer-lowtag)
    (move res x)))

(define-vop (pointer-hash)
  (:translate pointer-hash)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst and res ptr (dpb -1 (byte (- n-word-bits n-fixnum-tag-bits 1)
                                    n-fixnum-tag-bits) 0))))

;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-binding-stack-pointer int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))

;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    ;; 4 byte load, ignoring serial# in the high bits
    (inst ldr (32-bit-reg ndescr) (@ code (- 8 other-pointer-lowtag)))
    (inst add sap code ndescr)
    (inst sub sap sap other-pointer-lowtag)))

(define-vop (code-trailer-ref)
  (:translate code-trailer-ref)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0))
         (offset :scs (signed-reg) :to (:result 0)))
  (:arg-types * fixnum)
  (:results (res :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 10
    (inst ldr (32-bit-reg res) (@ code (- 4 other-pointer-lowtag)))
    (inst add res offset (lsl res word-shift))
    (inst sub res res other-pointer-lowtag)
    (inst ldr (32-bit-reg res) (@ code res))))

(define-vop (compute-fun)
  (:args (code :scs (descriptor-reg))
         (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (inst ldr (32-bit-reg ndescr) (@ code (- 8 other-pointer-lowtag)))
    (inst add ndescr offset ndescr)
    (inst sub ndescr ndescr (- other-pointer-lowtag fun-pointer-lowtag))
    (inst add func code ndescr)))
;;;
(define-vop (symbol-info-vector)
  (:policy :fast-safe)
  (:translate symbol-info-vector)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; If RES has list-pointer-lowtag, take its CDR. If not, use it as-is.
    (inst and temp res lowtag-mask)
    (inst cmp temp list-pointer-lowtag)
    (inst b :ne NE)
    (loadw res res cons-cdr-slot list-pointer-lowtag)
    NE))

(define-vop (symbol-plist)
  (:policy :fast-safe)
  (:translate symbol-plist)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:generator 1
    (loadw res x symbol-info-slot other-pointer-lowtag)
    ;; Instruction pun: (CAR x) is the same as (VECTOR-LENGTH x)
    ;; so if the info slot holds a vector, this gets a fixnum- it's not a plist.
    (loadw res res cons-car-slot list-pointer-lowtag)
    (inst tst res fixnum-tag-mask)
    (inst csel res null-tn res :eq)))

;;;; other miscellaneous VOPs

(defknown sb-unix::receive-pending-interrupt () (values))
(define-vop (sb-unix::receive-pending-interrupt)
  (:policy :fast-safe)
  (:translate sb-unix::receive-pending-interrupt)
  (:generator 1
    (inst brk pending-interrupt-trap)))

(define-vop (halt)
  (:generator 1
    (inst brk halt-trap)))

;;;; Dummy definition for a spin-loop hint VOP
(define-vop (spin-loop-hint)
  (:translate spin-loop-hint)
  (:policy :fast-safe)
  (:generator 0))

;;;

#+sb-thread
(progn
  (defun ldr-str-word-offset-encodable (x)
    (ldr-str-offset-encodable (ash x word-shift)))

  (define-vop (current-thread-offset-sap/c)
    (:results (sap :scs (sap-reg)))
    (:result-types system-area-pointer)
    (:translate current-thread-offset-sap)
    (:info n)
    (:arg-types (:constant (satisfies ldr-str-offset-encodable)))
    (:policy :fast-safe)
    (:generator 1
                (inst ldr sap (@ thread-tn (ash n word-shift)))))

  (define-vop (current-thread-offset-sap)
    (:results (sap :scs (sap-reg)))
    (:result-types system-area-pointer)
    (:translate current-thread-offset-sap)
    (:args (n :scs (signed-reg) :target sap))
    (:arg-types signed-num)
    (:policy :fast-safe)
    (:generator 2
                (inst ldr sap (@ thread-tn (extend n :lsl word-shift))))))

;;; Barriers
(define-vop (%compiler-barrier)
  (:policy :fast-safe)
  (:translate %compiler-barrier)
  (:generator 3))

(define-vop (%memory-barrier)
  (:policy :fast-safe)
  (:translate %memory-barrier)
  (:generator 3
    (inst dmb)))

(define-vop (%read-barrier)
  (:policy :fast-safe)
  (:translate %read-barrier)
  (:generator 3
    (inst dmb)))

(define-vop (%write-barrier)
  (:policy :fast-safe)
  (:translate %write-barrier)
  (:generator 3
    (inst dmb)))

(define-vop (%data-dependency-barrier)
  (:policy :fast-safe)
  (:translate %data-dependency-barrier)
  (:generator 3))
