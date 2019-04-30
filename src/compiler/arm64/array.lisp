;;;; array operations for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
         (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg) pa-flag ndescr)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (descriptor-reg)))
  (:generator 5
    ;; Compute the allocation size.
    (inst lsl ndescr rank (- word-shift n-fixnum-tag-bits))
    (inst add ndescr ndescr (+ (* array-dimensions-offset n-word-bytes)
                               lowtag-mask))
    (inst and ndescr ndescr (bic-mask lowtag-mask))
    (pseudo-atomic (pa-flag)
      (allocation header ndescr other-pointer-lowtag :flag-tn pa-flag :lip lip)
      ;; Now that we have the space allocated, compute the header
      ;; value.
      (inst lsl ndescr rank (- n-widetag-bits n-fixnum-tag-bits))
      (inst add ndescr ndescr (ash (1- array-dimensions-offset) n-widetag-bits))
      (inst orr ndescr ndescr (lsr type n-fixnum-tag-bits))
      ;; And store the header value.
      (storew ndescr header 0 other-pointer-lowtag))
    (move result header)))

(define-vop (make-array-header/c)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:arg-types (:constant t) (:constant t))
  (:info type rank)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (descriptor-reg)))
  (:generator 4
    (let* ((header-size (+ rank
                           (1- array-dimensions-offset)))
           (bytes (logandc2 (+ (* (1+ header-size) n-word-bytes)
                               lowtag-mask)
                            lowtag-mask))
           (header-bits (logior (ash header-size
                                     n-widetag-bits)
                                type)))
      (pseudo-atomic (pa-flag)
        (allocation header bytes other-pointer-lowtag :flag-tn pa-flag :lip lip)
        (load-immediate-word pa-flag header-bits)
        (storew pa-flag header 0 other-pointer-lowtag)))
    (move result header)))

;;;; Additional accessors and setters for the array header.
(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-lowtag
  (any-reg) positive-fixnum sb-kernel:%set-array-dimension)

(define-vop (array-rank-vop)
  (:translate sb-kernel:%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-lowtag)
    (inst asr temp temp n-widetag-bits)
    (inst sub temp temp (1- array-dimensions-offset))
    (inst lsl res temp n-fixnum-tag-bits)))

;;;; Bounds checking routine.
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg constant))
         (bound :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is bound immediate)
                                   (not (sc-is index immediate))
                                   (typep (tn-value bound)
                                          '(and sc-offset
                                            (satisfies fixnum-add-sub-immediate-p))))))
         (index :scs (any-reg descriptor-reg)
                :load-if (not (and (sc-is index immediate)
                                   (typep (tn-value index)
                                          '(and sc-offset
                                            (satisfies fixnum-add-sub-immediate-p)))))))
  (:variant-vars %test-fixnum)
  (:variant t)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index))
          (bound (if (sc-is bound immediate)
                     (let ((value (tn-value bound)))
                       (cond ((and %test-fixnum
                                   (power-of-two-limit-p (1- value)))
                              (lognot (fixnumize (1- value))))
                             ((sc-is index any-reg descriptor-reg)
                              (fixnumize value))
                             (t
                              value)))
                     bound))
          (index (if (sc-is index immediate)
                     (let ((value (tn-value index)))
                       (if (sc-is bound any-reg descriptor-reg)
                           (fixnumize value)
                           value))
                     index)))
      (cond ((eql bound -1)
             (inst cbnz index error))
            ((typep bound '(integer * -1))
             ;; Power of two bound, can be checked for fixnumness at
             ;; the same time as it always occupies a consecutive bit
             ;; range, everything else, including the tag, has to be
             ;; zero.
             (inst tst index bound)
             (inst b :ne error))
            (t
             (when (and %test-fixnum (not (integerp index)))
               (%test-fixnum index nil error t))
             (cond ((integerp bound)
                    (inst cmp index bound)
                    (inst b :hs error))
                   (t
                    (inst cmp bound index)
                    (inst b :ls error))))))))

(define-vop (check-bound/fast check-bound)
  (:policy :fast)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/fixnum check-bound)
  (:args (array)
         (bound)
         (index :scs (any-reg)))
  (:arg-types * * tagged-num)
  (:variant nil)
  (:variant-cost 4))

(define-vop (check-bound/untagged check-bound)
  (:args (array)
         (bound :scs (unsigned-reg signed-reg))
         (index :scs (unsigned-reg signed-reg)))
  (:arg-types * (:or unsigned-num signed-num)
                (:or unsigned-num signed-num))
  (:variant nil)
  (:variant-cost 5))
;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, 32, or 64 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-lowtag
       ,scs
       ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-lowtag ,scs ,element-type
       data-vector-set)))

           (def-partial-data-vector-frobs (type element-type size signed &rest scs)
  `(progn
     (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       ,size ,signed vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-ref)
     (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       ,size vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-set))))

  (def-full-data-vector-frobs simple-vector *
    descriptor-reg any-reg)

  (def-partial-data-vector-frobs simple-base-string character
    :byte nil character-reg)
  #+sb-unicode
  (def-partial-data-vector-frobs simple-character-string character
    :word nil character-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum
    :byte nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    :byte nil unsigned-reg signed-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum
    :short nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    :short nil unsigned-reg signed-reg)

  (def-partial-data-vector-frobs simple-array-unsigned-byte-31 positive-fixnum
    :word nil unsigned-reg signed-reg)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-32 positive-fixnum
    :word nil unsigned-reg signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-byte-63 unsigned-num
    unsigned-reg)

  (def-full-data-vector-frobs simple-array-unsigned-byte-64 unsigned-num
    unsigned-reg)

  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num
    :byte t signed-reg)

  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num
    :short t signed-reg)

  (def-partial-data-vector-frobs simple-array-signed-byte-32 tagged-num
    :word t signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum
    any-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num
    any-reg)

  (def-full-data-vector-frobs simple-array-signed-byte-64 signed-num
    signed-reg))

;;; Integer vectors whose elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
(macrolet ((def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor n-word-bits bits))
         (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate "DATA-VECTOR-REF/" type))
         (:note "inline array access")
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:arg-types ,type positive-fixnum)
         (:results (value :scs (any-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
         (:generator 20
           ;; Compute the offset for the word we're interested in.
           (inst lsr temp index ,bit-shift)
           ;; Load the word in question.
           (inst add lip object (lsl temp word-shift))
           (inst ldr result (@ lip
                               (- (* vector-data-offset n-word-bytes)
                                  other-pointer-lowtag)))
           ;; Compute the position of the bitfield we need.
           (inst and temp index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst eor temp temp ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst lsl temp temp ,(1- (integer-length bits)))))
           ;; Shift the field we need to the low bits of RESULT.
           (inst lsr result result temp)
           ;; Mask out the field we're interested in.
           (inst and result result ,(1- (ash 1 bits)))
           ;; And fixnum-tag the result.
           (inst lsl value result n-fixnum-tag-bits)))
       (define-vop (,(symbolicate "DATA-VECTOR-REF/" type "-C"))
         (:note "inline array access")
         (:translate data-vector-ref)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type (:constant index))
         (:results (value :scs (any-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (non-descriptor-reg) :to (:result 0)) result)
         (:generator 15
           (multiple-value-bind (index bit) (floor index ,elements-per-word)
             (inst ldr result (@ object
                                 (load-store-offset
                                  (+ (* index n-word-bytes)
                                     (- (* vector-data-offset n-word-bytes)
                                        other-pointer-lowtag)))))
             (inst ubfm result result (* bit ,bits) (+ (* bit ,bits) (1- ,bits)))
             (inst lsl value result n-fixnum-tag-bits))))
       (define-vop (,(symbolicate "DATA-VECTOR-SET/" type))
         (:note "inline array store")
         (:translate data-vector-set)
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg) :target shift)
                (value :scs (unsigned-reg immediate) :target result))
         (:arg-types ,type positive-fixnum positive-fixnum)
         (:results (result :scs (unsigned-reg)))
         (:result-types positive-fixnum)
         (:temporary (:scs (interior-reg)) lip)
         (:temporary (:scs (non-descriptor-reg)) temp old)
         (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
         (:generator 25
           ;; Compute the offset for the word we're interested in.
           (inst lsr temp index ,bit-shift)
           ;; Load the word in question.
           (inst add lip object (lsl temp word-shift))
           (inst ldr old (@ lip
                            (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))
           ;; Compute the position of the bitfield we need.
           (inst and shift index ,(1- elements-per-word))
           ,@(when (eq *backend-byte-order* :big-endian)
               `((inst eor shift ,(1- elements-per-word))))
           ,@(unless (= bits 1)
               `((inst lsl shift shift ,(1- (integer-length bits)))))
           ;; Clear the target bitfield.
           (unless (and (sc-is value immediate)
                        (= (tn-value value) ,(1- (ash 1 bits))))
             (inst mov temp ,(1- (ash 1 bits)))
             (inst lsl temp temp shift)
             (inst bic old old temp))
           ;; LOGIOR in the new value (shifted appropriatly).
           (sc-case value
             (immediate
              (inst mov temp (logand (tn-value value) ,(1- (ash 1 bits)))))
             (unsigned-reg
              (inst and temp value ,(1- (ash 1 bits)))))
           (inst lsl temp temp shift)
           (inst orr old old temp)
           ;; Write the altered word back to the array.
           (inst str old (@ lip
                            (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))
           ;; And present the result properly.
           (sc-case value
             (immediate
              (inst mov result (tn-value value)))
             (unsigned-reg
              (move result value)))))))))
  (def-small-data-vector-frobs simple-bit-vector 1)
  (def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
  (def-small-data-vector-frobs simple-array-unsigned-byte-4 4))

;;; And the float variants.
(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types single-float)
  (:generator 5
    (inst lsl offset index (1- (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst lsl offset index (1- (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))
    (unless (location= result value)
      (inst fmov result value))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))
    (unless (location= result value)
      (inst fmov result value))))

;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
              complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))
    (unless (location= result value)
      (inst fmov result value))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst ldr value (@ object offset))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
              complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst lsl offset index (1+ (- word-shift n-fixnum-tag-bits)))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst str value (@ object offset))
    (unless (location= result value)
      (inst s-mov result value))))

;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
(define-full-reffer vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %vector-raw-bits)
(define-full-setter set-vector-raw-bits * vector-data-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num %set-vector-raw-bits)

(define-vop (%compare-and-swap-svref word-index-cas)
  (:note "inline array compare-and-swap")
  (:policy :fast-safe)
  (:variant vector-data-offset other-pointer-lowtag)
  (:translate %compare-and-swap-svref)
  (:arg-types simple-vector positive-fixnum * *))

(define-vop (array-atomic-incf/word)
  (:translate %array-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg) :target offset)
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) offset)
  (:temporary (:sc non-descriptor-reg) sum)
  (:temporary (:sc interior-reg) lip)
  (:generator 4
    (inst lsl offset index (- word-shift n-fixnum-tag-bits))
    (inst add offset offset (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag))
    (inst add lip object offset)

    (inst dsb)
    LOOP
    (inst ldxr result lip)
    (inst add sum result diff)
    (inst stlxr tmp-tn sum lip)
    (inst cbnz tmp-tn LOOP)
    (inst dmb)))
