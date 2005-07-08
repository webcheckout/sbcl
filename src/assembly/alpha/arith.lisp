;;;; stuff to handle simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

#+sb-assembling
(defun %static-fun-call (static-fun-name lip nargs ocfp)
  (inst ldq lip (static-fun-offset static-fun-name) null-tn)
  (inst li (fixnumize 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip))
  
(define-assembly-routine (generic-+
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check for fixnums
  (inst or x y temp)
  (inst and temp fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  (inst move x temp2)                    ; save a copy of x
  (inst addq x y res)

  ;; check for overflow.  this is a long and complicated process because
  ;; Alpha doesn't provide any condition bits or anything like that,
  ;; prefering instead to provide instructions which trap to the OS on
  ;; overflow.  trapping to the OS would cause all sorts of problems, I
  ;; think, so we do things the long way.
  (inst xor temp2 y temp)		; combine x and y's sign bit
  (inst blt temp DONE)                  ; they were not equal, get out of here

  ;; even if the sign bits were equal, the addition might not have
  ;; overflowed.  check the sign bit of the result.
  (inst xor res temp2 temp)		; combine res and x's sign bit
  (inst bge temp DONE)			; they were equal, get out of here
  
  ;; do the add the proper way
  (inst sra temp2 n-fixnum-tag-bits temp)
  (inst sra y n-fixnum-tag-bits temp2)
  (inst addq temp temp2 temp)

  ; from move-from-signed
  (with-fixed-allocation (res temp2 bignum-widetag (1+ bignum-digits-offset))
    (storew temp res bignum-digits-offset other-pointer-lowtag))

  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'two-arg-+ lip nargs ocfp))


(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
                          (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check for fixnums
  (inst or x y temp)
  (inst and temp fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  (inst move x temp2)
  (inst subq x y res)

  ;; check for overflow
  (inst xor y temp2 temp)               ; combine sign bits
  (inst bge temp DONE)                  ; they were equal, get out of here

  ;; check the sign bit of the result
  (inst xor res temp2 temp)		; combine sign bits
  (inst bge temp DONE)			; they were equal, get out of here

  ;; do the subtract the proper way
  (inst sra temp2 n-fixnum-tag-bits temp)
  (inst sra y n-fixnum-tag-bits temp2)
  (inst subq temp temp2 temp)

  ; from move-from-signed
  (with-fixed-allocation (res temp2 bignum-widetag (1+ bignum-digits-offset))
    (storew temp res bignum-digits-offset other-pointer-lowtag))

  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'two-arg-- lip nargs ocfp))


(define-assembly-routine (generic-*
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lo non-descriptor-reg nl1-offset)
			  (:temp hi non-descriptor-reg nl2-offset)
                          (:temp temp2 non-descriptor-reg nl3-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check for fixnums
  (inst or x y temp)
  (inst and temp fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  ;; Remove the tag from one arg so that the result will have the
  ;; correct fixnum tag.
  (inst sra x n-fixnum-tag-bits temp)

  ;; The Alpha is a little bit interesting because the 'umulh'
  ;; instruction, which generates the upper 64 bits of a 64x64 multiply,
  ;; does a *unsigned* multiplication.  This means that if you multiply
  ;; an appropriate negative number by an appropriate positive number,
  ;; the upper bits will *not* all be ones, as you might expect.  We use
  ;; a method from _Hacker's Delight_, page 133, by Henry S. Warren to
  ;; convert the unsigned multiplication into a signed multiplication.
  (inst mulq temp y res)                ; low-order bits
  (inst umulh temp y hi)                ; high-order bits
  (inst sra temp 63 lo)                 ; compute 't1' in Warren's algorithm
  (inst and lo y lo)
  (inst sra y 63 temp2)                 ; compute 't2' in Warren's algorithm
  (inst and temp2 temp temp2)
  (inst subq hi lo hi)                  ; compute 'p' in Warren's algorithm
  (inst subq hi temp2 hi)

  ;; 'hi' now contains the high bits of a signed 64x64 multiplication.
  ;; We may now proceed to see whether the multiplication overflowed as
  ;; in the MIPS version of this routine.

  ;; Check to see if the result will fit in a fixnum.  (i.e. the high
  ;; word is just 64 copies of the sign bit of the low word.)
  (inst sra res 63 temp)
  (inst xor temp hi temp)
  (inst beq temp DONE)

  ;; No go.  Shift the double word hi:res down three bits into hi:lo to get rid
  ;; of the fixnum tag.
  (inst srl res n-fixnum-tag-bits lo)
  (inst sll hi (- n-word-bits n-fixnum-tag-bits) temp)
  (inst or lo temp lo)
  (inst sra hi n-fixnum-tag-bits hi)
 
  ;; Do we need one word or two?  Assume two.
  (inst li (logior (ash 2 n-widetag-bits) bignum-widetag) temp2)
  (inst sra lo 63 temp)
  (inst xor temp hi temp)
  (inst bne temp two-words)

  ;; Only need one word, fix the header.
  (inst li (logior (ash 1 n-widetag-bits) bignum-widetag) temp2)

  ;; Allocate one word.
  (pseudo-atomic (:extra (pad-data-block (1+ bignum-digits-offset)))
    (inst or alloc-tn other-pointer-lowtag res)
    (storew temp2 res 0 other-pointer-lowtag))
  ;; Store one word
  (storew lo res bignum-digits-offset other-pointer-lowtag)
  ;; Out of here
  (lisp-return lra lip :offset 2)

  TWO-WORDS
  ;; Allocate two words.
  (pseudo-atomic (:extra (pad-data-block (+ 2 bignum-digits-offset)))
    (inst or alloc-tn other-pointer-lowtag res)
    (storew temp2 res 0 other-pointer-lowtag))
  ;; Store two words.
  (storew lo res bignum-digits-offset other-pointer-lowtag)
  (storew hi res (1+ bignum-digits-offset) other-pointer-lowtag)
  ;; out of here
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'two-arg-* lip nargs ocfp)

  DONE)


;;;; division

(define-assembly-routine (signed-truncate
			  (:note "(signed-byte 64) truncate")
			  (:cost 60)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types signed-num signed-num)
			  (:result-types signed-num signed-num))

			 ((:arg dividend signed-reg nl0-offset)
			  (:arg divisor signed-reg nl1-offset)

			  (:res quo signed-reg nl2-offset)
			  (:res rem signed-reg nl3-offset)

			  (:temp quo-sign signed-reg nl5-offset)
			  (:temp rem-sign signed-reg nargs-offset)
			  (:temp temp1 non-descriptor-reg nl4-offset))
  
  (let ((error (generate-error-code nil division-by-zero-error
				    dividend divisor)))
    (inst beq divisor error))

  (inst xor dividend divisor quo-sign)
  (inst move dividend rem-sign)
  (let ((label (gen-label)))
    (inst bge dividend label)
    (inst subq zero-tn dividend dividend)
    (emit-label label))
  (let ((label (gen-label)))
    (inst bge divisor label)
    (inst subq zero-tn divisor divisor)
    (emit-label label))
  (inst move zero-tn rem)
  (inst move zero-tn quo)

  (dotimes (i 64)
    (inst srl dividend 63 temp1)
    (inst sll rem 1 rem)
    (inst or temp1 rem rem)
    (inst cmple divisor rem temp1)
    (inst sll quo 1 quo)
    (inst or temp1 quo quo)
    (inst sll dividend 1 dividend)
    (inst subq temp1 1 temp1)
    (inst zap divisor temp1 temp1)
    (inst subq rem temp1 rem))

  (let ((label (gen-label)))
    ;; If the quo-sign is negative, we need to negate quo.
    (inst bge quo-sign label)
    (inst subq zero-tn quo quo)
    (emit-label label))
  (let ((label (gen-label)))
    ;; If the rem-sign is negative, we need to negate rem.
    (inst bge rem-sign label)
    (inst subq zero-tn rem rem)
    (emit-label label)))


;;;; comparison routines

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp not-p)
       `(define-assembly-routine (,name
				  (:cost 10)
				  (:return-style :full-call)
				  (:policy :safe)
				  (:translate ,translate)
				  (:save-p t))
				 ((:arg x (descriptor-reg any-reg) a0-offset)
				  (:arg y (descriptor-reg any-reg) a1-offset)
				  
				  (:res res descriptor-reg a0-offset)
				  
				  (:temp temp non-descriptor-reg nl0-offset)
				  (:temp lip interior-reg lip-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst and x fixnum-tag-mask temp)
	  (inst bne temp DO-STATIC-FUN)
	  (inst and y fixnum-tag-mask temp)
	  (inst beq temp DO-COMPARE)
	  
	  DO-STATIC-FUN
          (%static-fun-call ',static-fn lip nargs ocfp)
	  
	  DO-COMPARE
	  ,cmp
	  (inst move null-tn res)
	  (inst ,(if not-p 'bne 'beq) temp done)
	  (load-symbol res t)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< (inst cmplt x y temp) nil)
  (define-cond-assem-rtn generic-> > two-arg-> (inst cmplt y x temp) nil))


(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check easy case
  (inst cmpeq x y temp)
  (inst bne temp RETURN-T)

  ;; if X is a fixnum, then we're done (since it's not equal to Y)
  (inst and x fixnum-tag-mask temp)
  (inst beq temp RETURN-NIL)
  ;; if Y is a fixnum, then we're done; if not, we really need to compare
  (inst and y fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  RETURN-NIL
  (inst move null-tn res)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'eql lip nargs ocfp)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check for fixnums
  (inst or x y temp)
  (inst and temp fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  (inst cmpeq x y temp)
  (inst bne temp RETURN-T)

  (inst move null-tn res)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'two-arg-= lip nargs ocfp)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-/=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate /=)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; check for fixnums
  (inst or x y temp)
  (inst and temp fixnum-tag-mask temp)
  (inst bne temp DO-STATIC-FUN)

  (inst cmpeq x y temp)
  (inst bne temp RETURN-NIL)

  (load-symbol res t)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (%static-fun-call 'two-arg-= lip nargs ocfp)

  RETURN-NIL
  (inst move null-tn res))
