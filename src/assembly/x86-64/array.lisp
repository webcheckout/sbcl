;;;; various array operations that are too expensive (in space) to do
;;;; inline

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Fill 'vector' with 'item', unrolling the loop, and taking care
;;; to deal with pre- and post-loop pieces for proper alignment.
;;; Alternatively, if the CPU has the enhanced MOVSB feature, use REP STOS
;;; depending on the number of elements to be written.
(define-assembly-routine (vector-fill/t
                          (:translate vector-fill/t)
                          (:policy :fast-safe))
                         ((:arg  vector (descriptor-reg) rdx-offset)
                          (:arg  item   (any-reg descriptor-reg) rax-offset)
                          (:arg  start  (any-reg descriptor-reg) rdi-offset)
                          (:arg  end    (any-reg descriptor-reg) rsi-offset)
                          (:res  res    (descriptor-reg) rdx-offset)
                          (:temp count unsigned-reg rcx-offset)
                          ;; storage class doesn't matter since all float regs
                          ;; and sse regs map to the same storage base.
                          (:temp wordpair double-reg float0-offset))
  (move res vector) ; to "use" res
  (move count end)
  (inst sub count start)
  ;; 'start' and 'limit' will be interior pointers into 'vector',
  ;; but 'vector' is pinned because it's in a register, so this is ok.
  ;; If we had a precise GC we'd want to keep start and limit as offsets
  ;; because we couldn't tie them both to the vector.
  (inst lea start (ea (- (ash vector-data-offset word-shift) other-pointer-lowtag)
                      vector start (ash 1 (- word-shift n-fixnum-tag-bits))))
  ;; REP STOS has a fixed cost that makes it suboptimal below
  ;; a certain fairly high threshold - about 350 objects in my testing.
  (inst cmp count (fixnumize 350))

  ;; *** tune_asm_routines_for_microarch() will replace this unconditional
  ;;     "JMP UNROLL" with "JL UNROLL" after the core file is parsed,
  ;;     if STOS is deemed to be preferable on this cpu.
  ;;     Otherwise we'll always jump over the REP STOS instruction.
  ;;     The preceding CMP is pointless in that case, but harmless.
  (inst jmp unroll)

  (inst shr count n-fixnum-tag-bits)
  (inst rep)
  (inst stos item)
  DONE
  (inst ret)
  UNROLL
  (inst test count count)
  (inst jmp :z DONE)
  ;; if address ends in 8, we must write 1 word before using MOVDQA
  (inst test :byte start #b1000)
  (inst jmp :z SETUP)
  (inst mov (ea start) item)
  (inst add start n-word-bytes)
  (inst sub count (fixnumize 1))
  SETUP
  ;; Compute (FLOOR COUNT 8) to compute the number of fast iterations.
  (inst shr count (+ 3 n-fixnum-tag-bits)) ; It's a native integer now.
  ;; For a very small number of elements, the unrolled loop won't execute.
  (inst jmp :z FINISH)
  ;; Load the xmm register.
  (inst movq wordpair item)
  (inst pshufd wordpair wordpair #b01000100)
  ;; Multiply count by 64 (= 8 lisp objects) and add to 'start'
  ;; to get the upper limit of the loop.
  (inst shl count 6)
  (inst add count start)
  ;; MOVNTDQ is supposedly faster, but would require a trailing SFENCE
  ;; which measurably harms performance on a small number of iterations.
  UNROLL-LOOP ; Write 4 double-quads = 8 lisp objects
  (inst movdqa (ea  0 start) wordpair)
  (inst movdqa (ea 16 start) wordpair)
  (inst movdqa (ea 32 start) wordpair)
  (inst movdqa (ea 48 start) wordpair)
  (inst add start (* 8 n-word-bytes))
  (inst cmp start count)
  (inst jmp :b UNROLL-LOOP)
  FINISH
  ;; Now recompute 'count' as the ending address
  (inst lea count (ea (- (ash vector-data-offset word-shift) other-pointer-lowtag)
                      vector
                      end (ash 1 (- word-shift n-fixnum-tag-bits))))
  (inst cmp start count)
  (inst jmp :ae DONE)
  FINAL-LOOP
  (inst mov (ea start) item)
  (inst add start n-word-bytes)
  (inst cmp start count)
  (inst jmp :b FINAL-LOOP))
