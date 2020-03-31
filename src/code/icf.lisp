;;;; Identical Code Folding (similar to what might be done by a C linker)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;

(in-package "SB-VM")

;;; Deduce equivalence between compiled code components so that all but
;;; one canonical representative of each can be eliminated.
;;;
;;; Unlike in C++ where this operation can be sometimes safe,
;;; and tools try to distinguish between safe and unsafe folding (see [*])
;;; in Lisp it is * NEVER * truly safe. But we can opt to do it anyway.
;;; The reason it is not safe is that we have no way of knowing whether
;;; some random bit of code will later execute something like:
;;;   (ASSERT (NOT (EQ (EVAL `#',(THING)) (FDEFINITION OTHERTHING))))
;;; to check some assumed invariant that would have been valid prior to folding
;;; but becomes invalid if function pointers are unexpectedly changed
;;; without the application's knowlege.
;;; (And needless to say, this affects EQ-based hashing which we take care of)
;;;
;;; [*] https://ai.google/research/pubs/pub36912
;;;

;;; Return T if any pointers were replaced in a code object.
(defun apply-forwarding-map (map print &aux any-change)
  (when print
    (let ((*print-pretty* nil))
      (dohash ((k v) map)
        (format t "~s = ~s~%" k v))))
  (flet ((forward (object)
           (typecase object
             (code-component (gethash object map object))
             (simple-fun
              (let* ((code (fun-code-header object))
                     (new (gethash code map)))
                (if new
                    (%primitive sb-c:compute-fun
                                new
                                (- (%fun-code-offset object)
                                   (ash (code-header-words new)
                                        word-shift)))
                    object)))
             (t object))))
    (map-allocated-objects
     (lambda (object widetag size &aux touchedp)
       (declare (ignore size))
       (macrolet ((rewrite (place &aux (accessor (if (listp place) (car place))))
                    ;; These two slots have no setters, but nor are
                    ;; they possibly affected by code folding.
                    (unless (member accessor '(symbol-name symbol-package))
                      `(let* ((oldval ,place) (newval (forward oldval)))
                         (unless (eq newval oldval)
                           ,(case accessor
                              (data-vector-ref
                               `(setf (svref ,@(cdr place)) newval touchedp t))
                              (value-cell-ref
                               ;; pinned already because we're iterating over the heap
                               ;; which disables GC, but maybe some day it won't.
                               `(with-pinned-objects (object)
                                  (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address object))
                                                         (- (ash value-cell-value-slot word-shift)
                                                            other-pointer-lowtag))
                                        newval)))
                              (weak-pointer-value
                               ;; Preserve gencgc invariant that a weak pointer
                               ;; can't point to an object younger than itself.
                               `(cond #+gencgc
                                      ((let ((newval-gen (generation-of newval)))
                                         (and (fixnump newval-gen)
                                              (< newval-gen (generation-of object))))
                                       #+nil
                                       (warn "Can't update weak pointer ~s" object))
                                      (t
                                       (with-pinned-objects (object)
                                         (setf (sap-ref-lispobj
                                                (int-sap (get-lisp-obj-address object))
                                                (- (ash weak-pointer-value-slot word-shift)
                                                   other-pointer-lowtag))
                                               newval)))))
                              (%primitive
                               (ecase (cadr place)
                                 (fast-symbol-global-value
                                  `(setf (symbol-global-value ,@(cddr place)) newval))))
                              (t
                               `(setf ,place newval)))
                           t)))))
         (do-referenced-object (object rewrite)
           (simple-vector
            :extend
            (when (and (logtest (get-header-data object) vector-addr-hashing-subtype)
                       touchedp)
              (setf (svref object 1) 1)))
           (code-component
            :override
            ;; We must perform replacements inside the raw bytes, otherwise GC lossage
            ;; could result. e.g. suppose the header contains #<FDEFN FOO> which points
            ;; to #<FOO>, and a machine instruction contains "CALL #<FOO>". If the ICF
            ;; pass replaces #<FOO> with #<BAR>, the instruction needs to change, because
            ;; if it didn't, there would be no traceable pointer from code to #<FOO>.
            (machine-code-icf object #'forward map print)
            (loop for i from code-constants-offset below (code-header-words object)
                  do (when (rewrite (code-header-ref object i))
                       (setq any-change t))))
           (fdefn
            :override
            (let* ((oldval (fdefn-fun object))
                   (newval (forward oldval)))
              (unless (eq newval oldval)
                (setf (fdefn-fun object) newval))))
           (closure
            :override
            (let* ((oldval (%closure-fun object))
                   (newval (forward oldval)))
              (unless (eq newval oldval)
                (with-pinned-objects (object newval)
                  (setf (sap-ref-sap (int-sap (- (get-lisp-obj-address object)
                                                 fun-pointer-lowtag))
                                     (ash closure-fun-slot word-shift))
                        (simple-fun-entry-sap newval)))))
            (dotimes (i (1- (get-closure-length object)))
              (let* ((oldval (%closure-index-ref object i))
                     (newval (forward oldval)))
                (unless (eq newval oldval)
                  (with-pinned-objects (object)
                    (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address object))
                                           (- (ash (+ i closure-info-offset) word-shift)
                                              fun-pointer-lowtag))
                          newval))))))
           (ratio :override)
           ((complex rational) :override)
           (t
            :extend
            (case widetag
              (#.value-cell-widetag
               (rewrite (value-cell-ref object)))
              (t
               (bug "Unknown object type #x~x addr=~x"
                    widetag (get-lisp-obj-address object))))))))
     :all))
  any-change)

;;; Compare function signatures, which are essentially the raw instruction bytes
;;; but with encodings of relative operands replaced by constant filler bytes,
;;; and a list of offset/value pairs corresponding to the smashed bytes
;;; that can be meaningfully compared (i.e. after un-relativization).
(defun fun-signature= (signature1 signature2)
  (let ((v1 (cdr signature1))
        (v2 (cdr signature2))
        (words1 (car signature1))
        (words2 (car signature2)))
    (declare (type (simple-array word 1) words1 words2)
             (type simple-vector v1 v2))
    (and (= (length v1) (length v2))
         (every #'eql v1 v2)
         (= (length words1) (length words2))
         (= 0 (alien-funcall
               (extern-alien "memcmp"
                (function int system-area-pointer system-area-pointer long))
               (vector-sap words1)
               (vector-sap words2)
               (* (length words1) n-word-bytes))))))

#-x86-64
(progn
(defun compute-code-signature (code dstate)
  (declare (ignore dstate))
  code)
(defun machine-code-icf (code mapper replacements print)
  (declare (ignore code mapper replacements print))))

(defun code-equivalent-p (obj1 obj2 &aux (code1 (car obj1)) (code2 (car obj2)))
  (declare (ignorable code1 code2))
  ;; The comparator is conceptually target-independent, however since
  ;; COMPUTE-CODE-SIGNATURE is stubbed out for most targets,
  ;; there is no meaningful way to compare functions.
  ;; Rather than conditionalize out the entirety of this file,
  ;; we'll conservatively say that no two blobs of code are equivalent
  ;; if there is no signature function.
  #+x86-64
  (with-pinned-objects (obj1 obj2)
    (and (= (code-header-words code1) (code-header-words code2))
         (= (code-n-entries code1) (code-n-entries code2))
         (= (%code-code-size code1) (%code-code-size code2))
         (= (code-n-unboxed-data-bytes code1)
            (code-n-unboxed-data-bytes code2))
         ;; Compare boxed constants. Ignore debug-info, fixups, and all
         ;; the simple-fun metadata (name, args, type, info) which will be compared
         ;; later based on how similar we require them to be.
         (loop for i from (+ code-constants-offset
                             (* code-slots-per-simple-fun (code-n-entries code1)))
               below (code-header-words code1)
               always (eq (code-header-ref code1 i) (code-header-ref code2 i)))
         ;; jump table word contains serial# which is arbitrary; don't compare it
         (= (code-jump-table-words code1) (code-jump-table-words code2))
         ;; Compare unboxed constants less 1 word which was already compared
         (let ((nwords (1- (ceiling (code-n-unboxed-data-bytes code1) n-word-bytes)))
               (sap1 (sap+ (code-instructions code1) n-word-bytes))
               (sap2 (sap+ (code-instructions code2) n-word-bytes)))
           (dotimes (i nwords t)
             (unless (= (sap-ref-word sap1 (ash i word-shift))
                        (sap-ref-word sap2 (ash i word-shift)))
               (return nil))))
         ;; Compare instruction bytes
         (every #'fun-signature= (cdr obj1) (cdr obj2))
         ;; Require that %SIMPLE-FUN-TYPE and %SIMPLE-FUN-LEXPR
         ;; satisfy EQUAL. The former because the compiler can introspect
         ;; to determine type in the absence of a proclamation; the latter
         ;; because FUNCTION-LAMBDA-EXPRESSION might be called by users.
         ;; Since most functions do not store the lambda expression, that
         ;; constraint should not impede much folding.
         ;; We should be able to fold a function that had no expression
         ;; with one that did, as long as the choice function chooses
         ;; the one that saved the expression. (Not done yet)
         (dotimes (i (code-n-entries code1) t)
           (let ((f1 (%code-entry-point code1 i))
                 (f2 (%code-entry-point code2 i)))
           (unless (and (equal (%simple-fun-type f1) (%simple-fun-type f2))
                        (equal (%simple-fun-lexpr f1) (%simple-fun-lexpr f2)))
             (return nil)))))))

(defun code/doc-equivalent-p (obj1 obj2)
  (and (code-equivalent-p obj1 obj2)
       (let ((code1 (car obj1)) (code2 (car obj2)))
         (dotimes (i (code-n-entries code1) t)
           (unless (equal (%simple-fun-doc (%code-entry-point code1 i))
                          (%simple-fun-doc (%code-entry-point code2 i)))
             (return nil))))))

;;; Compute a key for binning purposes.
(defun compute-code-hash-key (code)
  (with-pinned-objects (code)
    (list* (code-header-words code)
           (collect ((offs))
             (dotimes (i (code-n-entries code) (offs))
               (offs (%code-fun-offset code i)
                     (%simple-fun-text-len (%code-entry-point code i) i))))
           ;; Ignore the debug-info, fixups, and simple-fun metadata.
           ;; (Same things that are ignored by the CODE-EQUIVALENT-P predicate)
           (loop for i from (+ code-constants-offset
                               (* code-slots-per-simple-fun (code-n-entries code)))
                   below (code-header-words code)
                   collect (code-header-ref code i)))))

(declaim (inline default-allow-icf-p))
(defun default-allow-icf-p (code)
  (let ((name (sb-c::compiled-debug-info-name
               (%code-debug-info code))))
    (or (and (stringp name)
             (string= name "check-type"))
        (and (consp name)
             (member (car name)
                     ;; Every print-object method which is just a call
                     ;; to PRINT-UNREADABLE-OBJECT can be folded into one
                     ;; function (for matching values of :type and :identity).
                     ;; Perhaps we should recognize certain common idioms
                     ;; at compile-time, and give back a known function?
                     '(sb-pcl::fast-method
                       ;; This next one suggests that PCL isn't doing
                       ;; a good enough job of using FNGEN.
                       sb-pcl::emf
                       sb-c::vop
                       sb-c::deftransform
                       :source-transform))))))

(defun fold-identical-code (&key aggressive preserve-docstrings print)
  (loop
    #+gencgc (gc :gen 7)
    ;; Pass 1: count code objects.  I'd like to enhance MAP-ALLOCATED-OBJECTS
    ;; to have a mode that scans only GC pages with that can hold code
    ;; (or any subset of page types). This is fine though.
    (let ((code-objects
           (let ((count 0))
             (map-allocated-objects
              (lambda (obj widetag size)
                (declare (ignore size))
                (when (and (eql widetag code-header-widetag)
                           (plusp (code-n-entries obj)))
                  (incf count)))
              :all)
             (make-array count)))
          (referenced-objects
           (make-hash-table :test #'eq)))
      ;; Pass 2: collect them.
      (let ((i 0))
        (map-allocated-objects
         (lambda (obj widetag size)
           (declare (ignore size))
           (when (and (eql widetag code-header-widetag)
                      (plusp (code-n-entries obj)))
             (setf (aref code-objects i) obj)
             (incf i)))
         :all))
      (unless aggressive
        ;; Figure out which of those are referenced by any object
        ;; except for an fdefn.
        (flet ((visit (referent referer)
                 (when (typep referent 'simple-fun)
                   (setq referent (fun-code-header referent)))
                 (when (and (typep referent 'code-component)
                            (plusp (code-n-entries referent))
                            (not (gethash referent referenced-objects)))
                   (setf (gethash referent referenced-objects) referer))))
          ;; Scan the whole heap, and look at each object for pointers
          ;; to any code object.
          (map-allocated-objects
           (lambda (referer widetag size)
             (declare (ignore size))
             (unless (or (eql widetag fdefn-widetag)
                         (weak-vector-p referer))
               (do-referenced-object (referer visit referer)
                 ;; maybe this should be the default fallback?
                 (t
                  :extend
                  (case (widetag-of referer)
                    (#.value-cell-widetag
                     (visit (value-cell-ref referer) referer))
                    (t
                     (bug "Unknown object type #x~x ~s" widetag referer)))))))
           :all)))
      ;; Now place objects that possibly match into the same bin.
      (let ((bins (make-hash-table :test 'equal))
            (n 0))
        (dovector (x code-objects)
          (when (or (not (gethash x referenced-objects))
                    (default-allow-icf-p x))
            (incf n)
            (push x (gethash (compute-code-hash-key x) bins))))
        (when print
          (format t "ICF: ~d objects, ~d candidates, ~d bins~%"
                  (length code-objects) n (hash-table-count bins)))
        ;; Scan each bin that has more than one object in it
        ;; and check whether any are equivalent to one another.
        (let ((equiv-map (make-hash-table :test #'eq))
              (dstate (sb-disassem:make-dstate)))
          (dohash ((key objects) bins)
            (declare (ignore key))
            ;; Skip if there can not possibly be an equivalence class
            ;; with more than one thing in it.
            (when (cdr objects)
              (let (equivalences)
                (setq objects
                      (mapcar (lambda (x) (cons x (compute-code-signature x dstate)))
                              objects))
                (dolist (item objects)
                  (let ((found (assoc item equivalences
                                      :test (if preserve-docstrings
                                                #'code/doc-equivalent-p
                                                #'code-equivalent-p))))
                    (if found
                        (push item (cdr found))
                        (push (list item) equivalences))))
                (dolist (set equivalences)
                  (when (cdr set) ; have two or more, so choose one as canonical
                    (setq set (mapcar #'car set))
                    (let ((winner
                            (labels ((exported-p (x)
                                       (let* ((entry (%code-entry-point x 0))
                                              (name (and entry
                                                         (sb-kernel:%fun-name entry))))
                                         (and (symbolp name)
                                              (eq (nth-value 1 (find-symbol (symbol-name name)
                                                                            (symbol-package name)))
                                                  :external))))
                                     (keyfn (x)
                                       (%code-serialno x))
                                     (compare (a b)
                                       (let ((exported-a (exported-p a))
                                             (exported-b (exported-p b) ))
                                         (cond ((and exported-a
                                                     (not exported-b))
                                                a)
                                               ((and exported-b
                                                     (not exported-a))
                                                b)
                                               ((< (keyfn a) (keyfn b))
                                                a)
                                               (t
                                                b)))))
                             (reduce #'compare set))))
                      (dolist (obj (delete winner set))
                        (setf (gethash obj equiv-map) winner))))))))
          (unless (apply-forwarding-map equiv-map print)
            (return)))))))
