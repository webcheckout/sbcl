;;;; this file centralizes information about the array types
;;;; implemented by the system, where previously such information was
;;;; spread over several files.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defstruct (specialized-array-element-type-properties
            (:conc-name saetp-)
            (:constructor
             !make-saetp
             (specifier
              initial-element-default
              n-bits
              primitive-type-name
              &key (n-pad-elements 0) complex-typecode fixnum-p
              &aux (typecode
                    (symbol-value (symbolicate primitive-type-name "-WIDETAG")))))
            (:copier nil))
  ;; the element specifier, e.g. BASE-CHAR or (UNSIGNED-BYTE 4)
  ;; TYPE-SPECIFIER is too general - this doesn't allow CLASS/CLASSOID.
  (specifier (missing-arg) :type (or symbol list) :read-only t)
  ;; the element type, e.g. #<BUILT-IN-CLASS BASE-CHAR (sealed)> or
  ;; #<SB-KERNEL:NUMERIC-TYPE (UNSIGNED-BYTE 4)>
  (ctype nil :type (or ctype null))
  ;; true if the elements are tagged fixnums
  (fixnum-p nil :type boolean :read-only t)
  ;; what we get when the low-level vector-creation logic zeroes all
  ;; the bits (which also serves as the default value of MAKE-ARRAY's
  ;; :INITIAL-ELEMENT keyword)
  (initial-element-default (missing-arg) :read-only t)
  ;; how many bits per element
  (n-bits (missing-arg) :type index :read-only t)
  ;; the low-level type code (aka "widetag")
  (typecode (missing-arg) :type index :read-only t)
  ;; if an integer, a typecode corresponding to a complex vector
  ;; specialized on this element type.
  (complex-typecode nil :type (or index null) :read-only t)
  ;; the name of the primitive type of data vectors specialized on
  ;; this type
  (primitive-type-name (missing-arg) :type symbol :read-only t)
  ;; the number of extra elements we use at the end of the array for
  ;; low level hackery (e.g., one element for arrays of BASE-CHAR,
  ;; which is used for a fixed #\NULL so that when we call out to C
  ;; we don't need to cons a new copy)
  (n-pad-elements (missing-arg) :type index :read-only t))
(declaim (freeze-type specialized-array-element-type-properties))

(define-load-time-global *specialized-array-element-type-properties*
  (map 'simple-vector
       (lambda (args)
         (apply #'!make-saetp args))
       `(;; Erm.  Yeah.  There aren't a lot of things that make sense
         ;; for an initial element for (ARRAY NIL). -- CSR, 2002-03-07
         (nil #:mu 0 simple-array-nil
              :complex-typecode #.complex-vector-nil-widetag)
         #-sb-unicode
         (character ,(code-char 0) 8 simple-base-string
                    ;; (SIMPLE-BASE-STRINGs are stored with an extra
                    ;; trailing #\NULL for convenience in calling out
                    ;; to C.)
                    :n-pad-elements 1
                    :complex-typecode #.complex-base-string-widetag)
         #+sb-unicode
         (base-char ,(code-char 0) 8 simple-base-string
                    ;; (SIMPLE-BASE-STRINGs are stored with an extra
                    ;; trailing #\NULL for convenience in calling out
                    ;; to C.)
                    :n-pad-elements 1
                    :complex-typecode #.complex-base-string-widetag)
         #+sb-unicode
         (character ,(code-char 0) 32 simple-character-string
                    :n-pad-elements 1
                    :complex-typecode #.complex-character-string-widetag)
         (single-float $0.0f0 32 simple-array-single-float)
         (double-float $0.0d0 64 simple-array-double-float)
         (bit 0 1 simple-bit-vector
              :complex-typecode #.complex-bit-vector-widetag)
         ;; KLUDGE: The fact that these UNSIGNED-BYTE entries come
         ;; before their SIGNED-BYTE partners is significant in the
         ;; implementation of the compiler; some of the cross-compiler
         ;; code (see e.g. COERCE-TO-SMALLEST-ELTYPE in
         ;; src/compiler/debug-dump.lisp) attempts to create an array
         ;; specialized on (UNSIGNED-BYTE FOO), where FOO could be 7;
         ;; (UNSIGNED-BYTE 7) is SUBTYPEP (SIGNED-BYTE 8), so if we're
         ;; not careful we could get the wrong specialized array when
         ;; we try to FIND-IF, below. -- CSR, 2002-07-08
         ((unsigned-byte 2) 0 2 simple-array-unsigned-byte-2)
         ((unsigned-byte 4) 0 4 simple-array-unsigned-byte-4)
         ((unsigned-byte 7) 0 8 simple-array-unsigned-byte-7)
         ((unsigned-byte 8) 0 8 simple-array-unsigned-byte-8)
         ((unsigned-byte 15) 0 16 simple-array-unsigned-byte-15)
         ((unsigned-byte 16) 0 16 simple-array-unsigned-byte-16)
         #-64-bit
         ((unsigned-byte #.n-positive-fixnum-bits)
          0 32 simple-array-unsigned-fixnum
          :fixnum-p t)
         ((unsigned-byte 31) 0 32 simple-array-unsigned-byte-31)
         ((unsigned-byte 32) 0 32 simple-array-unsigned-byte-32)
         #+64-bit
         ((unsigned-byte #.n-positive-fixnum-bits)
          0 64 simple-array-unsigned-fixnum
          :fixnum-p t)
         #+64-bit
         ((unsigned-byte 63) 0 64 simple-array-unsigned-byte-63)
         #+64-bit
         ((unsigned-byte 64) 0 64 simple-array-unsigned-byte-64)
         ((signed-byte 8) 0 8 simple-array-signed-byte-8)
         ((signed-byte 16) 0 16 simple-array-signed-byte-16)
         ;; KLUDGE: See the comment in PRIMITIVE-TYPE-AUX,
         ;; compiler/generic/primtype.lisp, for why this is FIXNUM and
         ;; not (SIGNED-BYTE 30)
         #-64-bit
         (fixnum 0 32 simple-array-fixnum :fixnum-p t)
         ((signed-byte 32) 0 32 simple-array-signed-byte-32)
         ;; KLUDGE: see above KLUDGE for the 32-bit case
         #+64-bit
         (fixnum 0 64 simple-array-fixnum :fixnum-p t)
         #+64-bit
         ((signed-byte 64) 0 64 simple-array-signed-byte-64)
         ((complex single-float) ,(complex $0f0 $0f0) 64
          simple-array-complex-single-float)
         ((complex double-float) ,(complex $0d0 $0d0) 128
          simple-array-complex-double-float)
         #+long-float
         ((complex long-float) ,(complex $0L0 $0L0) #+x86 192 #+sparc 256
          simple-array-complex-long-float)
         (t 0 #.n-word-bits simple-vector))))

;; The compiler can see that the number of types that must be present in a
;; union of arrays to convert (OR (array t1) ... (array tN)) to (ARRAY *)
;; is a constant if (LENGTH *SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*) is
;; a constant. So proclaim the type of the global var. This works because
;; the compiler doesn't retroactively try to check the initializer of NIL.
#-sb-xc-host
(declaim (type (simple-vector
                #.(length *specialized-array-element-type-properties*))
               *specialized-array-element-type-properties*))

(defun valid-bit-bash-saetp-p (saetp)
  ;; BIT-BASHing isn't allowed on simple vectors that contain pointers
  (and (not (eq t (saetp-specifier saetp)))
       ;; Disallowing (VECTOR NIL) also means that we won't transform
       ;; sequence functions into bit-bashing code and we let the
       ;; generic sequence functions signal errors if necessary.
       (not (zerop (saetp-n-bits saetp)))
       ;; Due to limitations with the current BIT-BASHing code, we can't
       ;; BIT-BASH reliably on arrays whose element types are larger
       ;; than the word size.
       (<= (saetp-n-bits saetp) n-word-bits)))

(define-load-time-global *vector-without-complex-typecode-infos*
  #+sb-xc-host
  (loop for saetp across *specialized-array-element-type-properties*
        for specifier = (saetp-specifier saetp)
        unless (saetp-complex-typecode saetp)
        collect (list (if (atom specifier)
                          (intern (format nil "VECTOR-~A-P" specifier))
                          ;; at the moment, all specialized array
                          ;; specifiers are either atoms or
                          ;; two-element lists.
                          (intern (format nil "VECTOR-~A-~A-P" (car specifier) (cadr specifier))))
                      specifier))
  #-sb-xc-host
  '#.*vector-without-complex-typecode-infos*)

;;; Return the shift amount needed to turn length into bits
(defun saetp-n-bits-shift (saetp)
  (max (1- (integer-length (saetp-n-bits saetp)))
       0)) ;; because of NIL

(defun saetp-index-or-lose (element-type)
  (or (position element-type sb-vm:*specialized-array-element-type-properties*
                :key #'sb-vm:saetp-specifier :test #'equal)
      (error "No saetp for ~S" element-type)))

(in-package "SB-C")

(defun find-saetp (element-type)
  (find element-type sb-vm:*specialized-array-element-type-properties*
        :key #'sb-vm:saetp-specifier :test #'equal))

(defun find-saetp-by-ctype (ctype)
  (find ctype sb-vm:*specialized-array-element-type-properties*
        :key #'sb-vm:saetp-ctype :test #'csubtypep))
