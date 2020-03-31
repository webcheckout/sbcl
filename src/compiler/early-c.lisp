;;;; This file contains compiler code and compiler-related stuff which
;;;; can be built early on. Some of the stuff may be here because it's
;;;; needed early on, some other stuff (e.g. constants) just because
;;;; it might as well be done early so we don't have to think about
;;;; whether it's done early enough.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; An OPAQUE-BOX instance is used to pass data from IR1 to IR2 as
;;; a quoted object in a "source form" (not user-written) such that the
;;; contained object is in a for-evaluation position but ignored by
;;; the compiler's constant-dumping logic. In addition to this structure
;;; type, a few other IR1 object types are implicitly opaque.
(defstruct (opaque-box (:constructor opaquely-quote (value))
                       (:copier nil)
                       (:predicate opaque-box-p))
  value)
(declaim (freeze-type opaque-box))

;;; ANSI limits on compilation
(defconstant sb-xc:call-arguments-limit sb-xc:most-positive-fixnum
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including &REST args.")
(defconstant sb-xc:lambda-parameters-limit sb-xc:most-positive-fixnum
  "The exclusive upper bound on the number of parameters which may be specified
  in a given lambda list. This is actually the limit on required and &OPTIONAL
  parameters. With &KEY and &AUX you can get more.")
(defconstant sb-xc:multiple-values-limit sb-xc:most-positive-fixnum
  "The exclusive upper bound on the number of multiple VALUES that you can
  return.")

;;;; cross-compiler-only versions of CL special variables, so that we
;;;; don't have weird interactions with the host compiler

(defvar sb-xc:*compile-file-pathname*)
(defvar sb-xc:*compile-file-truename*)
(defvar sb-xc:*compile-print*)
(defvar sb-xc:*compile-verbose*)

;;;; miscellaneous types used both in the cross-compiler and on the target

;;;; FIXME: The INDEX and LAYOUT-DEPTHOID definitions probably belong
;;;; somewhere else, not "early-c", since they're after all not part
;;;; of the compiler.

;;; the type of LAYOUT-DEPTHOID and LAYOUT-LENGTH values.
;;; Each occupies two bytes of the %BITS slot when possible,
;;; otherwise a slot unto itself.
(def!type layout-depthoid () '(integer -1 #x7FFF))
(def!type layout-length () '(integer 0 #xFFFF))
(def!type layout-bitmap ()
  ;; FIXME: Probably should exclude negative bignum
  #+compact-instance-header 'integer
  #-compact-instance-header '(and integer (not (eql 0))))

;;; An INLINEP value describes how a function is called. The values
;;; have these meanings:
;;;     NIL     No declaration seen: do whatever you feel like, but don't
;;;             dump an inline expansion.
;;; NOTINLINE  NOTINLINE declaration seen: always do full function call.
;;;    INLINE  INLINE declaration seen: save expansion, expanding to it
;;;             if policy favors.
;;; MAYBE-INLINE
;;;             Retain expansion, but only use it opportunistically.
;;;             MAYBE-INLINE is quite different from INLINE. As explained
;;;             by APD on #lisp 2005-11-26: "MAYBE-INLINE lambda is
;;;             instantiated once per component, INLINE - for all
;;;             references (even under #'without FUNCALL)."
(def!type inlinep ()
  '(member inline maybe-inline notinline nil))

(defstruct (dxable-args (:constructor make-dxable-args (list))
                        (:predicate nil)
                        (:copier nil))
  (list nil :read-only t))
(defstruct (inlining-data (:include dxable-args)
                          (:constructor make-inlining-data (expansion list))
                          (:predicate nil)
                          (:copier nil))
  (expansion nil :read-only t))
(declaim (freeze-type dxable-args))

(defstruct (ir1-namespace (:conc-name "") (:copier nil) (:predicate nil))
  ;; FREE-VARS translates from the names of variables referenced
  ;; globally to the LEAF structures for them.
  (free-vars (make-hash-table :test 'eq) :read-only t :type hash-table)
  ;; FREE-FUNS is like FREE-VARS, only it deals with function names.
  (free-funs (make-hash-table :test 'equal) :read-only t :type hash-table)
  ;; These hashtables translate from constants to the LEAFs that
  ;; represent them.
  ;; Table 1: one entry for each distinct constant (according to object identity)
  (eq-constants (make-hash-table :test 'eq) :read-only t :type hash-table)
  ;; Table 2: one hash-table entry per EQUAL constant,
  ;; with the caveat that lookups must discriminate amongst constants that
  ;; are EQUAL but not similar.  The value in the hash-table is a list of candidates
  ;; (#<constant1> #<constant2> ... #<constantN>) such that CONSTANT-VALUE
  ;; of each is EQUAL to the key for the hash-table entry, but dissimilar
  ;; from each other. Notably, strings of different element types can't be similar.
  (similar-constants (make-hash-table :test 'equal) :read-only t :type hash-table))
(declaim (freeze-type ir1-namespace))

(sb-impl::define-thread-local *ir1-namespace*)
(declaim (type ir1-namespace *ir1-namespace*))

;;; *ALLOW-INSTRUMENTING* controls whether we should allow the
;;; insertion of instrumenting code (like a (CATCH ...)) around code
;;; to allow the debugger RETURN and STEP commands to function (we
;;; disallow it for internal stuff).
(defvar *allow-instrumenting*)

;;; miscellaneous forward declarations
#+sb-dyncount (defvar *collect-dynamic-statistics*)
(defvar *component-being-compiled*)
(defvar *compiler-error-context*)
(defvar *compiler-error-count*)
(defvar *compiler-warning-count*)
(defvar *compiler-style-warning-count*)
(defvar *compiler-note-count*)
;;; Bind this to a stream to capture various internal debugging output.
(defvar *compiler-trace-output* nil)
;;; These are the default, but the list can also include
;;; :pre-ir2-optimize and :symbolic-asm.
(defvar *compile-trace-targets* '(:ir1 :ir2 :vop :symbolic-asm :disassemble))
(defvar *constraint-universe*)
(defvar *current-path*)
(defvar *current-component*)
(defvar *delayed-ir1-transforms*)
#+sb-dyncount
(defvar *dynamic-counts-tn*)
(defvar *elsewhere-label*)
(defvar *event-note-threshold*)
(defvar *failure-p*)
(defvar *source-info*)
(defvar *source-plist*)
(defvar *source-namestring*)
(defvar *undefined-warnings*)
(defvar *warnings-p*)
(defvar *lambda-conversions*)
(defvar *compile-object* nil)
(defvar *location-context* nil)

(defvar *stack-allocate-dynamic-extent* t
  "If true (the default), the compiler respects DYNAMIC-EXTENT declarations
and stack allocates otherwise inaccessible parts of the object whenever
possible. Potentially long (over one page in size) vectors are, however, not
stack allocated except in zero SAFETY code, as such a vector could overflow
the stack without triggering overflow protection.")

;;; This lock is seized in the compiler, and related areas -- like the
;;; classoid/layout/class system.
;;; Assigning a literal object enables genesis to dump and load it
;;; without need of a cold-init function.
#-sb-xc-host
(!define-load-time-global **world-lock** (sb-thread:make-mutex :name "World Lock"))

#-sb-xc-host
(define-load-time-global *static-linker-lock*
    (sb-thread:make-mutex :name "static linker"))

(defmacro with-world-lock (() &body body)
  #+sb-xc-host `(progn ,@body)
  #-sb-xc-host `(sb-thread:with-recursive-lock (**world-lock**) ,@body))

;;;; miscellaneous utilities

;;; This is for "observers" who want to know if type names have been added.
;;; Rather than registering listeners, they can detect changes by comparing
;;; their stored nonce to the current nonce. Additionally the observers
;;; can detect whether function definitions have occurred.
#-sb-xc-host
(progn (declaim (fixnum *type-cache-nonce*))
       (!define-load-time-global *type-cache-nonce* 0))

(defstruct (undefined-warning
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t)
                               (prin1 (undefined-warning-name x) s))))
            (:copier nil))
  ;; the name of the unknown thing
  (name nil :type (or symbol list))
  ;; the kind of reference to NAME
  (kind (missing-arg) :type (member :function :type :variable))
  ;; the number of times this thing was used
  (count 0 :type unsigned-byte)
  ;; a list of COMPILER-ERROR-CONTEXT structures describing places
  ;; where this thing was used. Note that we only record the first
  ;; *UNDEFINED-WARNING-LIMIT* calls.
  (warnings () :type list))
(declaim (freeze-type undefined-warning))

;;; Delete any undefined warnings for NAME and KIND. This is for the
;;; benefit of the compiler, but it's sometimes called from stuff like
;;; type-defining code which isn't logically part of the compiler.
(declaim (ftype (function ((or symbol cons) keyword) (values))
                note-name-defined))
(defun note-name-defined (name kind)
  #-sb-xc-host (atomic-incf *type-cache-nonce*)
  ;; We do this BOUNDP check because this function can be called when
  ;; not in a compilation unit (as when loading top level forms).
  (when (boundp '*undefined-warnings*)
    (let ((name (uncross name)))
      (setq *undefined-warnings*
            (delete-if (lambda (x)
                         (and (equal (undefined-warning-name x) name)
                              (eq (undefined-warning-kind x) kind)))
                       *undefined-warnings*))))
  (values))

;;; to be called when a variable is lexically bound
(declaim (ftype (function (symbol) (values)) note-lexical-binding))
(defun note-lexical-binding (symbol)
    ;; This check is intended to protect us from getting silently
    ;; burned when we define
    ;;   foo.lisp:
    ;;     (DEFVAR *FOO* -3)
    ;;     (DEFUN FOO (X) (+ X *FOO*))
    ;;   bar.lisp:
    ;;     (DEFUN BAR (X)
    ;;       (LET ((*FOO* X))
    ;;         (FOO 14)))
    ;; and then we happen to compile bar.lisp before foo.lisp.
  (when (looks-like-name-of-special-var-p symbol)
    ;; FIXME: should be COMPILER-STYLE-WARNING?
    (style-warn 'asterisks-around-lexical-variable-name
                :format-control
                "using the lexical binding of the symbol ~
                 ~/sb-ext:print-symbol-with-prefix/, not the~@
                 dynamic binding"
                :format-arguments (list symbol)))
  (values))

;;; This is DEF!STRUCT so that when SB-C:DUMPABLE-LEAFLIKE-P invokes
;;; SB-XC:TYPEP in make-host-2, it does not need need to signal PARSE-UNKNOWN
;;; for each and every constant seen up until this structure gets defined.
(def!struct (debug-name-marker (:print-function print-debug-name-marker)
                               (:copier nil)))

(defvar *debug-name-level* 4)
(defvar *debug-name-length* 12)
(defvar *debug-name-punt*)
(define-load-time-global *debug-name-sharp* (make-debug-name-marker))
(define-load-time-global *debug-name-ellipsis* (make-debug-name-marker))

(defun print-debug-name-marker (marker stream level)
  (declare (ignore level))
  (cond ((eq marker *debug-name-sharp*)
         (write-char #\# stream))
        ((eq marker *debug-name-ellipsis*)
         (write-string "..." stream))
        (t
         (write-string "???" stream))))

(declaim (ftype (sfunction () list) name-context))
(defun debug-name (type thing &optional context)
  (let ((*debug-name-punt* nil))
    (labels ((walk (x)
               (typecase x
                 (cons
                  (if (plusp *debug-name-level*)
                      (let ((*debug-name-level* (1- *debug-name-level*)))
                        (do ((tail (cdr x) (cdr tail))
                             (name (cons (walk (car x)) nil)
                                   (cons (walk (car tail)) name))
                             (n (1- *debug-name-length*) (1- n)))
                            ((or (not (consp tail))
                                 (not (plusp n))
                                 *debug-name-punt*)
                             (cond (*debug-name-punt*
                                    (setf *debug-name-punt* nil)
                                    (nreverse name))
                                   ((atom tail)
                                    (nconc (nreverse name) (walk tail)))
                                   (t
                                    (setf *debug-name-punt* t)
                                    (nconc (nreverse name) (list *debug-name-ellipsis*)))))))
                      *debug-name-sharp*))
                 ((or symbol number string)
                  x)
                 (t
                  (type-of x)))))
      (let ((name (list* type (walk thing) (when context (name-context)))))
        (when (legal-fun-name-p name)
          (bug "~S is a legal function name, and cannot be used as a ~
                debug name." name))
        name))))

;;; Bound during eval-when :compile-time evaluation.
(defvar *compile-time-eval* nil)
(declaim (always-bound *compile-time-eval*))

#-immobile-code (defmacro code-immobile-p (thing) `(progn ,thing nil))

;;; Various error-code generating helpers
(defvar *adjustable-vectors*)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
                   (make-array 16
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t))))
     ;; Don't declare the length - if it gets adjusted and pushed back
     ;; onto the freelist, it's anyone's guess whether it was expanded.
     ;; This code was wrong for >12 years, so nobody must have needed
     ;; more than 16 elements. Maybe we should make it nonadjustable?
     (declare (type (vector (unsigned-byte 8)) ,var))
     (setf (fill-pointer ,var) 0)
     ;; No UNWIND-PROTECT here - semantics are unaffected by nonlocal exit,
     ;; and this macro is about speeding up the compiler, not slowing it down.
     ;; GC will clean up any debris, and since the vector does not point
     ;; to anything, even an accidental promotion to a higher generation
     ;; will not cause transitive garbage retention.
     (prog1 (progn ,@body)
       (push ,var *adjustable-vectors*))))


;;; The allocation quantum for boxed code header words.
;;; 2 implies an even length boxed header; 1 implies no restriction.
(defconstant code-boxed-words-align (+ 2 #+(or x86 x86-64) -1))

;;; Used as the CDR of the code coverage instrumentation records
;;; (instead of NIL) to ensure that any well-behaving user code will
;;; not have constants EQUAL to that record. This avoids problems with
;;; the records getting coalesced with non-record conses, which then
;;; get mutated when the instrumentation runs. Note that it's
;;; important for multiple records for the same location to be
;;; coalesced. -- JES, 2008-01-02
(defconstant +code-coverage-unmarked+ '%code-coverage-unmarked%)

;;; Stores the code coverage instrumentation results.
;;; The CAR is a hashtable. The CDR is a list of weak pointers to code objects
;;; having coverage marks embedded in the unboxed constants.
;;; Keys in the hashtable are namestrings, the
;;; value is a list of (CONS PATH STATE), where STATE is +CODE-COVERAGE-UNMARKED+
;;; for a path that has not been visited, and T for one that has.
#-sb-xc-host
(progn
  (define-load-time-global *code-coverage-info*
    (list (make-hash-table :test 'equal :synchronized t)))
  (declaim (type (cons hash-table) *code-coverage-info*)))

(deftype id-array ()
  '(and (array t (*))
        ;; Might as well be as specific as we can.
        ;; Really it should be (satisfies array-has-fill-pointer-p)
        ;; but that predicate is not total (errors on NIL).
        ;; And who knows what the host considers "simple".
        #-sb-xc-host (not simple-array)))

(defstruct (compilation (:copier nil)
                        (:predicate nil)
                        (:conc-name ""))
  (fun-names-in-this-file)
  ;; for constant coalescing across code components, and/or for situations
  ;; where SIMILARP does not do what you want.
  (constant-cache)
  (coverage-metadata nil :type (or (cons hash-table hash-table) null) :read-only t)
  (msan-unpoison nil :read-only t)
  (sset-counter 1 :type fixnum)
  ;; if emitting a cfasl, the fasl stream to that
  (compile-toplevel-object nil :read-only t)
  ;; The current block compilation state.  These are initialized to
  ;; the :Block-Compile and :Entry-Points arguments that COMPILE-FILE
  ;; was called with.  Subsequent START-BLOCK or END-BLOCK
  ;; declarations alter the values.
  (block-compile nil :type (member nil t :specified))
  (entry-points nil :type list)
  ;; When block compiling, used by PROCESS-FORM to accumulate top
  ;; level lambdas resulting from compiling subforms. (In reverse
  ;; order.)
  (toplevel-lambdas nil :type list)

  ;; Bidrectional map between IR1/IR2/assembler abstractions and a corresponding
  ;; small integer or string identifier. One direction could be done by adding
  ;; the ID as slot to each object, but we want both directions.
  ;; These could just as well be scoped by WITH-IR1-NAMESPACE, but
  ;; since it's primarily a debugging tool, it's nicer to have
  ;; a wider unique scope by ID.
  (objmap-obj-to-id      (make-hash-table :test 'eq) :read-only t)
  (objmap-id-to-node     nil :type (or null id-array)) ; number -> NODE
  (objmap-id-to-comp     nil :type (or null id-array)) ; number -> COMPONENT
  (objmap-id-to-leaf     nil :type (or null id-array)) ; number -> LEAF
  (objmap-id-to-cont     nil :type (or null id-array)) ; number -> CTRAN or LVAR
  (objmap-id-to-ir2block nil :type (or null id-array)) ; number -> IR2-BLOCK
  (objmap-id-to-tn       nil :type (or null id-array)) ; number -> TN
  (objmap-id-to-label    nil :type (or null id-array)) ; number -> LABEL
  )
(declaim (freeze-type compilation))

(sb-impl::define-thread-local *compilation*)
(declaim (type compilation *compilation*))

(in-package "SB-ALIEN")

;;; Information describing a heap-allocated alien.
(def!struct (heap-alien-info (:copier nil))
  ;; The type of this alien.
  (type (missing-arg) :type alien-type)
  ;; Its name.
  (alien-name (missing-arg) :type simple-string)
  ;; Data or code?
  (datap (missing-arg) :type boolean))
(!set-load-form-method heap-alien-info (:xc :target))

;; from 'llvm/projects/compiler-rt/lib/msan/msan.h':
;;  "#define MEM_TO_SHADOW(mem) (((uptr)(mem)) ^ 0x500000000000ULL)"
#+linux ; shadow space differs by OS
(defconstant sb-vm::msan-mem-to-shadow-xor-const #x500000000000)
