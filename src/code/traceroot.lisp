;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-EXT")

;;; Not loaded until warm build. package-data-list only affects symbols
;;; that are visible to genesis.
(export '(search-roots))

(define-alien-variable  "gc_object_watcher" unsigned)
(define-alien-variable  "gc_traceroot_criterion" int)

#+sb-thread
(defun find-symbol-from-tls-index (index)
  (unless (zerop index)
    ;; Search interned symbols first since that's probably enough
    (do-all-symbols (symbol)
      (when (= (sb-kernel:symbol-tls-index symbol) index)
        (return-from find-symbol-from-tls-index symbol)))
    ;; A specially bound uninterned symbol? how awesome
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:symbol-widetag)
                  (= (sb-kernel:symbol-tls-index obj) index))
         (return-from find-symbol-from-tls-index obj)))
     :all))
  0)

(defun find-lisp-thread-from-thread-struct (addr)
  ;; It is of course possible to do this without consing the list
  ;; of all threads, but I don't care.
  (dolist (thread (sb-thread:list-all-threads))
    (when (= (sb-thread::thread-primitive-thread thread) addr)
      (return thread))))

;;; Convert each path to (TARGET . NODES)
;;; where the first node in NODES is one of:
;;;
;;;  1. :static ; if a heap object (even if not "technically" static)
;;;  2. :pinned ; if unknown thread stack pins the root of the path
;;;  3. (#<thread>|thread-name symbol boolean)
;;;  4. (#<thread>|thread-name guessed-PC)
;;;
;;; For case 3, the boolean value is T if the symbol's current value in TLS
;;; is the root; otherwise a shadowed value on the binding stack is the root.
;;; For case 4, the guessed-PC is a probable code address within a function
;;; whose stack frame contains a reference to the first object in the path.
;;
;;; The rest of the path is a list of (OBJECT . WORD-INDEX).
(defun preprocess-traceroot-results (inputs outputs &aux results)
  (dovector (path outputs (nreverse results))
    (let ((target (weak-pointer-value (pop inputs))))
      (when (and target (listp path))
        (setq path (nreverse path))
        (let* ((root (car path))
               (root-kind (car root)))
          (if (eq root-kind 0) ; heap object
              (rplaca path :static)
              (let* ((thread (find-lisp-thread-from-thread-struct
                              (ash (cadr root) sb-vm:n-fixnum-tag-bits)))
                     (extra (cddr root))
                     (symbol
                      (unless (eql root-kind 1)
                        #+sb-thread
                        (find-symbol-from-tls-index (ash extra sb-vm:n-fixnum-tag-bits))
                        #-sb-thread
                        extra)))
                (awhen (and thread (sb-thread:thread-name thread))
                  (setq thread it)) ; if nameless, just show as #<thread ...>
                (rplaca path (ecase root-kind
                               (1                   ; control stack
                                (if thread
                                    `(,thread ,extra)
                                    :pinned))
                               (2 `(,thread ,symbol nil)) ; binding stack
                               (3 `(,thread ,symbol t))))))) ; TLS
        (push (cons target path) results)))))

(defun print-traceroot-path (path &key (stream *standard-output*) (multiline t))
  (destructuring-bind (target root . rest) path
    (cond (multiline
           (format stream "Path to ~S:~%" target)
           (cond ((eq root :static))
                 ((eq root :pinned)
                  ;; found in pins table, but thread unknown,
                  ;; as happens when run without GC
                  (format stream "from pinned object~%"))
                 ((symbolp (second root))
                  (destructuring-bind (thread symbol currentp) root
                    (format stream "from ~S ~S (~:[binding~;TLS~])~%"
                            thread symbol currentp)))
                 (t
                  (destructuring-bind (thread pc) root
                    (format stream "from ~S PC=~X in ~A~%"
                            thread (sap-int pc) (sb-di::code-header-from-pc pc)))))
           (let ((*print-pretty* nil)
                 (*print-circle* t))
             (dolist (node rest)
               (destructuring-bind (obj . slot) node
                 (format stream " ~D ~16X [~4D] "
                         (or (sb-kernel:generation-of obj) "S")
                         (sb-kernel:get-lisp-obj-address obj)
                         slot)
                 ;; Some objects print fairly concisely, so we'll show them.
                 ;; But do NOT show CONS, VECTOR, INSTANCE. Especially not those.
                 (typecase obj
                   (symbol
                    (format stream "~/sb-ext:print-symbol-with-prefix/~%" obj))
                   ((or package sb-kernel:fdefn sb-kernel:code-component
                        pathname sb-impl::host hash-table)
                    (format stream "~S~%" obj))
                   (t
                    (format stream "a ~(~a~)~%" (type-of obj))))))))
          (t
           (let ((*print-pretty* nil))
             (when (consp root)
               (if (symbolp (second root))
                   (destructuring-bind (thread symbol currentp) root
                     (format stream "~S:~S {~:[binding~;TLS~]}"
                             thread symbol currentp))
                   (destructuring-bind (thread pc) root
                     (format stream "~S:#x~X" thread (sap-int pc)))))
             (dolist (node rest)
               (destructuring-bind (obj . slot) node
                 (format stream " -> (~S) #x~X[~D]"
                         (type-of obj) (sb-kernel:get-lisp-obj-address obj) slot)))
             (format stream " -> #x~x~%" (sb-kernel:get-lisp-obj-address target)))))))

(defun print-traceroot-paths (paths &key (stream *standard-output*) (multiline t))
  (dolist (path paths)
    (print-traceroot-path path :stream stream :multiline multiline)))

(declaim (ftype (function ((or list sb-ext:weak-pointer)
                           &key
                           (:criterion (member :oldest :pseudo-static :static))
                           (:gc t)
                           (:print (or boolean (eql :verbose)))))
                search-roots))
(defun search-roots (weak-pointers &key (criterion :oldest) (gc nil) (print t))
  "Find roots keeping the targets of WEAK-POINTERS alive.

WEAK-POINTERS must be a single SB-EXT:WEAK-POINTER or a list of those,
pointing to objects for which roots should be searched.

GC controls whether the search is performed in the context of a
garbage collection, that is with all Lisp threads stopped. Possible
values are:

  T
    This is the more accurate of the object liveness proof generators,
    as there is no chance for other code to execute in between the
    garbage collection and production of the chain of referencing
    objects.

  NIL
    This works well enough, but might be adversely affected by actions
    of concurrent threads.

CRITERION determines just how rooty (how deep) a root must be in order
to be considered. Possible values are:

  :OLDEST
     This says we can stop upon seeing an object in the oldest gen to
     GC, or older. This is the easiest test to satisfy.

  :PSEUDO-STATIC
     This is usually the same as :OLDEST, unless the oldest gen to GC
     has been decreased.

  :STATIC
     To find a root of an image-backed object, you want to stop only at
     a truly :STATIC object.

PRINT controls whether discovered paths should be returned or
printed. Possible values are

  :VERBOSE
    Return no values. Print discovered paths using a verbose format
    with each node of each path on a separate line.

  true (other than :VERBOSE)
    Return no values. Print discovered paths using a compact format
    with all nodes of each path on a single line.

  NIL
    Do not print any output. Instead return the discovered paths as a
    list of lists. Each list has the form

      (TARGET . (ROOT NODE*))

    where TARGET is one of the target of one of the WEAK-POINTERS.

    ROOT is a description of the root at which the path starts and has
    one of the following forms:

      :STATIC
        If the root of the path is a non-collectible heap object.

      :PINNED
        If an unknown thread stack pins the root of the path.

      ((THREAD-NAME | THREAD-OBJECT) SYMBOL CURRENTP)
        If the path begins at a special binding of SYMBOL in a
        thread. CURRENTP is a BOOLEAN indicating whether the value is
        current or shadowed by another binding.

      ((THREAD-NAME | THREAD-OBJECT) GUESSED-PC)
        If the path begins at a lexical variable in the function whose
        code contains GUESSED-PC.

    Each NODE in the remainder of the path is a cons (OBJECT . SLOT)
    indicating that the slot at index SLOT in OBJECT references the
    next path node.

Experimental: subject to change without prior notice."
  (let* ((input (ensure-list weak-pointers))
         (output (make-array (length input)))
         (param (cons input (cons :result output)))
         (criterion-value (ecase criterion
                            (:oldest 0)
                            (:pseudo-static 1)
                            (:static 2))))
    (cond (gc
           (setf gc-traceroot-criterion criterion-value)
           (sb-sys:with-pinned-objects (param)
             (setf gc-object-watcher (sb-kernel:get-lisp-obj-address param)))
           (gc :full t)
           (setf gc-object-watcher 0))
          (t
           (sb-sys:without-gcing
             (alien-funcall
              (extern-alien "prove_liveness" (function int unsigned int))
              (sb-kernel:get-lisp-obj-address param)
              criterion-value))))
    (case (cadr param)
      (-1 (error "Input is not a proper list of weak pointers.")))
    (let ((paths (preprocess-traceroot-results input output)))
      (cond (print
             (print-traceroot-paths paths :multiline (eq print :verbose))
             (values))
            (t
             paths)))))
