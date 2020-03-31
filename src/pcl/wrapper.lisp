;;;; Bits and pieces of the wrapper machninery. This used to live in cache.lisp,
;;;; but doesn't really logically belong there.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defmacro wrapper-class (wrapper)
  `(classoid-pcl-class (layout-classoid ,wrapper)))

(declaim (inline make-wrapper-internal))
(defun make-wrapper-internal (hash &key (bitmap -1) length classoid)
  (make-layout hash classoid
               :length length :invalid nil
               :flags +pcl-object-layout-flag+ :bitmap bitmap))

;;; This is called in BRAID when we are making wrappers for classes
;;; whose slots are not initialized yet, and which may be built-in
;;; classes. We pass in the class name in addition to the class.
(defun !boot-make-wrapper (length name &optional class)
  (let ((found (find-classoid name nil)))
    (cond
     (found
      (unless (classoid-pcl-class found)
        (setf (classoid-pcl-class found) class))
      (aver (eq (classoid-pcl-class found) class))
      (let ((layout (classoid-layout found)))
        (aver layout)
        layout))
     (t
      (make-wrapper-internal (randomish-layout-clos-hash name)
                             :length length
                             :classoid (make-standard-classoid
                                        :name name :pcl-class class))))))

;;; In SBCL, as in CMU CL, the layouts (a.k.a wrappers) for built-in
;;; and structure classes already exist when PCL is initialized, so we
;;; don't necessarily always make a wrapper. Also, we help maintain
;;; the mapping between CL:CLASS and SB-KERNEL:CLASSOID objects.
(defun make-wrapper (length class)
  ;; SLOT-VALUE can't be inlined because we don't have the machinery
  ;; to perform ENSURE-ACCESSOR as yet.
  (declare (notinline slot-value))
  (cond
    ((or (typep class 'std-class)
         (typep class 'forward-referenced-class))
     (let* ((name)
            (classoid
             (let ((owrap (class-wrapper class)))
               (cond (owrap
                      (layout-classoid owrap))
                     ((or (*subtypep (class-of class) *the-class-standard-class*)
                          (*subtypep (class-of class) *the-class-funcallable-standard-class*)
                          (typep class 'forward-referenced-class))
                      (setq name (slot-value class 'name))
                      (make-standard-classoid :pcl-class class
                                              :name (and (symbolp name) name)))
                     (t
                      (bug "Got to T branch in ~S" 'make-wrapper))))))
       (make-wrapper-internal (randomish-layout-clos-hash name)
                              :length length
                              :classoid classoid)))
    (t
     (let* ((found (find-classoid (slot-value class 'name)))
            (layout (classoid-layout found)))
       (unless (classoid-pcl-class found)
         (setf (classoid-pcl-class found) class))
       (aver (eq (classoid-pcl-class found) class))
       (aver layout)
       layout))))

(declaim (inline wrapper-class*))
(defun wrapper-class* (wrapper)
  (or (wrapper-class wrapper)
      ;; FIXME: this branch seems unreachable.
      ;; It would be nice to eliminate WRAPPER-CLASS* if we can show that it
      ;; is only a holdover from an earlier way of bootstrapping that resulted
      ;; in the temporary absence of a PCL-CLASS for some non-standard-class.
      ;; Certainly no test gets here [changing it to (BUG "got here") worked].
      ;; Note however that
      ;;  (CLASSOID-PCL-CLASS (FIND-CLASSOID 'STANDARD-INSTANCE)) => NIL
      ;; which can be resolved by just ensuring one time that it has a CLASS.
      ;; And nothing else seems to be problematic.
      (let ((classoid (layout-classoid wrapper)))
        (ensure-non-standard-class
         (classoid-name classoid)
         classoid))))

;;; The wrapper cache machinery provides general mechanism for
;;; trapping on the next access to any instance of a given class. This
;;; mechanism is used to implement the updating of instances when the
;;; class is redefined (MAKE-INSTANCES-OBSOLETE). The same mechanism
;;; is also used to update generic function caches when there is a
;;; change to the superclasses of a class.
;;;
;;; Basically, a given wrapper can be valid or invalid. If it is
;;; invalid, it means that any attempt to do a wrapper cache lookup
;;; using the wrapper should trap. Also, methods on
;;; SLOT-VALUE-USING-CLASS check the wrapper validity as well. This is
;;; done by calling CHECK-WRAPPER-VALIDITY.

(declaim (inline invalid-wrapper-p))
(defun invalid-wrapper-p (wrapper)
  (not (null (layout-invalid wrapper))))

;;; The portable code inherited from the ancient PCL sources used an extremely
;;; clever mechanism to store a mapping from new layouts ("wrappers") to old.
;;; Old layouts point to new in a straighforward way - via the INVALID slot -
;;; but the invalidation logic wants backpointers from new to old as well.
;;; It arranges so that if the following state exists:
;;;   layout_A -> (:flush layout_B)
;;; and then we want to change layout_B from valid to invalid:
;;;   layout_B -> (:flush layout_C)
;;; the state of layout_A should be changed to (:flush layout_C)
;;; at the same time that the state of layout_B is changed.
;;;
;;; In general, alterating layout_B might might have to perform more than one
;;; update to older versions of the layout, so it's a one-to-many relation.
;;; The same "transitivity" could have been implemented at the time of flushing
;;; layout_A by chasing an extra pointer (the B -> C pointer) and recording that
;;; new state back into A. So I don't see how this "clever" code actually
;;; improves anything, as it merely performs work eagerly that could have
;;; been performed lazily. (PCL generally prefers lazy updates.)
;;;
;;; Anyway the extreme cleverness (and premature optimization, imho), was almost,
;;; but not quite, clever enough - it leaked three conses per redefinition,
;;; which it pretty much had to in order to remain portable.
;;; We can at least replicate it in a way that doesn't leak conses,
;;; or at least theoretically doesn't. A likely scenario is that old caches may
;;; never get adquately flushed to drop all references to obsolete layouts.
;;; If you call a GF once and not again, it won't flush; thus causing a retained
;;; old layout no matter what else is done.
;;;
;;; So the non-leaky thing to do is use weak pointers, which makes the object
;;; representation more transparent, because you can examine a classoid to get
;;; the mapping rather than having to guess at what layouts are pointed to
;;; by the variable formerly known as *previous-nwrappers*.
;;;
;;; And the code no longer relies on such spooky action at a distance - smashing
;;; the CAR and CDR of the cons cells that are shared across layouts - but instead
;;; performs a slot update on old layouts which, while still being a mutating
;;; operation, is far more obvious as to intent.
;;; This might even fix a data race, though I can't be certain.
;;; The bug is that it was possible for an observer to see (:FLUSH new-thing)
;;; in an older layout when it should actually have seen (:OBSOLETE newer-thing).
;;; On the other hand, it's still possible for an observer to see the
;;; wrong value in the INVALID slot itself. i.e. if it reads the slot
;;; before %invalidate-wrapper performs the transitivity operation,
;;; hence the possibility that this optimization is not only premature,
;;; but also pretty much not correct.
(defun %invalidate-wrapper (owrapper state nwrapper)
  (aver (member state '(:flush :obsolete)))
  #+sb-thread (aver (eq (sb-thread:mutex-owner sb-c::**world-lock**)
                        sb-thread:*current-thread*))
  (let ((classoid (layout-classoid nwrapper))
        (new-previous ())
        (new-state (cons state nwrapper)))
    (aver (eq (layout-classoid owrapper) classoid))
    ;; First off, a previous call to INVALIDATE-WRAPPER may have
    ;; recorded OWRAPPER as an NWRAPPER to update to. Since OWRAPPER
    ;; is about to be invalid, it no longer makes sense to update to
    ;; it.
    ;;
    ;; We go back and change the previously invalidated wrappers so
    ;; that they will now update directly to NWRAPPER. This
    ;; corresponds to a kind of transitivity of wrapper updates.
    (dolist (weak-pointer (sb-kernel::standard-classoid-old-layouts classoid))
      (let ((previous (weak-pointer-value weak-pointer)))
        (when previous
          (setf (layout-invalid previous)
                (cond ((and (eq (car (layout-invalid previous)) :obsolete)
                            (eq state :flush))
                       ;; :obsolete must stay :obsolete,
                       ;; requiring the protocol for obsolete instances.
                       (cons :obsolete nwrapper))
                      (t
                       ;; flush->{obsolete|flush} or {flush|obsolete}->obsolete
                       new-state)))
          (push weak-pointer new-previous))))

    ;; FIXME: We are here inside PCL lock, but might someone be
    ;; accessing the wrapper at the same time from outside the lock?
    (setf (layout-clos-hash owrapper) 0)
    ;; And shouldn't these assignments be flipped, so that if an observer
    ;; sees a 0 hash in owrapper, it can find the new layout?
    (setf (layout-invalid owrapper) new-state)
    (push (make-weak-pointer owrapper) new-previous)
    ;; This function is called for effect; return value is arbitrary.
    (setf  (sb-kernel::standard-classoid-old-layouts classoid)
           new-previous)))

;;; FIXME: This is not a good name: part of the contract here is that
;;; we return the valid wrapper, which is not obvious from the name
;;; (or the names of our callees.)
(defun check-wrapper-validity (instance)
  (with-world-lock ()
    (let* ((owrapper (layout-of instance))
           (state (layout-invalid owrapper)))
      (aver (not (eq state :uninitialized)))
      (cond ((not state)
             owrapper)
            ((not (layout-for-std-class-p owrapper))
             ;; Obsolete structure trap.
             (%obsolete-instance-trap owrapper nil instance))
            ((eq t state)
             ;; FIXME: I can't help thinking that, while this does cure
             ;; the symptoms observed from some class redefinitions,
             ;; this isn't the place to be doing this flushing.
             ;; Nevertheless... -- CSR, 2003-05-31
             ;;
             ;; CMUCL comment:
             ;;    We assume in this case, that the :INVALID is from a
             ;;    previous call to REGISTER-LAYOUT for a superclass of
             ;;    INSTANCE's class.  See also the comment above
             ;;    FORCE-CACHE-FLUSHES.  Paul Dietz has test cases for this.
             (let ((class (wrapper-class* owrapper)))
               (%force-cache-flushes class)
               ;; KLUDGE: avoid an infinite recursion, it's still better to
               ;; bail out with an error for server softwares. see FIXME above.
               ;; details: http://thread.gmane.org/gmane.lisp.steel-bank.devel/10175
               ;;
               ;; Error message here is trying to figure out a bit more about the
               ;; situation, since we don't have anything approaching a test-case
               ;; for the bug.
               (let ((new-state (layout-invalid (layout-of instance))))
                 (when (eq new-state t)
                   (cerror "Nevermind and recurse." 'bug
                           :format-control "~@<~4IProblem forcing cache flushes. Please report ~
                                               to sbcl-devel.~
                                            ~% Owrapper: ~S~
                                            ~% Wrapper-of: ~S~
                                            ~% Class-wrapper: ~S~%~:@>"
                           :format-arguments (mapcar (lambda (x)
                                                       (cons x (layout-invalid x)))
                                                     (list owrapper
                                                           (layout-of instance)
                                                           (class-wrapper class)))))))
             (check-wrapper-validity instance))
            ((consp state)
             (ecase (car state)
               (:flush
                (let ((new (cdr state)))
                  (cond ((std-instance-p instance)
                         (setf (%instance-layout instance) new))
                        ((fsc-instance-p instance)
                         (setf (%funcallable-instance-layout instance) new))
                        (t
                         (bug "unrecognized instance type")))))
               (:obsolete
                (%obsolete-instance-trap owrapper (cdr state) instance))))))))

(declaim (inline check-obsolete-instance))
(defun check-obsolete-instance (instance)
  (when (invalid-wrapper-p (layout-of instance))
    (check-wrapper-validity instance)))

(defun valid-wrapper-of (instance)
  (let ((wrapper (layout-of instance)))
    (if (invalid-wrapper-p wrapper)
        (check-wrapper-validity instance)
        wrapper)))

;;;  NIL: means nothing so far, no actual arg info has NILs in the
;;;  metatype.
;;;
;;;  CLASS: seen all sorts of metaclasses (specifically, more than one
;;;  of the next 5 values) or else have seen something which doesn't
;;;  fall into a single category (SLOT-INSTANCE, FORWARD).  Also used
;;;  when seen a non-standard specializer.
;;;
;;;  T: means everything so far is the class T.
;;;
;;;  The above three are the really important ones, as they affect how
;;;  discriminating functions are computed.  There are some other
;;;  possible metatypes:
;;;
;;;  * STANDARD-INSTANCE: seen only standard classes
;;;  * BUILT-IN-INSTANCE: seen only built in classes
;;;  * STRUCTURE-INSTANCE: seen only structure classes
;;;  * CONDITION-INSTANCE: seen only condition classes
;;;
;;;  but these are largely unexploited as of 2007-05-10.  The
;;;  distinction between STANDARD-INSTANCE and the others is used in
;;;  emitting wrapper/slot-getting code in accessor discriminating
;;;  functions (see EMIT-FETCH-WRAPPER and EMIT-READER/WRITER); it is
;;;  possible that there was an intention to use these metatypes to
;;;  specialize cache implementation or discrimination nets, but this
;;;  has not occurred as yet.
(defun raise-metatype (metatype new-specializer)
  (let ((slot      *the-class-slot-class*)
        (standard  *the-class-standard-class*)
        (fsc       *the-class-funcallable-standard-class*)
        (condition *the-class-condition-class*)
        (structure *the-class-structure-class*)
        (system    *the-class-system-class*)
        (frc       *the-class-forward-referenced-class*))
    (flet ((specializer->metatype (x)
             (let* ((specializer-class (if (eq **boot-state** 'complete)
                                           (specializer-class-or-nil x)
                                           x))
                    (meta-specializer (class-of specializer-class)))
               (cond
                 ((eq x *the-class-t*) t)
                 ((not specializer-class) 'non-standard)
                 ((*subtypep meta-specializer standard) 'standard-instance)
                 ((*subtypep meta-specializer fsc) 'standard-instance)
                 ((*subtypep meta-specializer condition) 'condition-instance)
                 ((*subtypep meta-specializer structure) 'structure-instance)
                 ((*subtypep meta-specializer system) 'system-instance)
                 ((*subtypep meta-specializer slot) 'slot-instance)
                 ((*subtypep meta-specializer frc) 'forward)
                 (t (error "~@<PCL cannot handle the specializer ~S ~
                            (meta-specializer ~S).~@:>"
                           new-specializer meta-specializer))))))
      ;; We implement the following table. The notation is
      ;; that X and Y are distinct meta specializer names.
      ;;
      ;;    NIL    <anything>    ===>  <anything>
      ;;    X      X             ===>  X
      ;;    X      Y             ===>  CLASS
      (let ((new-metatype (specializer->metatype new-specializer)))
        (cond ((eq new-metatype 'slot-instance) 'class)
              ((eq new-metatype 'forward) 'class)
              ((eq new-metatype 'non-standard) 'class)
              ((null metatype) new-metatype)
              ((eq metatype new-metatype) new-metatype)
              (t 'class))))))

(defmacro with-dfun-wrappers ((args metatypes)
                              (dfun-wrappers invalid-wrapper-p
                                             &optional wrappers classes types)
                              invalid-arguments-form
                              &body body)
  `(let* ((args-tail ,args) (,invalid-wrapper-p nil) (invalid-arguments-p nil)
          (,dfun-wrappers nil) (dfun-wrappers-tail nil)
          ,@(when wrappers
              `((wrappers-rev nil) (types-rev nil) (classes-rev nil))))
     (dolist (mt ,metatypes)
       (unless args-tail
         (setq invalid-arguments-p t)
         (return nil))
       (let* ((arg (pop args-tail))
              (wrapper nil)
              ,@(when wrappers
                  `((class *the-class-t*)
                    (type t))))
         (unless (eq mt t)
           (setq wrapper (layout-of arg))
           (when (invalid-wrapper-p wrapper)
             (setq ,invalid-wrapper-p t)
             (setq wrapper (check-wrapper-validity arg)))
           (cond ((null ,dfun-wrappers)
                  (setq ,dfun-wrappers wrapper))
                 ((not (consp ,dfun-wrappers))
                  (setq dfun-wrappers-tail (list wrapper))
                  (setq ,dfun-wrappers (cons ,dfun-wrappers dfun-wrappers-tail)))
                 (t
                  (let ((new-dfun-wrappers-tail (list wrapper)))
                    (setf (cdr dfun-wrappers-tail) new-dfun-wrappers-tail)
                    (setf dfun-wrappers-tail new-dfun-wrappers-tail))))
           ,@(when wrappers
               `((setq class (wrapper-class* wrapper))
                 (setq type `(class-eq ,class)))))
         ,@(when wrappers
             `((push wrapper wrappers-rev)
               (push class classes-rev)
               (push type types-rev)))))
     (if invalid-arguments-p
         ,invalid-arguments-form
         (let* (,@(when wrappers
                    `((,wrappers (nreverse wrappers-rev))
                      (,classes (nreverse classes-rev))
                      (,types (mapcar (lambda (class)
                                        `(class-eq ,class))
                                      ,classes)))))
           ,@body))))
