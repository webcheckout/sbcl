(in-package "SB!EVAL")

;;(declaim (optimize (speed 3) (debug 1) (safety 1)))

(defvar *special* (gensym "SPECIAL"))
(defvar *macro* (gensym "MACRO"))
(defvar *symbol-macro* (gensym "SYMBOL-MACRO"))
(defvar *not-present* (gensym "NOT-PRESENT"))

(define-condition interpreted-program-error (program-error simple-condition)
  ()
  (:report (lambda (condition stream)
	     (format stream "Error evaluating a form:~%  ")
             (apply #'format stream
                    (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition)))))

;;; ANSI defines that program syntax errors should be of type
;;; PROGRAM-ERROR.  Therefore...
(define-condition arg-count-program-error (sb!kernel::arg-count-error
                                           program-error)
  ())

(defun arg-count-program-error (datum &rest arguments)
  (declare (ignore datum))
  (apply #'error 'arg-count-program-error arguments))

;; OAOOM? (see destructuring-bind.lisp)
(defmacro program-destructuring-bind (lambda-list arg-list &rest body)
  (let ((arg-list-name (gensym "ARG-LIST-")))
    (multiple-value-bind (body local-decls)
        (sb!kernel:parse-defmacro lambda-list arg-list-name body nil
                                  'program-destructuring-bind
                                  :anonymousp t
                                  :doc-string-allowed nil
                                  :wrap-block nil
                                  :error-fun 'arg-count-program-error)
      `(let ((,arg-list-name ,arg-list))
         ,@local-decls
         ,body))))

(defun ip-error (format-control &rest format-arguments)
  (error 'interpreted-program-error
         :format-control format-control
         :format-arguments format-arguments))

(defmacro nconc-2 (a b)
  (let ((tmp (gensym))
        (tmp2 (gensym)))
    `(let ((,tmp ,a)
           (,tmp2 ,b))
       (if ,tmp
           (progn (setf (cdr (last ,tmp)) ,tmp2) ,tmp)
           ,tmp2))))

(declaim (inline fabricate-new-native-environment))
(defun fabricate-new-native-environment (old-lexenv new-funs new-expanders
                                         new-vars new-symbol-expansions)
  (labels ((to-native-funs (binding)
             (if (eq (cdr binding) *macro*)
                 (cons (car binding)
                       (cons 'sb!sys:macro
                             (cdr (assoc (car binding) new-expanders))))
                 (cons (car binding)
                       :bogus)))
           (to-native-vars (binding)
             (if (eq (cdr binding) *symbol-macro*)
                 (cons (car binding)
                       (cons 'sb!sys:macro
                             (cdr (assoc (car binding) new-symbol-expansions))))
                 (cons (car binding)
                       :bogus))))
    (sb!c::internal-make-lexenv
     (nconc-2 (mapcar #'to-native-funs new-funs)
              (sb!c::lexenv-funs old-lexenv))
     (nconc-2 (mapcar #'to-native-vars new-vars)
              (sb!c::lexenv-vars old-lexenv))
     nil nil nil nil nil nil nil
     (sb!c::lexenv-policy old-lexenv))))

(defstruct (env
             (:constructor %make-env 
                           (parent vars funs expanders symbol-expansions 
                            tags blocks declarations native-lexenv)))
  parent
  vars
  funs
  expanders
  symbol-expansions
  tags
  blocks
  declarations
  native-lexenv)

(declaim (inline make-env))
(defun make-env (&key parent vars funs expanders 
                 symbol-expansions tags blocks declarations)
  (%make-env parent
             (nconc-2 vars (env-vars parent))
             (nconc-2 funs (env-funs parent))
             (nconc-2 expanders (env-expanders parent))
             (nconc-2 symbol-expansions (env-symbol-expansions parent))
             (nconc-2 tags (env-tags parent))
             (nconc-2 blocks (env-blocks parent))
             declarations
             (fabricate-new-native-environment
              (env-native-lexenv parent)
              funs expanders
              vars symbol-expansions)))

(defun make-null-environment ()
  (%make-env nil nil nil nil nil nil nil nil
             (sb!c::internal-make-lexenv
              nil nil 
              nil nil nil nil nil nil nil 
              sb!c::*policy*)))

(declaim (inline push-var))
(defun push-var (name value env)
  (push (cons name value) (env-vars env))
  (push (cons name :bogus) (sb!c::lexenv-vars (env-native-lexenv env))))

(declaim (inline push-fun))
(defun push-fun (name value env)
  (push (cons name value) (env-funs env))
  (push (cons name :bogus) (sb!c::lexenv-funs (env-native-lexenv env))))

(sb!int:def!method print-object ((env env) stream)
  (print-unreadable-object (env stream :type t :identity t)))

(macrolet ((define-get-binding (name accessor &key (test '#'eq))
             ;; A macro, sadly, because an inline function here is
             ;; "too hairy"
             `(defmacro ,name (symbol env)
                `(assoc ,symbol (,',accessor ,env) :test ,',test))))
  (define-get-binding get-binding env-vars)
  (define-get-binding get-fbinding env-funs :test #'equal)
  (define-get-binding get-expander-binding env-expanders)
  (define-get-binding get-symbol-expansion-binding env-symbol-expansions)
  (define-get-binding get-tag-binding env-tags :test #'eql)
  (define-get-binding get-block-binding env-blocks))

(defun declared-specials (decls)
  (let ((specials nil))
    (dolist (decl decls)
      (when (eql (car decl) 'special)
        (dolist (var (cdr decl))
          (push var specials))))
    specials))

(defun specialp (symbol declared-specials env)
  (declare (ignorable env))
  (let ((type (sb!int:info :variable :kind symbol)))
    (cond
      ((eq type :constant)
       ;; Horrible place for this, but it works.
       (ip-error "Can't bind constant symbol ~S" symbol))
      ((eq type :special) t)
      ((member symbol declared-specials :test #'eq) t)
      (t nil))))

(defun binding-name (binding)
  (if (consp binding) (first binding) binding))
(defun binding-value (binding)
  (if (consp binding) (second binding) nil))
(defun supplied-p-parameter (spec)
  (if (consp spec) (third spec) nil))
(defun keyword-name (spec)
  (if (consp spec)
      (if (consp (first spec))
          (second (first spec))
          (first spec))
      spec))
(defun keyword-key (spec)
  (if (consp spec)
      (if (consp (first spec))
          (first (first spec))
          (intern (symbol-name (first spec)) "KEYWORD"))
      (intern (symbol-name spec) "KEYWORD")))
(defun keyword-default-value (spec)
  (if (consp spec) (second spec) nil))

(defun parse-arguments (arguments lambda-list)
  (multiple-value-bind (required optional rest-p rest keyword-p 
                        keyword allow-other-keys-p aux-p aux)
      (sb!int:parse-lambda-list lambda-list)
    (let* ((original-arguments arguments)
           (arguments-present (length arguments))
           (required-length (length required))
           (optional-length (length optional))
           (non-keyword-arguments (+ required-length optional-length))
           (optionals-present (- (min non-keyword-arguments arguments-present)
                                 required-length))
           (keywords-present-p (> arguments-present non-keyword-arguments))
           (let-like-bindings nil)
           (let*-like-bindings nil))
      (cond
        ((< arguments-present required-length)
         (ip-error "~@<Too few arguments in ~S to satisfy lambda list ~S.~:@>"
                   arguments lambda-list))
        ((and (not (or rest-p keyword-p)) keywords-present-p)
         (ip-error "~@<Too many arguments in ~S to satisfy lambda list ~S.~:@>"
                   arguments lambda-list))
        ((and keyword-p keywords-present-p
              (oddp (- arguments-present non-keyword-arguments)))
         (ip-error "~@<Odd number of &KEY arguments in ~S for ~S.~:@>"
                   arguments lambda-list)))
      (dotimes (i required-length) 
        (push (cons (pop required) (pop arguments)) let-like-bindings))
      (do ((optionals-parsed 0 (1+ optionals-parsed)))
          ((null optional))
        (let ((this-optional (pop optional))
              (supplied-p (< optionals-parsed optionals-present)))
          (push (cons (binding-name this-optional)
                      (if supplied-p
                          (list 'quote (pop arguments))
                          (binding-value this-optional)))
                let*-like-bindings)
          (when (supplied-p-parameter this-optional)
            (push (cons (supplied-p-parameter this-optional)
                        (list 'quote supplied-p))
                  let*-like-bindings))))
      (let ((keyword-plist arguments))
        (when rest-p
          (push (cons rest (list 'quote keyword-plist)) let*-like-bindings))
        (when keyword-p
          (unless (or allow-other-keys-p 
                      (getf keyword-plist :allow-other-keys))
            (loop for (key value) on keyword-plist by #'cddr doing
                  (when (and (not (eq key :allow-other-keys))
                             (not (member key keyword :key #'keyword-key)))
                    (ip-error "~@<Unknown &KEY argument ~S in ~S for ~S.~:@>"
                              key original-arguments lambda-list))))
          (dolist (keyword-spec keyword)
            (let ((supplied (getf keyword-plist (keyword-key keyword-spec)
                                  *not-present*)))
              (push (cons (keyword-name keyword-spec)
                          (if (eq supplied *not-present*)
                              (keyword-default-value keyword-spec)
                              (list 'quote supplied)))
                    let*-like-bindings)
              (when (supplied-p-parameter keyword-spec)
                (push (cons (supplied-p-parameter keyword-spec)
                            (list 'quote (not (eq supplied *not-present*))))
                      let*-like-bindings))))))
      (when aux-p
        (do ()
            ((null aux))
          (let ((this-aux (pop aux)))
            (push (cons (binding-name this-aux)
                        (binding-value this-aux))
                  let*-like-bindings))))
      (values (nreverse let-like-bindings) (nreverse let*-like-bindings)))))

(defun eval-next-let*-binding (bindings specials env end-action)
  (flet ((maybe-eval (exp)
           ;; Pick off the easy (QUOTE x) case which is very common
           ;; due to function calls.  (see PARSE-ARGUMENTS)
           (if (and (consp exp) (eq (car exp) 'quote))
               (second exp)
               (%eval exp env))))
    (if bindings
        (let* ((binding-name (car (car bindings)))
               (binding-value (cdr (car bindings))))
          (if (specialp binding-name specials env)
              (progv
                  (list binding-name)
                  (list (maybe-eval binding-value))
                (push-var binding-name *special* env)
                (eval-next-let*-binding (cdr bindings) 
                                        specials env end-action))
              (progn
                (push-var binding-name (maybe-eval binding-value) env)
                (eval-next-let*-binding (cdr bindings) 
                                        specials env end-action))))
        (funcall end-action))))

(defun call-with-new-env (old-env bindings declarations
                          free-specials-p function)
  (let* ((specials (declared-specials declarations))
         (dynamic-vars nil)
         (dynamic-values nil))
    (flet ((generate-binding (binding)
             (if (specialp (car binding) specials old-env)
                 (progn
                   (push (car binding) dynamic-vars)
                   (push (cdr binding) dynamic-values)
                   nil)
                 (list binding))))
      (let ((new-env (make-env
                      :parent old-env
                      :vars (mapcan #'generate-binding bindings)
                      :declarations declarations)))
        (dolist (special (if free-specials-p specials dynamic-vars))
          (push (cons special *special*) (env-vars new-env)))
        (if dynamic-vars
            (progv dynamic-vars dynamic-values
              (funcall function new-env))
            (funcall function new-env))))))

(defun call-with-new-env-full-parsing
    (old-env lambda-list arguments declarations function)
  (multiple-value-bind (let-like-bindings let*-like-binding)
      (parse-arguments arguments lambda-list)
    (let ((specials (declared-specials declarations))
          var-specials free-specials)
      (dolist (special specials)
        (if (or (member special let-like-bindings :key #'car)
                (member special let*-like-binding :key #'car))
            (push special var-specials)
            (push special free-specials)))
      (call-with-new-env
       old-env let-like-bindings declarations nil
       #'(lambda (env)
           (eval-next-let*-binding
            let*-like-binding var-specials env
            #'(lambda ()
                (dolist (special free-specials)
                  (push-var special *special* env))
                (funcall function env))))))))

(defun set-variable (symbol value env)
  (let ((binding (get-binding symbol env)))
    (if binding
        (cond
          ((eq (cdr binding) *special*)
           (setf (symbol-value symbol) value))
          ((eq (cdr binding) *symbol-macro*)
           (error "Tried to set a symbol-macrolet!"))
          (t (setf (cdr binding) value)))
        (setf (symbol-value symbol) value))))

(defun get-variable (symbol env)
  (let ((binding (get-binding symbol env)))
    (if binding
        (cond
          ((eq (cdr binding) *special*) 
           (values (symbol-value symbol) :variable))
          ((eq (cdr binding) *symbol-macro*)
           (values (cdr (get-symbol-expansion-binding symbol env)) 
                   :expansion))
          (t (values (cdr binding) :variable)))
        (case (sb!int:info :variable :kind symbol)
          (:macro (values (macroexpand-1 symbol) :expansion))
          (t (values (symbol-value symbol) :variable))))))

(defun get-function (name env)
  (let ((binding (get-fbinding name env)))
    (if binding
        (cond
          ((eq (cdr binding) *macro*)
           (values (cdr (get-expander-binding name env)) :macro))
          (t (values (cdr binding) :function)))
        (cond
          ((and (symbolp name) (macro-function name))
           (values (macro-function name) :macro))
          (t (values (fdefinition name) :function))))))

(defun lambdap (exp)
  (case (car exp) ((lambda
                    sb!int:named-lambda
                    sb!kernel:instance-lambda)
                   t)))

(defun parse-lambda-headers (body &key doc-string-allowed)
  (loop with documentation = nil
        with declarations = nil
        for form on body do
        (cond
          ((and doc-string-allowed (stringp (car form)))
           (if (cdr form)               ; CLHS 3.4.11
               (if documentation
                   (ip-error "~@<Duplicate doc string ~S.~:@>" (car form))
                   (setf documentation (car form)))
               (return (values form documentation declarations))))
          ((and (consp (car form)) (eql (caar form) 'declare))
           (setf declarations (append declarations (cdar form))))
          (t (return (values form documentation declarations))))
        finally (return (values nil documentation declarations))))

(defun eval-lambda (exp env)
  (case (car exp)
    ((lambda sb!kernel:instance-lambda) 
     (multiple-value-bind (body documentation declarations)
         (parse-lambda-headers (cddr exp) :doc-string-allowed t) 
       (make-interpreted-function :lambda-list (second exp)
                                  :env env :body body
                                  :documentation documentation
                                  :declarations declarations)))
    ((sb!int:named-lambda)
     (multiple-value-bind (body documentation declarations)
         (parse-lambda-headers (cdddr exp) :doc-string-allowed t) 
       (make-interpreted-function :name (second exp)
                                  :lambda-list (third exp)
                                  :env env :body body
                                  :documentation documentation
                                  :declarations declarations)))))

(defun eval-progn (body env)
  (let ((previous-exp nil))
    (dolist (exp body)
      (if previous-exp
          (%eval previous-exp env))
      (setf previous-exp exp))
    ;; preserve tail call
    (%eval previous-exp env)))

(defun eval-if (body env)
  (program-destructuring-bind (test if-true &optional if-false) body
    (if (%eval test env)
        (%eval if-true env)
        (%eval if-false env))))

(defun eval-let (body env)
  (program-destructuring-bind (bindings &body body) body
    (let ((bindings (mapcar 
                     #'(lambda (binding)
                         (cons (binding-name binding)
                               (%eval (binding-value binding) env)))
                     bindings)))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil) 
        (declare (ignore documentation))
        (call-with-new-env env bindings declarations t
                           #'(lambda (env)
                               (eval-progn body env)))))))

(defun eval-let* (body old-env)
  (program-destructuring-bind (bindings &body body) body
    (multiple-value-bind (body documentation declarations)
        (parse-lambda-headers body :doc-string-allowed nil) 
      (declare (ignore documentation))
      (let ((specials (declared-specials declarations))
            var-specials free-specials)
        (dolist (special specials)
          (if (member special bindings :key #'binding-name)
              (push special var-specials)
              (push special free-specials)))
        (let ((env (make-env :parent old-env
                             :declarations declarations)))
          (eval-next-let*-binding 
           (mapcar #'(lambda (binding)
                       (cons (binding-name binding)
                             (binding-value binding)))
                   bindings) 
           var-specials env
           #'(lambda ()
               (dolist (special free-specials)
                 (push-var special *special* env))
               (eval-progn body env))))))))

(defun eval-local-function-def (function-def env)
  (program-destructuring-bind (name lambda-list &body local-body) function-def
    (multiple-value-bind (local-body documentation declarations)
        (parse-lambda-headers local-body :doc-string-allowed t)
      (%eval `#'(sb!int:named-lambda ,name ,lambda-list
                  ,@(if documentation 
                        (list documentation) 
                        nil)
                  (declare ,@declarations)
                  (block ,(cond ((consp name) (second name))
                                (t name))
                    ,@local-body))
             env))))

(defun eval-flet (body env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (flet ((generate-fbinding (function-def)
             (cons (car function-def)
                   (eval-local-function-def function-def env))))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        (let ((new-env (make-env :parent env
                                 :funs (mapcar #'generate-fbinding 
                                               local-functions)
                                 :declarations declarations)))
          (eval-progn body new-env))))))

(defun eval-labels (body old-env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (multiple-value-bind (body documentation declarations)
        (parse-lambda-headers body :doc-string-allowed nil)
      (declare (ignore documentation))
      (let ((env (make-env :parent old-env
                           :declarations declarations)))
        (dolist (function-def local-functions)
          (push-fun (car function-def)
                    (eval-local-function-def function-def env)
                    env))
        (eval-progn body env)))))

(defun eval-local-macro-def (function-def env)
  (program-destructuring-bind (name lambda-list &body local-body) function-def
    (multiple-value-bind (local-body documentation declarations)
        (parse-lambda-headers local-body :doc-string-allowed t)
      (let (has-environment has-whole)
        (when (eq (car lambda-list) '&whole)
          (setf has-whole (second lambda-list))
          (setf lambda-list (cddr lambda-list)))
        (setf lambda-list
              (loop with skip = 0
                    for element in lambda-list
                    if (cond
                         ((/= skip 0)
                          (decf skip)
                          (setf has-environment element)
                          nil)
                         ((eq element '&environment)
                          (if has-environment
                              (ip-error "Repeated &ENVIRONMENT.")
                              (setf skip 1))
                          nil)
                         ((eq element '&whole)
                          (ip-error "&WHOLE may only appear first ~
                                     in MACROLET lambda-list."))
                         (t t))
                    collect element))
        (let ((outer-whole (gensym "WHOLE"))
              (environment (or has-environment (gensym "ENVIRONMENT")))
              (macro-name (gensym "NAME")))
          (%eval `#'(lambda (,outer-whole ,environment)
                      ,@(if documentation
                            (list documentation) 
                            nil)
                      (declare ,@(unless has-environment
                                   `((ignore ,environment)))
                               ,@declarations)
                      (program-destructuring-bind
                       (,@(if has-whole
                              (list '&whole has-whole)
                              nil)
                          ,macro-name ,@lambda-list)
                       ,outer-whole
                       (declare (ignore ,macro-name))
                       (block ,name ,@local-body)))
                 env))))))

(defun eval-macrolet (body env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (flet ((generate-fbinding (macro-def)
             (cons (car macro-def) *macro*))
           (generate-mbinding (macro-def)
             (cons (car macro-def) (eval-local-macro-def macro-def env))))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        (let ((new-env (make-env :parent env
                                 :funs (mapcar #'generate-fbinding
                                               local-functions)
                                 :expanders (mapcar #'generate-mbinding 
                                                    local-functions)
                                 :declarations declarations)))
          (eval-progn body new-env))))))

(defun eval-symbol-macrolet (body env)
  (program-destructuring-bind ((&rest bindings) &body body) body
    (flet ((generate-binding (binding)
             (cons (car binding) *symbol-macro*))
           (generate-sm-binding (binding)
             (cons (car binding) (second binding))))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        (let ((specials (declared-specials declarations)))
          (dolist (binding bindings)
            (when (specialp (binding-name binding) specials env)
              (ip-error "~@<Can't bind SYMBOL-MACROLET of special ~
                         variable ~S.~:@>"
                        (binding-name binding)))))
        (let ((new-env (make-env :parent env
                                 :vars (mapcar #'generate-binding bindings)
                                 :symbol-expansions (mapcar
                                                     #'generate-sm-binding 
                                                     bindings)
                                 :declarations declarations)))
          (eval-progn body new-env))))))

(defun eval-progv (body env)
  (program-destructuring-bind (vars vals &body body) body
    (progv (%eval vars env) (%eval vals env)
      (eval-progn body env))))

(defun eval-function (body env)
  (program-destructuring-bind (name) body
    (cond
      ((symbolp name) (nth-value 0 (get-function name env)))
      ((lambdap name) (eval-lambda name env))
      (t (nth-value 0 (get-function name env))))))

(defun eval-eval-when (body env)
  (program-destructuring-bind ((&rest situation) &body body) body
    (if (or (member :execute situation)
            (member 'eval situation))
        (eval-progn body env))))

(defun eval-quote (body env)
  (declare (ignore env))
  (program-destructuring-bind (object) body
    object))

(defun eval-setq (pairs env)
  (when (oddp (length pairs))
    (ip-error "~@<Odd number of args to SETQ: ~S~:@>" (cons 'setq pairs)))
  (let ((last nil))
    (loop for (var new-val) on pairs by #'cddr do
          (handler-case
              (multiple-value-bind (expansion type) (get-variable var env)
                (ecase type
                  (:expansion
                   (setf last 
                         (%eval (list 'setf expansion new-val) env)))
                  (:variable 
                   (setf last (set-variable var (%eval new-val env)
                                            env)))))
            (unbound-variable (c)
              (declare (ignore c))
              (setf last (setf (symbol-value var)
                               (%eval new-val env))))))
    last))

(defun eval-multiple-value-call (body env)
  (program-destructuring-bind (function-form &body forms) body
    (%apply (%eval function-form env)
            (loop for form in forms
                  nconc (multiple-value-list (%eval form env))))))

(defun eval-multiple-value-prog1 (body env)
  (program-destructuring-bind (first-form &body forms) body
    (multiple-value-prog1 (%eval first-form env)
      (eval-progn forms env))))

(defun eval-catch (body env)
  (program-destructuring-bind (tag &body forms) body
    (catch (%eval tag env)
      (eval-progn forms env))))

(defun eval-tagbody (body old-env)
  (let ((env (make-env :parent old-env))
        (tags nil)
        (start body)
        (target-tag nil))
    (tagbody
       (flet ((go-to-tag (tag)
                (setf target-tag tag)
                (go go-to-tag)))
         (do ((form body (cdr form)))
             ((null form) nil)
           (when (atom (car form))
             (push (cons (car form) (cdr form)) tags)
             (push (cons (car form) #'go-to-tag) (env-tags env)))))
       (go execute)
     go-to-tag
       (setf start (cdr (assoc target-tag tags)))
     execute
       (dolist (form start)
         (when (not (atom form))
           (%eval form env))))))

(defun eval-go (body env)
  (program-destructuring-bind (tag) body
    (let ((target (get-tag-binding tag env)))
      (if target
          (funcall (cdr target) tag)
          (ip-error "~@<Attempt to GO to nonexistent tag: ~S~:@>" tag)))))

(defun eval-block (body old-env)
  (flet ((return-from-eval-block (&rest values)
           (return-from eval-block (values-list values))))
    (program-destructuring-bind (name &body body) body
      (unless (symbolp name)
        (ip-error "~@<The block name ~S is not a symbol.~:@>" name))
      (let ((env (make-env 
                  :blocks (list (cons name #'return-from-eval-block))
                  :parent old-env)))
        (eval-progn body env)))))

(defun eval-return-from (body env)
  (program-destructuring-bind (name &optional result) body
    (let ((target (get-block-binding name env)))
      (if target
          (multiple-value-call (cdr target) (%eval result env))
          (ip-error "~@<Return for unknown block: ~S~:@>" name)))))

(defun eval-the (body env)
  (program-destructuring-bind (value-type form) body
    (declare (ignore value-type))
    (%eval form env)))

(defun eval-unwind-protect (body env)
  (program-destructuring-bind (protected-form &body cleanup-forms) body
    (unwind-protect (%eval protected-form env)
      (eval-progn cleanup-forms env))))

(defun eval-throw (body env)
  (program-destructuring-bind (tag result-form) body
    (throw (%eval tag env)
      (%eval result-form env))))

(defun eval-load-time-value (body env)
  (program-destructuring-bind (form &optional read-only-p) body
    (declare (ignore read-only-p))
    (%eval form env)))

(defun eval-locally (body env)
  (multiple-value-bind (body documentation declarations)
      (parse-lambda-headers body :doc-string-allowed nil)
    (declare (ignore documentation))
    (let* ((specials (declared-specials declarations))
           (new-env (if (or specials declarations)
                        (make-env :parent env
                                  :vars (mapcar #'(lambda (var)
                                                    (cons var *special*))
                                                specials)
                                  :declarations declarations)
                        env)))
      (eval-progn body new-env))))

(defun eval-args (args env)
  (mapcar #'(lambda (arg) (%eval arg env)) args))

(defun %eval (exp env)
  (incf *eval-calls*)
  (let ((*eval-level* (1+ *eval-level*))) ; breaks tail calls, but
                                          ; useful for debugging
    (when *eval-verbose*
      (let ((*print-circle* t))
        (format t "~&~vA~S~%" *eval-level* "" `(%eval ,exp))))
    (cond
      ((symbolp exp)
       ;; CLHS 3.1.2.1.1 Symbols as Forms
       (multiple-value-bind (value kind) (get-variable exp env)
         (ecase kind
           (:variable value)
           (:expansion (%eval value env)))))
      ;; CLHS 3.1.2.1.3 Self-Evaluating Objects
      ((atom exp) exp)
      ;; CLHS 3.1.2.1.2 Conses as Forms
      ((consp exp)
       (case (car exp)
         ;; CLHS 3.1.2.1.2.1 Special Forms
         ((block)                (eval-block (cdr exp) env))
         ((catch)                (eval-catch (cdr exp) env))
         ((eval-when)            (eval-eval-when (cdr exp) env))
         ((flet)                 (eval-flet (cdr exp) env))
         ((function)             (eval-function (cdr exp) env))
         ((go)                   (eval-go (cdr exp) env))
         ((if)                   (eval-if (cdr exp) env))
         ((labels)               (eval-labels (cdr exp) env))
         ((let)                  (eval-let (cdr exp) env))
         ((let*)                 (eval-let* (cdr exp) env))
         ((load-time-value)      (eval-load-time-value (cdr exp) env))
         ((locally)              (eval-locally (cdr exp) env))
         ((macrolet)             (eval-macrolet (cdr exp) env))
         ((multiple-value-call)  (eval-multiple-value-call (cdr exp) env))
         ((multiple-value-prog1) (eval-multiple-value-prog1 (cdr exp) env))
         ((progn)                (eval-progn (cdr exp) env))
         ((progv)                (eval-progv (cdr exp) env))
         ((quote)                (eval-quote (cdr exp) env))
         ((return-from)          (eval-return-from (cdr exp) env))
         ((setq)                 (eval-setq (cdr exp) env))
         ((symbol-macrolet)      (eval-symbol-macrolet (cdr exp) env))
         ((tagbody)              (eval-tagbody (cdr exp) env))
         ((the)                  (eval-the (cdr exp) env))
         ((throw)                (eval-throw (cdr exp) env))
         ((unwind-protect)       (eval-unwind-protect (cdr exp) env))
         ;; SBCL-specific:
         ((sb!ext:truly-the)     (eval-the (cdr exp) env))
         (t  
          (cond
            ;; CLHS 3.1.2.1.2.4 Lambda Forms
            ((and (consp (car exp)) (eq (caar exp) 'lambda))
             (interpreted-apply (eval-function (list (car exp)) env)
                                (eval-args (cdr exp) env)))
            (t
             (multiple-value-bind (function kind) (get-function (car exp) env)
               (ecase kind
                 ;; CLHS 3.1.2.1.2.3 Function Forms
                 (:function (%apply function (eval-args (cdr exp) env)))
                 ;; CLHS 3.1.2.1.2.2 Macro Forms
                 (:macro
                  (%eval (%apply function (list exp (env-native-lexenv env)))
                         env))))))))))))

(defun %apply (fun args)
  (etypecase fun
    (interpreted-function (interpreted-apply fun args))
    (function (apply fun args))
    (symbol (apply fun args))))

(defun interpreted-apply (fun args)
  (let ((lambda-list (interpreted-function-lambda-list fun))
        (env (interpreted-function-env fun))
        (body (interpreted-function-body fun))
        (declarations (interpreted-function-declarations fun)))
    (call-with-new-env-full-parsing
     env lambda-list args declarations
     #'(lambda (env)
         (eval-progn body env)))))

(define-condition environment-too-complex-error (simple-error)
  ())

(defun prepare-for-compile (function)
  (let ((env (interpreted-function-env function)))
    (when (or (env-tags env)
              (env-blocks env)
              (find-if-not #'(lambda (x) (eq x *macro*))
                           (env-funs env) :key #'cdr)
              (find-if-not #'(lambda (x) (eq x *symbol-macro*))
                           (env-vars env)
                           :key #'cdr))
      (error 'environment-too-complex-error
             :format-control
             "~@<Lexical environment of ~S is too complex to compile.~:@>"
             :format-arguments
             (list function)))
    ;; KLUDGE need to track decls here
    (values
     `(sb!int:named-lambda ,(interpreted-function-name function)
          ,(interpreted-function-lambda-list function)
        ,@(interpreted-function-body function))
     (env-native-lexenv env))))

(defun make-env-from-native-environment (lexenv)
  (let ((native-funs (sb!c::lexenv-funs lexenv))
        (native-vars (sb!c::lexenv-vars lexenv)))
    (flet ((is-macro (thing)
             (and (consp thing) (eq (car thing) 'sb!sys:macro))))
      (when (or (sb!c::lexenv-blocks lexenv)
                (sb!c::lexenv-cleanup lexenv)
                (sb!c::lexenv-disabled-package-locks lexenv)
                (sb!c::lexenv-handled-conditions lexenv)
                (sb!c::lexenv-lambda lexenv)
                (sb!c::lexenv-tags lexenv)
                (sb!c::lexenv-type-restrictions lexenv)
                (find-if-not #'is-macro native-funs :key #'cdr)
                (find-if-not #'is-macro native-vars :key #'cdr))
        (error 'environment-too-complex-error
               :format-control
               "~@<Lexical environment is too complex to evaluate in: ~S~:@>"
               :format-arguments
               (list lexenv))))
    (flet ((make-binding (native)
             (cons (car native) *symbol-macro*))
           (make-sm-binding (native)
             (cons (car native) (cddr native)))
           (make-fbinding (native)
             (cons (car native) *macro*))
           (make-mbinding (native)
             (cons (car native) (cddr native))))
      (%make-env nil
                 (mapcar #'make-binding native-vars)
                 (mapcar #'make-fbinding native-funs)
                 (mapcar #'make-mbinding native-funs)
                 (mapcar #'make-sm-binding native-vars)
                 nil
                 nil
                 nil
                 lexenv))))

(defun eval-in-environment (form env)
  (%eval form env))

(defun eval-in-native-environment (form lexenv)
  (handler-case
      (let ((env (make-env-from-native-environment lexenv)))
        (%eval form env))
    (environment-too-complex-error (condition)
      (declare (ignore condition))
      (sb!int:style-warn
       "~@<Native lexical environment too complex for SB-EVAL ~
       to evaluate ~S, falling back to OLD-EVAL-IN-LEXENV.  ~
       Lexenv: ~S~:@>"
       form lexenv)
      (sb!int:old-eval-in-lexenv form lexenv))))

(defun count-eval-calls (form)
  (let ((*eval-calls* 0))
    (values (eval form)
            *eval-calls*)))
