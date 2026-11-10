;;;; Compact compilation resolves macro references and load-time values.
;;;; Calls use native dispatch directly; literal objects retain their identity.
(defvar %compilation-complete t)
(defvar %file-compiling nil)
(defvar %file-load-values nil)
(defvar %file-lexical-names nil)

(defun %file-lexical-name (index)
  (loop while (>= index (length %file-lexical-names)) do
    (vector-push-extend (make-symbol (format nil "L~D" (length %file-lexical-names))) %file-lexical-names))
  (aref %file-lexical-names index))

(defun %file-lexical-specials (body)
  (let ((result nil))
    (dolist (form body)
      (when (and (consp form) (eq (car form) 'declare))
        (dolist (declaration (cdr form))
          (when (eq (car declaration) 'special) (setq result (append (cdr declaration) result))))))
    result))

(defun %file-lexical-bind (name environment specials)
  (acons name (if (or (member name specials) (%%compile 18 name)) name
                 (%file-lexical-name (length environment))) environment))

(defun %file-lexical-reference (name environment)
  (let ((binding (assoc name environment :test #'equal))) (if binding (cdr binding) name)))

(defun %file-code-cons (original operator args)
  (let ((flags (%%compile 8 original)))
    (if (plusp flags) (%%compile flags operator args) (cons operator args))))

(defun %file-lexical-forms (forms variables functions blocks tags)
  (dolist (name (%file-lexical-specials forms)) (push (cons name name) variables))
  (mapcar (lambda (form) (%file-lexical-form form variables functions blocks tags)) forms))

(defun %file-lexical-function (form name variables functions blocks tags)
  (let* ((body (cddr form)) (specials (%file-lexical-specials body)) (parameters nil))
    ;; Local functions acquire an implicit block when the loader creates their
    ;; closures. Keep RETURN-FROM paired with that block after renaming the
    ;; function. Explicit nested blocks establish their own shadowing scope.
    (unless (equal name (car form))
      (push (cons (if (consp (car form)) (second (car form)) (car form)) name) blocks))
    (dolist (parameter (second form))
      (if (eq parameter '&rest) (push parameter parameters)
          (progn
            ;; The file frontend has already normalized optional/key/aux
            ;; arguments. Their initialization code is ordinary lexical code.
            (unless (symbolp parameter) (error "Unnormalized compiled lambda list."))
            (setq variables (%file-lexical-bind parameter variables specials))
            (push (cdar variables) parameters))))
    (%file-code-cons form name
      (cons (nreverse parameters) (%file-lexical-forms body variables functions blocks tags)))))

(defun %file-lexical-call (form variables functions blocks tags)
  ;; Ordinary expression trees can be arbitrarily deep after macro expansion.
  ;; Binding constructs carry their scope on the worklist too; generated code
  ;; can nest LET/LET* and LOCALLY as deeply as calls and conditionals.
  (let* ((result (list nil)) (work (list (list form result variables))))
    (loop while work do
      (let* ((task (pop work)) (source (first task)) (destination (second task))
             (environment (third task)))
        (cond
          ((atom source)
           (rplaca destination (if (symbolp source) (%file-lexical-reference source environment) source)))
          ((member (car source) '(let let*))
           (let* ((outer environment) (specials (%file-lexical-specials (cddr source)))
                  (bindings nil) (initializers nil) (forms (cddr source))
                  (slots (make-list (length forms))))
             (dolist (binding (second source))
               (let* ((name (if (consp binding) (car binding) binding))
                      (init (if (consp binding) (second binding) nil))
                      (initial-environment (if (eq (car source) 'let*) environment outer)))
                 (setq environment (%file-lexical-bind name environment specials))
                 (let ((copy (list (cdar environment) nil)))
                   (push copy bindings)
                   (push (list init (cdr copy) initial-environment) initializers))))
             (dolist (name specials) (push (cons name name) environment))
             (rplaca destination (%file-code-cons source (car source) (cons (nreverse bindings) slots)))
             (loop for form in forms for slot on slots do (push (list form slot environment) work))
             (dolist (task initializers) (push task work))))
          ((eq (car source) 'locally)
           (let* ((forms (cdr source)) (slots (make-list (length forms))))
             (dolist (name (%file-lexical-specials forms)) (push (cons name name) environment))
             (rplaca destination (%file-code-cons source 'locally slots))
             (loop for form in forms for slot on slots do (push (list form slot environment) work))))
          ((member (car source)
             '(quote lambda let let* flet labels locally function block return-from tagbody go the
               defvar defparameter defconstant declare %%case-form %%list-test %%list-accumulate
               %%numeric-collect %%arithmetic-step %%symbol-iterate %%parallel-place-form %%condition-form))
           (rplaca destination (%file-lexical-form source environment functions blocks tags)))
          (t
           (let* ((operator (car source)) (args (cdr source))
                  (slots (make-list (length args)))
                  (copy (%file-code-cons source
                          (and (symbolp operator) (%file-lexical-reference operator functions)) slots))
                  (argument-environment environment))
             (rplaca destination copy)
             (dolist (name (%file-lexical-specials args))
               (push (cons name name) argument-environment))
             (loop for argument in args for slot on slots do
               (push (list argument slot argument-environment) work))
             (unless (symbolp operator) (push (list operator copy environment) work)))))))
    (car result)))

(defun %file-lexical-form (form variables functions blocks tags)
  ;; Alpha-convert only executable binding syntax. Quoted objects keep their
  ;; original identities, including quoted symbols which also name bindings.
  ;; Canonical names are private uninterned symbols shared within this file;
  ;; distinct scopes can consequently share immutable code after hash-consing.
  (unless (consp form)
    (return-from %file-lexical-form
      (if (symbolp form) (%file-lexical-reference form variables) form)))
  (let ((operator (car form)) (args (cdr form)))
    (case operator
      (quote form)
      (lambda (%file-lexical-function form operator variables functions blocks tags))
      ((let let*) (%file-lexical-call form variables functions blocks tags))
      ((flet labels)
       (let ((outer functions) (definitions nil))
         (dolist (definition (car args))
           (push (cons (car definition) (%file-lexical-name (+ (length variables) (length functions)))) functions))
         (dolist (definition (car args))
           (push (%file-lexical-function definition (%file-lexical-reference (car definition) functions)
                   variables (if (eq operator 'labels) functions outer) blocks tags) definitions))
         (%file-code-cons form operator
           (cons (nreverse definitions) (%file-lexical-forms (cdr args) variables functions blocks tags)))))
      (locally (%file-lexical-call form variables functions blocks tags))
      (function
       (%file-code-cons form operator
         (list (if (or (symbolp (car args)) (eq (caar args) 'setf))
                   (%file-lexical-reference (car args) functions)
                   (%file-lexical-form (car args) variables functions blocks tags)))))
      (block
       (push (cons (car args) (car args)) blocks)
       (%file-code-cons form operator
         (cons (car args) (%file-lexical-forms (cdr args) variables functions blocks tags))))
      (return-from
       (%file-code-cons form operator
         (cons (%file-lexical-reference (car args) blocks)
           (%file-lexical-forms (cdr args) variables functions blocks tags))))
      (tagbody
       (%file-code-cons form operator
         (mapcar (lambda (item) (if (atom item) item
                                   (%file-lexical-form item variables functions blocks tags))) args)))
      (go form)
      (the (%file-code-cons form operator
             (cons (car args) (%file-lexical-forms (cdr args) variables functions blocks tags))))
      ((defvar defparameter defconstant)
       ;; The name is a global definition, even inside a lexical binding of
       ;; the same symbol. Only its initializer is executable lexical code.
       (%file-code-cons form operator
         (cons (car args) (%file-lexical-forms (cdr args) variables functions blocks tags))))
      (declare
       (%file-code-cons form operator
         (mapcar (lambda (declaration)
           (case (car declaration)
             ((special optimize declaration) declaration)
             ((inline notinline)
              (cons (car declaration) (mapcar (lambda (name) (%file-lexical-reference name functions)) (cdr declaration))))
             (ftype (list* 'ftype (second declaration)
                      (mapcar (lambda (name) (%file-lexical-reference name functions)) (cddr declaration))))
             (type (list* 'type (second declaration)
                     (mapcar (lambda (name) (%file-lexical-reference name variables)) (cddr declaration))))
             (otherwise (cons (car declaration)
                          (mapcar (lambda (name) (%file-lexical-reference name variables)) (cdr declaration)))))) args)))
      (%%case-form
       (%file-code-cons form operator
         (list* (car args) (%file-lexical-form (second args) variables functions blocks tags)
           (mapcar (lambda (clause) (cons (car clause)
                     (%file-lexical-forms (cdr clause) variables functions blocks tags))) (cddr args)))))
      ((%%list-test %%list-accumulate %%numeric-collect)
       (let* ((local (%file-lexical-bind (second args) variables nil))
              (size (length args)) (index 0) (result nil))
         (dolist (item args)
           (push (cond ((= index 1) (cdar local))
                       ((and (eq operator '%%list-accumulate) (= size 8) (= index (1- size))
                             (symbolp item) (string-equal (symbol-name item) "IT")) item)
                       ((or (= index 3) (= index (1- size)) (and (= size 8) (= index 5)))
                        (%file-lexical-form item
                          (if (or (= index 3) (and (eq operator '%%numeric-collect) (= size 8) (= index 5))) variables local)
                          functions blocks tags))
                       (t item)) result)
           (incf index))
         (%file-code-cons form operator (nreverse result))))
      (%%arithmetic-step
       (%file-code-cons form operator
         (list (%file-lexical-reference (car args) variables)
           (%file-lexical-form (second args) variables functions blocks tags)
           (%file-lexical-form (third args) variables functions blocks tags)
           (fourth args) (fifth args))))
      (%%symbol-iterate
       ;; DO-SYMBOLS already binds this variable in its enclosing LET. This
       ;; instruction assigns that binding; it does not introduce a new one.
       (%file-code-cons form operator
         (list* (%file-lexical-reference (car args) variables)
           (%file-lexical-form (second args) variables functions blocks tags)
           (%file-lexical-forms (cddr args) variables functions blocks tags))))
      (%%parallel-place-form
       (%file-code-cons form operator
         (if (= (car args) 3)
             ;; MULTIPLE-VALUE-SETQ carries a list of variable names, rather
             ;; than an executable call. Rename every target, including CAR.
             (list 3 (mapcar (lambda (name) (%file-lexical-reference name variables)) (second args))
               (%file-lexical-form (third args) variables functions blocks tags))
             (%file-lexical-forms args variables functions blocks tags))))
      (%%condition-form
       (let ((kind (car args)) (forms (cdr args)))
         (%file-code-cons form operator
           (cons kind
             (case kind
               (check-type
                ;; The place and optional description contain executable code;
                ;; the type is literal syntax, even when it names a binding.
                (list* (%file-lexical-form (car forms) variables functions blocks tags)
                  (second forms) (%file-lexical-forms (cddr forms) variables functions blocks tags)))
               (handler-case
                (cons (%file-lexical-form (car forms) variables functions blocks tags)
                  (mapcar (lambda (clause) (%file-lexical-function clause (car clause) variables functions blocks tags))
                    (cdr forms))))
               (restart-case
                (cons (%file-lexical-form (car forms) variables functions blocks tags)
                  (mapcar (lambda (clause)
                    ;; Restart options are evaluated in the enclosing scope,
                    ;; before the restart's parameters are bound. Bare symbols
                    ;; here are function designators, not variable references.
                    (let ((body (cddr clause)) (options nil))
                      (loop while (member (car body) '(:report :interactive :test)) do
                        (push (pop body) options)
                        (let ((value (pop body)))
                          (push (if (symbolp value) (%file-lexical-reference value functions)
                                    (%file-lexical-form value variables functions blocks tags)) options)))
                      (let ((normalized (%file-lexical-function
                              (%file-code-cons clause (car clause) (cons (second clause) body))
                              (car clause) variables functions blocks tags)))
                        (%file-code-cons clause (car clause)
                          (cons (second normalized) (append (nreverse options) (cddr normalized)))))))
                    (cdr forms))))
               ((handler-bind restart-bind)
                (cons (mapcar (lambda (binding) (cons (car binding)
                                 (%file-lexical-forms (cdr binding) variables functions blocks tags))) (car forms))
                  (%file-lexical-forms (cdr forms) variables functions blocks tags)))
               (with-simple-restart
                (cons (cons (caar forms) (%file-lexical-forms (cdar forms) variables functions blocks tags))
                  (%file-lexical-forms (cdr forms) variables functions blocks tags)))
               (otherwise (%file-lexical-forms forms variables functions blocks tags)))))))
      (otherwise (%file-lexical-call form variables functions blocks tags)))))
(defun %compile-place-form (place environment)
  (let ((expanded (macroexpand place environment)))
    (if (consp expanded) (cons (car expanded) (%compile-forms (cdr expanded) environment)) expanded)))
(defun %compile-parallel-places (operator args environment)
  (let ((mode (car args)) (forms (cdr args)) (compiled nil))
    (case mode
      (3 (list operator mode (car forms) (%compile-form (cadr forms) environment)))
      (4 (list operator mode (%compile-place-form (car forms) environment) (%compile-form (cadr forms) environment)))
      (5 (list* operator mode (%compile-form (car forms) environment)
                (%compile-place-form (cadr forms) environment) (%compile-forms (cddr forms) environment)))
      (otherwise
        (loop while forms do
          (if (and (= mode 2) (not (cdr forms)))
              (push (%compile-form (pop forms) environment) compiled)
              (push (%compile-place-form (pop forms) environment) compiled))
          (when (= mode 0) (push (%compile-form (pop forms) environment) compiled)))
        (list* operator mode (nreverse compiled))))))
(defun %compile-loop-collector (operator args environment)
  (let ((compiled nil) (local (%%compile 3 environment (cadr args))) (size (length args)) (index 0))
    (dolist (form args)
      (push (if (or (= index 3) (= index (1- size))
                    (and (= size 8) (= index 5)))
                (%compile-form form (if (or (= index 3) (and (eq operator '%%numeric-collect) (= size 8) (= index 5))) environment local)) form) compiled)
      (incf index))
    (cons operator (nreverse compiled))))
(defun %compile-forms (forms environment)
  (let ((result nil))
    (dolist (form forms) (push (%compile-form form environment) result))
    (nreverse result)))
(defun %compile-body (body environment)
  (let ((declarations nil))
    (when (and (stringp (car body)) (cdr body)) (push (pop body) declarations))
    (loop while (and (consp (car body)) (eq (caar body) 'declare)) do
      (let ((form (pop body)))
        (dolist (spec (cdr form))
          (when (eq (car spec) 'special)
            (dolist (name (cdr spec)) (setq environment (%%compile 3 environment name)))))
        (push form declarations)))
    (append (nreverse declarations) (%compile-forms body environment))))
(defun %compile-lambda-parts (params body environment)
  (when (intersection params '(&optional &key &aux))
    (let ((normalized (%normalize-lambda params body)))
      (setq params (car normalized) body (cadr normalized))))
  (dolist (name params)
    (unless (eq name '&rest) (setq environment (%%compile 3 environment name))))
  ;; A proclamation may exist only in the compilation environment. Preserve
  ;; its binding semantics in the code rather than depending on the loader's
  ;; symbol flags (EVAL-WHEN need not repeat a proclamation at load time).
  (let ((specials (remove-if-not (lambda (name) (and (not (eq name '&rest)) (%%compile 18 name))) params)))
    (when specials
      (let ((declaration (list 'declare (cons 'special specials))))
        (setq body (if (and (stringp (car body)) (cdr body))
                       (list* (car body) declaration (cdr body))
                       (cons declaration body)))))
    (cons params (%compile-body body environment))))
(defun %compile-lambda-form (params body environment)
  (let ((parts (%compile-lambda-parts params body environment)))
    (if %compilation-complete (%%compile 2 'lambda parts)
        (list 'function (cons 'lambda parts)))))
(defun %compile-closure (params body environment)
  (let ((%compilation-complete t))
    (let ((parts (%compile-lambda-parts params body environment)))
      ;; Existing closures have already normalized their parameter lists.
      (cons %compilation-complete (cdr parts)))))
(defun %compile-bindings (operator bindings body environment)
  (let ((new-environment environment) (compiled nil) (specials nil))
    (dolist (binding bindings)
      (let ((name (if (consp binding) (car binding) binding))
            (init (if (consp binding) (cadr binding) nil)))
        (push (list name (%compile-form init (if (eq operator 'let*) new-environment environment))) compiled)
        (when (%%compile 18 name) (push name specials))
        (setq new-environment (%%compile 3 new-environment name))))
    (when specials (push (list 'declare (cons 'special (nreverse specials))) body))
    (list* operator (nreverse compiled) (%compile-body body new-environment))))
(defun %compile-local-functions (operator definitions body environment)
  (let ((new-environment environment) (compiled nil))
    (dolist (definition definitions)
      (setq new-environment (%%compile 4 new-environment (car definition))))
    (dolist (definition definitions)
      (let ((parts (%compile-lambda-parts (cadr definition) (cddr definition)
                     (if (eq operator 'labels) new-environment environment))))
        (push (if %compilation-complete (%%compile 2 (car definition) parts)
                  (cons (car definition) parts)) compiled)))
    (list* operator (nreverse compiled) (%compile-body body new-environment))))
(defun %expand-backquote (form)
  ;; Expand an inner backquote before processing its enclosing template.
  ;; In particular, ,,@ can splice several unquotes into the outer template.
  (when (typep form 'simple-vector)
    (return-from %expand-backquote
      (list 'apply '(function vector) (%expand-backquote (coerce form 'list)))))
  (unless (consp form) (return-from %expand-backquote (list 'quote form)))
  (when (member (car form) '(quasiquote unquote unquote-splicing unquote-nsplicing))
    (unless (= (length form) 2) (error 'program-error))
    (return-from %expand-backquote
      (case (car form)
        (quasiquote (%expand-backquote (%expand-backquote (cadr form))))
        (unquote (cadr form))
        (otherwise (error 'program-error)))))
  (let ((parts nil))
    (loop while (and (consp form)
                    (not (member (car form) '(quasiquote unquote unquote-splicing unquote-nsplicing))))
      do (let ((part (car form)))
           (if (and (consp part) (member (car part) '(unquote-splicing unquote-nsplicing)))
               (progn
                 (unless (= (length part) 2) (error 'program-error))
                 (push (cadr part) parts))
               ;; LIST must remain variadic: an enclosing ,,@ can splice
               ;; several arguments here, which a binary CONS cannot accept.
               (push (list 'list (%expand-backquote part)) parts)))
         (setq form (cdr form)))
    (cons 'append (nreverse (cons (%expand-backquote form) parts)))))
(defun %compile-backquote (form environment depth)
  (when (typep form 'simple-vector)
    (return-from %compile-backquote
      (%%compile 1 'apply (list '(function vector)
        (%compile-backquote (coerce form 'list) environment depth)))))
  (unless (consp form) (return-from %compile-backquote (list 'quote form)))
  (when (member (car form) '(quasiquote unquote unquote-splicing unquote-nsplicing))
    (unless (= (length form) 2) (error 'program-error))
    (cond ((eq (car form) 'quasiquote)
           (return-from %compile-backquote
             (%compile-backquote (%expand-backquote (cadr form)) environment depth)))
          ((= depth 1)
           (unless (eq (car form) 'unquote) (error 'program-error))
           (return-from %compile-backquote (%compile-form (cadr form) environment)))
          (t (decf depth)))
    (return-from %compile-backquote
      (%%compile 1 'list (list (list 'quote (car form))
        (%compile-backquote (cadr form) environment depth)))))
  (if (and (= depth 1) (consp (car form))
           (member (caar form) '(unquote-splicing unquote-nsplicing)))
      (progn
        (unless (= (length (car form)) 2) (error 'program-error))
        (%%compile 1 (if (eq (caar form) 'unquote-splicing) 'append 'nconc)
          (list (%compile-form (cadar form) environment)
                (%compile-backquote (cdr form) environment depth))))
      (%%compile 1 'cons (list (%compile-backquote (car form) environment depth)
                              (%compile-backquote (cdr form) environment depth)))))
(defun %compile-option-function (form environment)
  (cond ((symbolp form) form)
        ((and (consp form) (eq (car form) 'lambda))
         (%compile-lambda-form (cadr form) (cddr form) environment))
        (t (%compile-form form environment))))
(defun %compile-condition (operator args environment)
  (let ((compiled nil))
    (case operator
      (define-condition
       (let ((slots nil) (options nil))
         (dolist (slot (third args))
           (if (symbolp slot) (push slot slots)
               (let ((tail (cdr slot)) (parts nil))
                 (loop while tail do
                   (let ((key (pop tail)))
                     (unless tail (error 'program-error))
                     (push key parts)
                     (push (if (eq key :initform) (%compile-form (pop tail) environment) (pop tail)) parts)))
                 (push (cons (car slot) (nreverse parts)) slots))))
         (dolist (option (cdddr args))
           (push (case (car option)
                   (:report (list :report (if (stringp (second option)) (second option)
                                              (%compile-option-function (second option) environment))))
                   (:default-initargs
                    (let ((tail (cdr option)) (parts nil))
                      (loop while tail do
                        (push (pop tail) parts)
                        (unless tail (error 'program-error))
                        (push (%compile-form (pop tail) environment) parts))
                      (cons :default-initargs (nreverse parts))))
                   (otherwise option)) options))
         (setq compiled (list* (first args) (second args) (nreverse slots) (nreverse options)))))
      ((handler-case restart-case)
       (let ((primary (%compile-form (car args) environment)))
         (dolist (clause (cdr args))
           (let ((body (cddr clause)) (options nil))
             (when (eq operator 'restart-case)
               (loop while (member (car body) '(:report :interactive :test)) do
                 (let ((key (pop body)) (value (pop body)))
                   (push key options)
                   (push (if (and (eq key :report) (stringp value)) value
                             (%compile-option-function value environment)) options))))
             (let* ((parts (%compile-lambda-parts (cadr clause) body environment))
                    (tail (cons (car parts) (append (nreverse options) (cdr parts)))))
               (push (if %compilation-complete (%%compile 2 (car clause) tail)
                         (cons (car clause) tail)) compiled))))
         (setq compiled (cons primary (nreverse compiled)))))
      ((handler-bind restart-bind)
       (dolist (binding (car args))
         (let ((options (cddr binding)) (compiled-options nil))
           (loop while options do (push (pop options) compiled-options)
                                  (push (%compile-form (pop options) environment) compiled-options))
           (push (list* (car binding) (%compile-form (cadr binding) environment)
                        (nreverse compiled-options)) compiled)))
       (setq compiled (cons (nreverse compiled) (%compile-body (cdr args) environment))))
      (with-simple-restart
       (setq compiled (cons (cons (caar args) (%compile-forms (cdar args) environment))
                            (%compile-body (cdr args) environment))))
      (ignore-errors (setq compiled (%compile-body args environment)))
      (assert
       (setq compiled (cons (%compile-form (car args) environment)
         (cons (mapcar (lambda (place) (%compile-place-form place environment)) (cadr args))
               (%compile-forms (cddr args) environment)))))
      (check-type
       (setq compiled (list* (%compile-place-form (car args) environment) (cadr args)
                        (%compile-forms (cddr args) environment))))
      (with-condition-restarts (setq compiled (%compile-forms args environment)))
      (return (return-from %compile-condition (%compile-form (list* 'return-from nil args) environment)))
      ((block return-from catch throw unwind-protect tagbody go)
       (return-from %compile-condition (%compile-form (cons operator args) environment)))
      (otherwise (setq %compilation-complete nil compiled args)))
    (list* '%%condition-form operator compiled)))
(defun %compile-form (form environment)
  ;; Macro expansions routinely contain much deeper trees than their source.
  ;; Keep pending conversions on the heap, preserving expansion order and the
  ;; lexical environment of each initializer and body.
  (let ((result (list nil)) (work nil))
    (labels
        ((forms (items environment)
           (let ((slots (make-list (length items))) (tasks nil))
             (loop for item in items for slot on slots do
               (let ((source item) (place slot))
                 (push (lambda () (walk source environment place)) tasks)))
             (setq work (nconc (nreverse tasks) work))
             slots))
         (body (items environment)
           (let ((declarations nil))
             (when (and (stringp (car items)) (cdr items)) (push (pop items) declarations))
             (loop while (and (consp (car items)) (eq (caar items) 'declare)) do
               (let ((declaration (pop items)))
                 (dolist (spec (cdr declaration))
                   (when (eq (car spec) 'special)
                     (dolist (name (cdr spec)) (setq environment (%%compile 3 environment name)))))
                 (push declaration declarations)))
             (append (nreverse declarations) (forms items environment))))
         (bindings (remaining operator outer local compiled specials items destination)
           (if (null remaining)
               (progn
                 (when specials (push (list 'declare (cons 'special (nreverse specials))) items))
                 (rplaca destination (list* operator (nreverse compiled) (body items local))))
               (let* ((binding (car remaining))
                      (name (if (consp binding) (car binding) binding))
                      (slot (list nil)))
                 (push (lambda ()
                         (bindings (cdr remaining) operator outer (%%compile 3 local name)
                           (cons (list name (car slot)) compiled)
                           (if (%%compile 18 name) (cons name specials) specials) items destination)) work)
                 (push (lambda ()
                         (walk (if (consp binding) (cadr binding) nil)
                           (if (eq operator 'let*) local outer) slot)) work))))
         (walk (form environment destination)
           (setq form (macroexpand form environment))
           (cond
             ((atom form) (rplaca destination form))
             ((member (car form) '(quote go declare)) (rplaca destination form))
             ((member (car form) '(let let*))
              (bindings (second form) (car form) environment environment nil nil (cddr form) destination))
             ((member (car form) '(block return-from the))
              (rplaca destination (list* (car form) (second form) (forms (cddr form) environment))))
             ((eq (car form) 'locally)
              (rplaca destination (cons 'locally (body (cdr form) environment))))
             ((member (car form) '(if progn multiple-value-call multiple-value-prog1 catch throw unwind-protect progv %%while))
              (rplaca destination (cons (car form) (forms (cdr form) environment))))
             ((or (not (symbolp (car form))) (special-operator-p (car form))
                  (member (car form) '(quasiquote %%case-form %%list-test %%list-accumulate %%numeric-collect
                    %%parallel-place-form %%symbol-iterate %%arithmetic-step %%condition-form))
                  (and (not (symbol-package (car form))) (not (fboundp (car form)))))
              (rplaca destination (%compile-recursive-form form environment)))
             (t (rplaca destination (%%compile 1 (car form) (forms (cdr form) environment)))))))
      (push (lambda () (walk form environment result)) work)
      (loop while work do (funcall (pop work)))
      (car result))))

(defun %compile-recursive-form (form environment)
  (multiple-value-bind (expansion expanded) (macroexpand form environment)
    (when expanded (return-from %compile-recursive-form (%compile-form expansion environment))))
  (unless (consp form) (return-from %compile-recursive-form form))
  (let ((operator (car form)) (args (cdr form)))
    (case operator
      ((go declare) form)
      (quote form)
      (quasiquote (%compile-backquote (car args) environment 1))
      (function
        (if (and (consp (car args)) (eq (caar args) 'lambda))
            (%compile-lambda-form (cadar args) (cddar args) environment) form))
      (load-time-value
        (list 'quote
          (if %file-compiling
              (let ((marker (gensym)))
                (setf (gethash marker %file-load-values) (list (%compile-form (car args) nil))) marker)
              (eval (car args)))))
      ((let let*) (%compile-bindings operator (car args) (cdr args) environment))
      (progv
       (list* operator (%compile-form (car args) environment) (%compile-form (cadr args) environment)
         (%compile-body (cddr args) environment)))
      ((flet labels) (%compile-local-functions operator (car args) (cdr args) environment))
      (macrolet
        (let ((new-environment environment))
          (dolist (definition (car args))
            (let* ((name (car definition))
                   (lambda-form (apply '%macro-lambda name (cadr definition) (cddr definition)))
                   (function (%%compile 7 environment lambda-form)))
              (setq new-environment (%%compile 5 new-environment name function))))
          (cons 'locally (%compile-body (cdr args) new-environment))))
      (symbol-macrolet
        (let ((new-environment environment))
          (dolist (binding (car args))
            (unless (and (consp binding) (= (length binding) 2) (symbolp (car binding))
                         (not (constantp (car binding) environment)) (not (%%compile 18 (car binding))))
              (error 'program-error))
            (let ((body (cdr args)))
              (loop while (and (consp (car body)) (eq (caar body) 'declare)) do
                (dolist (declaration (cdr (pop body)))
                  (when (and (eq (car declaration) 'special) (member (car binding) (cdr declaration)))
                    (error 'program-error)))))
            (setq new-environment (%%compile 6 new-environment (car binding) (cadr binding))))
          (cons 'locally (%compile-body (cdr args) new-environment))))
      (setq
        (let ((compiled nil))
          (loop while args do
            (let ((name (pop args)) (value (pop args)))
              (if (nth-value 1 (macroexpand name environment))
                  (push (%compile-form (list 'setf name value) environment) compiled)
                  (push (list 'setq name (%compile-form value environment)) compiled))))
          (cons 'progn (nreverse compiled))))
      ((block return-from the)
        (list* operator (car args) (%compile-forms (cdr args) environment)))
      (tagbody
        (cons operator (mapcar (lambda (item)
          (if (consp item)
              (let ((statement (%compile-form item environment)))
                ;; Macro expansion must not turn a statement into a GO tag.
                (if (consp statement) statement (list 'progn statement)))
              item)) args)))
      (eval-when
        (if (intersection (car args) '(:execute eval))
            (cons 'progn (%compile-body (cdr args) environment)) nil))
      (locally (cons operator (%compile-body args environment)))
      ((if progn multiple-value-call multiple-value-prog1 catch throw unwind-protect progv)
        (cons operator (%compile-forms args environment)))
      (%%case-form
        (list* operator (car args)
          (if (>= (car args) 4) (%compile-place-form (cadr args) environment)
              (%compile-form (cadr args) environment))
          (mapcar (lambda (clause) (cons (car clause) (%compile-body (cdr clause) environment))) (cddr args))))
      ((%%list-test %%list-accumulate %%numeric-collect)
        (%compile-loop-collector operator args environment))
      (%%parallel-place-form (%compile-parallel-places operator args environment))
      (%%symbol-iterate
        (list* operator (car args) (%compile-form (cadr args) environment)
          (%compile-body (cddr args) (%%compile 3 environment (car args)))))
      (%%while (cons operator (%compile-forms args environment)))
      (%%arithmetic-step
        (list operator (car args) (%compile-form (cadr args) environment)
          (%compile-form (caddr args) environment) (cadddr args) (fifth args)))
      (otherwise
        (cond
          ((eq operator '%%condition-form)
           (%compile-condition (car args) (cdr args) environment))
          ((and (symbolp operator) (not (symbol-package operator)) (not (fboundp operator)))
           (setq %compilation-complete nil) form)
          ((consp operator)
           (unless (eq (car operator) 'lambda) (error 'program-error))
           (%%compile 1 'funcall
             (cons (%compile-lambda-form (cadr operator) (cddr operator) environment)
                   (%compile-forms args environment))))
          (t (%%compile 1 operator (%compile-forms args environment))))))))
