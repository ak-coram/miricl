;;;; Free declaration metadata. Binding-specific CLtL2 augmentation is unsupported.
(defpackage #:miricl-environment
  (:use #:cl)
  (:export #:define-declaration #:declaration-information #:augment-environment))
(in-package #:miricl-environment)

(defvar *global-declarations* nil)

(defun private-key (name)
  (or (miricl-compiler::compiler-primitive 10 1 name)
      (error "MIRICL environment service is unavailable: ~A." name)))

(defun syntactic-environment-p (object)
  (handler-case (progn (miricl-compiler::compiler-primitive 48 object) t)
    (error () nil)))

(defun check-environment (environment)
  (handler-case (miricl-compiler::compiler-primitive 48 environment)
    (error () (error 'type-error :datum environment :expected-type '(satisfies syntactic-environment-p))))
  environment)

(defun check-spine (object context)
  (miricl-compiler::compiler-list-length object context))

(defun reserved-name-p (name)
  (member name '(declaration dynamic-extent ftype ignorable ignore inline notinline optimize special type)))

(defun register-declaration (name handler)
  (unless (symbolp name) (error 'type-error :datum name :expected-type 'symbol))
  (when (reserved-name-p name) (error "Cannot replace standard declaration ~S." name))
  (unless (functionp handler) (error 'type-error :datum handler :expected-type 'function))
  ;; PROCLAIM is ordinary execution; DEFINE-DECLARATION invents no compile-time effects.
  (proclaim (list 'declaration name))
  (setf (get name (private-key "%DECLARATION-HANDLER")) handler)
  name)

(defmacro define-declaration (name lambda-list &body body)
  `(register-declaration ',name (lambda ,lambda-list ,@body)))

(defun snapshot-body-specifier (specifier)
  (check-spine specifier "declaration specifier")
  (let ((copy (copy-list specifier)))
    (case (car copy)
      ((type ftype)
       (setf (second copy) (miricl-compiler::copy-hir-debug-type (second copy)))
       (when (eq (car copy) 'ftype)
         (setf (cddr copy) (mapcar (lambda (name)
           (if (consp name) (copy-list name) name)) (cddr copy)))))
      (optimize
       (setf (cdr copy) (mapcar (lambda (quality)
         (if (consp quality) (copy-list quality) quality)) (cdr copy))))
      ((inline notinline)
       (setf (cdr copy) (mapcar (lambda (name)
         (if (consp name) (copy-list name) name)) (cdr copy))))
      (otherwise
       (unless (symbolp (car copy))
         (setf (car copy) (miricl-compiler::copy-hir-debug-type (car copy))))))
    copy))

(defun body-declarations (body)
  (check-spine body "declaration body")
  (miricl-compiler::compiler-body body)
  (let* ((prepared (copy-list body)) (tail prepared) (specifiers nil))
    (when (and (stringp (car tail)) (cdr tail)) (setq tail (cdr tail)))
    (loop while (and (consp (car tail)) (eq (caar tail) 'declare)) do
      (let ((specs (mapcar #'snapshot-body-specifier (cdar tail))))
        (setf (car tail) (cons 'declare specs))
        (setq specifiers (append specifiers specs) tail (cdr tail))))
    (values specifiers prepared)))

(defun process-declarations (specifiers environment &optional globalp bodyp)
  (check-environment environment)
  (let ((prepared-body nil))
    (when bodyp
      (multiple-value-setq (specifiers prepared-body) (body-declarations specifiers)))
    (check-spine specifiers "declaration specifier list")
    ;; Validate every input shell before the first callback. Operands are data.
    (let ((prepared nil))
      (dolist (specifier specifiers)
        (unless (consp specifier)
          (miricl-compiler::compiler-input-fail "declaration specifier" specifier "expected a nonempty proper list"))
        (check-spine specifier "declaration specifier")
        ;; Own traversal shells before callbacks; preserve every opaque operand.
        (push (copy-list specifier) prepared))
      (setq specifiers (nreverse prepared)))
    (let ((results nil) (handler-key (private-key "%DECLARATION-HANDLER")))
      (dolist (specifier specifiers)
        (let* ((name (car specifier))
               (handler (and (symbolp name) (get name handler-key))))
          (when handler
            (multiple-value-bind (kind data) (funcall handler specifier environment)
              (unless (eq kind :declare)
                (error "Declaration ~S returned an unsupported handler category; only :DECLARE is supported."
                       name))
              ;; The CDR is an arbitrary object, including shared or cyclic data.
              (unless (and (consp data) (symbolp (car data)))
                (error "Declaration ~S must return a (symbol . value) pair." name))
              (when (reserved-name-p (car data))
                (error "Custom declaration key ~S conflicts with a standard declaration." (car data)))
              (push (cons (car data) (cdr data)) results)))))
      ;; Publish only after all callback envelopes have passed validation.
      (values (if globalp
          (progn (setq *global-declarations* (append results *global-declarations*)) environment)
          (progn
            (dolist (data (nreverse results))
              (setq environment (miricl-compiler::compiler-primitive 53 environment (car data) (cdr data))))
            environment)) prepared-body))))

(defun declaration-information (name &optional environment)
  (unless (symbolp name) (error 'type-error :datum name :expected-type 'symbol))
  (check-environment environment)
  (when (reserved-name-p name)
    (error "Standard declaration information for ~S is not supported by this free declaration service." name))
  (let ((entry (miricl-compiler::compiler-primitive 54 environment name)))
    (cdr (if entry entry (assoc name *global-declarations*)))))

(defun augment-environment (environment &rest options)
  (check-environment environment)
  (let ((count (check-spine options "environment augmentation options")))
    (unless (evenp count) (error "Environment augmentation options require keyword/value pairs.")))
  (loop for tail on options by #'cddr do
    (unless (eq (car tail) :declare)
      (error "Unsupported environment augmentation option; only :DECLARE is supported.")))
  (values (process-declarations (getf options :declare) environment)))

;; The core retains only this live dispatcher hook and the name authority.
(setf (get (private-key "%DECLARATION-NAME") (private-key "%DECLARATION-PROCESSOR"))
      #'process-declarations)
