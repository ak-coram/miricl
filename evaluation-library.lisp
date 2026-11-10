;;;; Browser/Node evaluation policy; forms and literals stay in the local heap.
(defun %compile-evaluation (form name)
  ;; Compiler helpers can themselves use EVAL. Prevent recursive automatic
  ;; compilation while preparing code; execute the resulting application
  ;; function in the caller's dynamic environment.
  (let* ((miricl-compiler::*compiling-evaluation* t)
         (%compilation-complete nil)
         (expanded (%compile-form (list 'function form) nil)))
    ;; Resolve macros and LOAD-TIME-VALUE in the application's environment,
    ;; retaining the actual literal objects before native HIR lowering.
    (miricl-compiler:compile-native
      (if (eq (car expanded) 'function) (second expanded) expanded) :name name)))

(defun %compiler-evaluate (form)
  (when (or (not miricl-compiler:*compile-evaluations*)
            miricl-compiler::*compiling-evaluation*
            (atom form) (eq (car form) 'quote))
    (return-from %compiler-evaluate (%%eval form)))
  ;; Debug definitions execute their existing forms in the interpreter. The
  ;; native compiler retains instructions at DEBUG 2, without stepping support.
  (when (or (%debug-form-p form) (= (or (case (car form)
                 (defun (%body-debug-level (cdddr form) %debug-default-level))
                 (locally (%body-debug-level (cdr form) %debug-default-level))
                 (otherwise %debug-default-level)) 1) 3))
    (return-from %compiler-evaluate (%%eval form)))
  (let ((operator (and (consp form) (car form))))
    (case operator
      (progn
       (if (cdr form)
           (progn (dolist (child (butlast (cdr form))) (%compiler-evaluate child))
                  (%compiler-evaluate (car (last form)))) nil))
      (eval-when
       (unless (cdr form) (error 'program-error))
       (if (intersection (second form) '(:execute eval))
           (%compiler-evaluate (cons 'progn (cddr form))) nil))
      (defun
       (let* ((name (second form)) (body (cdddr form)) (prefix nil) (documentation nil))
         (loop while (or (and (stringp (car body)) (cdr body) (not documentation))
                         (and (consp (car body)) (eq (caar body) 'declare))) do
           (let ((item (pop body)))
             (if (stringp item) (setq documentation item) (push item prefix))))
         (let ((function (%compile-evaluation
                           (list* 'lambda (third form)
                             (append (nreverse prefix)
                               (list (list* 'block (if (consp name) (second name) name) body)))) name)))
           (setf (fdefinition name) function)
           (when (and (symbolp name) (macro-function name)) (setf (macro-function name) nil))
           (setf (documentation name 'function) documentation
                 (documentation function 'function) documentation) name)))
      ;; These establish the target's compilation environment and retain the
      ;; evaluator's existing semantics. Their generated methods/macros can be
      ;; compiled explicitly; ordinary expressions and DEFUN bodies use the
      ;; selected compiler.
      ((defvar defparameter)
       (if (and (<= (if (eq operator 'defvar) 2 3) (length form) 4) (symbolp (second form)))
           (progn
             (proclaim (list 'special (second form)))
             (if (and (cddr form) (or (eq operator 'defparameter) (not (boundp (second form)))))
                 (%%eval (list* operator (second form)
                         (list 'quote (%compiler-evaluate (third form))) (cdddr form)))
                 (%%eval form)))
           (%%eval form)))
      ((defpackage in-package defmacro deftype defclass defstruct define-condition
        defmethod defgeneric defconstant declaim proclaim)
       (%%eval form))
      (otherwise
       (multiple-value-bind (expansion expanded)
           (let ((miricl-compiler::*compiling-evaluation* t)) (macroexpand-1 form))
         (if expanded (%compiler-evaluate expansion)
             (funcall (%compile-evaluation (list 'lambda nil form) nil))))))))
