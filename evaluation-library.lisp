;;;; Browser/Node evaluation policy; forms and literals stay in the local heap.
(defvar miricl-compiler::*compiling-evaluation* nil)
(defun %compile-evaluation (form name &optional named-definition retained-entry)
  ;; Interpretation uses the same checked source lowering and compact HIR as
  ;; minimal COMPILE. The dynamic guard covers preparation, not user execution.
  ;; A retained entry requested by COMPILE can use an already ready compiler.
  (let* ((interpreted (or (not miricl-compiler:*compile-evaluations*)
                          miricl-compiler::*compiling-evaluation*
                          (eq miricl-compiler:*compile-evaluations* :interpreted)))
         (miricl-compiler::*compiling-evaluation* t))
    (when (and retained-entry
               (or interpreted
                   (not (boundp 'miricl-compiler::*compiler-state*))
                   (not (eq miricl-compiler::*compiler-state* :ready))
                   (not (miricl-compiler::compiler-primitive 21 nil))))
      (return-from %compile-evaluation nil))
    (miricl-compiler::compiler-source-form form)
    (miricl-compiler::compiler-function-name name "function name")
    (if (or interpreted
            (not (fboundp 'miricl-compiler:compile-native))
            (%debug-form-p form)
            (= (or (%body-debug-level (cddr form) %debug-default-level) 1) 3))
        (unless retained-entry
          (let* ((miricl-compiler::*hir-debug-info-enabled* t)
                 (miricl-compiler::*hir-debug-write-types* t)
                 (expanded (miricl-compiler::lower-source-to-hir form name)))
            (miricl-compiler::hir-unit-function expanded)))
        (let ((miricl-compiler::*wasm-single-use-entry* (and (null name) (not retained-entry))))
          (miricl-compiler::compile-native-source form name named-definition)))))

(defun %compiler-evaluate (form &optional path)
  ;; Keep the validated source route for bootstrap/compiler preparation and
  ;; when automatic compilation is disabled. Explicit :INTERPRETED and
  ;; DEBUG 3 compilation still use the checked HIR interpreter.
  (when miricl-compiler::*compiling-evaluation*
    (return-from %compiler-evaluate (%%eval form)))
  ;; Preserve EVAL's ordinary-call argument boundary before the compiler's
  ;; source validator. Special operators and macros keep their source errors.
  (when (and (consp form) (symbolp (car form))
             (not (special-operator-p (car form))) (not (macro-function (car form)))
             (not (and (listp (cdr form)) (%proper-sequence-p (cdr form)))))
    (error 'type-error :datum (cdr form) :expected-type %proper-list-type))
  (when (not miricl-compiler:*compile-evaluations*)
    (let ((miricl-compiler::*compiling-evaluation* t))
      (miricl-compiler::compiler-source-form form))
    ;; Keep sequential top-level processing and its executable-cycle guard
    ;; even when ordinary execution uses the source evaluator.
    (unless (and (consp form) (member (car form) '(progn eval-when)))
      (return-from %compiler-evaluate (%%eval form))))
  (when (or (atom form) (eq (car form) 'quote))
    (return-from %compiler-evaluate (%%eval form)))
  (let ((miricl-compiler::*compiling-evaluation* t))
    (miricl-compiler::compiler-source-form form))
  (when (member form path :test #'eq)
    (miricl-compiler::compiler-input-fail "source form" form "circular executable form"))
  ;; Process top-level effects in order at every policy. NIL disables native
  ;; compilation; execution still uses HIR, including compiler preparation.
  (let ((path (cons form path))
        (operator (and (consp form) (car form))))
    (case operator
      (progn
       (if (cdr form)
           (progn (dolist (child (butlast (cdr form))) (%compiler-evaluate child path))
                  (%compiler-evaluate (car (last form)) path)) nil))
      (eval-when
       (unless (cdr form) (error 'program-error))
       (if (intersection (second form) '(:execute eval))
           (%compiler-evaluate (cons 'progn (cddr form)) path) nil))
      (defun
       (let* ((name (second form)) (body (cdddr form)) (prefix nil) (documentation nil))
         (loop while (or (and (stringp (car body)) (cdr body) (not documentation))
                         (and (consp (car body)) (eq (caar body) 'declare))) do
           (let ((item (pop body)))
             (if (stringp item) (setq documentation item) (push item prefix))))
         (let ((function (%compile-evaluation
                           (list* 'lambda (third form)
                             (append (nreverse prefix)
                               (list (list* 'block (if (consp name) (second name) name) body)))) name t)))
           (setf (fdefinition name) function)
           (when (and (symbolp name) (macro-function name)) (setf (macro-function name) nil))
           (setf (documentation name 'function) documentation
                 (documentation function 'function) documentation) name)))
      ;; Variable binding and package/declaration operations keep their native
      ;; evaluator entry points. Defining macros with executable bodies use
      ;; their ordinary expansions below, compiling methods, macro expanders
      ;; and class initfunctions in the same lexical context as the definition.
      ((defvar defparameter)
       (if (and (<= (if (eq operator 'defvar) 2 3) (length form) 4) (symbolp (second form)))
           (progn
             (proclaim (list 'special (second form)))
             (if (and (cddr form) (or (eq operator 'defparameter) (not (boundp (second form)))))
                 (%%eval (list* operator (second form)
                         (list 'quote (%compiler-evaluate (third form) path)) (cdddr form)))
                 (%%eval form)))
           (%%eval form)))
      ((defpackage in-package defconstant declaim proclaim)
       (%%eval form))
      (otherwise
       (multiple-value-bind (expansion expanded)
           (let ((miricl-compiler::*compiling-evaluation* t)) (macroexpand-1 form))
         (if expanded (%compiler-evaluate expansion path)
             (funcall (%compile-evaluation (list 'lambda nil form) nil))))))))
