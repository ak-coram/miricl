;;;; Object-level control/data flow, separate from the definition linker IR.
(in-package :miricl-compiler)

(defstruct hir-unit functions)
(defstruct hir-function name parent parameters environment entry blocks captures bindings values
  (value-count 0) (block-count 0))
(defstruct hir-block id parameters instructions terminator predecessors environment)
(defstruct hir-value id kind owner producer (type t))
(defstruct hir-instruction opcode inputs output attributes origin effect)
(defstruct hir-binding name owner cell special-p captured-p assigned-p)
(defstruct hir-target name owner block token captured-p)
(defvar *hir-unit*)
(defvar *hir-function*)
(defvar *hir-block*)
(defvar *hir-dynamic*)
(defvar *hir-origin*)
(defvar *hir-specials*)

(defun hir-new-value (kind producer)
  (let ((value (make-hir-value :id (hir-function-value-count *hir-function*)
                 :kind kind :owner *hir-function* :producer producer)))
    (incf (hir-function-value-count *hir-function*))
    (push value (hir-function-values *hir-function*)) value))

(defun hir-new-block (&optional parameter-kind)
  (let ((block (make-hir-block :id (hir-function-block-count *hir-function*)
                 :environment (when (and (boundp '*hir-dynamic*)
                                         (eq (hir-value-owner *hir-dynamic*) *hir-function*)) *hir-dynamic*))))
    (incf (hir-function-block-count *hir-function*))
    (when parameter-kind
      (setf (hir-block-parameters block) (list (hir-new-value parameter-kind block))))
    (push block (hir-function-blocks *hir-function*)) block))

(defun hir-emit (opcode inputs kind &optional attributes (effect :pure))
  (when (hir-block-terminator *hir-block*) (error "HIR instruction after a terminator."))
  (let* ((instruction (make-hir-instruction :opcode opcode :inputs inputs
                        :attributes attributes :origin *hir-origin* :effect effect))
         (output (and kind (hir-new-value kind instruction))))
    (setf (hir-instruction-output instruction) output)
    (push instruction (hir-block-instructions *hir-block*)) output))

(defun hir-terminate (opcode inputs attributes)
  (when (hir-block-terminator *hir-block*) (error "Duplicate HIR terminator."))
  (setf (hir-block-terminator *hir-block*)
    (make-hir-instruction :opcode opcode :inputs inputs :attributes attributes
      :origin *hir-origin* :effect :control)))

(defun hir-single (value)
  (if (eq (hir-value-kind value) :multiple)
      (hir-emit :primary (list value) :single) value))

(defun hir-multiple (value)
  (if (eq (hir-value-kind value) :multiple) value
      (hir-emit :values (list value) :multiple)))

(defun hir-nth-value (value index)
  (if (eq (hir-value-kind value) :multiple)
      (let ((producer (hir-value-producer value)))
        (cond ((and (hir-instruction-p producer) (eq (hir-instruction-opcode producer) :values))
               (or (nth index (hir-instruction-inputs producer)) (hir-constant nil)))
              ((zerop index) (hir-single value))
              (t (hir-emit :value-ref (list value) :single (list index)))))
      (if (zerop index) value (hir-constant nil))))

(defun hir-constant (value) (hir-emit :constant nil :single (list value)))

(defun hir-capture (identity kind)
  (when (hir-target-p identity) (setf (hir-target-captured-p identity) t))
  (unless (member identity (hir-function-captures *hir-function*))
    (setf (hir-function-captures *hir-function*)
      (append (hir-function-captures *hir-function*) (list identity))))
  (hir-emit :capture nil kind (list (position identity (hir-function-captures *hir-function*)) identity)))

(defun hir-cell-for-binding (binding)
  (if (eq (hir-binding-owner binding) *hir-function*) (hir-binding-cell binding)
      (progn (setf (hir-binding-captured-p binding) t) (hir-capture binding :single))))

(defun hir-special-p (name declarations)
  (or (member name declarations) (member name *hir-specials*)
      (multiple-value-bind (special native) (compiler-primitive 18 name) (and native special))))

(defun hir-declarations (body &optional documentation)
  (let ((specials nil))
    (loop while body do
      (cond
        ((and documentation (stringp (first body)) (cdr body))
         (setq documentation nil) (pop body))
        ((and (consp (first body)) (eq (caar body) 'declare))
         (dolist (declaration (cdar body))
           (when (eq (car declaration) 'special) (setq specials (append (cdr declaration) specials))))
         (pop body))
        (t (return))))
    (values body specials)))

(defun hir-shadow-specials (variables specials)
  (dolist (name specials variables)
    (push (cons name (make-hir-binding :name name :owner *hir-function* :special-p t)) variables)))

(defun hir-bind-value (name value environment specials)
  (when (or (not (symbolp name)) (member name '(nil t)) (keywordp name))
    (error "Invalid HIR binding name ~S." name))
  (let* ((special (hir-special-p name specials))
         (binding (make-hir-binding :name name :owner *hir-function* :special-p special)))
    (if special
        (setq *hir-dynamic* (hir-emit :dynamic-bind (list *hir-dynamic* value) :dynamic
                             (list name) :allocation))
        (setf (hir-binding-cell binding) (hir-emit :cell-new (list value) :single (list binding) :allocation)))
    (push binding (hir-function-bindings *hir-function*))
    (acons name binding environment)))

(defun hir-read-variable (name environment)
  (let ((binding (cdr (assoc name environment))))
    (if (and binding (not (hir-binding-special-p binding)))
        (hir-emit :cell-read (list (hir-cell-for-binding binding)) :single nil :read)
        (hir-emit :global-read (list *hir-dynamic*) :single (list name) :read))))

(defun hir-write-variable (name value environment)
  (let ((binding (cdr (assoc name environment))))
    (when (or (member name '(nil t)) (keywordp name)) (error "Assignment to constant ~S." name))
    (if (and binding (not (hir-binding-special-p binding)))
        (progn (setf (hir-binding-assigned-p binding) t)
          (hir-emit :cell-write (list (hir-cell-for-binding binding) value) :single nil :write))
        (hir-emit :global-write (list *hir-dynamic* value) :single (list name) :write))))

(defun hir-body (forms variables functions targets)
  (let ((result nil))
    (dolist (form forms)
      (unless (hir-block-terminator *hir-block*)
        (setq result (catch 'hir-path-ended (hir-form form variables functions targets)))))
    (when (hir-block-terminator *hir-block*) (throw 'hir-path-ended nil))
    (or result (hir-constant nil))))

(defun hir-function-reference (name functions)
  (let ((binding (cdr (assoc name functions :test #'equal))))
    (if binding (hir-emit :cell-read (list (hir-cell-for-binding binding)) :single nil :read)
        (hir-emit :function-reference nil :single (list name) :read))))

(defun hir-close (lambda variables functions targets &optional implicit-block)
  (let* ((child (hir-lambda lambda *hir-function* variables functions targets nil implicit-block))
         (captures (mapcar (lambda (capture)
                    (etypecase capture
                      (hir-binding (hir-cell-for-binding capture))
                      (hir-target (if (eq (hir-target-owner capture) *hir-function*)
                                      (hir-target-token capture) (hir-capture capture :dynamic)))))
                   (hir-function-captures child))))
    (hir-emit :closure captures :single (list child) :allocation)))

(defun hir-choice (test yes-form no-form variables functions targets)
  (let ((yes (hir-new-block)) (no (hir-new-block)) (join (hir-new-block :multiple)))
    (hir-terminate :branch (list test) (list yes no))
    (loop for block in (list yes no) for form in (list yes-form no-form) do
      (setq *hir-block* block)
      (let ((value (catch 'hir-path-ended (hir-form form variables functions targets))))
        (unless (hir-block-terminator *hir-block*)
          (hir-terminate :jump (list (hir-multiple value)) (list join)))))
    (setq *hir-block* join) (first (hir-block-parameters join))))

(defun hir-small-membership (item elements)
  ;; The literal tails are shared with the original quoted list. MEMBER must
  ;; return the first matching tail, not a newly allocated list or a boolean.
  (let ((join (hir-new-block :multiple)) (eql (and elements (hir-function-reference 'eql nil))))
    (loop for tail on elements do
      (let* ((key (hir-constant (car tail)))
             (test (hir-single (hir-emit :call (list *hir-dynamic* eql item key) :multiple nil :call)))
             (yes (hir-new-block)) (no (hir-new-block)))
        (hir-terminate :branch (list test) (list yes no))
        (setq *hir-block* yes)
        (hir-terminate :jump (list (hir-multiple (hir-constant tail))) (list join))
        (setq *hir-block* no)))
    (hir-terminate :jump (list (hir-multiple (hir-constant nil))) (list join))
    (setq *hir-block* join) (first (hir-block-parameters join))))

(defun hir-call-arguments (function arguments variables functions targets)
  (let ((values nil))
    (dolist (argument arguments)
      (push (hir-single (hir-form argument variables functions targets)) values))
    (hir-emit :call (list* *hir-dynamic* function (nreverse values)) :multiple nil :call)))

(defun hir-case-form (form variables functions targets)
  (let ((key (hir-single (hir-form (second form) variables functions targets)))
        (join (hir-new-block :multiple)) (default nil) (expected nil)
        (typed (member (car form) '(typecase etypecase))))
    (loop for clauses on (cddr form) for clause = (car clauses) do
      (unless (consp clause) (error "Malformed CASE clause ~S." clause))
      (let* ((head (car clause))
             (otherwise (and (member (car form) '(case typecase)) (member head '(t otherwise))))
             (keys (if (and (not typed) (listp head)) head (list head))))
        (when default
          (if typed (return) (error "CASE default must be last.")))
        (unless otherwise
          (dolist (item keys) (push item expected))
          (let ((test (hir-single (if typed
                         (hir-emit :call (list *hir-dynamic* (hir-function-reference 'typep nil)
                                              key (hir-constant head)) :multiple nil :call)
                         (hir-small-membership key keys))))
                (yes (hir-new-block)) (no (hir-new-block)))
            (hir-terminate :branch (list test) (list yes no))
            (setq *hir-block* yes)
            (let ((value (catch 'hir-path-ended (hir-body (cdr clause) variables functions targets))))
              (unless (hir-block-terminator *hir-block*)
                (hir-terminate :jump (list (hir-multiple value)) (list join))))
            (setq *hir-block* no)))
        (when otherwise
          (setq default t)
          (let ((value (catch 'hir-path-ended (hir-body (cdr clause) variables functions targets))))
            (unless (hir-block-terminator *hir-block*)
              (hir-terminate :jump (list (hir-multiple value)) (list join)))))))
    (unless default
      (if (member (car form) '(ecase etypecase))
          (progn
            (hir-emit :call
              (list *hir-dynamic* (hir-function-reference 'error nil) (hir-constant 'type-error)
                (hir-constant :datum) key (hir-constant :expected-type)
                (hir-constant (cons (if typed 'or 'member) (nreverse expected)))) :multiple nil :call)
            (hir-terminate :unreachable nil nil))
          (hir-terminate :jump (list (hir-multiple (hir-constant nil))) (list join))))
    (setq *hir-block* join) (first (hir-block-parameters join))))

(defun hir-macro-environment (variables functions)
  (let ((environment nil))
    (dolist (entry (reverse variables))
      (setq environment (compiler-primitive (if (hir-binding-special-p (cdr entry)) 11 3)
                          environment (car entry))))
    (dolist (entry (reverse functions))
      (setq environment (compiler-primitive 4 environment (car entry) nil)))
    environment))

(defun hir-expand-local-macros (form variables functions)
  ;; Reuse the runtime's macro lambda-list parser and lexical macro environment
  ;; builder. Only expansion happens here; the resulting body still goes
  ;; through HIR and native code generation, without an interpreted fallback.
  (unless (fboundp 'miricl::%compile-form)
    (error "Local macro expansion requires the host's lexical environment adapter."))
  (miricl::%compile-form form (hir-macro-environment variables functions)))

(defun hir-expand-parallel-places (form variables functions)
  ;; Runtime fast macro expanders encode simple places compactly. For native
  ;; compilation use their saved standard expansions, preserving evaluation
  ;; order and the lexical SETF environment instead of interpreting places.
  (let ((mode (second form)))
    (unless (and (integerp mode) (<= 0 mode 5)) (error 'program-error))
    (funcall (symbol-value (nth mode '(miricl::%general-psetf miricl::%general-rotatef
                 miricl::%general-shiftf miricl::%general-multiple-value-setq
                 miricl::%general-remf miricl::%general-pushnew)))
      (cons (nth mode '(psetf rotatef shiftf multiple-value-setq remf pushnew)) (cddr form))
      (hir-macro-environment variables functions))))

(defun hir-expand-arithmetic-step (form)
  (unless (= (length form) 6) (error 'program-error))
  (let ((flags (fifth form)) (step (gensym)) (limit (gensym)) (next (gensym)))
    (unless (and (integerp flags) (<= 0 flags 3)) (error 'program-error))
    (list 'let* (list (list step (list 'the 'real (third form)))
                      (list limit (list 'the 'real (fourth form)))
                      (list next (list (if (logbitp 0 flags) '- '+)
                                       (list 'the 'real (second form)) step)))
      ;; Leave the iteration variable at its last in-range value. In
      ;; particular, the final step must not violate a declared FIXNUM type.
      (list 'if (list (if (logbitp 0 flags) (if (logbitp 1 flags) '<= '<)
                         (if (logbitp 1 flags) '>= '>)) next limit)
        (list 'return-from (or (compiler-primitive 10 2 "%FINISH") 'miricl::%finish) nil)
        (list 'setq (second form) (list 'the (or (sixth form) t) next))))))

(defun hir-assert-form (form variables functions)
  (unless (cdr form) (error 'program-error))
  (let ((bindings nil) (updates nil)
        (prompt (or (compiler-primitive 10 1 "%ASSERT-PROMPT") 'miricl::%assert-prompt)))
    (dolist (place (third form))
      (multiple-value-bind (temps values stores writer reader)
          (get-setf-expansion place (hir-macro-environment variables functions))
        (setq bindings (append bindings (mapcar #'list temps values)))
        (let ((new (gensym)) (supplied (gensym)))
          (push (list 'multiple-value-bind (list new supplied)
                  (list prompt (list 'quote place) (list 'multiple-value-list reader))
                  (list 'when supplied
                    (list 'multiple-value-bind stores (list 'values-list new) writer))) updates))))
    ;; Place subforms and error arguments stay on the failure path. Each place
    ;; is selected once per failed test; its writer accepts the full value group.
    (list 'loop (list 'if (second form) '(return nil))
      (list* 'let* bindings
        (list* 'cerror "Retry assertion."
          (or (cdddr form) (list "Assertion ~S failed." (list 'quote (second form)))))
        (nreverse updates)))))

(defun hir-restart-options (options casep variables functions targets)
  (let ((values nil))
    (loop while options do
      (let ((key (pop options)))
        (unless options (error 'program-error))
        (let ((value (pop options)))
          (when casep
            (setq key (case key (:report :report-function) (:interactive :interactive-function)
                              (:test :test-function) (otherwise (error 'program-error))))
            (unless (and (eq key :report-function) (stringp value))
              (unless (and (consp value) (eq (car value) 'function))
                (setq value (list 'function value)))))
          (unless (member key '(:report-function :interactive-function :test-function)) (error 'program-error))
          (push (hir-constant key) values)
          (push (hir-single (hir-form value variables functions targets)) values))))
    (hir-emit :list (nreverse values) :single nil :allocation)))

(defun hir-correctable-place-form (form variables functions)
  (let* ((checkp (eq (car form) 'check-type)) (typed (or checkp (eq (car form) 'ctypecase)))
         (key (gensym)) (new (gensym)) (done (gensym)) (description (gensym))
         (expected nil) (clauses nil)
         (interactive (or (compiler-primitive 10 1 "%READ-STORE-VALUE") 'miricl::%read-store-value)))
    (unless (and (cdr form) (or (not checkp) (<= 3 (length form) 4))) (error 'program-error))
    (multiple-value-bind (temps values stores writer reader)
        (get-setf-expansion (second form) (hir-macro-environment variables functions))
      (if checkp
          (setq expected (third form)
                clauses (list (list (list 'typep key (list 'quote expected)) (list 'return-from done nil))))
          (dolist (clause (cddr form))
            (unless (consp clause) (error 'program-error))
            (let* ((head (car clause)) (keys (if (and (not typed) (listp head)) head (list head))))
              (setq expected (append expected keys))
              (push (list (if typed (list 'typep key (list 'quote head))
                              (list 'member key (list 'quote keys)))
                      (list 'return-from done (cons 'progn (cdr clause)))) clauses))))
      (unless checkp (setq clauses (nreverse clauses) expected (cons (if typed 'or 'member) expected)))
      (let ((error-form (list 'error (list 'quote (if (and checkp (cdddr form)) 'simple-type-error 'type-error))
                         :datum key :expected-type (list 'quote expected))))
        (when (and checkp (cdddr form))
          (setq error-form (append error-form (list :format-control "The value ~S is not ~A."
                             :format-arguments (list 'list key description)))))
        (list 'let* (append (mapcar #'list temps values) (list (list key reader))
                      (when (and checkp (cdddr form)) (list (list description (fourth form)))))
          (list 'block done
            (list 'loop (cons 'cond clauses)
              (list 'restart-case error-form
                (list 'store-value (list new) :report "Supply a new value." :interactive interactive
                  (list 'multiple-value-bind stores (list 'values new) writer)
                  (list 'setq key new))))))))))

(defun hir-condition-initializer (form variables functions targets)
  (hir-emit :list (list (hir-constant 'miricl::%compiled-condition-init) (hir-constant form)
                       (hir-close (list 'lambda nil form) variables functions targets)) :single nil :allocation))

(defun hir-define-condition (form variables functions targets)
  (unless (<= 4 (length form)) (error 'program-error))
  (let ((slots nil) (arguments nil))
    (dolist (slot (fourth form))
      (if (symbolp slot) (push (hir-constant slot) slots)
          (let ((options (cdr slot)) (values (list (hir-constant (car slot)))))
            (loop while options do
              (let ((key (pop options)))
                (unless options (error 'program-error))
                (push (hir-constant key) values)
                (let ((value (pop options)))
                  (push (if (eq key :initform) (hir-condition-initializer value variables functions targets)
                            (hir-constant value)) values))))
            (push (hir-emit :list (nreverse values) :single nil :allocation) slots))))
    (push (hir-constant (second form)) arguments) (push (hir-constant (third form)) arguments)
    (push (hir-emit :list (nreverse slots) :single nil :allocation) arguments)
    (dolist (option (cddddr form))
      (unless (consp option) (error 'program-error))
      (let ((values (list (hir-constant (car option)))))
        (case (car option)
          (:report
           (unless (= (length option) 2) (error 'program-error))
           (push (if (stringp (second option)) (hir-constant (second option))
                     (hir-single (hir-form
                       (if (and (consp (second option)) (eq (car (second option)) 'function)) (second option)
                           (list 'function (second option))) variables functions targets))) values))
          (:default-initargs
           (let ((defaults (cdr option)))
             (loop while defaults do
               (push (hir-constant (pop defaults)) values)
               (unless defaults (error 'program-error))
               (push (hir-condition-initializer (pop defaults) variables functions targets) values))))
          (otherwise (dolist (value (cdr option)) (push (hir-constant value) values))))
        (push (hir-emit :list (nreverse values) :single nil :allocation) arguments)))
    (hir-emit :call (list *hir-dynamic*
                     (hir-function-reference (or (compiler-primitive 10 1 "%%DEFINE-CONDITION") 'miricl::%%define-condition) nil)
                     (hir-emit :list (nreverse arguments) :single nil :allocation)) :multiple nil :call)))

(defun hir-restart-expression (form restarts variables functions)
  (let* ((expanded (macroexpand form (hir-macro-environment variables functions)))
         (operator (and (consp expanded) (car expanded)))
         (mode (and (not (assoc operator functions)) (position operator '(signal error warn cerror)))))
    (if mode (list* 'miricl::%%native-restart-signal mode restarts (cdr expanded)) expanded)))

(defun hir-control-form (form variables functions targets)
  (let ((operator (car form)) (bindings nil) (no-error (hir-constant nil)) (payload nil)
        (body nil) (mode nil) (parameters nil))
    (case operator
      (catch
       (unless (cdr form) (error 'program-error))
       (setq payload (hir-single (hir-form (second form) variables functions targets)) body (cddr form) mode 0)
       (unless body (return-from hir-control-form (hir-constant nil))))
      (handler-bind
       (unless (cdr form) (error 'program-error))
       (unless (second form) (return-from hir-control-form (hir-body (cddr form) variables functions targets)))
       (dolist (binding (second form))
         (unless (= (length binding) 2) (error 'program-error))
         (push (hir-constant (first binding)) bindings)
         (push (hir-single (hir-form (second binding) variables functions targets)) bindings))
       (setq body (cddr form) mode 1))
      (handler-case
       (unless (cdr form) (error 'program-error))
       (unless (cddr form) (return-from hir-control-form (hir-form (second form) variables functions targets)))
       (setq body (list (second form)) mode 2)
       (dolist (clause (cddr form))
         (unless (and (consp clause) (cdr clause)) (error 'program-error))
         (if (eq (first clause) :no-error)
             (setq no-error (hir-close (cons 'lambda (cdr clause)) variables functions targets))
             (let* ((parameters (second clause)) (ignored (unless parameters (gensym))))
               (unless (and (listp parameters) (<= (length parameters) 1)) (error 'program-error))
               (push (hir-constant (first clause)) bindings)
               (push (hir-close (list* 'lambda (or parameters (list ignored))
                                  (if ignored (cons (list 'declare (list 'ignore ignored)) (cddr clause))
                                      (cddr clause))) variables functions targets) bindings)))))
      (unwind-protect
       (unless (cdr form) (error 'program-error))
       (let ((cleanup (remove nil (cddr form))))
         (unless cleanup (return-from hir-control-form (hir-form (second form) variables functions targets)))
         (setq payload (hir-close (list* 'lambda nil cleanup) variables functions targets)
               body (list (second form)) mode 3)))
      ((restart-bind restart-case)
       (unless (cdr form) (error 'program-error))
       (let ((casep (eq operator 'restart-case)))
         (setq mode (if casep 5 4))
         (dolist (clause (if casep (cddr form) (second form)))
           (unless (and (consp clause) (symbolp (car clause)) (cdr clause)) (error 'program-error))
           (let ((options (cddr clause)) (function nil))
             (if casep
                 (let ((pairs nil))
                   (loop while (member (car options) '(:report :interactive :test)) do
                     (push (pop options) pairs)
                     (unless options (error 'program-error))
                     (push (pop options) pairs))
                   (setq function (hir-close (list* 'lambda (second clause) options) variables functions targets)
                         options (nreverse pairs)))
                 (setq function (hir-single (hir-form (second clause) variables functions targets))))
             (push (hir-constant (car clause)) bindings) (push function bindings)
             (push (hir-restart-options options casep variables functions targets) bindings)))
         (if casep
             (let ((restarts (gensym)))
               (setq parameters (list restarts)
                     body (list (hir-restart-expression (second form) restarts variables functions))))
             (setq body (cddr form)))))
      ((with-condition-restarts progv)
       (unless (cddr form) (error 'program-error))
       (push (hir-single (hir-form (second form) variables functions targets)) bindings)
       (push (hir-single (hir-form (third form) variables functions targets)) bindings)
       (setq body (cdddr form) mode (if (eq operator 'progv) 7 6))))
    (let ((function (hir-close (list* 'lambda parameters body) variables functions targets)))
      (hir-emit :control-call
        (list* *hir-dynamic* function no-error (or payload (hir-constant nil)) (nreverse bindings))
        :multiple (list mode) :call))))

(defun hir-form (form variables functions targets)
  (let ((*hir-origin* form))
    (cond
      ((and (symbolp form) form (not (eq form t)) (not (keywordp form)))
       (unless (assoc form variables)
         (multiple-value-bind (expansion expanded) (macroexpand-1 form)
           (when expanded (return-from hir-form (hir-form expansion variables functions targets)))))
       (hir-read-variable form variables))
      ((atom form) (hir-constant form))
      (t
       (case (car form)
         (quote (unless (= (length form) 2) (error "Malformed QUOTE.")) (hir-constant (second form)))
         (progn (hir-body (cdr form) variables functions targets))
         ((catch handler-bind handler-case unwind-protect restart-bind restart-case with-condition-restarts progv)
          (hir-control-form form variables functions targets))
         (miricl::%%condition-form
          (unless (member (second form) '(catch throw handler-bind handler-case unwind-protect
                                          restart-bind restart-case with-condition-restarts with-simple-restart
                                          block return-from return tagbody go ignore-errors assert check-type define-condition))
            (error "Native condition lowering is not implemented for ~S." (second form)))
          (hir-form (cdr form) variables functions targets))
         (define-condition (hir-define-condition form variables functions targets))
         (miricl::%%native-restart-signal
          (let ((restarts (hir-single (hir-form (third form) variables functions targets))) (arguments nil))
            (dolist (argument (cdddr form))
              (push (hir-single (hir-form argument variables functions targets)) arguments))
            (hir-emit :restart-signal (list* *hir-dynamic* restarts (nreverse arguments)) :multiple
              (list (second form)) :call)))
         (with-simple-restart
          (let* ((specification (second form)) (control (gensym)) (arguments (gensym)) (stream (gensym)))
            (unless (and (consp specification) (cdr specification)) (error 'program-error))
            (hir-form
              (list 'let (list (list control (second specification))
                              (list arguments (cons 'list (cddr specification))))
                (list 'restart-case (cons 'progn (cddr form))
                  (list (first specification) nil :report
                    (list 'lambda (list stream) (list 'apply '(function format) stream control arguments))
                    '(values nil t)))) variables functions targets)))
         (ignore-errors
          (let ((condition (gensym)))
            (hir-form (list 'handler-case (cons 'progn (cdr form))
                        (list 'error (list condition) (list 'values nil condition))) variables functions targets)))
         (assert
          (hir-form (hir-assert-form form variables functions) variables functions targets))
         (check-type
          (hir-form (hir-correctable-place-form form variables functions) variables functions targets))
         (throw
          (unless (= (length form) 3) (error 'program-error))
          (let* ((tag (hir-single (hir-form (second form) variables functions targets)))
                 (values (hir-multiple (hir-form (third form) variables functions targets))))
            (hir-terminate :throw (list *hir-dynamic* tag values) nil)
            (throw 'hir-path-ended nil)))
         (miricl::quasiquote
          (unless (= (length form) 2) (error 'program-error))
          (unless (fboundp 'miricl::%expand-backquote) (error "Missing backquote expansion adapter."))
          (hir-form (miricl::%expand-backquote (second form)) variables functions targets))
         (miricl::%%parallel-place-form
          (hir-form (hir-expand-parallel-places form variables functions) variables functions targets))
         (miricl::%%arithmetic-step
          (hir-form (hir-expand-arithmetic-step form) variables functions targets))
         (if
          (unless (<= 3 (length form) 4) (error "Malformed IF."))
          (hir-choice (hir-single (hir-form (second form) variables functions targets))
            (third form) (fourth form) variables functions targets))
         ((case ecase typecase etypecase)
          (if (assoc (car form) functions)
              (hir-call-arguments (hir-function-reference (car form) functions) (cdr form) variables functions targets)
              (hir-case-form form variables functions targets)))
         (miricl::%%case-form
          (unless (and (integerp (second form)) (<= 0 (second form) 5)) (error 'program-error))
          (let ((expanded (cons (nth (second form) '(case typecase ecase etypecase ccase ctypecase)) (cddr form))))
            (hir-form (if (< (second form) 4) expanded
                          (hir-correctable-place-form expanded variables functions)) variables functions targets)))
         (the
          (unless (= (length form) 3) (error "Malformed THE."))
          (let ((value (hir-form (third form) variables functions targets)))
            ;; THE preserves the entire value group, including zero values.
            ;; Its declaration is an optimization fact, not a runtime TYPEP call.
            (setf (hir-value-type value) (second form)) value))
         (eval-when
          (if (intersection (second form) '(:execute eval))
              (hir-body (cddr form) variables functions targets) (hir-constant nil)))
         ((macrolet symbol-macrolet)
          (when (eq (car form) 'symbol-macrolet)
            (multiple-value-bind (body specials) (hir-declarations (cddr form))
              (declare (ignore body))
              (dolist (binding (second form))
                (let ((name (first binding)))
                  (unless (and (= (length binding) 2) (symbolp name)
                               (not (constantp name)) (not (hir-special-p name specials)))
                    (error 'program-error))))))
          (hir-form (hir-expand-local-macros form variables functions) variables functions targets))
         (miricl::%%symbol-iterate
          ;; The public macro already establishes the iteration variable,
          ;; declarations, implicit block, tags and final NIL binding. Keep
          ;; its symbol snapshot private, evaluating it just once; assigning
          ;; the user's iteration variable must not change the traversal.
          (let ((state (gensym)))
            (hir-form
              (list 'let (list (list state (third form)))
                (list* 'miricl::%%while state
                  (append (list (list 'setq (second form) (list 'car state)))
                    (cdddr form) (list (list 'setq state (list 'cdr state))))))
              variables functions targets)))
         (miricl::%%while
          (let ((header (hir-new-block)))
            (hir-terminate :jump nil (list header)) (setq *hir-block* header)
            (let ((test (hir-single (hir-form (second form) variables functions targets)))
                  (body (hir-new-block)) (exit (hir-new-block)))
              (hir-terminate :branch (list test) (list body exit))
              (setq *hir-block* body)
              (catch 'hir-path-ended (hir-body (cddr form) variables functions targets))
              (unless (hir-block-terminator *hir-block*) (hir-terminate :jump nil (list header)))
              (setq *hir-block* exit) (hir-constant nil))))
         (tagbody
          (let ((local targets) (tags nil) (exit (hir-new-block)) (*hir-dynamic* *hir-dynamic*))
            ;; Classify tags before macro expansion. An atomic macro expansion
            ;; remains a statement, and tag names have their own namespace.
            (dolist (item (cdr form))
              (when (atom item)
                (unless (or (symbolp item) (integerp item)) (error "Invalid TAGBODY tag ~S." item))
                (when (assoc item tags :test #'eql) (error "Duplicate TAGBODY tag ~S." item))
                (let ((target (make-hir-target :name item :owner *hir-function* :block (hir-new-block))))
                  (push (cons item target) tags) (push (cons (list :tag item) target) local))))
            ;; Captured tags share TAGBODY's dynamic extent. Only the tags
            ;; actually captured allocate runtime tokens in the emitter.
            (dolist (entry (reverse tags))
              (let* ((target (cdr entry))
                     (token (hir-emit :block-enter (list *hir-dynamic*) :dynamic (list target) :control)))
                (setf (hir-target-token target) token)
                (setq *hir-dynamic* (hir-emit :block-environment (list token) :dynamic (list target) :control))))
            (dolist (entry tags) (setf (hir-block-environment (hir-target-block (cdr entry))) *hir-dynamic*))
            (dolist (item (cdr form))
              (if (atom item)
                  (let ((block (hir-target-block (cdr (assoc item tags :test #'eql)))))
                    (unless (hir-block-terminator *hir-block*) (hir-terminate :jump nil (list block)))
                    (setq *hir-block* block))
                  (unless (hir-block-terminator *hir-block*)
                    (catch 'hir-path-ended (hir-form item variables functions local)))))
            (unless (hir-block-terminator *hir-block*) (hir-terminate :jump nil (list exit)))
            (setq *hir-block* exit) (hir-constant nil)))
         (go
          (unless (= (length form) 2) (error "Malformed GO."))
          (let ((target (cdr (assoc (list :tag (second form)) targets :test #'equal))))
            (unless target (error "No lexical tag named ~S." (second form)))
            (if (eq (hir-target-owner target) *hir-function*)
                (hir-terminate :jump nil (list (hir-target-block target)))
                (hir-terminate :block-return
                  (list *hir-dynamic* (hir-capture target :dynamic) (hir-multiple (hir-constant nil))) nil))
            (throw 'hir-path-ended nil)))
         ((let let*)
          (multiple-value-bind (body specials) (hir-declarations (cddr form))
            (let ((*hir-dynamic* *hir-dynamic*) (entry *hir-dynamic*) (local variables) (pending nil))
              ;; LET evaluates every initializer before installing bindings.
              (dolist (binding (second form))
                (let ((name (if (consp binding) (first binding) binding))
                      (value (hir-single (hir-form (if (consp binding) (second binding) nil)
                                          (if (eq (car form) 'let*) local variables) functions targets))))
                  (if (eq (car form) 'let*) (setq local (hir-bind-value name value local specials))
                      (push (cons name value) pending))))
              (dolist (binding (nreverse pending))
                (setq local (hir-bind-value (car binding) (cdr binding) local specials)))
              (let ((value (hir-body body (hir-shadow-specials local specials) functions targets)))
                (when (and (not (eq entry *hir-dynamic*)) (not (hir-block-terminator *hir-block*)))
                  (hir-emit :environment-restore (list entry) nil nil :control))
                value))))
         (setq
          (unless (oddp (length form)) (error "Malformed SETQ."))
          (let ((pairs (cdr form)) (result nil))
            (loop while pairs do
              (let ((name (pop pairs)) (value-form (pop pairs)))
                (unless (symbolp name) (error "Invalid SETQ variable ~S." name))
                (multiple-value-bind (expansion expanded) (unless (assoc name variables) (macroexpand-1 name))
                  (if expanded
                      (setq result (hir-single (hir-form (list 'setf expansion value-form) variables functions targets)))
                      (setq result (hir-write-variable name
                                     (hir-single (hir-form value-form variables functions targets)) variables))))))
            (or result (hir-constant nil))))
         (function
          (unless (= (length form) 2) (error "Malformed FUNCTION."))
          (if (and (consp (second form)) (eq (caadr form) 'lambda))
              (hir-close (second form) variables functions targets)
              (hir-function-reference (second form) functions)))
         (lambda (hir-close form variables functions targets))
         ((flet labels)
          (let ((local functions) (bindings nil))
            (dolist (definition (second form))
              ;; Function cells are filled after closures are created. LABELS
              ;; may capture these cells before that initialization write.
              (let ((binding (make-hir-binding :name (first definition) :owner *hir-function* :assigned-p t)))
                (setf (hir-binding-cell binding) (hir-emit :cell-new (list (hir-constant nil)) :single
                                                 (list binding) :allocation))
                (push binding bindings) (push binding (hir-function-bindings *hir-function*))
                (push (cons (first definition) binding) local)))
            (loop for definition in (second form) for binding in (nreverse bindings) do
              (let ((value (hir-close (cons 'lambda (cdr definition)) variables
                             (if (eq (car form) 'labels) local functions) targets
                             (list (if (consp (first definition)) (second (first definition)) (first definition))))))
                (hir-emit :cell-write (list (hir-cell-for-binding binding) value) :single nil :write)))
            (multiple-value-bind (body specials) (hir-declarations (cddr form))
              (hir-body body (hir-shadow-specials variables specials) local targets))))
         (multiple-value-call
          (let ((function (hir-single (hir-form (second form) variables functions targets))) (groups nil))
            (dolist (child (cddr form)) (push (hir-multiple (hir-form child variables functions targets)) groups))
            (hir-emit :multiple-value-call (list* *hir-dynamic* function (nreverse groups)) :multiple nil :call)))
         (multiple-value-bind
          ;; Keep the binding in this function. Expanding it to a call of an
          ;; optional/rest lambda needlessly captures every outer place
          ;; temporary in SETF and allocates a closure on each loop iteration.
          (let ((value (hir-form (third form) variables functions targets)))
            (multiple-value-bind (body specials) (hir-declarations (cdddr form))
              (let ((*hir-dynamic* *hir-dynamic*) (entry *hir-dynamic*) (local variables) (index 0))
                (dolist (name (second form))
                  (setq local (hir-bind-value name (hir-nth-value value index) local specials))
                  (incf index))
                (let ((result (hir-body body (hir-shadow-specials local specials) functions targets)))
                  (when (and (not (eq entry *hir-dynamic*)) (not (hir-block-terminator *hir-block*)))
                    (hir-emit :environment-restore (list entry) nil nil :control))
                  result)))))
         (funcall
          (when (null (cdr form)) (error "FUNCALL requires a function."))
          (hir-call-arguments
            (if (assoc 'funcall functions) (hir-function-reference 'funcall functions)
                (hir-single (hir-form (second form) variables functions targets)))
            (if (assoc 'funcall functions) (cdr form) (cddr form)) variables functions targets))
         (values
          (let ((function (and (assoc 'values functions) (hir-function-reference 'values functions)))
                (arguments nil))
            (dolist (argument (cdr form))
              (push (hir-single (hir-form argument variables functions targets)) arguments))
            (if function
                (hir-emit :call (list* *hir-dynamic* function (nreverse arguments)) :multiple nil :call)
                (hir-emit :values (nreverse arguments) :multiple))))
         (multiple-value-prog1
          (let ((value (hir-multiple (hir-form (second form) variables functions targets))))
            (hir-body (cddr form) variables functions targets) value))
         (block
          (let* ((join (hir-new-block :multiple))
                 (target (make-hir-target :name (second form) :owner *hir-function* :block join))
                 (token (hir-emit :block-enter (list *hir-dynamic*) :dynamic (list target) :control)))
            (setf (hir-target-token target) token)
            (let ((*hir-dynamic* (hir-emit :block-environment (list token) :dynamic (list target) :control)))
              (let ((value (catch 'hir-path-ended
                             (hir-body (cddr form) variables functions (acons (second form) target targets)))))
                (unless (hir-block-terminator *hir-block*)
                  (hir-terminate :block-return (list *hir-dynamic* token (hir-multiple value)) (list join)))))
            (setq *hir-block* join) (first (hir-block-parameters join))))
         (return-from
          (let ((target (cdr (assoc (second form) targets))))
            (unless target (error "No lexical block named ~S." (second form)))
            (let* ((value (hir-multiple (hir-form (third form) variables functions targets)))
                   (local (eq (hir-target-owner target) *hir-function*))
                   (token (if local (hir-target-token target) (hir-capture target :dynamic))))
              (hir-terminate :block-return (list *hir-dynamic* token value)
                (when local (list (hir-target-block target))))
              (throw 'hir-path-ended nil))))
         (locally
          (multiple-value-bind (body specials) (hir-declarations (cdr form))
            (hir-body body (hir-shadow-specials variables specials) functions targets)))
         (otherwise
          (when (and (symbolp (car form)) (not (assoc (car form) functions :test #'equal)))
            (multiple-value-bind (expansion expanded) (macroexpand-1 form)
              (when expanded (return-from hir-form (hir-form expansion variables functions targets)))))
          (when (and (symbolp (car form))
                     (or (special-operator-p (car form))
                         (member (car form) '(miricl::%%condition-form miricl::%%case-form
                           miricl::%%parallel-place-form miricl::%%list-test miricl::%%list-accumulate
                           miricl::%%numeric-collect miricl::%%arithmetic-step miricl::%%symbol-iterate
                           miricl::quasiquote miricl::unquote miricl::unquote-splicing))))
            (error "HIR lowering is not implemented for ~S." (car form)))
          (when (and (eq (car form) 'member) (not (assoc 'member functions)) (= (length form) 3)
                     (consp (third form)) (eq (car (third form)) 'quote))
            (let* ((elements (second (third form)))
                   (size (and (listp elements) (ignore-errors (list-length elements)))))
              ;; Bound code growth. Larger lists and keyword/test variants
              ;; retain the ordinary library call.
              (when (and size (<= size 8))
                (return-from hir-form
                  (hir-small-membership (hir-single (hir-form (second form) variables functions targets)) elements)))))
          (hir-call-arguments
            (if (symbolp (car form)) (hir-function-reference (car form) functions)
                (hir-single (hir-form (car form) variables functions targets)))
            (cdr form) variables functions targets)))))))

(defun hir-lambda (form parent variables functions targets name &optional implicit-block)
  (unless (and (consp form) (eq (first form) 'lambda)) (error "Expected a lambda expression."))
  (when (member '&key (second form))
    ;; Share the target's existing lambda-list semantics during bootstrap.
    ;; Its normalized required/rest bindings and keyword checks still compile
    ;; through HIR; application bodies do not fall back to interpretation.
    (unless (fboundp 'miricl::%normalize-lambda)
      (error "Keyword normalization requires the host's lambda-list adapter."))
    (when implicit-block
      ;; The local function's named block covers its body, not keyword
      ;; initializers. Insert it before normalization moves those into LET*.
      (let ((body (cddr form)) (prefix nil))
        (when (and (stringp (car body)) (cdr body)) (push (pop body) prefix))
        (loop while (and (consp (car body)) (eq (caar body) 'declare)) do (push (pop body) prefix))
        (setq form (list* 'lambda (second form)
                     (append (nreverse prefix) (list (list* 'block (first implicit-block) body))))
              implicit-block nil)))
    (let ((normalized (miricl::%normalize-lambda (second form) (cddr form))))
      (setq form (list* 'lambda (first normalized) (second normalized)))))
  (let* ((*hir-function* (make-hir-function :name name :parent parent :parameters (second form)))
         (*hir-origin* form) (*hir-block* (hir-new-block))
         (*hir-dynamic* (hir-new-value :dynamic *hir-function*)))
    (setf (hir-function-entry *hir-function*) *hir-block*)
    (setf (hir-block-environment *hir-block*) *hir-dynamic*)
    (setf (hir-function-environment *hir-function*) *hir-dynamic*)
    (push *hir-function* (hir-unit-functions *hir-unit*))
    (multiple-value-bind (body specials)
        (hir-declarations (cddr form) t)
      (let* ((local variables) (index 0) (parameters (second form)) (mode :required)
             (required (or (position-if (lambda (p) (member p lambda-list-keywords)) parameters) (length parameters)))
             (end (or (position-if (lambda (p) (member p '(&rest &key &aux))) parameters) (length parameters)))
             (maximum (if (member '&rest parameters) -1 (- end (if (member '&optional parameters) 1 0))))
             (names nil))
        (catch 'hir-path-ended
        (hir-emit :check-arguments nil nil (list required maximum) :check)
        (loop while parameters do
          (let ((parameter (pop parameters)))
            (cond
              ((eq parameter '&optional)
               (unless (eq mode :required) (error "Misplaced &OPTIONAL.")) (setq mode :optional))
              ((eq parameter '&rest)
               (unless (and (member mode '(:required :optional)) parameters) (error "Misplaced &REST."))
               (setq parameter (pop parameters))
               (when (member parameter names) (error "Duplicate parameter ~S." parameter))
               (push parameter names)
               (setq local (hir-bind-value parameter (hir-emit :rest-argument nil :single (list index)) local specials)
                     mode :after-rest))
              ((eq parameter '&aux)
               (when (eq mode :aux) (error "Duplicate &AUX.")) (setq mode :aux))
              ((member parameter lambda-list-keywords)
               (error "HIR parameter lowering is not implemented for ~S." parameter))
              (t
               (when (eq mode :after-rest) (error "Unexpected parameter after &REST."))
               (let* ((spec (if (consp parameter) parameter (list parameter)))
                      (variable (first spec)) (supplied (and (eq mode :optional) (third spec)))
                      (present nil) (value nil))
                 (unless (and (<= 1 (length spec) (case mode (:required 1) (:optional 3) (otherwise 2)))
                              (or (not (eq mode :required)) (symbolp parameter)))
                   (error "Malformed parameter ~S." parameter))
                 (when (and (not (eq mode :aux))
                            (or (member variable names) (and supplied (or (eq variable supplied) (member supplied names)))))
                   (error "Duplicate parameter ~S." parameter))
                 (case mode
                   (:required (setq value (hir-emit :argument nil :single (list index))))
                   (:aux (setq value (hir-single (hir-form (second spec) local functions targets))))
                   (:optional
                    (setq present (hir-emit :rest-argument nil :single (list index)))
                    (let ((yes (hir-new-block)) (no (hir-new-block)) (join (hir-new-block :multiple)))
                      (hir-terminate :branch (list present) (list yes no))
                      (setq *hir-block* yes)
                      (hir-terminate :jump (list (hir-multiple (hir-emit :argument nil :single (list index)))) (list join))
                      (setq *hir-block* no)
                      (let ((default (catch 'hir-path-ended (hir-single (hir-form (second spec) local functions targets)))))
                        (unless (hir-block-terminator *hir-block*)
                          (hir-terminate :jump (list (hir-multiple default)) (list join))))
                      (setq *hir-block* join value (hir-single (first (hir-block-parameters join)))))))
                 (setq local (hir-bind-value variable value local specials)) (push variable names)
                 (when supplied
                   (setq local (hir-bind-value supplied (hir-emit :truth (list present) :single) local specials))
                   (push supplied names))
                 (unless (eq mode :aux) (incf index)))))))
        (when implicit-block (setq body (list (list* 'block (first implicit-block) body))))
        (let ((value (hir-body body (hir-shadow-specials local specials) functions targets)))
          (unless (hir-block-terminator *hir-block*)
            (hir-terminate :return (list (hir-multiple value)) nil))))))
    (dolist (block (hir-function-blocks *hir-function*))
      (setf (hir-block-instructions block) (nreverse (hir-block-instructions block))))
    (setf (hir-function-blocks *hir-function*) (nreverse (hir-function-blocks *hir-function*))
          (hir-function-values *hir-function*) (nreverse (hir-function-values *hir-function*)))
    *hir-function*))

(defun compile-to-hir (lambda-expression &key name global-specials)
  "Lower a lambda to object-level control/data flow without executing its body.
Unsupported special operators and parameter syntax signal an error."
  (ensure-compiler)
  (let ((*hir-unit* (make-hir-unit)) (*hir-specials* global-specials))
    (hir-lambda lambda-expression nil nil nil nil name)
    (setf (hir-unit-functions *hir-unit*) (nreverse (hir-unit-functions *hir-unit*)))
    (verify-hir *hir-unit*) *hir-unit*))

(defun verify-hir (unit)
  (dolist (function (hir-unit-functions unit))
    (let ((blocks (hir-function-blocks function)) (index (make-hash-table :test 'eq)) (id 0))
      (dolist (block blocks)
        (unless (= (hir-block-id block) id) (error "HIR block IDs must be dense and ordered."))
        (incf id) (setf (gethash block index) t (hir-block-predecessors block) nil))
      (dolist (block blocks)
        (unless (hir-block-terminator block) (error "Unterminated HIR block."))
        (dolist (instruction (append (hir-block-instructions block) (list (hir-block-terminator block))))
          (dolist (input (hir-instruction-inputs instruction))
            (unless (and (hir-value-p input) (eq (hir-value-owner input) function))
              (error "HIR operand crosses function boundaries without a capture."))))
        (let* ((terminator (hir-block-terminator block)) (opcode (hir-instruction-opcode terminator))
               (successors (hir-instruction-attributes terminator)))
          (unless (member opcode '(:branch :jump :return :block-return :throw :unreachable)) (error "Invalid HIR terminator."))
          (let ((seen nil))
            (dolist (successor successors)
              (unless (gethash successor index) (error "HIR edge crosses function boundaries."))
              ;; Outdegree is bounded by two; avoid rescanning a large join's
              ;; entire predecessor list for every new incoming edge.
              (unless (member successor seen)
                (push successor seen) (push block (hir-block-predecessors successor)))))
          (case opcode
            (:branch (unless (and (= (length successors) 2)
                                  (eq (hir-value-kind (first (hir-instruction-inputs terminator))) :single))
                       (error "Invalid HIR conditional.")))
            ((:jump :block-return)
             (when successors
               (unless (and (= (length successors) 1)
                            (if (and (eq opcode :jump) (null (hir-instruction-inputs terminator)))
                                (null (hir-block-parameters (first successors)))
                                (and (= (length (hir-block-parameters (first successors))) 1)
                                     (eq (hir-value-kind (car (last (hir-instruction-inputs terminator)))) :multiple))))
                 (error "Invalid HIR block arguments.")))))))))
  t)
