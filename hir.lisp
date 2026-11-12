;;;; Object-level control/data flow, separate from the definition linker IR.
(in-package :miricl-compiler)

(defvar *hir-load-time-recipes* nil)

;; Internal records have fixed, named vector layouts on both hosts. Keep the
;; ordinary constructor, copier and predicate functions available, while hot
;; field references (including SETF places) expand to a single indexed access.
(defmacro define-hir-record (name &rest slots)
  (let* ((package (symbol-package name))
         (predicate (intern (concatenate 'string (symbol-name name) "-P") package))
         (constructor (intern (concatenate 'string "MAKE-" (symbol-name name) "-RECORD") package))
         (inline-macros (list constructor))
         (forms (list (list* 'defstruct (list name '(:type vector) :named) slots))))
    ;; Hot compiler paths know their record layout. Expand positional creation
    ;; to VECTOR, avoiding interpreted DEFSTRUCT construction and keyword/slot
    ;; lookup for every SSA value and instruction. Ordinary constructors remain
    ;; available for callers using the named keyword interface.
    (setf (get constructor 'miricl::runtime-inline-macro) t)
    (push (list 'defmacro constructor (cons '&optional slots)
            (list* 'list ''vector (list 'list ''quote (list 'quote name))
              (mapcar (lambda (slot) (if (consp slot) (car slot) slot)) slots))) forms)
    (push (list 'deftype name nil (list 'quote (list 'satisfies predicate))) forms)
    (loop for slot in slots for index from 1 do
      (let ((accessor (intern (concatenate 'string (symbol-name name) "-"
                               (symbol-name (if (consp slot) (car slot) slot))) package)))
        (setf (get accessor 'miricl::runtime-inline-macro) t)
        (push accessor inline-macros)
        (push (list 'defmacro accessor '(object)
                (list 'list ''svref (list 'list ''the ''simple-vector 'object) index)) forms)))
    (push (intern (concatenate 'string "%" (symbol-name predicate)) package) inline-macros)
    (setf (get (intern (concatenate 'string "%" (symbol-name predicate)) package)
               'miricl::runtime-inline-macro) t)
    (push (list 'defmacro (intern (concatenate 'string "%" (symbol-name predicate)) package) '(object)
            (list 'let '((record (gensym "HIR-RECORD")))
              (list 'list ''let (list 'list (list 'list 'record 'object))
                (list 'list ''and (list 'list ''simple-vector-p 'record)
                  (list 'list ''>= (list 'list ''length 'record) (1+ (length slots)))
                  (list 'list ''eq (list 'list ''svref (list 'list ''the ''simple-vector 'record) 0)
                    (list 'list ''quote (list 'quote name))))))) forms)
    ;; Macroexpansion side effects alone disappear when loading a cached host
    ;; FASL. Register the same metadata when the generated definitions load.
    (push (list 'eval-when '(:load-toplevel :execute)
            (list 'dolist (list 'macro (list 'quote inline-macros))
              (list 'setf (list 'get 'macro (list 'quote 'miricl::runtime-inline-macro)) t))) forms)
    (cons 'progn (nreverse forms))))

(defstruct hir-unit functions)
(define-hir-record hir-function name parent parameters environment entry blocks captures bindings values
  (value-count 0) (block-count 0) (debug-level 1))
(define-hir-record hir-block id parameters instructions terminator predecessors environment)
(define-hir-record hir-value id kind owner producer (type t))
(define-hir-record hir-instruction opcode inputs output attributes origin effect (safety-level 1))
(define-hir-record hir-binding name owner cell special-p captured-p assigned-p local)
(define-hir-record hir-local definition variables functions targets types policy debug-level used-p function (safety-level 1))
(define-hir-record hir-target name owner block token captured-p)
(define-hir-record hir-debug-site kind operator value variables functions targets types parent owner)
(defvar *hir-unit*)
(defvar *hir-function*)
(defvar *hir-block*)
(defvar *hir-dynamic*)
(defvar *hir-origin*)
(defvar *hir-specials*)
(defvar *hir-types* nil)
(defvar *hir-inline-policy* nil)
(defvar *hir-inlining* nil)
(defvar *hir-inline-small-functions* nil)
(defvar *hir-inline-budget* 128)
(defvar *hir-debug-level* 1)
(defvar *hir-safety-level* 1)
(defvar *hir-source-path* nil)
(defvar *hir-debug-sites* nil)
;; Keep marker generation disabled until both executable HIR and native
;; lowering support the debugger operations. DEBUG 3 currently uses the source
;; interpreter's existing debugger; unsupported markers must not reach either
;; backend while that migration is in progress.
(defvar *hir-debug-info-enabled* nil)

(defun hir-debug-origin (form variables functions targets)
  (unless (and *hir-debug-info-enabled* (= *hir-debug-level* 3))
    (return-from hir-debug-origin form))
  (unless *hir-debug-sites* (setq *hir-debug-sites* (make-hash-table :test 'eq)))
  ;; Debugger evaluation may name any visible binding, including one that the
  ;; program itself never reads. Capture those cells without retaining source.
  (dolist (entry (append variables functions targets))
    (let ((identity (cdr entry)))
      (when (and (%hir-binding-p identity) (not (hir-binding-special-p identity)))
        (setf (hir-binding-assigned-p identity) t))
      (when (%hir-target-p identity) (setf (hir-target-captured-p identity) t))
      (when (or (and (%hir-binding-p identity) (not (hir-binding-special-p identity))
                     (not (eq (hir-binding-owner identity) *hir-function*)))
                (and (%hir-target-p identity) (not (eq (hir-target-owner identity) *hir-function*))))
        (if (%hir-binding-p identity)
            (setf (hir-binding-captured-p identity) t (hir-binding-assigned-p identity) t)
            (setf (hir-target-captured-p identity) t))
        (unless (member identity (hir-function-captures *hir-function*))
          (setf (hir-function-captures *hir-function*)
            (append (hir-function-captures *hir-function*) (list identity)))))))
  (let* ((parent (and (gethash *hir-origin* *hir-debug-sites*)
                      (eq (hir-debug-site-owner *hir-origin*) *hir-function*) *hir-origin*))
         (kind (cond ((consp form) 2)
                     ((and (symbolp form) form (not (eq form t)) (not (keywordp form))) 1)
                     (t 0)))
         (site (make-hir-debug-site-record kind
                 (case kind (1 form) (2 (if (symbolp (car form)) (car form) 'funcall)))
                 nil variables functions targets *hir-types* parent *hir-function*)))
    (setf (gethash site *hir-debug-sites*) t)
    site))

(defun native-optimization-level (body quality-name inherited)
  (declare (type list body) (type symbol quality-name) (type (integer 0 3) inherited))
  (when (and (stringp (car body)) (cdr body)) (setq body (cdr body)))
  (loop while (and (consp (car body)) (eq (caar body) 'declare)) do
    (dolist (spec (cdar body))
      (when (eq (car spec) 'optimize)
        (dolist (quality (cdr spec))
          (let ((name (if (consp quality) (car quality) quality))
                (level (if (consp quality) (second quality) 3)))
            (unless (and (symbolp name) (integerp level) (<= 0 level 3)
                         (or (atom quality) (= (length quality) 2)))
              (error 'program-error))
            (when (eq name quality-name) (setq inherited level))))))
    (setq body (cdr body)))
  inherited)

(defun native-debug-level (body inherited)
  (native-optimization-level body 'debug inherited))

(defun native-safety-level (body inherited)
  (native-optimization-level body 'safety inherited))

(defun hir-new-value (kind producer)
  (declare (type symbol kind) (type simple-vector *hir-function*))
  (let ((value (make-hir-value-record (hir-function-value-count *hir-function*)
                 kind *hir-function* producer)))
    (incf (hir-function-value-count *hir-function*))
    (push value (hir-function-values *hir-function*)) value))

(defun hir-new-block (&optional parameter-kind)
  (declare (type symbol parameter-kind) (type simple-vector *hir-function*))
  (let ((block (make-hir-block-record (hir-function-block-count *hir-function*) nil nil nil nil
                 (when (and (boundp '*hir-dynamic*)
                            (eq (hir-value-owner *hir-dynamic*) *hir-function*)) *hir-dynamic*))))
    (incf (hir-function-block-count *hir-function*))
    (when parameter-kind
      (setf (hir-block-parameters block) (list (hir-new-value parameter-kind block))))
    (push block (hir-function-blocks *hir-function*)) block))

(defun hir-emit (opcode inputs kind &optional attributes (effect :pure))
  (declare (type symbol opcode kind effect) (type list inputs attributes)
           (type simple-vector *hir-block*))
  (when (hir-block-terminator *hir-block*) (error "HIR instruction after a terminator."))
  (let* ((instruction (make-hir-instruction-record opcode inputs nil attributes *hir-origin* effect
                        *hir-safety-level*))
         (output (and kind (hir-new-value kind instruction))))
    (setf (hir-instruction-output instruction) output)
    (when (and *hir-inline-policy* output (eq opcode :call) (not (eq (hir-value-type (second inputs)) t)))
      (setf (hir-value-type output) (hir-function-result-type (hir-value-type (second inputs)))))
    (push instruction (hir-block-instructions *hir-block*)) output))

(defun hir-terminate (opcode inputs attributes)
  (declare (type symbol opcode) (type list inputs attributes)
           (type simple-vector *hir-block*))
  (when (hir-block-terminator *hir-block*) (error "Duplicate HIR terminator."))
  (setf (hir-block-terminator *hir-block*)
    (make-hir-instruction-record opcode inputs nil attributes *hir-origin* :control)))

(defun hir-single (value)
  (declare (type simple-vector value))
  (if (eq (hir-value-kind value) :multiple)
      (hir-emit :primary (list value) :single) value))

(defun hir-multiple (value)
  (declare (type simple-vector value))
  (if (eq (hir-value-kind value) :multiple) value
      (hir-emit :values (list value) :multiple)))

(defun hir-nth-value (value index)
  (declare (type simple-vector value) (type (unsigned-byte 32) index))
  (if (eq (hir-value-kind value) :multiple)
      (let ((producer (hir-value-producer value)))
        (cond ((and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :values))
               (or (nth index (hir-instruction-inputs producer)) (hir-constant nil)))
              ((zerop index) (hir-single value))
              (t (hir-emit :value-ref (list value) :single (list index)))))
      (if (zerop index) value (hir-constant nil))))

(defun hir-constant (value) (hir-emit :constant nil :single (list value)))

(defun hir-capture (identity kind)
  (declare (type simple-vector identity *hir-function*) (type symbol kind))
  (when (%hir-target-p identity) (setf (hir-target-captured-p identity) t))
  (unless (member identity (hir-function-captures *hir-function*))
    (setf (hir-function-captures *hir-function*)
      (append (hir-function-captures *hir-function*) (list identity))))
  (hir-emit :capture nil kind (list (position identity (hir-function-captures *hir-function*)) identity)))

(defun hir-cell-for-binding (binding)
  (declare (type simple-vector binding))
  (if (eq (hir-binding-owner binding) *hir-function*) (hir-binding-cell binding)
      (progn (setf (hir-binding-captured-p binding) t) (hir-capture binding :single))))

(defun hir-special-p (name declarations)
  (declare (type symbol name) (type list declarations))
  (or (member name declarations) (member name *hir-specials*)
      (multiple-value-bind (special native) (compiler-primitive 18 name) (and native special))))

(defun hir-declarations (body &optional documentation)
  (declare (type list body))
  (let ((specials nil) (types nil) (policy nil))
    (loop while body do
      (cond
        ((and documentation (stringp (first body)) (cdr body))
         (setq documentation nil) (pop body))
        ((and (consp (first body)) (eq (caar body) 'declare))
         (dolist (declaration (cdar body))
           (case (car declaration)
             (special (setq specials (append (cdr declaration) specials)))
             (type (dolist (name (cddr declaration)) (push (cons name (second declaration)) types)))
             ((inline notinline)
              (dolist (name (cdr declaration)) (push (cons name (car declaration)) policy)))
             (ftype (dolist (name (cddr declaration))
                      (push (cons (list :ftype name) (second declaration)) policy)))
             ((optimize ignore ignorable dynamic-extent declaration))
             (otherwise
              ;; Unknown declaration names remain opaque type facts. Only
              ;; recognized type specifiers can enable native optimizations.
              (dolist (name (cdr declaration)) (push (cons name (car declaration)) types)))))
         (pop body))
        (t (return))))
    (values body specials (nreverse types) (nreverse policy))))

(defun hir-intersect-types (left right)
  (cond ((eq left t) right) ((eq right t) left) ((equal left right) left)
        (t (list 'and left right))))

(defun hir-type-environment (variables types)
  (declare (type list variables types))
  ;; Key facts by binding identity, not name: rebinding a variable must not
  ;; inherit a free declaration on an outer binding. Keep free declarations
  ;; lexical even when their binding is captured or subsequently assigned.
  (let ((environment *hir-types*))
    (dolist (entry types environment)
      (let* ((key (or (cdr (assoc (car entry) variables)) (car entry)))
             (previous (assoc key environment)))
        (push (cons key (hir-intersect-types (if previous (cdr previous) t) (cdr entry))) environment)))))

(defun hir-inline-environment (functions policy)
  (declare (type list functions policy))
  (let ((environment *hir-inline-policy*))
    (dolist (entry policy environment)
      (let* ((ftype (and (consp (car entry)) (eq (caar entry) :ftype)))
             (name (if ftype (second (car entry)) (car entry)))
             (binding (or (cdr (assoc name functions :test #'equal)) name))
             (key (if ftype (list :ftype binding) binding))
             (previous (and ftype (assoc key environment :test #'equal))))
        (push (cons key (if previous (hir-intersect-types (cdr previous) (cdr entry)) (cdr entry))) environment)))))

(defun hir-bound-type-environment (name variables types)
  (declare (type symbol name) (type list variables types))
  ;; Bound declarations follow the binding into later LET* and lambda-list
  ;; initializers. Free declarations still start at the body's boundary.
  (let ((environment *hir-types*))
    (dolist (entry types environment)
      (when (eq (car entry) name)
        (let* ((key (or (cdr (assoc name variables)) name))
               (previous (assoc key environment)))
          (push (cons key (hir-intersect-types (if previous (cdr previous) t) (cdr entry)))
                environment))))))

(defun hir-function-result-type (type)
  (cond ((and (consp type) (eq (car type) 'function)) (if (cddr type) (third type) t))
        ((and (consp type) (eq (car type) 'and))
         (let ((result t))
           (dolist (part (cdr type) result)
             (setq result (hir-intersect-types result (hir-function-result-type part))))))
        (t t)))

(defun hir-inline-size (form limit)
  (declare (type fixnum limit))
  (let ((work (list form)) (size 0))
    (declare (type fixnum size) (type list work))
    (loop while work do
      (let ((next (pop work)))
        (incf size)
        (when (> size limit) (return-from hir-inline-size nil))
        (when (and (consp next) (not (eq (car next) 'quote)))
          (dolist (child next) (push child work)))))
    size))

(defun hir-inline-candidate (name arguments functions)
  (declare (type list arguments functions))
  ;; Global INLINE names are symbols or SETF names; local declarations use
  ;; binding records. With automatic inlining disabled, avoid a linear scan
  ;; of the entire lexical function group unless a local INLINE is in scope.
  (when (or (>= *hir-debug-level* 2)
            (not (or *hir-inline-small-functions*
                     (loop for entry in *hir-inline-policy* thereis
                       (and (eq (cdr entry) 'inline) (simple-vector-p (car entry)))))))
    (return-from hir-inline-candidate nil))
  (let* ((binding (cdr (assoc name functions :test #'equal)))
         (info (and binding (hir-binding-local binding)))
         (definition (and info (hir-local-definition info)))
         (parameters (second definition))
         (policy (cdr (assoc binding *hir-inline-policy*))))
    (when (and info (< *hir-debug-level* 2) (< (hir-local-debug-level info) 2)
               (not (member binding *hir-inlining*))
               (not (eq policy 'notinline)) (or (eq policy 'inline) *hir-inline-small-functions*)
               (= (length arguments) (length parameters))
               (every (lambda (parameter)
                        (and (symbolp parameter) parameter (not (eq parameter t)) (not (keywordp parameter))
                             (not (member parameter lambda-list-keywords)))) parameters)
               (= (length parameters) (length (remove-duplicates parameters)))
               (hir-inline-size (cddr definition) (min *hir-inline-budget* (if (eq policy 'inline) 128 16)))
               ;; A recursive call can revisit its definition's source while
               ;; that definition is being lowered out of line. Distinct
               ;; definitions may also share body conses. Keep such calls
               ;; instead of presenting semantic inlining recursion as a
               ;; structural source cycle to HIR-FORM's input guard.
               (not (loop for form in (cddr definition) thereis
                      (member form *hir-source-path* :test #'eq))))
      binding)))

(defun hir-inline-body (binding arguments)
  (declare (type simple-vector binding) (type list arguments)
           (type fixnum *hir-inline-budget*))
  ;; Arguments have already been evaluated in the caller. Lower the body in
  ;; its definition's lexical environment so caller shadowing cannot capture
  ;; free variables, functions, blocks, or declaration facts.
  (let* ((info (hir-binding-local binding)) (definition (hir-local-definition info))
         (variables (hir-local-variables info)) (functions (hir-local-functions info))
         (targets (hir-local-targets info)) (*hir-types* (hir-local-types info))
         (*hir-inline-policy* (hir-local-policy info))
         (*hir-debug-level* (hir-local-debug-level info))
         (*hir-safety-level* (hir-local-safety-level info))
         (*hir-inlining* (cons binding *hir-inlining*))
         (*hir-dynamic* *hir-dynamic*) (entry *hir-dynamic*))
    (decf *hir-inline-budget* (hir-inline-size (cddr definition) 128))
    (multiple-value-bind (body specials types policy) (hir-declarations (cddr definition) t)
      (loop for name in (second definition) for value in arguments do
        (setq variables (hir-bind-value name value variables specials)))
      (setq variables (hir-shadow-specials variables specials))
      (let* ((*hir-types* (hir-type-environment variables types))
             (*hir-inline-policy* (hir-inline-environment functions policy))
             (name (first definition))
             (value (hir-block-form (list* 'block (if (consp name) (second name) name) body)
                      variables functions targets)))
        (when (and (not (eq entry *hir-dynamic*)) (not (hir-block-terminator *hir-block*)))
          (hir-emit :environment-restore (list entry) nil nil :control))
        value))))

(defun hir-shadow-specials (variables specials)
  (declare (type list variables specials))
  (dolist (name specials variables)
    (push (cons name (make-hir-binding-record name *hir-function* nil t)) variables)))

(defun hir-bind-value (name value environment specials)
  (declare (type simple-vector value) (type list environment specials))
  (when (or (not (symbolp name)) (member name '(nil t)) (keywordp name))
    (error "Invalid HIR binding name ~S." name))
  (let* ((special (hir-special-p name specials))
         (binding (make-hir-binding-record name *hir-function* nil special)))
    (if special
        (setq *hir-dynamic* (hir-emit :dynamic-bind (list *hir-dynamic* value) :dynamic
                             (list name) :allocation))
        (setf (hir-binding-cell binding) (hir-emit :cell-new (list value) :single (list binding) :allocation)))
    (push binding (hir-function-bindings *hir-function*))
    (acons name binding environment)))

(defun hir-read-variable (name environment)
  (declare (type symbol name) (type list environment))
  (let* ((binding (cdr (assoc name environment)))
         (value (if (and binding (not (hir-binding-special-p binding)))
                    (hir-emit :cell-read (list (hir-cell-for-binding binding)) :single nil :read)
                    (hir-emit :global-read (list *hir-dynamic*) :single (list name) :read)))
         (type (assoc (or binding name) *hir-types*)))
    (when type (setf (hir-value-type value) (cdr type)))
    value))

(defun hir-write-variable (name value environment)
  (declare (type symbol name) (type simple-vector value) (type list environment))
  (let ((binding (cdr (assoc name environment))))
    (when (or (member name '(nil t)) (keywordp name)) (error "Assignment to constant ~S." name))
    (if (and binding (not (hir-binding-special-p binding)))
        (progn (setf (hir-binding-assigned-p binding) t)
          (hir-emit :cell-write (list (hir-cell-for-binding binding) value) :single nil :write))
        (hir-emit :global-write (list *hir-dynamic* value) :single (list name) :write))))

(defun hir-body (forms variables functions targets)
  (declare (type list forms variables functions targets))
  (let ((result nil))
    (dolist (form forms)
      (unless (hir-block-terminator *hir-block*)
        (setq result (catch 'hir-path-ended (hir-form form variables functions targets)))))
    (when (hir-block-terminator *hir-block*) (throw 'hir-path-ended nil))
    (or result (hir-constant nil))))

(defun hir-function-reference (name functions)
  (declare (type list functions))
  (let ((binding (cdr (assoc name functions :test #'equal))))
    (when (and binding (hir-binding-local binding))
      (setf (hir-local-used-p (hir-binding-local binding)) t))
    (let ((value (if binding (hir-emit :cell-read (list (hir-cell-for-binding binding)) :single nil :read)
                     (hir-emit :function-reference nil :single
                       (let ((policy (cdr (assoc name *hir-inline-policy* :test #'equal))))
                         (if (member policy '(inline notinline)) (list name policy) (list name))) :read)))
          (type (and *hir-inline-policy* (assoc (list :ftype (or binding name)) *hir-inline-policy* :test #'equal))))
      (when type (setf (hir-value-type value) (cdr type)))
      value)))

(defun hir-close (lambda variables functions targets &optional implicit-block)
  (declare (type list lambda variables functions targets implicit-block))
  (let* ((child (hir-lambda lambda *hir-function* variables functions targets nil implicit-block))
         (captures (mapcar (lambda (capture)
                    (etypecase capture
                      (hir-binding (hir-cell-for-binding capture))
                      (hir-target (if (eq (hir-target-owner capture) *hir-function*)
                                      (hir-target-token capture) (hir-capture capture :dynamic)))))
                   (hir-function-captures child))))
    (hir-emit :closure captures :single (list child) :allocation)))

(defun hir-small-membership (item elements)
  (declare (type simple-vector item) (type list elements))
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

(defun hir-membership-branches (item elements)
  (declare (type simple-vector item) (type list elements))
  ;; A truth-only consumer needs no matching list tail or value-producing
  ;; joins. Keep first-match order and one EQL lookup for this key group.
  (let ((eql (and elements (hir-function-reference 'eql nil))) (branches nil))
    (dolist (element elements)
      (let ((test (hir-single (hir-emit :call
                    (list *hir-dynamic* eql item (hir-constant element)) :multiple nil :call)))
            (next (hir-new-block)))
        ;; Allocate the shared true block after the test chain, so its edges
        ;; stay forward. Complete these private edges before exposing the HIR.
        (push (hir-terminate :branch (list test) (list nil next)) branches)
        (setq *hir-block* next)))
    (let ((yes (hir-new-block)))
      (dolist (branch branches) (setf (first (hir-instruction-attributes branch)) yes))
      (values yes *hir-block*))))

(defun hir-call-arguments (function arguments variables functions targets)
  (declare (type simple-vector function) (type list arguments variables functions targets))
  (let ((values nil))
    (dolist (argument arguments)
      (push (hir-single (hir-form argument variables functions targets)) values))
    (hir-emit :call (list* *hir-dynamic* function (nreverse values)) :multiple nil :call)))

(defun hir-literal-apply-form (form functions)
  ;; A literal lambda cannot observe its function object here. Inline its
  ;; parameter bindings after checking the final APPLY argument. A sole &REST
  ;; parameter may share that list (CLHS APPLY). Bound required-parameter growth
  ;; and leave optional/keyword and mixed required/rest lists alone.
  (when (and (= (length form) 3) (not (assoc 'apply functions)))
    (let* ((designator (second form))
           (lambda-form (if (and (consp designator) (eq (car designator) 'function)
                                 (= (length designator) 2)) (second designator) designator))
           (parameters (and (consp lambda-form) (second lambda-form)))
           (rest-name (and (consp parameters) (eq (car parameters) '&rest)
                           (= (length parameters) 2) (second parameters)))
           (names (if rest-name (list rest-name) parameters)))
      (when (and (consp lambda-form) (eq (car lambda-form) 'lambda)
                 (listp names) (<= (length names) 4)
                 (every (lambda (name) (and (symbolp name) name (not (eq name t))
                                           (not (keywordp name)) (not (member name lambda-list-keywords))))
                        names)
                 (= (length (remove-duplicates names)) (length names)))
        (let ((arguments (gensym)) (body (cddr lambda-form)) (bindings nil) (index 0))
          (when (and (stringp (car body)) (cdr body)) (setq body (cdr body)))
          (if rest-name (push (list rest-name arguments) bindings)
              (dolist (name names)
                (push (list name (list 'miricl::%%checked-argument arguments index)) bindings)
                (incf index)))
          (list 'let (list (list arguments (third form)))
            (if rest-name (list 'miricl::%%check-apply-list arguments)
                (list 'miricl::%%check-apply-arguments arguments (length names)))
            (cons 'let (cons (nreverse bindings) body))))))))

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
          (multiple-value-bind (yes no)
              (if typed
                  (let ((test (hir-single (hir-emit :call
                                (list *hir-dynamic* (hir-function-reference 'typep nil)
                                      key (hir-constant head)) :multiple nil :call)))
                        (yes (hir-new-block)) (no (hir-new-block)))
                    (hir-terminate :branch (list test) (list yes no))
                    (values yes no))
                  (hir-membership-branches key keys))
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

(defun hir-tagbody-form (form variables functions targets)
  (let ((local targets) (tags nil) (exit (hir-new-block)) (*hir-dynamic* *hir-dynamic*))
    ;; Classify tags before macro expansion. An atomic macro expansion
    ;; remains a statement, and tag names have their own namespace.
    (dolist (item (cdr form))
      (when (atom item)
        (unless (or (symbolp item) (integerp item)) (error "Invalid TAGBODY tag ~S." item))
        (when (assoc item tags :test #'eql) (error "Duplicate TAGBODY tag ~S." item))
        (let ((target (make-hir-target-record item *hir-function* (hir-new-block))))
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

(defun hir-setq-form (form variables functions targets)
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

(defun hir-local-functions-form (form variables functions targets)
  (let ((local functions) (bindings nil))
    (dolist (definition (second form))
      ;; Function cells are filled after closures are created. LABELS
      ;; may capture these cells before that initialization write.
      (let ((binding (make-hir-binding-record (first definition) *hir-function* nil nil nil t)))
        (setf (hir-binding-cell binding) (hir-emit :cell-new (list (hir-constant nil)) :single
                                         (list binding) :allocation))
        (push binding bindings) (push binding (hir-function-bindings *hir-function*))
        (push (cons (first definition) binding) local)))
    (setq bindings (nreverse bindings))
    (loop for definition in (second form) for binding in bindings do
      (setf (hir-binding-local binding)
        (make-hir-local :definition definition :variables variables
          :functions (if (eq (car form) 'labels) local functions) :targets targets
          :types *hir-types* :policy *hir-inline-policy*
          :safety-level (native-safety-level (cddr definition) *hir-safety-level*)
          :debug-level (native-debug-level (cddr definition) *hir-debug-level*))))
    (multiple-value-bind (body specials types policy) (hir-declarations (cddr form))
      (let* ((variables (hir-shadow-specials variables specials))
             (*hir-debug-level* (native-debug-level (cddr form) *hir-debug-level*))
             (*hir-safety-level* (native-safety-level (cddr form) *hir-safety-level*))
             (*hir-types* (hir-type-environment variables types))
             (*hir-inline-policy* (hir-inline-environment local policy))
             (entry *hir-block*) (prefix (hir-block-instructions entry))
             (environment *hir-dynamic*) (compiled nil))
        (when (eq (car form) 'labels)
          (dolist (binding bindings)
            (setf (hir-local-policy (hir-binding-local binding)) *hir-inline-policy*)))
        (let* ((result (catch 'hir-path-ended (list (hir-body body variables local targets))))
               (instructions (hir-block-instructions entry)) (terminator (hir-block-terminator entry)))
        ;; Only materialize function objects which survive inlining. Discover
        ;; references transitively while compiling their bodies, then insert
        ;; their initialization before the already lowered enclosing body.
        (let ((*hir-block* entry) (*hir-dynamic* environment))
          (setf (hir-block-instructions entry) prefix (hir-block-terminator entry) nil)
          (loop
            (let ((pending (remove-if-not
                    (lambda (binding) (and (hir-local-used-p (hir-binding-local binding))
                                           (not (member binding compiled)))) bindings)))
              (unless pending (return))
              (dolist (binding pending)
                (push binding compiled)
                (let* ((info (hir-binding-local binding)) (definition (hir-local-definition info))
                       (*hir-types* (hir-local-types info)) (*hir-inline-policy* (hir-local-policy info))
                       (*hir-debug-level* (hir-local-debug-level info))
                       (*hir-safety-level* (hir-local-safety-level info))
                       (function (hir-close (cons 'lambda (cdr definition)) (hir-local-variables info)
                                   (hir-local-functions info) (hir-local-targets info)
                                   (list (if (consp (first definition)) (second (first definition)) (first definition))))))
                  (setf (hir-local-function info) (first (hir-instruction-attributes (hir-value-producer function))))
                  (hir-emit :cell-write (list (hir-cell-for-binding binding) function) :single nil :write)))))
          (setf (hir-block-instructions entry)
                  (append (ldiff instructions prefix) (hir-block-instructions entry))
                (hir-block-terminator entry) terminator))
        (unless result (throw 'hir-path-ended nil))
        (first result))))))

(defun hir-multiple-value-bind-form (form variables functions targets)
  ;; Keep the binding in this function. Expanding it to a call of an
  ;; optional/rest lambda needlessly captures every outer place
  ;; temporary in SETF and allocates a closure on each loop iteration.
  (let ((value (hir-form (third form) variables functions targets)))
    (multiple-value-bind (body specials types policy) (hir-declarations (cdddr form))
      (let ((*hir-dynamic* *hir-dynamic*) (entry *hir-dynamic*) (local variables) (index 0))
        (dolist (name (second form))
          (setq local (hir-bind-value name (hir-nth-value value index) local specials))
          (incf index))
        (setq local (hir-shadow-specials local specials))
        (let ((result (let ((*hir-debug-level* (native-debug-level (cdddr form) *hir-debug-level*))
                           (*hir-safety-level* (native-safety-level (cdddr form) *hir-safety-level*))
                           (*hir-types* (hir-type-environment local types))
                           (*hir-inline-policy* (hir-inline-environment functions policy)))
                        (hir-body body local functions targets))))
          (when (and (not (eq entry *hir-dynamic*)) (not (hir-block-terminator *hir-block*)))
            (hir-emit :environment-restore (list entry) nil nil :control))
          result)))))

(defun hir-block-form (form variables functions targets)
  (let* ((join (hir-new-block :multiple))
         (target (make-hir-target-record (second form) *hir-function* join))
         (token (hir-emit :block-enter (list *hir-dynamic*) :dynamic (list target) :control)))
    (setf (hir-target-token target) token)
    (let ((*hir-dynamic* (hir-emit :block-environment (list token) :dynamic (list target) :control)))
      (let ((value (catch 'hir-path-ended
                     (hir-body (cddr form) variables functions (acons (second form) target targets)))))
        (unless (hir-block-terminator *hir-block*)
          (hir-terminate :block-return (list *hir-dynamic* token (hir-multiple value)) (list join)))))
    (setq *hir-block* join) (first (hir-block-parameters join))))

(defun hir-simple-restart-form (form variables functions targets)
  (let* ((specification (second form)) (control (gensym)) (arguments (gensym)) (stream (gensym)))
    (unless (and (consp specification) (cdr specification)) (error 'program-error))
    (hir-form
      (list 'let (list (list control (second specification))
                      (list arguments (cons 'list (cddr specification))))
        (list 'restart-case (cons 'progn (cddr form))
          (list (first specification) nil :report
            (list 'lambda (list stream) (list 'apply '(function format) stream control arguments))
            '(values nil t)))) variables functions targets)))

(defun hir-restart-signal-form (form variables functions targets)
  (let ((restarts (hir-single (hir-form (third form) variables functions targets))) (arguments nil))
    (dolist (argument (cdddr form))
      (push (hir-single (hir-form argument variables functions targets)) arguments))
    (hir-emit :restart-signal (list* *hir-dynamic* restarts (nreverse arguments)) :multiple
      (list (second form)) :call)))

(defun hir-symbol-iterate-form (form variables functions targets)
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

(defun hir-while-form (form variables functions targets)
  (let ((header (hir-new-block)))
    (hir-terminate :jump nil (list header)) (setq *hir-block* header)
    (let ((test (hir-single (hir-form (second form) variables functions targets)))
          (body (hir-new-block)) (exit (hir-new-block)))
      (hir-terminate :branch (list test) (list body exit))
      (setq *hir-block* body)
      (catch 'hir-path-ended (hir-body (cddr form) variables functions targets))
      (unless (hir-block-terminator *hir-block*) (hir-terminate :jump nil (list header)))
      (setq *hir-block* exit) (hir-constant nil))))

(defun hir-multiple-value-call-form (form variables functions targets)
  ;; Reuse literal APPLY inlining after exposing the complete value
  ;; group as its argument list. SETF and MULTIPLE-VALUE-BIND expansions
  ;; commonly introduce these immediately invoked closures.
  (when (= (length form) 3)
    (let ((expanded (hir-literal-apply-form
                     (list 'apply (second form)
                       (list 'miricl::%%native-value-list (third form))) nil)))
      (when expanded
        (return-from hir-multiple-value-call-form (hir-form expanded variables functions targets)))))
  (let ((function (hir-single (hir-form (second form) variables functions targets))) (groups nil))
    (dolist (child (cddr form)) (push (hir-multiple (hir-form child variables functions targets)) groups))
    (hir-emit :multiple-value-call (list* *hir-dynamic* function (nreverse groups)) :multiple nil :call)))

(defun hir-return-from-form (form variables functions targets)
  (let ((target (cdr (assoc (second form) targets))))
    (unless target (error "No lexical block named ~S." (second form)))
    (let* ((value (hir-multiple (hir-form (third form) variables functions targets)))
           (local (eq (hir-target-owner target) *hir-function*))
           (token (if local (hir-target-token target) (hir-capture target :dynamic))))
      (hir-terminate :block-return (list *hir-dynamic* token value)
        (when local (list (hir-target-block target))))
      (throw 'hir-path-ended nil))))

(defun hir-literal-membership (form functions)
  (declare (type list functions))
  (when (and (consp form) (eq (car form) 'member) (< *hir-debug-level* 2)
             (not (assoc 'member functions)) (not (macro-function 'member)))
    ;; IF may recognize this call before its normal lowering task. Validate
    ;; syntax before inspecting it; quoted data can still be dotted or cyclic.
    (compiler-source-form form)
    (when (and (= (length form) 3) (consp (third form)) (eq (car (third form)) 'quote))
      (compiler-source-form (third form))
      (let* ((elements (second (third form)))
             (size (and (listp elements) (ignore-errors (list-length elements)))))
        (when (and size (<= size 8)) (cons (second form) elements))))))

(defun hir-application-form (form variables functions targets)
  (when (and (symbolp (car form)) (not (assoc (car form) functions :test #'equal)))
    (multiple-value-bind (expansion expanded) (macroexpand-1 form)
      (when expanded (return-from hir-application-form (hir-form expansion variables functions targets)))))
  (when (and (symbolp (car form))
             (or (special-operator-p (car form))
                 (member (car form) '(miricl::%%condition-form miricl::%%case-form
                   miricl::%%parallel-place-form miricl::%%list-test miricl::%%list-accumulate
                   miricl::%%numeric-collect miricl::%%arithmetic-step miricl::%%symbol-iterate
                   miricl::quasiquote miricl::unquote miricl::unquote-splicing))))
    (error "HIR lowering is not implemented for ~S." (car form)))
  (let ((membership (hir-literal-membership form functions)))
    (when membership
      (return-from hir-application-form
        (hir-small-membership (hir-single (hir-form (car membership) variables functions targets)) (cdr membership)))))
  (hir-call-arguments
    (if (symbolp (car form)) (hir-function-reference (car form) functions)
        (hir-single (hir-form (car form) variables functions targets)))
    (cdr form) variables functions targets))

(defun hir-form (form variables functions targets)
  ;; Lower expression trees with heap continuations. Each task carries the
  ;; source and lexical dynamic context in which its instructions are emitted;
  ;; *HIR-BLOCK* alone follows control flow between tasks. Branch boundaries
  ;; catch ended paths before lowering the next arm or continuation.
  (let ((work nil) (result nil) (*hir-safety-level* *hir-safety-level*))
    (declare (type list variables functions targets work)
             (type (integer 0 3) *hir-safety-level*))
    (labels
        ((defer (function &optional boundary)
           (push (list function boundary *hir-origin* *hir-debug-level* *hir-dynamic*
                   ;; Most tasks have no declaration environment. Keep their
                   ;; policy as an immediate instead of allocating a seventh
                   ;; task slot or an otherwise empty environment list.
                   (if (or *hir-types* *hir-inline-policy* *hir-inlining*)
                       (list *hir-types* *hir-inline-policy* *hir-inlining* *hir-safety-level*)
                       *hir-safety-level*) *hir-source-path*) work))
         (body (forms variables functions targets)
           (if (null forms) (setq result (hir-constant nil))
               (progn
                 (when (cdr forms)
                   (defer (lambda () (body (cdr forms) variables functions targets))))
                 (lower (car forms) variables functions targets))))
         (arguments (forms values function variables functions targets &optional valuesp inline)
           (if (null forms)
               (setq result
                 (cond (inline (hir-inline-body inline (nreverse values)))
                       (valuesp (hir-emit :values (nreverse values) :multiple))
                       (t (hir-emit :call (list* *hir-dynamic* function (nreverse values)) :multiple nil :call))))
               (progn
                 (defer (lambda ()
                          (arguments (cdr forms) (cons (hir-single result) values)
                            function variables functions targets valuesp inline)))
                 (lower (car forms) variables functions targets))))
         (bindings (remaining local pending sequential specials types policy forms entry functions targets debug-level safety-level)
           (if remaining
               (let* ((binding (car remaining))
                      (name (if (consp binding) (first binding) binding)))
                 (defer (lambda ()
                          (let ((value (hir-single result)))
                            (when sequential
                              (setq local (hir-bind-value name value local specials))
                              (when types (setq *hir-types* (hir-bound-type-environment name local types))))
                            (bindings (cdr remaining)
                              local
                              (if sequential pending (cons (cons name value) pending))
                              sequential specials types policy forms entry functions targets debug-level safety-level))))
                 (lower (if (consp binding) (second binding) nil) local functions targets))
               (progn
                 (dolist (binding (nreverse pending))
                   (setq local (hir-bind-value (car binding) (cdr binding) local specials)))
                 (unless (eq entry *hir-dynamic*)
                   (defer (lambda ()
                            (unless (hir-block-terminator *hir-block*)
                              (hir-emit :environment-restore (list entry) nil nil :control)))))
                 (setq local (hir-shadow-specials local specials))
                 (let ((*hir-debug-level* debug-level)
                       (*hir-safety-level* safety-level)
                       (*hir-types* (hir-type-environment local types))
                       (*hir-inline-policy* (hir-inline-environment functions policy)))
                   (body forms local functions targets)))))
         (lower (form variables functions targets)
           (compiler-source-form form)
           (when (and (consp form) (member form *hir-source-path* :test #'eq))
             (compiler-input-fail "source form" form "circular executable form"))
           (let* ((*hir-origin* (if *hir-debug-info-enabled*
                                   (hir-debug-origin form variables functions targets)
                                   form))
                  (site (when (and *hir-debug-info-enabled* (= *hir-debug-level* 3)) *hir-origin*))
                 (*hir-source-path* (if (consp form) (cons form *hir-source-path*) *hir-source-path*)))
             (when site
               (hir-emit :debug-step nil nil (list site) :debug)
               (defer (lambda ()
                        (setf (hir-debug-site-value site) result)
                        (unless (hir-block-terminator *hir-block*)
                          (hir-emit :debug-resume nil nil (list (hir-debug-site-parent site)) :debug)))))
             (defer
               (lambda ()
                 (when (>= *hir-debug-level* 2)
                   (setf (hir-function-debug-level *hir-function*)
                         (max *hir-debug-level* (hir-function-debug-level *hir-function*))))
                 (cond
                   ((atom form)
                    (if (and (symbolp form) form (not (eq form t)) (not (keywordp form))
                             (not (assoc form variables)))
                        (multiple-value-bind (expansion expanded) (macroexpand-1 form)
                          (if expanded (lower expansion variables functions targets)
                              (setq result (hir-read-variable form variables))))
                        (setq result (if (and (symbolp form) form (not (eq form t)) (not (keywordp form)))
                                         (hir-read-variable form variables) (hir-constant form)))))
                   ((eq (car form) 'if)
                    (unless (<= 3 (length form) 4) (error "Malformed IF."))
                    (let ((membership (hir-literal-membership (second form) functions)))
                    (defer
                      (lambda ()
                        (multiple-value-bind (yes no)
                            (if membership (hir-membership-branches (hir-single result) (cdr membership))
                                (let ((yes (hir-new-block)) (no (hir-new-block)))
                                  (hir-terminate :branch (list (hir-single result)) (list yes no))
                                  (values yes no)))
                         (let ((join (hir-new-block :multiple)))
                          (setq *hir-block* yes)
                          (defer
                            (lambda ()
                              (unless (hir-block-terminator *hir-block*)
                                (hir-terminate :jump (list (hir-multiple result)) (list join)))
                              (setq *hir-block* no)
                              (defer
                                (lambda ()
                                  (unless (hir-block-terminator *hir-block*)
                                    (hir-terminate :jump (list (hir-multiple result)) (list join)))
                                  (setq *hir-block* join result (first (hir-block-parameters join)))) t)
                              (lower (fourth form) variables functions targets)) t)
                          (lower (third form) variables functions targets)))))
                    (lower (if membership (car membership) (second form)) variables functions targets)))
                   ((eq (car form) 'progn) (body (cdr form) variables functions targets))
                   ((eq (car form) 'locally)
                    (let ((*hir-debug-level* (native-debug-level (cdr form) *hir-debug-level*))
                          (*hir-safety-level* (native-safety-level (cdr form) *hir-safety-level*)))
                      (multiple-value-bind (forms specials types policy) (hir-declarations (cdr form))
                        (let* ((variables (hir-shadow-specials variables specials))
                               (*hir-types* (hir-type-environment variables types))
                               (*hir-inline-policy* (hir-inline-environment functions policy)))
                          (body forms variables functions targets)))))
                   ((member (car form) '(let let*))
                    (multiple-value-bind (forms specials types policy) (hir-declarations (cddr form))
                      (bindings (second form) variables nil (eq (car form) 'let*) specials types policy forms
                        *hir-dynamic* functions targets (native-debug-level (cddr form) *hir-debug-level*)
                        (native-safety-level (cddr form) *hir-safety-level*))))
                   ((eq (car form) 'the)
                    (unless (= (length form) 3) (error "Malformed THE."))
                    (defer (lambda () (setf (hir-value-type result)
                                       (hir-intersect-types (hir-value-type result) (second form)))))
                    (lower (third form) variables functions targets))
                   ((eq (car form) 'values)
                    (arguments (cdr form) nil
                      (and (assoc 'values functions) (hir-function-reference 'values functions))
                      variables functions targets (not (assoc 'values functions))))
                   ((eq (car form) 'funcall)
                    (when (null (cdr form)) (error "FUNCALL requires a function."))
                    (if (assoc 'funcall functions)
                        (arguments (cdr form) nil (hir-function-reference 'funcall functions) variables functions targets)
                        (progn
                          (defer (lambda ()
                                   (arguments (cddr form) nil (hir-single result) variables functions targets)))
                          (lower (second form) variables functions targets))))
                   ((eq (car form) 'apply)
                    (let ((expanded (hir-literal-apply-form form functions)))
                      (if expanded (lower expanded variables functions targets)
                          (arguments (cdr form) nil (hir-function-reference 'apply functions) variables functions targets))))
                   ;; Keep the specialized special-operator and MEMBER lowering.
                   ;; Their expression children re-enter this worklist frontend.
                   ((or (and (symbolp (car form)) (special-operator-p (car form)))
                        (member (car form) '(lambda case ecase typecase etypecase member
                          define-condition with-simple-restart ignore-errors assert check-type
                          handler-bind handler-case restart-bind restart-case with-condition-restarts
                          multiple-value-bind
                          miricl::%%condition-form miricl::%%case-form miricl::%%parallel-place-form
                          miricl::%%list-test miricl::%%list-accumulate miricl::%%numeric-collect
                          miricl::%%arithmetic-step miricl::%%symbol-iterate miricl::%%while
                          miricl::%%native-restart-signal miricl::%%native-value-list
                          miricl::%%check-apply-list miricl::%%check-apply-arguments miricl::%%checked-argument
                          miricl::quasiquote miricl::unquote miricl::unquote-splicing)))
                    (setq result (hir-recursive-form form variables functions targets)))
                   (t
                    (multiple-value-bind (expansion expanded)
                        (if (and (symbolp (car form)) (not (assoc (car form) functions :test #'equal)))
                            (macroexpand-1 form) (values form nil))
                      (cond
                        (expanded (lower expansion variables functions targets))
                        ((symbolp (car form))
                         (let ((inline (and (or *hir-inline-small-functions* *hir-inline-policy*)
                                         (hir-inline-candidate (car form) (cdr form) functions))))
                           (arguments (cdr form) nil (unless inline (hir-function-reference (car form) functions))
                             variables functions targets nil inline)))
                        (t
                         (defer (lambda ()
                                  (arguments (cdr form) nil (hir-single result) variables functions targets)))
                         (lower (car form) variables functions targets)))))))))))
      (lower form variables functions targets)
      (loop while work do
        (let* ((task (pop work)) (context (sixth task)) (facts (and (consp context) context))
               (*hir-origin* (third task))
               (*hir-debug-level* (fourth task)) (*hir-dynamic* (fifth task))
               (*hir-source-path* (seventh task))
               (*hir-types* (first facts)) (*hir-inline-policy* (second facts)) (*hir-inlining* (third facts)))
          ;; Reuse this invocation's dynamic policy binding. Creating a fresh
          ;; binding for every continuation adds allocation and lookup work.
          ;; Nested HIR-FORM invocations still restore their caller's policy.
          (setq *hir-safety-level* (if facts (fourth facts) context))
          (unless (catch 'hir-path-ended (funcall (first task)) t)
            (loop while (and work (not (second (car work)))) do (pop work))
            (unless work (throw 'hir-path-ended nil)))))
      result)))

(defun hir-recursive-form (form variables functions targets)
  (let ((*hir-origin* (if (and *hir-debug-sites* (gethash *hir-origin* *hir-debug-sites*))
                          *hir-origin* form)))
    (when (>= *hir-debug-level* 2)
      (setf (hir-function-debug-level *hir-function*)
            (max *hir-debug-level* (hir-function-debug-level *hir-function*))))
    (cond
      ((atom form) (hir-form form variables functions targets))
      (t
       (case (car form)
         (quote (unless (= (length form) 2) (error "Malformed QUOTE.")) (hir-constant (second form)))
         (load-time-value
          (unless (and (<= 2 (length form) 3) (member (third form) '(nil t)))
            (error 'program-error))
          ;; COMPILE evaluates this form in the null lexical environment and
          ;; retains only its primary value. Bundle emission can also retain
          ;; factories introduced by macros during HIR lowering.
          (let ((value (eval (second form))))
            (when (and *hir-load-time-recipes*
                       (not (or (numberp value) (characterp value)
                                (and (symbolp value) (symbol-package value)))))
              (setf (gethash value *hir-load-time-recipes*) (second form)))
            (hir-constant value)))
         ;; HIR-FORM owns these common forms. Normal lowering handles them
         ;; before entering this fallback; retain one implementation here too.
         ((if progn the let let* values funcall apply locally)
          (hir-form form variables functions targets))
         ((catch handler-bind handler-case unwind-protect restart-bind restart-case with-condition-restarts progv)
          (hir-control-form form variables functions targets))
         (miricl::%%condition-form
          (unless (member (second form) '(catch throw handler-bind handler-case unwind-protect
                                          restart-bind restart-case with-condition-restarts with-simple-restart
                                          block return-from return tagbody go ignore-errors assert check-type define-condition))
            (error "Native condition lowering is not implemented for ~S." (second form)))
          (hir-form (cdr form) variables functions targets))
         (define-condition (hir-define-condition form variables functions targets))
         (miricl::%%native-restart-signal (hir-restart-signal-form form variables functions targets))
         (with-simple-restart (hir-simple-restart-form form variables functions targets))
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
         ((case ecase typecase etypecase)
          (if (assoc (car form) functions)
              (hir-call-arguments (hir-function-reference (car form) functions) (cdr form) variables functions targets)
              (hir-case-form form variables functions targets)))
         (miricl::%%case-form
          (unless (and (integerp (second form)) (<= 0 (second form) 5)) (error 'program-error))
          (let ((expanded (cons (nth (second form) '(case typecase ecase etypecase ccase ctypecase)) (cddr form))))
            (hir-form (if (< (second form) 4) expanded
                          (hir-correctable-place-form expanded variables functions)) variables functions targets)))
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
         (miricl::%%symbol-iterate (hir-symbol-iterate-form form variables functions targets))
         (miricl::%%while (hir-while-form form variables functions targets))
         (tagbody (hir-tagbody-form form variables functions targets))
         (go
          (unless (= (length form) 2) (error "Malformed GO."))
          (let ((target (cdr (assoc (list :tag (second form)) targets :test #'equal))))
            (unless target (error "No lexical tag named ~S." (second form)))
            (if (eq (hir-target-owner target) *hir-function*)
                (hir-terminate :jump nil (list (hir-target-block target)))
                (hir-terminate :block-return
                  (list *hir-dynamic* (hir-capture target :dynamic) (hir-multiple (hir-constant nil))) nil))
            (throw 'hir-path-ended nil)))
         (setq (hir-setq-form form variables functions targets))
         (function
          (unless (= (length form) 2) (error "Malformed FUNCTION."))
          (if (and (consp (second form)) (eq (caadr form) 'lambda))
              (hir-close (second form) variables functions targets)
              (hir-function-reference (second form) functions)))
         (lambda (hir-close form variables functions targets))
         ((flet labels) (hir-local-functions-form form variables functions targets))
         (multiple-value-call (hir-multiple-value-call-form form variables functions targets))
         (miricl::%%native-value-list
          (hir-emit :values-list (list (hir-multiple (hir-form (second form) variables functions targets)))
            :single))
         (multiple-value-bind (hir-multiple-value-bind-form form variables functions targets))
         (miricl::%%check-apply-list
          (hir-emit :check-apply-list
            (list (hir-single (hir-form (second form) variables functions targets))) nil nil :check))
         (miricl::%%check-apply-arguments
          (hir-emit :check-apply-arguments
            (list (hir-single (hir-form (second form) variables functions targets)))
            nil (list (third form)) :check))
         (miricl::%%checked-argument
          (hir-emit :checked-argument
            (list (hir-single (hir-form (second form) variables functions targets)))
            :single (list (third form)) :read))
         (multiple-value-prog1
          (let ((value (hir-multiple (hir-form (second form) variables functions targets))))
            (hir-body (cddr form) variables functions targets) value))
         (block (hir-block-form form variables functions targets))
         (return-from (hir-return-from-form form variables functions targets))
         (otherwise (hir-application-form form variables functions targets)))))))

(defun hir-lambda (form parent variables functions targets name &optional implicit-block)
  (declare (type (or null simple-vector) parent)
           (type list variables functions targets implicit-block))
  (compiler-source-form form)
  (unless (and (consp form) (eq (first form) 'lambda))
    (compiler-input-fail "lambda expression" form "expected (LAMBDA lambda-list body...)"))
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
  (let* ((*hir-debug-level* (native-debug-level (cddr form) *hir-debug-level*))
         (*hir-safety-level* (native-safety-level (cddr form) *hir-safety-level*))
         (*hir-types* *hir-types*)
         (*hir-function* (make-hir-function-record name parent (second form)
                           nil nil nil nil nil nil 0 0 *hir-debug-level*))
         (*hir-origin* form) (*hir-block* (hir-new-block)) (*hir-inline-budget* 128)
         (*hir-dynamic* (hir-new-value :dynamic *hir-function*)))
    (setf (hir-function-entry *hir-function*) *hir-block*)
    (setf (hir-block-environment *hir-block*) *hir-dynamic*)
    (setf (hir-function-environment *hir-function*) *hir-dynamic*)
    (push *hir-function* (hir-unit-functions *hir-unit*))
    (multiple-value-bind (body specials types policy)
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
                     mode :after-rest)
               (when types (setq *hir-types* (hir-bound-type-environment parameter local types))))
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
                 (when types (setq *hir-types* (hir-bound-type-environment variable local types)))
                 (when supplied
                   (setq local (hir-bind-value supplied (hir-emit :truth (list present) :single) local specials))
                   (when types (setq *hir-types* (hir-bound-type-environment supplied local types)))
                   (push supplied names))
                 (unless (eq mode :aux) (incf index)))))))
        (when implicit-block (setq body (list (list* 'block (first implicit-block) body))))
        (setq local (hir-shadow-specials local specials))
        (let* ((*hir-types* (hir-type-environment local types))
               (*hir-inline-policy* (hir-inline-environment functions policy))
               (value (hir-body body local functions targets)))
          (unless (hir-block-terminator *hir-block*)
            (hir-terminate :return (list (hir-multiple value)) nil))))))
    (dolist (block (hir-function-blocks *hir-function*))
      (setf (hir-block-instructions block) (nreverse (hir-block-instructions block))))
    (setf (hir-function-blocks *hir-function*) (nreverse (hir-function-blocks *hir-function*))
          (hir-function-values *hir-function*) (nreverse (hir-function-values *hir-function*)))
    *hir-function*))

(defun hir-clear-source (unit)
  ;; Inlining needs source while lowering. Finished HIR owns semantic code;
  ;; retaining expansion environments, default forms and origin subtrees
  ;; would keep another executable AST alive behind that code.
  (dolist (function (hir-unit-functions unit))
    (setf (hir-function-parameters function)
      (mapcar (lambda (parameter) (if (consp parameter) (first parameter) parameter))
        (hir-function-parameters function)))
    (dolist (binding (hir-function-bindings function))
      (let ((local (hir-binding-local binding)))
        (when local
          (setf (hir-local-definition local) nil (hir-local-variables local) nil
                (hir-local-functions local) nil (hir-local-targets local) nil
                (hir-local-types local) nil (hir-local-policy local) nil))))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (let ((origin (hir-instruction-origin instruction)))
          (setf (hir-instruction-origin instruction)
            (cond ((and *hir-debug-sites* (gethash origin *hir-debug-sites*)) origin)
                  ((consp origin) (if (symbolp (car origin)) (car origin) 'funcall))
                  ((symbolp origin) origin) (t :literal)))))))
  unit)

(defun compile-to-hir (lambda-expression &key name global-specials)
  "Lower a lambda to object-level control/data flow without executing its body.
Unsupported special operators and parameter syntax signal an error."
  (declare (optimize (safety 3)))
  (compiler-source-form lambda-expression)
  (compiler-function-name name "function name")
  (compiler-list-length global-specials "global special variables")
  (dolist (variable global-specials) (compiler-variable-name variable "global special variable"))
  (ensure-compiler)
  (optimize-hir-checked (lower-source-to-hir lambda-expression name global-specials)))

(defun lower-source-to-hir (lambda-expression &optional name global-specials)
  ;; Internal lowering stage for validated source; public compilation always
  ;; follows it with HIR optimization. IR tests can inspect this boundary.
  (let ((*hir-unit* (make-hir-unit)) (*hir-specials* global-specials)
        (*hir-debug-level* (or (compiler-primitive 33 nil) 1)) (*hir-types* nil)
        (*hir-safety-level* (or (compiler-primitive 36 nil) 1))
        (*hir-inline-policy* nil) (*hir-inlining* nil)
        (*hir-source-path* (and (consp lambda-expression) (list lambda-expression)))
        (*hir-debug-sites* nil))
    (hir-lambda lambda-expression nil nil nil nil name)
    (setf (hir-unit-functions *hir-unit*) (nreverse (hir-unit-functions *hir-unit*)))
    (hir-clear-source *hir-unit*)
    (verify-hir *hir-unit*)
    *hir-unit*))

(defun compile-module-to-hir (definitions &key (entry-points nil entriesp) (inline t))
  "Lower a sealed group of DEFUN forms. Only ENTRY-POINTS escape the module.
The module factory returns an alist of names and functions, without installing
global definitions. Internal calls refer to this group's lexical definitions."
  (declare (optimize (safety 3)))
  (compiler-list-length definitions "module definitions")
  (when entriesp (compiler-list-length entry-points "module entry points"))
  (let ((names nil) (local nil) (*hir-inline-small-functions* inline))
    (dolist (definition definitions)
      (unless (>= (compiler-list-length definition "module definition") 3)
        (compiler-input-fail "module definition" definition "expected (DEFUN name lambda-list body...)"))
      (compiler-function-name (second definition) "module function name")
      (compiler-lambda-list (third definition)) (compiler-body (cdddr definition))
      (unless (and (eq (first definition) 'defun) (not (member (second definition) names :test #'equal)))
        (compiler-input-fail "module definition" definition "expected distinct DEFUN definitions"))
      (push (second definition) names) (push (cdr definition) local))
    (unless entriesp (setq entry-points (nreverse (copy-list names))))
    (dolist (name entry-points)
      (compiler-function-name name "module entry point")
      (unless (member name names :test #'equal) (error "Unknown module entry point ~S." name)))
    (compile-to-hir
      (list 'lambda nil (list 'labels (nreverse local)
        (cons 'list (mapcar (lambda (name) (list 'cons (list 'quote name) (list 'function name))) entry-points)))))))

(defparameter *hir-instruction-shapes*
  ;; Validation consults this fixed opcode grammar for every instruction.
  ;; Build its index once; graph-dependent validation tables remain fresh.
  (let ((table (make-hash-table :test 'eq)))
    (dolist (shape '((:constant 0 0 1 1) (:argument 0 0 1 1) (:rest-argument 0 0 1 1)
                      (:check-arguments 0 0 2 2) (:check-apply-arguments 1 1 1 1)
                      (:check-apply-list 1 1 0 0) (:checked-argument 1 1 1 1)
                      (:truth 1 1 0 0) (:function-reference 0 0 1 2)
                      (:block-enter 1 1 1 1) (:block-environment 1 1 1 1)
                      (:global-read 1 1 1 1) (:global-write 2 2 1 1) (:dynamic-bind 2 2 1 1)
                      (:environment-restore 1 1 0 0) (:values 0 nil 0 0) (:values-list 1 1 0 0)
                      (:list 0 nil 0 0) (:value-ref 1 1 1 1) (:cell-new 1 1 1 1)
                      (:cell-read 1 1 0 0) (:cell-write 2 2 0 0) (:primary 1 1 0 0) (:copy 1 1 0 0)
                      (:call 2 nil 0 0) (:control-call 4 nil 1 1) (:restart-signal 2 nil 1 1)
                      (:multiple-value-call 2 nil 0 0) (:capture 0 0 2 2) (:closure 0 nil 1 1)
                      (:branch 1 1 2 2) (:jump 0 1 1 1) (:return 1 1 0 0)
                      (:block-return 2 3 0 1) (:throw 3 3 0 0) (:unreachable 0 0 0 0)
                      (:debug-step 0 0 1 1) (:debug-resume 0 0 1 1)))
      (setf (gethash (first shape) table) shape))
    table))

(defun validate-hir-shapes (unit)
  (declare (optimize (safety 3)))
  (unless (hir-unit-p unit) (compiler-input-fail "HIR unit" unit "expected an HIR-UNIT"))
  (let* ((functions (hir-unit-functions unit))
         (count (compiler-list-length functions "HIR function list"))
         (owners (make-hash-table :test 'eq)) (producers (make-hash-table :test 'eq))
         (value-owners (make-hash-table :test 'eq)) (block-owners (make-hash-table :test 'eq))
         (types-checked (make-hash-table :test 'eq)) (debug-sites (make-hash-table :test 'eq))
         (shapes *hir-instruction-shapes*))
    (declare (type hash-table shapes))
    (dolist (function functions)
      (unless (%hir-function-p function)
        (compiler-input-fail "HIR function" function "invalid function record"))
      (when (gethash function owners)
        (compiler-input-fail "HIR function list" function "duplicate function"))
      (setf (gethash function owners) t (gethash function producers) function))
    (dolist (function functions)
      (let ((values (compiler-list-length (hir-function-values function) "HIR values"))
            (blocks (compiler-list-length (hir-function-blocks function) "HIR blocks")))
        (unless (and (typep (hir-function-value-count function) '(unsigned-byte 32))
                     (= (hir-function-value-count function) values)
                     (typep (hir-function-block-count function) '(unsigned-byte 32))
                     (= (hir-function-block-count function) blocks)
                     (typep (hir-function-debug-level function) '(integer 0 3)))
          (compiler-input-fail "HIR function" function "invalid value/block counts or debug level")))
      (dolist (item (hir-function-values function))
        (unless (%hir-value-p item) (compiler-input-fail "HIR value" item "invalid value record"))
        (when (gethash item value-owners) (compiler-input-fail "HIR value" item "duplicate value"))
        (unless (and (eq (hir-value-owner item) function)
                     (typep (hir-value-id item) '(unsigned-byte 32))
                     (member (hir-value-kind item) '(:single :multiple :dynamic)))
          (compiler-input-fail "HIR value" item "invalid value record, owner, ID or kind"))
        (setf (gethash item value-owners) function))
      (dolist (block (hir-function-blocks function))
        (unless (%hir-block-p block) (compiler-input-fail "HIR block" block "invalid block record"))
        (when (gethash block block-owners) (compiler-input-fail "HIR block" block "duplicate block"))
        (setf (gethash block block-owners) function)))
    (labels
        ((value (object)
           ;; Registration checked each canonical record once. Repeated
           ;; operands need only prove membership in this validation's table;
           ;; no certificate survives the call or a later client mutation.
           (unless (gethash object value-owners)
             (compiler-input-fail "HIR value" object "invalid value record, owner, ID or kind")))
         (binding (object)
           (unless (and (%hir-binding-p object) (gethash (hir-binding-owner object) owners))
             (compiler-input-fail "HIR binding" object "invalid binding record or owner"))
           (when (hir-binding-cell object) (value (hir-binding-cell object)))
           (when (and (hir-binding-cell object)
                      (not (eq (hir-value-owner (hir-binding-cell object)) (hir-binding-owner object))))
             (compiler-input-fail "HIR binding" object "cell belongs to another function"))
           (unless (or (hir-binding-special-p object) (hir-binding-cell object))
             (compiler-input-fail "HIR binding" object "lexical binding has no cell"))
           (when (hir-binding-local object)
             (unless (%hir-local-p (hir-binding-local object))
               (compiler-input-fail "HIR local function" (hir-binding-local object) "invalid local-function record"))
             (let ((function (hir-local-function (hir-binding-local object))))
               (when (and function (not (gethash function owners)))
                 (compiler-input-fail "HIR local function" function "function does not belong to this unit")))))
         (target (object)
           (unless (and (%hir-target-p object) (gethash (hir-target-owner object) owners)
                        (eq (gethash (hir-target-block object) block-owners) (hir-target-owner object)))
             (compiler-input-fail "HIR target" object "invalid target record, owner or block"))
           (value (hir-target-token object))
           (unless (eq (hir-value-owner (hir-target-token object)) (hir-target-owner object))
             (compiler-input-fail "HIR target" object "token belongs to another function")))
         (capture-identity (object)
           (cond ((%hir-binding-p object) (binding object))
                 ((%hir-target-p object) (target object))
                 (t (compiler-input-fail "HIR capture" object "expected a binding or control target"))))
         (debug-site (site)
           (unless (and (%hir-debug-site-p site) (gethash (hir-debug-site-owner site) owners)
                        (typep (hir-debug-site-kind site) '(integer 0 2))
                        (symbolp (hir-debug-site-operator site)))
             (compiler-input-fail "HIR debug site" site "invalid site record or owner"))
           (unless (gethash site debug-sites)
             (setf (gethash site debug-sites) t)
             (when (hir-debug-site-value site)
               (value (hir-debug-site-value site))
               (unless (eq (hir-value-owner (hir-debug-site-value site)) (hir-debug-site-owner site))
                 (compiler-input-fail "HIR debug site" site "value belongs to another function")))
             (dolist (environment (list (hir-debug-site-variables site) (hir-debug-site-functions site)
                                      (hir-debug-site-targets site) (hir-debug-site-types site)))
               (compiler-list-length environment "HIR debug environment")
               (dolist (entry environment)
                 (unless (consp entry) (compiler-input-fail "HIR debug environment" entry "expected an environment entry"))))
             (dolist (entry (hir-debug-site-variables site))
               (compiler-variable-name (car entry) "HIR debug variable") (binding (cdr entry)))
             (dolist (entry (hir-debug-site-functions site))
               (compiler-function-name (car entry) "HIR debug function") (binding (cdr entry)))
             (dolist (entry (hir-debug-site-targets site)) (target (cdr entry)))
             (dolist (entry (hir-debug-site-types site))
               (unless (symbolp (car entry)) (binding (car entry)))
               (compiler-type-specifier (cdr entry) types-checked))))
         (instruction (object)
           (unless (%hir-instruction-p object)
             (compiler-input-fail "HIR instruction" object "invalid instruction record"))
           (let* ((opcode (hir-instruction-opcode object))
                  (inputs (hir-instruction-inputs object)) (attributes (hir-instruction-attributes object))
                  (n (compiler-list-length inputs "HIR instruction operands"))
                  (a (compiler-list-length attributes "HIR instruction attributes"))
                  (shape (gethash opcode shapes)))
             (unless (and shape (>= n (second shape)) (or (null (third shape)) (<= n (third shape)))
                          (<= (fourth shape) a (fifth shape))
                          (typep (hir-instruction-safety-level object) '(integer 0 3)))
               (compiler-input-fail "HIR instruction" (list opcode (hir-instruction-origin object))
                 "unknown opcode or invalid operand/attribute count or safety level"))
             (dolist (input inputs) (value input))
             (when (hir-instruction-output object) (value (hir-instruction-output object)))
             (when (eq opcode :copy)
               (unless (and (hir-instruction-output object)
                            (eq (hir-value-kind (first inputs)) :single)
                            (eq (hir-value-kind (hir-instruction-output object)) :single))
                 (compiler-input-fail "HIR copy" object "copy requires single-value operands")))
             (case opcode
               ((:argument :rest-argument :check-apply-arguments :checked-argument :value-ref :capture)
                (unless (typep (first attributes) '(unsigned-byte 32))
                  (compiler-input-fail "HIR index" attributes "expected a nonnegative 32-bit index")))
               (:check-arguments
                (unless (and (typep (first attributes) '(unsigned-byte 32))
                             (integerp (second attributes))
                             (or (= (second attributes) -1) (<= (first attributes) (second attributes) #x7fffffff)))
                  (compiler-input-fail "HIR arity" attributes "invalid argument count limits")))
               (:function-reference (compiler-function-name (first attributes) "HIR function name"))
               ((:global-read :global-write :dynamic-bind)
                (unless (symbolp (first attributes))
                  (compiler-input-fail "HIR variable" attributes "expected a symbol")))
               (:cell-new (binding (first attributes)))
               ((:block-enter :block-environment) (target (first attributes)))
               (:closure
                (unless (gethash (first attributes) owners)
                  (compiler-input-fail "HIR closure" attributes "function does not belong to this unit")))
               (:debug-step (debug-site (first attributes)))
               (:debug-resume (when (first attributes) (debug-site (first attributes))))
               ((:control-call :restart-signal)
                (unless (typep (first attributes) '(unsigned-byte 32))
                  (compiler-input-fail "HIR control operation" attributes "expected a nonnegative operation code"))))
             (when (eq opcode :capture) (capture-identity (second attributes)))
             (when (member opcode '(:branch :jump :block-return))
               (dolist (block attributes)
                 (unless (%hir-block-p block)
                   (compiler-input-fail "HIR successor" block "invalid block record")))))))
      (dolist (function functions)
        (compiler-list-length (hir-function-bindings function) "HIR bindings")
        (compiler-list-length (hir-function-captures function) "HIR captures")
        (compiler-lambda-list (hir-function-parameters function))
        (let ((parent function) (depth 0))
          (loop while parent do
            (unless (gethash parent owners)
              (compiler-input-fail "HIR parent" parent "parent does not belong to this unit"))
            (when (> (incf depth) count)
              (compiler-input-fail "HIR parent" function "circular function-parent chain"))
            (setq parent (hir-function-parent parent))))
        (dolist (item (hir-function-values function))
          (value item)
          (unless (eq (hir-value-type item) t)
            (compiler-type-specifier (hir-value-type item) types-checked))
          (unless (or (%hir-instruction-p (hir-value-producer item))
                      (%hir-block-p (hir-value-producer item))
                      (gethash (hir-value-producer item) owners))
            (compiler-input-fail "HIR producer" (hir-value-producer item) "invalid value producer")))
        (value (hir-function-environment function))
        (unless (eq (hir-value-owner (hir-function-environment function)) function)
          (compiler-input-fail "HIR environment" function "environment belongs to another function"))
        (dolist (item (hir-function-bindings function)) (binding item))
        (dolist (item (hir-function-captures function)) (capture-identity item))
        (unless (member (hir-function-entry function) (hir-function-blocks function) :test #'eq)
          (compiler-input-fail "HIR entry block" function "entry does not belong to this function"))
        (dolist (block (hir-function-blocks function))
          (unless (and (%hir-block-p block) (typep (hir-block-id block) '(unsigned-byte 32)))
            (compiler-input-fail "HIR block" block "invalid block record or ID"))
          (compiler-list-length (hir-block-parameters block) "HIR block parameters")
          (compiler-list-length (hir-block-instructions block) "HIR block instructions")
          (dolist (parameter (hir-block-parameters block))
            (value parameter)
            (unless (and (eq (hir-value-owner parameter) function) (eq (hir-value-producer parameter) block))
              (compiler-input-fail "HIR block parameter" parameter "parameter has the wrong owner or producer")))
          (when (hir-block-environment block)
            (value (hir-block-environment block))
            (unless (eq (hir-value-owner (hir-block-environment block)) function)
              (compiler-input-fail "HIR block environment" block "environment belongs to another function")))
          (setf (gethash block producers) function)
          (dolist (op (hir-block-instructions block))
            (instruction op) (setf (gethash op producers) function))
          (instruction (hir-block-terminator block))
          (setf (gethash (hir-block-terminator block) producers) function))))
    (maphash (lambda (site present)
               (declare (ignore present))
               (loop with owner = (hir-debug-site-owner site)
                     for parent = site then (hir-debug-site-parent parent)
                     for depth from 0 while parent do
                 (unless (and (gethash parent debug-sites) (eq (hir-debug-site-owner parent) owner)
                              (< depth (hash-table-count debug-sites)))
                   (compiler-input-fail "HIR debug ancestry" site "invalid or circular parent chain")))) debug-sites)
    (dolist (function functions)
      (dolist (item (hir-function-values function))
        (let ((producer (hir-value-producer item)))
          (unless (and (eq (gethash producer producers) (hir-value-owner item))
                       (or (not (%hir-instruction-p producer)) (eq (hir-instruction-output producer) item)))
            (compiler-input-fail "HIR producer" producer "producer is detached from the unit or does not produce this value"))))))
  t)

(defun verify-hir (unit)
  (declare (optimize (safety 3)))
  (validate-hir-shapes unit)
  (verify-hir-contents unit))

(defun verify-hir-contents (unit)
  ;; Only VERIFY-HIR calls this kernel, after validating every record shape,
  ;; list spine, ownership field and allocation count. Keep the semantic
  ;; checks below, including checking IDs before indexing the value vector.
  (declare (type hir-unit unit))
  (dolist (function (hir-unit-functions unit))
    (let ((blocks (hir-function-blocks function)) (index (make-hash-table :test 'eq)) (id 0)
          (values (make-array (hir-function-value-count function) :initial-element nil)) (count 0))
      (declare (type simple-vector function values) (type hash-table index)
               (type fixnum id count) (type list blocks))
      ;; Indexed analyses require unique dense IDs owned by this function.
      ;; Check the canonical value identity as well as its numeric index.
      (dolist (value (hir-function-values function))
        (unless (and (%hir-value-p value) (eq (hir-value-owner value) function)
                     (integerp (hir-value-id value)) (<= 0 (hir-value-id value))
                     (< (hir-value-id value) (length values))
                     (null (aref values (hir-value-id value))))
          (error "HIR value IDs must be unique, dense and owned by their function."))
        (setf (aref values (hir-value-id value)) value) (incf count))
      (unless (= count (length values)) (error "HIR value count does not match its values."))
      (dolist (block blocks)
        (unless (= (hir-block-id block) id)
          (compiler-input-fail "HIR block ID" block "block IDs must be dense and ordered"))
        (incf id) (setf (gethash block index) t (hir-block-predecessors block) nil))
      (dolist (block blocks)
        (unless (hir-block-terminator block) (error "Unterminated HIR block."))
        (dolist (instruction (append (hir-block-instructions block) (list (hir-block-terminator block))))
          (dolist (input (if (hir-instruction-output instruction)
                            (cons (hir-instruction-output instruction) (hir-instruction-inputs instruction))
                            (hir-instruction-inputs instruction)))
            (unless (and (%hir-value-p input) (eq (hir-value-owner input) function)
                         (integerp (hir-value-id input)) (<= 0 (hir-value-id input))
                         (< (hir-value-id input) (length values))
                         (eq input (aref values (hir-value-id input))))
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
                                  (every (lambda (target) (null (hir-block-parameters target))) successors)
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
