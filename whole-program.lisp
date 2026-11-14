;;;; Portable whole-program IR and conservative definition reachability.
;;;; The first backend emits ordinary MIRICL FASL units. It never evaluates
;;;; application initializers or guesses dependencies from a running image.
(in-package :miricl-compiler)

(defstruct ir-node kind name source group roots references barriers effect-p details
  retained-p reason)
(defstruct ir-unit source records roots nodes bytes output-bytes record-count file payload-start payload-end)
(defstruct ir-program units nodes definitions)

(defun symbol-key (symbol)
  (declare (type symbol symbol))
  (list (symbol-name symbol)
        (and (symbol-package symbol) (package-name (symbol-package symbol)))))

(defun function-key (name)
  (if (consp name) (list :setf (symbol-key (second name))) (symbol-key name)))

(defun literal-initializer-p (form)
  (or (and (atom form) (or (not (symbolp form)) (member form '(nil t)) (keywordp form)))
      (and (consp form) (eq (car form) 'quote))))

(defun static-type-form-p (form)
  (or (literal-initializer-p form)
      (and (consp form)
           (or (eq (car form) 'class-of)
               (and (eq (car form) 'find-class) (literal-initializer-p (second form)))))))

(defun structure-info (form)
  ;; Generated names belong to the package at macroexpansion, which need not
  ;; be the home package of the structure name. Slot defaults execute in the
  ;; constructor, not while registering the structure.
  (let* ((head (second form)) (name (if (consp head) (car head) head))
         (text (symbol-name name)) (package (package-name *package*))
         (prefix (concatenate 'string text "-")) (include nil) (typed nil) (named nil) (readonly nil)
         (provided nil) (constructors nil) (constructor-option nil)
         (predicate (list (concatenate 'string text "-P") package))
         (copier (list (concatenate 'string "COPY-" text) package)) (slots nil))
    (dolist (option (and (consp head) (cdr head)))
      (when (symbolp option) (setq option (list option)))
      (case (car option)
        (:conc-name (setq prefix (and (second option) (string (second option)))))
        (:include
         (setq include (symbol-key (second option)))
         (dolist (slot (cddr option))
           (when (and (consp slot) (getf (cddr slot) :read-only))
             (push (symbol-name (car slot)) readonly))))
        (:type (setq typed t))
        (:named (setq named t))
        (:constructor
         (setq constructor-option t)
         (when (or (null (cdr option)) (second option))
           (push (if (cdr option) (function-key (second option))
                     (list (concatenate 'string "MAKE-" text) package)) constructors)))
        (:predicate (when (cdr option) (setq predicate (and (second option) (function-key (second option))))))
        (:copier (when (cdr option) (setq copier (and (second option) (function-key (second option))))))))
    (unless constructor-option (push (list (concatenate 'string "MAKE-" text) package) constructors))
    (when (and typed (not named)) (setq predicate nil))
    (setq provided (append constructors (and predicate (list predicate)) (and copier (list copier))))
    (dolist (slot (cddr form))
      (unless (stringp slot)
        (push (symbol-key (if (consp slot) (car slot) slot)) slots)
        (when (and (consp slot) (getf (cddr slot) :read-only))
          (push (symbol-name (car slot)) readonly))))
    (list :provided provided :slots (nreverse slots) :read-only readonly :include include :prefix prefix :package package)))

(defun class-info (form)
  (let ((provided nil) (effect nil))
    (dolist (option (cddddr form))
      (when (and (eq (car option) :metaclass) (not (eq (second option) 'standard-class)))
        (setq effect t)))
    (dolist (slot (fourth form))
      (when (consp slot)
        (when (and (eq (getf (cdr slot) :allocation) :class)
                   (member :initform (cdr slot))) (setq effect t))
        (loop for (option name) on (cdr slot) by #'cddr do
          (case option
            ((:reader :writer) (push (function-key name) provided))
            (:accessor (push (function-key name) provided)
                       (push (list :setf (symbol-key name)) provided))))))
    (list :provided provided :definition-effect effect)))

(defun method-info (form)
  (let ((tail (cddr form)) (specializers nil) (effect nil))
    (loop while (and tail (not (listp (car tail)))) do (pop tail))
    (dolist (parameter (car tail))
      (when (member parameter lambda-list-keywords) (return))
      (let ((specializer (if (consp parameter) (second parameter) t)))
        (if (symbolp specializer) (push (symbol-key specializer) specializers)
            (progn
              (push :eql specializers)
              (unless (literal-initializer-p (second specializer)) (setq effect t))))))
    (list :specializers (nreverse specializers) :definition-effect effect)))

(defun definition-info (form group)
  ;; Preserve the defining form before macroexpansion turns it into arbitrary
  ;; registration code. Every emitted root of that form belongs to one node.
  (when (consp form)
    (let* ((operator (car form))
           (kind (case operator
                   (defun :function) (defmacro :macro) (deftype :type)
                   (defmethod :method) (defgeneric :generic)
                   (defstruct :structure) ((defclass define-condition) :class)
                   ((defvar defparameter) :variable) (defconstant :constant))))
      (when kind
        (let* ((name (second form))
               (details (case kind (:structure (structure-info form))
                          (:class (and (eq operator 'defclass) (class-info form)))
                          (:method (method-info form)))))
          (when (and (eq kind :structure) (consp name)) (setq name (car name)))
          (list group kind (function-key name)
                ;; Removing a dead binding with a literal initializer has no
                ;; observable effect in the explicitly closed production world.
                ;; Registration of an unreachable ordinary class/structure
                ;; can be removed in the closed world. Preserve custom class
                ;; construction, shared slot initialization and EQL effects.
                (or (getf details :definition-effect)
                    (and (eq kind :class) (null details))
                    (and (member kind '(:variable :constant))
                         (not (literal-initializer-p (third form))))) details))))))

(defun namespace (kind)
  (case kind ((:function :method :generic) :function)
        ((:class :structure) :class) ((:variable :constant) :variable)
        (otherwise kind)))

(defun form-written-variables (form)
  ;; A coarse, bounded proof for lexical function values. Shadowed writes may
  ;; invalidate extra candidates; uncertainty must never erase a call barrier.
  (let ((written (make-hash-table :test 'eq)) (seen (make-hash-table :test 'eq))
        (work (list form)) (unknown nil) (fuel 65536))
    (loop while work do
      (when (minusp (decf fuel)) (setq unknown t) (return))
      (let ((value (pop work)))
        (when (and (consp value) (not (gethash value seen)))
          (setf (gethash value seen) t)
          (compiler-list-length value "reachability form")
          (case (car value)
            (quote nil)
            (declare
             (when (assoc 'special (cdr value)) (setq unknown t)))
            (setq
             (loop for pairs on (cdr value) by #'cddr do
               (setf (gethash (car pairs) written) t)
               (push (second pairs) work)))
            (otherwise
             (when (or (member (car value) '(setf psetf psetq rotatef shiftf incf decf push pop
                                             macrolet symbol-macrolet progv miricl::%%parallel-place-form))
                       (and (symbolp (car value)) (not (eq (car value) 'lambda))
                            (macro-function (car value))))
               (setq unknown t))
             ;; The checked spine is finite. SEEN bounds shared subtrees,
             ;; without interpreting quoted data as assignments.
             (dolist (child (cdr value)) (push child work)))))))
    (values written unknown)))

(defun form-references (form &optional load-values)
  ;; Syntax is walked without evaluating it. Constants are scanned separately:
  ;; a quoted symbol may be a function/class designator, but a quoted list must
  ;; never be mistaken for an executable call (notably a quoted EVAL).
  (multiple-value-bind (written unknown-values) (form-written-variables form)
  (let ((references nil) (barriers nil) (work (list (list form :code nil)))
        (functions nil)
        (seen-code (make-hash-table :test 'eq))
        (seen-data (make-hash-table :test 'eq))
        (seen-functions (make-hash-table :test 'eq)))
    (labels ((reference (kind name)
               (when (and (symbolp name) (symbol-package name) (not (keywordp name)))
                 (pushnew (list kind (symbol-key name)) references :test #'equal)))
             (queue (value mode) (push (list value mode functions) work))
             (global-function-p (name) (not (assoc name functions :test #'equal)))
             (function-reference (name &optional escaping)
               (when (and escaping (member name '(make-instance typep subtypep coerce make-sequence
                                          concatenate map merge make-array adjust-array)))
                 (pushnew (list :indirect-call "TYPE-DESIGNATOR") barriers :test #'equal))
               (let ((binding (assoc name functions :test #'equal)))
                 (if binding
                     (unless (gethash binding seen-functions)
                       (setf (gethash binding seen-functions) t)
                       (let ((outer functions))
                         (setq functions (third binding))
                         (body (second binding))
                         (setq functions outer)))
                     (if (symbolp name) (reference :function name)
                         (pushnew (list :function (function-key name)) references :test #'equal)))))
             (body (forms) (dolist (child forms) (queue child :code)))
             (literal (value) (queue value :data)))
      (loop while work do
        (let* ((item (pop work)) (value (car item)) (mode (second item)))
          (setq functions (third item))
          (cond
            ((symbolp value)
             (reference (if (eq mode :data) :literal :variable) value)
             (when (and load-values (gethash value load-values))
               (queue (car (gethash value load-values)) :code)))
            ((and (arrayp value) (not (stringp value)))
             (unless (gethash value seen-data)
               (setf (gethash value seen-data) t)
               (dotimes (i (array-total-size value)) (literal (row-major-aref value i)))))
            ((consp value)
             (let ((seen (if (eq mode :data) seen-data
                             (or (gethash functions seen-code)
                                 (setf (gethash functions seen-code) (make-hash-table :test 'eq))))))
               (unless (gethash value seen)
                 (setf (gethash value seen) t)
                 (if (eq mode :data)
                     (progn (literal (car value)) (literal (cdr value)))
                     (let ((operator (car value)) (args (cdr value)))
                       (case operator
                         (quote (literal (car args)))
                         (declare nil)
                         (if
                          (let* ((test (car args))
                                 (quoted (and (consp test) (eq (car test) 'quote)))
                                 (known (or quoted (and (atom test)
                                   (or (not (symbolp test)) (member test '(nil t)) (keywordp test))))))
                            (queue test :code)
                            (if known
                                (queue (if (if quoted (second test) test) (second args) (third args)) :code)
                                (body (cdr args)))))
                         (proclaim
                          ;; INLINE and NOTINLINE name compilation policies,
                          ;; not callable values. Keeping those names as literal
                          ;; roots makes a library's optimization declarations
                          ;; retain every optional implementation it contains.
                          (function-reference operator)
                          (let ((declaration
                                  (and (consp (car args)) (eq (caar args) 'quote)
                                       (cadar args))))
                            (unless (and (global-function-p operator) (null (cdr args))
                                         (consp declaration)
                                         (member (car declaration) '(inline notinline optimize declaration)))
                              (body args))))
                         ((block return-from) (body (cdr args)))
                         (the (literal (car args)) (body (cdr args)))
                         (miricl::%%case-form
                          (queue (second args) :code)
                          (dolist (clause (cddr args))
                            (literal (car clause)) (body (cdr clause))))
                         (miricl::%%condition-form
                          (let ((kind (car args)) (forms (cdr args)))
                            (case kind
                              ((handler-case restart-case)
                               (queue (car forms) :code)
                               (dolist (clause (cdr forms))
                                 (when (eq kind 'handler-case) (literal (car clause)))
                                 (let ((forms (cddr clause)))
                                   (when (eq kind 'restart-case)
                                     (loop while (member (car forms) '(:report :interactive :test)) do
                                       (pop forms)
                                       (let ((option (pop forms)))
                                         (if (symbolp option) (function-reference option t) (queue option :code)))))
                                   (body forms))))
                              ((handler-bind restart-bind)
                               (dolist (binding (car forms))
                                 (when (eq kind 'handler-bind) (literal (car binding)))
                                 (body (cdr binding)))
                               (body (cdr forms)))
                              (with-simple-restart (body (cdar forms)) (body (cdr forms)))
                              (otherwise (body forms)))))
                         (go nil)
                         (tagbody (dolist (child args) (when (consp child) (queue child :code))))
                         ((let let*)
                          (let ((outer functions) (inner functions))
                            (dolist (binding (car args))
                              (setq functions (if (eq operator 'let*) inner outer))
                              (let* ((name (if (consp binding) (car binding) binding))
                                     (init (and (consp binding) (second binding)))
                                     (lambda (if (and (consp init) (eq (car init) 'function)) (second init) init)))
                                (queue init :code)
                                (push (list (list :value name)
                                        (and (not unknown-values) (not (gethash name written))
                                             (not (compiler-primitive 18 name))
                                             (consp lambda) (eq (car lambda) 'lambda))) inner)))
                            (setq functions inner) (body (cdr args)) (setq functions outer)))
                         ((flet labels)
                          ;; Closure construction does not execute its body.
                          ;; Visit local definitions only when called or taken
                          ;; as function values, including mutual recursion.
                          (let* ((outer functions)
                                 (bindings (mapcar (lambda (definition)
                                             (list (car definition) (list (cons 'lambda (cdr definition))) nil)) (car args)))
                                 (inner (append bindings outer)))
                            (dolist (binding bindings)
                              (setf (third binding) (if (eq operator 'labels) inner outer)))
                            (setq functions inner) (body (cdr args))
                            (setq functions outer)))
                         (lambda
                          (let ((outer functions) (parameters (list (car args))))
                            ;; Mask value facts at parameter bindings, including
                            ;; optional/key/aux names; extra masks are harmless.
                            (loop while parameters do
                              (let ((parameter (pop parameters)))
                                (cond ((symbolp parameter) (push (list (list :value parameter) nil) functions))
                                      ((consp parameter) (push (car parameter) parameters) (push (cdr parameter) parameters)))))
                            (body (cdr args)) (setq functions outer)))
                         (function
                          (if (symbolp (car args))
                              (function-reference (car args) t)
                              (if (eq (caar args) 'setf)
                                  (function-reference (car args) t)
                                  (queue (car args) :code))))
                         (miricl::%ensure-method
                          ;; Installing a method needs the generic binding,
                          ;; not every sibling method's body. Actual generic
                          ;; calls in the method body still create call edges.
                          (if (and (consp (car args)) (eq (caar args) 'quote))
                              (progn
                                (pushnew (list :binding (function-key (cadar args))) references :test #'equal)
                                (body (cdr args)))
                              (body args)))
                         (otherwise
                          (if (symbolp operator)
                              (function-reference operator)
                              (queue operator :code))
                          ;; An unbounded reflective lookup can expose every
                          ;; supplied definition. Retain them, and explain why.
                          (when (global-function-p operator)
                            (when (member operator '(eval compile load compile-file read read-preserving-whitespace read-from-string))
                              (pushnew (symbol-name operator) barriers :test #'equal))
                            (when (member operator '(miricl::class-direct-subclasses miricl::class-direct-superclasses
                                     miricl::class-slots miricl::class-direct-slots miricl::class-precedence-list
                                     miricl::generic-function-methods miricl::specializer-direct-methods))
                              (pushnew (symbol-name operator) barriers :test #'equal))
                            (when (member operator '(typep subtypep make-instance coerce make-sequence concatenate map merge))
                              (dolist (type (case operator
                                              ((typep coerce) (list (second args)))
                                              (subtypep (list (first args) (second args)))
                                              (otherwise (list (first args)))))
                                (unless (static-type-form-p type)
                                  (pushnew (symbol-name operator) barriers :test #'equal))))
                            (when (member operator '(make-array adjust-array))
                              (loop for options on (if (eq operator 'make-array) (cdr args) (cddr args)) by #'cddr
                                    while (cdr options) do
                                (when (or (not (keywordp (car options)))
                                          (and (eq (car options) :element-type)
                                               (not (static-type-form-p (second options)))))
                                  (pushnew (symbol-name operator) barriers :test #'equal))))
                            (when (and (eq operator 'coerce) (equal (second args) '(quote function)))
                              (if (and (consp (first args)) (eq (caar args) 'quote)
                                       (symbolp (cadar args)))
                                  (function-reference (cadar args) t)
                                  (pushnew (list :indirect-call "COERCE-FUNCTION") barriers :test #'equal)))
                            (when (member operator '(intern find-symbol))
                              (pushnew (list :symbol-input (symbol-name operator)) barriers :test #'equal))
                            (when (member operator '(funcall apply))
                              (when (and (consp (car args)) (eq (caar args) 'quote)
                                         (symbolp (cadar args)))
                                (function-reference (cadar args) t))
                              (unless (or (and (consp (car args))
                                               (member (caar args) '(function quote lambda)))
                                          (and (symbolp (car args))
                                               (second (assoc (list :value (car args)) functions :test #'equal))))
                                (pushnew (list :indirect-call (symbol-name operator)) barriers :test #'equal)))
                            (when (member operator '(symbol-function fdefinition macro-function find-class))
                              (when (and (member operator '(symbol-function fdefinition))
                                         (consp (car args)) (eq (caar args) 'quote))
                                (function-reference (cadar args) t))
                              (unless (and (consp (car args)) (eq (caar args) 'quote))
                                (pushnew (symbol-name operator) barriers :test #'equal)))
                            (when (and (eq operator 'fdefinition) (consp (car args))
                                       (eq (caar args) 'quote) (consp (cadar args))
                                       (eq (caadar args) 'setf))
                              (pushnew (list :function (function-key (cadar args))) references :test #'equal))
                            (when (member operator '(fboundp boundp symbol-value))
                              (unless (and (consp (car args)) (eq (caar args) 'quote))
                                (pushnew (symbol-name operator) barriers :test #'equal))))
                          (body args)))))))))))
      (values references barriers)))))

(defun read-units (file &key (records t))
  (let ((file (truename file)) (units nil))
    (with-open-file (stream file :element-type '(unsigned-byte 8))
      (dolist (byte '(0 97 115 109 1 0 0 0))
        (unless (= byte (read-byte stream)) (error "Invalid FASL header.")))
      (loop while (< (file-position stream) (file-length stream)) do
        (let ((start (file-position stream)))
          (unless (zerop (read-byte stream)) (error "Expected a FASL custom section."))
          (let* ((size (read-uleb stream)) (end (+ (file-position stream) size)))
            (unless (and (> size 12) (<= end (file-length stream)) (= (read-uleb stream) 11))
              (error "Invalid FASL section length."))
            (loop for character across "miricl.fasl" do
              (unless (= (char-code character) (read-byte stream)) (error "Unexpected FASL section.")))
            (let ((payload-start (file-position stream)))
              (multiple-value-bind (payload count) (read-unit-payload stream end :metadata t :records records)
                (push (make-ir-unit :roots (first payload) :records (and records (coerce (second payload) 'vector))
                        :nodes (third payload) :bytes (- end start) :record-count count
                        :file (pathname file) :payload-start payload-start :payload-end end) units)))
            (file-position stream end)))))
    (nreverse units)))

(defun read-unit-records (unit)
  ;; Analysis needs roots and definition metadata, not the externalized object
  ;; graph. Decode a disk-backed unit only while its graph is being processed.
  (or (ir-unit-records unit)
      (with-open-file (stream (ir-unit-file unit) :element-type '(unsigned-byte 8))
        (file-position stream (ir-unit-payload-start unit))
        (multiple-value-bind (payload count) (read-unit-payload stream (ir-unit-payload-end unit))
          (unless (and (= count (ir-unit-record-count unit)) (equal (first payload) (ir-unit-roots unit)))
            (error "FASL unit changed during compilation."))
          (second payload)))))

(defun analyze-file (file &key (records t))
  "Read whole-program IR without executing FILE. Units without analysis are opaque effects."
  (when (ensure-compiler) (compile-linker))
  (let ((units (read-units file :records records)) (nodes nil) (definitions (make-hash-table :test 'equal)))
    (dolist (unit units)
      (let* ((metadata (ir-unit-nodes unit))
             (valid (and (eq (first metadata) :miricl-ir) (= (length metadata) 3)))
             (groups (make-hash-table)))
        (when (and metadata (not valid)) (error "Invalid compiler IR metadata."))
        (setf (ir-unit-source unit) (and valid (second metadata)))
        (if (not valid)
            (let ((node (make-ir-node :kind :opaque :effect-p t :roots (ir-unit-roots unit))))
              (setf (ir-unit-nodes unit) (list node)) (push node nodes))
            (progn
              (unless (equal (ir-unit-roots unit) (mapcar #'car (third metadata)))
                (error "Compiler IR roots do not match FASL roots."))
              (setf (ir-unit-nodes unit) nil)
              (dolist (entry (third metadata))
                (destructuring-bind (root descriptor references barriers) entry
                  (destructuring-bind (group kind name effect details) descriptor
                    (let ((node (gethash group groups)))
                      (unless node
                        (setq node (make-ir-node :kind kind :name name :group group :effect-p effect :details details
                                     :source (ir-unit-source unit)))
                        (setf (gethash group groups) node)
                        (push node nodes) (push node (ir-unit-nodes unit))
                        (when name (push node (gethash (list (namespace kind) name) definitions))))
                      (push root (ir-node-roots node))
                      (when effect (setf (ir-node-effect-p node) t))
                      (setf (ir-node-references node) (union references (ir-node-references node) :test #'equal)
                            (ir-node-barriers node) (union barriers (ir-node-barriers node) :test #'equal))))))
              (setf (ir-unit-nodes unit) (nreverse (ir-unit-nodes unit)))))))
    (let ((program (make-ir-program :units units :nodes (nreverse nodes) :definitions definitions)))
      (index-generated-bindings program)
      program)))

(defun index-generated-bindings (program)
  (declare (type ir-program program))
  ;; One defining form owns its generated constructors/accessors as well as its
  ;; type name. Include inherited DEFSTRUCT slots even when their parent lives
  ;; in another compilation unit. An external parent's unknown slots keep the
  ;; child conservative rather than silently losing an inherited accessor.
  (let ((definitions (ir-program-definitions program)) (observe-classes nil))
    (dolist (node (ir-program-nodes program))
      (when (and (eq (ir-node-kind node) :method)
                 (equal (second (ir-node-name node)) "COMMON-LISP")
                 (member (first (ir-node-name node))
                   '("INITIALIZE-INSTANCE" "REINITIALIZE-INSTANCE" "SHARED-INITIALIZE" "ALLOCATE-INSTANCE") :test #'equal)
                 (let ((specializer (first (getf (ir-node-details node) :specializers))))
                   (or (eq specializer :eql)
                       (and (consp specializer) (equal (second specializer) "COMMON-LISP")
                         (member (first specializer) '("T" "CLASS" "STANDARD-CLASS" "STRUCTURE-CLASS") :test #'equal)))))
        (setq observe-classes t)))
    (dolist (node (ir-program-nodes program))
      (when (and observe-classes (member (ir-node-kind node) '(:structure :class)))
        (setf (ir-node-effect-p node) t))
      (dolist (name (getf (ir-node-details node) :provided))
        (pushnew node (gethash (list :function name) definitions) :test #'eq))
      (when (eq (ir-node-kind node) :structure)
        (let ((slots nil) (readonly nil) (work (list node)) (seen (make-hash-table :test 'eq))
              (details (ir-node-details node)))
          (loop while work do
            (let ((ancestor (pop work)))
              (unless (gethash ancestor seen)
                (setf (gethash ancestor seen) t)
                (setq slots (union slots (getf (ir-node-details ancestor) :slots) :test #'equal))
                (setq readonly (union readonly (getf (ir-node-details ancestor) :read-only) :test #'equal))
                (let ((include (getf (ir-node-details ancestor) :include)))
                  (when include
                    (let ((parents (gethash (list :class include) definitions)))
                      (if (and parents (every (lambda (parent) (eq (ir-node-kind parent) :structure)) parents))
                          (setq work (append parents work))
                          (setf (ir-node-effect-p node) t))))))))
          (dolist (slot slots)
            (let ((name (if (getf details :prefix)
                            (list (concatenate 'string (getf details :prefix) (first slot)) (getf details :package)) slot)))
              (pushnew node (gethash (list :function name) definitions) :test #'eq)
              (unless (member (first slot) readonly :test #'equal)
                (pushnew node (gethash (list :function (list :setf name)) definitions) :test #'eq)))))))))

(defun entry-key (entry)
  (cond ((symbolp entry) (function-key entry))
        ((consp entry) (function-key entry))
        ((stringp entry)
         (let* ((colon (position #\: entry))
                (package (if colon (subseq entry 0 colon) "COMMON-LISP-USER"))
                (start (if colon (1+ colon) 0)))
           (when (and (< start (length entry)) (char= (char entry start) #\:)) (incf start))
           (when (or (zerop (length package)) (>= start (length entry))
                     (position #\: entry :start start))
             (error "Expected PACKAGE:FUNCTION, received ~S." entry))
           (list (string-upcase (subseq entry start)) (string-upcase package))))
        (t (error "Invalid entry point: ~S." entry))))

(defun mark-program (program entries keep &optional (strip t))
  (declare (type ir-program program) (type list entries keep))
  (unless strip
    (dolist (node (ir-program-nodes program))
      (setf (ir-node-retained-p node) t (ir-node-reason node) '(:no-strip)))
    (return-from mark-program program))
  (let ((work nil) (definitions (ir-program-definitions program))
        (symbol-input nil) (indirect-call nil) (all-retained nil) (open-types nil)
        (method-waiters (make-hash-table :test 'equal)))
    (labels ((method-ready-p (method)
               (every (lambda (specializer)
                 (or (atom specializer)
                     (let ((classes (gethash (list :class specializer) definitions)))
                       (or (null classes) (some #'ir-node-retained-p classes)))))
                 (getf (ir-node-details method) :specializers)))
             (retain (node reason)
               (unless (ir-node-retained-p node)
                 (setf (ir-node-retained-p node) t (ir-node-reason node) reason)
                 (push node work)
                 (when (member (ir-node-kind node) '(:class :structure))
                   (dolist (method (gethash (ir-node-name node) method-waiters))
                     (when (method-ready-p method) (retain method (list :runtime-method)))))))
             (reference (kind name reason)
               (dolist (node (gethash (list (if (eq kind :binding) :function kind) name) definitions))
                 (unless (and (eq kind :binding) (eq (ir-node-kind node) :method)) (retain node reason)))
               ;; Native parallel-place forms may retain their accessor syntax
               ;; until runtime. Keep a supplied setter with its accessor until
               ;; the expression IR models those places individually.
               (when (and (eq kind :function) (stringp (first name)))
                 (dolist (node (gethash (list :function (list :setf name)) definitions))
                   (retain node reason))))
             (public-root (entry reason)
               (let ((key (entry-key entry)) (found nil))
                 (dolist (kind '(:function :macro :type :class :variable))
                   (dolist (node (gethash (list kind key) definitions))
                     (setq found t) (retain node (list reason entry))))
                 (unless found (error "No supplied definition for entry point ~S." entry)))))
      (dolist (node (ir-program-nodes program))
        (when (or (ir-node-effect-p node) (member (ir-node-kind node) '(:effect :opaque))
                  ;; Keyword class names are legal. Ordinary keyword argument
                  ;; data is not indexed as an ABI reference, so retain these
                  ;; uncommon definitions until typed references distinguish it.
                  (and (member (ir-node-kind node) '(:class :structure))
                       (equal (second (ir-node-name node)) "KEYWORD"))
                  ;; Generated uninterned global names have no package ABI.
                  ;; Keep their definitions until the IR carries lexical symbol
                  ;; identities across roots instead of guessing from names.
                  (and (ir-node-name node) (stringp (first (ir-node-name node)))
                       (null (second (ir-node-name node)))))
          (retain node (list :initialization))))
      ;; Runtime-provided generic functions (printing, initialization, etc.)
      ;; can invoke application methods without a visible application call.
      ;; Their implementation is outside this IR. Methods requiring a supplied
      ;; class are needed only when that class becomes reachable; native class
      ;; and EQL specializers remain conservative. Watchers avoid rescanning all
      ;; methods for every newly retained node.
      (dolist (node (ir-program-nodes program))
        (when (and (eq (ir-node-kind node) :method)
                   (or (equal (second (ir-node-name node)) "COMMON-LISP")
                       (not (some (lambda (candidate) (eq (ir-node-kind candidate) :generic))
                              (gethash (list :function (ir-node-name node)) definitions)))))
          (dolist (specializer (getf (ir-node-details node) :specializers))
            (when (and (consp specializer) (gethash (list :class specializer) definitions))
              (pushnew node (gethash specializer method-waiters) :test #'eq)))
          (when (method-ready-p node) (retain node (list :runtime-method)))))
      (dolist (entry entries) (public-root entry :entry))
      (dolist (entry keep) (public-root entry :keep))
      (loop while work do
        (let ((node (pop work)))
          (dolist (ref (ir-node-references node))
            (destructuring-bind (kind name) ref
              (if (eq kind :literal)
                  (dolist (space '(:function :macro :type :class :variable))
                    (reference space name (list :reference (ir-node-name node) (ir-node-source node))))
                  (reference kind name (list :reference (ir-node-name node) (ir-node-source node))))))
          (let ((hard nil))
            (dolist (barrier (ir-node-barriers node))
              (cond ((stringp barrier) (push barrier hard))
                    ((member (car barrier) '(:symbol-input :indirect-call))
                     (if (eq (car barrier) :symbol-input) (setq symbol-input barrier) (setq indirect-call barrier))
                     (unless open-types
                       ;; An escaped type factory or unknown callback can use
                       ;; a class designator unavailable to this local walk.
                       ;; Retain the class world until interprocedural value
                       ;; flow can bound its inputs; do not guess from callers.
                       (setq open-types t)
                       (dolist (class (ir-program-nodes program))
                         (when (member (ir-node-kind class) '(:class :structure))
                           (retain class (list :unknown-type-flow barrier))))))))
            (when (and (not all-retained)
                       (or (eq (ir-node-kind node) :opaque) hard (and symbol-input indirect-call)))
              ;; One unbounded reflection barrier closes the whole world.
              ;; Repeating this scan for every later node was quadratic in
              ;; large libraries such as FSet, without retaining anything new.
              (dolist (other (ir-program-nodes program))
                (retain other (list :reflection (or hard (and symbol-input indirect-call
                                                        (list symbol-input indirect-call)))
                                    (ir-node-name node))))
              (setq all-retained t work nil))))))
    program))

(defun record-edges (record)
  (let ((kind (first record)))
    (cond
      ((member kind '(0 1 2 3 4 8)) nil)
      ((member kind '(5 9)) (list (second record) (third record)))
      ((= kind 6) (cons (third record) (fourth record)))
      ((member kind '(7 10)) (list (second record)))
      ((= kind 11) (list* (second record) (third record) (remove -1 (fourth record))))
      (t (error "Unknown FASL record kind ~S." kind)))))

(defun coalesce-records (records live)
  (declare (type vector records) (type simple-vector live))
  ;; COMPILE-FILE may coalesce similar constants within a compilation unit
  ;; (CLHS 3.2.4.4). Resolved code is immutable too. Hash-cons its graph in
  ;; postorder, using canonical child IDs rather than recursively hashing Lisp
  ;; objects. Do not merge fresh symbols, MAKE-LOAD-FORM or LOAD-TIME-VALUE
  ;; records. Cycles and their ancestors retain their original representatives.
  ;; In particular, two mutable LOAD-TIME-VALUE results remain distinct even
  ;; when their creation forms happen to be identical.
  (multiple-value-bind (mapping native) (compiler-primitive 25 records live)
    (when native (return-from coalesce-records mapping)))
  (let* ((count (length records)) (states (make-array count :initial-element 0))
         (mapping (make-array count)) (table (make-hash-table :test 'equal)))
    (declare (type (unsigned-byte 32) count) (type simple-vector states mapping)
             (type hash-table table))
    (dotimes (i count) (setf (aref mapping i) i))
    (dotimes (root count)
      (when (and (aref live root) (zerop (aref states root)))
        (let ((work (list root)))
          (loop while work do
            (let* ((id (car work)) (record (aref records id))
                   (kind (first record))
                   (left (and (= kind 5) (second record)))
                   (right (and (= kind 5) (third record))))
              (if (zerop (aref states id))
                  (progn
                    (setf (aref states id) 1)
                    ;; Reserve before descent, so circular constants terminate.
                    ;; Other record kinds keep their identity, so their edges
                    ;; need not be traversed to compare their parents. Every
                    ;; live cons is still visited by the outer loop.
                    (when left
                      (when (zerop (aref states left)) (push left work))
                      (when (and (/= left right) (zerop (aref states right))) (push right work))))
                  (let* ((cyclic (and left
                                      (or (member (aref states left) '(1 3))
                                          (member (aref states right) '(1 3)))))
                         (key (and (not cyclic)
                                   (cond
                                     ((member kind '(0 1 2 3)) record)
                                     ((and (= kind 4) (third record) (zerop (fourth record))) record)
                                     ((= kind 5)
                                      (list 5 (aref mapping (second record))
                                            (aref mapping (third record)) (fourth record)))))))
                    (when key
                      (multiple-value-bind (canonical present) (gethash key table)
                        (if present (setf (aref mapping id) canonical)
                            (setf (gethash key table) id))))
                    (setf (aref states id) (if cyclic 3 2))
                    (pop work))))))))
    mapping))

(defun pool-record-less-p (records left right)
  (declare (type vector records) (type (unsigned-byte 32) left right))
  (let* ((a (aref records left)) (b (aref records right)) (kind (first a)))
    (unless (= kind (first b)) (return-from pool-record-less-p (< kind (first b))))
    (case kind
      ((1 3 8) (if (equal (second a) (second b)) (< left right) (string< (second a) (second b))))
      (2 (< (second a) (second b)))
      (4
       (let ((ap (or (third a) "")) (bp (or (third b) "")))
         (cond ((not (equal ap bp)) (string< ap bp))
               ((not (equal (second a) (second b))) (string< (second a) (second b)))
               ((/= (fourth a) (fourth b)) (< (fourth a) (fourth b)))
               (t (< left right)))))
      (otherwise (< left right)))))

(defun layout-records (records live canonical roots)
  (declare (type vector records) (type simple-vector live canonical) (type list roots))
  ;; Put frequently referenced atoms and shared graphs in a small pool, then lay out list spines
  ;; before their CAR subgraphs. Adjacent CDRs can be represented implicitly in
  ;; the binary stream. This changes addresses, never sharing or literal identities.
  (let* ((count (length records)) (frequency (make-array count :initial-element 0))
         (cars (make-array count :initial-element 0)) (nil-id nil) (edges 0)
         (mapping (make-array count :initial-element nil)) (pool nil) (next 0))
    (declare (type (unsigned-byte 32) count next) (type (integer 0 *) edges)
             (type simple-vector frequency cars mapping) (type list pool)
             (type (or null (unsigned-byte 32)) nil-id))
    (dotimes (i count)
      (when (and (aref live i) (= i (aref canonical i)))
        (let ((record (aref records i)))
          (when (zerop (first record)) (setq nil-id i))
          (dolist (edge (record-edges record))
            (incf edges) (incf (aref frequency (aref canonical edge))))
          (when (= (first record) 5) (incf (aref cars (aref canonical (second record))))))))
    (when nil-id (setf (aref frequency nil-id) (1+ edges)))
    (dotimes (i count)
      (when (and (aref live i) (= i (aref canonical i)))
        (let* ((kind (first (aref records i))) (uses (aref frequency i)))
          (when (or (member kind '(0 1 2 3 4 8))
                    (and (= kind 5)
                         (or (and (= uses (aref cars i)) (> uses 7)) (> uses 11))))
            ;; Moving a shared list tail can split a run. Discount its extra
            ;; header/edge cost instead of pooling every twice-used cons.
            (when (= kind 5) (decf (aref frequency i) (if (> uses (aref cars i)) 11 7)))
            (push i pool)))))
    (setq pool (sort pool (lambda (left right)
      (let ((a (aref frequency left)) (b (aref frequency right)))
        (if (= a b) (< left right) (> a b))))))
    ;; Preserve frequency ranks between both raw and tagged reference-width
    ;; tiers, but group
    ;; similar names within each wider tier for prefix encoding. No reference
    ;; acquires a wider absolute ID merely to save symbol-name bytes.
    (let ((rest pool) (groups nil) (position 0))
      (declare (type list rest groups) (type (unsigned-byte 32) position))
      (dolist (limit (list 64 128 8192 16384 1048576 2097152 134217728 268435456 count))
        (when rest
          (let ((group nil))
            (loop while (and rest (< position limit)) do (push (pop rest) group) (incf position))
            (push (if (= limit 64) (nreverse group)
                      (sort group (lambda (left right) (pool-record-less-p records left right)))) groups))))
      (setq pool (apply #'nconc (nreverse groups))))
    (dolist (i pool) (setf (aref mapping i) next) (incf next))
    (let ((work (copy-list roots)))
      (dolist (i pool)
        (dolist (edge (record-edges (aref records i))) (push edge work)))
      (loop while work do
        (let ((id (aref canonical (pop work))))
          (unless (aref mapping id)
            (setf (aref mapping id) next) (incf next)
            ;; RECORD-EDGES lists CAR before CDR. The stack visits CDR first.
            (dolist (edge (record-edges (aref records id))) (push edge work))))))
    (dotimes (i count)
      (when (aref live i)
        (unless (aref mapping (aref canonical i)) (error "Unreachable canonical FASL record."))
        (setf (aref mapping i) (aref mapping (aref canonical i)))))
    (values mapping next)))

(defun compact-unit (unit &optional index-definitions)
  (declare (type ir-unit unit))
  (let* ((records (read-unit-records unit)) (count (length records))
         (live (make-array count :initial-element nil))
         (mapping nil)
         (kept (make-hash-table)) (roots nil) (work nil) (next 0))
    (declare (type vector records) (type simple-vector live)
             (type (or null simple-vector) mapping) (type hash-table kept)
             (type list roots work) (type (unsigned-byte 32) count next))
    (dolist (node (ir-unit-nodes unit))
      (when (ir-node-retained-p node)
        (dolist (root (ir-node-roots node)) (setf (gethash root kept) t))))
    (dolist (root (ir-unit-roots unit))
      (when (gethash root kept) (push root roots) (push root work)))
    (setq roots (nreverse roots))
    (loop while work do
      (let ((id (pop work)))
        (unless (and (integerp id) (<= 0 id) (< id count)) (error "Invalid FASL reference ~S." id))
        (unless (aref live id)
          (setf (aref live id) t)
          (dolist (edge (record-edges (aref records id))) (push edge work)))))
    (let ((canonical (coalesce-records records live)))
      (multiple-value-setq (mapping next) (layout-records records live canonical roots)))
    (let ((result (make-array next :initial-element nil)))
      (dotimes (i count)
        (when (and (aref live i) (null (aref result (aref mapping i))))
          (let* ((record (aref records i)) (kind (first record)))
            (setf (aref result (aref mapping i))
              (cond
                ((= kind 5) (list 5 (aref mapping (second record)) (aref mapping (third record)) (fourth record)))
                ((= kind 6) (list 6 (second record) (aref mapping (third record))
                         (mapcar (lambda (id) (aref mapping id)) (fourth record))))
                ((= kind 7) (list 7 (aref mapping (second record)) (third record)))
                ((= kind 9) (list 9 (aref mapping (second record)) (aref mapping (third record))))
                ((= kind 10) (list 10 (aref mapping (second record))))
                ((= kind 11) (list 11 (aref mapping (second record)) (aref mapping (third record))
                  (mapcar (lambda (id) (if (= id -1) -1 (aref mapping id))) (fourth record))))
                (t record))))))
      (let ((definitions nil))
        (when index-definitions
          (dolist (node (ir-unit-nodes unit))
            (when (ir-node-retained-p node)
              (push (list (ir-node-kind node) (ir-node-name node) (ir-node-effect-p node)
                      (mapcar (lambda (id) (aref mapping id)) (ir-node-roots node))) definitions))))
        (values (mapcar (lambda (id) (aref mapping id)) roots) result
          (and index-definitions (list (ir-unit-source unit) (nreverse definitions))))))))

(defun write-unit (stream roots records &optional definitions)
  (declare (type stream stream) (type list roots) (type vector records))
  (write-indexed-unit stream roots records :definitions definitions))

(defun write-report (program stream before after &optional output-records)
  (declare (type ir-program program) (type stream stream)
           (type (integer 0 *) before after))
  (format stream "MIRICL whole-program reachability (FASL graph)~%")
  (format stream "Input: ~D bytes; output: ~D bytes.~%" before after)
  (format stream "Retained ~D of ~D definition/initialization nodes.~%"
    (count-if #'ir-node-retained-p (ir-program-nodes program)) (length (ir-program-nodes program)))
  (when output-records
    (format stream "Graph: ~D -> ~D records.~%"
      (reduce #'+ (ir-program-units program)
        :key (lambda (unit) (or (ir-unit-record-count unit) (length (ir-unit-records unit)))) :initial-value 0)
      output-records))
  (dolist (unit (ir-program-units program))
    (format stream "UNIT ~D -> ~D bytes ~S~%" (ir-unit-bytes unit)
      (or (ir-unit-output-bytes unit) 0) (ir-unit-source unit)))
  (dolist (node (ir-program-nodes program))
    (format stream "~A ~S ~S ~S: ~S~%" (if (ir-node-retained-p node) "KEEP" "DROP")
      (ir-node-kind node) (ir-node-name node) (ir-node-source node) (ir-node-reason node))))

(defun fresh-program-path (output)
  (loop for path = (make-pathname :name (format nil ".~A-~A" (or (pathname-name (pathname output)) "fasl")
                                               (symbol-name (gensym "BUILD-")))
                                  :defaults (pathname output))
        unless (probe-file path) return path))

(defun call-with-program-output (output writer)
  ;; Spool validated units instead of retaining their decoded graphs. Publish
  ;; only after the final unit succeeds, including for an in-place build.
  (let* ((output (merge-pathnames output (truename "./")))
         (temporary (fresh-program-path output)) (backup nil) (complete nil))
    (ensure-directories-exist output)
    (unwind-protect
        (let ((result
                (with-open-file (stream temporary :direction :output :if-exists :error
                                        :element-type '(unsigned-byte 8))
                  (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte stream))
                  (funcall writer stream))))
          ;; RENAME-FILE need not overwrite an existing pathname. Keep a
          ;; backup until publication, then release the old byte storage.
          (when (probe-file output)
            (setq backup (fresh-program-path output))
            (rename-file output backup))
          (rename-file temporary output)
          (setq complete t)
          result)
      (unless complete
        (when (and backup (probe-file backup) (not (probe-file output)))
          (rename-file backup output)))
      (when (probe-file temporary) (delete-file temporary))
      (when (and complete backup (probe-file backup)) (delete-file backup)))))

(defun write-program (program output &key report index-definitions)
  "Compact and write an already analyzed and marked program."
  (when (and report (equal (namestring (merge-pathnames output)) (namestring (merge-pathnames report))))
    (error "The retention report and application output must be different files."))
  (let* ((before (+ 8 (reduce #'+ (ir-program-units program) :key #'ir-unit-bytes :initial-value 0)))
         (after 0) (output-records 0))
    (setq after
      (call-with-program-output output
        (lambda (stream)
          (dolist (unit (ir-program-units program))
            (multiple-value-bind (roots records definitions) (compact-unit unit index-definitions)
              (when roots
                (incf output-records (length records))
                (let ((start (file-position stream)))
                  (write-unit stream roots records definitions)
                  (setf (ir-unit-output-bytes unit) (- (file-position stream) start))))))
          (file-position stream))))
    (when report
      (ensure-directories-exist report)
      (with-open-file (stream report :direction :output :if-exists :supersede)
        (write-report program stream before after output-records)))
    (values (pathname output) program)))

(defun shake-file (input output &key entry-points keep report index-definitions (strip t))
  "Retain ENTRY-POINTS, KEEP and observable initialization, without running INPUT.
Unbounded reflection or units without analysis conservatively retain the whole input.
OUTPUT is a loadable FASL, not yet native application code.
With STRIP NIL, retain every definition and compact only the representation."
  (when (and strip (null entry-points)) (error "Whole-program optimization requires explicit entry points."))
  (write-program (mark-program (analyze-file input :records nil) entry-points keep strip)
    output :report report :index-definitions index-definitions))

(defun optimize-system (system &key output entry-points keep report index-definitions ((:native native)) (strip t) (save t))
  "Build and prune an ASDF system; NATIVE compiles its retained code to Wasm."
  (when (and strip (null entry-points)) (error "Whole-program optimization requires explicit entry points."))
  (let* ((bundle (miricl-bundle:dump-system system :save nil))
         (target (or output bundle)))
    (shake-file bundle target :entry-points entry-points :keep keep :report report :index-definitions index-definitions :strip strip)
    (when native
      (compile-native-bundle target target)
      (when report
        (with-open-file (stream report :direction :output :if-exists :append)
          (with-open-file (bundle target :element-type '(unsigned-byte 8))
            (format stream "Native WASM bundle: ~D bytes.~%" (file-length bundle))))))
    (when save
      (miricl::%%filesystem 7 (miricl::%source-path target) nil)
      (when report (miricl::%%filesystem 7 (miricl::%source-path report) nil)))
    (pathname target)))

(defun prepare-native-unit (unit count progress load)
  (declare (type ir-unit unit) (type (unsigned-byte 32) count))
  (let ((functions nil) (entries nil) (recipes nil) (initializer-count 0)
        (ordinary (make-hash-table :test 'eq)) (seen (make-hash-table :test 'eq))
        (*hir-load-time-recipes* (make-hash-table :test 'eq)))
    (declare (type list functions entries recipes) (type (unsigned-byte 32) initializer-count)
             (type (or null hash-table) ordinary seen *hir-load-time-recipes*))
    (let* ((miricl::%fasl-records (coerce (read-unit-records unit) 'vector))
           (miricl::%fasl-values (make-array (length miricl::%fasl-records)))
           (miricl::%fasl-states (make-array (length miricl::%fasl-records) :initial-element 0))
           (miricl::%fasl-readtable (copy-readtable nil)) (miricl::%fasl-pending nil))
      (declare (special miricl::%fasl-records miricl::%fasl-values miricl::%fasl-states
                        miricl::%fasl-readtable miricl::%fasl-pending))
      (dolist (root (ir-unit-roots unit))
        (let ((form (miricl::%fasl-object root)))
          (miricl::%fasl-finish-initializations)
          (incf count)
          (when progress (funcall progress (ir-unit-source unit) count))
          (let ((hir
                  (if (and (consp form) (eq (first form) 'funcall) (null (cddr form))
                           (consp (second form)) (eq (first (second form)) 'load-hir-code)
                           (null (cddr (second form)))
                           (consp (second (second form)))
                           (eq (first (second (second form))) 'quote)
                           (null (cddr (second (second form)))))
                      (hir-code-to-unit (second (second (second form))))
                      (compile-to-hir (list 'lambda nil form)))))
            (push (first (hir-unit-functions hir)) entries)
            (push (hir-unit-functions hir) functions)
            (when load
              ;; Execute native initializers in order, retaining TCO for deep
              ;; calls, using the same optimization pipeline as the shared output.
              (multiple-value-bind (bytes literals debug)
                  (emit-native-module hir)
                (declare (ignore debug))
                (multiple-value-bind (function native) (compiler-primitive 17 bytes literals nil)
                  (unless native (error "This host cannot execute native bundle initializers."))
                  (funcall function)))))))
      ;; A LOAD-TIME-VALUE can create objects that cannot be dumped,
      ;; including closures or instances containing functions. Retain its
      ;; creation recipe instead of imposing MAKE-LOAD-FORM on the result.
      ;; Ordinary literal records take precedence when an LTV aliases one.
      (progn
        (dotimes (index (length miricl::%fasl-records))
          (when (and (= (aref miricl::%fasl-states index) 4)
                     (/= (car (aref miricl::%fasl-records index)) 10))
            (setf (gethash (aref miricl::%fasl-values index) ordinary) t)))
        (dotimes (index (length miricl::%fasl-records))
          (let* ((record (aref miricl::%fasl-records index))
                 (value (aref miricl::%fasl-values index)))
            (when (and (= (car record) 10) (= (aref miricl::%fasl-states index) 4)
                       (not (gethash value ordinary)) (not (gethash value seen))
                       ;; Immediate values and interned symbols can also
                       ;; occur in newly emitted module metadata.
                       (not (or (numberp value) (characterp value)
                                (and (symbolp value) (symbol-package value)))))
              (setf (gethash value seen) t)
              (push (cons value (miricl::%fasl-object (cadr record))) recipes))))))
    (setq initializer-count (length entries))
    (let ((pending (nreverse recipes)) (compiled nil))
      (loop
        (maphash (lambda (value form)
                   (unless (or (gethash value seen) (gethash value ordinary))
                     (setf (gethash value seen) t)
                     (push (cons value form) pending))) *hir-load-time-recipes*)
        (unless pending (return))
        (let* ((recipe (pop pending)) (hir (compile-to-hir (list 'lambda nil (cdr recipe)))))
          (push recipe compiled)
          (push (first (hir-unit-functions hir)) entries)
          (push (hir-unit-functions hir) functions)))
      (setq recipes (nreverse compiled)))
    ;; These identity sets cover the decoded input graph. They are no
    ;; longer needed once every creation recipe has been compiled.
    (clrhash ordinary)
    (clrhash seen)
    (clrhash *hir-load-time-recipes*)
    (setq ordinary nil seen nil *hir-load-time-recipes* nil)
    (setq entries (nreverse entries))
    (if entries
        (multiple-value-bind (code literals debug)
            (emit-native-module (make-hir-unit :functions (apply #'append (nreverse functions)))
                                :entries entries)
          (values code literals debug (mapcar #'car recipes) initializer-count count))
        (values nil nil nil nil initializer-count count))))

(defun native-unit-records (unit count progress load)
  (declare (type ir-unit unit) (type (unsigned-byte 32) count))
  ;; Return from the HIR's owning frame before building the output graph.
  ;; Its blocks, values and source forms can then be collected during dumping.
  (multiple-value-bind (code literals debug recipe-values initializer-count next-count)
      (prepare-native-unit unit count progress load)
    (let ((miricl::%fasl-records (make-array 32 :adjustable t :fill-pointer 0))
          (miricl::%fasl-seen (make-hash-table :test 'eq))
          (miricl::%file-load-values (make-hash-table :test 'eq))
          ;; Native output has no dependency IR; avoid analyzing references
          ;; that would be discarded while externalizing large literal graphs.
          (miricl::%file-collect-object-references nil) (roots nil))
      (declare (special miricl::%fasl-records miricl::%fasl-seen miricl::%file-load-values
                        miricl::%file-collect-object-references))
      (when code
        ;; Share one module across initializers, but reconstruct each entry's
        ;; literals only after preceding initializers ran.
        (setq code (map 'string #'code-char code))
        (when debug (setf (svref debug 0) code))
        (flet ((entry-form (index literal-vector)
                 (list 'funcall
                   (let ((entry (list 'miricl::%%compile 17
                                  (list 'quote (cons code index))
                                  (list 'quote literal-vector) nil)))
                     (if debug
                         (list 'miricl::%%compile 34 entry (list 'quote debug)
                           (svref (svref debug 2) index))
                         entry)))))
          ;; Creation recipes are native entries in the same module, invoked
          ;; by the loader when their opaque result is first needed.
          (loop for value in recipe-values
                for literal-vector in (nthcdr initializer-count literals)
                for index from initializer-count do
            (setf (gethash value miricl::%file-load-values)
                  (list (entry-form index literal-vector))))
          (loop for literal-vector in literals for index from 0 below initializer-count do
            (push (miricl::%fasl-dump (entry-form index literal-vector)) roots))))
      ;; Record indices now preserve sharing. Release the identity tables
      ;; before returning, including references cached in dynamic cells.
      (let ((roots (nreverse roots))
            (records (coerce miricl::%fasl-records 'simple-vector)))
        (setq miricl::%fasl-records nil miricl::%fasl-seen nil miricl::%file-load-values nil)
        (values roots records next-count)))))

(defun compile-native-bundle (input output &key progress load)
  "Compile a loaded application's FASL initializers and functions to Wasm.
Run after dependency pruning, in the build environment that loaded the system.
The result loads without invoking the Lisp compiler.
With LOAD true, construct the build environment by executing each initializer
as native code before decoding the next one, in the original load order."
  (let ((program (analyze-file input :records nil)) (count 0))
    (call-with-program-output output
      (lambda (stream)
        (dolist (unit (ir-program-units program))
          (multiple-value-bind (roots records next-count)
              (native-unit-records unit count progress load)
            (setq count next-count)
            ;; Dumping has returned: only encoded records remain live while
            ;; compaction pools their immutable data and orders the graph.
            (multiple-value-bind (compacted-roots compacted-records)
                (compact-unit (make-ir-unit :roots roots :records records
                                :nodes (list (make-ir-node :roots roots :retained-p t))))
              (write-unit stream compacted-roots compacted-records nil))))))
    (pathname output)))
