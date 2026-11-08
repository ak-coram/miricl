;;;; Portable whole-program IR and conservative definition reachability.
;;;; The first backend emits ordinary MIRICL FASL units. It never evaluates
;;;; application initializers or guesses dependencies from a running image.
(in-package :miricl-compiler)

(defstruct ir-node kind name source group roots references barriers effect-p details
  retained-p reason)
(defstruct ir-unit source records roots nodes bytes output-bytes)
(defstruct ir-program units nodes definitions)

(defun symbol-key (symbol)
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

(defun form-references (form &optional load-values)
  ;; Syntax is walked without evaluating it. Constants are scanned separately:
  ;; a quoted symbol may be a function/class designator, but a quoted list must
  ;; never be mistaken for an executable call (notably a quoted EVAL).
  (let ((references nil) (barriers nil) (work (list (list form :code nil)))
        (functions nil)
        (seen-code (make-hash-table :test 'eq))
        (seen-data (make-hash-table :test 'eq))
        (seen-functions (make-hash-table :test 'eq)))
    (labels ((reference (kind name)
               (when (and (symbolp name) (symbol-package name)
                          (not (eq (symbol-package name) (find-package "KEYWORD"))))
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
                          (dolist (binding (car args))
                            (when (consp binding) (queue (second binding) :code)))
                          (body (cdr args)))
                         ((flet labels)
                          ;; Closure construction does not execute its body.
                          ;; Visit local definitions only when called or taken
                          ;; as function values, including mutual recursion.
                          (let* ((outer functions)
                                 (bindings (mapcar (lambda (definition)
                                             (list (car definition) (cddr definition) nil)) (car args)))
                                 (inner (append bindings outer)))
                            (dolist (binding bindings)
                              (setf (third binding) (if (eq operator 'labels) inner outer)))
                            (setq functions inner) (body (cdr args))
                            (setq functions outer)))
                         (lambda (body (cdr args)))
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
                          (when (and (global-function-p operator)
                                     (member operator '(eval compile load compile-file read read-preserving-whitespace read-from-string)))
                            (pushnew (symbol-name operator) barriers :test #'equal))
                          (when (and (global-function-p operator)
                                     (member operator '(miricl::class-direct-subclasses miricl::class-direct-superclasses
                                       miricl::class-slots miricl::class-direct-slots miricl::class-precedence-list
                                       miricl::generic-function-methods miricl::specializer-direct-methods)))
                            (pushnew (symbol-name operator) barriers :test #'equal))
                          (when (and (global-function-p operator)
                                     (member operator '(typep subtypep make-instance coerce make-sequence concatenate map merge)))
                            (dolist (type (case operator
                                            ((typep coerce) (list (second args)))
                                            (subtypep (list (first args) (second args)))
                                            (otherwise (list (first args)))))
                              (unless (static-type-form-p type)
                                (pushnew (symbol-name operator) barriers :test #'equal))))
                          (when (and (global-function-p operator) (member operator '(make-array adjust-array)))
                            (loop for options on (if (eq operator 'make-array) (cdr args) (cddr args)) by #'cddr
                                  while (cdr options) do
                              (when (or (not (keywordp (car options)))
                                        (and (eq (car options) :element-type)
                                             (not (static-type-form-p (second options)))))
                                (pushnew (symbol-name operator) barriers :test #'equal))))
                          (when (and (global-function-p operator) (eq operator 'coerce)
                                     (equal (second args) '(quote function)))
                            (if (and (consp (first args)) (eq (caar args) 'quote)
                                     (symbolp (cadar args)))
                                (function-reference (cadar args) t)
                                (pushnew (list :indirect-call "COERCE-FUNCTION") barriers :test #'equal)))
                          (when (and (global-function-p operator) (member operator '(intern find-symbol)))
                            (pushnew (list :symbol-input (symbol-name operator)) barriers :test #'equal))
                          (when (and (global-function-p operator) (member operator '(funcall apply)))
                            (when (and (consp (car args)) (eq (caar args) 'quote)
                                       (symbolp (cadar args)))
                              (function-reference (cadar args) t))
                            (unless (and (consp (car args))
                                         (member (caar args) '(function quote lambda)))
                              (pushnew (list :indirect-call (symbol-name operator)) barriers :test #'equal)))
                          (when (and (global-function-p operator) (member operator '(symbol-function fdefinition macro-function find-class)))
                            (when (and (member operator '(symbol-function fdefinition))
                                       (consp (car args)) (eq (caar args) 'quote))
                              (function-reference (cadar args) t))
                            (unless (and (consp (car args)) (eq (caar args) 'quote))
                              (pushnew (symbol-name operator) barriers :test #'equal)))
                          (when (and (global-function-p operator) (eq operator 'fdefinition) (consp (car args))
                                     (eq (caar args) 'quote) (consp (cadar args))
                                     (eq (caadar args) 'setf))
                            (pushnew (list :function (function-key (cadar args))) references :test #'equal))
                          (when (and (global-function-p operator) (member operator '(fboundp boundp symbol-value)))
                            (unless (and (consp (car args)) (eq (caar args) 'quote))
                              (pushnew (symbol-name operator) barriers :test #'equal)))
                          (body args)))))))))))
      (values references barriers))))

(defun read-units (file)
  (let ((units nil))
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
            (let ((payload (read-unit-payload stream end :metadata t)))
              (push (make-ir-unit :roots (first payload) :records (coerce (second payload) 'vector)
                      :nodes (third payload) :bytes (- end start)) units))
            (file-position stream end)))))
    (nreverse units)))

(defun analyze-file (file)
  "Read whole-program IR without executing FILE. Units without analysis are opaque effects."
  (when (ensure-compiler) (compile-linker))
  (let ((units (read-units file)) (nodes nil) (definitions (make-hash-table :test 'equal)))
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
      (t (error "Unknown FASL record kind ~S." kind)))))

(defun coalesce-records (records live)
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
  ;; Put frequently referenced atoms and shared graphs in a small pool, then lay out list spines
  ;; before their CAR subgraphs. Adjacent CDRs can be represented implicitly in
  ;; the binary stream. This changes addresses, never sharing or literal identities.
  (let* ((count (length records)) (frequency (make-array count :initial-element 0))
         (cars (make-array count :initial-element 0)) (nil-id nil) (edges 0)
         (mapping (make-array count :initial-element nil)) (pool nil) (next 0))
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
  (let* ((records (ir-unit-records unit)) (count (length records))
         (live (make-array count :initial-element nil))
         (mapping nil)
         (kept (make-hash-table)) (roots nil) (work nil) (next 0))
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
  (write-indexed-unit stream roots records :definitions definitions))

(defun write-report (program stream before after &optional output-records)
  (format stream "MIRICL whole-program reachability (interpreted FASL backend)~%")
  (format stream "Input: ~D bytes; output: ~D bytes.~%" before after)
  (format stream "Retained ~D of ~D definition/initialization nodes.~%"
    (count-if #'ir-node-retained-p (ir-program-nodes program)) (length (ir-program-nodes program)))
  (when output-records
    (format stream "Graph: ~D -> ~D records.~%"
      (reduce #'+ (ir-program-units program)
        :key (lambda (unit) (length (ir-unit-records unit))) :initial-value 0)
      output-records))
  (dolist (unit (ir-program-units program))
    (format stream "UNIT ~D -> ~D bytes ~S~%" (ir-unit-bytes unit)
      (or (ir-unit-output-bytes unit) 0) (ir-unit-source unit)))
  (dolist (node (ir-program-nodes program))
    (format stream "~A ~S ~S ~S: ~S~%" (if (ir-node-retained-p node) "KEEP" "DROP")
      (ir-node-kind node) (ir-node-name node) (ir-node-source node) (ir-node-reason node))))

(defun write-program (program output &key report index-definitions)
  "Compact and write an already analyzed and marked program."
  (when (and report (equal (namestring (merge-pathnames output)) (namestring (merge-pathnames report))))
    (error "The retention report and application output must be different files."))
  (let* ((before (+ 8 (reduce #'+ (ir-program-units program) :key #'ir-unit-bytes :initial-value 0)))
         (after 0) (units nil))
    ;; Validate every graph before replacing an existing output, including when
    ;; optimizing in place. No malformed late unit can leave a partial file.
    (dolist (unit (ir-program-units program))
      (multiple-value-bind (roots records definitions) (compact-unit unit index-definitions)
        (when roots (push (list unit roots records definitions) units))))
    (setq units (nreverse units))
    (ensure-directories-exist output)
    (with-open-file (stream output :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte stream))
      (dolist (unit units)
        (let ((start (file-position stream)))
          (write-unit stream (second unit) (third unit) (fourth unit))
          (setf (ir-unit-output-bytes (first unit)) (- (file-position stream) start))))
      (setq after (file-position stream)))
    (when report
      (ensure-directories-exist report)
      (with-open-file (stream report :direction :output :if-exists :supersede)
        (write-report program stream before after
          (reduce #'+ units :key (lambda (unit) (length (third unit))) :initial-value 0))))
    (values (pathname output) program)))

(defun shake-file (input output &key entry-points keep report index-definitions (strip t))
  "Retain ENTRY-POINTS, KEEP and observable initialization, without running INPUT.
Unbounded reflection or units without analysis conservatively retain the whole input.
OUTPUT is a loadable FASL, not yet native application code.
With STRIP NIL, retain every definition and compact only the representation."
  (when (and strip (null entry-points)) (error "Whole-program optimization requires explicit entry points."))
  (write-program (mark-program (analyze-file input) entry-points keep strip) output :report report :index-definitions index-definitions))

(defun optimize-system (system &key output entry-points keep report index-definitions (strip t) (save t))
  "Build an ASDF system, then emit a conservatively shaken interpreted FASL."
  (when (and strip (null entry-points)) (error "Whole-program optimization requires explicit entry points."))
  (let* ((bundle (miricl-bundle:dump-system system :save nil))
         (target (or output bundle)))
    (shake-file bundle target :entry-points entry-points :keep keep :report report :index-definitions index-definitions :strip strip)
    (when save
      (miricl::%%filesystem 7 (miricl::%source-path target) nil)
      (when report (miricl::%%filesystem 7 (miricl::%source-path report) nil)))
    (pathname target)))
