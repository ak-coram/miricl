;;;; Host tools only. Do not include this adapter in distributed guest images.
(in-package #:miricl-cross)

(defun require-target-macro-expansion (expander form environment)
  (declare (ignore expander environment))
  ;; A host expansion can silently call implementation-private functions,
  ;; even for SETF of a standard place such as SLOT-VALUE. Fail before emitting
  ;; that dependency. The initial interface accepts core forms, or an explicit
  ;; target expansion hook with the standard *MACROEXPAND-HOOK* convention.
  (miricl-compiler::compiler-input-fail "cross-compilation macro" form
    "a target macro expansion hook is required; host expansions are not portable"))

(defun require-target-load-time-evaluation (form)
  (miricl-compiler::compiler-input-fail "cross-compilation LOAD-TIME-VALUE" form
    "a target load-time evaluator is required; host evaluation is not portable"))

(defvar *bootstrap-loop-expander* nil)
(defvar *bootstrap-destructuring-expander* nil)
(defvar *bootstrap-macro-lambda-constructor* nil)

(defun bootstrap-source-constructor (source entry &optional adapters)
  "Host execution of shared target source with isolated private bindings.
ADAPTERS explicitly implements the target primitives used by the constructor."
      (let* ((package (make-package (symbol-name (gensym "MIRICL-TARGET-CONSTRUCTOR-")) :use '("COMMON-LISP")))
             (forward (make-hash-table :test 'eq)) (backward (make-hash-table :test 'eq)))
        (labels ((translate (object table &optional inbound)
                   (cond ((consp object) (cons (translate (car object) table inbound)
                                               (translate (cdr object) table inbound)))
                         ((and (symbolp object) inbound
                               (eq (symbol-package object) (find-package :miricl)))
                          (or (gethash object table)
                              (let ((private (intern (symbol-name object) package)))
                                (setf (gethash object table) private
                                      (gethash private backward) object)
                                private)))
                         ((symbolp object) (or (gethash object table) object))
                         (t object))))
          (let* ((*macroexpand-hook* #'funcall)
                 (while (translate 'miricl::%%while forward t)))
            (setf (macro-function while)
              (lambda (form environment)
                (declare (ignore environment))
                (let ((next (gensym)) (done (gensym)))
                  `(tagbody ,next (if ,(second form) nil (go ,done))
                     ,@(cddr form) (go ,next) ,done))))
            (dolist (adapter adapters)
              (setf (fdefinition (translate (car adapter) forward t)) (cdr adapter)))
            (dolist (form source) (eval (translate form forward t))))
          (let ((function (fdefinition (translate entry forward t))))
            (lambda (&rest arguments)
              (translate (apply function arguments) backward))))))

(defun bootstrap-loop-expander ()
  "Run the target LOOP constructor on the host, without capturing a guest heap."
  (or *bootstrap-loop-expander*
      (let ((source
              (with-open-file (stream (asdf:system-relative-pathname :miricl/cross "runtime-loop.lisp"))
                (let ((*package* (find-package :miricl)) (*read-eval* nil))
                  (loop for form = (read stream nil stream) until (eq form stream)
                        when (and (consp form) (eq (first form) 'defparameter)
                                  (eq (second form) 'miricl::*loop-library*))
                          return (second (third (third form))))))))
        (unless source (error "Missing target LOOP source."))
        (setf *bootstrap-loop-expander* (bootstrap-source-constructor source 'miricl::%expand-loop)))))

(defun prepare-bootstrap-source-adapters ()
  "Provide the target lambda-list constructor required by host HIR lowering."
  (unless (fboundp 'miricl::%normalize-lambda)
    (let* ((names '(miricl::%quote miricl::%keyword miricl::%normalize-lambda miricl::%normalize-lambda-1))
           (source
             (with-open-file (stream (asdf:system-relative-pathname :miricl/cross "clos-library.lisp"))
               (let ((*package* (find-package :miricl)) (*read-eval* nil))
                 (loop for form = (read stream nil stream) until (eq form stream)
                       when (and (consp form) (eq (first form) 'defun) (member (second form) names))
                         collect form)))))
      (unless (= (length source) (length names)) (error "Missing target lambda-list constructor source."))
      ;; Source validation already established the lambda/body spines. This
      ;; constructor only asks %%TAG whether a body element is a cons.
      (setf (fdefinition 'miricl::%normalize-lambda)
        (bootstrap-source-constructor source 'miricl::%normalize-lambda
          (list (cons 'miricl::%%tag (lambda (object) (if (consp object) 7 0)))
                (cons 'miricl::%member (lambda (item list) (member item list :test #'eq))))))))
  (unless *bootstrap-destructuring-expander*
    (let* ((names '(miricl::%function-body miricl::%destructure-lambda
                    miricl::%destructuring-form miricl::%macro-lambda))
           (source (with-open-file (stream (asdf:system-relative-pathname :miricl/cross "development-library.lisp"))
                     (let ((*package* (find-package :miricl)) (*read-eval* nil))
                       (loop for form = (read stream nil stream) until (eq form stream)
                         when (and (consp form) (eq (first form) 'defun) (member (second form) names))
                           collect form))))
           (adapters (list (cons 'miricl::%member (lambda (item list) (member item list :test #'eq))))))
      (unless (= (length source) (length names)) (error "Missing target destructuring constructor source."))
      (setf *bootstrap-destructuring-expander*
              (bootstrap-source-constructor source 'miricl::%destructuring-form adapters)
            *bootstrap-macro-lambda-constructor*
              (bootstrap-source-constructor source 'miricl::%macro-lambda adapters))))
  (values))

(defun bootstrap-place-expansion (place environment)
  "Return the target setter protocol for the bootstrap's portable places.
Do not use host GET-SETF-EXPANSION, which may expose host private functions."
  (cond
    ((symbolp place)
     (miricl-compiler::compiler-variable-name place "bootstrap SETF place")
     (let ((store (gensym "STORE")))
       (values nil nil (list store) `(setq ,place ,store) place)))
    ((and (consp place) (symbolp (car place))
          (get (car place) 'miricl::runtime-inline-macro)
          (macro-function (car place)))
     (bootstrap-place-expansion (funcall (macro-function (car place)) place environment) environment))
    ((and (consp place) (member (car place)
           '(car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
             first second third fourth fifth sixth seventh eighth ninth tenth rest
             svref aref row-major-aref elt char schar gethash get symbol-value symbol-plist
             symbol-function fdefinition fill-pointer)))
     (miricl-compiler::compiler-list-length place "bootstrap SETF place")
     (let ((temporaries (mapcar (lambda (form) (declare (ignore form)) (gensym "PLACE")) (cdr place)))
           (store (gensym "STORE")))
       (values temporaries (cdr place) (list store)
         `(funcall #',(list 'setf (car place)) ,store ,@temporaries)
         (cons (car place) temporaries))))
    (t (miricl-compiler::compiler-input-fail "bootstrap SETF place" place
         "no explicit target place adapter is registered"))))

(defun bootstrap-modify-place (place environment writer)
  (multiple-value-bind (temporaries forms stores write read) (bootstrap-place-expansion place environment)
    (funcall writer temporaries forms (car stores) write read)))

(defun bootstrap-macro-syntax (form)
  ;; Constructors also inspect binding patterns and clause lists, beyond the
  ;; executable-form boundary. Quoted objects remain opaque literal data.
  (let ((seen (make-hash-table :test 'eq)) (work (list (cons form nil))))
    (loop while work for job = (pop work) for object = (car job) when (consp object) do
      (cond ((cdr job) (setf (gethash object seen) :done))
            ((eq (gethash object seen) :active)
             (miricl-compiler::compiler-input-fail "bootstrap macro syntax" form "circular syntax"))
            ((not (gethash object seen))
             (setf (gethash object seen) :active)
             (push (cons object t) work)
             (unless (eq (car object) 'quote)
               (push (cons (cdr object) nil) work)
               (push (cons (car object) nil) work)))))))

(defun bootstrap-macroexpand-hook (expander form environment)
  "Explicit target macro adapter for building the shared frontend on the host.
Uses shared target constructors and core CL forms; never invokes a host CL macro."
  (let* ((operator (and (consp form) (car form)))
         (size (miricl-compiler::compiler-list-length form "bootstrap macro form"))
         (bounds (case operator
                   ((when unless dolist dotimes prog1) '(2))
                   ((do do* prog2) '(3))
                   ((incf decf) '(2 3))
                   (push '(3 3)) (pop '(2 2))
                   (return '(1 2)) (loop-finish '(1 1)))))
    (bootstrap-macro-syntax form)
    (unless (or (not bounds)
                (and (>= size (first bounds)) (or (not (second bounds)) (<= size (second bounds)))))
      (miricl-compiler::compiler-input-fail "bootstrap macro form" form "invalid argument count"))
    (case operator
      (destructuring-bind
       (miricl-compiler::compiler-list-length form "bootstrap DESTRUCTURING-BIND")
       (unless (cddr form) (miricl-compiler::compiler-input-fail "bootstrap DESTRUCTURING-BIND" form "missing value form"))
       (apply *bootstrap-destructuring-expander* (cdr form)))
      ((and or) (miricl-runtime:expand-logical-form form))
      ((when unless cond)
       (miricl-runtime::expand-conditional-form form (position operator '(when unless cond))))
      ((multiple-value-list nth-value) (miricl-runtime::expand-values-form form))
      ((dolist dotimes)
       (let* ((spec (second form))
              (size (miricl-compiler::compiler-list-length spec "bootstrap iteration binding")))
         (unless (<= 2 size 3)
           (miricl-compiler::compiler-input-fail "bootstrap iteration binding" spec
             "expected (variable source [result])"))
         (miricl-compiler::compiler-variable-name (first spec) "bootstrap iteration variable")
         (miricl-runtime::expand-iteration-form (first spec) (second spec) (third spec)
           (vector (cddr form) (if (eq operator 'dotimes) 1 0) 'miricl::%%while 'miricl::%%symbol-iterate))))
      ((do do*)
       (miricl-compiler::compiler-list-length (second form) "bootstrap DO bindings")
       (dolist (binding (second form))
         (if (symbolp binding)
             (miricl-compiler::compiler-variable-name binding "bootstrap DO variable")
             (progn
               (unless (<= 1 (miricl-compiler::compiler-list-length binding "bootstrap DO binding") 3)
                 (miricl-compiler::compiler-input-fail "bootstrap DO binding" binding
                   "expected a variable, optional initializer and optional step"))
               (miricl-compiler::compiler-variable-name (first binding) "bootstrap DO variable"))))
       (unless (plusp (miricl-compiler::compiler-list-length (third form) "bootstrap DO end clause"))
         (miricl-compiler::compiler-input-fail "bootstrap DO end clause" (third form) "missing end test"))
       (miricl-runtime:expand-do-form form (eq operator 'do*) 'miricl::%%while))
      (loop (funcall (bootstrap-loop-expander) (cdr form)))
      (loop-finish '(return-from miricl::%finish nil))
      (return (list 'return-from nil (second form)))
      (prog1 (let ((value (gensym))) `(let ((,value ,(second form))) ,@(cddr form) ,value)))
      (prog2 `(progn ,(second form) (prog1 ,@(cddr form))))
      (psetq
       (let ((bindings nil) (writes nil) (pairs (cdr form)))
         (unless (evenp (length pairs)) (error 'program-error))
         (loop for (name value) on pairs by #'cddr for temporary = (gensym) do
           (miricl-compiler::compiler-variable-name name "bootstrap PSETQ variable")
           (push (list temporary value) bindings) (push `(setq ,name ,temporary) writes))
         `(let ,(nreverse bindings) ,@(nreverse writes) nil)))
      (setf
       (unless (evenp (length (cdr form))) (error 'program-error))
       (if (null (cdr form)) nil
           (if (cdddr form)
               `(progn ,@(loop for (place value) on (cdr form) by #'cddr collect `(setf ,place ,value)))
               (bootstrap-modify-place (second form) environment
                 (lambda (temporaries forms store write read)
                   (declare (ignore read))
                   `(let* ,(mapcar #'list temporaries forms)
                      (let ((,store ,(third form))) ,write)))))))
      ((incf decf)
       (bootstrap-modify-place (second form) environment
         (lambda (temporaries forms store write read)
           (let ((delta (gensym)))
             `(let* ,(append (mapcar #'list temporaries forms)
                       (list (list delta (if (cddr form) (third form) 1))
                             (list store (list (if (eq operator 'incf) '+ '-) read delta))))
                ,write)))))
      (push
       (bootstrap-modify-place (third form) environment
         (lambda (temporaries forms store write read)
           (let ((value (gensym)))
             `(let* ,(append (list (list value (second form))) (mapcar #'list temporaries forms)
                       (list (list store (list 'cons value read)))) ,write)))))
      (pop
       (bootstrap-modify-place (second form) environment
         (lambda (temporaries forms store write read)
           (let ((old (gensym)))
             `(let* ,(append (mapcar #'list temporaries forms)
                       (list (list old read) (list store (list 'cdr old)))) ,write (car ,old))))))
      (otherwise
       ;; These definitions are shared project source, not implementation CL
       ;; expanders. Record macros explicitly mark their portable expansions.
       (if (or (get operator 'miricl::runtime-inline-macro)
               (member operator '(miricl-compiler:with-compilation-statistics miricl-compiler::compiler-statistic)))
           (funcall expander form environment)
           (require-target-macro-expansion expander form environment))))))

(defun compile-lambda (source &key name global-specials
                                 (macroexpand-hook #'require-target-macro-expansion)
                                 (load-time-evaluator #'require-target-load-time-evaluation))
  "Return target Wasm bytes, literals and debug metadata, without executing SOURCE.
SOURCE uses Common Lisp core forms. Macro expansion requires an explicit target
MACROEXPAND-HOOK; host implementation macros are rejected by default. Target
global special bindings are supplied through GLOBAL-SPECIALS, not host state.
LOAD-TIME-VALUE requires an explicit target LOAD-TIME-EVALUATOR.
No compiler is bootstrapped in Wasm, and no guest function bindings are changed."
  (when (eq macroexpand-hook #'bootstrap-macroexpand-hook) (prepare-bootstrap-source-adapters))
  (let ((miricl-compiler::*compiler-state* :interpreted)
        (miricl-compiler::*hir-load-time-evaluator*
          (or load-time-evaluator #'require-target-load-time-evaluation))
        (*macroexpand-hook* macroexpand-hook))
    (miricl-compiler::emit-native-module-checked
      (miricl-compiler:compile-to-hir source :name name :global-specials global-specials) nil)))

(defun compile-module (definitions &key (entry-points nil entriesp) (inline t) global-specials
                                       (macroexpand-hook #'require-target-macro-expansion)
                                       (load-time-evaluator #'require-target-load-time-evaluation))
  "Cross-compile a closed DEFUN group; return Wasm bytes, literals, debug and HIR.
Only ENTRY-POINTS escape; omitted ENTRY-POINTS exposes every supplied definition.
Macro expansion and load-time evaluation have the same explicit target contract
as COMPILE-LAMBDA. GLOBAL-SPECIALS supplies target global special declarations;
host proclamations are not target declarations."
  (when (eq macroexpand-hook #'bootstrap-macroexpand-hook) (prepare-bootstrap-source-adapters))
  (let ((miricl-compiler::*compiler-state* :interpreted)
        (miricl-compiler::*hir-load-time-evaluator*
          (or load-time-evaluator #'require-target-load-time-evaluation))
        (*macroexpand-hook* macroexpand-hook))
    (let ((unit (if entriesp
                    (miricl-compiler:compile-module-to-hir definitions
                      :entry-points entry-points :inline inline :global-specials global-specials)
                    (miricl-compiler:compile-module-to-hir definitions :inline inline
                      :global-specials global-specials))))
      (multiple-value-bind (bytes literals debug) (miricl-compiler::emit-native-module-checked unit nil)
        (values bytes literals debug unit)))))

(defvar *kernel-cache* (make-hash-table :test 'equal))
(defvar *kernel-backend* nil)
(defvar *kernel-module-workers* nil)
(defvar *kernel-worker-preserve-abi* nil)
(defvar *kernel-shared-workers* nil)
(defvar *kernel-share-helpers* t)

(defun kernel-fixed-imports ()
  ;; Reuse the checked, primary-value ABI of the actual fixed-call dispatcher.
  ;; MIRICL/CROSS alone has no runtime registry and therefore no such adapters.
  ;; Kernel callbacks currently support only one to three object operands.
  (when (fboundp 'miricl::native-fixed-primitive-entries)
    (loop for (primitive arity target) in (miricl::native-fixed-primitive-entries)
          for index from 0 when (member arity '(1 2 3)) collect
      (list (format nil "native-kernel-fixed-~D" index) arity target primitive))))

(defun kernel-fixed-callback-descriptor (descriptor)
  "Validate and copy the private adapter's zero-based formal/primitive contract."
  (let ((context "runtime kernel fixed callbacks") (seen nil) (copy nil)
        (registry (kernel-fixed-imports)))
    (when (> (miricl-compiler::compiler-list-length descriptor context) 4)
      (miricl-compiler::compiler-input-fail context descriptor "at most four fixed callbacks are supported"))
    (dolist (item descriptor (nreverse copy))
      (unless (= (miricl-compiler::compiler-list-length item context) 2)
        (miricl-compiler::compiler-input-fail context item "expected (formal-index registered-primitive)"))
      (let* ((index (first item)) (primitive (second item))
             (entry (and (symbolp primitive) (find primitive registry :key #'fourth :test #'eq))))
        (unless (and (typep index '(integer 0 3)) (not (member index seen)))
          (miricl-compiler::compiler-input-fail context item "formal index must be unique and between zero and three"))
        (unless (and entry (member (second entry) '(1 2 3)) (symbolp (third entry)))
          (miricl-compiler::compiler-input-fail context item "primitive has no registered checked fixed-call ABI"))
        (push index seen)
        (push (list index primitive) copy)))))

(defun kernel-fixed-callback-facts (unit selected descriptor)
  "Prove callback identity on this fresh, privately owned closed HIR only.
Intersect every internal ingress, including reentry into SELECTED, with the
private adapter's ingress. Exhausted work and unknown provenance prove nothing."
  (let* ((functions (miricl-compiler:hir-unit-functions unit))
         (arguments (make-hash-table :test 'eq)) (owners (make-hash-table :test 'eq))
         (uses (make-hash-table :test 'eq)) (calls (make-hash-table :test 'eq))
         (incoming (make-hash-table :test 'eq)) (reachable (make-hash-table :test 'eq))
         (unsafe (make-hash-table :test 'eq)) (edges (make-hash-table :test 'eq))
         (flow (miricl-compiler::hir-primary-flow unit (miricl-compiler::hir-optimization-barriers unit)))
         (masks (make-hash-table :test 'eq)) (facts (make-hash-table :test 'eq))
         (entries (mapcar (lambda (item) (find (second item) (kernel-fixed-imports) :key #'fourth :test #'eq)) descriptor))
         (budget 32768) (all (1- (ash 1 (length entries)))))
    (block proof
      (labels ((spend () (when (minusp (decf budget)) (return-from proof nil)))
               (origin (value)
                 (loop for producer = (miricl-compiler::hir-value-producer value) do
                   (spend)
                   (unless (and (miricl-compiler::%hir-instruction-p producer)
                                (eq (miricl-compiler::hir-instruction-opcode producer) :copy))
                     (return producer))
                   (setq value (first (miricl-compiler::hir-instruction-inputs producer)))))
               (callee (value)
                 (let ((producer (origin value)))
                   (and (miricl-compiler::%hir-instruction-p producer)
                        (eq (miricl-compiler::hir-instruction-opcode producer) :closure)
                        (gethash (first (miricl-compiler::hir-instruction-attributes producer)) arguments)
                        (first (miricl-compiler::hir-instruction-attributes producer)))))
               (value-mask (value &optional (depth 0))
                 (spend)
                 (when (or (null value) (> depth 64) (gethash value unsafe)) (return-from value-mask 0))
                 (let ((sources (gethash value flow)))
                   (when sources
                     (return-from value-mask
                       (let ((mask all))
                         (dolist (source sources mask)
                           (setq mask (logand mask (value-mask source (1+ depth)))))))))
                 (let ((producer (miricl-compiler::hir-value-producer value)))
                   (if (not (miricl-compiler::%hir-instruction-p producer)) 0
                     (case (miricl-compiler::hir-instruction-opcode producer)
                       (:argument (gethash value masks 0))
                       (:copy (value-mask (first (miricl-compiler::hir-instruction-inputs producer)) (1+ depth)))
                       (:cell-read
                        (let* ((cell (first (miricl-compiler::hir-instruction-inputs producer)))
                               (creation (origin cell)))
                          (if (and (not (gethash cell unsafe))
                                   (miricl-compiler::%hir-instruction-p creation)
                                   (eq (miricl-compiler::hir-instruction-opcode creation) :cell-new)
                                   (not (gethash (miricl-compiler::hir-instruction-output creation) unsafe)))
                              (value-mask (first (miricl-compiler::hir-instruction-inputs creation)) (1+ depth)) 0)))
                       (otherwise 0))))))
        (unless (and flow (<= (length functions) 64) (member selected functions :test #'eq))
          (return-from proof nil))
        (dolist (function functions)
          (unless (and (null (miricl-compiler::hir-function-parent function))
                       (null (miricl-compiler::hir-function-captures function)))
            (return-from proof nil))
          (let ((arity (miricl-compiler::native-fixed-arity function)))
            (unless arity (return-from proof nil))
            (setf (gethash function arguments) (make-array arity :initial-element nil)))
          (dolist (block (miricl-compiler::hir-function-blocks function))
            (dolist (instruction (cons (miricl-compiler::hir-block-terminator block)
                                      (miricl-compiler::hir-block-instructions block)))
              (spend)
              (setf (gethash instruction owners) function)
              (when (eq (miricl-compiler::hir-instruction-opcode instruction) :argument)
                (setf (aref (gethash function arguments)
                            (first (miricl-compiler::hir-instruction-attributes instruction)))
                      (miricl-compiler::hir-instruction-output instruction)))
              (loop for input in (miricl-compiler::hir-instruction-inputs instruction) for index from 0 do
                (spend) (push (cons instruction index) (gethash input uses))))))
        (dolist (item descriptor)
          (unless (< (first item) (length (gethash selected arguments)))
            (miricl-compiler::compiler-input-fail "runtime kernel fixed callbacks" item "formal index exceeds entry arity")))
        ;; Resolve internal edges before proving any callback, so a replacement
        ;; arriving through a recursive or otherwise inconvenient edge counts.
        (maphash (lambda (instruction owner)
          (spend)
          (when (eq (miricl-compiler::hir-instruction-opcode instruction) :call)
            (let* ((inputs (miricl-compiler::hir-instruction-inputs instruction))
                   (target (callee (second inputs))))
              (when target
                (unless (= (length (cddr inputs)) (length (gethash target arguments)))
                  (return-from proof nil))
                (setf (gethash instruction calls) target)
                (push (cons instruction owner) (gethash target incoming)))))) owners)
        (let ((work (list selected)))
          (loop while work for function = (pop work) unless (gethash function reachable) do
            (spend) (setf (gethash function reachable) t)
            (maphash (lambda (instruction target)
              (spend) (when (eq (gethash instruction owners) function) (push target work))) calls)))
        ;; Private cells permit reads only. Escape, mutation, capture and an
        ;; unknown argument consumer invalidate every upstream identity fact.
        (maphash (lambda (value consumers)
          (dolist (use consumers)
            (spend)
            (let* ((instruction (car use)) (index (cdr use))
                   (opcode (miricl-compiler::hir-instruction-opcode instruction))
                   (output (miricl-compiler::hir-instruction-output instruction))
                   (target (gethash instruction calls)))
              (cond ((and (member opcode '(:copy :primary :values :cell-new :cell-read)) (zerop index))
                     (push output (gethash value edges)))
                    ((and (or (and (eq opcode :jump) (zerop index))
                              (and (eq opcode :block-return) (= index 2)))
                          (first (miricl-compiler::hir-instruction-attributes instruction)))
                     ;; Forward the primary certificate and its unsafe-use
                     ;; dependency together. A join with unknown/replacement
                     ;; ingress is rejected by VALUE-MASK's intersection.
                     (push (first (miricl-compiler::hir-block-parameters
                                    (first (miricl-compiler::hir-instruction-attributes instruction))))
                           (gethash value edges)))
                    ((and (eq opcode :call) (= index 1)))
                    ((and target (>= index 2))
                     (push (aref (gethash target arguments) (- index 2)) (gethash value edges)))
                    (t (setf (gethash value unsafe) t)))))) uses)
        (loop with changed = t while changed do
          (setq changed nil)
          (maphash (lambda (value targets)
            (spend)
            (when (and (not (gethash value unsafe)) (some (lambda (target) (gethash target unsafe)) targets))
              (setf (gethash value unsafe) t changed t))) edges))
        (maphash (lambda (function values)
          (loop for value across values for index from 0 do
            (spend)
            (setf (gethash value masks)
              (cond ((not (gethash function reachable)) 0)
                    ((eq function selected)
                     (loop for item in descriptor for bit from 0
                           when (= (first item) index) return (ash 1 bit) finally (return 0)))
                    (t all))))) arguments)
        (loop with changed = t while changed do
          (setq changed nil)
          (maphash (lambda (function values)
            (loop for value across values for index from 0 do
              (spend)
              (let ((mask (gethash value masks 0)))
                (dolist (edge (gethash function incoming))
                  (when (gethash (cdr edge) reachable)
                    (setq mask (logand mask (value-mask
                      (nth (+ index 2) (miricl-compiler::hir-instruction-inputs (car edge))))))))
                (unless (= mask (gethash value masks 0))
                  (setf (gethash value masks) mask changed t))))) arguments))
        (maphash (lambda (instruction owner)
          (when (and (gethash owner reachable)
                     (eq (miricl-compiler::hir-instruction-opcode instruction) :call))
            (let* ((inputs (miricl-compiler::hir-instruction-inputs instruction))
                   (value (second inputs)) (mask (value-mask value)))
              (loop for entry in entries for bit from 0
                    when (and (logbitp bit mask) (= (length (cddr inputs)) (second entry))) do
                      (setf (gethash value facts) entry) (return))))) owners)
        facts))))

(defun kernel-fixed-callback-emitter (facts)
  (when facts
    (lambda (inputs)
      (let ((entry (gethash (second inputs) facts)))
        (when (and entry (= (length (cddr inputs)) (second entry)))
          ;; All operands already have SSA values. Keep their source evaluation
          ;; order and the callee snapshot even when its eventual use disappears.
          (dolist (input (cddr inputs)) (miricl-compiler::wasm-get input))
          (miricl-compiler::wasm-call (first entry))
          t)))))

(defun kernel-emit-native-module (unit selected descriptor entries)
  ;; Kernel literals are resolved by the worker adapter before whole-image
  ;; outlining. Its selected worker ABI does not include private helpers.
  (let* ((miricl-compiler::*native-outlining-budget* 0)
         (facts (and descriptor (kernel-fixed-callback-facts unit selected descriptor)))
         (callbacks (kernel-fixed-callback-emitter facts))
         (miricl-compiler::*wasm-fixed-call-emitter*
           (lambda (inputs)
             (or (and callbacks (funcall callbacks inputs))
                 (let* ((producer (miricl-compiler::hir-value-producer (second inputs)))
                        (name (and (miricl-compiler::%hir-instruction-p producer)
                                   (eq (miricl-compiler::hir-instruction-opcode producer) :function-reference)
                                   (first (miricl-compiler::hir-instruction-attributes producer))))
                        (entry (assoc name '((miricl-runtime:packet-length "native-packet-length" 1)
                                             (miricl-runtime:packet-ref "native-packet-ref" 2)))))
                   ;; These representation operations exist only in the
                   ;; trusted host kernel linker; ordinary Lisp compilation
                   ;; retains the normal function binding contracts.
                   (when (and entry (= (length (cddr inputs)) (third entry)))
                     (dolist (input (cddr inputs)) (miricl-compiler::wasm-get input))
                     (miricl-compiler::wasm-call (second entry)) t)))))
         (miricl-compiler::*native-imports*
           (append miricl-compiler::*native-imports*
             '(("native-packet-length" (:object) :object)
               ("native-packet-ref" (:object :object) :object))
             (when facts
               (loop for (name arity) in (kernel-fixed-imports) collect
                 (list name (make-list arity :initial-element :object) :object))))))
    (miricl-compiler::emit-native-module-checked unit entries nil)))

(defun kernel-macroexpand-hook (expander form environment)
  ;; Use the target's logical expansion: host AND/OR expanders can introduce
  ;; implementation-private operators unavailable in the shipped runtime.
  (when (and (consp form) (member (car form) '(and or)))
    (return-from kernel-macroexpand-hook (miricl-runtime:expand-logical-form form)))
  ;; The host's SETF expansion may name private operators; a SETF SVREF
  ;; function binding is not required on the host. Preserve place/value order.
  (if (and (consp form) (eq (first form) 'setf) (= (length form) 3)
           (consp (second form))
           (or (member (first (second form)) '(svref char))
               (and (eq (first (second form)) 'row-major-aref)
                    (eq (cdr (assoc '(setf row-major-aref) miricl-compiler::*hir-inline-policy*
                                    :test #'equal)) 'inline)))
           (= (length (second form)) 3))
      (let ((vector (gensym "VECTOR")) (index (gensym "INDEX")))
        `(let ((,vector ,(second (second form))) (,index ,(third (second form))))
           (funcall #',(list 'setf (first (second form))) ,(third form) ,vector ,index)))
      (if (and (consp form) (eq (first form) 'setf) (= (length form) 3)
               (consp (second form))
               (member (first (second form)) '(elt char row-major-aref gethash fill-pointer symbol-value symbol-plist symbol-function))
               (= (length (second form))
                  (if (member (first (second form)) '(fill-pointer symbol-value symbol-plist symbol-function)) 2 3)))
          ;; Supply the target's accessor-function ABI explicitly. Host SETF
          ;; expanders may instead name implementation-private setters.
          (let* ((place (second form)) (arguments (cdr place))
                 (temporaries (mapcar (lambda (argument) (declare (ignore argument)) (gensym "PLACE")) arguments)))
            `(let ,(mapcar #'list temporaries arguments)
               (funcall #',(list 'setf (car place)) ,(third form) ,@temporaries)))
          (require-target-macro-expansion expander form environment))))

(defun kernel-backend ()
  ;; REPL reloads can change an emitter or proof helper while kernel source
  ;; stays unchanged. Cache against the actual host definitions, not a build
  ;; timestamp, and discard old generations instead of retaining their code.
  (list (fdefinition 'kernel-worker-wat) (fdefinition 'kernel-literal-plan)
    (fdefinition 'kernel-import-name)
    (fdefinition 'kernel-closed-helper-unit) (fdefinition 'kernel-helper-group-wat)
    (fdefinition 'kernel-link-helper-group)
    (fdefinition 'kernel-fixed-imports)
    (fdefinition 'kernel-fixed-callback-descriptor)
    (fdefinition 'kernel-fixed-callback-facts)
    (fdefinition 'kernel-fixed-callback-emitter)
    (fdefinition 'kernel-emit-native-module)
    (when (fboundp 'miricl::native-fixed-primitive-entries)
      (fdefinition 'miricl::native-fixed-primitive-entries))
    (copy-tree (kernel-fixed-imports))
    (fdefinition 'kernel-macroexpand-hook)
    (copy-tree miricl-compiler::*native-imports*)
    (loop for name in miricl-compiler::*native-compiler-seeds*
          when (and (fboundp name) (not (macro-function name)))
            collect (cons name (fdefinition name)))))

(defparameter *kernel-source-files*
  '("runtime-kernel-library.lisp" "runtime-character-kernels.lisp"
    "runtime-iteration-expansion-kernels.lisp"
    "runtime-list-mapping-kernels.lisp" "runtime-reader-kernels.lisp"
    "runtime-integer-field-kernels.lisp" "runtime-reduce-kernels.lisp"
    "runtime-sequence-edit-kernels.lisp" "runtime-count-kernels.lisp"
    "runtime-string-case-loop-kernels.lisp" "runtime-search-kernels.lisp"
    "runtime-mismatch-kernels.lisp" "runtime-set-kernels.lisp"
    "runtime-character-sequence-kernels.lisp" "runtime-package-kernels.lisp"
    "runtime-package-entry-kernels.lisp"
    "runtime-tree-kernels.lisp" "runtime-format-kernels.lisp"
    "runtime-array-type-kernels.lisp" "runtime-stream-sequence-kernels.lisp"
    "runtime-clos-method-kernels.lisp"
    "runtime-real-type-kernels.lisp" "runtime-array-upgrade-kernels.lisp"
    "runtime-clos-initarg-kernels.lisp"
    "runtime-generic-dispatch-kernels.lisp" "runtime-compound-type-kernels.lisp"))

(defun kernel-definitions ()
  ;; One closed group lets private helpers inline across migration batches.
  ;; Read the sources on each query so persistent host REPL edits invalidate
  ;; the existing source-keyed cache without rebuilding the compiler image.
  (loop for file in *kernel-source-files* append
    (with-open-file (stream (if (uiop:absolute-pathname-p file)
                               file
                               (asdf:system-relative-pathname :miricl file)))
      (let ((*package* (find-package :miricl-runtime)) (*read-eval* nil))
        (loop for form = (read stream nil stream) until (eq form stream)
              when (and (consp form) (eq (car form) 'defun)
                        (not (member (second form) '(miricl-runtime:packet-length miricl-runtime:packet-ref))))
                collect form)))))

(defun kernel-export-index (bytes name)
  (let ((input (miricl-compiler::wasm-input bytes 8)))
    (loop while (< (miricl-compiler::wasm-input-position input) (length bytes)) do
      (let* ((kind (miricl-compiler::module-byte input))
             (size (miricl-compiler::module-uleb input))
             (end (+ size (miricl-compiler::wasm-input-position input))))
        (when (= kind 7)
          (dotimes (i (miricl-compiler::module-uleb input))
            (let* ((text (map 'string #'code-char (miricl-compiler::module-name input)))
                   (export-kind (miricl-compiler::module-byte input))
                   (index (miricl-compiler::module-uleb input)))
              (when (and (zerop export-kind) (string= text name))
                (return-from kernel-export-index index)))))
        (setf (miricl-compiler::wasm-input-position input) end))))
  (error "Missing compiled runtime export ~S." name))

(defun kernel-import-name (import)
  "Return a current runtime helper's canonical name."
  (unless (string= (first import) "m")
    (error "Unsupported native runtime import namespace: ~S." (first import)))
  (concatenate 'string "native-" (second import)))

(defun kernel-literal-plan (ops imports literals)
  "Resolve bootstrap symbols, setter references and integers without a pool."
  (let ((replacements (make-hash-table)) (skipped (make-hash-table))
        (flow (map 'vector #'copy-seq ops)))
    (loop for op across ops for i from 0
          when (and (= (miricl-compiler::native-op-code op) #x10)
                    (< (miricl-compiler::native-op-argument op) (length imports))
                    (member (second (aref imports (miricl-compiler::native-op-argument op)))
                      '("native-literal" "native-literal-function") :test #'equal)) do
      (unless (and (>= i 2)
                   (= (miricl-compiler::native-op-code (aref ops (- i 2))) #x20)
                   (zerop (miricl-compiler::native-op-argument (aref ops (- i 2))))
                   (= (miricl-compiler::native-op-code (aref ops (1- i))) #x41))
        (error "Runtime kernel has a nonconstant literal lookup."))
      (let* ((fused (string= (second (aref imports (miricl-compiler::native-op-argument op)))
                              "native-literal-function"))
             (slot (miricl-compiler::native-op-argument (aref ops (1- i))))
             (literal (and (<= 0 slot) (< slot (length literals)) (aref literals slot)))
             (id (and (symbolp literal) (<= 0 slot) (< slot (length literals))
                      (position literal miricl::*runtime-symbols*)))
             ;; Inspect only a fixed number of cells, so malformed/cyclic
             ;; literal graphs cannot hang this trusted runtime linker.
             (setter-id (and (consp literal) (eq (car literal) 'setf)
                             (consp (cdr literal)) (null (cddr literal))
                             (symbolp (second literal))
                             (position (second literal) miricl::*runtime-symbols*)))
             (setter-reference
               (and setter-id (or fused (and (< (1+ i) (length ops))
                    (let ((next (aref ops (1+ i))))
                      (and (= (miricl-compiler::native-op-code next) #x10)
                           (< (miricl-compiler::native-op-argument next) (length imports))
                           (string= (second (aref imports (miricl-compiler::native-op-argument next)))
                                    "native-function-reference"))))))))
        (unless (or id setter-reference
                    (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807)))
          (error "Runtime kernel requires a bootstrap literal at slot ~S: ~A." slot
            (let ((*print-circle* t) (*print-length* 10) (*print-level* 5))
              (write-to-string literal))))
        (when (and setter-reference (not fused)) (setf (gethash (1+ i) skipped) t))
        (setf (gethash (- i 2) replacements)
                (cond (id (cons (if fused :function :symbol) id))
                      (setter-reference (cons :setter-function setter-id))
                      (t (cons :integer literal)))
              (gethash (1- i) skipped) t (gethash i skipped) t
              ;; Remove only the proved constant lookup's closure dependency
              ;; from the ABI liveness check; every other read stays visible.
              (miricl-compiler::native-op-code (aref flow (- i 2))) #xd0
              (miricl-compiler::native-op-argument (aref flow (- i 2))) -17)))
    (values replacements skipped flow)))

(defun kernel-worker-wat (bytes index name arity result interruptible &optional (literals #()))
  (declare (notinline kernel-literal-plan))
  (multiple-value-bind (body imports offset signature)
      (miricl-compiler::native-disassembly-body bytes index)
    (declare (ignore offset))
    (unless (equal signature '(("externref" "externref" "externref" "externref" "externref") ("externref")))
      (error "Unsupported runtime worker signature: ~S." signature))
    (let* ((imports (map 'vector (lambda (import)
                                  (list (first import) (kernel-import-name import))) imports))
           (fixed-imports (kernel-fixed-imports))
           (ops (miricl-compiler::native-read-ops body))
           (input (miricl-compiler::wasm-input body)) (locals nil) (raw-truth nil)
           (vector-read nil) (vector-write nil) (vector-index nil) (vector-object nil) (vector-value nil)
           (call-slots 0) (call-base 0) (integer-call nil) (integer-left 0) (integer-right 0))
      (dotimes (i (miricl-compiler::module-uleb input))
        (let ((count (miricl-compiler::module-uleb input))
              (type (miricl-compiler::module-byte input)))
          (unless (member type '(#x7f #x7e #x6f)) (error "Unsupported runtime local type: ~D." type))
          (dotimes (j count) (push (case type (#x7f "i32") (#x7e "i64") (otherwise "(ref null eq)")) locals))))
      (multiple-value-bind (literal-replacements literal-skips flow-ops)
          (kernel-literal-plan ops imports literals)
      ;; An entry has no closure object or padded arguments. Prove those
      ;; private ABI inputs dead before changing their slots into zeroed locals.
      (miricl-compiler::native-op-liveness flow-ops (+ 5 (length locals))
        (miricl-compiler::native-op-flow flow-ops))
      (let ((live (miricl-compiler::native-op-live-in (aref flow-ops 0))))
        (when (or (logbitp 0 live)
                  (loop for i from (1+ arity) below 5 thereis (logbitp i live)))
          (error "Runtime kernel depends on a closure or padded argument: ~S." name)))
      (loop for op across ops
            when (and (= (miricl-compiler::native-op-code op) #x10)
                      (< (miricl-compiler::native-op-argument op) (length imports))) do
        (let ((target (second (aref imports (miricl-compiler::native-op-argument op)))))
          (when (string= target "native-vector-ref-inline") (setf vector-read t))
          (when (string= target "native-vector-set-inline") (setf vector-write t))
          (let ((arity (position target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4") :test #'equal)))
            (when arity (setf call-slots (max call-slots (1+ arity)))))
          (when (member target '("native-call" "native-primary-call" "native-character-code") :test #'equal)
            (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-ref-inline") (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-set-inline") (setf call-slots (max call-slots 3)))
          (when (string= target "native-puthash-inline") (setf call-slots (max call-slots 3)))
          (when (string= target "native-simple-character-set-inline")
            (setf integer-call t call-slots (max call-slots 2)))
          (when (string= target "native-code-character-inline") (setf call-slots (max call-slots 1)))
          (when (string= target "native-logand-general-inline") (setf call-slots (max call-slots 2)))
          (when (member target '("native-mod-general-inline" "native-truncate-primary-inline"
                                "native-add-integer" "native-sub-integer" "native-mul-integer") :test #'equal)
            (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-code-at-inline") (setf call-slots (max call-slots 2)))
          (when (member target '("native-character-code-at64-inline" "native-character-code-at64-unboxed-inline") :test #'equal)
            (setf integer-call t call-slots (max call-slots 1)))
          (when (string= target "native-character-code-at-unboxed-inline")
            (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-code-inline") (setf call-slots (max call-slots 1)))
          (when (equal (cdr (assoc target miricl-compiler::*native-imports* :test #'equal))
                       '((:object :i64 :i64) :object))
            (setf integer-call t call-slots (max call-slots 1)))
          (when (string= target "native-character-ref") (setf call-slots (max call-slots 3)))
          (when (member target '("native-arguments" "native-argument-set" "native-argument"
                                "native-rest-argument" "native-check-arguments" "native-argument-present"
                                "native-keyword-present" "native-keyword-argument" "native-check-keywords") :test #'equal)
            (setf integer-call t call-slots (max call-slots 3)))))
      (setf vector-index (+ 5 (length locals)) vector-object (1+ vector-index) vector-value (+ 2 vector-index)
            call-base (+ vector-index (if vector-write 3 (if vector-read 1 0)))
            integer-left (+ call-base call-slots) integer-right (1+ integer-left)
            locals (nreverse locals))
      (with-output-to-string (out)
        (format out "(func ~A (param~{ ~A~}) (result ~A)~%" name
          (make-list (if *kernel-worker-preserve-abi* 5 arity) :initial-element "(ref null eq)")
          (if (eq result :truth) "i32" "(ref null eq)"))
        (format out "(local~{ ~A~})~%" (append (unless *kernel-worker-preserve-abi*
                                              (make-list (- 5 arity) :initial-element "(ref null eq)"))
                                                        locals
                                                        (when (or vector-read vector-write) '("i32"))
                                                        (when vector-write '("(ref null eq)" "(ref null eq)"))
                                                        (make-list call-slots :initial-element "(ref null eq)")
                                                        (when integer-call '("i64" "i64"))))
        (labels ((return-next-p (index)
                   (loop for i from (1+ index) below (length ops)
                         for next = (aref ops i) for code = (miricl-compiler::native-op-code next)
                         do (cond
                              ((and (= code #x10) (< (miricl-compiler::native-op-argument next) (length imports))
                                    (member (second (aref imports (miricl-compiler::native-op-argument next)))
                                      '("native-poll" "native-return1") :test #'equal)))
                              (t (return (or (= code #x0f)
                                             (and (= code #x0b) (= (1+ i) (length ops))))))))))
        (loop for op across ops for index from 0 for code = (miricl-compiler::native-op-code op)
              for argument = (miricl-compiler::native-op-argument op)
              for last = (= (miricl-compiler::native-op-end op) (length body)) do
          (cond
            ((gethash index literal-skips))
            ((nth-value 1 (gethash index literal-replacements))
             (let ((replacement (gethash index literal-replacements)))
               (ecase (car replacement)
                 (:symbol (format out "i32.const ~D call $symbol~%" (cdr replacement)))
                 (:function (format out "i32.const ~D call $symbol call $kernel-function-reference~%" (cdr replacement)))
                 (:setter-function
                  (format out "i32.const ~D call $kernel-setter-function-reference~%" (cdr replacement)))
                 (:integer (format out "i64.const ~D call $int64~%" (cdr replacement))))))
            ((and (= code #x0b) last)
             (when (and (eq result :truth) (not raw-truth)) (write-line "ref.is_null i32.eqz" out)))
            ((= code #x0f)
             (when (and (eq result :truth) (not raw-truth)) (write-line "ref.is_null i32.eqz" out))
             (write-line "return" out) (setq raw-truth nil))
            ((and (member code '(#x02 #x03 #x04)) (= argument #x6f)
                  (= (- (miricl-compiler::native-op-end op) (miricl-compiler::native-op-start op)) 2))
             ;; Values carried by control edges use the same raw reference
             ;; ABI as parameters and locals after kernel adaptation.
             (format out "~A (result (ref null eq))~%"
               (case code (#x02 "block") (#x03 "loop") (#x04 "if"))))
            ((member code '(#x20 #x21 #x22))
             (format out "~A ~D~%" (case code (#x20 "local.get") (#x21 "local.set") (#x22 "local.tee"))
               (cond (*kernel-worker-preserve-abi* argument)
                     ((zerop argument) arity) ((<= argument arity) (1- argument)) (t argument))))
            ((member code '(#x10 #x12 #xd2))
             (cond
              ((and (member code '(#x10 #x12)) *kernel-module-workers*
                    (gethash argument *kernel-module-workers*))
               (format out "~A ~A~%" (if (= code #x12) "return_call" "call")
                 (gethash argument *kernel-module-workers*)))
              (t
             (unless (and (= code #x10) (< argument (length imports)))
               (error "Runtime kernel retains a general or indirect call."))
             (let ((target (second (aref imports argument))))
               (cond
                 ((assoc target fixed-imports :test #'equal)
                  ;; Exact same checkpoint and success-only primary-value
                  ;; cleanup as NATIVE-CALL1..3's registered primitive branch.
                  (write-line "(if (global.get $thread-active) (then (call $thread-checkpoint)))" out)
                  (format out "call ~A~%call $clear-values~%"
                    (miricl:wat-string (third (assoc target fixed-imports :test #'equal)))))
                 ;; Raw callers own value state. Preserve backedge polling
                 ;; for interruptible kernels; the other kernels explicitly
                 ;; match their original non-suspending primitive contract.
                 ((string= target "native-poll")
                  (when interruptible (write-line "call $thread-checkpoint" out)))
                 ((string= target "native-return1")
                  (when *kernel-worker-preserve-abi* (write-line "call $clear-values" out)))
                 ((and *kernel-worker-preserve-abi* (string= target "native-return"))
                  (write-line "call $return-values" out))
                 ((and *kernel-worker-preserve-abi* (string= target "native-tail-enter"))
                  (when interruptible (write-line "call $thread-checkpoint" out))
                  (write-line "call $clear-values" out))
                 ((string= target "native-boolean")
                  (if (and (eq result :truth) (return-next-p index))
                      (progn (write-line "i32.eqz i32.eqz" out) (setq raw-truth t))
                      (write-line "call $bool" out)))
                 ((member target '("native-car-list-inline" "native-car-inline-checked") :test #'equal)
                  (write-line "call $car" out))
                 ((member target '("native-cdr-list-inline" "native-cdr-inline-checked") :test #'equal)
                  (write-line "call $cdr" out))
                 ((string= target "native-eq-inline") (write-line "ref.eq call $bool" out))
                 ((string= target "native-eq-test") (write-line "ref.eq" out))
                 ((string= target "native-eql-inline") (write-line "call $eql call $bool" out))
                 ((string= target "native-eql-test") (write-line "call $eql" out))
                 ((string= target "native-cons") (write-line "call $cons" out))
                 ((string= target "native-cons2") (write-line "call $cons2" out))
                 ((string= target "native-list2") (write-line "ref.null eq call $cons2" out))
                 ((string= target "native-function-reference") (write-line "call $kernel-function-reference" out))
                 ((string= target "native-environment") (write-line "ref.null eq" out))
                 ((string= target "native-check-apply-list") (write-line "call $check-apply-list" out))
                 ((string= target "native-call")
                  (format out "local.set ~D local.set ~D drop local.get ~D local.get ~D call $apply call $value-list call $clear-values~%"
                    (1+ call-base) call-base call-base (1+ call-base)))
                 ((string= target "native-primary-call")
                  (format out "local.set ~D local.set ~D drop local.get ~D local.get ~D call $native-primary-apply~%"
                    (1+ call-base) call-base call-base (1+ call-base)))
                 ((string= target "native-primary") (write-line "call $car" out))
                 ((string= target "native-packet-length") (write-line "call $native-packet-length-cl" out))
                 ((string= target "native-packet-ref") (write-line "call $native-packet-ref-cl" out))
                 ((member target '("native-arguments" "native-argument-set" "native-argument"
                                   "native-rest-argument" "native-check-arguments" "native-argument-present"
                                   "native-keyword-present" "native-keyword-argument" "native-check-keywords") :test #'equal)
                  ;; Keep private packets as raw references. Imports have mixed
                  ;; boxed-reference and i32 operands; save each in a matching
                  ;; local before converting only the reference transport.
                  (let* ((signature (cdr (assoc target miricl-compiler::*native-imports* :test #'equal)))
                         (object-slot call-base) (integer-slot integer-left)
                         (slots (mapcar (lambda (type)
                                          (if (eq type :object)
                                              (prog1 (list type object-slot) (incf object-slot))
                                              (prog1 (list type integer-slot) (incf integer-slot))))
                                        (first signature))))
                    (dolist (slot (reverse slots))
                      (when (eq (first slot) :i32) (write-line "i64.extend_i32_s" out))
                      (format out "local.set ~D~%" (second slot)))
                    (dolist (slot slots)
                      (format out "local.get ~D ~A~%" (second slot)
                        (if (eq (first slot) :object) "extern.convert_any" "i32.wrap_i64")))
                    (format out "call $~A~%" target)
                    (when (second signature) (write-line "any.convert_extern ref.cast (ref null eq)" out))))
                 ((string= target "native-character-code-at64-inline")
                  (format out "local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D call $~A any.convert_extern ref.cast (ref null eq)~%"
                    integer-left call-base call-base integer-left target))
                 ((string= target "native-character-code-at64-unboxed-inline")
                  (format out "local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D call $~A~%"
                    integer-left call-base call-base integer-left target))
                 ((string= target "native-character-code-at-unboxed-inline")
                  (format out "local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D extern.convert_any call $~A~%"
                    (1+ call-base) call-base call-base (1+ call-base) target))
                 ((equal (cdr (assoc target miricl-compiler::*native-imports* :test #'equal))
                         '((:object :i64 :i64) :object))
                  ;; Keep the function context for arbitrary-precision fallback
                  ;; and the full scalar operands selected by the range proof.
                  (format out "local.set ~D local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D local.get ~D call $~A any.convert_extern ref.cast (ref null eq)~%"
                    integer-right integer-left call-base call-base integer-left integer-right target))
                 ((member target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4" "native-character-code" "native-character-ref" "native-character-ref-inline" "native-character-code-inline" "native-character-code-at-inline") :test #'equal)
                  (let ((count (cond ((string= target "native-character-code") 2)
                                     ((string= target "native-character-ref") 3)
                                     ((string= target "native-character-code-inline") 1)
                                     ((string= target "native-character-ref-inline") 2)
                                     ((string= target "native-character-code-at-inline") 2)
                                     (t (1+ (position target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4") :test #'equal))))))
                    (loop for slot downfrom (1- count) to 0 do (format out "local.set ~D~%" (+ call-base slot)))
                    (dotimes (slot count) (format out "local.get ~D extern.convert_any~%" (+ call-base slot)))
                    (format out "call $~A any.convert_extern ref.cast (ref null eq)~%" target)))
                 ((string= target "native-int64") (write-line "call $box-int64" out))
                 ((string= target "native-unbox-int64") (write-line "ref.as_non_null call $scalar-integer-value" out))
                 ;; These imports already represent proven simple-vector
                 ;; operations. Lower their access directly in the core ABI;
                 ;; extra helper calls substantially slow the swap loop.
                 ((string= target "native-vector-ref-inline")
                  (format out "i32.wrap_i64 local.set ~D call $vector-data local.get ~D array.get $objects~%"
                    vector-index vector-index))
                 ((string= target "native-vector-set-inline")
                  (format out "i32.wrap_i64 local.set ~D local.set ~D local.set ~D~%"
                    vector-index vector-object vector-value)
                  (format out "local.get ~D call $vector-data local.get ~D local.get ~D array.set $objects local.get ~D~%"
                    vector-object vector-index vector-value vector-value))
                 ((string= target "native-vector-length-inline") (write-line "call $vector-data array.len call $int" out))
                 ((string= target "native-sbit-ref-inline")
                  (write-line "call $checked-sbit-ref" out))
                 ((string= target "native-row-major-ref-inline")
                  (write-line "ref.null eq i32.const 0 call $array-element" out))
                 ((string= target "native-row-major-set-inline")
                  (write-line "call $checked-row-major-set" out))
                 ((string= target "native-length-inline")
                  (write-line "extern.convert_any call $native-length-inline any.convert_extern ref.cast (ref null eq)" out))
                 ((string= target "native-list-length-inline")
                  (write-line "call $list-length-native" out))
                 ((string= target "native-gethash-primary-inline")
                  (write-line "call $checked-gethash-primary" out))
                 ((string= target "native-puthash-inline")
                  (format out "local.set ~D local.set ~D local.set ~D~%local.get ~D local.get ~D local.get ~D call $puthash-native~%"
                    (+ call-base 2) (1+ call-base) call-base
                    (1+ call-base) (+ call-base 2) call-base))
                 ((member target '("native-consp-inline" "native-consp-test") :test #'equal)
                  (write-line "call $tag i32.const 7 i32.eq" out)
                  (when (string= target "native-consp-inline") (write-line "call $bool" out)))
                 ((string= target "native-simple-vector-p-inline")
                  (write-line "call $tag i32.const 34 i32.eq call $bool" out))
                 ((string= target "native-vector-p-inline")
                  (write-line "call $vectorp-native call $bool" out))
                 ((string= target "native-string-p-inline")
                  (write-line "call $stringp-native call $bool" out))
                 ((string= target "native-integer-p-inline")
                  (write-line "call $tag i32.const 1 i32.eq call $bool" out))
                 ((string= target "native-character-p-inline")
                  (write-line "call $tag i32.const 16 i32.eq call $bool" out))
                 ((string= target "native-integer-length-inline")
                  (write-line "i32.const 1 call $integer-bit-measure" out))
                 ((string= target "native-ordered-compare-inline")
                  (write-line "call $ordered-compare-native" out))
                 ((string= target "native-zero-inline")
                  (write-line "call $number call $zero call $bool" out))
                 ((string= target "native-ash-general-inline")
                  (write-line "call $ash-native" out))
                 ((string= target "native-logand-general-inline")
                  (format out "local.set ~D local.set ~D i32.const 8 local.get ~D local.get ~D i32.const 0 call $boole-native~%"
                    (1+ call-base) call-base call-base (1+ call-base)))
                 ((member target '("native-code-character-inline" "native-make-character-vector-inline" "native-make-string-inline") :test #'equal)
                  (format out "extern.convert_any call $~A any.convert_extern ref.cast (ref null eq)~%" target))
                 ((string= target "native-make-vector-inline")
                  (write-line "call $make-vector-native" out))
                 ((string= target "native-character-set-inline")
                  (format out "local.set ~D local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any local.get ~D extern.convert_any call $native-character-set-inline any.convert_extern ref.cast (ref null eq)~%"
                    (+ call-base 2) (1+ call-base) call-base call-base (1+ call-base) (+ call-base 2)))
                 ((string= target "native-simple-character-set-inline")
                  (format out "local.set ~D local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any local.get ~D call $native-simple-character-set-inline any.convert_extern ref.cast (ref null eq)~%"
                    integer-left (1+ call-base) call-base call-base (1+ call-base) integer-left))
                 ((member target '("native-mod-general-inline" "native-truncate-primary-inline"
                                   "native-add-integer" "native-sub-integer" "native-mul-integer") :test #'equal)
                  (format out "local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any call $~A any.convert_extern ref.cast (ref null eq)~%"
                    (1+ call-base) call-base call-base (1+ call-base) target))
                 (t (error "Unsupported runtime kernel dependency: ~S." target)))))))
            ((= code #xd0)
             (if (and (eq result :truth) (return-next-p index))
                 (progn (write-line "i32.const 0" out) (setq raw-truth t))
                 (write-line "ref.null eq" out)))
            ((= code #xfb)
             ;; Change the module's external reference view to the core's
             ;; raw eqref ABI. All kernel inputs and permitted primitives use
             ;; this representation, so these view conversions are identities.
             (let ((position (1+ (miricl-compiler::native-op-start op))) (subcode nil))
               (multiple-value-setq (subcode position) (miricl-compiler::native-read-uleb body position))
               (case subcode
                 ((#x1a #x1b))
                 (#x1c (write-line "ref.i31" out))
                 (#x1d (write-line "i31.get_s" out))
                 (#x14
                  (unless (= (miricl-compiler::native-read-sleb body position) 1)
                    (error "Unsupported runtime heap type test."))
                  (write-line "ref.test (ref $obj)" out))
                 ((2 5) (let ((type nil) (field nil))
                      (multiple-value-setq (type position) (miricl-compiler::native-read-uleb body position))
                      (multiple-value-setq (field position) (miricl-compiler::native-read-uleb body position))
                      (unless (= type 1) (error "Unsupported runtime field type: ~D." type))
                      (when (and (= subcode 5) (not (member field '(4 5))))
                        (error "Runtime kernel writes a non-cons field: ~D." field))
                      (format out "struct.~A $obj ~D~%" (if (= subcode 5) "set" "get") field)))
                 ((#x16 #x17)
                  (let ((type (miricl-compiler::native-read-sleb body position)))
                    (cond ((= type 1) (format out "ref.cast (ref ~A$obj)~%" (if (= subcode #x17) "null " "")))
                          ((= type -20) (format out "ref.cast (ref ~Ai31)~%" (if (= subcode #x17) "null " "")))
                          ((and (= subcode #x17) (= type -19)))
                          (t (error "Unsupported runtime reference cast: ~D." type)))))
                 (otherwise (error "Unsupported runtime GC operation: ~D." subcode)))))
            (t (write-line (miricl-compiler::native-disassembly-instruction op body imports) out)))))
        (write-line ")" out))))))

(defun kernel-closed-helper-unit (unit entry)
  "Remove only call-only, immutable module function cells from a fresh unit."
  (let* ((factory (first (miricl-compiler:hir-unit-functions unit)))
         (children (rest (miricl-compiler:hir-unit-functions unit)))
         (members (make-hash-table :test 'eq)) (uses (make-hash-table :test 'eq))
         (closed (make-hash-table :test 'eq)) (budget 32768) (selected nil))
    (unless (and children (<= (length children) 64))
      (error "Runtime helper group exceeds the closed-worker bound: ~S." entry))
    (labels ((spend () (when (minusp (decf budget))
                        (error "Runtime helper proof budget exhausted: ~S." entry)))
             (fail (reason) (miricl-compiler::compiler-input-fail "closed runtime helper" entry reason))
             (binding-target (binding)
               (when (and (miricl-compiler::%hir-binding-p binding)
                          (eq (miricl-compiler::hir-binding-owner binding) factory))
                 (let* ((local (miricl-compiler::hir-binding-local binding))
                        (target (and local (miricl-compiler::hir-local-function local))))
                   (and (gethash target members) target))))
             (closed-callee (value)
               ;; Charge COPY-chain traversal to this proof's total budget;
               ;; the ordinary emitter's per-query bound is not a total bound.
               (loop for producer = (miricl-compiler::hir-value-producer value) do
                 (spend)
                 (unless (and (miricl-compiler::%hir-instruction-p producer)
                              (eq (miricl-compiler::hir-instruction-opcode producer) :copy))
                   (return (and (miricl-compiler::%hir-instruction-p producer)
                                (eq (miricl-compiler::hir-instruction-opcode producer) :closure)
                                (first (miricl-compiler::hir-instruction-attributes producer)))))
                 (setf value (first (miricl-compiler::hir-instruction-inputs producer))))))
      (dolist (function children)
        (spend)
        (unless (and (eq (miricl-compiler::hir-function-parent function) factory)
                     (< (miricl-compiler::hir-function-debug-level function) 2)
                     (miricl-compiler::native-fixed-arity function))
          (fail "expected fixed-arity module children with DEBUG below 2"))
        (setf (gethash function members) t (gethash function closed) t))
      (dolist (binding (miricl-compiler::hir-function-bindings factory))
        (spend)
        (when (equal (miricl-compiler::hir-binding-name binding) entry)
          (setf selected (binding-target binding))))
      (unless selected (fail "entry does not name a retained module child"))
      ;; Build uses once. A capture cell may be read, but never mutated,
      ;; returned, passed to another function, or placed into another closure.
      (dolist (function children)
        (dolist (block (miricl-compiler::hir-function-blocks function))
          (dolist (instruction (cons (miricl-compiler::hir-block-terminator block)
                                    (miricl-compiler::hir-block-instructions block)))
            (spend)
            (loop for input in (miricl-compiler::hir-instruction-inputs instruction)
                  for position from 0 do (spend) (push (cons instruction position) (gethash input uses))))))
      (dolist (function children)
        (dolist (identity (miricl-compiler::hir-function-captures function))
          (spend)
          (unless (binding-target identity) (fail "captures non-module state")))
        (dolist (block (miricl-compiler::hir-function-blocks function))
          (dolist (instruction (miricl-compiler::hir-block-instructions block))
            (spend)
            (when (eq (miricl-compiler::hir-instruction-opcode instruction) :capture)
              (let* ((binding (second (miricl-compiler::hir-instruction-attributes instruction)))
                     (target (binding-target binding)))
                (unless target (fail "capture is not an immutable module function binding"))
                ;; LABELS initialization is the only admitted write. Check the
                ;; actual factory rather than relying on a declaration flag.
                (let ((writes 0))
                  (dolist (factory-block (miricl-compiler::hir-function-blocks factory))
                    (dolist (store (miricl-compiler::hir-block-instructions factory-block))
                      (spend)
                      (when (and (eq (miricl-compiler::hir-instruction-opcode store) :cell-write)
                                 (eq (first (miricl-compiler::hir-instruction-inputs store))
                                     (miricl-compiler::hir-binding-cell binding)))
                        (incf writes)
                        (unless (eq (closed-callee
                                      (second (miricl-compiler::hir-instruction-inputs store))) target)
                          (fail "module function binding has an unknown write")))))
                  (unless (= writes 1) (fail "module function binding is not initialized exactly once")))
                (dolist (use (gethash (miricl-compiler::hir-instruction-output instruction) uses))
                  (spend)
                  (let ((read (car use)))
                    (unless (and (zerop (cdr use))
                                 (eq (miricl-compiler::hir-instruction-opcode read) :cell-read))
                      (fail "module function cell escapes or is mutated"))
                    (setf (miricl-compiler::hir-instruction-opcode read) :closure
                          (miricl-compiler::hir-instruction-inputs read) nil
                          (miricl-compiler::hir-instruction-attributes read) (list target)
                          (miricl-compiler::hir-instruction-effect read) :allocation)))
                (setf (miricl-compiler::hir-instruction-opcode instruction) :constant
                      (miricl-compiler::hir-instruction-attributes instruction) (list nil)
                      (miricl-compiler::hir-instruction-effect instruction) :pure))))))
      ;; A function object itself may flow only through COPY to an exact-arity
      ;; CALL. Unknown joins/escapes keep the ordinary closure implementation.
      (let ((seen (make-hash-table :test 'eq)) (work nil))
        (dolist (function children)
          (dolist (block (miricl-compiler::hir-function-blocks function))
            (dolist (instruction (miricl-compiler::hir-block-instructions block))
              (spend)
              (when (eq (miricl-compiler::hir-instruction-opcode instruction) :closure)
                (unless (and (gethash (first (miricl-compiler::hir-instruction-attributes instruction)) members)
                             (null (miricl-compiler::hir-instruction-inputs instruction)))
                  (fail "nested closure or lexical state remains"))
                (push (miricl-compiler::hir-instruction-output instruction) work)))))
        (loop while work for value = (pop work) unless (gethash value seen) do
          (setf (gethash value seen) t) (spend)
          (dolist (use (gethash value uses))
            (spend)
            (let* ((instruction (car use)) (opcode (miricl-compiler::hir-instruction-opcode instruction)))
              (cond ((eq opcode :copy) (push (miricl-compiler::hir-instruction-output instruction) work))
                    ((and (eq opcode :call) (= (cdr use) 1)
                          (let ((target (closed-callee value)))
                            (and (gethash target members)
                                 (= (- (length (miricl-compiler::hir-instruction-inputs instruction)) 2)
                                    (miricl-compiler::native-fixed-arity target))))))
                    (t (fail "function reference escapes or has an unsupported call arity")))))))
      (dolist (function children)
        (setf (miricl-compiler::hir-function-parent function) nil
              (miricl-compiler::hir-function-captures function) nil))
      (setf (miricl-compiler:hir-unit-functions unit) children)
      (values unit selected closed))))

(defun kernel-link-helper-group (group)
  ;; GROUP is cached before module ownership is assigned. Link it again for
  ;; every assembly, including warm-cache builds and standalone generators.
  (destructuring-bind (bytes records selected-index arity name result interruptible) group
    (let ((*kernel-worker-preserve-abi* t)
          (*kernel-module-workers* (make-hash-table))
          (dependencies (make-hash-table))
          (emitted (make-hash-table))
          (done (make-hash-table)) (pending (copy-list records)) (budget 32768))
      (dolist (record records)
        (setf (gethash (first record) *kernel-module-workers*) (fourth record)))
      (when *kernel-shared-workers*
        (dolist (record records)
          (multiple-value-bind (body) (miricl-compiler::native-disassembly-body bytes (first record))
            (if (or (not (plusp budget)) (> (length body) 262144))
                (setf budget 0)
              (loop for op across (miricl-compiler::native-read-ops body)
                    while (plusp budget) do
                (decf budget)
                (when (and (member (miricl-compiler::native-op-code op) '(#x10 #x12))
                           (gethash (miricl-compiler::native-op-argument op) *kernel-module-workers*)
                           (/= (miricl-compiler::native-op-argument op) (first record)))
                  (pushnew (miricl-compiler::native-op-argument op)
                           (gethash (first record) dependencies))))))))
      (with-output-to-string (out)
        (labels ((emit (record share)
                   (destructuring-bind (index debug arity original pool policy) record
                     ;; No function object or closure survives the closed-group
                     ;; proof. Literals are resolved by KERNEL-WORKER-WAT before
                     ;; comparison, so equal pools never justify sharing.
                     (let* ((key (when (and share (zerop debug)
                                            (< (hash-table-count *kernel-shared-workers*) 4096))
                                   (setf (gethash index *kernel-module-workers*) "$kernel-sharing-self")
                                   (kernel-worker-wat bytes index "$kernel-sharing-self" arity
                                     :object interruptible pool)))
                            (previous (and key (gethash key *kernel-shared-workers*))))
                       (setf (gethash index *kernel-module-workers*) (or previous original)
                             (gethash index done) t)
                       (miricl-compiler::record-wasm-outline-policy (or previous original) policy)
                       (unless previous
                         (setf (gethash index emitted)
                           (kernel-worker-wat bytes index original arity :object interruptible pool))
                         (when key (setf (gethash key *kernel-shared-workers*) original)))))))
          ;; Resolve callees first. Self recursion is normalized in the key;
          ;; other recursive components retain their ordinary distinct bodies.
          (loop while pending
                for ready = (and *kernel-shared-workers* (plusp budget)
                                 (find-if (lambda (record)
                                            (every (lambda (callee) (gethash callee done))
                                                   (gethash (first record) dependencies))) pending))
                do (if ready
                       (progn (emit ready t) (setf pending (remove ready pending :test #'eq)))
                       (progn (dolist (record pending) (emit record nil)) (setf pending nil)))))
        ;; Dependency order decides ownership, not layout. With no duplicate
        ;; body, module bytes keep their original function ordering.
        (dolist (record records)
          (let ((text (gethash (first record) emitted)))
            (when text (write-string text out))))
        (format out "(func ~A (param~{ ~A~}) (result ~A)~%ref.null eq~%" name
          (make-list arity :initial-element "(ref null eq)")
          (if (eq result :truth) "i32" "(ref null eq)"))
        (dotimes (i arity) (format out "local.get ~D~%" i))
        (dotimes (i (- 4 arity)) (write-line "ref.null eq" out))
        (format out "call ~A~%" (gethash selected-index *kernel-module-workers*))
        (when (eq result :truth) (write-line "ref.is_null i32.eqz" out))
        (write-line ")" out)))))

(defun kernel-helper-group-wat (unit entry name result interruptible &optional defer fixed-callbacks)
  (multiple-value-bind (unit selected closed) (kernel-closed-helper-unit unit entry)
    (let* ((children (miricl-compiler:hir-unit-functions unit))
           (miricl-compiler::*wasm-closed-worker-functions* closed)
           (*kernel-worker-preserve-abi* t)
           (*kernel-module-workers* (make-hash-table))
           (records nil))
      (multiple-value-bind (bytes literals) (kernel-emit-native-module unit selected fixed-callbacks children)
        (loop for function in children for slot from 0 for pool in literals
              for arity = (miricl-compiler::native-fixed-arity function)
              for index = (kernel-export-index bytes (format nil "direct-entry~D-~D" slot arity))
              for previous = (assoc index records) do
          (when (and previous (not (equalp pool (fifth previous))))
            (error "Runtime helpers share code with different literal ownership: ~S." entry))
          (when previous
            (setf (sixth previous)
              (miricl-compiler::record-wasm-outline-policy (fourth previous)
                (if (>= (miricl-compiler::hir-function-debug-level function) 2) 0
                    (miricl-compiler::hir-function-optimization-policy function)))))
          (unless previous
            (setf (gethash index *kernel-module-workers*) (format nil "~A-worker~D" name slot))
            (push (list index (miricl-compiler::hir-function-debug-level function) arity
                        (gethash index *kernel-module-workers*) pool
                        (if (>= (miricl-compiler::hir-function-debug-level function) 2) 0
                    (miricl-compiler::hir-function-optimization-policy function))) records)))
        (let* ((slot (position selected children)) (arity (miricl-compiler::native-fixed-arity selected))
               (index (kernel-export-index bytes (format nil "direct-entry~D-~D" slot arity)))
               (group (list bytes (nreverse records) index arity name result interruptible)))
          (miricl-compiler::record-wasm-outline-policy name
            (if (>= (miricl-compiler::hir-function-debug-level selected) 2) 0
                (miricl-compiler::hir-function-optimization-policy selected)))
          (if defer group (kernel-link-helper-group group)))))))

(defun runtime-kernel-wat (wat-name entry &key (result :object) interruptible fixed-callbacks)
  "Cross-compile one single-valued internal runtime kernel.
Compile its closed helper group with inlining; link only the verified worker.
INTERRUPTIBLE preserves compiler backedge polls as core scheduler checkpoints.
Fixed bootstrap symbols and ordinary Lisp calls are linked explicitly.
FIXED-CALLBACKS is a trusted private adapter contract: each (formal-index
registered-primitive) names the immutable saved callback supplied on every
external entry. Internal ingresses are proved separately on fresh HIR.
Unsupported dependencies and captures are errors, never fallbacks."
  ;; The cache tracks replacements of this linker entry. Same-file calls may
  ;; otherwise retain the compiled definition on hosts such as ECL.
  (declare (notinline kernel-worker-wat kernel-macroexpand-hook))
  (unless (member result '(:object :truth)) (error "Unsupported runtime result ABI: ~S." result))
  (miricl-compiler::compiler-function-name entry "runtime kernel entry")
  (setf fixed-callbacks (kernel-fixed-callback-descriptor fixed-callbacks))
  (let ((backend (kernel-backend)))
    (unless (equal backend *kernel-backend*)
      (clrhash *kernel-cache*) (setq *kernel-backend* backend)))
  (let* ((definitions (kernel-definitions)) (key (list wat-name entry result interruptible fixed-callbacks definitions))
         (cached
           (or (gethash key *kernel-cache*)
               (setf (gethash key *kernel-cache*)
                 (let ((miricl-compiler::*compiler-state* :interpreted)
                       (miricl-compiler::*hir-load-time-evaluator* #'require-target-load-time-evaluation)
                       (*macroexpand-hook* (fdefinition 'kernel-macroexpand-hook)))
                   (let* ((unit (miricl-compiler:compile-module-to-hir definitions :entry-points (list entry)))
                          (children (rest (miricl-compiler:hir-unit-functions unit))))
                     (if (not (and children (null (cdr children))))
                         (kernel-helper-group-wat unit entry (miricl:wat-string wat-name) result interruptible t fixed-callbacks)
                         (progn
                           (when (miricl-compiler::hir-function-captures (first children))
                             (error "Runtime kernel retains lexical captures: ~S." entry))
                           ;; The discarded factory owns definitions, not runtime state.
                           (setf (miricl-compiler::hir-function-parent (first children)) nil
                                 (miricl-compiler:hir-unit-functions unit) children)
                           (let ((arity (miricl-compiler::native-fixed-arity (first children))))
                             (unless arity (error "Runtime kernel requires fixed arity: ~S." entry))
                             (multiple-value-bind (bytes literals)
                                 (kernel-emit-native-module unit (first children) fixed-callbacks children)
                               (miricl-compiler::record-wasm-outline-policy (miricl:wat-string wat-name)
                                 (if (>= (miricl-compiler::hir-function-debug-level (first children)) 2) 0
                                     (miricl-compiler::hir-function-optimization-policy (first children))))
                               (kernel-worker-wat bytes
                                 (kernel-export-index bytes (format nil "direct-entry0-~D" arity))
                                 (miricl:wat-string wat-name) arity result interruptible (first literals))))))))))))
    (if (stringp cached) cached (kernel-link-helper-group cached))))
