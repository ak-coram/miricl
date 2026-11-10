;;;; Object-system tests run in the guest, including custom metaclasses.
(in-package #:miricl-test)
(in-suite :miricl)

(defun clos-value (value)
  ;; Source APIs preserve guest package identities; compare names in this suite.
  (cond ((m:guest-symbol-p value)
         (intern (m:guest-symbol-name value)
                 (if (and (m:guest-symbol-package value)
                          (string= "KEYWORD" (m:guest-package-name (m:guest-symbol-package value))))
                     :keyword :miricl-test)))
        ((consp value) (cons (clos-value (car value)) (clos-value (cdr value))))
        (t value)))
(defun clos-eval (vm source) (clos-value (m:evaluate-string vm source)))

(test packed-library-literals
  ;; Exercise the actual packed initializer with payloads on LEB boundaries,
  ;; mixed symbol namespaces, unaligned doubles, and supplementary characters.
  (let* ((prefixes
           (loop for (tag symbol length) in miricl::*runtime-library-prefixes* append
             (list (cons symbol (make-list (1- length) :initial-element 17))
                   (cons symbol 17)
                   (cons symbol (make-list length :initial-element 17)))))
         (shared (list 'car 17))
         (literal (list nil t 'identity 'packed-private-marker :packed-keyword
                        0 -1 63 64 127 128 -128 -129 16383 16384
                        -2147483648 2147483647 1.25 -2.5d0 -0.0d0
                        #\Space #\λ #\🙂 (code-char #x10ffff)
                        (make-string 128 :initial-element #\λ)
                        '(1 . 2)
                        ;; Short-list tag boundaries and long proper lists;
                        ;; dotted lengths also cross the counted LEB boundary.
                        (loop for length in '(1 2 63 64 65 95 96 97 127 128 8191 8192)
                              collect (make-list length :initial-element 'identity))
                        (loop for length in '(1 63 64 96 97)
                              collect (nconc (make-list length :initial-element nil) 'identity))
                        prefixes (list shared shared)))
         ;; Long spines, nested CARs, and dotted tails must survive bootstrap
         ;; without consuming either the host or WASM call stack per element.
         (long-literal (nconc (make-list 20000 :initial-element '(1 . 2)) 3))
         (miricl::*clos-library*
           (append miricl::*clos-library*
             (list (miricl::lower-runtime-library
               `(defparameter *compile-file-pathname* ',literal))
                   (miricl::lower-runtime-library
               `(defparameter *compile-file-truename* ',long-literal)))))
         (miricl::*clos-library-symbols*
           (union miricl::*clos-primitives* (miricl::runtime-library-symbols miricl::*clos-library*)))
         (miricl::*interpreter-wasm* nil))
    (m:with-interpreter (vm)
      ;; Private prefix operators are uninterned guest symbols. CLOS-VALUE
      ;; compares their names in this test package, as for other private names.
      (labels ((names (object)
                 (cond ((consp object) (cons (names (car object)) (names (cdr object))))
                       ((and (symbolp object) (not (keywordp object))
                             (not (member object m::*runtime-symbols*)))
                        (intern (symbol-name object) :miricl-test))
                       (t object))))
        (is (equal (names literal) (clos-eval vm "*compile-file-pathname*"))))
      ;; Repeated abbreviated forms must still allocate independent conses.
      (is (eq t (clos-eval vm "(let ((pair (car (last *compile-file-pathname*))))
        (and (not (eq (first pair) (second pair)))
             (not (eq (cdr (first pair)) (cdr (second pair))))
             (progn (rplaca (first pair) 'cdr) (eq (car (second pair)) 'car))))")))
      (is (equal '((1 . 2) (1 . 2) 3)
                 (clos-eval vm "(list (car *compile-file-truename*)
                   (nth 19999 *compile-file-truename*)
                   (nthcdr 20000 *compile-file-truename*))"))))))

(test packed-library-tags
  ;; Verify the compact ranges and fallbacks without depending on library size.
  (flet ((pack (form) (miricl::runtime-library-bytes (list form) nil 24)))
    (is (equalp #(0) (pack nil)))
    (is (equalp (vector (+ 128 (position t miricl::*runtime-library-symbol-codes* :key #'first))) (pack t)))
    (dolist (id '(127 128))
      (let* ((symbol (nth id miricl::*runtime-symbols*))
             (compact (position symbol miricl::*runtime-library-symbol-codes* :key #'first)))
        (is (equalp (if compact (vector (+ 128 compact))
                       (if (< id 128) (vector 6 id) (vector 6 128 1))) (pack symbol)))))
    (is (equalp #(32 0) (pack '(nil))))
    (is (equalp #(33 0 0) (pack '(nil nil))))
    (is (= 63 (aref (pack (make-list 32)) 0)))
    (is (equalp #(7 66) (subseq (pack (make-list 33)) 0 2)))
    (is (equalp #(7 128 1) (subseq (pack (make-list 64)) 0 3)))
    (is (equalp #(7 130 1) (subseq (pack (make-list 65)) 0 3)))
    (is (equalp #(7 192 1) (subseq (pack (make-list 96)) 0 3)))
    (is (equalp #(7 194 1) (subseq (pack (make-list 97)) 0 3)))
    (let ((code (+ 128 (position t miricl::*runtime-library-symbol-codes* :key #'first))))
      (is (equalp (vector 7 3 code code) (pack '(t . t)))))))

(test packed-library-symbol-codes
  (is (= 128 (length m::*runtime-library-symbol-codes*)))
  (is (= 128 (length (remove-duplicates m::*runtime-library-symbol-codes* :test #'equal))))
  (loop for (symbol length namespace) in m::*runtime-library-symbol-codes* for code from 128 do
    (let* ((table (case namespace (:clos m::*clos-library-symbols*) (:loop m::*loop-library-symbols*)))
           (wire-tag (if (eq namespace :loop) 25 24))
           (bytes (m::runtime-library-bytes (list symbol) table wire-tag)))
      (is (zerop length))
      (is (equalp (vector code) bytes))
      (when namespace
        (is (< 1 (length (m::runtime-library-bytes (list symbol) (copy-list table) wire-tag))))))))

(test clos-classes-and-slots
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "
      (let ((initial 11))
        (defclass shape () ((x :initarg :x :initform initial :accessor shape-x)
                           (missing) (count :initform 0 :allocation :class :accessor shape-count))))
      (defclass colored () ((color :initarg :color :initform 'red :reader shape-color)))
      (defclass point (shape colored) ((y :initarg :y :initform 2 :accessor point-y))
        (:default-initargs :x 17))")
    (is (equal '(17 2 red t t t nil)
      (clos-eval vm "(let ((p (make-instance 'point)))
        (list (shape-x p) (point-y p) (shape-color p) (typep p 'shape)
              (typep p (find-class 'point)) (slot-exists-p p 'missing) (slot-boundp p 'missing)))")))
    (is (equal '(42 42 11)
      (clos-eval vm "(let ((a (make-instance 'shape)) (b (make-instance 'point)))
        (setf (shape-count a) 42)
        (list (shape-count a) (shape-count b) (shape-x a)))")))
    (is (equal '(30 nil t)
      (clos-eval vm "(let ((p (make-instance 'point)))
        (with-slots (x (vertical y)) p (setq x 10 vertical 20))
        (list (with-accessors ((x shape-x) (y point-y)) p (+ x y))
              (progn (slot-makunbound p 'x) (slot-boundp p 'x))
              (handler-case (slot-value p 'missing) (unbound-slot (c)
                (eq p (unbound-slot-instance c))))))")))
    (is (eq t (clos-eval vm "(handler-case (make-instance 'point :unknown 1)
                                     (program-error (c) t))")))
    (let ((object (clos-eval vm "(make-instance 'point :x 27)")))
      (is (m:guest-instance-p object))
      (is (eq object (m:evaluate vm object)))
      (is (= 27 (m:evaluate vm `(slot-value ,object 'x)))))))

(test clos-generic-functions
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "
      (defclass animal () ()) (defclass dog (animal) ())
      (defgeneric describe-animal (animal &optional detail))
      (defmethod describe-animal ((a animal) &optional (detail 0)) (+ 10 detail))
      (defmethod describe-animal ((a dog) &optional (detail 0)) (+ 100 (call-next-method)))
      (defmethod describe-animal ((a (eql 'spot)) &optional detail) 999)
      (defgeneric pair-rank (a b) (:argument-precedence-order b a))
      (defmethod pair-rank ((a integer) b) 1)
      (defmethod pair-rank (a (b integer)) 2)")
    (is (equal '(115 999 2 t t)
      (clos-eval vm "(list (describe-animal (make-instance 'dog) 5)
        (describe-animal 'spot) (pair-rank 1 2) (functionp #'describe-animal)
        (typep #'describe-animal 'generic-function))")))
    (is (= 7 (clos-eval vm "(defmethod key-method ((x integer) &key (scale 2) &allow-other-keys) (+ x scale))
                                    (key-method 3 :scale 4 :ignored t)")))
    (is (= 42 (clos-eval vm "(defmethod (setf custom-value) ((new integer) (a animal)) new)
                                     (setf (custom-value (make-instance 'dog)) 42)")))
    (is (equal '(t nil)
      (clos-eval vm "(defmethod next-available ((a animal)) (next-method-p))
        (defmethod next-available ((a dog)) (list (next-method-p) (call-next-method)))
        (next-available (make-instance 'dog))")))
    (is (= 10 (clos-eval vm "(let* ((gf #'describe-animal)
          (method (find-method gf nil (list (find-class 'dog)))))
        (remove-method gf method) (describe-animal (make-instance 'dog)))")))))

(test method-function-keywords
  (m:with-interpreter (vm)
    (dolist (source
      '("(typep #'function-keywords 'generic-function)"
        "(defmethod keyword-info ((x integer) &optional (b 2)
                    &key (c 3) ((:dee d) 4) e ((eff f)) &aux local)
           (declare (ignore x b c d e f local)) nil)
         (let ((method (find-method #'keyword-info nil (list (find-class 'integer)))))
           (equal (multiple-value-list (function-keywords method)) '((:c :dee :e eff) nil)))"
        "(defmethod keyword-rest ((x t) &rest rest) (declare (ignore x rest)))
         (equal (multiple-value-list (function-keywords (find-method #'keyword-rest nil (list (find-class t)))))
                '(nil nil))"
        "(defmethod keyword-allow ((x t) &key a b &allow-other-keys &aux local)
           (declare (ignore x a b local)))
         (equal (multiple-value-list (function-keywords (find-method #'keyword-allow nil (list (find-class t)))))
                '((:a :b) t))"
        "(defmethod keyword-empty ((x t) &key &allow-other-keys) (declare (ignore x)))
         (equal (multiple-value-list (function-keywords (find-method #'keyword-empty nil (list (find-class t)))))
                '(nil t))"
        "(defmethod keyword-required ((x t) &optional y &aux z) (declare (ignore x y z)))
         (equal (multiple-value-list (function-keywords (find-method #'keyword-required nil (list (find-class t)))))
                '(nil nil))"
        "(let* ((method (find-method #'keyword-allow nil (list (find-class t))))
                (keys (function-keywords method)))
           (setf (car keys) :changed)
           (equal (function-keywords method) '(:a :b)))"
        "(handler-case (function-keywords) (program-error () t))"
        "(handler-case (function-keywords nil nil) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "FUNCTION-KEYWORDS mismatch: ~A" source))))

(test documentation-generic-protocol
  (m:with-interpreter (vm)
    (dolist (source
      '("(and (typep #'documentation 'generic-function)
              (typep (fdefinition '(setf documentation)) 'generic-function))"
        "(and (equal (mapcar #'symbol-name (mop:generic-function-argument-precedence-order #'documentation))
                          '(\"TYPE\" \"OBJECT\"))
              (equal (mapcar #'symbol-name (mop:generic-function-argument-precedence-order (fdefinition '(setf documentation))))
                     '(\"TYPE\" \"OBJECT\" \"VALUE\")))"
        "(let ((specializers (list (find-class 'function) (mop:intern-eql-specializer t))))
           (not (null (find-method #'documentation nil specializers))))"
        "(defclass doc-extension () ((text :initform nil :accessor doc-extension-text)))
         (defmethod documentation ((object doc-extension) (type (eql t))) (doc-extension-text object))
         (defmethod (setf documentation) ((value string) (object doc-extension) (type (eql t)))
           (setf (doc-extension-text object) value))
         (let ((object (make-instance 'doc-extension)))
           (and (null (documentation object t))
                (string= (setf (documentation object t) \"Custom documentation\") \"Custom documentation\")
                (string= (documentation object t) \"Custom documentation\")))"
        "(defgeneric documented-protocol (x) (:documentation \"Generic documentation\"))
         (defmethod documentation ((object standard-generic-function) (type symbol)) \"Broad method\")
         (and (string= (documentation #'documented-protocol t) \"Generic documentation\")
              (string= (documentation #'documented-protocol :custom) \"Broad method\"))"
        "(defpackage :documentation-package (:use) (:documentation \"Original package documentation\"))
         (let ((package (find-package :documentation-package)))
           (and (string= (documentation package t) \"Original package documentation\")
                (string= (setf (documentation package t) \"Updated package documentation\") \"Updated package documentation\")
                (string= (documentation package t) \"Updated package documentation\")))"
        "(let ((package (find-package :documentation-package)))
           (rename-package package \"RENAMED-DOCUMENTATION-PACKAGE\")
           (and (string= (documentation package t) \"Updated package documentation\")
                (progn (setf (documentation package t) nil) (null (documentation package t)))))"
        "(define-method-combination documented-combination :operator list :documentation \"Combination documentation\")
         (and (string= (documentation 'documented-combination 'method-combination) \"Combination documentation\")
              (string= (setf (documentation 'documented-combination 'method-combination) \"Updated combination documentation\")
                       \"Updated combination documentation\")
              (string= (documentation 'documented-combination 'method-combination) \"Updated combination documentation\"))"
        "(defgeneric combined-documentation (x) (:method-combination documented-combination))
         (let ((object (mop:generic-function-method-combination #'combined-documentation)))
           (and (string= (documentation object t) \"Updated combination documentation\")
                (string= (setf (documentation object t) \"Object documentation\") \"Object documentation\")
                (string= (documentation object 'method-combination) \"Object documentation\")
                (string= (documentation 'documented-combination 'method-combination) \"Updated combination documentation\")))"
        "(let ((object (mop:generic-function-method-combination #'combined-documentation)))
           (setf (documentation object t) nil)
           (null (documentation object t)))"
        "(handler-case (documentation) (program-error () t))"
        "(handler-case (documentation nil t nil) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Documentation protocol mismatch: ~A" source))))

(test generic-arity-and-literal-method-bodies
  (m:with-interpreter (vm)
    (dolist (source
      '("(defmethod literal-method ((x t)) \"Result string\")
         (string= (literal-method 1) \"Result string\")"
        "(defmethod declared-literal-method ((x t)) (declare (ignore x)) \"Declared result\")
         (string= (declared-literal-method 1) \"Declared result\")"
        "(defmethod documented-literal-method ((x t)) \"Method documentation\"
           (declare (ignore x)) \"Documented result\")
         (and (string= (documented-literal-method 1) \"Documented result\")
              (string= (documentation (find-method #'documented-literal-method nil (list (find-class t))) t)
                       \"Method documentation\"))"
        "(defgeneric arity-empty (x &optional y))
         (handler-case (arity-empty) (program-error () t))"
        "(handler-case (arity-empty 1 2 3) (program-error () t))"
        "(defvar *arity-hook-called* nil)
         (defmethod no-applicable-method ((gf (eql #'arity-empty)) &rest args)
           (declare (ignore args)) (setq *arity-hook-called* t) :handled)
         (and (eq (arity-empty 1) :handled) *arity-hook-called*)"
        "(setq *arity-hook-called* nil)
         (and (handler-case (arity-empty 1 2 3) (program-error () t))
              (not *arity-hook-called*))"
        "(defmethod arity-empty ((x integer) &optional y) (list x y))
         (and (equal (arity-empty 1) '(1 nil)) (equal (arity-empty 1 2) '(1 2))
              (handler-case (arity-empty 1 2 3) (program-error () t)))"))
      (is (eq t (m:evaluate-string vm source)) "Generic arity/literal body mismatch: ~A" source))))

(test generic-cache-class-and-eql-dispatch
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defgeneric cache-class (x))
      (defmethod cache-class ((x t)) :other)
      (defclass cache-parent () ()) (defclass cache-child (cache-parent) ())")
    (let ((cases
      '((null :null "nil") (symbol :symbol "'x") (fixnum :fixnum "536870911")
        (fixnum :fixnum "-536870912") (fixnum :fixnum "1073741824") (bignum :bignum "9223372036854775808")
        (fixnum :fixnum "-1073741825") (bignum :bignum "-9223372036854775809") (bignum :bignum "(ash 1 200)")
        (ratio :ratio "3/7") (single-float :single "1.25f0") (double-float :double "1.25d0")
        (complex :complex "#c(1 2)") (cons :cons "'(1 . 2)")
        (function :function "#'car") (character :character "#\\λ")
        (string :string "\"Text\"") (string :string "(make-array 3 :element-type 'character :initial-element #\\a :adjustable t)")
        (vector :vector "#(1 2)") (bit-vector :bits "#*101")
        (array :array "(make-array '(2 2))") (package :package "*package*")
        (hash-table :hash "(make-hash-table)") (readtable :readtable "(copy-readtable)")
        (pathname :pathname "(pathname \"test.lisp\")") (random-state :random "(make-random-state)")
        (string-stream :string-stream "(make-string-input-stream \"text\")")
        (broadcast-stream :broadcast "(make-broadcast-stream)")
        (two-way-stream :two-way "(make-two-way-stream (make-string-input-stream \"\") (make-string-output-stream))")
        (synonym-stream :synonym "(make-synonym-stream '*standard-output*)")
        (error :error "(make-condition 'simple-error)")
        (cache-parent :parent "(make-instance 'cache-parent)")
        (cache-child :child "(make-instance 'cache-child)"))))
      (dolist (case cases)
        (m:evaluate-string vm (format nil "(defmethod cache-class ((x ~A)) ~S)" (first case) (second case))))
      ;; Revisit every populated class entry, in both cache traversal directions.
      (dolist (case (append cases (reverse cases) cases))
        (is (eq t (m:evaluate-string vm (format nil "(eq ~S (cache-class ~A))" (second case) (third case))))
            "Cached class dispatch mismatch for ~A" (third case))))
    (loop for arity from 1 to 4 do
      (dotimes (mask (ash 1 arity))
        (let* ((name (format nil "CACHE-EQL-~D-~D" arity mask))
               (variables (loop for i below arity collect (format nil "ARG~D" i)))
               (arguments (loop for i below arity collect (+ 17 i)))
               (specializers (loop for i below arity for variable in variables
                                  collect (format nil "(~A ~A)" variable
                                    (if (logbitp i mask) (format nil "(eql ~D)" (+ 17 i)) "integer")))))
          (m:evaluate-string vm (format nil "(defgeneric ~A (~{~A~^ ~}))
            (defmethod ~A (~{~A~^ ~}) :general)
            (defmethod ~A (~{~A~^ ~}) :specific)"
              name variables name variables name specializers))
          (dotimes (round 3)
            (is (eq t (m:evaluate-string vm (format nil "(eq :specific (~A ~{~D~^ ~}))" name arguments))))
            (loop for i below arity do
              (let ((different (copy-list arguments)))
                (setf (nth i different) (if (logbitp i mask) -1 1.5f0))
                (is (eq t (m:evaluate-string vm (format nil "(eq :general (~A ~{~S~^ ~}))" name different))))))))))))

(test generic-cache-local-invalidation
  (m:with-interpreter (vm)
    (dolist (source
      '("(defgeneric warm-dispatch (x))
         (defmethod warm-dispatch ((x t)) :warm)
         (defgeneric edited-dispatch (x))
         (defmethod edited-dispatch ((x t)) :old)
         (defun dispatch-cache (gf)
           (funcall (miricl-compiler::compiler-primitive 10 1 \"%GET\") gf :cache))
         (assert (eq (warm-dispatch 1) :warm))
         (assert (eq (edited-dispatch 1) :old))
         (let ((cache (dispatch-cache #'warm-dispatch)))
           (assert cache)
           (defgeneric fresh-dispatch (x))
           (defclass fresh-dispatch-class () ())
           (ensure-generic-function 'edited-dispatch :lambda-list '(x))
           (assert (eq (warm-dispatch 1) :warm))
           (assert (eq cache (dispatch-cache #'warm-dispatch)))
           (defmethod edited-dispatch ((x integer)) :integer)
           (assert (eq (edited-dispatch 1) :integer))
           (assert (eq (warm-dispatch 1) :warm))
           (assert (eq cache (dispatch-cache #'warm-dispatch)))
           (remove-method #'edited-dispatch
             (find-method #'edited-dispatch nil (list (find-class 'integer))))
           (and (eq (edited-dispatch 1) :old)
                (eq cache (dispatch-cache #'warm-dispatch))))"
        "(defclass counted-generic (standard-generic-function)
            ((initializations :initform 0)))
         (defmethod shared-initialize :after ((gf counted-generic) slots &rest initargs)
           (declare (ignore slots initargs)) (incf (slot-value gf 'initializations)))
         (defgeneric counted-dispatch (x) (:generic-function-class counted-generic))
         (let ((count (slot-value #'counted-dispatch 'initializations)))
           (defmethod counted-dispatch ((x t)) :general)
           (defmethod counted-dispatch ((x integer)) :integer)
           (and (= count (slot-value #'counted-dispatch 'initializations))
                (eq (counted-dispatch 1) :integer) (eq (counted-dispatch nil) :general)))"
        "(defgeneric relevant-dispatch (x y))
         (defmethod relevant-dispatch ((x integer) y) (values x y))
         (assert (equal (multiple-value-list (relevant-dispatch 1 nil)) '(1 nil)))
         (let ((cache (dispatch-cache #'relevant-dispatch)))
           (assert (equal (multiple-value-list (relevant-dispatch 2 :different-class)) '(2 :different-class)))
           (assert (eq cache (dispatch-cache #'relevant-dispatch)))
           (defmethod relevant-dispatch ((x integer) (y integer)) (+ x y))
           (and (= (relevant-dispatch 3 4) 7)
                (equal (multiple-value-list (relevant-dispatch 5 nil)) '(5 nil))))"
        "(defgeneric all-arguments-general (x y))
         (defmethod all-arguments-general (x y) (list x y))
         (assert (equal (all-arguments-general 1 nil) '(1 nil)))
         (let ((cache (dispatch-cache #'all-arguments-general)))
           (and (equal (all-arguments-general :other '(:list)) '(:other (:list)))
                (eq cache (dispatch-cache #'all-arguments-general))))"
        "(defgeneric shared-dispatch-plan (x))
         (defmethod shared-dispatch-plan ((x number)) x)
         (assert (= (shared-dispatch-plan 1) 1))
         (assert (= (shared-dispatch-plan 1.5) 1.5))
         (let ((cache (dispatch-cache #'shared-dispatch-plan)))
           (and (= (length cache) 2) (eq (third (first cache)) (third (second cache)))))"
        "(defgeneric no-required-dispatch (&optional x))
         (defmethod no-required-dispatch (&optional (x 17)) x)
         (assert (= (no-required-dispatch) 17))
         (let ((cache (dispatch-cache #'no-required-dispatch)))
           (and (= (no-required-dispatch 23) 23)
                (eq cache (dispatch-cache #'no-required-dispatch))
                (handler-case (no-required-dispatch 1 2) (program-error () t))))"
        "(defgeneric reordered-dispatch (x y))
         (defmethod reordered-dispatch ((x integer) y) :x)
         (defmethod reordered-dispatch (x (y integer)) :y)
         (assert (eq (reordered-dispatch 1 2) :x))
         (ensure-generic-function 'reordered-dispatch :argument-precedence-order '(y x))
         (eq (reordered-dispatch 1 2) :y)"))
      (is (eq t (m:evaluate-string vm source))))))

(test generic-cache-method-plans-and-invalidation
  (m:with-interpreter (vm)
    (dolist (source
      '("(defvar *plan-events* nil)
         (defgeneric planned-call (x))
         (defmethod planned-call ((x t)) (push (list :primary-t x) *plan-events*) (values x 19))
         (defmethod planned-call ((x integer)) (push (list :primary-integer x) *plan-events*) (call-next-method (1+ x)))
         (defmethod planned-call :before ((x t)) (push (list :before-t x) *plan-events*))
         (defmethod planned-call :before ((x integer)) (push (list :before-integer x) *plan-events*))
         (defmethod planned-call :after ((x t)) (push (list :after-t x) *plan-events*) (values))
         (defmethod planned-call :after ((x integer)) (push (list :after-integer x) *plan-events*) (values 99 100))
         (defmethod planned-call :around ((x number))
           (push (list :around-number-in x) *plan-events*)
           (multiple-value-prog1 (call-next-method) (push (list :around-number-out x) *plan-events*)))
         (defmethod planned-call :around ((x integer))
           (push (list :around-integer-in x) *plan-events*)
           (multiple-value-prog1 (call-next-method (1+ x)) (push (list :around-integer-out x) *plan-events*)))
         (dotimes (round 3 t)
           (setq *plan-events* nil)
           (assert (equal (multiple-value-list (planned-call 17)) '(19 19)))
           (assert (equal (reverse *plan-events*)
             '((:around-integer-in 17) (:around-number-in 18)
               (:before-integer 18) (:before-t 18) (:primary-integer 18) (:primary-t 19)
               (:after-t 18) (:after-integer 18) (:around-number-out 18) (:around-integer-out 17)))))"
        "(defgeneric planned-zero (x))
         (defmethod planned-zero ((x t)) (values))
         (defmethod planned-zero :before ((x t)) (values 1 2))
         (defmethod planned-zero :after ((x t)) (values 3 4))
         (defmethod planned-zero :around ((x t)) (call-next-method))
         (dotimes (round 3 t) (assert (null (multiple-value-list (planned-zero 17)))))"
        "(defgeneric planned-last (x))
         (defmethod planned-last ((x t)) (call-next-method))
         (defmethod no-next-method ((gf (eql #'planned-last)) method &rest args)
           (assert (eq method (find-method gf nil (list (find-class t)))))
           (values :hook args))
         (dotimes (round 3 t)
           (assert (equal (multiple-value-list (planned-last 17)) '(:hook (17)))))"
        "(defgeneric changed-cache (x))
         (defmethod changed-cache ((x t)) :old)
         (assert (eq (changed-cache 17) :old)) (assert (eq (changed-cache 17) :old))
         (defmethod changed-cache ((x integer)) :new)
         (and (eq (changed-cache 17) :new) (eq (changed-cache 17) :new)
              (progn (remove-method #'changed-cache (find-method #'changed-cache nil (list (find-class 'integer))))
                     (eq (changed-cache 17) :old)))"
        "(defclass changed-parent-a () ()) (defclass changed-parent-b () ())
         (defclass changed-child (changed-parent-a) ())
         (defmethod changed-cache ((x changed-parent-a)) :a)
         (defmethod changed-cache ((x changed-parent-b)) :b)
         (defparameter *changed-object* (make-instance 'changed-child))
         (assert (eq (changed-cache *changed-object*) :a)) (assert (eq (changed-cache *changed-object*) :a))
         (defclass changed-child (changed-parent-b) ())
         (and (eq (changed-cache *changed-object*) :b) (eq (changed-cache *changed-object*) :b))"
        "(mop:set-funcallable-instance-function #'changed-cache (lambda (&rest args) (values :custom args)))
         (equal (multiple-value-list (changed-cache 17 23)) '(:custom (17 23)))"
        "(defgeneric foreign-dispatch (x)) (defmethod foreign-dispatch ((x t)) (values :foreign x))
         (defgeneric own-dispatch (x)) (defmethod own-dispatch ((x t)) (values :own x))
         (let ((function (mop:compute-discriminating-function #'foreign-dispatch)))
           (defmethod mop:compute-discriminating-function ((gf (eql #'own-dispatch))) function))
         (defmethod own-dispatch ((x integer)) (values :own-integer x))
         (equal (multiple-value-list (own-dispatch 17)) '(:foreign 17))"
        "(define-condition newly-classified-condition (error) ())
         (let* ((object (make-condition 'newly-classified-condition)) (class (class-of object)))
           (and class (eq class (find-class 'newly-classified-condition)) (typep object class)))"
        "(handler-case (class-of) (program-error () t))"
        "(handler-case (class-of nil nil) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Cached method plan mismatch: ~A" source))))

(test method-lambda-lowering-effects-and-captures
  (m:with-interpreter (vm)
    (dolist (source
      '("(defvar *method-default-events* nil)
         (defgeneric default-effects (x &optional y &rest args &key a alias))
         (defmethod default-effects ((x integer)
              &optional (y (progn (push :optional *method-default-events*) (+ x 1)) yp)
              &rest rest &key (a (progn (push :a *method-default-events*) (+ y 1)))
              ((:alias b) (progn (push :b *method-default-events*) (+ a 1)) bp)
              &aux (c (progn (push :aux *method-default-events*) (+ a b))))
           (list x y yp rest a b bp c))
         (dotimes (round 3 t)
           (setq *method-default-events* nil)
           (assert (equal (default-effects 1) '(1 2 nil nil 3 4 nil 7)))
           (assert (equal (reverse *method-default-events*) '(:optional :a :b :aux))))"
        "(dotimes (round 3 t)
           (setq *method-default-events* nil)
           (assert (equal (default-effects 1 10 :a 20 :alias 30)
                          '(1 10 t (:a 20 :alias 30) 20 30 t 50)))
           (assert (equal *method-default-events* '(:aux))))"
        "(defgeneric captured-default (x &optional y))
         (let ((bound 17)) (defmethod captured-default ((x t) &optional (y bound)) y))
         (and (= (captured-default nil) 17) (= (captured-default nil 23) 23))"
        "(defgeneric escaped-next (x &key delta))
         (defmethod escaped-next ((x t) &key (delta 0)) (+ x delta))
         (defmethod escaped-next ((x integer) &key (delta 0))
           (declare (ignore delta)) (lambda () (call-next-method)))
         (let ((a (escaped-next 17 :delta 3)) (b (escaped-next 23 :delta 4)))
           (and (= (funcall a) 20) (= (funcall b) 27) (= (funcall a) 20)))"
        "(equal (function-keywords (find-method #'default-effects nil (list (find-class 'integer))))
                 '(:a :alias))"))
      (is (eq t (m:evaluate-string vm source)) "Method lowering effect/capture mismatch: ~A" source))))

(test method-next-context-unwinding
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm "
      (defmethod unwind-inner ((x t)) (throw 'inner :left))
      (defmethod unwind-outer ((x t)) (values :base x))
      (defmethod unwind-outer ((x integer))
        (assert (eq (catch 'inner (unwind-inner x)) :left))
        (call-next-method))
      (dotimes (round 3 t)
        (assert (equal (multiple-value-list (unwind-outer 17)) '(:base 17))))")))))

(test method-combination-primary-requirements
  (m:with-interpreter (vm)
    (dolist (combination '(standard + append nconc list max min and or progn))
      (let* ((name (format nil "PRIMARY-REQUIRED-~A" combination))
             (primary-qualifier (if (eq combination 'standard) "" (symbol-name combination))))
        (m:evaluate-string vm (format nil "(defvar *primary-around-ran* nil)
          (defgeneric ~A (x) (:method-combination ~A))
          (defmethod ~A :around ((x t)) (setq *primary-around-ran* t) 17)
          (defmethod ~A ~A ((x integer)) x)" name combination name name primary-qualifier))
        (is (eq t (m:evaluate-string vm (format nil
          "(progn (setq *primary-around-ran* nil)
             (and (handler-case (~A 'a) (error () t)) (not *primary-around-ran*)))" name))))
        (is (= 17 (m:evaluate-string vm (format nil "(~A 1)" name))))))))

(test change-class-initialization-keywords
  (m:with-interpreter (vm)
    (dolist (source
      '("(defclass change-key-old () ((kept :initarg :kept :initform 1)))
         (defclass change-key-new () ((kept :initarg :kept) (added :initarg :added :initform 2)))
         (handler-case (change-class (make-instance 'change-key-old) 'change-key-new :nonsense t)
           (program-error () t))"
        "(let* ((object (make-instance 'change-key-old :kept 17))
                (result (change-class object 'change-key-new :added 23)))
           (and (eq object result) (= (slot-value object 'kept) 17) (= (slot-value object 'added) 23)))"
        "(let ((object (change-class (make-instance 'change-key-old) 'change-key-new
                         :unknown 17 :allow-other-keys t)))
           (= (slot-value object 'added) 2))"
        "(defmethod update-instance-for-different-class :after
                     ((previous change-key-old) (current change-key-new) &key accepted)
           (declare (ignore previous)) (setf (slot-value current 'added) accepted))
         (let ((object (change-class (make-instance 'change-key-old) 'change-key-new :accepted 31)))
           (= (slot-value object 'added) 31))"
        "(defmethod shared-initialize :after ((object change-key-new) slots &key shared-key)
           (declare (ignore slots)) (when shared-key (setf (slot-value object 'added) shared-key)))
         (let ((object (change-class (make-instance 'change-key-old) 'change-key-new :shared-key 47 :accepted 53)))
           (= (slot-value object 'added) 53))"
        "(handler-case (change-class (make-instance 'change-key-old) 'change-key-new :accepted)
           (program-error () t))"
        "(handler-case (change-class (make-instance 'change-key-old) 'change-key-new 17 23 :allow-other-keys t)
           (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "CHANGE-CLASS initialization mismatch: ~A" source))))

(test long-method-combination-groups
  (m:with-interpreter (vm)
    (dolist (source
      '("(define-method-combination grouped-methods ()
              ((first (:first *) (:first)) (rest *))
           (flet ((calls (methods) (mapcar (lambda (m) `(call-method ,m)) methods)))
             `(list (list ,@(calls first)) (list ,@(calls rest)))))
         (defgeneric grouped-call (x) (:method-combination grouped-methods))
         (defmethod grouped-call :first \"role\" ((x integer)) :typed)
         (defmethod grouped-call :first ((x t)) :base)
         (defmethod grouped-call :other ((x t)) :other)
         (equal (grouped-call 17) '((:typed :base) (:other)))"
        "(define-method-combination dotted-methods () ((methods (:tag . *)))
           `(list ,@(mapcar (lambda (m) `(call-method ,m)) methods)))
         (defgeneric dotted-call (x) (:method-combination dotted-methods))
         (defmethod dotted-call :tag ((x t)) :base)
         (defmethod dotted-call :tag one two ((x integer)) :typed)
         (equal (dotted-call 17) '(:typed :base))"
        "(let ((limit 10))
           (flet ((qualifies (qualifiers) (and (= (length qualifiers) 1) (integerp (car qualifiers))
                                             (< (car qualifiers) limit))))
             (define-method-combination predicate-methods () ((methods qualifies))
               `(list ,@(mapcar (lambda (m) `(call-method ,m))
                               (sort methods #'< :key (lambda (m) (car (method-qualifiers m)))))))))
         (defgeneric predicate-call (x) (:method-combination predicate-methods))
         (defmethod predicate-call 1 ((x t)) :first)
         (defmethod predicate-call 2 ((x t)) :second)
         (equal (predicate-call 17) '(:first :second))"
        "(define-method-combination ordered-methods (&optional (order :most-specific-first))
              ((methods * :order order :description \"Qualifiers ~S\"))
           `(list ,@(mapcar (lambda (m) `(call-method ,m)) methods)))
         (defgeneric ordered-call (x) (:method-combination ordered-methods :most-specific-last))
         (defmethod ordered-call ((x t)) :base) (defmethod ordered-call ((x integer)) :typed)
         (equal (ordered-call 17) '(:base :typed))"
        "(defgeneric invalid-order-call (x) (:method-combination ordered-methods :invalid))
         (defmethod invalid-order-call ((x t)) nil)
         (handler-case (invalid-order-call 17) (error () t))"
        "(define-method-combination string-qualifiers () ((methods *))
           `(list ,@(mapcar (lambda (m) `(call-method ,m)) methods)))
         (defgeneric string-qualified-call (x) (:method-combination string-qualifiers))
         (defmethod string-qualified-call \"role\" ((x t)) :old)
         (defmethod string-qualified-call \"role\" ((x t)) :new)
         (and (equal (string-qualified-call 17) '(:new))
              (= 1 (length (mop:generic-function-methods #'string-qualified-call)))
              (find-method #'string-qualified-call (list (copy-seq \"role\")) (list (find-class t))) t)"
        "(defvar *combination-body-ran* nil)
         (define-method-combination required-methods () ((methods (:accepted) :required t))
           (setq *combination-body-ran* t) `(call-method ,(car methods)))
         (defgeneric required-call (x) (:method-combination required-methods))
         (defmethod required-call :other ((x t)) nil)
         (and (handler-case (required-call 17) (error () t)) (not *combination-body-ran*))"
        "(define-method-combination rejected-methods () ((methods (:accepted)))
           (setq *combination-body-ran* t) nil)
         (defgeneric rejected-call (x) (:method-combination rejected-methods))
         (defmethod rejected-call :other ((x t)) nil)
         (and (handler-case (rejected-call 17) (error () t)) (not *combination-body-ran*))"
        "(handler-case (eval '(define-method-combination malformed-groups () ((methods))))
           (program-error () t))"
        "(handler-case (eval '(define-method-combination malformed-order () ((methods * :order))))
           (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Long method grouping mismatch: ~A" source))))

(test long-method-combination-argument-forms
  (m:with-interpreter (vm)
    (dolist (source
      '("(defvar *combination-defaults* 0)
         (let ((increment 1))
           (define-method-combination argument-methods (offset) ((methods *))
             (:arguments &whole whole a b &optional (c (progn (incf *combination-defaults*) (+ a offset increment)) cp)
                         &rest rest &key ((:alias key) :missing kp) &aux (aux (+ a 100)))
             (:generic-function generic)
             \"Argument documentation\"
             `(progn (call-method ,(car methods) nil)
                     (list ,whole ,a ,b ,c ,c ,cp ,rest ,key ,kp ,aux ',generic))))
         (defgeneric argument-call (x &optional y &rest rest) (:method-combination argument-methods 3))
         (defmethod argument-call ((x t) &optional y &rest rest) nil)
         (let ((result (argument-call 17)))
           (and (equal (butlast result) '((17) 17 nil 21 21 nil nil :missing nil 117))
                (eq (car (last result)) #'argument-call) (= *combination-defaults* 1)))"
        "(let ((result (argument-call 17 23 :alias 31)))
           (and (equal (butlast result) '((17 23 :alias 31) 17 nil 23 23 t (:alias 31) 31 t 117))
                (= *combination-defaults* 1)))"
        "(define-method-combination fewer-arguments () ((methods *))
           (:arguments a &optional (b :missing bp) &rest rest)
           `(progn (call-method ,(car methods) nil) (list ,a ,b ,bp ,rest)))
         (defgeneric fewer-call (x y &optional z &rest rest) (:method-combination fewer-arguments))
         (defmethod fewer-call ((x t) (y t) &optional z &rest rest) nil)
         (and (equal (fewer-call 1 2) '(1 :missing nil nil))
              (equal (fewer-call 1 2 3 :key 4) '(1 3 t (:key 4))))"
        "(define-method-combination extra-optionals () ((methods *))
           (:arguments a &optional (b :b bp) (c (+ b 1) cp) &rest rest)
           `(progn (call-method ,(car methods) nil) (list ,a ,b ,bp ,c ,cp ,rest)))
         (defgeneric extra-call (x &optional y &rest rest) (:method-combination extra-optionals))
         (defmethod extra-call ((x t) &optional y &rest rest) nil)
         (equal (extra-call 1 2 3 4) '(1 2 t 3 nil (3 4)))"
        "(string= (documentation 'argument-methods 'method-combination) \"Argument documentation\")"
        "(define-method-combination missing-next () ((methods *)) `(call-method ,(car methods)))
         (defgeneric missing-next-call (x) (:method-combination missing-next))
         (defmethod missing-next-call ((x t)) (assert (not (next-method-p))) (call-next-method))
         (handler-case (missing-next-call 17) (control-error () t))"
        "(define-method-combination wrapped-arguments () ((around (:around)) (primary () :required t))
           (:arguments x)
           `(call-method ,(car around) ((make-method (progn (call-method ,(car primary) nil) (list ,x))))))
         (defgeneric wrapped-call (x) (:method-combination wrapped-arguments))
         (defmethod wrapped-call ((x t)) nil)
         (defmethod wrapped-call :around ((x t)) (call-next-method))
         (equal (wrapped-call 17) '(17))"))
      (is (eq t (m:evaluate-string vm source)) "Method combination argument mismatch: ~A" source))))

(test clos-method-combination
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "
      (defparameter *events* nil)
      (defclass base () ()) (defclass derived (base) ())
      (defgeneric action (x) (:method-combination standard))
      (defmethod action ((x base)) (setq *events* (append *events* '(base))) (values 7 8))
      (defmethod action ((x derived)) (setq *events* (append *events* '(derived))) (call-next-method))
      (defmethod action :before ((x base)) (setq *events* (append *events* '(before-base))))
      (defmethod action :before ((x derived)) (setq *events* (append *events* '(before-derived))))
      (defmethod action :after ((x base)) (setq *events* (append *events* '(after-base))))
      (defmethod action :after ((x derived)) (setq *events* (append *events* '(after-derived))))
      (defmethod action :around ((x base))
        (setq *events* (append *events* '(around))) (call-next-method))")
    (is (equal '((7 8) (around before-derived before-base derived base after-base after-derived))
      (clos-eval vm "(list (multiple-value-list (action (make-instance 'derived))) *events*)")))
    (is (= 12 (clos-eval vm "(defgeneric total (x) (:method-combination +))
      (defmethod total + ((x base)) 5) (defmethod total + ((x derived)) 7)
      (total (make-instance 'derived))")))))

(test clos-inheritance-and-redefinition
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "(defclass a () ((x :initarg :x :initform 1)))
      (defclass b (a) ()) (defclass c (a) ()) (defclass d (b c) ())")
    (is (equal '(d b c a standard-object t)
      (clos-eval vm "(loop for c in (mop:class-precedence-list (find-class 'd)) collect (class-name c))")))
    (is (eq t (clos-eval vm "(handler-case (defclass a (d) ()) (error (c) t))")))
    (is (equal '(9 2 nil)
      (clos-eval vm "(defparameter *old* (make-instance 'a :x 9))
        (defclass a () ((x) (y :initform 2)))
        (list (slot-value *old* 'x) (slot-value *old* 'y) (slot-exists-p *old* 'removed))")))
    (is (equal '(9 30 t)
      (clos-eval vm "(defclass replacement () ((x) (z :initform 30)))
        (let ((same *old*)) (change-class *old* 'replacement)
          (list (slot-value *old* 'x) (slot-value *old* 'z) (eq same *old*)))")))
    (is (eq t (clos-eval vm "(defclass forward-child (forward-parent) ())
      (defclass forward-parent () ()) (typep (make-instance 'forward-child) 'forward-parent)")))))

(test clos-mop-extension
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "
      (defclass tracked-class (standard-class) ())
      (defmethod mop:validate-superclass ((class tracked-class) (super standard-class)) t)
      (defparameter *writes* 0)
      (defmethod (setf mop:slot-value-using-class) :around
          (value (class tracked-class) object slot)
        (setq *writes* (1+ *writes*)) (call-next-method))
      (defclass tracked () ((x :initform 4 :accessor tracked-x)) (:metaclass tracked-class))")
    (is (equal '(8 2 t t (x))
      (clos-eval vm "(let ((object (make-instance 'tracked)))
        (setf (tracked-x object) 8)
        (list (tracked-x object) *writes*
          (typep (find-class 'tracked) 'tracked-class)
          (mop:class-finalized-p (find-class 'tracked))
          (loop for slot in (mop:class-slots (find-class 'tracked)) collect (mop:slot-definition-name slot))))")))
    (is (eq t (clos-eval vm "(typep #'mop:compute-slots 'generic-function)")))
    (is (equal '(t t t)
      (clos-eval vm "(let* ((gf #'tracked-x) (method (car (mop:generic-function-methods gf))))
        (list (eq gf (mop:method-generic-function method))
              (functionp (mop:method-function method))
              (eq (car (mop:method-specializers method)) (find-class 'tracked))))")))))

(test clos-structures
  (m:with-interpreter (vm :fuel 1000000000)
    ;; POSITION also names a standard function; use a local structure symbol.
    (clos-eval vm "(shadow \"POSITION\")
      (defstruct position (x 1 :type integer) y)
      (defstruct (colored-position (:include position (x 4)) (:conc-name cp-)) (color 'blue))")
    (is (equal '(4 nil blue t t 17 4)
      (clos-eval vm "(let* ((p (make-colored-position)) (copy (copy-colored-position p)))
        (setf (cp-x copy) 17)
        (list (cp-x p) (cp-y p) (cp-color p) (position-p p) (typep p 'structure-object)
              (cp-x copy) (position-x p)))")))
    (is (eq t (clos-eval vm "(handler-case (setf (cp-x (make-colored-position)) 'bad)
                                      (type-error (c) t))")))
    (is (equal '(7 8)
      (clos-eval vm "(defstruct (pair (:constructor pair (x &optional (y 8)))) x y)
        (let ((p (pair 7))) (list (pair-x p) (pair-y p)))")))
    (is (equal '(4 2 3 0)
      (clos-eval vm "(defparameter *boa-inits* 0)
        (defstruct (record (:constructor record (x &optional y)))
          (x (setq *boa-inits* (1+ *boa-inits*))) (y 2) (z 3))
        (let ((r (record 4))) (list (record-x r) (record-y r) (record-z r) *boa-inits*))")))))

(test native-structure-storage
  (m:with-interpreter (vm)
    (clos-eval vm "(defparameter *structure-defaults* 0)
      (defstruct fast-record
        (count (progn (incf *structure-defaults*) (values 7 99)) :type integer)
        (payload (list :shared)) (fixed :fixed :read-only t))
      (defstruct (fast-child (:include fast-record (count 8))) extra)")
    (is (equal '(1 7 1)
      (clos-eval vm "(let ((results (multiple-value-list (make-fast-record))))
        (list (length results) (fast-record-count (first results)) *structure-defaults*))")))
    (is (equal '(42 1)
      (clos-eval vm "(let ((r (make-fast-record :count 42 :count 99)))
        (list (fast-record-count r) *structure-defaults*))")))
    (is (equal '(nil t t t 8 3)
      (clos-eval vm "(let* ((r (make-fast-child :extra 3)) (c (copy-fast-record r)))
        (list (eq r c) (typep c 'fast-child)
              (eq (fast-record-payload r) (fast-record-payload c))
              (eq (fast-record-fixed r) (fast-record-fixed c))
              (fast-record-count c) (fast-child-extra c)))")))
    (is (equal '(7 12 1 1)
      (clos-eval vm "(let* ((r (make-fast-record)) (c (copy-structure r)))
        (list (fast-record-count r)
              (setf (fast-record-count c) 12)
              (length (multiple-value-list (fast-record-count c)))
              (length (multiple-value-list (setf (fast-record-count c) 13)))))")))
    (is (eq t (clos-eval vm "(handler-case (fast-record-count nil) (type-error () t))")))
    (is (eq t (clos-eval vm "(handler-case (make-fast-record :count :bad) (type-error () t))")))
    (is (eq t (clos-eval vm "(handler-case (make-fast-record :unknown t) (program-error () t))")))
    (is (eq t (clos-eval vm "(handler-case (make-fast-record :count) (program-error () t))")))
    (is (eq t (clos-eval vm "(fast-record-p (make-fast-record :unknown t :allow-other-keys t))")))
    (is (eq t (clos-eval vm "(handler-case (setf (fast-record-count (make-fast-child)) :bad)
                              (type-error () t))")))
    (is (null (clos-eval vm "(fboundp '(setf fast-record-fixed))")))))

(test structure-options-and-documentation
  (m:with-interpreter (vm)
    (dolist (source
      '("(defstruct (bare-options :constructor :predicate :copier :conc-name) (bare-slot 17))
         (let* ((x (make-bare-options)) (y (copy-bare-options x)))
           (and (bare-options-p x) (bare-options-p y) (not (eq x y)) (= (bare-slot y) 17)))"
        "(defstruct (empty-options (:constructor) (:predicate) (:copier) (:conc-name)) (empty-slot 9))
         (= (empty-slot (copy-empty-options (make-empty-options))) 9)"
        "(defstruct (no-functions (:constructor nil) (:predicate nil) (:copier nil)) x)
         (not (or (fboundp 'make-no-functions) (fboundp 'no-functions-p) (fboundp 'copy-no-functions)))"
        "(defstruct (char-prefix (:conc-name #\\X)) (entry 11))
         (= (xentry (make-char-prefix)) 11)"
        "(defstruct (many-constructors :constructor (:constructor another-constructor) (:constructor by-order (x))) x)
         (and (= (many-constructors-x (make-many-constructors :x 1)) 1)
              (= (many-constructors-x (another-constructor :x 2)) 2)
              (= (many-constructors-x (by-order 3)) 3))"
        "(defstruct documented-record \"Structure documentation\" x)
         (and (equal (documentation 'documented-record 'structure) \"Structure documentation\")
              (equal (documentation 'documented-record 'type) \"Structure documentation\")
              (equal (documentation (find-class 'documented-record) t) \"Structure documentation\"))"
        "(setf (documentation 'documented-record 'structure) \"Updated\")
         (equal (documentation 'documented-record 'type) \"Updated\")"
        "(defstruct include-base (x 0 :type number))
         (defstruct (include-child (:include include-base (x 2 :type integer :read-only t))))
         (and (= (include-base-x (make-include-child)) 2)
              (not (fboundp '(setf include-child-x)))
              (fboundp '(setf include-base-x))
              (handler-case (make-include-child :x 1.5) (type-error () t))
              (= (include-base-x (make-include-base :x 1.5)) 1.5))"
        "(defstruct constant-slots (nil 1) (t 2) (:key 3))
         (let ((x (make-constant-slots :nil 11 :t 12 :key 13)))
           (and (= (constant-slots-nil x) 11) (= (constant-slots-t x) 12)
                (= (constant-slots-key x) 13) (= (setf (constant-slots-nil x) 17) 17)))"
        "(defstruct empty-slot (x nil :type nil))
         (handler-case (make-empty-slot) (type-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Structure mismatch: ~A" source))))

(test structure-accessor-identities
  (m:with-interpreter (vm)
    (clos-eval vm "(defpackage struct-slots (:use))")
    (dolist (option '(":conc-name" "(:conc-name)" "(:conc-name nil)"))
      (is (eq t (clos-eval vm (format nil
        "(defstruct (external-slot ~A) (struct-slots::entry 9))
         (let ((x (make-external-slot)))
           (and (= (struct-slots::entry x) 9)
                (= (setf (struct-slots::entry x) 12) 12)
                (= (funcall #'struct-slots::entry (copy-external-slot x)) 12)
                (not (fboundp 'entry))))" option)))))
    (is (eq t (clos-eval vm
      "(defstruct (empty-prefix-slot (:conc-name \"\")) (struct-slots::empty-entry 11))
       (and (= (empty-entry (make-empty-prefix-slot)) 11)
            (not (fboundp 'struct-slots::empty-entry)))")))
    (is (eq t (clos-eval vm
      "(defstruct (shared-parent (:conc-name shared-)) (x 1))
       (let ((reader #'shared-x) (writer (fdefinition '(setf shared-x))))
         (defstruct (shared-child (:include shared-parent) (:conc-name shared-)) y)
         (defstruct (shared-grandchild (:include shared-child) (:conc-name shared-)) z)
         (and (eq reader #'shared-x) (eq writer (fdefinition '(setf shared-x)))
              (= (shared-x (make-shared-parent)) 1)
              (= (shared-x (make-shared-child :x 2)) 2)
              (= (shared-x (make-shared-grandchild :x 3)) 3)
              (= (setf (shared-x (make-shared-parent)) 4) 4)))")))
    (dolist (source '("(defstruct duplicate-same x x)"
                      "(defstruct duplicate-package entry struct-slots::entry)"
                      "(defstruct (duplicate-inherited (:include external-slot)) entry)"))
      (is (eq t (clos-eval vm (format nil "(handler-case ~A (program-error () t))" source)))))
    (is (eq t (clos-eval vm
      "(defpackage struct-expand (:use :cl))
       (defpackage struct-execute (:use :cl))
       (let* ((*package* (find-package :struct-expand))
              (expansion (macroexpand-1 '(defstruct delayed-record (value 7)))))
         (let ((*package* (find-package :struct-execute))) (eval expansion))
         (and (fboundp 'struct-expand::make-delayed-record)
              (not (fboundp 'struct-execute::make-delayed-record))
              (= (struct-expand::delayed-record-value (struct-expand::make-delayed-record)) 7)))")))))

(test typed-structure-representations
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (representation '("list" "vector" "(vector t)"))
      (dolist (named '(nil t))
        (let* ((tag (if named ":named" "")) (start (if named 3 2))
               (listp (string= representation "list")))
          (is (eq t (clos-eval vm (format nil
            "(when (fboundp 'sequence-record-p) (fmakunbound 'sequence-record-p))
             (defstruct (sequence-record (:type ~A) ~A (:initial-offset 2))
               \"Sequence docs\" (x 7 :type integer) (y (list 'fresh)))
             (let* ((a (make-sequence-record :x 9)) (b (copy-sequence-record a)))
               (and (typep a '~A) (= (length a) ~D)
                    (= (elt a ~D) 9) (= (sequence-record-x a) 9)
                    (eq (sequence-record-y a) (sequence-record-y b)) (not (eq a b))
                    (not (eq (sequence-record-y a) (sequence-record-y (make-sequence-record))))
                    (= (setf (sequence-record-x b) 12) 12) (= (sequence-record-x a) 9)
                    (null (find-class 'sequence-record nil))
                    (equal (documentation 'sequence-record 'structure) \"Sequence docs\")
                    (null (documentation 'sequence-record 'type))
                    ~A))"
            representation tag representation (+ start 2) start
            (if named "(and (sequence-record-p a) (not (sequence-record-p nil))
                           (not (sequence-record-p '(a . b)))
                           (not (sequence-record-p #()))
                           (eq (elt a 2) 'sequence-record))"
                "(not (fboundp 'sequence-record-p))")))))
          (when named
            (is (eq t (clos-eval vm (if listp
              "(sequence-record-p '(nil nil sequence-record))"
              "(sequence-record-p #(nil nil sequence-record))")))))
          (is (eq t (clos-eval vm "(handler-case (make-sequence-record :x :bad) (type-error () t))")))
          (is (eq t (clos-eval vm "(handler-case (make-sequence-record :unknown 1) (program-error () t))"))))))
    (dolist (source
      '("(defstruct (bit-record (:type (vector bit))) (a 0 :type bit) (b 1 :type bit))
         (let ((x (make-bit-record))) (and (equal x #*01) (= (setf (bit-record-a x) 1) 1)
           (equal x #*11) (equal (copy-bit-record x) #*11)))"
        "(defstruct (char-record (:type (vector character))) (a #\\g) (b #\\k))
         (let ((x (make-char-record))) (and (equal x \"gk\")
           (char= (setf (char-record-a x) #\\a) #\\a) (equal x \"ak\")))"
        "(defstruct (list-parent (:type list) :named (:initial-offset 2)) (x 1 :type number))
         (defstruct (list-child (:type list) :named (:initial-offset 3)
                               (:include list-parent (x 2 :type integer))) (y 3))
         (let ((p (make-list-parent :x 1.5)) (c (make-list-child :y 4)))
           (and (equal c '(nil nil list-parent 2 nil nil nil list-child 4))
                (list-parent-p p) (list-parent-p c) (list-child-p c) (not (list-child-p p))
                (= (list-parent-x c) 2) (= (list-child-x c) 2)
                (= (list-parent-x p) 1.5) (= (list-child-y c) 4)
                (handler-case (make-list-child :x 1.5) (type-error () t))))"
        "(defstruct (vector-parent (:type vector) :named) (x 1))
         (defstruct (vector-child (:type (vector t)) :named (:initial-offset 2)
                                 (:include vector-parent)) y)
         (let ((c (make-vector-child :x 7 :y 8)))
           (and (equalp c #(vector-parent 7 nil nil vector-child 8))
                (vector-parent-p c) (vector-child-p c) (= (vector-parent-x c) 7)))"
        "(defstruct (boa-sequence (:type list) :named
                     (:constructor boa-sequence (&optional a &key (b 4) &aux (c (+ a b)) d)))
           (a 3) (b 99) c (d (error \"AUX default must be suppressed\")))
         (equal (boa-sequence) '(boa-sequence 3 4 7 nil))"
        "(defstruct (boa-suppression (:constructor boa-suppression (&aux a)))
           (a (error \"AUX default must be suppressed\") :type integer))
         (let ((x (boa-suppression)))
           (and (boa-suppression-p x) (= (setf (boa-suppression-a x) 9) 9)
                (= (boa-suppression-a (copy-boa-suppression x)) 9)))"
        "(let ((counter 0))
           (defstruct (lazy-sequence (:type vector))
             (x (incf counter)) (y (values (incf counter) 999)))
           (let ((a (make-lazy-sequence :x 17)))
             (and (equalp a #(17 1)) (= counter 1)
                  (equal (multiple-value-list (lazy-sequence-y a)) '(1)))))"
        "(setf (documentation 'vector-parent 'structure) \"Changed\")
         (and (equal (documentation 'vector-parent 'structure) \"Changed\")
              (null (documentation 'vector-parent 'type)))"
        "(defstruct (typed-constants (:type list)) (nil 1) (t 2) (:key 3))
         (equal (make-typed-constants :nil 11 :t 12 :key 13) '(11 12 13))"
        "(defstruct (typed-empty-slot (:type list)) (x nil :type nil))
         (handler-case (make-typed-empty-slot) (type-error () t))"
        "(every #'class-of (symbol-plist 'vector-parent))"))
      (is (eq t (clos-eval vm source)) "Typed structure mismatch: ~A" source))
    (dolist (case '(("(defstruct (bad-offset (:initial-offset 1)) x)" program-error)
                    ("(defstruct (bad-predicate (:type list) :predicate) x)" simple-error)
                    ("(defstruct (bad-named (:type (vector bit)) :named) x)" simple-error)
                    ("(defstruct (bad-include (:type vector) (:include list-parent)) x)" program-error)))
      (is (eq t (clos-eval vm (format nil "(handler-case ~A (~S () t))" (car case) (cadr case))))))))

(test structure-function-introspection
  (m:with-interpreter (vm)
    (clos-eval vm "(let ((initial 42)) (defstruct inspected-structure (value initial)))")
    (dolist (source
      '("(let ((reader #'inspected-structure-value)
               (writer (fdefinition '(setf inspected-structure-value)))
               (constructor #'make-inspected-structure))
           (every (lambda (fn)
             (multiple-value-bind (expression closure name) (function-lambda-expression fn)
               (declare (ignore name))
               (and (consp expression) (eq (car expression) 'lambda) closure
                    (not (compiled-function-p fn)) (compiled-function-p (compile nil fn)))))
             (list reader writer constructor)))"
        "(setf (documentation #'inspected-structure-value t) \"Reader documentation\")
         (setf (documentation #'make-inspected-structure t) \"Constructor documentation\")
         (let* ((x (make-inspected-structure)) (reader #'inspected-structure-value))
           (and (= (funcall reader x) 42)
                (equal (documentation reader t) \"Reader documentation\")
                (equal (documentation #'make-inspected-structure t) \"Constructor documentation\")
                (consp (function-lambda-expression reader))
                (equal (multiple-value-list (funcall reader x)) '(42))
                (= (funcall (fdefinition '(setf inspected-structure-value)) 17 x) 17)
                (= (funcall reader x) 17)))"
        "(let ((reader #'inspected-structure-value) (x (make-inspected-structure)))
           (setf (fdefinition 'inspected-structure-value) (lambda (object) (declare (ignore object)) :replacement))
           (and (eq (inspected-structure-value x) :replacement) (= (funcall reader x) 42)))"))
      (is (eq t (clos-eval vm source))))
    (dolist (form '("(funcall (fdefinition '(setf inspected-structure-value)))"
                    "(funcall (fdefinition '(setf inspected-structure-value)) 1)"
                    "(funcall (fdefinition '(setf inspected-structure-value)) 1 nil 3)"
                    "(make-inspected-structure :value)"
                    "(make-inspected-structure :unknown 1)"))
      (is (eq t (clos-eval vm (format nil "(handler-case ~A (program-error () t))" form)))))))

(test generic-function-class-changes
  (m:with-interpreter (vm)
    (is (eq t (clos-eval vm "(progn
      (defclass upgraded-generic-function (standard-generic-function)
        ((state :initform :ready :accessor upgrade-state))
        (:metaclass mop:funcallable-standard-class))
      (defgeneric class-change-target (x) (:method ((x integer)) (+ x 1)))
      (let ((saved #'class-change-target))
        (ensure-generic-function 'class-change-target :generic-function-class 'upgraded-generic-function)
        (and (eq saved #'class-change-target) (typep saved 'upgraded-generic-function)
             (eq (upgrade-state saved) :ready) (= (funcall saved 7) 8)
             (progn (ensure-generic-function 'class-change-target)
                    (typep saved 'upgraded-generic-function))
             (progn (defgeneric class-change-target (x)
                      (:generic-function-class standard-generic-function)
                      (:method ((x integer)) (+ x 2)))
                    (and (eq saved #'class-change-target) (= (funcall saved 7) 9)
                         (not (slot-exists-p saved 'state)))))))")))))

(test generic-applicable-keyword-unions
  (m:with-interpreter (vm :fuel 1000000000)
    (dolist (source
      '("(defgeneric keyword-union (x &key))
         (defmethod keyword-union ((x integer) &rest args &key foo &aux (saved args))
           (list foo saved))
         (defmethod keyword-union ((x number) &key bar) bar)
         (defmethod keyword-union ((x t) &key baz) baz)
         (equal '(17 (:foo 17 :bar 23 :baz 31))
           (keyword-union 1 :foo 17 :bar 23 :baz 31))"
        "(equal '(nil (:bar 23)) (keyword-union 1 :bar 23))"
        "(handler-case (keyword-union 'a :foo 17) (program-error () t))"
        "(handler-case (keyword-union 1 :unknown 17) (program-error () t))"
        "(handler-case (keyword-union 1 :foo) (program-error () t))"
        "(handler-case (keyword-union 1 17 23 :allow-other-keys t) (program-error () t))"
        "(equal '(nil (:unknown 17 :allow-other-keys t))
           (keyword-union 1 :unknown 17 :allow-other-keys t))"
        "(handler-case (keyword-union 1 :unknown 17 :allow-other-keys nil :allow-other-keys t)
           (program-error () t))"
        "(defgeneric explicit-allow (x &key &allow-other-keys))
         (defmethod explicit-allow ((x integer) &rest args &key foo) (list foo args))
         (equal '(nil (:unknown 17 :allow-other-keys nil))
           (explicit-allow 1 :unknown 17 :allow-other-keys nil))"
        "(defmethod keyword-union ((x number) &key bar &allow-other-keys) bar)
         (equal '(nil (:unknown 17)) (keyword-union 1 :unknown 17))"
        "(remove-method #'keyword-union (find-method #'keyword-union nil (list (find-class 'number))))
         (handler-case (keyword-union 1 :unknown 17) (program-error () t))"
        "(defgeneric rest-with-key-method (x &rest args))
         (defmethod rest-with-key-method ((x integer) &key foo) foo)
         (and (= 17 (rest-with-key-method 1 :foo 17))
           (handler-case (rest-with-key-method 1 :unknown 17) (program-error () t)))"
        "(defgeneric rest-without-key (x &rest args))
         (defmethod rest-without-key ((x t) &rest args) args)
         (equal '(17 23 31) (rest-without-key 1 17 23 31))"
        "(defgeneric before-keys (x &key))
         (defmethod before-keys ((x t) &rest args &key foo) (list foo args))
         (defmethod before-keys :before ((x integer) &key bar) (values))
         (equal '(nil (:bar 23)) (before-keys 1 :bar 23))"
        "(defgeneric optional-keys (x &optional y &key))
         (defmethod optional-keys ((x t) &optional (y 17) &key foo) (list y foo))
         (and (equal '(17 nil) (optional-keys 1))
              (equal '(23 31) (optional-keys 1 23 :foo 31)))"
        "(defgeneric key-no-applicable (x &key))
         (defmethod key-no-applicable ((x integer) &key foo) foo)
         (defmethod no-applicable-method ((gf (eql #'key-no-applicable)) &rest args)
           (values :no-method args))
         (equal '(:no-method (a :unknown 17))
           (multiple-value-list (key-no-applicable 'a :unknown 17)))"
        "(defclass keyword-initialized () ((x :initarg :x :reader keyword-initialized-x)))
         (defmethod initialize-instance :after ((object keyword-initialized) &key extra)
           (declare (ignore extra)) (values))
         (and (= 17 (keyword-initialized-x (make-instance 'keyword-initialized :x 17 :extra 23)))
              (handler-case (make-instance 'keyword-initialized :unknown 31) (program-error () t)))"
        "(every (lambda (name)
                  (let ((ll (mop:generic-function-lambda-list (fdefinition name))))
                    (and (member '&key ll) (member '&allow-other-keys ll))))
                '(allocate-instance initialize-instance make-instance reinitialize-instance shared-initialize
                  update-instance-for-different-class update-instance-for-redefined-class))"
        "(let ((ll (mop:method-lambda-list
                    (find-method #'keyword-union nil (list (find-class 'integer))))))
           (and (member '&key ll) (not (member '&allow-other-keys ll))))"))
      (is (eq t (m:evaluate-string vm source)) "Generic keyword mismatch ~A" source))))

(test slot-binding-scopes-and-class-renaming
  (m:with-interpreter (vm :fuel 1000000000)
    (dolist (source
      '("(defclass scope-object () ((x :initarg :x :accessor scope-x) (y :initarg :y :accessor scope-y)))
         (defparameter *outer-scope* (make-instance 'scope-object :x 17 :y 19))
         (defparameter *inner-scope* (make-instance 'scope-object :x 23 :y 29))
         (equal '(23 29) (with-slots (x y) *outer-scope*
                          (with-slots (x y) *inner-scope* (list x y))))"
        "(equal '(23 29) (with-accessors ((x scope-x) (y scope-y)) *outer-scope*
                          (with-accessors ((x scope-x) (y scope-y)) *inner-scope* (list x y))))"
        "(equal '(17 23) (with-slots (x) *outer-scope*
                          (with-slots ((inner x)) *inner-scope* (list x inner))))"
        "(equal '(31 17) (with-slots (x) *outer-scope* (list (let ((x 31)) x) x)))"
        "(equal '(17 19) (with-slots (x y) *outer-scope*
                          (flet ((captured () (list x y))) (captured))))"
        "(equal '(37 17) (with-slots (x) *outer-scope*
                          (list (funcall (lambda (x) x) 37) (funcall (lambda (&optional (a x)) a)))))"
        "(equal '(17 t) (with-slots (x) *outer-scope*
                          (macrolet ((read-place (&environment env)
                                       (multiple-value-bind (form expanded) (macroexpand 'x env)
                                         (list 'list form expanded))))
                            (read-place))))"
        "(let ((calls 0))
           (and (= 17 (with-slots (x) (progn (incf calls) *outer-scope*) (declare (ignorable x)) x))
                (= calls 1)))"
        "(progn (with-slots (x y) *outer-scope* (setq x 41 y 43))
                (equal '(41 43) (list (scope-x *outer-scope*) (scope-y *outer-scope*))))"
        "(equal '(41 43) (multiple-value-list (with-slots (x y) *outer-scope* (values x y))))"
        "(let ((reader (with-accessors ((x scope-x)) *outer-scope* (lambda () x))))
           (with-slots (x) *outer-scope* (setq x 47)) (= 47 (funcall reader)))"
        "(and (typep #'(setf class-name) 'standard-generic-function)
              (= 1 (length (multiple-value-list (setf (class-name (find-class 'scope-object)) 'scope-renamed)))))"
        "(defparameter *renamed-class* (find-class 'scope-object))
         (defclass scope-object () ((x :initform 53)))
         (and (not (eq *renamed-class* (find-class 'scope-object)))
              (eq 'scope-renamed (class-name *renamed-class*))
              (typep *outer-scope* *renamed-class*) (not (typep *outer-scope* 'scope-object)))"
        "(defclass anonymous-replaced () ((x)))
         (defparameter *anonymous-class* (find-class 'anonymous-replaced))
         (setf (class-name *anonymous-class*) nil)
         (defclass anonymous-replaced () ((y)))
         (and (null (class-name *anonymous-class*))
              (not (eq *anonymous-class* (find-class 'anonymous-replaced))))"
        "(defmethod eql-method-lookup ((object (eql 12345678901234567890))) :large)
         (defmethod eql-method-lookup ((object (eql nil))) :nil)
         (defmethod eql-method-lookup ((object t)) :default)
         (eq (find-method #'eql-method-lookup nil '((eql 12345678901234567890)))
             (find-method #'eql-method-lookup nil
               (list (mop:intern-eql-specializer (+ 12345678901234567889 1)))))"
        "(eq (find-method #'eql-method-lookup nil '((eql nil)))
             (find-method #'eql-method-lookup nil (list (mop:intern-eql-specializer nil))))"
        "(and (null (find-method #'eql-method-lookup nil '((eql missing-object)) nil))
              (eq :default (eql-method-lookup 'missing-object)))"
        "(= 1 (length (multiple-value-list (find-method #'eql-method-lookup nil (list (find-class t))))))"))
      (is (eq t (m:evaluate-string vm source)) "Slot scope/class identity ~A" source))))

(test generic-initialization-and-accessor-arguments
  (m:with-interpreter (vm :fuel 1000000000)
    (dolist (source
      '("(defclass argument-checked ()
           ((x :initarg :x :initform 7 :accessor argument-checked-x :writer write-argument-checked-x)))
         (defparameter *argument-checked* (make-instance 'argument-checked :x 17))
         (= 17 (argument-checked-x *argument-checked*))"
        "(handler-case (argument-checked-x *argument-checked* 23) (program-error () t))"
        "(handler-case (write-argument-checked-x 23 *argument-checked* 31) (program-error () t))"
        "(handler-case (setf (argument-checked-x *argument-checked* 31) 23) (program-error () t))"
        "(= 17 (argument-checked-x *argument-checked*))"
        "(handler-case (ensure-generic-function (gensym) :lambda-list) (program-error () t))"
        "(handler-case (ensure-generic-function (gensym) 17 23) (program-error () t))"
        "(handler-case (shared-initialize *argument-checked* nil :x) (program-error () t))"
        "(handler-case (shared-initialize *argument-checked* nil '(x) 23) (program-error () t))"
        "(defmethod reinitialize-instance :after ((object argument-checked) &key (extra nil supplied))
           (when supplied (setf (argument-checked-x object) extra)))
         (and (eq *argument-checked* (reinitialize-instance *argument-checked* :x 23))
              (= 23 (argument-checked-x *argument-checked*)))"
        "(and (eq *argument-checked* (reinitialize-instance *argument-checked* :extra 31))
              (= 31 (argument-checked-x *argument-checked*)))"
        "(progn (reinitialize-instance *argument-checked* :extra 37 :extra 41)
                (= 37 (argument-checked-x *argument-checked*)))"
        "(handler-case (make-instance 'argument-checked :extra 23) (program-error () t))"
        "(handler-case (reinitialize-instance *argument-checked* :unknown 23) (program-error () t))"
        "(defmethod shared-initialize :after ((object argument-checked) (slots (eql nil)) &key shared-extra)
           (when shared-extra (setf (argument-checked-x object) shared-extra)))
         (progn (reinitialize-instance *argument-checked* :shared-extra 43)
                (= 43 (argument-checked-x *argument-checked*)))"
        "(handler-case (make-instance 'argument-checked :shared-extra 23) (program-error () t))"
        "(defmethod initialize-instance :after ((object argument-checked) &key init-extra &aux auxiliary)
           (declare (ignore init-extra auxiliary)) (values))
         (handler-case (make-instance 'argument-checked :auxiliary 23) (program-error () t))"
        "(handler-case (progn (defmethod bad-next-method ((object t)) (next-method-p nil))
                              (bad-next-method nil)) (program-error () t))"
        "(defmethod retained-next-method ((object t)) #'next-method-p)
         (let ((function (retained-next-method nil)))
           (and (null (funcall function))
                (handler-case (funcall function nil) (program-error () t))))"))
      (is (eq t (m:evaluate-string vm source)) "Initialization/accessor arguments ~A" source))))

(test generic-definition-method-ownership
  (m:with-interpreter (vm)
    (dolist (source
      '("(defgeneric replace-inline (x) (:method ((x symbol)) :old))
         (defparameter *inline-identity* #'replace-inline)
         (defgeneric replace-inline (x) (:method ((x t)) :new))
         (and (eq *inline-identity* #'replace-inline) (eq :new (replace-inline 'a)))"
        "(defgeneric replace-arity (x) (:method ((x t)) x))
         (defgeneric replace-arity (x y) (:method ((x t) (y t)) (list x y)))
         (equal '(17 23) (replace-arity 17 23))"
        "(defgeneric preserve-external (x) (:method ((x symbol)) :inline))
         (defmethod preserve-external ((x integer)) :external)
         (defgeneric preserve-external (x) (:method ((x t)) :replacement))
         (and (eq :external (preserve-external 1)) (eq :replacement (preserve-external 'a)))"
        "(defgeneric replaced-origin (x) (:method ((x symbol)) :inline))
         (defmethod replaced-origin ((x symbol)) :external)
         (defgeneric replaced-origin (x))
         (eq :external (replaced-origin 'a))"
        "(defgeneric removed-inline (x) (:method ((x symbol)) :inline))
         (defparameter *removed-inline-method* (find-method #'removed-inline nil (list (find-class 'symbol))))
         (defgeneric removed-inline (x))
         (and (null (find-method #'removed-inline nil (list (find-class 'symbol)) nil))
              (null (mop:method-generic-function *removed-inline-method*)))"
        "(defgeneric retained-constraint (x &optional y))
         (defmethod retained-constraint ((x t) &optional y) (list x y))
         (handler-case (defgeneric retained-constraint (x)) (program-error () t))"
        "(defgeneric reset-order (x y) (:argument-precedence-order y x))
         (defgeneric reset-order (a b c))
         (equal '(a b c) (mop:generic-function-argument-precedence-order #'reset-order))"))
      (is (eq t (m:evaluate-string vm source)) "Generic method ownership mismatch ~A" source))))

(test slot-exception-method-dispatch
  (m:with-interpreter (vm)
    (m:evaluate-string vm
      "(defclass slot-protocol () ((x :reader protocol-x) (empty :reader protocol-empty)))
       (defparameter *slot-exception-arguments* nil)
       (defmethod slot-missing ((class t) (object slot-protocol) name operation
                               &optional (value nil supplied))
         (setq *slot-exception-arguments* (list name operation value supplied))
         (values 17 23))
       (defmethod slot-unbound ((class t) (object slot-protocol) name)
         (setq *slot-exception-arguments* (list (class-name class) name))
         (if (eq name 'empty) (values) (values 31 37)))")
    (dolist (source
      '("(equal '(17) (multiple-value-list (slot-value (make-instance 'slot-protocol) 'missing)))"
        "(equal '(missing slot-value nil nil) *slot-exception-arguments*)"
        "(equal '(31) (multiple-value-list (slot-value (make-instance 'slot-protocol) 'x)))"
        "(equal '(slot-protocol x) *slot-exception-arguments*)"
        "(equal '(31) (multiple-value-list (protocol-x (make-instance 'slot-protocol))))"
        "(equal '(nil) (multiple-value-list (protocol-empty (make-instance 'slot-protocol))))"
        "(equal '(t) (multiple-value-list (slot-boundp (make-instance 'slot-protocol) 'missing)))"
        "(equal '(missing slot-boundp nil nil) *slot-exception-arguments*)"
        "(let ((object (make-instance 'slot-protocol)))
           (equal (list object) (multiple-value-list (slot-makunbound object 'missing))))"
        "(equal '(missing slot-makunbound nil nil) *slot-exception-arguments*)"
        "(equal '(41) (multiple-value-list (setf (slot-value (make-instance 'slot-protocol) 'missing) 41)))"
        "(equal '(missing setf 41 t) *slot-exception-arguments*)"
        "(handler-case (slot-value (make-instance 'standard-object) 'missing) (error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Slot exception protocol mismatch ~A" source))))

(test generic-declarations-and-documentation
  (m:with-interpreter (vm)
    (dolist (source
      '("(defgeneric documented-generic (x)
           (declare (optimize (safety 3))) (:documentation \"generic documentation\")
           (:method ((x integer))
             (declare (optimize (speed 0))) \"method documentation\"
             (declare (optimize (safety 3))) (values x 17))
           (declare (optimize (debug 3))))
         (equal '(23 17) (multiple-value-list (documented-generic 23)))"
        "(equal \"generic documentation\" (documentation 'documented-generic 'function))"
        "(equal \"generic documentation\" (documentation #'documented-generic t))"
        "(let ((method (find-method #'documented-generic nil (list (find-class 'integer)))))
           (and (equal \"method documentation\" (documentation method t))
                (equal \"changed\" (setf (documentation method t) \"changed\"))
                (equal \"changed\" (documentation method t))))"
        "(defmethod separate-documentation ((x symbol))
           (declare (optimize (speed 0))) \"separate method\"
           (declare (ignore x)) :good)
         (and (eq :good (separate-documentation 'a))
              (equal \"separate method\"
                (documentation (find-method #'separate-documentation nil (list (find-class 'symbol))) t)))"
        "(handler-case
           (defgeneric repeated-generic-doc () (:documentation \"a\") (:documentation \"b\"))
           (program-error () t))"
        "(handler-case
           (defgeneric repeated-method-class () (:method-class standard-method) (:method-class standard-method))
           (program-error () t))"
        "(eq #'documented-generic
           (ensure-generic-function 'documented-generic :documentation \"replacement generic documentation\"))"
        "(equal \"replacement generic documentation\" (documentation 'documented-generic 'function))"))
      (is (eq t (m:evaluate-string vm source)) "Generic declaration mismatch ~A" source))))

(test class-definition-option-errors
  (m:with-interpreter (vm)
    (dolist (option '(:initform :type :documentation :allocation))
      (dolist (value (case option (:initform '(nil 17)) (:type '(t integer))
                       (:documentation '("" "slot documentation")) (:allocation '(:instance :class))))
        (dolist (operation '(eval macroexpand-1))
          (is (eq t (m:evaluate vm
            `(handler-case
               (,operation '(defclass invalid-slot-option () ((x ,option ,value ,option ,value))))
               (program-error () t))))))))
    (dolist (source
      '("(handler-case (defclass duplicate-default () () (:default-initargs :x 1 :x 2))
           (program-error () t))"
        "(handler-case (defclass duplicate-nil-default () () (:default-initargs nil 1 nil 2))
           (program-error () t))"
        "(handler-case (defclass duplicate-documentation () () (:documentation \"a\") (:documentation \"b\"))
           (program-error () t))"
        "(handler-case (defclass duplicate-metaclass () () (:metaclass standard-class) (:metaclass standard-class))
           (program-error () t))"
        "(handler-case (defclass duplicate-default-option () () (:default-initargs :x 1) (:default-initargs :y 2))
           (program-error () t))"
        "(let ((counter 0))
           (defclass option-preserved () ((x :initform 17 :reader option-preserved-x)))
           (let ((class (find-class 'option-preserved)))
             (and (handler-case
               (eval '(defclass option-preserved () ((x :initform (incf counter) :initform 2))))
               (program-error () t))
               (eq class (find-class 'option-preserved))
               (= (option-preserved-x (make-instance 'option-preserved)) 17) (zerop counter))))"
        "(defclass repeated-readers ()
           ((x :initarg :x :initarg :y :initform 17
               :reader repeated-reader-a :reader repeated-reader-b
               :accessor repeated-accessor-a :accessor repeated-accessor-b)))
         (let ((object (make-instance 'repeated-readers :y 23)))
           (and (= (repeated-reader-a object) 23) (= (repeated-reader-b object) 23)
                (= (setf (repeated-accessor-a object) 31) 31)
                (= (repeated-accessor-b object) 31)))"
        "(defclass symbol-default-initarg () ((x :initarg ordinary-initarg :reader symbol-default-x))
           (:default-initargs ordinary-initarg 17))
         (= (symbol-default-x (make-instance 'symbol-default-initarg)) 17)"))
      (is (eq t (m:evaluate-string vm source)) "Class option mismatch ~A" source))))

(test standard-system-class-specializers
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm
      "(typep (find-class 'structure-object) 'structure-class)")))
    (is (eq nil (m:evaluate-string vm
      "(typep (find-class 'structure-object) 'standard-class)")))
    (is (equal '(structure-object t) (m:evaluate-string vm
      "(mapcar #'class-name (mop:class-precedence-list (find-class 'structure-object)))")))
    (dolist (entry '((broadcast-stream stream) (concatenated-stream stream) (echo-stream stream)
                     (file-stream stream) (logical-pathname pathname)))
      (destructuring-bind (name parent) entry
        (is (eq t (clos-eval vm (format nil
          "(let ((class (find-class '~A)))
             (and (eq (class-of class) (find-class 'built-in-class))
                  (equal (mapcar #'class-name (mop:class-precedence-list class)) '(~A ~A t))
                  (equal (multiple-value-list (subtypep '~A '~A)) '(t t))
                  (equal (multiple-value-list (subtypep '~A '~A)) '(nil t))))"
          name name parent name parent parent name))))
        (is (null (m:evaluate vm `(typep 17 ',name))))
        (is (null (m:evaluate vm `(typep (make-string-output-stream) ',name))))))
    (clos-eval vm "(defgeneric system-class-list (object) (:method-combination list))")
    (dolist (name '(t array broadcast-stream concatenated-stream echo-stream file-stream stream string-stream
                    pathname logical-pathname condition error type-error arithmetic-error number integer))
      (is (eq t (clos-eval vm (format nil
        "(typep (defmethod system-class-list list ((object ~A)) '~A) 'method)" name name)))))
    (is (equal '(string-stream stream t) (clos-eval vm "(system-class-list (make-string-output-stream))")))
    (is (equal '(integer number t) (clos-eval vm "(system-class-list 17)")))
    (is (equal '(type-error error condition t) (clos-eval vm "(system-class-list (make-condition 'type-error))")))))

(test clos-lambda-lists
  (m:with-interpreter (vm)
    (is (equal '(1 2 nil nil 3 nil 6)
      (clos-eval vm "(funcall (lambda (x &optional (y 2 yp) &rest rest &key (z 3 zp) &aux (sum (+ x y z)))
        (list x y yp rest z zp sum)) 1)")))
    (is (equal '(1 4 t (:z 5) 5 t 10)
      (clos-eval vm "(funcall (lambda (x &optional (y 2 yp) &rest rest &key (z 3 zp) &aux (sum (+ x y z)))
        (list x y yp rest z zp sum)) 1 4 :z 5)")))
    (is (= 9 (clos-eval vm "(funcall (lambda (&key ((:external local) 2)) local) :external 9)")))
    (is (eq t (clos-eval vm "(handler-case (funcall (lambda (&key x) x) :bad 1) (program-error (c) t))")))))

(test clos-protocol-edge-cases
  (m:with-interpreter (vm :fuel 1000000000)
    (is (= 9 (clos-eval vm "(defclass empty () ()) (defparameter *empty* (make-instance 'empty))
      (defclass empty () ((new :initform 9))) (slot-value *empty* 'new)")))
    (is (equal '(3 7 3 3)
      (clos-eval vm "(defclass scoped () ((x :initform 3)))
        (let ((object (make-instance 'scoped)))
          (with-slots (x) object
            (list x (let ((x 7)) x) (funcall (lambda () x))
                  (funcall (lambda (&optional (y x)) y)))))")))
    (is (= 12 (clos-eval vm "(defmethod retained ((x number)) 12)
      (defmethod retained ((x integer)) #'call-next-method)
      (funcall (retained 3))")))
    (is (= 5 (clos-eval vm "(defmethod declared ((x integer) &key (y 2))
      (declare (type integer x y)) (+ x y)) (declared 3)")))
    (is (string= "value=3" (clos-eval vm "(defmethod print-object ((object scoped) stream)
      (format stream \"value=~A\" (slot-value object 'x)))
      (format nil \"~S\" (make-instance 'scoped :allow-other-keys t))")))
    (is (= 15 (clos-eval vm "(define-method-combination add-values :operator + :identity-with-one-argument t)
      (defgeneric added (x) (:method-combination add-values))
      (defmethod added add-values ((x number)) 10)
      (defmethod added add-values ((x integer)) 5) (added 1)")))
    (is (equal '(t 9 7)
      (clos-eval vm "(defmethod callable-rank ((object function)) 7)
        (defclass callable () ((x :initform 9)) (:metaclass mop:funcallable-standard-class))
        (let ((object (make-instance 'callable)))
          (mop:set-funcallable-instance-function object (lambda (n) (+ n (slot-value object 'x))))
          (list (functionp object) (funcall object 0) (callable-rank object)))")))
    (is (= 23 (clos-eval vm "(define-condition object-error (error) ((value :initarg :value)))
      (defmethod condition-value ((c object-error)) (slot-value c 'value))
      (condition-value (make-condition 'object-error :value 23))")))
    (is (= 31 (clos-eval vm "(defgeneric constructed (x))
      (add-method #'constructed (make-instance 'standard-method :lambda-list '(x)
        :specializers (list (find-class 'integer)) :qualifiers nil
        :function (lambda (args next) 31))) (constructed 1)")))
    (is (eq t (clos-eval vm "(handler-case (defmethod constructed ((x integer) y) y)
      (program-error (c) t))")))))

(test clos-mop-metaobjects
  (m:with-interpreter (vm :fuel 1000000000)
    (clos-eval vm "
      (defclass annotated-class (standard-class) ())
      (defclass annotated-slot (mop:standard-direct-slot-definition) ((mark :initform 19)))
      (defmethod mop:direct-slot-definition-class ((class annotated-class) &rest args)
        (find-class 'annotated-slot))
      (defclass annotated () ((x :initform 1)) (:metaclass annotated-class))
      (defclass annotated-method (standard-method) ((mark :initform 29)))
      (defclass annotated-generic (standard-generic-function) ((mark :initform 39)))
      (defgeneric annotated-call (x) (:generic-function-class annotated-generic)
        (:method-class annotated-method))
      (defmethod annotated-call ((x integer)) 5)")
    (is (= 19 (clos-eval vm "(slot-value (car (mop:class-direct-slots (find-class 'annotated))) 'mark)")))
    (is (equal '(39 29 5)
      (clos-eval vm "(list (slot-value #'annotated-call 'mark)
        (slot-value (car (mop:generic-function-methods #'annotated-call)) 'mark)
        (annotated-call 1))")))
    (is (equal '(1 t)
      (clos-eval vm "(multiple-value-bind (methods valid)
          (mop:compute-applicable-methods-using-classes #'annotated-call (list (find-class 'integer)))
        (list (length methods) valid))")))
    (is (= 6 (clos-eval vm "(defmethod mop:compute-effective-method :around
        ((gf annotated-generic) combination methods)
        (list '+ 1 (call-next-method))) (annotated-call 1)")))
    (is (= 41 (clos-eval vm "(defmethod mop:compute-effective-method
        ((gf annotated-generic) combination methods)
        (list 'call-method (car methods) nil))
        (defmethod annotated-call ((x integer)) 40) (annotated-call 1)")))
    (is (eq t (clos-eval vm "(let ((class (find-class 'annotated)))
      (setf (symbol-plist 'annotated) nil) (eq class (find-class 'annotated)))")))))

(test precise-type-of-and-obsolescence
  (m:with-interpreter (vm)
    (is (eq t (clos-eval vm "(and (eq (type-of t) 'boolean)
      (eq (type-of :precise-type) 'keyword)
      (every (lambda (x) (and (typep x (type-of x))
        (subtypep (type-of x) (class-of x))
        (subtypep (type-of x) (if (minusp x) 'signed-byte 'unsigned-byte))))
        (list 0 1 -1 536870912 -536870913 (expt 2 90))))")))
    (is (eq t (clos-eval vm "(let* ((class (defclass unnamed-type-example () ()))
      (object (make-instance class))) (setf (class-name class) nil)
      (and (eq (type-of object) class) (typep object (type-of object))))")))
    (is (eq t (clos-eval vm "(let* ((class (defclass forgotten-type-example () ()))
      (object (make-instance class))) (setf (find-class 'forgotten-type-example) nil)
      (and (eq (type-of object) class) (typep object (type-of object))))")))
    (is (eq t (clos-eval vm "(progn
      (defclass obsolete-example () ((value :initarg :value)))
      (defvar *obsolete-visits* nil)
      (defmethod make-instances-obsolete :before ((class standard-class))
        (when (eq (class-name class) 'obsolete-example) (push class *obsolete-visits*)))
      (let* ((class (find-class 'obsolete-example)) (object (make-instance class :value 19)))
        (setq *obsolete-visits* nil)
        (and (typep #'make-instances-obsolete 'standard-generic-function)
          (find-method #'make-instances-obsolete nil (list (find-class 'symbol)))
          (find-method #'make-instances-obsolete nil (list (find-class 'standard-class)))
          (eq (make-instances-obsolete 'obsolete-example) class)
          (equal *obsolete-visits* (list class)) (= (slot-value object 'value) 19))))")))))

(test numeric-and-declaration-type-errors
  (m:with-interpreter (vm)
    (is (eq t (clos-eval vm "(every (lambda (bad)
      (handler-case (progn (gcd 1 bad) nil)
        (type-error (c) (and (eql (type-error-datum c) bad)
          (equal (type-error-expected-type c) 'integer)))))
      (list nil 'x 1.5 2/3 #\\A (list 1) (make-hash-table)))")))
    (is (eq t (clos-eval vm "(every (lambda (type)
      (handler-case (progn (typep nil type) nil) (error () t)))
      '(values (values) (values integer) (function) (function nil t)))")))
    (is (eq t (clos-eval vm "(and (typep #'identity 'function)
      (eq (the (function (t) t) #'identity) #'identity)
      (= (funcall (the (function (integer) integer) (lambda (x) (1+ x))) 4) 5)
      (handler-case (progn (the (function (t) t) 17) nil) (type-error () t)))")))))

(test native-bignum-interval-subtypes
  (m:with-interpreter (vm)
    (dolist (lower '(* -9223372036854775810 -9223372036854775809 -9223372036854775808 -1 0 9223372036854775807 9223372036854775808 9223372036854775809))
      (dolist (upper '(* -9223372036854775810 -9223372036854775809 -9223372036854775808 -1 0 9223372036854775807 9223372036854775808 9223372036854775809))
        (let* ((expected (or (and (integerp lower) (integerp upper) (> lower upper))
                             (and (integerp upper) (< upper -9223372036854775808))
                             (and (integerp lower) (> lower 9223372036854775807))))
               (source (format nil "(and (equal (multiple-value-list (subtypep '(integer ~S ~S) 'bignum)) '(~S t))
                 (equal (multiple-value-list (subtypep '(integer ~S ~S) (find-class 'bignum))) '(~S t)))"
                 lower upper expected lower upper expected)))
          (is (eq t (clos-eval vm source))))))))

(test native-metadata-method-bodies
  (m:with-interpreter (vm)
    (clos-eval vm "(defclass metadata-native-example () ((x :initform 17 :accessor metadata-native-x)))
      (defgeneric metadata-native-operation (x))
      (defmethod metadata-native-operation ((x integer)) (1+ x))")
    (is (eq t (clos-eval vm "(let* ((class (find-class 'metadata-native-example))
      (slot (car (mop:class-slots class))) (gf #'metadata-native-operation)
      (method (car (mop:generic-function-methods gf))))
      (every (lambda (entry)
        (let* ((getter (car entry)) (object (cadr entry))
          (function (mop:method-function (car (compute-applicable-methods getter (list object))))))
          (and (equal (multiple-value-list (funcall getter object))
                      (multiple-value-list (funcall function (list object) nil)))
               (function-lambda-expression function) (not (compiled-function-p function)))))
        (append
          (mapcar (lambda (getter) (list getter class))
            (list #'class-name #'mop:class-direct-superclasses #'mop:class-direct-subclasses
                  #'mop:class-direct-slots #'mop:class-finalized-p))
          (mapcar (lambda (getter) (list getter slot))
            (list #'mop:slot-definition-name #'mop:slot-definition-allocation
                  #'mop:slot-definition-initargs #'mop:slot-definition-initform
                  #'mop:slot-definition-initfunction #'mop:slot-definition-type
                  #'mop:slot-definition-readers #'mop:slot-definition-writers
                  #'mop:slot-definition-location))
          (mapcar (lambda (getter) (list getter gf))
            (list #'mop:generic-function-name #'mop:generic-function-lambda-list
                  #'mop:generic-function-methods #'mop:generic-function-argument-precedence-order
                  #'mop:generic-function-method-class #'mop:generic-function-method-combination))
          (mapcar (lambda (getter) (list getter method))
            (list #'mop:method-function #'mop:method-generic-function #'mop:method-specializers
                  #'mop:method-lambda-list #'method-qualifiers)))))")))
    (is (eq t (clos-eval vm "(let* ((class (find-class 'metadata-native-example))
      (function (mop:method-function (car (compute-applicable-methods #'class-name (list class))))))
      (and (every (lambda (args) (handler-case (progn (apply function args) nil)
                    (program-error () t))) (list nil (list nil) (list nil nil nil)))
           (every (lambda (args) (handler-case (progn (funcall function args nil) nil)
                    (program-error () t))) (list nil (list class class)))))")))
    (is (eq t (clos-eval vm "(progn
      (defparameter *metadata-native-visits* nil)
      (defmethod mop:slot-definition-name :around ((slot mop:slot-definition))
        (push :name *metadata-native-visits*) (call-next-method))
      (let ((slot (car (mop:class-slots (find-class 'metadata-native-example)))))
        (setq *metadata-native-visits* nil)
        (and (eq (mop:slot-definition-name slot) 'x)
             (equal *metadata-native-visits* '(:name)))))")))
    (is (eq t (clos-eval vm "(progn
      (defmethod mop:class-slots :around ((class standard-class))
        (push :slots *metadata-native-visits*) (call-next-method))
      (let ((object (make-instance 'metadata-native-example)))
        (setq *metadata-native-visits* nil)
        (and (= (metadata-native-x object) 17)
             (>= (count :slots *metadata-native-visits*) 2))))")))))

(test precise-array-and-character-type-of
  (m:with-interpreter (vm)
    (is (eq t (clos-eval vm "(let ((objects
      (list #\\Space #\\Newline #\\A #\\λ #() #* #*101 \"abc\"
        (make-array 0 :element-type nil) (make-array '(0 2))
        (make-array nil :initial-element 17)
        (make-array 3 :adjustable t :fill-pointer 1)
        (make-array 4 :element-type 'character :initial-element #\\A :fill-pointer 2)
        (make-array 3 :element-type 'bit :initial-element 1 :adjustable t)
        (make-array 2 :displaced-to (vector 1 2 3) :displaced-index-offset 1))))
      (every (lambda (object)
        (let ((type (type-of object)))
          (and (typep object type) (subtypep type (class-of object))
            (every (lambda (builtin) (or (not (typep object builtin)) (subtypep type builtin)))
              '(t atom character base-char standard-char array simple-array vector
                simple-vector string simple-string base-string simple-base-string
                bit-vector simple-bit-vector))))) objects))")))
    (is (eq t (clos-eval vm "(let* ((object (make-array '(2 3))) (type (type-of object)))
      (and (equal type '(simple-array t (2 3)))
        (progn (rplaca (third type) 17) (equal (array-dimensions object) '(2 3)))
        (equal (type-of object) '(simple-array t (2 3)))))")))))

(test empty-and-removed-instance-layouts
  (m:with-interpreter (vm)
    (clos-eval vm "(defclass empty-layout-example () ())
      (defvar *empty-layout-updates* nil)
      (defmethod update-instance-for-redefined-class :after
        ((object empty-layout-example) added discarded values &rest initargs)
        (declare (ignore initargs))
        (push (list added discarded values) *empty-layout-updates*))
      (defparameter *empty-layout-object* (make-instance 'empty-layout-example))")
    (is (eq t (clos-eval vm "(progn
      (make-instances-obsolete 'empty-layout-example)
      (and (null (slot-exists-p *empty-layout-object* 'missing))
           (equal *empty-layout-updates* '((nil nil nil)))
           (null (slot-exists-p *empty-layout-object* 'missing))
           (= (length *empty-layout-updates*) 1)))")))
    (is (eq t (clos-eval vm "(progn
      (make-instances-obsolete (find-class 'empty-layout-example))
      (handler-case (slot-value *empty-layout-object* 'missing) (error () nil))
      (= (length *empty-layout-updates*) 2))")))
    (is (eq t (clos-eval vm "(progn
      (defclass empty-layout-example () ((value :initform 17)))
      (and (= (slot-value *empty-layout-object* 'value) 17)
        (equal (car *empty-layout-updates*) '((value) nil nil))))")))
    (is (eq t (clos-eval vm "(progn
      (defclass empty-layout-example () ())
      (handler-case (slot-value *empty-layout-object* 'value) (error () nil))
      (and (equal (car *empty-layout-updates*) '(nil (value) (value 17)))
        (= (length *empty-layout-updates*) 4)))")))
    (is (eq t (clos-eval vm "(progn
      (make-instances-obsolete 'empty-layout-example)
      (handler-case (setf (slot-value *empty-layout-object* 'missing) 19) (error () nil))
      (= (length *empty-layout-updates*) 5))")))))
