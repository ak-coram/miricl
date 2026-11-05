;;;; Conditions execute in the guest, including all handlers and restarts.
(in-package #:miricl-test)
(in-suite :miricl)

(test lightweight-error-backtrace
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defun trace-host-leaf () (error \"host trace\"))
      (defun trace-host-root () (trace-host-leaf))")
    (dolist (form '("(trace-host-root)" (trace-host-root)))
      (handler-case
          (progn (if (stringp form) (m:evaluate-string vm form) (m:evaluate vm form))
                 (fail "Expected a guest error."))
        (m:interpreter-error (condition)
          (is (string= "host trace" (m::interpreter-error-message condition)))
          (is (search "COMMON-LISP-USER::TRACE-HOST-LEAF" (m:interpreter-error-backtrace condition)))
          (is (search "COMMON-LISP-USER::TRACE-HOST-ROOT" (m:interpreter-error-backtrace condition)))
          (is (search "Backtrace" (princ-to-string condition))))))
    (is (= 42 (m:evaluate vm '(+ 20 22))))))

(test lexical-tagbody-control
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((multiple-value-list (tagbody))
        (multiple-value-list (tagbody (values 1 2)))
        (let ((x 0)) (tagbody a (incf x) (when (< x 10) (go a))) x)
        (let ((x 0)) (tagbody (go 9) (setq x 99) 9 (incf x)) x)
        (let ((x 0)) (tagbody nil (incf x) (when (< x 3) (go nil))) x)
        (let ((x 0))
          (tagbody (flet ((jump () (go done))) (jump)) (setq x 99) done) x)
        (let ((x 0))
          (tagbody start (incf x)
            (tagbody start (incf x) (go end) (setq x 99) end)
            (when (< x 4) (go start))) x)
        (let ((x 0) (seen nil))
          (tagbody start
            (unwind-protect (progn (incf x) (when (< x 3) (go start))) (push x seen)))
          (reverse seen))
        (macrolet ((statement () 'x))
          (let ((x 9)) (tagbody (statement)) x))
        (let ((name 'outer) (x 0))
          (tagbody outer (incf x) (when (< x 3) (go outer))) (list name x))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (is (eq t (m:evaluate vm
      '(let ((escape nil))
         (tagbody done (setq escape (lambda () (go done))))
         (handler-case (funcall escape) (control-error () t))))))
    (is (equal '(t t) (m:evaluate vm
      '(list (special-operator-p 'tagbody) (special-operator-p 'go)))))))

(test condition-classes-and-slots
  (m:with-interpreter (vm)
    (is (equal '(t t t nil)
      (m:evaluate vm '(let ((c (make-condition 'simple-type-error :datum 7 :expected-type 'string)))
        (list (typep c 'condition) (typep c 'error) (typep c 'simple-condition) (typep c 'warning))))))
    (is (equal '(7 string "bad ~A" (7))
      (m:evaluate vm '(let ((c (make-condition 'simple-type-error :datum 7 :expected-type 'string
                                              :format-control "bad ~A" :format-arguments '(7))))
        (list (type-error-datum c) (type-error-expected-type c)
              (simple-condition-format-control c) (simple-condition-format-arguments c))))))
    (m:evaluate-string vm "(define-condition parent-condition (error)
        ((detail :initarg :detail :initform 10 :reader condition-detail)))
      (define-condition child-condition (parent-condition simple-condition)
        ((code :initarg :code :accessor condition-code))
        (:default-initargs :detail 11) (:report \"Child failed.\"))")
    (is (equal '(t t 11 8)
      (m:evaluate-string vm "(let ((c (make-condition 'child-condition :code 7)))
        (setf (condition-code c) 8)
        (list (typep c 'parent-condition) (typep c 'simple-condition)
              (condition-detail c) (condition-code c)))")))
    (m:evaluate-string vm "(let ((default 29))
      (define-condition lexical-condition () ((x :initform default :reader lexical-detail))))")
    (is (= 29 (m:evaluate-string vm "(lexical-detail (make-condition 'lexical-condition))")))
    (m:evaluate-string vm "(define-condition inherited-condition (parent-condition) ((detail)))
      (define-condition overridden-condition (parent-condition) ((detail :initform 20)))")
    (is (= 10 (m:evaluate-string vm "(condition-detail (make-condition 'inherited-condition))")))
    (is (= 20 (m:evaluate-string vm "(condition-detail (make-condition 'overridden-condition))")))
    (is (= 30 (m:evaluate-string vm "(condition-detail (make-condition 'overridden-condition :detail 30))")))
    (let ((c (m:evaluate vm '(make-condition 'simple-error :format-control "round trip"))))
      (is (m:guest-condition-p c))
      (is (eq c (m:evaluate vm c)))
      (is (string= "round trip" (m:evaluate vm `(simple-condition-format-control ,c)))))
    (signals m:interpreter-error (m:evaluate-string vm "(define-condition cycle (cycle) ())"))))

(test undefined-function-reporting
  (m:with-interpreter (vm)
    (is (equal "Undefined function: MISSING-REPORT-FUNCTION."
      (m:evaluate vm '(handler-case (missing-report-function)
                        (undefined-function (c) (princ-to-string c))))))
    (is (equal "Undefined function: (SETF MISSING-REPORT-SETTER)."
      (m:evaluate vm '(princ-to-string (make-condition 'undefined-function :name '(setf missing-report-setter))))))
    (is (equal "custom report" (m:evaluate vm '(progn
      (define-condition reported-undefined-function (undefined-function) () (:report "custom report"))
      (princ-to-string (make-condition 'reported-undefined-function :name 'missing))))))))

(test condition-handlers
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate vm '(handler-case (/ 1 0) (division-by-zero (c) (typep c 'arithmetic-error))))))
    (is (equal '(7 number)
      (m:evaluate vm '(handler-case (+ 1 'bad) (type-error (c)
        (list (if (eq (type-error-datum c) 'bad) 7 0) (type-error-expected-type c)))))))
    (is (equal '("bad" integer)
      (m:evaluate vm '(handler-case (the integer "bad")
        (type-error (c) (list (type-error-datum c) (type-error-expected-type c)))))))
    (is (equal '(/ (1 0))
      (m:evaluate vm '(handler-case (/ 1 0) (division-by-zero (c)
        (list (arithmetic-error-operation c) (arithmetic-error-operands c)))))))
    (is (eq t (m:evaluate vm '(handler-case missing-variable
      (unbound-variable (c) (eq 'missing-variable (cell-error-name c)))))))
    (is (eq t (m:evaluate vm '(handler-case (missing-function)
      (undefined-function (c) (eq 'missing-function (cell-error-name c)))))))
    (is (equal '(nil t)
      (m:evaluate vm '(multiple-value-bind (value condition) (ignore-errors (/ 1 0))
        (list value (typep condition 'division-by-zero))))))
    (is (equal '(1 2) (multiple-value-list (m:evaluate vm '(ignore-errors (values 1 2))))))
    (is (null (multiple-value-list (m:evaluate vm '(handler-case (values) (error () 1))))))
    (is (= 7 (m:evaluate vm '(handler-case (values 3 4) (:no-error (a b) (+ a b))))))
    ;; Handlers decline by returning; newest clusters run first, left to right.
    (is (equal '(3 2 1)
      (m:evaluate vm '(let ((seen nil))
        (handler-bind ((condition (lambda (c) (setq seen (cons 3 seen)))))
          (handler-bind ((condition (lambda (c) (setq seen (cons 1 seen))))
                         (condition (lambda (c) (setq seen (cons 2 seen)))))
            (signal 'simple-condition))) seen))))
    ;; Signaling within a handler hides the entire cluster and all inner clusters.
    (is (= 102 (m:evaluate vm '(let ((n 0))
      (handler-bind ((condition (lambda (c) (setq n (1+ n)))))
        (handler-bind ((condition (lambda (c) (signal c)))
                       (condition (lambda (c) (setq n (+ n 100)))))
          (signal 'simple-condition))) n))))
    (is (= 42 (m:evaluate vm '(block done
      (handler-bind ((error (lambda (c) (return-from done 42)))) (/ 1 0))))))
    ;; The no-error clause and handler bodies run outside the established handlers.
    (is (= 9 (m:evaluate vm '(handler-case
      (handler-case 1 (error () 2) (:no-error (v) (error "no-error failed")))
      (error () 9)))))
    (is (= 9 (m:evaluate vm '(handler-case
      (handler-case (/ 1 0) (error () (error "handler failed"))) (error () 9)))))
    (is (null (m:evaluate vm '(signal "unhandled signal"))))))

(test condition-restarts
  (m:with-interpreter (vm)
    (is (= 42 (m:evaluate vm '(restart-case
      (handler-bind ((error (lambda (c) (invoke-restart 'use 42)))) (error "bad"))
      (use (value) value)))))
    (is (equal '(7 8) (multiple-value-list (m:evaluate vm '(restart-case (invoke-restart 'use)
      (use () (values 7 8)))))))
    (is (equal '(1 2) (multiple-value-list (m:evaluate vm '(restart-case (values 1 2) (use () 3))))))
    (is (= 43 (m:evaluate vm '(restart-bind ((use (lambda (v) (+ v 1)))) (+ (invoke-restart 'use 41) 1)))))
    (is (equal '(nil t) (multiple-value-list (m:evaluate vm '(with-simple-restart (continue "Continue") (continue))))))
    (is (equal '(1 2) (multiple-value-list (m:evaluate vm '(with-simple-restart (continue "Continue") (values 1 2))))))
    (is (equal '(t t)
      (m:evaluate vm '(restart-case
        (let ((r (find-restart 'use))) (list (typep r 'restart) (eq (restart-name r) 'use)))
        (use () nil)))))
    (is (= 17 (m:evaluate vm '(restart-case (invoke-restart-interactively 'use)
      (use (value) :interactive (lambda () (list 17)) value)))))
    (is (= 7 (m:evaluate vm '(handler-bind ((error (lambda (c) (invoke-restart (find-restart 'use c)))) )
      (restart-case (error "bad") (use () :test (lambda (c) (typep c 'error)) 7))))))
    (is (null (m:evaluate vm '(restart-case (find-restart 'hidden)
      (hidden () :test (lambda (c) nil) 1)))))
    (is (equal '(t nil) (m:evaluate vm '(let ((c (make-condition 'error)) (other (make-condition 'error)))
      (restart-case
        (with-condition-restarts c (list (find-restart 'use))
          (list (if (find-restart 'use c) t nil) (if (find-restart 'use other) t nil)))
        (use () nil))))))
    ;; A restart handle remains an object but cannot be invoked after its scope ends.
    (let ((r (m:evaluate vm '(restart-case (find-restart 'use) (use () 1)))))
      (is (m:guest-restart-p r))
      (is (null (m:evaluate vm `(find-restart ,r))))
      (signals m:interpreter-error (m:evaluate vm `(invoke-restart ,r))))
    (is (null (m:evaluate vm '(continue))))
    (signals m:interpreter-error (m:evaluate vm '(abort)))
    (is (= 9 (m:evaluate vm '(handler-case (invoke-restart 'absent) (control-error () 9)))))))

(test restart-case-lexical-options-and-expansion
  (m:with-interpreter (vm)
    (dolist (form
      '((flet ((local-restart-report (stream) (write-string "Lexical report" stream)))
          (restart-case (format nil "~A" (find-restart 'use))
            (use () :report local-restart-report)))
        (let ((prefix "Captured report"))
          (labels ((local-restart-report (stream) (write-string prefix stream)))
            (restart-case (format nil "~A" (find-restart 'use))
              (use () :report local-restart-report))))
        (flet ((local-restart-arguments () (list 7 9)))
          (restart-case (invoke-restart-interactively 'use)
            (use (a b) :interactive local-restart-arguments (values b a))))
        (let ((visible t))
          (flet ((local-restart-test (condition) (declare (ignore condition)) visible))
            (restart-case (and (find-restart 'use)
                               (progn (setq visible nil) (not (find-restart 'use))))
              (use () :test local-restart-test))))
        (macrolet ((local-restart-error (&rest args) (cons 'error args)))
          (handler-bind ((error (lambda (condition) (invoke-restart (find-restart 'use condition)))))
            (handler-bind ((error (lambda (condition) (declare (ignore condition)) (error "Secondary"))))
              (restart-case (restart-case (local-restart-error "Original") (use () :inner))
                (use () :outer)))))
        (symbol-macrolet ((local-restart-error (error "Original")))
          (handler-bind ((error (lambda (condition) (invoke-restart (find-restart 'use condition)))))
            (handler-bind ((error (lambda (condition) (declare (ignore condition)) (error "Secondary"))))
              (restart-case (restart-case local-restart-error (use () :inner)) (use () :outer)))))
        (macrolet ((outer-restart-error (&rest args) (cons 'error args)))
          (macrolet ((local-restart-error (&rest args &environment environment)
                       (macroexpand (cons 'outer-restart-error args) environment)))
            (handler-bind ((error (lambda (condition) (invoke-restart (find-restart 'use condition)))))
              (handler-bind ((error (lambda (condition) (declare (ignore condition)) (error "Secondary"))))
                (restart-case (restart-case (local-restart-error "Original") (use () :inner))
                  (use () :outer))))))
        (macrolet ((local-restart-signal (object) (list 'signal object)))
          (handler-bind ((error (lambda (condition) (invoke-restart (find-restart 'use condition) 17))))
            (restart-case (local-restart-signal (make-condition 'error)) (use (value) value))))
        (let ((count 0))
          (macrolet ((local-restart-error (&rest args) (cons 'error args)))
            (handler-bind ((error (lambda (condition) (invoke-restart (find-restart 'use condition)))))
              (restart-case (local-restart-error (progn (incf count) "Original"))
                (use () count)))))
        (restart-case (values 1 2 3) (unused () nil))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
          "Restart lexical scope/association mismatch: ~S" form))))

(test condition-unwinding
  (m:with-interpreter (vm)
    (is (equal '(1 2) (multiple-value-list (m:evaluate vm '(catch 'done (throw 'done (values 1 2)))))))
    (is (null (multiple-value-list (m:evaluate vm '(block done (return-from done (values)))))))
    (is (= 4 (m:evaluate vm '(block nil (return 4)))))
    (is (equal '(1 2) (multiple-value-list (m:evaluate vm '(unwind-protect (values 1 2) (values 3 4))))))
    (is (equal '(4 2)
      (m:evaluate vm '(let ((cleaned 0))
        (list (block done (unwind-protect
          (unwind-protect (return-from done 4) (setq cleaned (1+ cleaned)))
          (setq cleaned (1+ cleaned)))) cleaned)))))
    (is (= 2 (m:evaluate vm '(catch 'x (unwind-protect (throw 'x 1) (throw 'x 2))))))
    (is (equal '(1 2)
      (m:evaluate vm '(let ((n 0)) (list
        (handler-case (unwind-protect (/ 1 0) (setq n 2)) (error () 1)) n)))))
    ;; A nested caught transfer during cleanup must preserve the original transfer.
    (is (= 3 (m:evaluate vm '(catch 'x (unwind-protect (throw 'x 3)
      (handler-case (/ 1 0) (error () 7)))))))
    (m:evaluate-string vm "(defvar *scope-test* 1)")
    (is (equal '(1 2 1)
      (m:evaluate-string vm "(let ((seen 0)) (list
        (handler-case (let ((*scope-test* 2))
          (unwind-protect (/ 1 0) (setq seen *scope-test*)))
          (error () *scope-test*)) seen *scope-test*))")))
    (m:evaluate-string vm "(setq cleaned nil)")
    (signals m:interpreter-error (m:evaluate-string vm "(unwind-protect (error \"unhandled\") (setq cleaned t))"))
    (is (eq t (m:evaluate-string vm "cleaned")))
    (is (= 9 (m:evaluate vm '(handler-case (throw 'absent 1) (control-error () 9)))))
    (m:evaluate vm '(block gone (setq escaped (lambda () (return-from gone 1)))))
    (is (= 9 (m:evaluate vm '(handler-case (funcall escaped) (control-error () 9)))))
    (is (null (m:evaluate vm '(compute-restarts))))))

(test condition-warning-debugger-and-checks
  (m:with-interpreter (vm)
    (signals m:interpreter-warning (m:evaluate vm '(warn "hello ~A" "world")))
    (is (null (m:evaluate vm '(handler-bind ((warning (lambda (c) (muffle-warning c)))) (warn "quiet")))))
    (is (null (m:evaluate vm '(handler-bind ((error (lambda (c) (continue c)))) (cerror "Continue" "bad")))))
    (is (= 42 (m:evaluate vm '(block done (let ((*debugger-hook*
      (lambda (c hook) (return-from done (if (typep c 'error) 42 0))))) (error "bad"))))))
    (is (null (m:evaluate vm '(let ((x 0))
      (handler-bind ((error (lambda (c) (setq x 1) (continue c)))) (assert (= x 1)))))))
    (is (= 42 (m:evaluate vm '(let ((x "bad"))
      (handler-bind ((type-error (lambda (c) (store-value 42 c)))) (check-type x integer)) x))))
    (is (= 42 (m:evaluate vm '(handler-bind ((unbound-variable (lambda (c) (use-value 42 c))))
      (symbol-value 'unbound-recovery)))))
    (is (null (m:evaluate vm '(boundp 'unbound-recovery))))
    (is (= 43 (m:evaluate vm '(handler-bind ((unbound-variable (lambda (c) (store-value 43 c))))
      (symbol-value 'unbound-recovery)))))
    (is (= 43 (m:evaluate vm 'unbound-recovery)))
    (handler-case (m:evaluate vm '(error "Bad ~A: ~D" "value" 7))
      (m:interpreter-error (c) (is (string= "Bad value: 7" (m::interpreter-error-message c)))))
    (is (= 3 (m:evaluate vm '(+ 1 2))))))

(test condition-reports
  (m:with-interpreter (vm)
    (is (string= "bad λ: 7" (m:evaluate vm '(format nil "bad ~A: ~D" "λ" 7))))
    (m:evaluate-string vm "(define-condition report-error (error)
      ((value :initarg :value :reader report-value :writer set-report-value))
      (:report (lambda (c stream) (write-string \"Bad \" stream) (format stream \"~A\" (report-value c)))))")
    (is (string= "Bad λ" (m:evaluate-string vm "(format nil \"~A\" (make-condition 'report-error :value \"λ\"))")))
    (is (string= "Bad 7" (m:evaluate-string vm "(let ((c (make-condition 'report-error :value 0)))
      (set-report-value 7 c) (format nil \"~A\" c))")))
    (handler-case (m:evaluate-string vm "(error 'report-error :value 8)")
      (m:interpreter-error (c) (is (string= "Bad 8" (m::interpreter-error-message c)))))
    (is (string= "λ" (m:evaluate vm '(format nil (lambda (stream x) (write-string x stream)) "λ"))))))

(test restart-reports-and-debugger-scopes
  (m:with-interpreter (vm)
    (is (string= "Use 7" (m:evaluate vm '(with-simple-restart (use "Use ~D" 7)
      (format nil "~A" (find-restart 'use))))))
    (is (string= "Restart" (m:evaluate vm '(restart-bind
      ((use (lambda () nil) :report-function (lambda (s) (write-string "Restart" s))))
      (format nil "~A" (find-restart 'use))))))
    (is (= 7 (m:evaluate vm '(handler-case
      (with-simple-restart (use "Use") (invoke-restart 'use 1)) (program-error () 7)))))
    (m:evaluate vm '(setq debug-called nil))
    (signals m:interpreter-error (m:evaluate vm '(let ((*debugger-hook*
      (lambda (c hook) (setq debug-called t)))) (break "Break"))))
    (is (null (m:evaluate vm 'debug-called)))
    (signals m:interpreter-error (m:evaluate vm '(let ((*break-on-signals* 'error))
      (handler-case (error "Break on signal") (error () 7)))))
    (is (null (m:evaluate vm '*break-on-signals*)))
    (is (= 7 (m:evaluate vm '(handler-case (error "Ordinary error") (error () 7)))))))

(test condition-generic-accessors-and-reports
  (m:with-interpreter (vm)
    (m:evaluate-string vm "
      (define-condition reader-condition-a () ((x :initform 17 :accessor shared-condition-x)))
      (define-condition reader-condition-b () ((x :initform 23 :accessor shared-condition-x)))
      (define-condition string-report-condition () () (:report \"original report\"))
      (define-condition inherited-report-condition (string-report-condition) ())")
    (is (eq t (m:evaluate-string vm "(and (typep #'shared-condition-x 'generic-function)
      (typep #'(setf shared-condition-x) 'generic-function)
      (find-method #'shared-condition-x nil (list (find-class 'reader-condition-a)))
      (find-method #'shared-condition-x nil (list (find-class 'reader-condition-b)))
      (let ((a (make-condition 'reader-condition-a)) (b (make-condition 'reader-condition-b)))
        (and (= (shared-condition-x a) 17) (= (shared-condition-x b) 23)
          (= (setf (shared-condition-x a) 31) 31) (= (shared-condition-x a) 31)
          (= (shared-condition-x b) 23))))")))
    (is (eq t (m:evaluate-string vm "(let* ((method (find-method #'shared-condition-x nil
        (list (find-class 'reader-condition-a)))) (function (mop:method-function method))
        (b (make-condition 'reader-condition-b)))
      (and (function-lambda-expression function)
        (handler-case (progn (funcall function (list b) nil) nil)
          (type-error (c) (and (eq (type-error-datum c) b)
            (eq (type-error-expected-type c) 'reader-condition-a))))))")))
    (is (equal '("original report" "original report" t)
      (m:evaluate-string vm "(let ((object (make-condition 'inherited-report-condition)))
        (list (princ-to-string object) (format nil \"~A\" object)
          (not (null (search \"INHERITED-REPORT-CONDITION\" (prin1-to-string object))))))")))
    (is (eq t (m:evaluate-string vm "(let ((object (make-condition 'string-report-condition))
        (*print-escape* nil))
      (and (string= (with-output-to-string (stream)
        (assert (eq (print-object object stream) object))) \"original report\")
        (find-method #'print-object nil (list (find-class 'string-report-condition) (find-class t))) t))")))
    (m:evaluate-string vm "(let ((prefix \"lexical\"))
      (flet ((local-reporter (condition stream) (declare (ignore condition))
               (format stream \"~A report\" prefix)))
        (define-condition lexical-report-condition () () (:report local-reporter))))")
    (is (equal "lexical report" (m:evaluate-string vm
      "(princ-to-string (make-condition 'lexical-report-condition))")))
    (m:evaluate-string vm "(defmethod print-object ((object inherited-report-condition) stream)
      (if *print-escape* (call-next-method)
        (progn (write-string \"[\" stream) (call-next-method) (write-string \"]\" stream))) object)
      (defmethod shared-condition-x :around ((object reader-condition-a)) (1+ (call-next-method)))")
    (is (equal "[original report]" (m:evaluate-string vm
      "(format nil \"~A\" (make-condition 'inherited-report-condition))")))
    (is (= 18 (m:evaluate-string vm "(shared-condition-x (make-condition 'reader-condition-a))")))
    (is (eq t (m:evaluate-string vm "(let ((*print-escape* t) (*print-readably* t))
      (and (string= (format nil \"~A\" (make-condition 'lexical-report-condition)) \"lexical report\")
           *print-escape* *print-readably*))")))
    (is (eq t (m:evaluate-string vm "(handler-case
      (let ((*print-readably* t) (*print-escape* nil))
        (with-output-to-string (stream) (print-object (make-condition 'string-report-condition) stream)) nil)
      (print-not-readable () t))")))
    (m:evaluate-string vm "(define-condition broken-report-condition () ()
      (:report (lambda (object stream) (declare (ignore object stream)) (error \"report failed\"))))")
    (is (eq t (m:evaluate-string vm "(let ((*print-escape* t) (*print-readably* t))
      (handler-case (progn (format nil \"~A\" (make-condition 'broken-report-condition)) nil)
        (error () (and *print-escape* *print-readably*))))")))))

(test condition-construction-type-specifiers
  (m:with-interpreter (vm)
    (dolist (source
      '("(let ((class (find-class 'simple-type-error)))
           (let ((c (make-condition class :datum 17 :expected-type 'string)))
             (and (typep c class) (typep c 'simple-type-error) (= (type-error-datum c) 17))))"
        "(let ((c (make-condition '(or program-error type-error))))
           (typep c '(or program-error type-error)))"
        "(let ((c (make-condition '(and simple-error type-error) :datum 17 :expected-type 'string)))
           (and (typep c '(and simple-error type-error)) (= (type-error-datum c) 17)))"
        "(progn (deftype aliased-condition () '(or program-error type-error))
           (typep (make-condition 'aliased-condition) 'aliased-condition))"
        "(let ((initial 23)) (define-condition class-designated-condition (condition)
             ((value :initform initial :reader class-designated-value))))
           (= (class-designated-value (make-condition (find-class 'class-designated-condition))) 23)"
        "(let ((calls 0)) (define-condition unrelated-condition (condition)
             ((value :initform (incf calls))))
           (make-condition '(or program-error type-error)) (= calls 0))"
        "(let ((calls 0))
           (define-condition compound-left (condition) ((left :initform (incf calls) :reader compound-left-value)))
           (define-condition compound-right (condition) ((right :initform 23 :reader compound-right-value)))
           (let* ((counter *gensym-counter*) (a (make-condition '(and compound-left compound-right)))
                  (b (make-condition '(and compound-left compound-right))))
             (and (eq (class-of a) (class-of b)) (= calls 2) (= (compound-left-value a) 1)
                  (= (compound-left-value b) 2) (= (compound-right-value a) 23)
                  (= counter *gensym-counter*))))"))
      (is (eq t (m:evaluate-string vm source))))))

(test warning-destinations-and-correctable-handlers
  (m:with-interpreter (vm)
    (dolist (source
      '("(search \"Warning: captured\" (with-output-to-string (*error-output*) (warn \"captured\")))"
        "(let ((buffer (make-string-output-stream)))
           (let ((*error-output* (make-synonym-stream '*standard-output*)) (*standard-output* buffer))
             (warn \"synonym\")) (search \"Warning: synonym\" (get-output-stream-string buffer)))"
        "(handler-case (warn (make-condition 'simple-warning) :format-control \"invalid\") (type-error () t))"
        "(handler-case (error (make-condition 'simple-error) :format-control \"invalid\") (type-error () t))"
        "(let ((calls 0)) (handler-bind ((error (lambda (condition) (incf calls) (continue condition))))
           (cerror \"Continue ~A\" (make-condition 'simple-error) :extra)
           (cerror \"Continue\" \"Second\") (= calls 2)))"
        "(let ((calls 0)) (handler-bind ((warning (lambda (condition) (incf calls) (muffle-warning condition))))
           (warn \"First\") (warn \"Second\") (= calls 2)))"))
      (is (not (null (m:evaluate-string vm source)))))))

(test condition-macro-expansion-functions
  (m:with-interpreter (vm)
    (dolist (name '(define-condition handler-bind handler-case ignore-errors restart-bind restart-case
                   with-simple-restart with-condition-restarts assert check-type))
      (is (eq t (m:evaluate vm `(and (macro-function ',name) (not (special-operator-p ',name))))))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t))))))
      (m:evaluate vm `(setf (macro-function ',(intern (concatenate 'string "COPIED-" (symbol-name name))))
                           (macro-function ',name))))
    (dolist (source
      '("(let ((seen 0)) (equal (multiple-value-list
           (copied-handler-bind ((simple-condition (lambda (c) (declare (ignore c)) (incf seen))))
             (signal \"notice\") (values seen 23))) '(1 23)))"
        "(let ((value 17)) (= (copied-handler-case (error \"failure\") (error () value)) 17))"
        "(equal (multiple-value-list (copied-handler-case (values 17 23)
           (:no-error (a b) (values b a)))) '(23 17))"
        "(null (multiple-value-list (copied-ignore-errors (values))))"
        "(multiple-value-bind (value condition) (copied-ignore-errors (error \"failure\"))
           (and (null value) (typep condition 'error)))"
        "(let ((value 17)) (copied-restart-bind ((bump (lambda (n) (incf value n))))
           (invoke-restart 'bump 6) (= value 23)))"
        "(let ((value 17)) (equal (multiple-value-list
           (copied-restart-case (invoke-restart 'recover 23)
             (recover (x) (values value x)))) '(17 23)))"
        "(equal (multiple-value-list (copied-with-simple-restart (recover \"Recover\")
           (invoke-restart 'recover))) '(nil t))"
        "(let ((condition (make-condition 'simple-error)) (events nil))
           (copied-restart-bind ((recover (lambda () 17)))
             (copied-with-condition-restarts (progn (push :condition events) condition)
                 (progn (push :restarts events) (list (find-restart 'recover)))
               (and (find-restart 'recover condition) (equal events '(:restarts :condition))))))"
        "(let ((attempts 0)) (handler-bind ((error (lambda (c) (incf attempts) (continue c))))
           (copied-assert (> attempts 2)) (= attempts 3)))"
        "(let ((value :bad)) (handler-bind ((type-error (lambda (c) (store-value 17 c))))
           (copied-check-type value integer) (= value 17)))"
        "(let ((value 17)) (copied-define-condition expanded-condition (condition)
           ((value :initform value :reader expanded-condition-value))
           (:report (lambda (c stream) (format stream \"Value ~D\" (expanded-condition-value c)))))
           (let ((c (make-condition 'expanded-condition)))
             (and (= (expanded-condition-value c) 17) (string= (format nil \"~A\" c) \"Value 17\"))))"
        "(equal (multiple-value-list (eval (macroexpand '(handler-case (values 17 23))))) '(17 23))"
        "(let ((counter *gensym-counter*)) (handler-bind () (assert t) (check-type counter integer))
           (ignore-errors (restart-bind () (restart-case (with-simple-restart (recover \"Recover\") 17))))
           (= counter *gensym-counter*))"))
      (is (eq t (m:evaluate-string vm source)) "Condition expansion mismatch: ~A" source))))

(test aesthetic-object-method-bindings
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defclass aesthetic-method-example () ())
      (defmethod print-object ((object aesthetic-method-example) stream)
        (write-string (if (or *print-escape* *print-readably*) \"escaped\" \"plain\") stream) object)")
    (is (equal '("plain" "(plain)" "escaped" t t)
      (m:evaluate-string vm "(let ((*print-escape* t) (*print-readably* t)
          (object (make-instance 'aesthetic-method-example)))
        (list (format nil \"~A\" object) (format nil \"~A\" (list object))
          (prin1-to-string object) *print-escape* *print-readably*))")))))
