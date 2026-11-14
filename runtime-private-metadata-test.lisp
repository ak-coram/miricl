;;;; Behavioral coverage for the generic checked fixed-argument private ABI.
(in-package #:miricl-test)
(in-suite :miricl)

(defun private-metadata-test-wat ()
  (labels ((variable (name)
             (format nil "(call $intern ~A (global.get $current-package))"
               (miricl::wat-string (miricl::wat-text-literal name))))
           (store (name value)
             (format nil "(drop (call $set-symbol-value ~A ~A))" (variable name) value)))
    (miricl::wat-fragment
      (format nil
        "(func (export ~S)
           ~A ~A)
         (func (export ~S) (param $arity i32) (param $full i32) (result i32)
           (local $function (ref null eq)) (local $args (ref $objects))
           (local $a (ref null eq)) (local $b (ref null eq)) (local $c (ref null eq))
           (local $result (ref null any)) (local $values (ref null eq))
           (local.set $function (call $symbol-value ~A))
           (local.set $args (call $vector-data (call $symbol-value ~A)))
           (local.set $a (array.get $objects (local.get $args) (i32.const 0)))
           (local.set $b (array.get $objects (local.get $args) (i32.const 1)))
           (local.set $c (array.get $objects (local.get $args) (i32.const 2)))
           (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
           (global.set $value-count (i32.const 2))
           (block $failed (try_table (catch $transfer $failed)
             (if (local.get $full)
               (then (local.set $result (call $apply (local.get $function)
                 (call $cons (local.get $a) (call $cons (local.get $b)
                   (call $cons (local.get $c) (ref.null eq)))))))
               (else
                 (if (i32.eq (local.get $arity) (i32.const 2))
                   (then (local.set $result (any.convert_extern (call $native-call2
                     (extern.convert_any (local.get $function))
                     (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))))))
                   (else (if (i32.eq (local.get $arity) (i32.const 4))
                     (then (local.set $result (any.convert_extern (call $native-call4
                       (extern.convert_any (local.get $function))
                       (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
                       (extern.convert_any (local.get $c)) (extern.convert_any (ref.null eq))))))
                     (else (local.set $result (any.convert_extern (call $native-call3
                       (extern.convert_any (local.get $function))
                       (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
                       (extern.convert_any (local.get $c)))))))))))
             (local.set $values (call $value-list (ref.cast (ref null eq) (local.get $result))))
             ~A ~A (call $clear-values) (return (i32.const 1))))
           ~A
           (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
           (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
           (call $clear-values) (i32.const 0))"
        "private-metadata-test-install"
        (store "*METADATA-TEST-ARRAY-SYMBOL*"
          (miricl::wat-string (miricl::clos-library-reference 'miricl::%%array-type-metadata)))
        (store "*METADATA-TEST-STREAM-SYMBOL*"
          (miricl::wat-string (miricl::clos-library-reference 'miricl::%%stream-sequence-metadata)))
        "private-metadata-test-invoke"
        (variable "*METADATA-TEST-FUNCTION*") (variable "*METADATA-TEST-ARGUMENTS*")
        (store "*METADATA-TEST-RESULT*" "(ref.cast (ref null eq) (local.get $result))")
        (store "*METADATA-TEST-VALUES*" "(local.get $values)")
        (store "*METADATA-TEST-CONDITION*" "(global.get $last-condition)")))))

(defun private-metadata-test-invoke (vm setup expected &key (arity 3) full condition)
  ;; Setups may contain cycles; their values are target objects, not a request
  ;; for the host transport to recursively decode the fixture.
  (m:evaluate-string vm (format nil "(progn ~A (values))" setup))
  (is (= (if condition 0 1)
         (wt:instance-call (miricl::interpreter-context vm) (miricl::interpreter-instance vm)
                           "private-metadata-test-invoke" arity (if full 1 0))))
  (is (eq t (m:evaluate-string vm
    (if condition
        (format nil "(typep *metadata-test-condition* '~S)" condition)
        (format nil "(equalp *metadata-test-values* '~S)" expected))))
    "Setup ~A; expected values ~S or condition ~S" setup expected condition)
  (is (= 42 (m:evaluate-string vm "(+ 20 22)"))))

(test private-metadata-fixed-calls
  (let ((original (fdefinition 'miricl::native-code-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::native-code-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (private-metadata-test-wat))))
          (let* ((miricl::*interpreter-only-wasm* nil)
                 (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
            (m:with-interpreter (vm)
              (wt:instance-call (miricl::interpreter-context vm) (miricl::interpreter-instance vm)
                                "private-metadata-test-install")
              (m:evaluate-string vm "(progn
                (defparameter *metadata-test-array* (fdefinition *metadata-test-array-symbol*))
                (defparameter *metadata-test-stream* (fdefinition *metadata-test-stream-symbol*))
                (defparameter *metadata-test-out* (make-string-output-stream)))")
              (dolist (case '(("(vector 0 'integer nil)" (integer))
                              ("(vector 1 'bit nil)" (2))
                              ("(vector 3 '(a b c) nil)" (3))))
                (private-metadata-test-invoke vm
                  (format nil "(setq *metadata-test-function* *metadata-test-array* *metadata-test-arguments* ~A)" (first case))
                  (second case)))
              (private-metadata-test-invoke vm
                "(let ((cycle (list 1 2))) (setf (cdr (last cycle)) cycle)
                  (setq *metadata-test-function* *metadata-test-array*
                        *metadata-test-arguments* (vector 3 cycle nil)))" '(nil))
              (dolist (arguments '("(vector :bad 'integer nil)" "(vector -1 'integer nil)"
                                   "(vector 5 'integer nil)" "(vector (ash 1 100) 'integer nil)"
                                   "(vector 1/2 'integer nil)"))
                (private-metadata-test-invoke vm
                  (format nil "(setq *metadata-test-function* *metadata-test-array* *metadata-test-arguments* ~A)" arguments)
                  nil :condition 'program-error))
              (dolist (arguments '("(vector 3 '(a . b) nil)" "(vector 4 '(array bit (a)) 2)"
                                   "(vector 4 '(array bit (a)) nil)"
                                   "(vector 4 '(array bit (a)) #(1 2))"
                                   "(vector 4 '(array bit (a)) (ash 1 100))"
                                   "(let ((reason (list 1))) (setf (cdr reason) reason) (vector 4 '(array bit (a)) reason))"))
                (private-metadata-test-invoke vm
                  (format nil "(setq *metadata-test-function* *metadata-test-array* *metadata-test-arguments* ~A)" arguments)
                  nil :condition 'type-error))
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* *metadata-test-stream* *metadata-test-arguments* (vector *metadata-test-out* 0 nil))" '(22))
              (dolist (arguments '("(vector :bad 0 nil)" "(vector *metadata-test-out* -1 nil)"
                                   "(vector *metadata-test-out* 4 nil)" "(vector *metadata-test-out* (ash 1 100) nil)"
                                   "(vector *metadata-test-out* 3 nil)" "(vector *metadata-test-out* 3 #())"
                                   "(vector *metadata-test-out* 3 #(\"a\" -1 1 2))"
                                   "(vector *metadata-test-out* 3 #(\"a\" 0 2 2))"))
                (private-metadata-test-invoke vm
                  (format nil "(setq *metadata-test-function* *metadata-test-stream* *metadata-test-arguments* ~A)" arguments)
                  nil :condition 'type-error))
              (dolist (function '("*metadata-test-array*" "*metadata-test-stream*"))
                (dolist (arity '(2 4))
                  (private-metadata-test-invoke vm
                    (format nil "(setq *metadata-test-function* ~A *metadata-test-arguments* #(nil nil nil))" function)
                    nil :arity arity :condition 'program-error)))
              (m:evaluate-string vm "(setf (fdefinition *metadata-test-array-symbol*)
                (lambda (a b c) (values (list a b c) :extra)))")
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* (fdefinition *metadata-test-array-symbol*) *metadata-test-arguments* #(:a :b :c))"
                '((:a :b :c)))
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* (fdefinition *metadata-test-array-symbol*) *metadata-test-arguments* #(:a :b :c))"
                '((:a :b :c) :extra) :full t)
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* *metadata-test-array* *metadata-test-arguments* (vector 3 '(a b c) nil))" '(3))
              (m:evaluate-string vm "(setf (fdefinition *metadata-test-array-symbol*) *metadata-test-array*)")
              (m:evaluate-string vm "(setf (fdefinition *metadata-test-stream-symbol*)
                (lambda (a b c) (values (list c b a) :extra)))")
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* (fdefinition *metadata-test-stream-symbol*) *metadata-test-arguments* #(:a :b :c))"
                '((:c :b :a)))
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* (fdefinition *metadata-test-stream-symbol*) *metadata-test-arguments* #(:a :b :c))"
                '((:c :b :a) :extra) :full t)
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* *metadata-test-stream* *metadata-test-arguments* (vector *metadata-test-out* 0 nil))" '(22))
              (m:evaluate-string vm "(setf (fdefinition *metadata-test-stream-symbol*) *metadata-test-stream*)")
              (private-metadata-test-invoke vm
                "(setq *metadata-test-function* *metadata-test-stream* *metadata-test-arguments* (vector *metadata-test-out* 3 #(\"aλ🙂b\" 1 2 2)))" '(nil))
              (is (equal "λ🙂" (m:evaluate-string vm "(get-output-stream-string *metadata-test-out*)"))))))
      (setf (fdefinition 'miricl::native-code-runtime-wat) original))))
