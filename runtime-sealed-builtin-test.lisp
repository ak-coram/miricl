;;;; Canonical dependencies belong only to the sealed runtime-kernel adapter.
(in-package #:miricl-test)
(in-suite :miricl)

(defun sealed-builtin-test-wat ()
  (labels ((variable (name)
             (format nil "(call $intern ~A (global.get $current-package))"
               (miricl::wat-string (miricl::wat-text-literal name))))
           (store (name value)
             (format nil "(drop (call $set-symbol-value ~A ~A))" (variable name) value)))
    (miricl::wat-fragment
      (format nil
        "(func (export ~S) (param $dynamic i32) (result i32)
           (local $name (ref null eq)) (local $result (ref null eq))
           (local.set $name (call $symbol-value ~A))
           (block $failed (try_table (catch $transfer $failed)
             (local.set $result
               (if (result (ref null eq)) (local.get $dynamic)
                 (then (call $fdefinition (local.get $name)))
                 (else (call $kernel-function-reference (local.get $name)))))
             ~A (call $clear-values) (return (i32.const 1))))
           ~A
           (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
           (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
           (call $clear-values) (i32.const 0))"
        "sealed-builtin-test-resolve" (variable "*SEALED-TEST-NAME*")
        (store "*SEALED-TEST-RESULT*" "(local.get $result)")
        (store "*SEALED-TEST-CONDITION*" "(global.get $last-condition)")))))

(defun sealed-builtin-test-resolve (vm &key dynamic condition)
  (is (= (if condition 0 1)
         (wt:instance-call (miricl::interpreter-context vm) (miricl::interpreter-instance vm)
                           "sealed-builtin-test-resolve" (if dynamic 1 0))))
  (when condition
    (is (eq t (m:evaluate-string vm
      (format nil "(typep *sealed-test-condition* '~S)" condition)))))
  (is (= 42 (m:evaluate-string vm "(+ 20 22)"))))

(test runtime-kernel-canonical-builtin-bindings
  (let ((original (fdefinition 'miricl::symbol-functions-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::symbol-functions-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (sealed-builtin-test-wat))))
          (let* ((miricl::*interpreter-only-wasm* nil)
                 (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
            (m:with-interpreter (vm)
              ;; Save the actual installed function objects, then alias another
              ;; function into the public binding. Resolution must preserve EQ.
              (dolist (name '(realp rational float vector make-array abs signum /= equal
                             integerp rationalp floatp numberp zerop plusp minusp
                             floor ceiling truncate round ffloor fceiling ftruncate fround mod rem
                             error (setf svref) (setf char) (setf row-major-aref)
                             (setf elt) (setf gethash) (setf fill-pointer)
                             (setf symbol-value) (setf symbol-plist) (setf symbol-function)))
                (m:evaluate-string vm
                  (format nil "(setq *sealed-test-name* '~S
                                     *sealed-test-saved* (fdefinition '~S)
                                     *sealed-test-replacement* (lambda (&rest args) (declare (ignore args)) :replacement))"
                          name name))
                (unwind-protect
                    (progn
                      (sealed-builtin-test-resolve vm)
                      (is (eq t (m:evaluate-string vm "(eq *sealed-test-result* *sealed-test-saved*)")))
                      (m:evaluate-string vm "(setf (fdefinition *sealed-test-name*) *sealed-test-replacement*)")
                      (sealed-builtin-test-resolve vm)
                      (is (eq t (m:evaluate-string vm "(eq *sealed-test-result* *sealed-test-saved*)")))
                      (sealed-builtin-test-resolve vm :dynamic t)
                      (is (eq t (m:evaluate-string vm "(eq *sealed-test-result* *sealed-test-replacement*)"))))
                  (m:evaluate-string vm "(setf (fdefinition *sealed-test-name*) *sealed-test-saved*)")))
              ;; Non-core and MOP bindings preserve the ordinary dynamic lookup.
              (dolist (name '("MOP:CLASS-SLOTS" "SEALED-TEST-PRIVATE-FUNCTION" "(SETF REALP)"))
                (m:evaluate-string vm
                  (format nil "(setq *sealed-test-name* '~A
                                     *sealed-test-saved* (and (fboundp '~A) (fdefinition '~A))
                                     *sealed-test-replacement* (lambda (&rest args) (declare (ignore args)) :replacement))"
                          name name name))
                (unwind-protect
                    (progn
                      (m:evaluate-string vm "(setf (fdefinition *sealed-test-name*) *sealed-test-replacement*)")
                      (sealed-builtin-test-resolve vm)
                      (is (eq t (m:evaluate-string vm "(eq *sealed-test-result* *sealed-test-replacement*)"))))
                  (m:evaluate-string vm
                    "(if *sealed-test-saved* (setf (fdefinition *sealed-test-name*) *sealed-test-saved*)
                         (fmakunbound *sealed-test-name*))")))
              ;; Fixed-shape validation rejects dotted, oversized and cyclic
              ;; function names, preserving the exact offending target object.
              (dolist (form '("7" "#(setf aref)" "'(setf . aref)" "'(setf aref extra)"
                              "'(setf 7)" "(let ((name (list 'setf 'aref))) (setf (cddr name) name) name)"))
                ;; Keep the cyclic fixture inside the target; the host value
                ;; transport is not part of this name-validation boundary.
                (m:evaluate-string vm (format nil "(progn (setq *sealed-test-name* ~A) (values))" form))
                (sealed-builtin-test-resolve vm :condition 'type-error)
                (is (eq t (m:evaluate-string vm
                  "(eq (type-error-datum *sealed-test-condition*) *sealed-test-name*)")))
                (m:evaluate-string vm "(setq *sealed-test-name* 'realp)")
                (sealed-builtin-test-resolve vm))
              (m:evaluate-string vm "(setq *sealed-test-name* '(setf sealed-undefined))")
              (sealed-builtin-test-resolve vm :condition 'undefined-function))))
      (setf (fdefinition 'miricl::symbol-functions-runtime-wat) original))))
