;;;; Actual CL adjustment workers with an explicit host target-storage adapter.
(in-package :cl-user)
(defparameter *array-adjust-metadata* #'miricl-test::array-adjust-test-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "array-adjust/" (first case))) miricl-port-bench:*cases*)
    '(("array-adjust/shape-scalar" array-adjust-shape-value (list 64 1 *array-adjust-metadata*) :calls 20000)
      ("array-adjust/shape-zero-overflow" array-adjust-shape-value (list '(536870910 2 0) 3 *array-adjust-metadata*) :calls 20000)
      ("array-adjust/chain-empty" array-adjust-displacement-check-value
         (list (make-array 1 :adjustable t) nil *array-adjust-metadata*) :calls 20000)
      ("array-adjust/chain-leaf" array-adjust-displacement-check-value
         (list (make-array 1 :adjustable t) (make-array 16) *array-adjust-metadata*) :calls 20000)
      ("array-adjust/copy-scalar" array-adjust-copy-value
         (list (make-array nil :initial-element :old) (make-array nil) *array-adjust-metadata*) :calls 20000)
      ("array-adjust/copy-vector" array-adjust-copy-value
         (list (make-array 64 :initial-element :old) (make-array 96) *array-adjust-metadata*) :calls 10000)
      ("array-adjust/copy-grid" array-adjust-copy-value
         (list (make-array '(16 16) :initial-element :old) (make-array '(8 32)) *array-adjust-metadata*) :calls 5000)
      ("array-adjust/copy-rank4" array-adjust-copy-value
         (list (make-array '(2 3 4 5) :initial-element :old) (make-array '(3 2 5 4)) *array-adjust-metadata*) :calls 5000)
      ("array-adjust/adjust-simple" array-adjust-value
         (list (make-array 32 :initial-element :old) (vector 48 t :new nil 1 nil nil 0) *array-adjust-metadata*) :calls 10000)
      ("array-adjust/adjust-view" array-adjust-value
         (list (make-array '(4 8) :adjustable t :initial-element :old)
               (vector '(4 8) t :new nil 1 nil nil 0) *array-adjust-metadata*) :calls 5000)
      ("array-adjust/stack-full" vector-stack-value
         (list 1 (list :new (make-array 16 :adjustable t :fill-pointer 16)) *array-adjust-metadata*) :calls 20000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun $array-adjust-copy-benchmark-cl ()
  (runtime-function-definition '$array-adjust-copy-benchmark-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$array-adjust-copy-benchmark-cl 'miricl-runtime:array-adjust-copy-value))))
(defun array-adjust-diagnostic-wat ()
  (runtime-wat-sequence
    ($array-adjust-copy-benchmark-cl)
    (runtime-function-definition '$test-array-adjust-install
      (wat-fragment #@(func $test-array-adjust-install (export "test-array-adjust-install")
        (drop (call $set-symbol-value
          (call $intern ,(wat-text-literal "*ARRAY-ADJUST-METADATA*") (global.get $current-package))
          (call $array-adjust-metadata-function))))))
    (runtime-function-definition '$test-array-adjust-forged
      (wat-fragment #@(func $test-array-adjust-forged (export "test-array-adjust-forged") (param $mode i32) (result i32)
        (local $object (ref null eq)) (local $caught i32)
        (local.set $object (call $node (if (result i32) (i32.eq (local.get $mode) (i32.const 1))
          (then (i32.const 34)) (else (i32.const 35)))
          (i32.const 0) (ref.null eq) (ref.null eq) (ref.null eq)))
        (if (i32.eq (local.get $mode) (i32.const 3))
          (then (local.set $object (call $array-new (array.new_default $digits (i32.const 256)) (i32.const 4)
            (call $vector-new (array.new_default $objects (i32.const 0))) (i32.const 0) (i32.const -1)))))
        (block $failed (try_table (catch $transfer $failed)
          (drop (call $array-adjust-metadata-value
            (call $int (if (result i32) (i32.eq (local.get $mode) (i32.const 2))
              (then (i32.const 2)) (else (i32.const 0)))) (local.get $object) (ref.null eq)))
          (local.set $caught (i32.const 1))))
        (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
        (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
        (call $clear-values) (i32.eqz (local.get $caught)))))
    (runtime-function-definition '$test-array-adjust-packets
      (wat-fragment #@(func $test-array-adjust-packets (export "test-array-adjust-packets") (param $error-mode i32) (result i32)
        (local $packet (ref null eq)) (local $caught i32) (local $ok i32)
        (local.set $packet (call $cons (call $int (i32.const 21)) (ref.null eq)))
        (global.set $extra-values (local.get $packet)) (global.set $value-count (i32.const 7))
        (block $failed (try_table (catch $transfer $failed)
          (drop (call $adjust-array-native
            (call $vector-new (array.new_fixed $objects 1 (call $int (i32.const 1))))
            (call $int (i32.const 2)) (call $symbol (i32.const 1)) (ref.null eq) (ref.null eq)
            (i32.const 0) (if (result (ref null eq)) (local.get $error-mode)
              (then (call $int (i32.const 1))) (else (ref.null eq)))
            (ref.null eq) (call $int (i32.const 0))))
          (local.set $caught (i32.const 1))))
        (local.set $ok (i32.and
          (i32.and (ref.eq (global.get $extra-values) (local.get $packet)) (i32.eq (global.get $value-count) (i32.const 7)))
          (i32.eq (local.get $caught) (i32.eqz (local.get $error-mode)))))
        (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
        (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
        (call $clear-values) (local.get $ok))))))

(in-package #:miricl-port-bench)
(defun array-adjust-kernel-calls ()
  ;; Frozen explicit descriptors: discovery never reads concurrent source edits.
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry '((miricl-runtime:array-adjust-displacement-check-value miricl::$array-adjust-chain-cl)
                     (miricl-runtime:array-adjust-shape-value miricl::$array-adjust-shape-cl)
                     (miricl-runtime:array-adjust-copy-value miricl::$array-adjust-copy-benchmark-cl)
                     (miricl-runtime:array-adjust-value miricl::$array-adjust-cl)
                     (miricl-runtime:vector-stack-value miricl::$vector-stack-cl)))
      (setf (gethash (first entry) table) (list (second entry) (first entry)))) table))
(defun call-with-array-adjust-descriptors (function)
  (let ((original (fdefinition 'kernel-calls)))
    (unwind-protect
        (progn (setf (fdefinition 'kernel-calls) #'array-adjust-kernel-calls) (funcall function))
      (setf (fdefinition 'kernel-calls) original))))
(defun write-array-adjust-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::array-adjust-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::array-adjust-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::array-adjust-diagnostic-wat))))
          (call-with-array-adjust-descriptors
            (lambda () (write-benchmark-image path :catalog catalog :wat-path wat-path
                            :names '("array-adjust-shape-value" "array-adjust-displacement-check-value" "array-adjust-copy-value" "array-adjust-value" "vector-stack-value")))))
      (setf (fdefinition 'miricl::array-adjust-runtime-wat) original))))

;; Stable stack fixtures exercise each mutation branch without unbounded growth.
;; The reset is included on both sides; these are operation-pair measurements.
(in-package :cl-user)
(setf miricl-port-bench:*cases*
  (append miricl-port-bench:*cases*
    '(("array-adjust/stack-push-reset" vector-stack-value
       (list 1 (list :new (make-array 16 :adjustable t :fill-pointer 0)) *array-adjust-metadata*) :calls 20000)
      ("array-adjust/stack-pop-reset" vector-stack-value
       (list 0 (list (make-array 16 :adjustable t :fill-pointer 1 :initial-element :old)) *array-adjust-metadata*) :calls 20000)
      ("array-adjust/stack-grow-reset" vector-stack-value
       (list 2 (list :new (make-array 1 :adjustable t :fill-pointer 1 :initial-element :old) 3) *array-adjust-metadata*) :calls 5000))))
(in-package :miricl-port-bench)
(defun array-adjust-stack-wrapper-wat (entry worker arity truth)
  (declare (ignore arity truth))
  (format nil
    "(func $~A (export ~S) (param $count i32) (result i32)
      (local $fixture (ref null eq)) (local $mode (ref null eq)) (local $args (ref null eq)) (local $metadata (ref null eq))
      (local $array (ref null eq)) (local $fill i32) (local $size i32) (local $sum i32)
      (local.set $fixture (call $symbol-value (call $intern ~A (call $find-package ~A))))
      (local.set $mode (call $car (local.get $fixture))) (local.set $fixture (call $cdr (local.get $fixture)))
      (local.set $args (call $car (local.get $fixture))) (local.set $metadata (call $car (call $cdr (local.get $fixture))))
      (local.set $array (if (result (ref null eq)) (i32.eqz (i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null (local.get $mode)))))
        (then (call $car (local.get $args))) (else (call $car (call $cdr (local.get $args))))))
      (local.set $fill (struct.get $array $fill (call $require-fill-pointer (local.get $array))))
      (local.set $size (call $array-length (local.get $array)))
      (block $done (loop $run (br_if $done (i32.eqz (local.get $count)))
        (if (i32.eq (i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null (local.get $mode)))) (i32.const 2))
          (then (drop (call $adjust-array-native (local.get $array) (call $int (local.get $size))
            (call $array-kind-type (call $array-kind (local.get $array)))
            (ref.null eq) (ref.null eq) (i32.const 0) (call $int (local.get $fill)) (ref.null eq) (call $int (i32.const 0)))))
          (else (drop (call $array-fill-pointer (local.get $array) (call $int (local.get $fill)) (i32.const 1)))))
        (local.set $sum (i32.add (local.get $sum) (i32.eqz (ref.is_null (call ~(~A~) (local.get $mode) (local.get $args) (local.get $metadata))))))
        (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $run))) (local.get $sum))"
    (export-name entry) (export-name entry)
    (miricl::wat-string (miricl::wat-text-literal "*RUNTIME-PORT-BENCH-ARGUMENTS*"))
    (miricl::wat-string (miricl::wat-text-literal "COMMON-LISP-USER")) worker))
(defun array-adjust-install-stack-driver (entry)
  (compile (driver-name entry)
    `(lambda (count fixture)
       (declare (type fixnum count) (optimize (speed 3) (safety 0) (debug 0)) (notinline ,entry))
       (let* ((mode (first fixture)) (args (second fixture)) (metadata (third fixture))
              (array (if (= mode 0) (car args) (second args)))
              (fill (fill-pointer array)) (size (array-total-size array))
              (reset (vector size t nil nil 0 fill nil 0)) (sum 0))
         (declare (type fixnum sum))
         (dotimes (i count sum)
           (if (= mode 2)
               (miricl-runtime:array-adjust-value array reset metadata)
               (setf (fill-pointer array) fill))
           (when (,entry mode args metadata) (incf sum)))))))
;; Wrap the existing descriptor scope, keeping every override temporary.
(let ((descriptor-scope (fdefinition 'call-with-array-adjust-descriptors)))
  (setf (fdefinition 'call-with-array-adjust-descriptors)
    (lambda (function)
      (let ((wrapper (fdefinition 'wrapper-wat)) (driver (fdefinition 'install-driver)))
        (unwind-protect
            (progn
              (setf (fdefinition 'wrapper-wat)
                (lambda (entry worker arity truth)
                  (if (eq entry 'miricl-runtime:vector-stack-value)
                      (array-adjust-stack-wrapper-wat entry worker arity truth)
                      (funcall wrapper entry worker arity truth))))
              (setf (fdefinition 'install-driver)
                (lambda (entry arity)
                  (if (eq entry 'miricl-runtime:vector-stack-value)
                      (array-adjust-install-stack-driver entry)
                      (funcall driver entry arity))))
              (funcall descriptor-scope function))
          (setf (fdefinition 'wrapper-wat) wrapper (fdefinition 'install-driver) driver))))))
