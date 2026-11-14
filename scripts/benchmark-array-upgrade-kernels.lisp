;;;; Equivalent target-model upgrading fixtures, warmed in separate drivers.
(in-package #:cl-user)
(defun array-upgrade-benchmark-metadata (selector object argument)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (case selector
    (0 (if (consp object) (miricl-runtime:check-type-control-value object #'array-upgrade-benchmark-metadata))
       (let ((expanded (if (eq object 'array-upgrade-bench-bit) 'bit object)))
         (if (consp expanded) (miricl-runtime:check-type-control-value expanded #'array-upgrade-benchmark-metadata))
         expanded))
    (1 (miricl-runtime:integer-type-query-value object nil (vector 0 nil argument)
                                              #'array-upgrade-benchmark-metadata))
    (2 (miricl-runtime:integer-endpoint-compare-value object argument))
    (3 (list-length object))
    (4 (error 'simple-type-error :datum object :expected-type nil
              :format-control "Malformed type control, reason ~D." :format-arguments (list argument)))))
(defparameter *array-upgrade-metadata* #'array-upgrade-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "array-upgrade/" (first case))) miricl-port-bench:*cases*)
    '(("array-upgrade/empty" upgraded-array-kind-value (list nil nil *array-upgrade-metadata*) :calls 20000)
      ("array-upgrade/character" upgraded-array-kind-value (list 'character nil *array-upgrade-metadata*) :calls 20000)
      ("array-upgrade/bit" upgraded-array-kind-value (list 'bit nil *array-upgrade-metadata*) :calls 20000)
      ("array-upgrade/derived" upgraded-array-kind-value (progn (deftype array-upgrade-bench-bit () 'bit) (list 'array-upgrade-bench-bit nil *array-upgrade-metadata*)) :calls 10000)
      ("array-upgrade/interval-bit" upgraded-array-kind-value (list '(integer 0 1) nil *array-upgrade-metadata*) :calls 10000)
      ("array-upgrade/interval-empty" upgraded-array-kind-value (list '(integer 2 1) nil *array-upgrade-metadata*) :calls 10000)
      ("array-upgrade/symbolic" upgraded-array-kind-value (list '(unsigned-byte 100000000000000000000000000000000) nil *array-upgrade-metadata*) :calls 5000)
      ("array-upgrade/member-bit" upgraded-array-kind-value (list '(member 0 1 1 0) nil *array-upgrade-metadata*) :calls 10000)
      ("array-upgrade/member-character" upgraded-array-kind-value (list '(member #\a #\b #\c) nil *array-upgrade-metadata*) :calls 10000)
      ("array-upgrade/member-mixed" upgraded-array-kind-value (list '(member 0 #\a) nil *array-upgrade-metadata*) :calls 10000)
      ("array-upgrade/empty-or" upgraded-array-kind-value (list '(or) nil *array-upgrade-metadata*) :calls 10000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun array-upgrade-diagnostic-wat ()
  (runtime-wat-sequence
    (runtime-function-definition '$test-array-upgrade-install
      (wat-fragment #@(func $test-array-upgrade-install (export "test-array-upgrade-install")
        (drop (call $set-symbol-value
          (call $intern ,(wat-text-literal "*ARRAY-UPGRADE-METADATA*") (global.get $current-package))
          (call $array-upgrade-metadata-function))))))
    (runtime-function-definition '$test-array-upgrade-packets
      (wat-fragment #@(func $test-array-upgrade-packets (export "test-array-upgrade-packets") (param $error-mode i32) (result i32)
        (local $packet (ref null eq)) (local $caught i32) (local $ok i32)
        (local.set $packet (call $cons (call $int (i32.const 21)) (ref.null eq)))
        (global.set $extra-values (local.get $packet)) (global.set $value-count (i32.const 7))
        (block $failed (try_table (catch $transfer $failed)
          (if (local.get $error-mode)
            (then (drop (call $upgraded-array-kind
              (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'eql))) (ref.null eq)))))
            (else (drop (call $upgraded-array-kind
              (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
                (call $cons (call $int (i32.const 0)) (call $cons (call $int (i32.const 1)) (ref.null eq))))))))
          (local.set $caught (i32.const 1))))
        (local.set $ok (i32.and
          (i32.and (ref.eq (global.get $extra-values) (local.get $packet)) (i32.eq (global.get $value-count) (i32.const 7)))
          (i32.eq (local.get $caught) (i32.eqz (local.get $error-mode)))))
        (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
        (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
        (call $clear-values) (local.get $ok))))))

(in-package #:miricl-port-bench)
(defun write-array-upgrade-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::array-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::array-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::array-upgrade-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path
            :names '("upgraded-array-kind-value")))
      (setf (fdefinition 'miricl::array-runtime-wat) original))))
