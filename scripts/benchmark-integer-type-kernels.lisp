;;;; Shared warmed benchmarks of the actual emitted integer type workers.
(in-package #:cl-user)
(defun integer-type-benchmark-metadata (selector object environment)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (case selector
    (0 (if environment (or (cdr (assoc object environment :test #'equal)) object) object))
    (2 nil)
    (3 (list-length object))
    (4 (error 'type-error :datum object :expected-type 'list))))
(defparameter *integer-type-metadata* #'integer-type-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "integer-type/" (first case))) miricl-port-bench:*cases*)
    '(("integer-type/endpoint-power" integer-power-endpoint-value (list 100000000000000000000000000000000000000 1 nil) :calls 20000)
      ("integer-type/endpoint-zero" integer-power-endpoint-value (list 0 -1 t) :calls 100000)
      ("integer-type/width-integer" integer-endpoint-width-value (list 9223372036854775807) :calls 100000)
      ("integer-type/width-symbolic" integer-endpoint-width-value (list (vector 1 100000000000000000000000000000000000000 nil)) :calls 20000)
      ("integer-type/compare-integer" integer-endpoint-compare-value (list -100 100) :calls 100000)
      ("integer-type/compare-symbolic" integer-endpoint-compare-value (list (vector 1 100000000000000000000000000000000000000 nil) (vector 1 100000000000000000000000000000000000001 t)) :calls 10000)
      ("integer-type/compare-boundary" integer-endpoint-compare-value (list (vector 1 64 t) 18446744073709551615) :calls 10000)
      ("integer-type/interval" integer-type-query-value (list '(integer (0) (17)) nil (vector 0 nil nil) *integer-type-metadata*) :calls 10000)
      ("integer-type/interval-huge" integer-type-query-value (list '(unsigned-byte 100000000000000000000000000000000000000) nil (vector 0 nil nil) *integer-type-metadata*) :calls 10000)
      ("integer-type/intersection" integer-type-query-value (list '(and integer (real 1/2 7/2) (integer 2 4)) nil (vector 0 nil nil) *integer-type-metadata*) :calls 2000)
      ("integer-type/subtype-yes" integer-type-query-value (list '(unsigned-byte 63) 'fixnum (vector 1 nil nil) *integer-type-metadata*) :calls 5000)
      ("integer-type/subtype-no" integer-type-query-value (list '(signed-byte 65) 'fixnum (vector 1 nil nil) *integer-type-metadata*) :calls 5000)
      ("integer-type/subtype-bignum" integer-type-query-value (list '(integer 9223372036854775808 *) 'bignum (vector 1 nil nil) *integer-type-metadata*) :calls 5000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun integer-type-diagnostic-wat ()
  (runtime-wat-sequence
  (runtime-function-definition '$test-integer-type-install
    (wat-fragment #@(func $test-integer-type-install (export "test-integer-type-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*INTEGER-TYPE-METADATA*") (global.get $current-package))
        (call $array-type-metadata-function))))))
  (runtime-function-definition '$test-integer-type-packets
    (wat-fragment #@(func $test-integer-type-packets (export "test-integer-type-packets") (result i32)
      (local $saved (ref null eq)) (local $count i32) (local $packet (ref null eq))
      (local $endpoint (ref eq)) (local $good i32)
      (local.set $saved (global.get $extra-values))
      (local.set $count (global.get $value-count))
      (local.set $packet (call $cons (call $int (i32.const 21)) (ref.null eq)))
      (global.set $extra-values (local.get $packet))
      (global.set $value-count (i32.const 7))
      (local.set $endpoint (call $integer-power-endpoint (call $int (i32.const 64)) (i32.const 1) (i32.const 1)))
      (drop (call $integer-endpoint-width (local.get $endpoint)))
      (drop (call $integer-endpoint-compare (local.get $endpoint) (call $int (i32.const 1))))
      (call $integer-type-interval (call $symbol (i32.const ,(runtime-symbol-id 'integer))) (i32.const 0) (ref.null eq))
      (drop) (drop) (drop)
      (local.set $good (i32.and (ref.eq (global.get $extra-values) (local.get $packet))
        (i32.eq (global.get $value-count) (i32.const 7))))
      (global.set $extra-values (local.get $saved))
      (global.set $value-count (local.get $count))
      (local.get $good))))))

(in-package #:miricl-port-bench)
(defun write-integer-type-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::integer-type-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::integer-type-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::integer-type-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path
            :names '("integer-power-endpoint-value" "integer-endpoint-width-value"
                     "integer-endpoint-compare-value" "integer-type-query-value")))
      (setf (fdefinition 'miricl::integer-type-runtime-wat) original))))
