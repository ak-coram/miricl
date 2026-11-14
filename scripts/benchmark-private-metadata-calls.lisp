;;;; Diagnostic initialization for checked private metadata callbacks.
;;;; Load after the shared array/stream benchmark fixtures.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun private-metadata-diagnostic-wat ()
  (runtime-wat-sequence
  (runtime-function-definition '$test-private-metadata-install
    (wat-fragment #@(func $test-private-metadata-install (export "test-private-metadata-install")
      (drop (call $stream-sequence-function (i32.const 0)))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*ARRAY-TYPE-METADATA*") (global.get $current-package))
        (call $array-type-metadata-function)))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*STREAM-SEQUENCE-BENCH-OPERATIONS*") (global.get $current-package))
        (global.get $stream-sequence-functions)))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*METADATA-ARRAY-SYMBOL*") (global.get $current-package))
        ,(clos-library-reference '%%array-type-metadata)))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*METADATA-STREAM-SYMBOL*") (global.get $current-package))
        ,(clos-library-reference '%%stream-sequence-metadata))))))
  (runtime-function-definition '$test-private-metadata-values
    (wat-fragment #@(func $test-private-metadata-values (export "test-private-metadata-values") (result i32)
      (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
      (global.set $value-count (i32.const 2))
      (drop (call $native-call3
        (extern.convert_any (call $array-type-metadata-function))
        (extern.convert_any (call $int (i32.const 3)))
        (extern.convert_any (ref.null eq)) (extern.convert_any (ref.null eq))))
      (i32.and (i32.eq (global.get $value-count) (i32.const 1))
        (ref.is_null (global.get $extra-values))))))))
(in-package #:miricl-port-bench)
(defun write-private-metadata-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::native-code-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::native-code-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::private-metadata-diagnostic-wat))))
          (write-benchmark-image path :names cl-user::*metadata-names* :catalog catalog :wat-path wat-path))
      (setf (fdefinition 'miricl::native-code-runtime-wat) original))))
