;;;; Duplicate removal shares keyword binding and the sequence copying/relinking loop.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $duplicate-sequence-keys-cl ()
  (runtime-function-definition '$duplicate-sequence-keys-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$duplicate-sequence-keys-cl 'miricl-runtime:duplicate-sequence-keys))))

(defun $mark-duplicate-values-cl ()
  (runtime-function-definition '$mark-duplicate-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$mark-duplicate-values-cl 'miricl-runtime:mark-duplicate-values))))

(define-runtime-function $remove-duplicates-native
    (($sequence :nullable-object) ($key :nullable-object) ($test :nullable-object) ($test-not :nullable-object)
     ($negate i32) ($start :nullable-object) ($end :nullable-object) ($from-end i32) ($write i32)) (:nullable-object)
  (:locals ($length i32) ($first i32) ($stop i32) ($n i32) ($hash-test i32) ($kind i32)
           ($removed i32) ($keys :nullable-object) ($marks :digits) ($function :nullable-object))
  #@(local.set $length (call $proper-sequence-length (local.get $sequence)))
  #@(local.set $stop (if (result i32) (ref.is_null (local.get $end))
    (then (local.get $length)) (else (call $bounded-index (local.get $end) (local.get $length)))))
  #@(local.set $first (call $bounded-index (local.get $start) (local.get $stop)))
  #@(local.set $n (i32.sub (local.get $stop) (local.get $first)))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(local.set $function (call $callable (if (result (ref null eq)) (local.get $negate)
    (then (local.get $test-not)) (else (local.get $test)))))
  #@(if (i32.lt_u (local.get $n) (i32.const 2)) (then (call $clear-values) (return (local.get $sequence))))
  #@(local.set $keys (call $duplicate-sequence-keys-cl (local.get $sequence) (local.get $key)
    (call $int (local.get $first)) (call $int (local.get $stop))))
  #@(local.set $marks (array.new_default $digits (i32.shr_u (i32.add (local.get $n) (i32.const 31)) (i32.const 5))))
  #@(if (i32.eqz (local.get $negate))
    (then
      (local.set $hash-test (call $hash-test-function-id (local.get $function)))
      (if (i32.and (i32.eq (call $tag (local.get $sequence)) (i32.const 10)) (ref.is_null (local.get $key)))
        (then
          (local.set $kind (call $sequence-character-kind (local.get $function)))
          (if (i32.eq (local.get $kind) (i32.const 2))
            (then (local.set $hash-test (i32.const ,(runtime-symbol-id 'eql)))))
          (if (i32.eq (local.get $kind) (i32.const 3))
            (then (local.set $hash-test (i32.const ,(runtime-symbol-id 'equalp)))))))))
  #@(local.set $removed (i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $mark-duplicate-values-cl (local.get $keys) (call $sequence-mark-view (local.get $marks) (local.get $n))
      (local.get $function) (call $vector-new (array.new_fixed $objects 3
        (call $bool (local.get $negate)) (call $bool (local.get $from-end))
        (if (result (ref null eq)) (local.get $hash-test)
          (then (call $symbol (local.get $hash-test))) (else (ref.null eq))))))))))
  #@(call $copy-with-removals (local.get $sequence) (local.get $length) (local.get $first) (local.get $stop)
    (local.get $marks) (local.get $removed) (local.get $write)))

(define-runtime-section sequence-duplicates-runtime-wat ()
  ($duplicate-sequence-keys-cl) ($mark-duplicate-values-cl) ($remove-duplicates-native))
