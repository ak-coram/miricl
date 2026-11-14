;;;; Bounded removal for lists and strings, with native matching and copying.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

;; The private marker array uses the same least-significant-bit-first words
;; as a simple bit-vector. The view shares storage and adds only its header.
(define-runtime-function $sequence-mark-view (($data :digits) ($bits i32)) (:object)
  (wat-fail-when #@(i64.gt_u (i64.extend_i32_u (local.get $bits))
    (i64.shl (i64.extend_i32_u (array.len (local.get $data))) (i64.const 5))) :malformed-form)
  (wat-new-object :bit-vector :n #@(local.get $bits) :digits #@(local.get $data)))

(defun $remove-marked-values-cl ()
  (runtime-function-definition '$remove-marked-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$remove-marked-values-cl 'miricl-runtime:remove-marked-values))))

(define-runtime-function $copy-with-removals
    (($sequence :nullable-object) ($length i32) ($first i32) ($stop i32)
     ($marks :digits) ($removed i32) ($write i32)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(call $clear-values)
  #@(if (i32.eqz (local.get $removed)) (then (return (local.get $sequence))))
  #@(local.set $result (call $remove-marked-values-cl (local.get $sequence)
    (call $sequence-mark-view (local.get $marks) (i32.sub (local.get $stop) (local.get $first)))
    (call $vector-new (array.new_fixed $objects 5 (call $int (local.get $length))
      (call $int (local.get $first)) (call $int (local.get $stop)) (call $int (local.get $removed))
      (call $bool (local.get $write))))))
  #@(call $clear-values)
  #@(local.get $result))

(defun $substitute-marked-values-cl ()
  (runtime-function-definition '$substitute-marked-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$substitute-marked-values-cl 'miricl-runtime:substitute-marked-values))))

;; Copying already uses the shared compiled CL subsequence kernel. This
;; adapter selects that existing implementation before the marked write loop.
(define-runtime-function $copy-with-substitutions
    (($sequence :nullable-object) ($first i32) ($stop i32) ($marks :digits)
     ($changed i32) ($write i32) ($new :nullable-object)) (:nullable-object)
  #@(call $clear-values)
  #@(if (i32.eqz (local.get $changed)) (then (return (local.get $sequence))))
  #@(if (i32.eqz (local.get $write))
    (then (local.set $sequence (call $subsequence-native (local.get $sequence) (call $int (i32.const 0))
      (ref.null eq) (ref.null eq) (i32.const 0)))))
  #@(local.set $sequence (call $substitute-marked-values-cl (local.get $sequence)
    (call $sequence-mark-view (local.get $marks) (i32.sub (local.get $stop) (local.get $first))) (local.get $new)
    (call $vector-new (array.new_fixed $objects 2 (call $int (local.get $first)) (call $int (local.get $stop))))))
  #@(call $clear-values)
  #@(local.get $sequence))

(defun $mark-sequence-values-cl ()
  (runtime-function-definition '$mark-sequence-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$mark-sequence-values-cl 'miricl-runtime:mark-sequence-values))))

(define-runtime-function $remove-sequence-native
    (($mode i32) ($item :nullable-object) ($sequence :nullable-object) ($key :nullable-object)
     ($test :nullable-object) ($test-not :nullable-object) ($negate i32)
     ($start :nullable-object) ($end :nullable-object) ($from-end i32)
     ($count :nullable-object) ($write i32) ($new :nullable-object) ($substitute i32)) (:nullable-object)
  (:locals ($length i32) ($first i32) ($stop i32) ($selected i32) ($limit i32)
           ($reverse i32) ($string i32) ($removed i32) ($needle i32) ($fast-character i32)
           ($function :nullable-object) ($marks (ref $digits)))
  #@(local.set $length (call $proper-sequence-length (local.get $sequence)))
  #@(local.set $stop (if (result i32) (ref.is_null (local.get $end))
    (then (local.get $length)) (else (call $bounded-index (local.get $end) (local.get $length)))))
  #@(local.set $first (call $bounded-index (local.get $start) (local.get $stop)))
  #@(local.set $selected (i32.sub (local.get $stop) (local.get $first)))
  #@(local.set $limit (local.get $selected))
  #@(if (i32.eqz (ref.is_null (local.get $count)))
    (then
      (if (i32.ne (call $tag (local.get $count)) (i32.const 1))
        (then (call $type-fail (local.get $count)
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'or)))
            (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
              (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'null))) (ref.null eq))))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (local.set $limit (if (result i32) (i32.lt_s (call $value-n (local.get $count)) (i32.const 0))
        (then (i32.const 0)) (else (call $natural-count (local.get $count)))))
      (if (i32.gt_u (local.get $limit) (local.get $selected)) (then (local.set $limit (local.get $selected))))))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(local.set $function (call $callable
    (if (result (ref null eq)) (local.get $mode) (then (local.get $item))
      (else (if (result (ref null eq)) (local.get $negate)
        (then (local.get $test-not)) (else (local.get $test)))))))
  #@(if (i32.eqz (local.get $limit))
    (then (call $clear-values) (return (local.get $sequence))))
  #@(local.set $string (i32.eq (call $tag (local.get $sequence)) (i32.const 10)))
  #@(local.set $reverse (i32.and (local.get $from-end)
    (i32.or (local.get $substitute) (i32.eqz (ref.is_null (local.get $count))))))
  #@(if (i32.and (local.get $string) (i32.and (i32.eqz (local.get $mode)) (ref.is_null (local.get $key))))
    (then (local.set $fast-character (call $sequence-character-kind (local.get $function)))
      (if (local.get $fast-character)
        (then (local.set $needle (call $sequence-character-needle (local.get $fast-character) (local.get $item)))))))
  ;; One bit per selected position; predicates run before DELETE relinks cells.
  #@(local.set $marks (array.new_default $digits (i32.shr_u (i32.add (local.get $selected) (i32.const 31)) (i32.const 5))))
  #@(local.set $removed (i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $mark-sequence-values-cl (local.get $item) (local.get $sequence)
      (call $sequence-mark-view (local.get $marks) (local.get $selected))
      (call $vector-new (array.new_fixed $objects 11 (call $int (local.get $mode))
        (local.get $key) (local.get $function) (call $bool (local.get $negate))
        (call $int (local.get $first)) (call $int (local.get $stop))
        (call $bool (local.get $reverse)) (call $int (local.get $limit))
        (call $int (local.get $fast-character)) (call $int (local.get $needle))
        (call $sequence-character-fold-function (i32.eq (local.get $fast-character) (i32.const 3))))))))))
  #@(if (local.get $substitute)
    (then (return (call $copy-with-substitutions (local.get $sequence) (local.get $first) (local.get $stop)
      (local.get $marks) (local.get $removed) (local.get $write) (local.get $new)))))
  #@(call $copy-with-removals (local.get $sequence) (local.get $length) (local.get $first) (local.get $stop)
    (local.get $marks) (local.get $removed) (local.get $write)))

(define-runtime-section sequence-removal-runtime-wat ()
  ($sequence-mark-view) ($remove-marked-values-cl) ($copy-with-removals)
  ($substitute-marked-values-cl) ($copy-with-substitutions) ($mark-sequence-values-cl) ($remove-sequence-native))
