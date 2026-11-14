;;;; Native bounded comparison, with unboxed string and linear list traversal.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

;; Canonical comparisons use CL traversal as well as the general callback path.
(defun $mismatch-characters-cl ()
  (runtime-function-definition '$mismatch-characters-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$mismatch-characters-cl 'miricl-runtime:mismatch-character-regions))))

(define-runtime-function $mismatch-characters
    (($a :nullable-object) ($b :nullable-object) ($first1 i32) ($stop1 i32) ($first2 i32) ($stop2 i32)
     ($fast i32) ($negate i32) ($reverse i32)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $mismatch-characters-cl (local.get $a) (local.get $b)
    (call $vector-new (array.new_fixed $objects 6
      (call $int (local.get $first1)) (call $int (local.get $stop1))
      (call $int (local.get $first2)) (call $int (local.get $stop2))
      (call $int (i32.or (local.get $negate) (i32.shl (local.get $reverse) (i32.const 1))))
      (call $sequence-character-fold-function (i32.eq (local.get $fast) (i32.const 3)))))))
  #@(call $clear-values) #@(local.get $result))

(defun $mismatch-sequence-value-cl ()
  (runtime-function-definition '$mismatch-sequence-value-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$mismatch-sequence-value-cl 'miricl-runtime:mismatch-sequence-value))))

(define-runtime-function $mismatch-native
    (($a :nullable-object) ($b :nullable-object) ($first1 i32) ($stop1 i32) ($first2 i32) ($stop2 i32)
     ($key :nullable-object) ($function :nullable-object) ($negate i32) ($reverse i32)) (:nullable-object)
  (:locals ($fast i32))
  #@(if (i32.and (ref.is_null (local.get $key))
      (i32.and (i32.eq (call $tag (local.get $a)) (i32.const 10))
               (i32.eq (call $tag (local.get $b)) (i32.const 10))))
    (then
      (local.set $fast (call $sequence-character-kind (local.get $function)))
      (if (local.get $fast)
        (then (return (call $mismatch-characters (local.get $a) (local.get $b)
          (local.get $first1) (local.get $stop1) (local.get $first2) (local.get $stop2)
          (local.get $fast) (local.get $negate) (local.get $reverse)))))))
  #@(call $clear-values)
  #@(call $mismatch-sequence-value-cl (local.get $a) (local.get $b) (local.get $function)
    (call $vector-new (array.new_fixed $objects 6 (local.get $key)
      (call $int (local.get $first1)) (call $int (local.get $stop1))
      (call $int (local.get $first2)) (call $int (local.get $stop2))
      (call $int (i32.or (local.get $negate) (i32.shl (local.get $reverse) (i32.const 1))))))))

;; The CL worker retains KMP for canonical equivalences only.
(defun $search-characters-cl ()
  (runtime-function-definition '$search-characters-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$search-characters-cl 'miricl-runtime:search-character-regions))))

(define-runtime-function $search-characters
    (($pattern :digits) ($source :digits) ($first1 i32) ($stop1 i32) ($first2 i32) ($stop2 i32)
     ($fold i32) ($reverse i32)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $search-characters-cl (call $text (local.get $pattern)) (call $text (local.get $source))
    (call $vector-new (array.new_fixed $objects 6
      (call $int (local.get $first1)) (call $int (local.get $stop1))
      (call $int (local.get $first2)) (call $int (local.get $stop2))
      (call $bool (local.get $reverse)) (call $sequence-character-fold-function (local.get $fold))))))
  #@(call $clear-values) #@(local.get $result))

(defun $search-sequence-value-cl ()
  (runtime-function-definition '$search-sequence-value-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$search-sequence-value-cl 'miricl-runtime:search-sequence-value))))

(define-runtime-function $search-sequence-native
    (($a :nullable-object) ($b :nullable-object) ($first1 i32) ($stop1 i32) ($first2 i32) ($stop2 i32)
     ($key :nullable-object) ($function :nullable-object) ($negate i32) ($reverse i32)) (:nullable-object)
  (:locals ($length i32) ($fast i32))
  #@(call $clear-values)
  #@(local.set $length (i32.sub (local.get $stop1) (local.get $first1)))
  #@(if (i32.eqz (local.get $length))
    (then (return (call $int (select (local.get $stop2) (local.get $first2) (local.get $reverse))))))
  #@(if (i32.gt_u (local.get $length) (i32.sub (local.get $stop2) (local.get $first2)))
    (then (return (ref.null eq))))
  #@(if (i32.and (i32.eqz (local.get $negate)) (i32.and (ref.is_null (local.get $key))
      (i32.and (i32.eq (call $tag (local.get $a)) (i32.const 10)) (i32.eq (call $tag (local.get $b)) (i32.const 10)))))
    (then
      (local.set $fast (call $sequence-character-kind (local.get $function)))
      (if (local.get $fast)
        (then (return (call $search-characters
          (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $a))))
          (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $b))))
          (local.get $first1) (local.get $stop1) (local.get $first2) (local.get $stop2)
          (i32.eq (local.get $fast) (i32.const 3)) (local.get $reverse)))))))
  #@(call $search-sequence-value-cl (local.get $a) (local.get $b) (local.get $function)
    (call $vector-new (array.new_fixed $objects 6 (local.get $key)
      (call $int (local.get $first1)) (call $int (local.get $stop1))
      (call $int (local.get $first2)) (call $int (local.get $stop2))
      (call $int (i32.or (local.get $negate) (i32.shl (local.get $reverse) (i32.const 1))))))))

;; Both sequence bounds are validated even when an empty region determines
;; the result. SEEN distinguishes missing bounds from explicit NIL starts.
(define-runtime-function $sequence-comparison-apply (($args :nullable-object) ($search i32)) (:nullable-object)
  (:locals ($a :nullable-object) ($b :nullable-object) ($scan :nullable-object)
           ($keyword :nullable-object) ($value :nullable-object) ($key :nullable-object)
           ($test :nullable-object) ($test-not :nullable-object)
           ($start1 :nullable-object) ($end1 :nullable-object) ($start2 :nullable-object) ($end2 :nullable-object)
           ($seen i32) ($id i32) ($unknown i32) ($allow i32) ($reverse i32) ($negate i32)
           ($first1 i32) ($first2 i32) ($stop1 i32) ($stop2 i32) ($length1 i32) ($length2 i32))
  (wat-arity 2 2147483647)
  #@(local.set $a (call $car (local.get $args)))
  #@(local.set $b (call $car (call $cdr (local.get $args))))
  #@(local.set $scan (call $cdr (call $cdr (local.get $args))))
  #@(block $bound (loop $keys (br_if $bound (ref.is_null (local.get $scan)))
    (local.set $keyword (call $car (local.get $scan)))
    (local.set $scan (call $cdr (local.get $scan)))
    ,(wat-fail-when #@(ref.is_null (local.get $scan)) :wrong-arity)
    (local.set $value (call $car (local.get $scan)))
    (local.set $scan (call $cdr (local.get $scan)))
    (local.set $id (call $id (local.get $keyword)))
    ,(runtime-wat-forms
      (loop for (keyword variable) in '((:start1 $start1) (:end1 $end1) (:start2 $start2) (:end2 $end2)
                                       (:key $key) (:test $test) (:test-not $test-not)
                                       (:from-end $reverse) (:allow-other-keys $allow))
        for bit = 1 then (* bit 2) collect
        `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id keyword)))
          (then
            (if (i32.eqz (i32.and (local.get $seen) (i32.const ,bit)))
              (then (local.set ,variable
                ,(if (member variable '($reverse $allow))
                     '(i32.eqz (ref.is_null (local.get $value))) '(local.get $value)))
                (local.set $seen (i32.or (local.get $seen) (i32.const ,bit)))))
            (br $keys)))))
    (local.set $unknown (i32.const 1)) (br $keys)))
  (wat-fail-when #@(i32.and (local.get $unknown) (i32.eqz (local.get $allow))) :wrong-arity)
  (wat-fail-when #@(i32.eq (i32.and (local.get $seen) (i32.const 96)) (i32.const 96)) :wrong-arity)
  #@(local.set $length1 (call $proper-sequence-length (local.get $a)))
  #@(local.set $length2 (call $proper-sequence-length (local.get $b)))
  #@(local.set $stop1 (if (result i32) (ref.is_null (local.get $end1))
    (then (local.get $length1)) (else (call $bounded-index (local.get $end1) (local.get $length1)))))
  #@(local.set $stop2 (if (result i32) (ref.is_null (local.get $end2))
    (then (local.get $length2)) (else (call $bounded-index (local.get $end2) (local.get $length2)))))
  #@(if (i32.and (local.get $seen) (i32.const 1))
    (then (local.set $first1 (call $bounded-index (local.get $start1) (local.get $stop1)))))
  #@(if (i32.and (local.get $seen) (i32.const 4))
    (then (local.set $first2 (call $bounded-index (local.get $start2) (local.get $stop2)))))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(local.set $negate (i32.ne (i32.and (local.get $seen) (i32.const 64)) (i32.const 0)))
  #@(if (i32.eqz (i32.and (local.get $seen) (i32.const 96)))
    (then (local.set $test (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'eql)))))))
  #@(local.set $test (call $callable (if (result (ref null eq)) (local.get $negate)
    (then (local.get $test-not)) (else (local.get $test)))))
  #@(if (local.get $search)
    (then (return (call $search-sequence-native (local.get $a) (local.get $b)
      (local.get $first1) (local.get $stop1) (local.get $first2) (local.get $stop2)
      (local.get $key) (local.get $test) (local.get $negate) (local.get $reverse)))))
  #@(call $mismatch-native (local.get $a) (local.get $b)
    (local.get $first1) (local.get $stop1) (local.get $first2) (local.get $stop2)
    (local.get $key) (local.get $test) (local.get $negate) (local.get $reverse)))

(define-runtime-section sequence-comparison-runtime-wat ()
  ($mismatch-characters-cl) ($mismatch-characters) ($mismatch-sequence-value-cl) ($mismatch-native) ($search-characters-cl) ($search-characters) ($search-sequence-value-cl) ($search-sequence-native) ($sequence-comparison-apply))
