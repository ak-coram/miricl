;;;; Tree substitution copies only after the first change in each list spine.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $substitute-tree-values-cl ()
  (runtime-function-definition '$substitute-tree-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$substitute-tree-values-cl 'miricl-runtime:substitute-tree-values))))

(define-runtime-function $tree-substitute-native
    (($tree :nullable-object) ($item :nullable-object) ($new :nullable-object)
     ($key :nullable-object) ($function :nullable-object) ($mode i32) ($negate i32) ($write i32) ($alist i32)) (:nullable-object)
  #@(call $substitute-tree-values-cl (local.get $tree) (local.get $item) (local.get $new)
    (call $vector-new (array.new_fixed $objects 6 (local.get $key) (local.get $function)
      (call $int (local.get $mode)) (call $bool (local.get $negate))
      (call $bool (local.get $write)) (call $bool (local.get $alist))))))

(defun $tree-equal-native-cl ()
  (runtime-function-definition '$tree-equal-native-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$tree-equal-native-cl 'miricl-runtime:tree-value-equal-p :result :truth))))

(define-runtime-function $tree-equal-native (($a :nullable-object) ($b :nullable-object) ($function :nullable-object) ($negate i32)) (i32)
  #@(call $tree-equal-native-cl (local.get $a) (local.get $b) (local.get $function) (call $bool (local.get $negate))))

(define-runtime-function $tree-operation-apply (($args :nullable-object) ($op i32)) (:nullable-object)
  (:locals ($new :nullable-object) ($item :nullable-object) ($tree :nullable-object) ($key :nullable-object)
           ($test :nullable-object) ($test-not :nullable-object) ($function :nullable-object)
           ($keyword :nullable-object) ($value :nullable-object) ($result :nullable-object)
           ($mode i32) ($write i32) ($alist i32) ($seen i32) ($id i32) ($unknown i32) ($allow i32) ($negate i32))
  #@(if (i32.lt_u (local.get $op) (i32.const 6))
    (then
      ,(wat-arity 3 2147483647)
      (local.set $new (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
      (local.set $mode (i32.rem_u (local.get $op) (i32.const 3)))
      (local.set $write (i32.ge_u (local.get $op) (i32.const 3))))
    (else ,(wat-arity 2 2147483647)
      (local.set $alist (i32.lt_u (local.get $op) (i32.const 8)))
      (local.set $write (i32.eq (local.get $op) (i32.const 7)))))
  #@(local.set $item (call $car (local.get $args)))
  #@(local.set $tree (call $car (call $cdr (local.get $args))))
  #@(local.set $args (call $cdr (call $cdr (local.get $args))))
  #@(block $bound (loop $keys (br_if $bound (ref.is_null (local.get $args)))
    (local.set $keyword (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    ,(wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
    (local.set $value (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    (local.set $id (call $id (local.get $keyword)))
    ,(runtime-wat-forms
      (loop for (keyword variable enabled) in
        '((:key $key (i32.ne (local.get $op) (i32.const 8)))
          (:test $test (i32.eqz (local.get $mode))) (:test-not $test-not (i32.eqz (local.get $mode)))
          (:allow-other-keys $allow nil))
        for bit = 1 then (* bit 2) collect
        `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id keyword)))
          (then
            ,@(when enabled `((if (i32.eqz ,enabled) (then (local.set $unknown (i32.const 1)) (br $keys)))))
            (if (i32.eqz (i32.and (local.get $seen) (i32.const ,bit)))
              (then (local.set ,variable ,(if (eq variable '$allow)
                '(i32.eqz (ref.is_null (local.get $value))) '(local.get $value)))
                (local.set $seen (i32.or (local.get $seen) (i32.const ,bit))))) (br $keys)))))
    (local.set $unknown (i32.const 1)) (br $keys)))
  (wat-fail-when #@(i32.and (local.get $unknown) (i32.eqz (local.get $allow))) :wrong-arity)
  (wat-fail-when #@(i32.eq (i32.and (local.get $seen) (i32.const 6)) (i32.const 6)) :wrong-arity)
  #@(local.set $negate (i32.ne (i32.and (local.get $seen) (i32.const 4)) (i32.const 0)))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(if (i32.eqz (i32.and (local.get $seen) (i32.const 6)))
    (then (local.set $test (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'eql)))))))
  #@(local.set $function (call $callable (if (result (ref null eq)) (local.get $mode)
    (then (local.get $item)) (else (if (result (ref null eq)) (local.get $negate)
      (then (local.get $test-not)) (else (local.get $test)))))))
  #@(if (i32.eq (local.get $op) (i32.const 8))
    (then (local.set $result (call $bool (call $tree-equal-native (local.get $item) (local.get $tree)
      (local.get $function) (local.get $negate)))))
    (else
      (if (local.get $alist)
        (then (drop (call $length (local.get $item)))
          (if (ref.is_null (local.get $item)) (then (call $clear-values) (return (local.get $tree))))))
      (local.set $result (call $tree-substitute-native (local.get $tree) (local.get $item) (local.get $new)
        (local.get $key) (local.get $function) (local.get $mode) (local.get $negate) (local.get $write) (local.get $alist)))))
  #@(call $clear-values) #@(local.get $result))

(define-runtime-section tree-operations-runtime-wat ()
  ($substitute-tree-values-cl) ($tree-substitute-native) ($tree-equal-native-cl) ($tree-equal-native) ($tree-operation-apply))
