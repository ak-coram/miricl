;;;; Stable bottom-up merging, with native traversal and standard predicates.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $ordering-predicate ()
  (runtime-function-definition '$ordering-predicate
    (wat-fragment (miricl-cross:runtime-kernel-wat '$ordering-predicate 'miricl-runtime:ordering-predicate :result :truth))))

(define-runtime-function $ordering-less
    (($a :nullable-object) ($b :nullable-object) ($key :nullable-object)
     ($function :nullable-object) ($id i32)) (i32)
  #@(call $ordering-predicate (local.get $a) (local.get $b) (local.get $key) (local.get $function)))

;; Take the right element only when it is strictly before the left. Equal
;; elements consequently retain input order, including across MERGE inputs.
(defun $ordering-merge-runs-cl ()
  (runtime-function-definition '$ordering-merge-runs-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$ordering-merge-runs-cl 'miricl-runtime:merge-ordering-buffers))))

(define-runtime-function $ordering-merge-runs (($input (ref $objects)) ($output (ref $objects)) ($first i32) ($middle i32) ($stop i32) ($cells i32) ($key :nullable-object) ($function :nullable-object) ($id i32)) ()
  #@(drop (call $ordering-merge-runs-cl (call $vector-new (local.get $input)) (call $vector-new (local.get $output))
    (call $cons (call $int (local.get $first)) (call $cons (call $int (local.get $middle))
      (call $cons (call $int (local.get $stop)) (call $cons (call $bool (local.get $cells))
        (call $cons (local.get $key) (call $cons (local.get $function) (ref.null eq))))))))))

(defun $ordering-read-cl ()
  (runtime-function-definition '$ordering-read-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$ordering-read-cl 'miricl-runtime:read-ordering-buffer))))

(define-runtime-function $ordering-read (($sequence :nullable-object) ($buffer (ref $objects)) ($offset i32) ($count i32) ($cells i32)) ()
  #@(drop (call $ordering-read-cl (local.get $sequence) (call $vector-new (local.get $buffer))
    (call $cons (call $int (local.get $offset)) (call $cons (call $int (local.get $count)) (call $cons (call $bool (local.get $cells)) (ref.null eq)))))))

(defun $merge-list-runs ()
  (runtime-function-definition '$merge-list-runs
    (wat-fragment (miricl-cross:runtime-kernel-wat '$merge-list-runs 'miricl-runtime:merge-list-runs))))

(define-runtime-function $merge-lists-native
    (($a :nullable-object) ($b :nullable-object) ($key :nullable-object) ($function :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $merge-list-runs (local.get $a) (local.get $b) (local.get $key) (local.get $function)))
  #@(call $clear-values)
  #@(local.get $result))

(define-runtime-function $ordering-native
    (($sequence :nullable-object) ($other :nullable-object) ($type :nullable-object)
     ($key :nullable-object) ($function :nullable-object) ($merge i32)) (:nullable-object)
  (:locals ($length i32) ($count1 i32) ($count2 i32) ($cells i32) ($list i32) ($id i32)
           ($width i32) ($first i32) ($middle i32) ($stop i32) ($i i32) ($new-list i32)
           ($input (ref $objects)) ($output (ref $objects)) ($swap (ref $objects))
           ($result :nullable-object) ($scan :nullable-object) ($cell :nullable-object))
  #@(local.set $count1 (call $proper-sequence-length (local.get $sequence)))
  #@(local.set $length (local.get $count1))
  #@(local.set $list (i32.eqz (call $arrayp-native (local.get $sequence))))
  #@(if (local.get $merge)
    (then
      (local.set $count2 (call $proper-sequence-length (local.get $other)))
      ,(wat-fail-when #@(i32.gt_u (local.get $count2) (i32.sub (i32.const 536870910) (local.get $count1))) :allocation)
      (local.set $length (i32.add (local.get $count1) (local.get $count2)))
      (block $allocated
        (if (i32.eq (call $id (local.get $type)) (i32.const ,(runtime-symbol-id 'list)))
          (then
            (if (i32.and (local.get $list) (i32.eqz (call $arrayp-native (local.get $other))))
              (then
                ;; Proper lists overlap exactly when their last cons is shared.
                ;; Keep the buffered path for those inputs so no cycle is made.
                (if (i32.or (i32.eqz (local.get $count1)) (i32.eqz (local.get $count2)))
                  (then (return (call $merge-lists-native (local.get $sequence) (local.get $other)
                    (local.get $key) (local.get $function)))))
                (if (i32.eqz (ref.eq (call $last-native (local.get $sequence) (call $int (i32.const 1)))
                                     (call $last-native (local.get $other) (call $int (i32.const 1)))))
                  (then (return (call $merge-lists-native (local.get $sequence) (local.get $other)
                    (local.get $key) (local.get $function)))))))
            (local.set $new-list (i32.const 1)) (br $allocated)))
        (if (i32.eq (call $id (local.get $type)) (i32.const ,(runtime-symbol-id 'vector)))
          (then (local.set $result (call $vector-new (array.new_default $objects (local.get $length)))) (br $allocated)))
        (local.set $result (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'make-sequence)))
          (call $cons (local.get $type) (call $cons (call $int (local.get $length)) (ref.null eq))))))
      (local.set $list (i32.eqz (call $arrayp-native (local.get $result)))))
    (else
      (if (i32.lt_u (local.get $length) (i32.const 2))
        (then (call $clear-values) (return (local.get $sequence))))
      (local.set $cells (local.get $list)) (local.set $result (local.get $sequence))))
  #@(local.set $id (i32.sub (call $value-n (local.get $function)) (i32.const 1)))
  #@(local.set $input (array.new_default $objects (local.get $length)))
  #@(local.set $output (array.new_default $objects (local.get $length)))
  #@(call $ordering-read (local.get $sequence) (local.get $input) (i32.const 0) (local.get $count1) (local.get $cells))
  #@(if (local.get $merge)
    (then
      (call $ordering-read (local.get $other) (local.get $input) (local.get $count1) (local.get $count2) (i32.const 0))
      (call $ordering-merge-runs (local.get $input) (local.get $output) (i32.const 0) (local.get $count1)
        (local.get $length) (i32.const 0) (local.get $key) (local.get $function) (local.get $id))
      (local.set $input (local.get $output)))
    (else
      (local.set $width (i32.const 1))
      (block $sorted (loop $pass (br_if $sorted (i32.ge_u (local.get $width) (local.get $length)))
        (local.set $first (i32.const 0))
        (block $done (loop $runs (br_if $done (i32.ge_u (local.get $first) (local.get $length)))
          (local.set $middle (i32.add (local.get $first) (local.get $width)))
          (local.set $middle (select (local.get $middle) (local.get $length) (i32.lt_u (local.get $middle) (local.get $length))))
          (local.set $stop (i32.add (local.get $middle) (local.get $width)))
          (local.set $stop (select (local.get $stop) (local.get $length) (i32.lt_u (local.get $stop) (local.get $length))))
          (call $ordering-merge-runs (local.get $input) (local.get $output) (local.get $first) (local.get $middle)
            (local.get $stop) (local.get $cells) (local.get $key) (local.get $function) (local.get $id))
          (local.set $first (local.get $stop)) (br $runs)))
        (local.set $swap (local.get $input)) (local.set $input (local.get $output)) (local.set $output (local.get $swap))
        (local.set $width (i32.shl (local.get $width) (i32.const 1))) (br $pass)))))
  #@(local.set $scan (local.get $result))
  #@(block $done (loop $write (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (local.set $cell (array.get $objects (local.get $input) (local.get $i)))
    (if (local.get $new-list)
      (then
        (local.set $cell (call $cons (local.get $cell) (ref.null eq)))
        (if (ref.is_null (local.get $scan)) (then (local.set $result (local.get $cell)))
          (else (struct.set $obj $b (ref.cast (ref $obj) (local.get $scan)) (local.get $cell))))
        (local.set $scan (local.get $cell)))
      (else (if (local.get $cells)
      (then
        (if (i32.eqz (local.get $i)) (then (local.set $result (local.get $cell))))
        (struct.set $obj $b (ref.cast (ref $obj) (local.get $cell))
          (if (result (ref null eq)) (i32.lt_u (i32.add (local.get $i) (i32.const 1)) (local.get $length))
            (then (array.get $objects (local.get $input) (i32.add (local.get $i) (i32.const 1))))
            (else (ref.null eq)))))
      (else
        (if (local.get $list)
          (then (struct.set $obj $a (ref.cast (ref $obj) (local.get $scan)) (local.get $cell)) (local.set $scan (call $cdr (local.get $scan))))
          (else (drop (call $array-raw-element (local.get $result) (local.get $i) (local.get $cell) (i32.const 1)))))))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $write)))
  #@(call $clear-values) #@(local.get $result))

(define-runtime-function $ordering-apply (($args :nullable-object) ($merge i32)) (:nullable-object)
  (:locals ($sequence :nullable-object) ($other :nullable-object) ($type :nullable-object)
           ($key :nullable-object) ($function :nullable-object) ($keyword :nullable-object) ($value :nullable-object)
           ($seen-key i32) ($seen-allow i32) ($allow i32) ($unknown i32) ($id i32))
  #@(if (local.get $merge)
    (then ,(wat-arity 4 2147483647)
      (local.set $type (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
      (local.set $sequence (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
      (local.set $other (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args))))
    (else ,(wat-arity 2 2147483647)
      (local.set $sequence (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))))
  #@(local.set $function (call $car (local.get $args))) #@(local.set $args (call $cdr (local.get $args)))
  #@(block $bound (loop $keys (br_if $bound (ref.is_null (local.get $args)))
    (local.set $keyword (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    ,(wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
    (local.set $value (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    (local.set $id (call $id (local.get $keyword)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id :key)))
      (then (if (i32.eqz (local.get $seen-key))
        (then (local.set $key (local.get $value)) (local.set $seen-key (i32.const 1)))) (br $keys)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id :allow-other-keys)))
      (then (if (i32.eqz (local.get $seen-allow))
        (then (local.set $allow (i32.eqz (ref.is_null (local.get $value)))) (local.set $seen-allow (i32.const 1)))) (br $keys)))
    (local.set $unknown (i32.const 1)) (br $keys)))
  (wat-fail-when #@(i32.and (local.get $unknown) (i32.eqz (local.get $allow))) :wrong-arity)
  #@(local.set $function (call $callable (local.get $function)))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(call $ordering-native (local.get $sequence) (local.get $other) (local.get $type)
    (local.get $key) (local.get $function) (local.get $merge)))

(define-runtime-section sequence-ordering-runtime-wat ()
  ($ordering-predicate) ($ordering-less) ($ordering-merge-runs-cl) ($ordering-merge-runs) ($ordering-read-cl) ($ordering-read) ($merge-list-runs) ($merge-lists-native) ($ordering-native) ($ordering-apply))
