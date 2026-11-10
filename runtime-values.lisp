;;;; Multiple-value state shared by the evaluator and I/O.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $clear-values
    ()
    ()
  #@(global.set $extra-values (ref.null eq))
  #@(global.set $value-count (i32.const 1)))

(define-runtime-function $return-values
    (($values :nullable-object))
    (:nullable-object)
  #@(global.set $extra-values (call $cdr (local.get $values)))
  #@(global.set $value-count (call $length (local.get $values)))
  #@(call $car (local.get $values)))

(define-runtime-function $snapshot-values (($values :nullable-object)) (:nullable-object)
  (:locals ($scan :nullable-object) ($slow :nullable-object) ($head :nullable-object)
           ($tail :nullable-object) ($cell :nullable-object) ($count i32))
  ;; Public VALUES/APPLY and VALUES-LIST may receive a caller-owned spine.
  ;; Capture its elements so subsequent mutation cannot alter saved values.
  #@(if (ref.is_null (local.get $values))
    (then (global.set $extra-values (ref.null eq)) (global.set $value-count (i32.const 0))
      (return (ref.null eq))))
  #@(block $invalid
    (br_if $invalid (i32.ne (call $tag (local.get $values)) (i32.const 7)))
    (local.set $slow (local.get $values))
    (local.set $scan (call $cdr (local.get $values)))
    (local.set $count (i32.const 1))
    (block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
      (br_if $invalid (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
      (local.set $cell (call $cons (call $car (local.get $scan)) (ref.null eq)))
      (if (ref.is_null (local.get $tail)) (then (local.set $head (local.get $cell)))
        (else (struct.set $obj $b (ref.cast (ref $obj) (local.get $tail)) (local.get $cell))))
      (local.set $tail (local.get $cell))
      (local.set $scan (call $cdr (local.get $scan)))
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      (if (i32.eqz (i32.and (local.get $count) (i32.const 1)))
        (then (local.set $slow (call $cdr (local.get $slow)))
          (br_if $invalid (ref.eq (local.get $scan) (local.get $slow)))))
      (br $next)))
    (global.set $extra-values (local.get $head))
    (global.set $value-count (local.get $count))
    (return (call $car (local.get $values))))
  #@(call $type-fail (local.get $values) (call $symbol-value ,(clos-library-reference '%proper-list-type))
    (i32.const ,(runtime-error-code :expected-designator)))
  #@(unreachable))

(define-runtime-function $two-values
    (($a :nullable-object) ($b :nullable-object))
    (:nullable-object)
  #@(global.set $extra-values (call $cons (local.get $b) (ref.null eq)))
  #@(global.set $value-count (i32.const 2))
  #@(local.get $a))

(define-runtime-function $three-values
    (($a :nullable-object) ($b :nullable-object) ($c :nullable-object)) (:nullable-object)
  #@(global.set $extra-values (call $cons (local.get $b) (call $cons (local.get $c) (ref.null eq))))
  #@(global.set $value-count (i32.const 3))
  #@(local.get $a))

(define-runtime-function $value-list
    (($primary :nullable-object))
    (:nullable-object)
  #@(if (result (ref null eq)) (global.get $value-count)
      (then (call $cons (local.get $primary) (global.get $extra-values))) (else (ref.null eq))))

(define-runtime-section values-runtime-wat ()
  #@(global $extra-values (mut (ref null eq)) (ref.null eq))
  #@(global $value-count (mut i32) (i32.const 1))
  ($clear-values)
  ($return-values)
  ($snapshot-values)
  ($two-values) ($three-values)
  ($value-list))
