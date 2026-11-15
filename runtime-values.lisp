;;;; Multiple-value state shared by the evaluator and I/O.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $clear-values
    ()
    ()
  #@(global.set $extra-values (ref.null eq))
  #@(global.set $secondary-value (ref.null eq))
  #@(global.set $tertiary-value (ref.null eq))
  #@(global.set $value-count (i32.const 1)))

;; The public runtime ABI still returns the primary value. Small groups keep
;; their remaining values in fixed slots; a non-NIL EXTRA-VALUES overrides the
;; slots and contains the complete secondary-value spine. Only list consumers
;; and legacy state-preservation boundaries materialize that spine.
(define-runtime-function $small-values
    (($a :nullable-object) ($b :nullable-object) ($c :nullable-object) ($count i32))
    (:nullable-object)
  #@(global.set $extra-values (ref.null eq))
  #@(global.set $secondary-value (local.get $b))
  #@(global.set $tertiary-value (local.get $c))
  #@(global.set $value-count (local.get $count))
  #@(local.get $a))

(define-runtime-function $secondary-value () (:nullable-object)
  #@(if (result (ref null eq)) (i32.gt_u (global.get $value-count) (i32.const 1))
    (then (if (result (ref null eq)) (ref.is_null (global.get $extra-values))
      (then (global.get $secondary-value)) (else (call $car (global.get $extra-values)))))
    (else (ref.null eq))))

(define-runtime-function $extra-value-list () (:nullable-object)
  #@(if (result (ref null eq)) (ref.is_null (global.get $extra-values))
    (then (if (result (ref null eq)) (i32.gt_u (global.get $value-count) (i32.const 1))
      (then (call $cons (global.get $secondary-value)
        (if (result (ref null eq)) (i32.gt_u (global.get $value-count) (i32.const 2))
          (then (call $cons (global.get $tertiary-value) (ref.null eq))) (else (ref.null eq)))))
      (else (ref.null eq))))
    (else (global.get $extra-values))))

;; A native Wasm result tuple lets evaluator consumers keep every component
;; live in locals across binding, callbacks, and nested evaluations.
(define-runtime-function $capture-values ()
    (:nullable-object :nullable-object :nullable-object i32)
  #@(if (result (ref null eq) (ref null eq) (ref null eq) i32)
      (ref.is_null (global.get $extra-values))
    (then (global.get $secondary-value) (global.get $tertiary-value)
      (ref.null eq) (global.get $value-count))
    (else (call $car (global.get $extra-values))
      (call $car (call $cdr (global.get $extra-values)))
      (global.get $extra-values) (global.get $value-count))))

(define-runtime-function $restore-values
    (($b :nullable-object) ($c :nullable-object) ($extra :nullable-object) ($count i32)) ()
  #@(global.set $secondary-value (local.get $b))
  #@(global.set $tertiary-value (local.get $c))
  #@(global.set $extra-values (local.get $extra))
  #@(global.set $value-count (local.get $count)))

(define-runtime-function $return-values
    (($values :nullable-object))
    (:nullable-object)
  #@(call $return-values-count (local.get $values) (call $length (local.get $values))))

;; Only callers owning a freshly collected, checked spine supply its count.
(define-runtime-function $return-values-count
    (($values :nullable-object) ($count i32)) (:nullable-object)
  #@(if (i32.le_u (local.get $count) (i32.const 3))
    (then (return (call $small-values (call $car (local.get $values))
      (call $car (call $cdr (local.get $values)))
      (call $car (call $cdr (call $cdr (local.get $values)))) (local.get $count)))))
  #@(global.set $extra-values (call $cdr (local.get $values)))
  #@(global.set $secondary-value (ref.null eq))
  #@(global.set $tertiary-value (ref.null eq))
  #@(global.set $value-count (local.get $count))
  #@(call $car (local.get $values)))

(define-runtime-function $snapshot-values (($values :nullable-object)) (:nullable-object)
  (:locals ($scan :nullable-object) ($slow :nullable-object) ($head :nullable-object)
           ($tail :nullable-object) ($cell :nullable-object) ($count i32)
           ($a :nullable-object) ($b :nullable-object) ($c :nullable-object))
  ;; Public VALUES/APPLY and VALUES-LIST may receive a caller-owned spine.
  ;; Capture its elements so subsequent mutation cannot alter saved values.
  #@(local.set $scan (local.get $values))
  #@(block $large (loop $small
    (if (ref.is_null (local.get $scan))
      (then (return (call $small-values (local.get $a) (local.get $b) (local.get $c) (local.get $count)))))
    (br_if $large (i32.eq (local.get $count) (i32.const 3)))
    (br_if $large (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
    (if (i32.eqz (local.get $count)) (then (local.set $a (call $car (local.get $scan))))
      (else (if (i32.eq (local.get $count) (i32.const 1))
        (then (local.set $b (call $car (local.get $scan))))
        (else (local.set $c (call $car (local.get $scan)))))))
    (local.set $scan (call $cdr (local.get $scan)))
    (local.set $count (i32.add (local.get $count) (i32.const 1))) (br $small)))
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
    (global.set $secondary-value (ref.null eq))
    (global.set $tertiary-value (ref.null eq))
    (global.set $value-count (local.get $count))
    (return (call $car (local.get $values))))
  #@(call $type-fail (local.get $values) (call $symbol-value ,(clos-library-reference '%proper-list-type))
    (i32.const ,(runtime-error-code :expected-designator)))
  #@(unreachable))

(define-runtime-function $two-values
    (($a :nullable-object) ($b :nullable-object))
    (:nullable-object)
  #@(call $small-values (local.get $a) (local.get $b) (ref.null eq) (i32.const 2)))

(define-runtime-function $three-values
    (($a :nullable-object) ($b :nullable-object) ($c :nullable-object)) (:nullable-object)
  #@(call $small-values (local.get $a) (local.get $b) (local.get $c) (i32.const 3)))

(define-runtime-function $value-list
    (($primary :nullable-object))
    (:nullable-object)
  #@(if (result (ref null eq)) (global.get $value-count)
      (then (call $cons (local.get $primary) (call $extra-value-list))) (else (ref.null eq))))

(define-runtime-section values-runtime-wat ()
  #@(global $extra-values (mut (ref null eq)) (ref.null eq))
  #@(global $secondary-value (mut (ref null eq)) (ref.null eq))
  #@(global $tertiary-value (mut (ref null eq)) (ref.null eq))
  #@(global $value-count (mut i32) (i32.const 1))
  ($clear-values)
  ($small-values) ($secondary-value) ($extra-value-list) ($capture-values) ($restore-values)
  ($return-values)
  ($return-values-count)
  ($snapshot-values)
  ($two-values) ($three-values)
  ($value-list))
