;;;; Array subtype rules live in ordinary CL above checked target metadata.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $array-type-query-cl ()
  (runtime-function-definition '$array-type-query-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$array-type-query-cl 'miricl-runtime:array-type-query-value))))

(defun $check-type-control-cl ()
  (runtime-function-definition '$check-type-control-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$check-type-control-cl 'miricl-runtime:check-type-control-value))))

(define-runtime-function $check-type-control-structure (($type :nullable-object)) ()
  (:locals ($saved :nullable-object) ($count i32))
  #@(if (i32.ne (call $tag (local.get $type)) (i32.const 7)) (then (return)))
  #@(local.set $saved (call $extra-value-list))
  #@(local.set $count (global.get $value-count))
  #@(block $failed (try_table (catch $transfer $failed)
    (drop (call $check-type-control-cl (local.get $type) (call $array-type-metadata-function)))
    (global.set $extra-values (local.get $saved))
    (global.set $value-count (local.get $count))
    (return)))
  #@(global.set $extra-values (local.get $saved))
  #@(global.set $value-count (local.get $count))
  #@(throw $transfer))

(define-runtime-function $array-type-metadata-function () (:nullable-object)
  #@(if (ref.is_null (global.get $array-type-metadata-function))
    (then (global.set $array-type-metadata-function
      (call $node (i32.const 8) (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%array-type-metadata)))
        (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(global.get $array-type-metadata-function))

(define-runtime-function $array-query
    (($a :nullable-object) ($b :nullable-object) ($mode i32) ($flags i32) ($rank i32) ($env :nullable-object)) (:nullable-object)
  (:locals ($saved :nullable-object) ($count i32) ($result :nullable-object))
  #@(local.set $saved (call $extra-value-list))
  #@(local.set $count (global.get $value-count))
  #@(block $failed (try_table (catch $transfer $failed)
    (if (i32.eq (local.get $mode) (i32.const 1))
      (then ,(wat-fail-when #@(i32.or (i32.gt_u (local.get $flags) (i32.const 2047))
        (i32.lt_s (local.get $rank) (i32.const -1))) :malformed-type)))
    (if (i32.ne (local.get $mode) (i32.const 1))
      (then (call $check-type-control-structure (local.get $a))
        (if (i32.eq (local.get $mode) (i32.const 3))
          (then (call $check-type-control-structure (local.get $b))))))
    (local.set $result (call $array-type-query-cl (local.get $a) (local.get $b)
      (call $vector-new (array.new_fixed $objects 4
        (call $int (local.get $mode)) (call $int (local.get $flags)) (call $int (local.get $rank)) (local.get $env)))
      (call $array-type-metadata-function)))
    (global.set $extra-values (local.get $saved))
    (global.set $value-count (local.get $count))
    (return (local.get $result))))
  #@(global.set $extra-values (local.get $saved))
  #@(global.set $value-count (local.get $count))
  #@(throw $transfer))

(define-runtime-function $array-query-parts (($result :nullable-object)) (i32 i32 :nullable-object)
  (:locals ($parts (ref $objects)) ($flags :nullable-object) ($rank :nullable-object))
  (wat-fail-when #@(i32.ne (call $tag (local.get $result)) (i32.const 34)) :malformed-type)
  #@(local.set $parts (call $vector-data (local.get $result)))
  (wat-fail-when #@(i32.ne (array.len (local.get $parts)) (i32.const 3)) :malformed-type)
  #@(local.set $flags (array.get $objects (local.get $parts) (i32.const 0)))
  #@(local.set $rank (array.get $objects (local.get $parts) (i32.const 1)))
  (wat-fail-when #@(i32.or (i32.eqz (call $scalar-integer-p (local.get $flags)))
    (i32.eqz (call $scalar-integer-p (local.get $rank)))) :malformed-type)
  #@(i32.wrap_i64 (call $scalar-integer-value (local.get $flags)))
  #@(i32.wrap_i64 (call $scalar-integer-value (local.get $rank)))
  #@(array.get $objects (local.get $parts) (i32.const 2)))

(define-runtime-function $array-type-info (($type :nullable-object) ($env :nullable-object)) (i32 i32 :nullable-object)
  #@(call $array-query-parts (call $array-query (local.get $type) (ref.null eq)
    (i32.const 0) (i32.const 0) (i32.const -1) (local.get $env))))

(define-runtime-function $array-shape-info (($flags i32) ($rank i32) ($shape :nullable-object)) (i32 i32 :nullable-object)
  #@(call $array-query-parts (call $array-query (local.get $shape) (ref.null eq)
    (i32.const 1) (local.get $flags) (local.get $rank) (ref.null eq))))

(define-runtime-function $array-intersection-info (($type :nullable-object) ($env :nullable-object)) (i32 i32 :nullable-object)
  #@(call $array-query-parts (call $array-query (local.get $type) (ref.null eq)
    (i32.const 2) (i32.const 0) (i32.const -1) (local.get $env))))

(define-runtime-function $array-subtypep (($a :nullable-object) ($b :nullable-object) ($env :nullable-object)) (i32)
  #@(i32.wrap_i64 (call $scalar-integer-value
    (call $array-query (local.get $a) (local.get $b) (i32.const 3) (i32.const 0) (i32.const -1) (local.get $env)))))

;; One checked private callback exposes existing target services. No array
;; parsing, shape scan, intersection or subtype algorithm lives in this glue.
(define-runtime-function $array-type-metadata-primitive (($op i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($tail :nullable-object))
  #@(local.set $tail (local.get $args))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.eqz (ref.is_null (local.get $tail))) :wrong-arity)
  #@(call $array-type-metadata-value (call $car (local.get $args))
    (call $car (call $cdr (local.get $args)))
    (call $car (call $cdr (call $cdr (local.get $args))))))

;; The fixed-call ABI enters the same checked service after its callers have
;; evaluated three arguments. Packet shape is checked only at the APPLY entry.
(define-runtime-function $array-type-metadata-value
    (($selector :nullable-object) ($type :nullable-object) ($environment :nullable-object)) (:nullable-object)
  (:locals ($tail :nullable-object) ($class :nullable-object) ($code i64))
  (wat-fail-when #@(i32.eqz (call $scalar-integer-p (local.get $selector))) :malformed-type)
  #@(local.set $code (call $scalar-integer-value (local.get $selector)))
  (wat-fail-when #@(i64.gt_u (local.get $code) (i64.const 4)) :malformed-type)
  #@(call $clear-values)
  #@(if (i64.eqz (local.get $code))
    (then
      (local.set $tail (local.get $type))
      (local.set $type (call $expand-derived-type (local.get $type) (local.get $environment)))
      (if (i32.eqz (ref.eq (local.get $tail) (local.get $type)))
        (then (call $check-type-control-structure (local.get $type))))
      (return (local.get $type))))
  #@(if (i64.eq (local.get $code) (i64.const 1))
    (then (call $check-type-control-structure (local.get $type))
      (return (call $int (call $upgraded-array-kind-environment (local.get $type) (local.get $environment))))))
  #@(if (i64.eq (local.get $code) (i64.const 2))
    (then
      (local.set $class (call $subtype-class (local.get $type)))
      (if (ref.is_null (local.get $class)) (then (return (ref.null eq))))
      (return (call $cons
        (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
          ,(clos-library-reference :name)))
        (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
          ,(clos-library-reference :finalized)))))))
  #@(if (i64.eq (local.get $code) (i64.const 3))
    (then (return_call $list-length-native (local.get $type))))
  ;; The diagnostic reason must print in bounded space even for malformed
  ;; private calls; never pass an arbitrary or circular object to FORMAT.
  #@(if (i32.eqz (call $scalar-integer-p (local.get $environment)))
    (then (call $list-type-error (local.get $environment) (i32.const ,(runtime-symbol-id 'fixnum)))))
  #@(drop (call $apply (call $canonical-builtin-function
    (i32.const ,(runtime-symbol-id 'error)) (i32.const 0))
    (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'simple-type-error)))
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id :datum)))
      (call $cons (local.get $type)
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id :expected-type)))
      (call $cons (ref.null eq)
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id :format-control)))
      (call $cons ,(wat-text-literal "Malformed type control (reason ~D: 1=arity, 2=shape, 3=cycle).")
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id :format-arguments)))
      (call $cons (call $cons (local.get $environment) (ref.null eq))
      (ref.null eq))))))))))))
  #@(unreachable))

(define-runtime-section array-type-runtime-wat ()
  #@(global $array-type-metadata-function (mut (ref null eq)) (ref.null eq))
  ($array-type-metadata-primitive) ($array-type-metadata-value)
  ($array-type-metadata-function) ($array-type-query-cl)
  ($check-type-control-cl) ($check-type-control-structure)
  ($array-query) ($array-query-parts)
  ($array-type-info) ($array-shape-info) ($array-intersection-info) ($array-subtypep))
