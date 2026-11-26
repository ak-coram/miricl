;;;; Real interval algorithms in Common Lisp above checked numeric services.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $real-type-interval-cl ()
  (runtime-function-definition '$real-type-interval-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$real-type-interval-cl 'miricl-runtime:real-type-interval-value))))
(defun $float-interval-bound-cl ()
  (runtime-function-definition '$float-interval-bound-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$float-interval-bound-cl 'miricl-runtime:float-interval-bound-value))))
(defun $real-subtype-query-cl ()
  (runtime-function-definition '$real-subtype-query-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$real-subtype-query-cl 'miricl-runtime:real-subtype-query-value))))

(define-runtime-function $real-type-metadata-function () (:nullable-object)
  #@(if (ref.is_null (global.get $real-type-metadata-function))
    (then (global.set $real-type-metadata-function
      (call $node (i32.const 8) (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%real-type-metadata)))
        (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(global.get $real-type-metadata-function))

(define-runtime-function $real-type-metadata-primitive (($op i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($tail :nullable-object))
  #@(local.set $tail (local.get $args))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.ne (call $tag (local.get $tail)) (i32.const 7)) :wrong-arity)
  #@(local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
  (wat-fail-when #@(i32.eqz (ref.is_null (local.get $tail))) :wrong-arity)
  #@(call $real-type-metadata-value (call $car (local.get $args))
    (call $car (call $cdr (local.get $args))) (call $car (call $cdr (call $cdr (local.get $args))))))

(define-runtime-function $real-type-metadata-call (($args :nullable-object)) (:nullable-object)
  #@(call $real-type-metadata-primitive (i32.const 0) (local.get $args)))

;; Checked class metadata, IEEE representation operations, and ordinary TYPEP.
;; Interval construction, rounding direction and subtype decisions live in CL.
(define-runtime-function $real-type-metadata-value
    (($selector :nullable-object) ($value :nullable-object) ($argument :nullable-object)) (:nullable-object)
  (:locals ($code i64) ($kind i32) ($adjacent f64))
  (wat-fail-when #@(i32.eqz (call $scalar-integer-p (local.get $selector))) :malformed-type)
  #@(local.set $code (call $scalar-integer-value (local.get $selector)))
  (wat-fail-when #@(i64.gt_u (local.get $code) (i64.const 5)) :malformed-type)
  #@(call $clear-values)
  #@(if (i64.eqz (local.get $code))
    (then (return_call $array-type-metadata-value (call $int (i32.const 2)) (local.get $value) (ref.null eq))))
  #@(if (i64.eq (local.get $code) (i64.const 1))
    (then
      (local.set $kind (call $tag (local.get $value)))
      ,(wat-fail-when #@(i32.and (i32.ne (local.get $kind) (i32.const 3)) (i32.ne (local.get $kind) (i32.const 4))) :malformed-type)
      (local.set $adjacent (call $to-float (ref.as_non_null (local.get $value))))
      ,(wat-fail-when #@(i64.eq (i64.and (i64.reinterpret_f64 (local.get $adjacent)) (i64.const 9218868437227405312))
        (i64.const 9218868437227405312)) :invalid-float)
      (local.set $adjacent (call $adjacent-float (call $to-float (ref.as_non_null (local.get $value))) (local.get $kind)
        (i32.eqz (ref.is_null (local.get $argument)))))
      ;; The CL worker uses +/-1 only as private out-of-range edge sentinels.
      ;; Never construct an invalid target infinity object.
      (if (f64.eq (f64.abs (local.get $adjacent)) (f64.const inf))
        (then (return (call $int (select (i32.const -1) (i32.const 1) (ref.is_null (local.get $argument)))))))
      (return (call $float (local.get $kind) (local.get $adjacent)))))
  #@(if (i64.eq (local.get $code) (i64.const 2))
    (then (call $check-type-control-structure (local.get $argument))
      (return (call $bool (call $typep (local.get $value) (local.get $argument))))))
  #@(if (i64.le_u (local.get $code) (i64.const 4))
    (then (return_call $array-type-metadata-value (local.get $selector) (local.get $value) (local.get $argument))))
  (wat-fail-when #@(i32.eqz (call $scalar-integer-p (local.get $value))) :malformed-type)
  (wat-fail-when #@(i32.and
    (i64.ne (call $scalar-integer-value (local.get $value)) (i64.const 3))
    (i64.ne (call $scalar-integer-value (local.get $value)) (i64.const 4))) :malformed-type)
  #@(local.set $kind (i32.wrap_i64 (call $scalar-integer-value (local.get $value))))
  #@(call $float (local.get $kind)
    (if (result f64) (i32.eq (local.get $kind) (i32.const 3))
      (then (f64.const 3.4028234663852886e38)) (else (f64.const 1.7976931348623157e308)))))

(define-runtime-function $real-type-worker
    (($a :nullable-object) ($b :nullable-object) ($mode i32)) (:nullable-object)
  (:locals ($saved :nullable-object) ($count i32) ($result :nullable-object))
  #@(local.set $saved (call $extra-value-list))
  #@(local.set $count (global.get $value-count))
  #@(block $failed (try_table (catch $transfer $failed)
    (if (i32.ne (local.get $mode) (i32.const 1))
      (then (call $check-type-control-structure (local.get $a))
        (if (i32.eq (local.get $mode) (i32.const 2)) (then (call $check-type-control-structure (local.get $b))))))
    (local.set $result (if (result (ref null eq)) (i32.eqz (local.get $mode))
      (then (call $real-type-interval-cl (local.get $a) (local.get $b) (call $real-type-metadata-function)))
      (else (if (result (ref null eq)) (i32.eq (local.get $mode) (i32.const 1))
        (then (call $float-interval-bound-cl (local.get $a) (local.get $b) (call $real-type-metadata-function)))
        (else (call $real-subtype-query-cl (local.get $a) (local.get $b) (call $real-type-metadata-function)))))))
    (global.set $extra-values (local.get $saved)) (global.set $value-count (local.get $count))
    (return (local.get $result))))
  #@(global.set $extra-values (local.get $saved))
  #@(global.set $value-count (local.get $count))
  #@(throw $transfer))

(define-runtime-function $real-type-interval
    (($type :nullable-object) ($coverage i32)) (:nullable-object :nullable-object i32 i32)
  (:locals ($parts (ref $objects)))
  #@(local.set $parts (call $vector-data (call $real-type-worker (local.get $type) (call $bool (local.get $coverage)) (i32.const 0))))
  #@(array.get $objects (local.get $parts) (i32.const 0))
  #@(array.get $objects (local.get $parts) (i32.const 1))
  #@(i32.wrap_i64 (call $scalar-integer-value (array.get $objects (local.get $parts) (i32.const 2))))
  #@(i32.wrap_i64 (call $scalar-integer-value (array.get $objects (local.get $parts) (i32.const 3)))))

;; Adjacent representable value, with infinities used only as internal empty
;; interval sentinels. Both zero signs have the same ordering for type bounds.
(define-runtime-function $adjacent-float (($value f64) ($kind i32) ($up i32)) (f64)
  (:locals ($bits i64))
  #@(if (f64.eq (local.get $value) (f64.const 0))
    (then (return (f64.copysign
      (if (result f64) (i32.eq (local.get $kind) (i32.const 3))
        (then (f64.const 1.401298464324817e-45)) (else (f64.const 4.9406564584124654e-324)))
      (f64.convert_i32_s (select (i32.const 1) (i32.const -1) (local.get $up)))))))
  #@(local.set $bits (if (result i64) (i32.eq (local.get $kind) (i32.const 3))
    (then (i64.extend_i32_u (i32.reinterpret_f32 (f32.demote_f64 (local.get $value)))))
    (else (i64.reinterpret_f64 (local.get $value)))))
  #@(local.set $bits (i64.add (local.get $bits)
    (select (i64.const 1) (i64.const -1) (i32.eq (f64.gt (local.get $value) (f64.const 0)) (local.get $up)))))
  #@(if (result f64) (i32.eq (local.get $kind) (i32.const 3))
    (then (f64.promote_f32 (f32.reinterpret_i32 (i32.wrap_i64 (local.get $bits)))))
    (else (f64.reinterpret_i64 (local.get $bits)))))

(define-runtime-function $float-interval-bound
    (($bound :nullable-object) ($exclusive i32) ($kind i32) ($lower i32)) (f64)
  (:locals ($result :object))
  #@(local.set $result (ref.as_non_null (call $real-type-worker (local.get $bound)
    (call $vector-new (array.new_fixed $objects 3 (call $bool (local.get $exclusive))
      (call $int (local.get $kind)) (call $bool (local.get $lower)))) (i32.const 1))))
  #@(if (call $scalar-integer-p (local.get $result))
    (then (return (f64.copysign (f64.const inf) (f64.convert_i64_s (call $scalar-integer-value (local.get $result)))))))
  #@(call $to-float (local.get $result)))

(define-runtime-function $real-subtypep (($a :nullable-object) ($b :nullable-object)) (:nullable-object)
  (:locals ($result i32))
  #@(local.set $result (i32.wrap_i64 (call $scalar-integer-value
    (call $real-type-worker (local.get $a) (local.get $b) (i32.const 2)))))
  #@(call $two-values (call $bool (i32.eq (local.get $result) (i32.const 1))) (call $bool (local.get $result))))

(define-runtime-function $numeric-subtypep
    (($a :nullable-object) ($b :nullable-object) ($env :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $integer-subtypep (local.get $a) (local.get $b) (local.get $env)))
  #@(if (i32.eqz (ref.is_null (call $secondary-value))) (then (return (local.get $result))))
  #@(call $real-subtypep (local.get $a) (local.get $b)))

(define-runtime-function $subtype-class (($type :nullable-object)) (:nullable-object)
  #@(if (call $symbolp (local.get $type)) (then (return (call $named-class (local.get $type)))))
  #@(if (i32.eq (call $tag (local.get $type)) (i32.const 23))
    (then (if (i32.eqz (ref.is_null (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $type)))
        ,(clos-library-reference :class-metaobject)))))
      (then (return (local.get $type))))))
  #@(ref.null eq))

(defun $cons-type-parts-cl ()
  (runtime-function-definition '$cons-type-parts-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$cons-type-parts-cl 'miricl-runtime:cons-type-components))))

(define-runtime-function $cons-type-parts (($type :nullable-object)) (:nullable-object :nullable-object i32)
  (:locals ($parts :nullable-object))
  #@(if (i32.eq (call $tag (local.get $type)) (i32.const 7)) (then (call $check-apply-list (local.get $type))))
  #@(local.set $parts (call $cons-type-parts-cl (local.get $type)))
  #@(if (ref.is_null (local.get $parts)) (then (return (ref.null eq) (ref.null eq) (i32.const 0))))
  #@(call $car (local.get $parts)) #@(call $cdr (local.get $parts)) #@(i32.const 1))

(define-runtime-function $cons-positive-subtypep
    (($a :nullable-object) ($b :nullable-object) ($env :nullable-object)) (i32)
  (:locals ($acar :nullable-object) ($acdr :nullable-object) ($bcar :nullable-object) ($bcdr :nullable-object)
           ($valid i32))
  #@(call $cons-type-parts (local.get $a))
  #@(local.set $valid) #@(local.set $acdr) #@(local.set $acar)
  #@(if (i32.eqz (local.get $valid)) (then (return (i32.const 0))))
  #@(call $cons-type-parts (local.get $b))
  #@(local.set $valid) #@(local.set $bcdr) #@(local.set $bcar)
  #@(if (i32.eqz (local.get $valid)) (then (return (i32.const 0))))
  #@(if (ref.is_null (call $subtypep-native (local.get $acar) (local.get $bcar) (local.get $env)))
    (then (return (i32.const 0))))
  #@(i32.eqz (ref.is_null (call $subtypep-native (local.get $acdr) (local.get $bcdr) (local.get $env)))))

(define-runtime-function $finite-subtype-target-p
    (($type :nullable-object) ($class :nullable-object)) (i32)
  (:locals ($name :nullable-object))
  #@(if (i32.eqz (ref.is_null (local.get $class))) (then (return (i32.const 1))))
  #@(local.set $name (local.get $type))
  #@(if (i32.eq (call $tag (local.get $type)) (i32.const 7))
    (then (local.set $name (call $car (local.get $type))))
    (else
      ,(wat-symbol-case #@(call $id (local.get $name))
         ((nil atom boolean keyword compiled-function base-char standard-char extended-char
           simple-array simple-vector simple-string base-string simple-base-string simple-bit-vector)
          #@(return (i32.const 1))))))
  ;; These operands are bounds, widths, or literal objects, rather than
  ;; embedded type expressions that could evaluate a SATISFIES predicate.
  (wat-symbol-case #@(call $id (local.get $name))
    ((integer rational real float short-float single-float double-float long-float
      signed-byte unsigned-byte bit mod eql member) #@(return (i32.const 1))))
  #@(i32.const 0))

;; Return 0 for unknown, 1 for a subset, and 2 for a counterexample. Avoid
;; allocating a multiple-value list until the public result is established.
(define-runtime-function $finite-subtypep
    (($a :nullable-object) ($b :nullable-object) ($env :nullable-object) ($class :nullable-object)) (i32)
  (:locals ($id i32) ($members :nullable-object))
  #@(local.set $id (call $id (local.get $a)))
  #@(if (i32.and (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id 'null)))
                 (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id 'boolean))))
    (then
      (local.set $id (call $operator (local.get $a)))
      (if (i32.and (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id 'eql)))
                   (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id 'member))))
        (then (return (i32.const 0))))
      (local.set $members (call $cdr (local.get $a)))
      (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'eql)))
        (then (call $arity (local.get $members) (i32.const 1) (i32.const 1))))))
  #@(if (i32.eqz (call $finite-subtype-target-p (local.get $b) (local.get $class)))
    (then (return (i32.const 0))))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'null)))
                (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'boolean))))
    (then
      (if (i32.eqz (call $typep-environment (ref.null eq) (local.get $b) (local.get $env)))
        (then (return (i32.const 2))))
      (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'boolean)))
        (then (if (i32.eqz (call $typep-environment (call $symbol (i32.const 1)) (local.get $b) (local.get $env)))
          (then (return (i32.const 2))))))
      (return (i32.const 1))))
  #@(block $done (loop $next (br_if $done (ref.is_null (local.get $members)))
    (if (i32.eqz (call $typep-environment (call $car (local.get $members)) (local.get $b) (local.get $env)))
      (then (return (i32.const 2))))
    (local.set $members (call $cdr (local.get $members))) (br $next)))
  #@(i32.const 1))

(define-runtime-function $subtypep-native
    (($a :nullable-object) ($b :nullable-object) ($env :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object) ($ca :nullable-object) ($cb :nullable-object) ($cpl :nullable-object)
           ($proof i32))
  ;; Validate control spines before user expansion and again on its results.
  ;; Literal MEMBER/EQL values and opaque type arguments remain data.
  #@(call $check-type-control-structure (local.get $a))
  #@(call $check-type-control-structure (local.get $b))
  #@(local.set $a (call $expand-derived-type (local.get $a) (local.get $env)))
  #@(local.set $b (call $expand-derived-type (local.get $b) (local.get $env)))
  #@(call $check-type-control-structure (local.get $a))
  #@(call $check-type-control-structure (local.get $b))
  #@(if (i32.or (ref.eq (local.get $a) (local.get $b))
        (i32.or (ref.is_null (local.get $a)) (ref.eq (local.get $b) (call $symbol (i32.const 1)))))
    (then (return (call $two-values (call $symbol (i32.const 1)) (call $symbol (i32.const 1))))))
  #@(local.set $result (call $numeric-subtypep (local.get $a) (local.get $b) (local.get $env)))
  #@(if (i32.eqz (ref.is_null (call $secondary-value))) (then (return (local.get $result))))
  #@(local.set $ca (call $subtype-class (local.get $a)))
  #@(local.set $cb (call $subtype-class (local.get $b)))
  #@(if (i32.eqz (ref.is_null (local.get $ca)))
    (then
      (if (ref.is_null (local.get $b))
        (then (return (call $two-values (ref.null eq) (call $symbol (i32.const 1))))))
      ;; A finalized CPL is the same metadata consulted by the Lisp fallback.
      ;; Unfinalized classes retain the generic finalization protocol.
      (if (i32.eqz (ref.is_null (local.get $cb)))
        (then
          (if (i32.eqz (ref.is_null (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $ca)))
                ,(clos-library-reference :finalized)))))
            (then
              (local.set $cpl (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $ca)))
                ,(clos-library-reference :cpl))))
              (if (i32.eqz (ref.is_null (local.get $cpl)))
                (then (return (call $two-values (call $bool (call $member-eq (local.get $cb) (local.get $cpl)))
                  (call $symbol (i32.const 1))))))))))
      (if (i32.eq (call $id (local.get $b)) (i32.const ,(runtime-symbol-id 'atom)))
        (then
          (local.set $cpl (call $car (call $property-cell
            (struct.get $obj $b (ref.cast (ref $obj) (call $named-class (call $symbol (i32.const ,(runtime-symbol-id 'cons))))))
            ,(clos-library-reference :cpl))))
          (return (call $two-values (call $bool (i32.eqz (call $member-eq (local.get $ca) (local.get $cpl))))
            (call $symbol (i32.const 1))))))))
  #@(local.set $proof (call $finite-subtypep (local.get $a) (local.get $b) (local.get $env) (local.get $cb)))
  #@(if (local.get $proof)
    (then (return (call $two-values (call $bool (i32.eq (local.get $proof) (i32.const 1)))
      (call $symbol (i32.const 1))))))
  #@(if (call $cons-positive-subtypep (local.get $a) (local.get $b) (local.get $env))
    (then (return (call $two-values (call $symbol (i32.const 1)) (call $symbol (i32.const 1))))))
  #@(local.set $proof (call $array-subtypep (local.get $a) (local.get $b) (local.get $env)))
  #@(if (local.get $proof)
    (then (return (call $two-values (call $bool (i32.eq (local.get $proof) (i32.const 1)))
      (call $symbol (i32.const 1))))))
  #@(local.set $proof (call $complex-subtypep (local.get $a) (local.get $b) (local.get $env)))
  #@(if (local.get $proof)
    (then (return (call $two-values (call $bool (i32.eq (local.get $proof) (i32.const 1)))
      (call $symbol (i32.const 1))))))
  #@(call $apply ,(clos-library-reference '%subtypep-general)
    (call $cons (local.get $a) (call $cons (local.get $b) (call $cons (local.get $env) (ref.null eq))))))

(define-runtime-section real-type-runtime-wat ()
  #@(global $real-type-metadata-function (mut (ref null eq)) (ref.null eq))
  ($real-type-metadata-function) ($real-type-metadata-call) ($real-type-metadata-primitive) ($real-type-metadata-value)
  ($real-type-interval-cl) ($float-interval-bound-cl) ($real-subtype-query-cl) ($real-type-worker)
  ($real-type-interval) ($adjacent-float) ($float-interval-bound) ($real-subtypep) ($numeric-subtypep)
  ($subtype-class) ($cons-type-parts-cl) ($cons-type-parts) ($cons-positive-subtypep) ($finite-subtype-target-p) ($finite-subtypep)
  ($subtypep-native))
