;;;; IEEE float properties without rational conversion or interpreted loops.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $require-float (($x :nullable-object)) (:object)
  #@(if (i32.gt_u (i32.sub (call $tag (local.get $x)) (i32.const 3)) (i32.const 1))
    (then (call $list-type-error (local.get $x) (i32.const ,(runtime-symbol-id 'float)))))
  #@(ref.as_non_null (local.get $x)))

(define-runtime-function $float-bits (($x :nullable-object)) (:object)
  #@(local.set $x (call $require-float (local.get $x)))
  #@(call $uint64
    (if (result i64) (i32.eq (call $tag (local.get $x)) (i32.const 3))
      (then (i64.extend_i32_u (i32.reinterpret_f32
        (f32.demote_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $x)))))))
      (else (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $x))))))))

(define-runtime-function $bits-float (($bits :nullable-object) ($prototype :nullable-object)) (:object)
  (:locals ($kind i32) ($width i32) ($i i32) ($word i64) ($value f64))
  #@(local.set $prototype (call $require-float (local.get $prototype)))
  #@(local.set $bits (call $require-integer (local.get $bits)))
  #@(local.set $kind (call $tag (local.get $prototype)))
  #@(local.set $width (select (i32.const 32) (i32.const 64) (i32.eq (local.get $kind) (i32.const 3))))
  #@(if (i32.or (i32.lt_s (call $value-n (local.get $bits)) (i32.const 0))
                (i32.gt_u (call $bitlength (ref.as_non_null (local.get $bits))) (local.get $width)))
    (then (call $type-fail (local.get $bits)
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'unsigned-byte)))
        (call $cons (call $int (local.get $width)) (ref.null eq)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(if (call $scalar-integer-p (local.get $bits))
    (then (local.set $word (call $scalar-integer-value (ref.as_non_null (local.get $bits)))) )
    (else
      (local.set $i (call $integer-length (ref.as_non_null (local.get $bits))))
      (loop $digits
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (local.set $word (i64.or (i64.shl (local.get $word) (i64.const 15))
          (i64.extend_i32_u (call $digit (ref.as_non_null (local.get $bits)) (local.get $i)))))
        (br_if $digits (local.get $i)))))
  #@(local.set $value
    (if (result f64) (i32.eq (local.get $kind) (i32.const 3))
      (then (f64.promote_f32 (f32.reinterpret_i32 (i32.wrap_i64 (local.get $word)))))
      (else (f64.reinterpret_i64 (local.get $word)))))
  (wat-new-object #@(local.get $kind) :f #@(local.get $value)))

;; Return the stored integer significand and its binary scale. Subnormal
;; significands have no hidden bit; their actual bit length is the precision.
(define-runtime-function $float-parts (($x :object)) (i64 i32)
  (:locals ($bits i64) ($mantissa i64) ($exponent i32) ($shift i32) ($bias i32))
  (wat-fail-when #@(i64.eq
    (i64.and (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $x))))
      (i64.const 9218868437227405312)) (i64.const 9218868437227405312)) :invalid-float)
  #@(if (i32.eq (call $tag (local.get $x)) (i32.const 3))
    (then
      (local.set $bits (i64.extend_i32_u (i32.reinterpret_f32 (f32.demote_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $x)))))))
      (local.set $shift (i32.const 23)) (local.set $bias (i32.const 127))
      (local.set $mantissa (i64.and (local.get $bits) (i64.const 8388607)))
      (local.set $exponent (i32.and (i32.wrap_i64 (i64.shr_u (local.get $bits) (i64.const 23))) (i32.const 255))))
    (else
      (local.set $bits (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $x)))))
      (local.set $shift (i32.const 52)) (local.set $bias (i32.const 1023))
      (local.set $mantissa (i64.and (local.get $bits) (i64.const 4503599627370495)))
      (local.set $exponent (i32.and (i32.wrap_i64 (i64.shr_u (local.get $bits) (i64.const 52))) (i32.const 2047)))))
  #@(if (local.get $exponent)
    (then (local.set $mantissa (i64.or (local.get $mantissa) (i64.shl (i64.const 1) (i64.extend_i32_u (local.get $shift))))))
    (else (local.set $exponent (i32.const 1))))
  #@(local.get $mantissa)
  #@(i32.sub (i32.sub (local.get $exponent) (local.get $bias)) (local.get $shift)))

;; Split powers beyond the normal exponent range so that neither a premature
;; infinity nor a zero factor destroys a representable final result.
(define-runtime-function $scale-binary (($value f64) ($exponent i32)) (f64)
  #@(if (f64.eq (local.get $value) (f64.const 0)) (then (return (local.get $value))))
  #@(block $normal (loop $large
    (br_if $normal (i32.le_s (local.get $exponent) (i32.const 1023)))
    (local.set $value (f64.mul (local.get $value) (f64.const 8.98846567431158e307)))
    (local.set $exponent (i32.sub (local.get $exponent) (i32.const 1023))) (br $large)))
  #@(block $normal (loop $small
    (br_if $normal (i32.ge_s (local.get $exponent) (i32.const -1022)))
    (local.set $value (f64.mul (local.get $value) (f64.const 2.2250738585072014e-308)))
    (local.set $exponent (i32.add (local.get $exponent) (i32.const 1022))) (br $small)))
  #@(f64.mul (local.get $value) (f64.reinterpret_i64
    (i64.shl (i64.extend_i32_u (i32.add (local.get $exponent) (i32.const 1023))) (i64.const 52)))))

(define-runtime-function $float-scale-overflow (($args :nullable-object)) ()
  #@(drop (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'error)))
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'floating-point-overflow)))
            (call $cons (call $symbol (i32.const ,(runtime-symbol-id :operation)))
              (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'scale-float)))
                (call $cons (call $symbol (i32.const ,(runtime-symbol-id :operands)))
                  (call $cons (local.get $args) (ref.null eq))))))))
  #@(unreachable))

(define-runtime-function $float-property-apply (($id i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($x :object) ($y :object) ($integer :nullable-object) ($sign :nullable-object)
           ($primary :nullable-object) ($kind i32) ($exponent i32) ($precision i32)
           ($mantissa i64) ($value f64))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'float-sign)))
                (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'scale-float))))
    (then ,(wat-arity 1 2)) (else ,(wat-arity 1)))
  #@(local.set $x (call $require-float (call $car (local.get $args))))
  #@(local.set $kind (call $tag (local.get $x)))
  #@(local.set $value (struct.get $obj $f (ref.cast (ref $obj) (local.get $x))))
  (wat-symbol-case #@(local.get $id)
    (float-sign
      #@(if (ref.is_null (call $cdr (local.get $args)))
        (then (return (call $float (local.get $kind) (f64.copysign (f64.const 1) (local.get $value))))))
      #@(local.set $y (call $require-float (call $car (call $cdr (local.get $args)))))
      #@(return (call $float (select (local.get $kind) (call $tag (local.get $y))
          (i32.gt_u (local.get $kind) (call $tag (local.get $y))))
        (f64.copysign (struct.get $obj $f (ref.cast (ref $obj) (local.get $y))) (local.get $value)))))
    (float-radix #@(return (call $int (i32.const 2))))
    (float-digits #@(return (call $int (select (i32.const 24) (i32.const 53) (i32.eq (local.get $kind) (i32.const 3))))))
    (scale-float
      (wat-arity 2)
      #@(local.set $integer (call $car (call $cdr (local.get $args))))
      #@(if (i32.ne (call $tag (local.get $integer)) (i32.const 1))
        (then (call $list-type-error (local.get $integer) (i32.const ,(runtime-symbol-id 'integer)))))
      ;; Every finite input has an exponent within 2,098 places of overflow or
      ;; zero. Saturating only the exponent handles arbitrary Lisp integers.
      #@(local.set $exponent (i32.mul (call $value-n (local.get $integer))
        (if (result i32) (i32.gt_s (call $cmpabs (ref.as_non_null (local.get $integer)) (call $int (i32.const 4096))) (i32.const 0))
          (then (i32.const 4096)) (else (call $digit (ref.as_non_null (local.get $integer)) (i32.const 0))))))
      #@(if (f64.eq (local.get $value) (f64.const 0))
        (then (return (call $float (local.get $kind) (local.get $value)))))
      #@(call $float-parts (local.get $x)) #@(local.set $precision) #@(local.set $mantissa)
      #@(if (i32.gt_s
          (i32.add (i32.add (local.get $exponent) (local.get $precision))
            (i32.sub (i32.const 63) (i32.wrap_i64 (i64.clz (local.get $mantissa)))))
          (select (i32.const 127) (i32.const 1023) (i32.eq (local.get $kind) (i32.const 3))))
        (then (call $float-scale-overflow (local.get $args))))
      #@(local.set $value (call $scale-binary (local.get $value) (local.get $exponent)))
      #@(if (i32.eq (local.get $kind) (i32.const 3))
        (then (local.set $value (f64.promote_f32 (f32.demote_f64 (local.get $value))))))
      #@(if (i64.eq (i64.and (i64.reinterpret_f64 (local.get $value)) (i64.const 9218868437227405312)) (i64.const 9218868437227405312))
        (then (call $float-scale-overflow (local.get $args))))
      #@(return (call $float (local.get $kind) (local.get $value)))))
  #@(call $float-parts (local.get $x)) #@(local.set $exponent) #@(local.set $mantissa)
  #@(local.set $precision (i32.sub (i32.const 64) (i32.wrap_i64 (i64.clz (local.get $mantissa)))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'float-precision)))
    (then (return (call $int (local.get $precision)))))
  #@(if (i64.eqz (local.get $mantissa)) (then (local.set $exponent (i32.const 0))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'integer-decode-float)))
    (then
      (local.set $primary (call $uint64 (local.get $mantissa)))
      (local.set $sign (call $int (select (i32.const -1) (i32.const 1)
        (i64.lt_s (i64.reinterpret_f64 (local.get $value)) (i64.const 0))))))
    (else
      (local.set $primary (call $float (local.get $kind)
        (call $scale-binary (f64.convert_i64_u (local.get $mantissa)) (i32.sub (i32.const 0) (local.get $precision)))))
      (local.set $exponent (i32.add (local.get $exponent) (local.get $precision)))
      (local.set $sign (call $float (local.get $kind) (f64.copysign (f64.const 1) (local.get $value))))))
  #@(call $three-values (local.get $primary) (call $int (local.get $exponent)) (local.get $sign)))

(define-runtime-section float-properties-runtime-wat ()
  ($require-float) ($float-bits) ($bits-float) ($float-parts) ($scale-binary) ($float-scale-overflow) ($float-property-apply))
