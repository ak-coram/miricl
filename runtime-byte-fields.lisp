;;;; Byte-field adapters to ordinary CL extraction and replacement workers.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $check-byte-count-layout
    (($value :nullable-object) ($poll i32)) ()
  #@(call $validate-bit-integer-layout (local.get $value))
  #@(if (i32.ne (call $tag (local.get $value)) (i32.const 1))
    (then
      (call $list-type-error (local.get $value) (i32.const ,(runtime-symbol-id 'unsigned-byte)))
      (return)))
  #@(if (if (result i32) (local.get $poll)
        (then
          ;; Preserve the existing linked MINUSP invocation and its poll on
          ;; the bytespec's first, ordered validation.
          (i32.eqz (ref.is_null (any.convert_extern (call $native-call1
            (extern.convert_any (call $kernel-function-reference (call $symbol
              (i32.const ,(runtime-symbol-id 'minusp)))))
            (extern.convert_any (local.get $value)))))))
        (else (i32.lt_s (call $value-n (local.get $value)) (i32.const 0))))
    (then (call $list-type-error (local.get $value) (i32.const ,(runtime-symbol-id 'unsigned-byte))))))

(define-runtime-function $byte-spec-component
    (($spec :nullable-object) ($position-p :nullable-object)) (:nullable-object)
  (:locals ($size :nullable-object) ($position :nullable-object))
  ;; CAR/CDR already diagnose ordinary non-list Lisp values. Their tag read
  ;; requires an object or immediate before either access.
  (wat-fail-when #@(i32.eqz (i32.or (ref.test (ref null i31) (local.get $spec))
    (ref.test (ref $obj) (local.get $spec)))) :malformed-form)
  #@(if (ref.test (ref $fixnum) (local.get $spec))
    (then (call $validate-bit-integer-layout (local.get $spec))))
  #@(local.set $size (call $car (local.get $spec)))
  #@(local.set $position (call $cdr (local.get $spec)))
  #@(call $check-byte-count-layout (local.get $size) (i32.const 1))
  #@(call $check-byte-count-layout (local.get $position) (i32.const 1))
  #@(select (result (ref null eq)) (local.get $position) (local.get $size)
    (i32.eqz (ref.is_null (local.get $position-p)))))

(define-runtime-function $byte-component (($spec :nullable-object) ($position i32)) (:nullable-object)
  #@(call $byte-spec-component (local.get $spec) (call $bool (local.get $position))))

(defun $extract-integer-field-cl ()
  (runtime-function-definition '$extract-integer-field-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$extract-integer-field-cl 'miricl-runtime:extract-integer-field))))

(define-runtime-function $try-scalar-integer-field
    (($x :nullable-object) ($size :nullable-object) ($position :nullable-object)) (i64 i32)
  (:locals ($value i64) ($width i64) ($shift i64))
  #@(block $miss
    ;; Reject wide fields before decoding the other operands. The checked
    ;; caller still diagnoses errors in its ordinary order after any miss.
    ,(wat-decode-scalar-integer '$size '$width '$miss)
    (br_if $miss (i64.gt_u (local.get $width) (i64.const 63)))
    ,(wat-decode-scalar-integer '$x '$value '$miss)
    ,(wat-decode-scalar-integer '$position '$shift '$miss)
    (br_if $miss (i64.lt_s (local.get $shift) (i64.const 0)))
    ;; Wasm wraps shift counts. Shifting by 63 already gives the sign bits
    ;; for any higher position. Width 63 keeps the masked result in signed i64.
    (local.set $shift (select (local.get $shift) (i64.const 63)
      (i64.lt_s (local.get $shift) (i64.const 63))))
    (return (i64.and
      (i64.shr_s (local.get $value) (local.get $shift))
      (i64.sub (i64.shl (i64.const 1) (local.get $width)) (i64.const 1)))
      (i32.const 0)))
  #@(i64.const 0)
  #@(i32.const 1))

(define-runtime-function $ldb-native
    (($x :nullable-object) ($size :nullable-object) ($position :nullable-object) ($predicate i32)) (:nullable-object)
  (:locals ($field i64))
  #@(block $fallback
    (call $try-scalar-integer-field (local.get $x) (local.get $size) (local.get $position))
    (br_if $fallback)
    (local.set $field)
    (if (local.get $predicate) (then (return (call $bool (i64.ne (local.get $field) (i64.const 0))))))
    (return (call $int64 (local.get $field))))
  ;; A foreign canonical object can hold conflicting tags or private GC
  ;; values. Check the storage before the CL worker reads tags or digits.
  #@(call $validate-bit-integer-layout (local.get $x))
  #@(call $validate-bit-integer-layout (local.get $size))
  #@(call $validate-bit-integer-layout (local.get $position))
  ;; The CL boundary validates VALUE, SIZE and POSITION in that order; counts
  ;; remain arbitrary precision until exact support bounds establish a result.
  #@(call $extract-integer-field-cl (local.get $x) (local.get $size)
    (local.get $position) (call $bool (local.get $predicate))))

(defun $deposit-native-cl ()
  (runtime-function-definition '$deposit-native-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$deposit-native-cl 'miricl-runtime:deposit-integer-field))))

(defun wat-decode-deposit-scalar (source destination miss)
  (runtime-wat-sequence
    (wat-decode-scalar-integer source destination miss)
    ;; The existing decoder checks the physical subtype and integer tag.
    ;; A scalar box also carries a sign header, which must agree with its word.
    #@(if (ref.test (ref $fixnum) (local.get ,source))
      (then
        (br_if ,miss (i32.ne
          (struct.get $obj $n (ref.cast (ref $fixnum) (local.get ,source)))
          (i32.sub (i64.gt_s (local.get ,destination) (i64.const 0))
                   (i64.lt_s (local.get ,destination) (i64.const 0)))))))))

(define-runtime-function $try-deposit-scalar
    (($new :nullable-object) ($spec :nullable-object) ($x :nullable-object) ($aligned i32)) (:nullable-object)
  (:locals ($value i64) ($replacement i64) ($size i64) ($position i64) ($delta i64)
           ($bytespec (ref $obj)) ($component :nullable-object))
  #@(block $fallback
    ;; This probe neither polls nor signals. NIL means that the caller still
    ;; owns the original operands, validation order and application frame.
    ,(wat-decode-deposit-scalar '$x '$value '$fallback)
    ,(wat-decode-deposit-scalar '$new '$replacement '$fallback)
    ;; The numeric subtype must retain its own tag. Ordinary boxed numbers
    ;; still reach the checked list error for a rejected bytespec.
    (br_if $fallback (i32.or (i32.eqz (ref.test (ref $obj) (local.get $spec)))
      (ref.test (ref $fixnum) (local.get $spec))))
    (local.set $bytespec (ref.cast (ref $obj) (local.get $spec)))
    (br_if $fallback (i32.ne
      (i32.and (struct.get $obj $tag (local.get $bytespec)) (i32.const 63))
      (i32.const 7)))
    (local.set $component (struct.get $obj $a (local.get $bytespec)))
    ,(wat-decode-deposit-scalar '$component '$size '$fallback)
    (br_if $fallback (i64.gt_u (local.get $size) (i64.const 63)))
    (local.set $component (struct.get $obj $b (local.get $bytespec)))
    ,(wat-decode-deposit-scalar '$component '$position '$fallback)
    (if (i64.eqz (local.get $size))
      (then
        (br_if $fallback (i64.lt_s (local.get $position) (i64.const 0)))
        (return (local.get $x))))
    ;; An unsigned bound rejects negative positions and ensures that all
    ;; changed bits lie below bit 63, preserving VALUE's infinite sign bits.
    (br_if $fallback (i64.gt_u (local.get $position)
      (i64.sub (i64.const 63) (local.get $size))))
    (if (i32.eqz (local.get $aligned))
      (then (local.set $replacement (i64.shl (local.get $replacement) (local.get $position)))))
    (local.set $delta (i64.and (i64.xor (local.get $value) (local.get $replacement))
      (i64.shl (i64.sub (i64.shl (i64.const 1) (local.get $size)) (i64.const 1))
        (local.get $position))))
    (if (i64.eqz (local.get $delta)) (then (return (local.get $x))))
    (return_call $int64 (i64.xor (local.get $value) (local.get $delta))))
  #@(ref.null eq))

(define-runtime-function $deposit-native
    (($new :nullable-object) ($spec :nullable-object) ($x :nullable-object) ($aligned i32)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $try-deposit-scalar
    (local.get $new) (local.get $spec) (local.get $x) (local.get $aligned)))
  #@(if (i32.eqz (ref.is_null (local.get $result))) (then (return (local.get $result))))
  #@(local.set $x (call $require-ash-integer (local.get $x)))
  #@(local.set $new (call $require-ash-integer (local.get $new)))
  #@(drop (call $byte-component (local.get $spec) (i32.const 1)))
  ;; The first validator can poll after capturing both slots. Check what the
  ;; CL kernel will actually read next. Its slot reads precede any Lisp call.
  #@(call $check-byte-count-layout (call $car (local.get $spec)) (i32.const 0))
  #@(call $check-byte-count-layout (call $cdr (local.get $spec)) (i32.const 0))
  #@(call $deposit-native-cl (local.get $new) (local.get $spec) (local.get $x) (call $bool (local.get $aligned))))

(define-runtime-section byte-fields-runtime-wat ()
  ($check-byte-count-layout) ($byte-spec-component) ($byte-component) ($extract-integer-field-cl)
  ($try-scalar-integer-field) ($ldb-native) ($deposit-native-cl) ($try-deposit-scalar) ($deposit-native))
