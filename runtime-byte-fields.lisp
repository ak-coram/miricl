;;;; Byte-field adapters to ordinary CL extraction and replacement workers.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun $byte-spec-component ()
  (runtime-function-definition '$byte-spec-component
    (wat-fragment (miricl-cross:runtime-kernel-wat '$byte-spec-component 'miricl-runtime:byte-spec-component))))

(define-runtime-function $byte-component (($spec :nullable-object) ($position i32)) (:nullable-object)
  #@(call $byte-spec-component (local.get $spec) (call $bool (local.get $position))))

(defun $extract-integer-field-cl ()
  (runtime-function-definition '$extract-integer-field-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$extract-integer-field-cl 'miricl-runtime:extract-integer-field))))

(define-runtime-function $ldb-native
    (($x :nullable-object) ($size :nullable-object) ($position :nullable-object) ($predicate i32)) (:nullable-object)
  (:locals ($value i64) ($width i64) ($shift i64) ($field i64))
  #@(block $fallback
    ,(wat-decode-scalar-integer '$x '$value '$fallback)
    ,(wat-decode-scalar-integer '$size '$width '$fallback)
    ,(wat-decode-scalar-integer '$position '$shift '$fallback)
    (br_if $fallback (i64.gt_u (local.get $width) (i64.const 63)))
    (br_if $fallback (i64.lt_s (local.get $shift) (i64.const 0)))
    ;; Wasm wraps shift counts. Shifting by 63 already gives the sign bits
    ;; for any higher position. Width 63 keeps the masked result in signed i64.
    (local.set $shift (select (local.get $shift) (i64.const 63)
      (i64.lt_s (local.get $shift) (i64.const 63))))
    (local.set $field (i64.and
      (i64.shr_s (local.get $value) (local.get $shift))
      (i64.sub (i64.shl (i64.const 1) (local.get $width)) (i64.const 1))))
    (if (local.get $predicate) (then (return (call $bool (i64.ne (local.get $field) (i64.const 0))))))
    (return (call $int64 (local.get $field))))
  ;; A foreign canonical cons can hold private GC values. Check the fallback
  ;; operands' shapes before the CL worker reads their Lisp tags.
  (runtime-wat-forms
    (loop for operand in '($x $size $position) collect
      (wat-fail-when #@(i32.eqz (i32.or (ref.test (ref null i31) (local.get ,operand))
        (ref.test (ref $obj) (local.get ,operand)))) :malformed-form)))
  ;; The CL boundary validates VALUE, SIZE and POSITION in that order; counts
  ;; remain arbitrary precision until exact support bounds establish a result.
  #@(call $extract-integer-field-cl (local.get $x) (local.get $size)
    (local.get $position) (call $bool (local.get $predicate))))

(defun $deposit-native-cl ()
  (runtime-function-definition '$deposit-native-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$deposit-native-cl 'miricl-runtime:deposit-integer-field))))

(define-runtime-function $deposit-native
    (($new :nullable-object) ($spec :nullable-object) ($x :nullable-object) ($aligned i32)) (:nullable-object)
  #@(local.set $x (call $require-integer (local.get $x)))
  #@(local.set $new (call $require-integer (local.get $new)))
  #@(drop (call $byte-component (local.get $spec) (i32.const 1)))
  #@(call $deposit-native-cl (local.get $new) (local.get $spec) (local.get $x) (call $bool (local.get $aligned))))

(define-runtime-section byte-fields-runtime-wat ()
  ($byte-spec-component) ($byte-component) ($extract-integer-field-cl) ($ldb-native) ($deposit-native-cl) ($deposit-native))
