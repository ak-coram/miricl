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
