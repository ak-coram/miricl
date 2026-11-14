;;;; General vectors own their element array in a WasmGC object subtype.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $vector-data (($x :nullable-object)) ((ref $objects))
  #@(struct.get $vector $data (ref.cast (ref $vector) (local.get $x))))

(define-runtime-function $vector-new (($data (ref $objects))) (:object)
  #@(struct.new $vector (call $allocate-object-tag (i32.const 34)) (i32.const 0) (f64.const 0)
    (ref.null $digits) (ref.null eq) (ref.null eq) (ref.null eq) (local.get $data)))

(define-runtime-function $make-vector-native (($size :nullable-object) ($initial :nullable-object)) (:object)
  #@(call $vector-new (array.new $objects (local.get $initial)
    (call $bounded-index (local.get $size) (i32.const 536870910)))))

(define-runtime-function $vector-from-list (($list :nullable-object) ($length i32)) (:object)
  (wat-fail-when #@(i32.gt_u (local.get $length) (i32.const 536870910)) :allocation)
  #@(ref.as_non_null (call $fill-vector-from-list
    (call $vector-new (array.new_default $objects (local.get $length))) (local.get $list))))

(defun $fill-vector-from-list ()
  (runtime-function-definition '$fill-vector-from-list
    (wat-fragment (miricl-cross:runtime-kernel-wat '$fill-vector-from-list 'miricl-runtime:fill-vector-from-list))))

(defun $vector-list ()
  (runtime-function-definition '$vector-list
    (wat-fragment (miricl-cross:runtime-kernel-wat '$vector-list 'miricl-runtime:vector-list-copy))))

(defun $vector-slice-cl ()
  (runtime-function-definition '$vector-slice-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$vector-slice-cl 'miricl-runtime:slice-simple-vector))))

(define-runtime-function $vector-slice (($vector :nullable-object) ($first i32) ($count i32)) (:object)
  #@(ref.as_non_null (call $vector-slice-cl (local.get $vector) (call $int (local.get $first)) (call $int (local.get $count)))))

(define-runtime-function $vector-reverse (($vector :nullable-object) ($write i32)) (:nullable-object)
  #@(if (i32.eqz (local.get $write))
    (then (local.set $vector (call $vector-slice (local.get $vector) (i32.const 0)
      (array.len (call $vector-data (local.get $vector)))))))
  #@(call $vector-reverse-in-place (local.get $vector)))

(defun $vector-reverse-in-place ()
  (runtime-function-definition '$vector-reverse-in-place
    (wat-fragment (miricl-cross:runtime-kernel-wat '$vector-reverse-in-place 'miricl-runtime:reverse-simple-vector-in-place))))

(define-runtime-function $vector-element
    (($vector :nullable-object) ($index :nullable-object) ($value :nullable-object) ($write i32)) (:nullable-object)
  (:locals ($length i32) ($n i32) ($data (ref $objects)))
  #@(if (i32.ne (call $tag (local.get $vector)) (i32.const 34))
    (then (call $type-fail (local.get $vector) (call $symbol (i32.const ,(runtime-symbol-id 'simple-vector)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(local.set $data (call $vector-data (local.get $vector)))
  #@(local.set $length (array.len (local.get $data)))
  #@(local.set $n (call $natural-count (local.get $index)))
  #@(if (i32.ge_u (local.get $n) (local.get $length))
    (then (call $type-fail (local.get $index)
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
        (call $cons (call $int (i32.const 0))
          (call $cons (call $cons (call $int (local.get $length)) (ref.null eq)) (ref.null eq))))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(if (local.get $write)
    (then (array.set $objects (local.get $data) (local.get $n) (local.get $value)) (return (local.get $value))))
  #@(array.get $objects (local.get $data) (local.get $n)))

(defun $vector-equalp-cl ()
  (runtime-function-definition '$vector-equalp-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$vector-equalp-cl 'miricl-runtime:vector-elements-equalp :result :truth))))

(define-runtime-function $vector-equalp (($a :nullable-object) ($b :nullable-object)) (i32)
  #@(call $vector-equalp-cl (local.get $a) (local.get $b)))

(define-runtime-function $source-vector (($size :nullable-object)) (:nullable-object)
  (:locals ($items :nullable-object) ($scan :nullable-object) ($count i32) ($length i32))
  #@(local.set $items (call $source-list))
  #@(if (i32.or (global.get $text-status) (global.get $source-suppress)) (then (return (ref.null eq))))
  #@(local.set $scan (local.get $items))
  #@(block $counted (loop $count (br_if $counted (ref.is_null (local.get $scan)))
    ,(wat-fail-when #@(i32.ne (call $tag (local.get $scan)) (i32.const 7)) :reader-syntax)
    (local.set $count (i32.add (local.get $count) (i32.const 1)))
    (local.set $scan (call $cdr (local.get $scan))) (br $count)))
  #@(local.set $length (local.get $count))
  #@(if (i32.eqz (ref.is_null (local.get $size)))
    (then (local.set $length (call $natural-count (local.get $size)))
      ,(wat-fail-when #@(i32.or (i32.lt_u (local.get $length) (local.get $count))
        (i32.and (i32.ne (local.get $length) (i32.const 0)) (i32.eqz (local.get $count)))) :reader-syntax)))
  #@(call $vector-from-list (local.get $items) (local.get $length)))

(define-runtime-function $read-vector () (:nullable-object)
  (:locals ($length i32) ($i i32) ($data (ref $objects)))
  #@(local.set $length (call $read32))
  (wat-fail-when #@(i32.gt_u (local.get $length)
    (i32.div_u (i32.sub (global.get $limit) (global.get $cursor)) (i32.const 4))) :malformed-form)
  #@(local.set $data (array.new_default $objects (local.get $length)))
  #@(block $done (loop $next (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (array.set $objects (local.get $data) (local.get $i) (call $read))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(call $vector-new (local.get $data)))

(define-runtime-function $write-vector (($vector :nullable-object)) ()
  (:locals ($data (ref $objects)) ($i i32))
  #@(local.set $data (call $vector-data (local.get $vector)))
  #@(call $write32 (array.len (local.get $data)))
  #@(block $done (loop $next (br_if $done (i32.eq (local.get $i) (array.len (local.get $data))))
    (call $write (array.get $objects (local.get $data) (local.get $i)))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next))))

(define-runtime-function $print-vector (($vector :nullable-object) ($depth i32) ($aesthetic i32)) ()
  (:locals ($data (ref $objects)) ($i i32) ($limit i32))
  #@(if (call $print-array-suppressed (local.get $vector)) (then (return)))
  #@(if (i32.ge_u (local.get $depth) (call $print-limit (i32.const ,(runtime-symbol-id '*print-level*))))
    (then (call $emit-char (i32.const 35)) (return)))
  #@(local.set $limit (call $print-limit (i32.const ,(runtime-symbol-id '*print-length*))))
  #@(local.set $data (call $vector-data (local.get $vector)))
  (wat-string (emit-text "#("))
  #@(block $done (loop $next (br_if $done (i32.eq (local.get $i) (array.len (local.get $data))))
    (if (local.get $i) (then (call $emit-char (i32.const 32))))
    (if (i32.eq (local.get $i) (local.get $limit)) (then ,(emit-text "...") (br $done)))
    (if (local.get $aesthetic)
      (then (call $print-aesthetic-depth (array.get $objects (local.get $data) (local.get $i)) (i32.add (local.get $depth) (i32.const 1))))
      (else (call $print-object (array.get $objects (local.get $data) (local.get $i)) (i32.add (local.get $depth) (i32.const 1)))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(call $emit-char (i32.const 41)))

(define-runtime-section vector-runtime-wat ()
  ($vector-data) ($vector-new) ($make-vector-native) ($fill-vector-from-list) ($vector-from-list) ($vector-list) ($vector-slice-cl) ($vector-slice) ($vector-reverse-in-place) ($vector-reverse)
  ($vector-element) ($vector-equalp-cl) ($vector-equalp)
  ($source-vector) ($read-vector) ($write-vector) ($print-vector))
