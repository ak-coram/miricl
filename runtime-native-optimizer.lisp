;;;; Decode the native emitter's instruction subset without Lisp dispatch for
;;;; each byte. Unsupported encodings use the portable decoder instead.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $native-op-operand
    (($bytes :digits) ($position i32) ($signed i32)) (i64 i32)
  (:locals ($byte i32) ($shift i32) ($value i64))
  #@(loop $next
    (if (i32.or (i32.ge_u (local.get $position) (array.len (local.get $bytes)))
                (i32.ge_u (local.get $shift) (i32.const 35)))
      (then (return (i64.const 0) (i32.const -1))))
    (local.set $byte (array.get $digits (local.get $bytes) (local.get $position)))
    (local.set $position (i32.add (local.get $position) (i32.const 1)))
    (local.set $value (i64.or (local.get $value)
      (i64.shl (i64.extend_i32_u (i32.and (local.get $byte) (i32.const 127)))
        (i64.extend_i32_u (local.get $shift)))))
    (local.set $shift (i32.add (local.get $shift) (i32.const 7)))
    (if (i32.lt_u (local.get $byte) (i32.const 128))
      (then
        (if (i32.and (local.get $signed) (i32.ne (i32.and (local.get $byte) (i32.const 64)) (i32.const 0)))
          (then (local.set $value (i64.sub (local.get $value)
            (i64.shl (i64.const 1) (i64.extend_i32_u (local.get $shift)))))))
        (return (local.get $value) (local.get $position))))
    (br $next))
  #@(unreachable))

;; The operand helper signals unsupported/truncated encodings with -1. The
;; surrounding block returns NIL and lets the Lisp decoder diagnose them.
(defun wat-native-op-operand (signed)
  `(block
    (call $native-op-operand (local.get $bytes) (local.get $position) ,signed)
    (local.set $position) (local.set $value)
    (br_if $unsupported (i32.lt_s (local.get $position) (i32.const 0)))))

(defun wat-native-op-member (codes)
  (reduce (lambda (a b) `(i32.or ,a ,b)) codes
    :key (lambda (code) `(i32.eq (local.get $code) (i32.const ,code)))))

(define-runtime-function $native-read-ops
    (($source :nullable-object) ($references :nullable-object)) (:nullable-object)
  (:locals ($bytes :digits) ($length i32) ($position i32) ($start i32)
           ($offset i32) ($locals i32) ($groups i32) ($count i32)
           ($code i32) ($kind i32) ($i i32) ($size i32) ($capacity i32)
           ($value i64) ($argument :nullable-object) ($depths :nullable-object)
           ($ops (ref $objects)) ($grown (ref $objects)))
  #@(block $unsupported
    ;; Compiler byte vectors have general object storage, including adjustable
    ;; vectors. Resolve their views through the standard array accessor once
    ;; per byte, then decode the unboxed buffer directly.
    (local.set $length (call $array-sequence-length (local.get $source)))
    (local.set $bytes (array.new_default $digits (local.get $length)))
    (loop $copy
      (if (i32.lt_u (local.get $i) (local.get $length))
        (then
          (array.set $digits (local.get $bytes) (local.get $i)
            (call $natural-count (call $array-raw-element (local.get $source)
              (local.get $i) (ref.null eq) (i32.const 0))))
          (br_if $unsupported (i32.gt_u
            (array.get $digits (local.get $bytes) (local.get $i)) (i32.const 255)))
          (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy))))
    (local.set $offset (i32.const 2))
    ,(wat-native-op-operand '(i32.const 0))
    (br_if $unsupported (i64.gt_u (local.get $value) (i64.extend_i32_u (local.get $length))))
    (local.set $groups (i32.wrap_i64 (local.get $value)))
    (block $locals-done (loop $local
      (br_if $locals-done (i32.eqz (local.get $groups)))
      ,(wat-native-op-operand '(i32.const 0))
      (br_if $unsupported (i64.gt_u (local.get $value) (i64.const 2147483647)))
      (br_if $unsupported (i32.ge_u (local.get $position) (local.get $length)))
      (local.set $kind (array.get $digits (local.get $bytes) (local.get $position)))
      (if (i32.eq (local.get $kind) (i32.const 127))
        (then (local.set $offset (i32.add (local.get $offset) (i32.wrap_i64 (local.get $value)))))
        (else
          (br_if $unsupported (i32.ne (local.get $kind) (i32.const 111)))
          (local.set $locals (i32.add (local.get $locals) (i32.wrap_i64 (local.get $value))))))
      (br_if $unsupported (i32.or (i32.lt_s (local.get $offset) (i32.const 0))
                                  (i32.lt_s (local.get $locals) (i32.const 0))))
      (local.set $position (i32.add (local.get $position) (i32.const 1)))
      (local.set $groups (i32.sub (local.get $groups) (i32.const 1))) (br $local)))
    (local.set $capacity (select (local.get $length) (i32.const 256)
      (i32.lt_u (local.get $length) (i32.const 256))))
    (local.set $ops (array.new_default $objects (local.get $capacity)))
    (block $done (loop $next
      (br_if $done (i32.ge_u (local.get $position) (local.get $length)))
      (local.set $start (local.get $position))
      (local.set $code (array.get $digits (local.get $bytes) (local.get $position)))
      (local.set $position (i32.add (local.get $position) (i32.const 1)))
      (local.set $argument (ref.null eq))
      (block $decoded
        (if ,(wat-native-op-member '(#x02 #x03 #x04 #x08 #x0c #x0d #x10 #x12 #x20 #x21 #x22 #x41 #xd0 #xd2))
          (then
            ,(wat-native-op-operand '(i32.eq (local.get $code) (i32.const 65)))
            (local.set $argument (call $int64 (local.get $value))) (br $decoded)))
        (if (i32.eq (local.get $code) (i32.const 14))
          (then
            ,(wat-native-op-operand '(i32.const 0))
            (br_if $unsupported (i64.ge_u (local.get $value) (i64.extend_i32_u (local.get $length))))
            (local.set $count (i32.add (i32.wrap_i64 (local.get $value)) (i32.const 1)))
            (local.set $depths (ref.null eq))
            (loop $depth
              ,(wat-native-op-operand '(i32.const 0))
              (if (ref.is_null (local.get $references))
                (then (local.set $depths (call $cons (call $int64 (local.get $value)) (local.get $depths)))))
              (local.set $count (i32.sub (local.get $count) (i32.const 1)))
              (br_if $depth (local.get $count)))
            (local.set $argument (call $reverse-onto (local.get $depths) (ref.null eq) (i32.const 1)))
            (br $decoded)))
        (if (i32.eq (local.get $code) (i32.const 31))
          (then
            (local.set $position (i32.add (local.get $position) (i32.const 1)))
            ,(wat-native-op-operand '(i32.const 0))
            (br_if $unsupported (i64.gt_u (local.get $value) (i64.extend_i32_u (local.get $length))))
            (local.set $count (i32.wrap_i64 (local.get $value)))
            (block $catches-done (loop $catch
              (br_if $catches-done (i32.eqz (local.get $count)))
              (br_if $unsupported (i32.ge_u (local.get $position) (local.get $length)))
              (local.set $kind (array.get $digits (local.get $bytes) (local.get $position)))
              (local.set $position (i32.add (local.get $position) (i32.const 1)))
              (if (i32.lt_u (local.get $kind) (i32.const 2))
                (then ,(wat-native-op-operand '(i32.const 0))))
              ,(wat-native-op-operand '(i32.const 0))
              (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $catch)))
            (br $decoded)))
        (if (i32.eq (local.get $code) (i32.const 251))
          (then
            ,(wat-native-op-operand '(i32.const 0))
            (local.set $argument (call $int64 (local.get $value)))
            (local.set $count (i32.const 0))
            (block $gc-operands
              (if (i64.eq (local.get $value) (i64.const 2))
                (then (local.set $count (i32.const 2)) (br $gc-operands)))
              (if (i32.or (i64.eq (local.get $value) (i64.const 20))
                    (i32.or (i64.eq (local.get $value) (i64.const 22)) (i64.eq (local.get $value) (i64.const 23))))
                (then (local.set $count (i32.const 1)) (br $gc-operands)))
              (br_if $unsupported (i64.gt_u (i64.sub (local.get $value) (i64.const 26)) (i64.const 3))))
            (block $gc-done (loop $gc-next
              (br_if $gc-done (i32.eqz (local.get $count)))
              ,(wat-native-op-operand '(i32.const 0))
              (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $gc-next)))
            (br $decoded)))
        (br_if $unsupported (i32.eqz ,(wat-native-op-member
          '(#x00 #x05 #x0b #x0f #x1a #x45 #x46 #x48 #x4a #x4c #x4e #x53 #x55 #x57 #x59 #x71 #x7c #x7d #x7e #xa7 #xac #xd1)))))
      (if (i32.or (ref.is_null (local.get $references))
                  ,(wat-native-op-member '(#x10 #x12 #xd2)))
        (then
          (if (i32.eq (local.get $size) (local.get $capacity))
            (then
              (local.set $capacity (i32.mul (local.get $capacity) (i32.const 2)))
              (local.set $grown (array.new_default $objects (local.get $capacity)))
              (array.copy $objects $objects (local.get $grown) (i32.const 0)
                (local.get $ops) (i32.const 0) (local.get $size))
              (local.set $ops (local.get $grown))))
          (array.set $objects (local.get $ops) (local.get $size)
            (call $vector-new (array.new_fixed $objects 9 (call $int (local.get $code))
              (local.get $argument) (call $int (local.get $start)) (call $int (local.get $position))
              (ref.null eq) (call $int (i32.const 0)) (call $int (i32.const 0))
              (call $int (i32.const 0)) (call $int (i32.const 0)))))
          (local.set $size (i32.add (local.get $size) (i32.const 1)))))
      (br $next)))
    (local.set $grown (array.new_default $objects (local.get $size)))
    (array.copy $objects $objects (local.get $grown) (i32.const 0)
      (local.get $ops) (i32.const 0) (local.get $size))
    (return (call $vector-new (array.new_fixed $objects 3
      (call $vector-new (local.get $grown)) (call $int (local.get $offset)) (call $int (local.get $locals))))))
  #@(ref.null eq))

(define-runtime-section native-optimizer-runtime-wat ()
  ($native-op-operand) ($native-read-ops))
