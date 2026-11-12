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
        (if ,(wat-native-op-member '(#x02 #x03 #x04 #x08 #x0c #x0d #x10 #x12 #x20 #x21 #x22 #x41 #x42 #xd0 #xd2))
          (then
            ,(wat-native-op-operand '(i32.or (i32.eq (local.get $code) (i32.const 65))
                                           (i32.eq (local.get $code) (i32.const 66))))
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
          '(#x00 #x05 #x0b #x0f #x1a #x45 #x46 #x48 #x4a #x4c #x4e #x50 #x51 #x52 #x53 #x55 #x57 #x59 #x71 #x7c #x7d #x7e #x83 #x84 #x85 #xa7 #xac #xd1)))))
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

;; This kernel only consumes the native emitter's private decoded records,
;; after source/HIR validation. Core modules with other stack effects or
;; non-defaultable local types use the general Lisp pass. Dense stacks hold
;; instruction indices plus one (zero denotes an actual operand), avoiding
;; allocation and Lisp dispatch for each simulated stack operation.
(define-runtime-function $native-stackify-ops
    (($source :nullable-object) ($configuration :nullable-object) ($total i32)) (:nullable-object)
  (:locals ($ops (ref $objects)) ($config (ref $objects)) ($effects (ref $objects))
           ($op (ref $objects)) ($store-op (ref $objects))
           ($stack :digits) ($barriers :digits) ($candidates :digits) ($epochs :digits)
           ($i i32) ($code i32) ($local i32) ($epoch i32) ($sp i32) ($bp i32)
           ($store i32) ($candidate i32) ($slot i32) ($consumed i32) ($produced i32)
           ($effect i32) ($argument i32) ($exceptions i32) ($value :nullable-object))
  #@(local.set $ops (call $vector-data (local.get $source)))
  #@(local.set $config (call $vector-data (local.get $configuration)))
  #@(local.set $exceptions (i32.eqz (ref.is_null
    (array.get $objects (local.get $config) (i32.const 0)))))
  #@(local.set $effects (call $vector-data (array.get $objects (local.get $config) (i32.const 1))))
  #@(local.set $stack (array.new_default $digits (array.len (local.get $ops))))
  #@(local.set $barriers (array.new_default $digits (array.len (local.get $ops))))
  #@(local.set $candidates (array.new_default $digits (local.get $total)))
  #@(local.set $epochs (array.new_default $digits (local.get $total)))
  #@(local.set $epoch (i32.const 1))
  #@(block $done (loop $next
    (br_if $done (i32.ge_u (local.get $i) (array.len (local.get $ops))))
    (block $continue
      (local.set $op (call $vector-data (array.get $objects (local.get $ops) (local.get $i))))
      (local.set $value (array.get $objects (local.get $op) (i32.const 0)))
      (br_if $continue (ref.is_null (local.get $value)))
      (local.set $code (call $natural-count (local.get $value)))
      (local.set $candidate (i32.const 0))
      (if ,(wat-native-op-member '(#x20 #x21 #x22))
        (then (local.set $local (call $natural-count
          (array.get $objects (local.get $op) (i32.const 1))))))
      (if (i32.eq (local.get $code) (i32.const 32))
        (then
          (if (i32.eq (array.get $digits (local.get $epochs) (local.get $local)) (local.get $epoch))
            (then
              (local.set $slot (array.get $digits (local.get $candidates) (local.get $local)))
              (local.set $store (array.get $digits (local.get $stack) (local.get $slot)))
              (if (i32.eqz (local.get $bp))
                (then (local.set $candidate (local.get $store)))
                (else (if (i32.gt_u (local.get $store)
                    (array.get $digits (local.get $barriers) (i32.sub (local.get $bp) (i32.const 1))))
                  (then (local.set $candidate (local.get $store))))))))))
      (if (i32.and (local.get $exceptions) ,(wat-native-op-member '(#x08 #x10 #x12)))
        (then (local.set $epoch (i32.add (local.get $epoch) (i32.const 1)))))
      (if ,(wat-native-op-member '(#x21 #x22))
        (then
          (if (ref.is_null (call $logbitp-native (call $int (local.get $local))
                (array.get $objects (local.get $op) (i32.const 6))))
            (then
              (if (i32.eq (local.get $code) (i32.const 34))
                (then
                  (array.set $objects (local.get $op) (i32.const 0) (ref.null eq))
                  (br $continue)))
              (local.set $code (i32.const 26))
              (array.set $objects (local.get $op) (i32.const 0) (call $int (local.get $code)))))))
      (if ,(wat-native-op-member '(#x00 #x02 #x03 #x04 #x05 #x08 #x0b #x0c #x0d #x0e #x0f #x1f))
        (then
          (local.set $sp (i32.const 0)) (local.set $bp (i32.const 0))
          (local.set $epoch (i32.add (local.get $epoch) (i32.const 1))) (br $continue)))
      (if (i32.and (i32.eq (local.get $code) (i32.const 32))
                  (i32.ne (local.get $candidate) (i32.const 0)))
        (then
          (if (ref.is_null (call $logbitp-native (call $int (local.get $local))
                (array.get $objects (local.get $op) (i32.const 6))))
            (then
              (local.set $store-op (call $vector-data (array.get $objects (local.get $ops)
                (i32.sub (local.get $candidate) (i32.const 1)))))
              (array.set $objects (local.get $store-op) (i32.const 0) (ref.null eq))
              (array.set $objects (local.get $op) (i32.const 0) (ref.null eq))
              (array.set $digits (local.get $barriers) (local.get $bp) (local.get $candidate))
              (local.set $bp (i32.add (local.get $bp) (i32.const 1)))
              (array.set $digits (local.get $stack) (local.get $slot) (i32.const 0))
              (array.set $digits (local.get $epochs) (local.get $local) (i32.const 0))
              (br $continue)))))
      (if (i32.eq (local.get $code) (i32.const 32))
        (then (array.set $digits (local.get $epochs) (local.get $local) (i32.const 0))))
      (local.set $consumed (i32.const 0)) (local.set $produced (i32.const 0))
      (block $effect-done
        (if ,(wat-native-op-member '(#x20 #x41 #x42 #xd0 #xd2))
          (then (local.set $produced (i32.const 1)) (br $effect-done)))
        (if ,(wat-native-op-member '(#x21 #x1a))
          (then (local.set $consumed (i32.const 1)) (br $effect-done)))
        (if ,(wat-native-op-member '(#x22 #x45 #x50 #xa7 #xac #xd1 #xfb))
          (then (local.set $consumed (i32.const 1)) (local.set $produced (i32.const 1)) (br $effect-done)))
        (if ,(wat-native-op-member '(#x46 #x48 #x4a #x4c #x4e #x51 #x52 #x53 #x55 #x57 #x59 #x71 #x7c #x7d #x7e #x83 #x84 #x85))
          (then (local.set $consumed (i32.const 2)) (local.set $produced (i32.const 1)) (br $effect-done)))
        (if ,(wat-native-op-member '(#x10 #x12))
          (then
            (local.set $argument (call $natural-count (array.get $objects (local.get $op) (i32.const 1))))
            (local.set $produced (i32.eq (local.get $code) (i32.const 16)))
            (local.set $consumed (i32.const 2))
            (if (i32.lt_u (local.get $argument) (array.len (local.get $effects)))
              (then
                (local.set $effect (call $natural-count (array.get $objects (local.get $effects) (local.get $argument))))
                (local.set $consumed (i32.shr_u (local.get $effect) (i32.const 1)))
                (local.set $produced (i32.and (local.get $produced) (local.get $effect))))))))
      (block $consumed (loop $consume
        (br_if $consumed (i32.eqz (local.get $consumed)))
        (block $operand (loop $skip-store
          (br_if $operand (i32.eqz (local.get $sp)))
          (local.set $store (array.get $digits (local.get $stack) (i32.sub (local.get $sp) (i32.const 1))))
          (br_if $operand (i32.eqz (local.get $store)))
          (local.set $store-op (call $vector-data (array.get $objects (local.get $ops)
            (i32.sub (local.get $store) (i32.const 1)))))
          (array.set $digits (local.get $epochs)
            (call $natural-count (array.get $objects (local.get $store-op) (i32.const 1))) (i32.const 0))
          (local.set $sp (i32.sub (local.get $sp) (i32.const 1))) (br $skip-store)))
        (if (local.get $sp)
          (then (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
            (local.set $bp (i32.sub (local.get $bp) (i32.const 1)))))
        (local.set $consumed (i32.sub (local.get $consumed) (i32.const 1))) (br $consume)))
      (if ,(wat-native-op-member '(#x21 #x22))
        (then (array.set $digits (local.get $epochs) (local.get $local) (i32.const 0))))
      (if (i32.eq (local.get $code) (i32.const 33))
        (then
          (array.set $digits (local.get $stack) (local.get $sp) (i32.add (local.get $i) (i32.const 1)))
          (array.set $digits (local.get $candidates) (local.get $local) (local.get $sp))
          (array.set $digits (local.get $epochs) (local.get $local) (local.get $epoch))
          (local.set $sp (i32.add (local.get $sp) (i32.const 1))))
        (else (if (local.get $produced)
          (then
            (array.set $digits (local.get $stack) (local.get $sp) (i32.const 0))
            (array.set $digits (local.get $barriers) (local.get $bp) (i32.add (local.get $i) (i32.const 1)))
            (local.set $sp (i32.add (local.get $sp) (i32.const 1)))
            (local.set $bp (i32.add (local.get $bp) (i32.const 1))))))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(call $bool (i32.const 1)))

;; Validate untrusted list spines before any unchecked Lisp traversal. Return
;; an integer count or a private negative status; the Lisp boundary supplies
;; its context and signals COMPILER-INPUT-ERROR. Floyd's algorithm allocates
;; nothing while traversing and never follows an unchecked dotted tail.
(define-runtime-function $compiler-list-length (($fast :nullable-object)) (:nullable-object)
  (:locals ($slow :nullable-object) ($count i64))
  #@(local.set $slow (local.get $fast))
  #@(loop $next
    (if (ref.is_null (local.get $fast)) (then (return (call $int64 (local.get $count)))))
    (if (i32.ne (call $tag (local.get $fast)) (i32.const 7))
      (then (return (call $int (i32.const -1)))))
    (local.set $fast (call $cdr (local.get $fast)))
    (local.set $count (i64.add (local.get $count) (i64.const 1)))
    (if (ref.is_null (local.get $fast)) (then (return (call $int64 (local.get $count)))))
    (if (i32.ne (call $tag (local.get $fast)) (i32.const 7))
      (then (return (call $int (i32.const -1)))))
    (local.set $fast (call $cdr (local.get $fast)))
    (local.set $slow (call $cdr (local.get $slow)))
    (local.set $count (i64.add (local.get $count) (i64.const 1)))
    (if (ref.eq (local.get $fast) (local.get $slow))
      (then (return (call $int (i32.const -2)))))
    (br $next))
  #@(unreachable))

;; The bit-operation selectors are the runtime-bits.lisp truth tables: OR=14,
;; ANDC2=4, NOT(first)=3. Keep arbitrary-width Lisp integers for liveness sets.
(defun wat-native-bitmap (operation left right)
  `(call $boole-native (i32.const ,operation) ,left ,right (i32.const 0)))

(defun wat-native-op-code ()
  '(local.set $code (if (result i32)
    (ref.is_null (array.get $objects (local.get $op) (i32.const 0)))
    (then (i32.const -1))
    (else (call $natural-count (array.get $objects (local.get $op) (i32.const 0)))))))

;; Same block summaries and backwards equations as NATIVE-OP-LIVENESS-LISP.
;; Private decoded ops use fields 5/6 for live-in/out and 7/8 for uses/defs;
;; block records use fields 3/4 for uses/defs and 5/6 for live-in/out.
(define-runtime-function $native-op-liveness
    (($source :nullable-object) ($configuration :nullable-object) ($total i32)) (:nullable-object)
  (:locals ($ops (ref $objects)) ($config (ref $objects)) ($blocks (ref $objects))
           ($block (ref $objects)) ($next-block (ref $objects)) ($op (ref $objects))
           ($zero :nullable-object) ($all :nullable-object) ($uses :nullable-object)
           ($definitions :nullable-object) ($in :nullable-object) ($out :nullable-object)
           ($scan :nullable-object) ($b i32) ($i i32) ($stop i32) ($start i32)
           ($code i32) ($local i32) ($exceptions i32) ($cyclic i32) ($changed i32))
  #@(local.set $ops (call $vector-data (local.get $source)))
  #@(local.set $config (call $vector-data (local.get $configuration)))
  #@(local.set $blocks (call $vector-data (array.get $objects (local.get $config) (i32.const 0))))
  #@(local.set $exceptions (i32.eqz (ref.is_null (array.get $objects (local.get $config) (i32.const 1)))))
  #@(local.set $zero (call $int (i32.const 0)))
  #@(local.set $all (local.get $zero))
  #@(if (local.get $exceptions)
    (then (local.set $all ,(wat-native-bitmap 3
      '(call $ash-native (call $int (i32.const -1)) (call $int (local.get $total))) '(local.get $zero)))))
  #@(block $summaries-done (loop $summary
    (br_if $summaries-done (i32.ge_u (local.get $b) (array.len (local.get $blocks))))
    (local.set $block (call $vector-data (array.get $objects (local.get $blocks) (local.get $b))))
    (local.set $scan (array.get $objects (local.get $block) (i32.const 2)))
    (block $edges-done (loop $edge
      (br_if $edges-done (ref.is_null (local.get $scan)))
      (if (i32.le_u (call $natural-count (call $car (local.get $scan))) (local.get $b))
        (then (local.set $cyclic (i32.const 1))))
      (local.set $scan (call $cdr (local.get $scan))) (br $edge)))
    (array.set $objects (local.get $block) (i32.const 3) (local.get $zero))
    (array.set $objects (local.get $block) (i32.const 4) (local.get $zero))
    (array.set $objects (local.get $block) (i32.const 5) (local.get $zero))
    (array.set $objects (local.get $block) (i32.const 6) (local.get $zero))
    (local.set $i (call $natural-count (array.get $objects (local.get $block) (i32.const 0))))
    (local.set $stop (call $natural-count (array.get $objects (local.get $block) (i32.const 1))))
    (block $ops-done (loop $operation
      (br_if $ops-done (i32.ge_u (local.get $i) (local.get $stop)))
      (local.set $op (call $vector-data (array.get $objects (local.get $ops) (local.get $i))))
      ,(wat-native-op-code)
      (local.set $uses (local.get $zero)) (local.set $definitions (local.get $zero))
      (if ,(wat-native-op-member '(#x20 #x21 #x22))
        (then (local.set $local (call $natural-count (array.get $objects (local.get $op) (i32.const 1))))))
      (if (i32.eq (local.get $code) (i32.const 32))
        (then (local.set $uses (call $ash-native (call $int (i32.const 1)) (call $int (local.get $local)))))
        (else (if (i32.and (local.get $exceptions) ,(wat-native-op-member '(#x08 #x10 #x12)))
          (then (local.set $uses (local.get $all))))))
      (if ,(wat-native-op-member '(#x21 #x22))
        (then (local.set $definitions (call $ash-native (call $int (i32.const 1)) (call $int (local.get $local))))))
      (array.set $objects (local.get $op) (i32.const 7) (local.get $uses))
      (array.set $objects (local.get $op) (i32.const 8) (local.get $definitions))
      (if (i32.eq (local.get $code) (i32.const 32))
        (then
          (if (i32.and
                (ref.is_null (call $logbitp-native (call $int (local.get $local))
                  (array.get $objects (local.get $block) (i32.const 4))))
                (ref.is_null (call $logbitp-native (call $int (local.get $local))
                  (array.get $objects (local.get $block) (i32.const 3)))))
            (then (array.set $objects (local.get $block) (i32.const 3)
              ,(wat-native-bitmap 14 '(array.get $objects (local.get $block) (i32.const 3)) '(local.get $uses))))))
        (else (if (i32.eqz (ref.eq (local.get $uses) (local.get $zero)))
          (then (array.set $objects (local.get $block) (i32.const 3)
            ,(wat-native-bitmap 14 '(array.get $objects (local.get $block) (i32.const 3))
              (wat-native-bitmap 4 '(local.get $uses) '(array.get $objects (local.get $block) (i32.const 4)))))))))
      (if (i32.eqz (ref.eq (local.get $definitions) (local.get $zero)))
        (then (if (ref.is_null (call $logbitp-native (call $int (local.get $local))
                    (array.get $objects (local.get $block) (i32.const 4))))
          (then (array.set $objects (local.get $block) (i32.const 4)
            ,(wat-native-bitmap 14 '(array.get $objects (local.get $block) (i32.const 4)) '(local.get $definitions)))))))
      (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $operation)))
    (local.set $b (i32.add (local.get $b) (i32.const 1))) (br $summary)))
  #@(local.set $changed (i32.const 1))
  #@(loop $solve
    (local.set $changed (i32.const 0))
    (local.set $b (array.len (local.get $blocks)))
    (block $blocks-done (loop $backward
      (br_if $blocks-done (i32.eqz (local.get $b)))
      (local.set $b (i32.sub (local.get $b) (i32.const 1)))
      (local.set $block (call $vector-data (array.get $objects (local.get $blocks) (local.get $b))))
      (local.set $out (local.get $zero))
      (local.set $scan (array.get $objects (local.get $block) (i32.const 2)))
      (block $successors-done (loop $successor
        (br_if $successors-done (ref.is_null (local.get $scan)))
        (local.set $next-block (call $vector-data (array.get $objects (local.get $blocks)
          (call $natural-count (call $car (local.get $scan))))))
        (local.set $out ,(wat-native-bitmap 14 '(local.get $out)
          '(array.get $objects (local.get $next-block) (i32.const 5))))
        (local.set $scan (call $cdr (local.get $scan))) (br $successor)))
      (local.set $in ,(wat-native-bitmap 14 '(array.get $objects (local.get $block) (i32.const 3))
        (wat-native-bitmap 4 '(local.get $out) '(array.get $objects (local.get $block) (i32.const 4)))))
      (if (i32.eqz (i32.and
            (call $eql (local.get $in) (array.get $objects (local.get $block) (i32.const 5)))
            (call $eql (local.get $out) (array.get $objects (local.get $block) (i32.const 6)))))
        (then
          (local.set $changed (local.get $cyclic))
          (array.set $objects (local.get $block) (i32.const 5) (local.get $in))
          (array.set $objects (local.get $block) (i32.const 6) (local.get $out))))
      (br $backward)))
    (br_if $solve (local.get $changed)))
  #@(local.set $b (i32.const 0))
  #@(block $instructions-done (loop $instruction-block
    (br_if $instructions-done (i32.ge_u (local.get $b) (array.len (local.get $blocks))))
    (local.set $block (call $vector-data (array.get $objects (local.get $blocks) (local.get $b))))
    (local.set $out (array.get $objects (local.get $block) (i32.const 6)))
    (local.set $start (call $natural-count (array.get $objects (local.get $block) (i32.const 0))))
    (local.set $i (call $natural-count (array.get $objects (local.get $block) (i32.const 1))))
    (block $block-done (loop $instruction
      (br_if $block-done (i32.le_u (local.get $i) (local.get $start)))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (local.set $op (call $vector-data (array.get $objects (local.get $ops) (local.get $i))))
      ,(wat-native-op-code)
      (array.set $objects (local.get $op) (i32.const 6) (local.get $out))
      (local.set $uses (array.get $objects (local.get $op) (i32.const 7)))
      (local.set $definitions (array.get $objects (local.get $op) (i32.const 8)))
      (if (i32.eqz (ref.eq (local.get $definitions) (local.get $zero)))
        (then
          (local.set $local (call $natural-count (array.get $objects (local.get $op) (i32.const 1))))
          (if (i32.eqz (ref.is_null (call $logbitp-native (call $int (local.get $local)) (local.get $out))))
            (then (local.set $out ,(wat-native-bitmap 4 '(local.get $out) '(local.get $definitions)))))))
      (if (i32.eqz (ref.eq (local.get $uses) (local.get $zero)))
        (then
          (block $already-live
            (if (i32.eq (local.get $code) (i32.const 32))
              (then
                (local.set $local (call $natural-count (array.get $objects (local.get $op) (i32.const 1))))
                (br_if $already-live (i32.eqz (ref.is_null
                  (call $logbitp-native (call $int (local.get $local)) (local.get $out)))))))
            (local.set $out ,(wat-native-bitmap 14 '(local.get $out) '(local.get $uses))))))
      (array.set $objects (local.get $op) (i32.const 5) (local.get $out))
      (br $instruction)))
    (local.set $b (i32.add (local.get $b) (i32.const 1))) (br $instruction-block)))
  #@(call $bool (i32.const 1)))

;;;; The native instruction simplifier. Keep the Lisp implementation as the
;;;; reference and host fallback; this kernel only changes code/argument slots.
(defun wat-native-simplify-field (name index)
  `(array.get $objects (ref.as_non_null (local.get ,name)) (i32.const ,index)))

(defun wat-native-simplify-clear (name)
  `(array.set $objects (ref.as_non_null (local.get ,name)) (i32.const 0) (ref.null eq)))

(define-runtime-function $native-simplify-code (($op (ref null $objects))) (i32)
  (:locals ($value :nullable-object))
  #@(if (ref.is_null (local.get $op)) (then (return (i32.const -1))))
  #@(local.set $value ,(wat-native-simplify-field '$op 0))
  #@(if (ref.is_null (local.get $value)) (then (return (i32.const -1))))
  #@(call $natural-count (local.get $value)))

(define-runtime-function $native-simplify-argument (($op (ref null $objects))) (i32)
  #@(i32.wrap_i64 (call $scalar-integer-value
    (ref.as_non_null ,(wat-native-simplify-field '$op 1)))))

(defun wat-native-fold-i32-cases ()
  (apply #'runtime-wat-sequence
    (loop for (code opcode) in
      '((#x46 i32.eq) (#x47 i32.ne) (#x48 i32.lt_s) (#x49 i32.lt_u)
        (#x4a i32.gt_s) (#x4b i32.gt_u) (#x4c i32.le_s) (#x4d i32.le_u)
        (#x4e i32.ge_s) (#x4f i32.ge_u) (#x6a i32.add) (#x6b i32.sub)
        (#x6c i32.mul) (#x71 i32.and) (#x72 i32.or) (#x73 i32.xor)
        (#x74 i32.shl) (#x75 i32.shr_s) (#x76 i32.shr_u))
      collect (wat-fragment (wat-string
        `(if (i32.eq (local.get $code) (i32.const ,code))
           (then (return (,opcode (local.get $left) (local.get $right))))))))))

(define-runtime-function $native-fold-i32 (($code i32) ($left i32) ($right i32)) (i32)
  (wat-native-fold-i32-cases)
  #@(unreachable))

(define-runtime-function $native-simplify-matches (($ops (ref $objects))) ((ref $digits) (ref $digits))
  (:locals ($ends (ref $digits)) ($elses (ref $digits)) ($stack (ref $digits))
           ($op (ref $objects)) ($i i32) ($top i32) ($code i32) ($start i32))
  #@(local.set $ends (array.new $digits (i32.const -1) (array.len (local.get $ops))))
  #@(local.set $elses (array.new $digits (i32.const -1) (array.len (local.get $ops))))
  #@(local.set $stack (array.new_default $digits (array.len (local.get $ops))))
  #@(block $done (loop $next
    (br_if $done (i32.ge_u (local.get $i) (array.len (local.get $ops))))
    (local.set $op (call $vector-data (array.get $objects (local.get $ops) (local.get $i))))
    (local.set $code (call $native-simplify-code (local.get $op)))
    (if ,(wat-native-op-member '(#x02 #x03 #x04 #x1f))
      (then
        (array.set $digits (local.get $stack) (local.get $top) (local.get $i))
        (local.set $top (i32.add (local.get $top) (i32.const 1)))))
    (if (i32.eq (local.get $code) (i32.const 5))
      (then (array.set $digits (local.get $elses)
        (array.get $digits (local.get $stack) (i32.sub (local.get $top) (i32.const 1))) (local.get $i))))
    (if (i32.and (i32.eq (local.get $code) (i32.const 11)) (i32.gt_u (local.get $top) (i32.const 0)))
      (then
        (local.set $top (i32.sub (local.get $top) (i32.const 1)))
        (array.set $digits (local.get $ends)
          (array.get $digits (local.get $stack) (local.get $top)) (local.get $i))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(local.get $ends)
  #@(local.get $elses))

(define-runtime-function $native-simplify-ops (($source :nullable-object) ($bytes :nullable-object)) (:nullable-object)
  (:locals ($ops (ref $objects)) ($op (ref $objects))
           ($previous (ref null $objects)) ($prior (ref null $objects))
           ($active (ref $digits)) ($ends (ref null $digits)) ($elses (ref null $digits))
           ($i i32) ($top i32) ($code i32) ($previous-code i32) ($prior-code i32)
           ($argument i32) ($result i32) ($dead i32) ($clean i32) ($void i32)
           ($zero i32) ($otherwise i32) ($end i32) ($j i32) ($limit i32))
  #@(local.set $ops (call $vector-data (local.get $source)))
  #@(local.set $active (array.new_default $digits (array.len (local.get $ops))))
  #@(block $done (loop $next
    (br_if $done (i32.ge_u (local.get $i) (array.len (local.get $ops))))
    (block $continue
      (local.set $op (call $vector-data (array.get $objects (local.get $ops) (local.get $i))))
      (local.set $code (call $native-simplify-code (local.get $op)))
      (br_if $continue (i32.lt_s (local.get $code) (i32.const 0)))
      (if (i32.eq (local.get $code) (i32.const 1))
        (then ,(wat-native-simplify-clear '$op) (br $continue)))
      (if (i32.and (local.get $dead)
            (i32.eqz ,(wat-native-op-member '(#x02 #x03 #x04 #x05 #x0b #x1f))))
        (then ,(wat-native-simplify-clear '$op) (br $continue)))
      (local.set $previous (ref.null $objects)) (local.set $prior (ref.null $objects))
      (if (i32.gt_u (local.get $top) (i32.const 0))
        (then (local.set $previous (call $vector-data (array.get $objects (local.get $ops)
          (array.get $digits (local.get $active) (i32.sub (local.get $top) (i32.const 1))))))))
      (if (i32.gt_u (local.get $top) (i32.const 1))
        (then (local.set $prior (call $vector-data (array.get $objects (local.get $ops)
          (array.get $digits (local.get $active) (i32.sub (local.get $top) (i32.const 2))))))))
      (local.set $previous-code (call $native-simplify-code (local.get $previous)))
      (local.set $prior-code (call $native-simplify-code (local.get $prior)))
      (block $rewritten
        (if (i32.eq (local.get $code) (i32.const 209))
          (then
            (if (i32.or (i32.eq (local.get $previous-code) (i32.const 208))
                        (i32.eq (local.get $previous-code) (i32.const 210)))
              (then
                (array.set $objects (ref.as_non_null (local.get $previous)) (i32.const 1)
                  (call $int (i32.eq (local.get $previous-code) (i32.const 208))))
                (array.set $objects (ref.as_non_null (local.get $previous)) (i32.const 0) (call $int (i32.const 65)))
                ,(wat-native-simplify-clear '$op)))
            (br $rewritten)))
        (if (i32.eq (local.get $code) (i32.const 69))
          (then
            (if (i32.eq (local.get $previous-code) (i32.const 65))
              (then
                (array.set $objects (ref.as_non_null (local.get $previous)) (i32.const 1)
                  (call $int (i32.eqz (call $native-simplify-argument (local.get $previous)))))
                ,(wat-native-simplify-clear '$op)))
            (br $rewritten)))
        (if ,(wat-native-op-member '(#x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f
                                    #x6a #x6b #x6c #x71 #x72 #x73 #x74 #x75 #x76))
          (then
            (if (i32.and (i32.eq (local.get $previous-code) (i32.const 65))
                         (i32.eq (local.get $prior-code) (i32.const 65)))
              (then
                (local.set $result (call $native-fold-i32 (local.get $code)
                  (call $native-simplify-argument (local.get $prior))
                  (call $native-simplify-argument (local.get $previous))))
                (array.set $objects (ref.as_non_null (local.get $prior)) (i32.const 1) (call $int (local.get $result)))
                ,(wat-native-simplify-clear '$previous) ,(wat-native-simplify-clear '$op)
                (local.set $top (i32.sub (local.get $top) (i32.const 1))) (br $rewritten)))
            (if (i32.eq (local.get $previous-code) (i32.const 65))
              (then
                (local.set $argument (call $native-simplify-argument (local.get $previous)))
                (if (i32.or
                      (i32.and ,(wat-native-op-member '(#x6a #x6b #x72 #x73)) (i32.eqz (local.get $argument)))
                      (i32.or
                        (i32.and (i32.eq (local.get $code) (i32.const 108)) (i32.eq (local.get $argument) (i32.const 1)))
                        (i32.and (i32.eq (local.get $code) (i32.const 113)) (i32.eq (local.get $argument) (i32.const -1)))))
                  (then ,(wat-native-simplify-clear '$previous) ,(wat-native-simplify-clear '$op)
                    (local.set $top (i32.sub (local.get $top) (i32.const 1)))))))
            (br $rewritten)))
        (if (i32.eq (local.get $code) (i32.const 36))
          (then
            (if (i32.eq (local.get $previous-code) (i32.const 35))
              (then (if (i32.eq (call $native-simplify-argument (local.get $op))
                               (call $native-simplify-argument (local.get $previous)))
                (then ,(wat-native-simplify-clear '$previous) ,(wat-native-simplify-clear '$op)
                  (local.set $top (i32.sub (local.get $top) (i32.const 1)))))))
            (br $rewritten)))
        (if (i32.eq (local.get $code) (i32.const 26))
          (then
            (block $drop-done (loop $drop
              (br_if $drop-done (i32.eqz (local.get $top)))
              (local.set $previous (call $vector-data (array.get $objects (local.get $ops)
                (array.get $digits (local.get $active) (i32.sub (local.get $top) (i32.const 1))))))
              (local.set $previous-code (call $native-simplify-code (local.get $previous)))
              (if ,(subst '$previous-code '$code (wat-native-op-member '(#x20 #x23 #x41 #x42 #x43 #x44 #xd0 #xd2)))
                (then ,(wat-native-simplify-clear '$previous) ,(wat-native-simplify-clear '$op)
                  (local.set $top (i32.sub (local.get $top) (i32.const 1))) (br $drop-done)))
              (if (i32.eq (local.get $previous-code) (i32.const 34))
                (then
                  (array.set $objects (ref.as_non_null (local.get $previous)) (i32.const 0) (call $int (i32.const 33)))
                  ,(wat-native-simplify-clear '$op) (br $drop-done)))
              (if ,(subst '$previous-code '$code
                  (wat-native-op-member '(#x45 #x50 #x67 #x68 #x69 #x79 #x7a #x7b #xa7 #xac #xad #xc0 #xc1 #xc2 #xc3 #xc4 #xd1)))
                (then ,(wat-native-simplify-clear '$previous)
                  (local.set $top (i32.sub (local.get $top) (i32.const 1))) (br $drop)))
              (br $drop-done)))
            (br $rewritten)))
        (if ,(wat-native-op-member '(#x04 #x0d))
          (then
            (if (if (result i32) (i32.eq (local.get $previous-code) (i32.const 65))
                  (then (if (result i32) (i32.eq (local.get $code) (i32.const 4))
                    (then (i32.const 1))
                    (else (i32.eq (call $natural-count (call $array-raw-element (local.get $bytes)
                      (call $natural-count ,(wat-native-simplify-field '$op 2)) (ref.null eq) (i32.const 0))) (i32.const 13)))))
                  (else (i32.const 0)))
              (then
                (local.set $zero (i32.eqz (call $native-simplify-argument (local.get $previous))))
                ,(wat-native-simplify-clear '$previous)
                (local.set $top (i32.sub (local.get $top) (i32.const 1)))
                (local.set $clean (i32.const 1))
                (if (i32.eq (local.get $code) (i32.const 13))
                  (then
                    (if (local.get $zero) (then ,(wat-native-simplify-clear '$op))
                      (else (array.set $objects (local.get $op) (i32.const 0) (call $int (i32.const 12))))))
                  (else
                    (if (ref.is_null (local.get $ends))
                      (then (call $native-simplify-matches (local.get $ops)) (local.set $elses) (local.set $ends)))
                    (local.set $otherwise (array.get $digits (ref.as_non_null (local.get $elses)) (local.get $i)))
                    (local.set $end (array.get $digits (ref.as_non_null (local.get $ends)) (local.get $i)))
                    (array.set $objects (local.get $op) (i32.const 0) (call $int (i32.const 2)))
                    (local.set $j (select (i32.add (local.get $i) (i32.const 1))
                      (select (local.get $otherwise) (local.get $end) (i32.ge_s (local.get $otherwise) (i32.const 0)))
                      (local.get $zero)))
                    (local.set $limit (select
                      (select (i32.add (local.get $otherwise) (i32.const 1)) (local.get $end)
                        (i32.ge_s (local.get $otherwise) (i32.const 0))) (local.get $end) (local.get $zero)))
                    (block $pruned (loop $prune
                      (br_if $pruned (i32.ge_u (local.get $j) (local.get $limit)))
                      (array.set $objects (call $vector-data (array.get $objects (local.get $ops) (local.get $j)))
                        (i32.const 0) (ref.null eq))
                      (local.set $j (i32.add (local.get $j) (i32.const 1))) (br $prune))))))))))
      (local.set $code (call $native-simplify-code (local.get $op)))
      (if (i32.ge_s (local.get $code) (i32.const 0))
        (then
          (array.set $digits (local.get $active) (local.get $top) (local.get $i))
          (local.set $top (i32.add (local.get $top) (i32.const 1)))
          (if ,(wat-native-op-member '(#x02 #x03))
            (then (if (call $eql ,(wat-native-simplify-field '$op 1) (call $int (i32.const 64)))
              (then (local.set $void (i32.const 1))))))
          (local.set $dead ,(wat-native-op-member '(#x00 #x08 #x0c #x0e #x0f #x12))))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  ;; Success plus clean-scopes/void-scopes flags; zero is never an unsupported
  ;; marker (NIL is the host fallback marker).
  #@(call $int (i32.or (i32.const 1)
    (i32.or (i32.shl (local.get $clean) (i32.const 1)) (i32.shl (local.get $void) (i32.const 2))))))

(define-runtime-section native-optimizer-runtime-wat ()
  (if *include-compiler*
      (runtime-wat-sequence
        ($native-op-operand) ($native-read-ops) ($native-stackify-ops) ($compiler-list-length)
        ($native-op-liveness) ($native-simplify-code) ($native-simplify-argument)
        ($native-fold-i32) ($native-simplify-matches) ($native-simplify-ops))
      ($compiler-list-length)))
