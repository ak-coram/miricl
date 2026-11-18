;;;; Cons cells and list traversal.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $cons
    (($a :nullable-object) ($b :nullable-object))
    (:object)
  #@(call $node (i32.const 7) (i32.const 0) (local.get $a) (local.get $b) (ref.null eq)))

(define-runtime-function $cons2
    (($a :nullable-object) ($b :nullable-object) ($tail :nullable-object)) (:object)
  #@(call $cons (local.get $a) (call $cons (local.get $b) (local.get $tail))))

(define-runtime-function $car
    (($x :nullable-object))
    (:nullable-object)
  #@(if (ref.is_null (local.get $x)) (then (return (ref.null eq))))
  #@(if (i32.ne (call $tag (local.get $x)) (i32.const 7))
    (then (call $type-fail (local.get $x) (call $symbol (i32.const ,(runtime-symbol-id 'list)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(struct.get $obj $a (ref.cast (ref $obj) (local.get $x))))

(define-runtime-function $cdr
    (($x :nullable-object))
    (:nullable-object)
  #@(if (ref.is_null (local.get $x)) (then (return (ref.null eq))))
  #@(if (i32.ne (call $tag (local.get $x)) (i32.const 7))
    (then (call $type-fail (local.get $x) (call $symbol (i32.const ,(runtime-symbol-id 'list)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(struct.get $obj $b (ref.cast (ref $obj) (local.get $x))))

;; Checked proper-list count for internal argument spines. Keep the original
;; raw ABI while rejecting dotted and cyclic input before unchecked callers.
(define-runtime-function $length (($list :nullable-object)) (i32)
  (:locals ($fast :nullable-object) ($slow :nullable-object) ($count i32))
  #@(local.set $fast (local.get $list)) #@(local.set $slow (local.get $list))
  #@(block $done (loop $next
    (br_if $done (ref.is_null (local.get $fast)))
    (local.set $fast (call $cdr (local.get $fast)))
    (local.set $count (i32.add (local.get $count) (i32.const 1)))
    (br_if $done (ref.is_null (local.get $fast)))
    (local.set $fast (call $cdr (local.get $fast)))
    ;; FAST has already checked every cell traversed by SLOW.
    (local.set $slow (struct.get $obj $b (ref.cast (ref $obj) (local.get $slow))))
    (local.set $count (i32.add (local.get $count) (i32.const 1)))
    (if (ref.eq (local.get $fast) (local.get $slow))
      (then (call $type-fail (local.get $list)
        (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'satisfies)))
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'list-length))) (ref.null eq)))
        (i32.const ,(runtime-error-code :expected-designator)))))
    (br $next)))
  #@(local.get $count))

(define-runtime-function $arity
    (($args :nullable-object) ($min i32) ($max i32))
    ()
  (:locals ($n i32))
  #@(local.set $n (call $length (local.get $args)))
  (wat-fail-when #@(i32.or (i32.lt_u (local.get $n) (local.get $min)) (i32.gt_u (local.get $n) (local.get $max))) :wrong-arity))

;; Shared exact checks avoid repeating two constant operands at every call.
;; LENGTH retains the same dotted-list validation as the general checker.
(define-runtime-function $arity0 (($args :nullable-object)) ()
  (wat-fail-when #@(call $length (local.get $args)) :wrong-arity))
(define-runtime-function $arity1 (($args :nullable-object)) ()
  (wat-fail-when #@(i32.ne (call $length (local.get $args)) (i32.const 1)) :wrong-arity))
(define-runtime-function $arity2 (($args :nullable-object)) ()
  (:locals ($tail :nullable-object))
  ;; Ending at NIL after two checked cons cells proves the common successful
  ;; shape without a count or cycle cursor. On every miss, LENGTH still sees
  ;; the original input, preserving dotted/cyclic errors before arity errors.
  #@(block $slow
    (br_if $slow (i32.ne (call $tag (local.get $args)) (i32.const 7)))
    (local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $args))))
    (br_if $slow (i32.ne (call $tag (local.get $tail)) (i32.const 7)))
    (if (ref.is_null (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
      (then (return))))
  (wat-fail-when #@(i32.ne (call $length (local.get $args)) (i32.const 2)) :wrong-arity))
(define-runtime-function $arity3 (($args :nullable-object)) ()
  (wat-fail-when #@(i32.ne (call $length (local.get $args)) (i32.const 3)) :wrong-arity))

(define-runtime-function $list-accessor-code (($id i32)) (i32)
  ;; The low bit selects the first traversal step (A=0/D=1), and the
  ;; highest set bit terminates it. Preserve established bootstrap IDs.
  (runtime-wat-forms
    (loop for (first last base) in '((caar cddr 4) (caaar cdddr 8)) collect
      `(if (i32.le_u (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id first)))
                     (i32.const ,(- (runtime-symbol-id last) (runtime-symbol-id first))))
        (then (return (i32.add (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id first))) (i32.const ,base)))))))
  #@(if (i32.le_u (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'first))) (i32.const 4))
    (then (return (i32.sub (i32.shl (i32.const 3)
      (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'first)))) (i32.const 1)))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'rest))) (then (return (i32.const 3))))
  #@(if (i32.le_u (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'sixth))) (i32.const 4))
    (then (return (i32.sub (i32.shl (i32.const 96)
      (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'sixth)))) (i32.const 1)))))
  #@(if (i32.le_u (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'cadddr))) (i32.const 1))
    (then (return (i32.add (i32.const 23)
      (i32.shl (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'cadddr))) (i32.const 3))))))
  #@(local.set $id (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'caaaar))))
  #@(if (i32.lt_u (local.get $id) (i32.const 14))
    (then (return (i32.add (i32.const 16) (i32.add (local.get $id) (i32.div_u (local.get $id) (i32.const 7)))))))
  #@(i32.const 0))

(defun $read-list-path ()
  (runtime-function-definition '$read-list-path
    (wat-fragment (miricl-cross:runtime-kernel-wat '$read-list-path 'miricl-runtime:read-list-path))))

(defun $write-list-path ()
  (runtime-function-definition '$write-list-path
    (wat-fragment (miricl-cross:runtime-kernel-wat '$write-list-path 'miricl-runtime:write-list-path))))

(define-runtime-function $list-accessor
    (($x :nullable-object) ($code i32) ($write i32) ($value :nullable-object)) (:nullable-object)
  #@(if (result (ref null eq)) (local.get $write)
    (then (call $write-list-path (local.get $x) (call $int (local.get $code)) (local.get $value)))
    (else (call $read-list-path (local.get $x) (call $int (local.get $code))))))

(define-runtime-function $checked-list-path-code (($code i32)) (:object)
  ;; The high terminator bit requires a positive path of at least one step.
  ;; Check the raw import boundary before entering the SAFETY 0 Lisp worker.
  #@(if (i32.lt_s (local.get $code) (i32.const 2))
    (then (call $type-fail (call $int (local.get $code))
      (call $cons2 (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
        (call $int (i32.const 2))
        (call $cons (call $int (i32.const 2147483647)) (ref.null eq)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(call $int (local.get $code)))

(define-runtime-function $native-list-path (($object externref) ($code i32)) (externref)
  (:export-raw "native-list-path")
  (:locals ($checked :nullable-object) ($path :object) ($result :nullable-object))
  #@(local.set $checked (call $native-checked-value (local.get $object)))
  #@(local.set $path (call $checked-list-path-code (local.get $code)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $result (call $read-list-path (local.get $checked) (local.get $path)))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-set-list-path
    (($value externref) ($object externref) ($code i32)) (externref)
  (:export-raw "native-set-list-path")
  (:locals ($checked :nullable-object) ($new :nullable-object) ($path :object) ($result :nullable-object))
  ;; The value-first ABI matches a SETF writer. HIR evaluates place operands
  ;; and the new value before loading these already computed operands.
  #@(local.set $new (call $native-checked-value (local.get $value)))
  #@(local.set $checked (call $native-checked-value (local.get $object)))
  #@(local.set $path (call $checked-list-path-code (local.get $code)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $result (call $write-list-path (local.get $checked) (local.get $path) (local.get $new)))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-section list-runtime-wat ()
  ($cons) ($cons2)
  ($car)
  ($cdr)
  ($length)
  ($arity) ($arity0) ($arity1) ($arity2) ($arity3) ($list-accessor-code) ($read-list-path) ($write-list-path) ($list-accessor)
  ($checked-list-path-code) ($native-list-path) ($native-set-list-path))

(defun $member-eq ()
  (runtime-function-definition '$member-eq
    (wat-fragment (miricl-cross:runtime-kernel-wat '$member-eq 'miricl-runtime:member-eq :result :truth))))

(defun $remove-eq ()
  (runtime-function-definition '$remove-eq
    (wat-fragment (miricl-cross:runtime-kernel-wat '$remove-eq 'miricl-runtime:remove-first-eq))))

(defun $as-list ()
  (runtime-function-definition '$as-list
    (wat-fragment (miricl-cross:runtime-kernel-wat '$as-list 'miricl-runtime:ensure-list))))

(define-runtime-section list-utilities-runtime-wat ()
  ($member-eq)
  ($remove-eq)
  ($as-list))
