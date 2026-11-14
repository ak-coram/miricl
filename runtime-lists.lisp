;;;; Cons cells and list traversal.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $cons
    (($a :nullable-object) ($b :nullable-object))
    (:object)
  #@(call $node (i32.const 7) (i32.const 0) (local.get $a) (local.get $b) (ref.null eq)))

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

(define-runtime-function $length
    (($list :nullable-object))
    (i32)
  (:locals ($n i32))
  (wat-list-loop ($list)
    #@(local.set $n (i32.add (local.get $n) (i32.const 1))))
  #@(local.get $n))

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

(define-runtime-function $list-accessor
    (($x :nullable-object) ($code i32) ($write i32) ($value :nullable-object)) (:nullable-object)
  #@(block $last (loop $next (br_if $last (i32.le_u (local.get $code) (i32.const 3)))
    (local.set $x (if (result (ref null eq)) (i32.and (local.get $code) (i32.const 1))
      (then (call $cdr (local.get $x))) (else (call $car (local.get $x)))))
    (local.set $code (i32.shr_u (local.get $code) (i32.const 1))) (br $next)))
  #@(if (local.get $write)
    (then
      (if (i32.ne (call $tag (local.get $x)) (i32.const 7))
        (then (call $type-fail (local.get $x) (call $symbol (i32.const ,(runtime-symbol-id 'cons)))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (if (i32.and (local.get $code) (i32.const 1))
        (then (struct.set $obj $b (ref.cast (ref $obj) (local.get $x)) (local.get $value)))
        (else (struct.set $obj $a (ref.cast (ref $obj) (local.get $x)) (local.get $value))))
      (return (local.get $value))))
  #@(if (result (ref null eq)) (i32.and (local.get $code) (i32.const 1))
    (then (call $cdr (local.get $x))) (else (call $car (local.get $x)))))

(define-runtime-section list-runtime-wat ()
  ($cons)
  ($car)
  ($cdr)
  ($length)
  ($arity) ($arity0) ($arity1) ($arity2) ($arity3) ($list-accessor-code) ($list-accessor))

(defun $member-eq ()
  (runtime-function-definition '$member-eq
    (wat-fragment (miricl-cross:runtime-kernel-wat '$member-eq 'miricl-runtime:member-eq :result :truth))))

(define-runtime-function $remove-eq
    (($x :nullable-object) ($list :nullable-object))
    (:nullable-object)
  #@(if (ref.is_null (local.get $list)) (then (return (ref.null eq))))
  #@(if (ref.eq (local.get $x) (call $car (local.get $list))) (then (return (call $cdr (local.get $list)))))
  #@(call $cons (call $car (local.get $list)) (call $remove-eq (local.get $x) (call $cdr (local.get $list)))))

(define-runtime-function $as-list
    (($x :nullable-object))
    (:nullable-object)
  #@(if (result (ref null eq)) (i32.or (ref.is_null (local.get $x)) (i32.eq (call $tag (local.get $x)) (i32.const 7)))
      (then (local.get $x)) (else (call $cons (local.get $x) (ref.null eq)))))

(define-runtime-section list-utilities-runtime-wat ()
  ($member-eq)
  ($remove-eq)
  ($as-list))
