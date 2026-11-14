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

(defun $length-cl ()
  (runtime-function-definition '$length-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$length-cl 'miricl-runtime:proper-list-count))))

(define-runtime-function $length (($list :nullable-object)) (i32)
  #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null (call $length-cl (local.get $list))))))

(defun $arity-cl ()
  (runtime-function-definition '$arity-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$arity-cl 'miricl-runtime:check-arity-count))))

(define-runtime-function $arity (($args :nullable-object) ($min i32) ($max i32)) ()
  #@(drop (call $arity-cl (call $int (call $length (local.get $args))) (call $int (local.get $min)) (call $int (local.get $max)))))

(defun $arity0-cl ()
  (runtime-function-definition '$arity0-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$arity0-cl 'miricl-runtime:check-zero-arity))))

(define-runtime-function $arity0 (($args :nullable-object)) ()
  #@(drop (call $arity0-cl (call $int (call $length (local.get $args))))))

(defun $arity1-cl ()
  (runtime-function-definition '$arity1-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$arity1-cl 'miricl-runtime:check-one-arity))))

(define-runtime-function $arity1 (($args :nullable-object)) ()
  #@(drop (call $arity1-cl (call $int (call $length (local.get $args))))))

(defun $arity2-cl ()
  (runtime-function-definition '$arity2-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$arity2-cl 'miricl-runtime:check-two-arity))))

(define-runtime-function $arity2 (($args :nullable-object)) ()
  #@(drop (call $arity2-cl (call $int (call $length (local.get $args))))))

(defun $arity3-cl ()
  (runtime-function-definition '$arity3-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$arity3-cl 'miricl-runtime:check-three-arity))))

(define-runtime-function $arity3 (($args :nullable-object)) ()
  #@(drop (call $arity3-cl (call $int (call $length (local.get $args))))))

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

(define-runtime-section list-runtime-wat ()
  ($cons)
  ($car)
  ($cdr)
  ($length-cl) ($length)
  ($arity-cl) ($arity) ($arity0-cl) ($arity0) ($arity1-cl) ($arity1) ($arity2-cl) ($arity2) ($arity3-cl) ($arity3) ($list-accessor-code) ($read-list-path) ($write-list-path) ($list-accessor))

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
