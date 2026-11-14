;;;; Complex subtype proofs follow the implementation's three part formats.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

;; Zero denotes an unrelated type; eight denotes an empty complex type.
(define-runtime-function $complex-type-mask (($type :nullable-object) ($env :nullable-object)) (i32)
  (:locals ($args :nullable-object) ($part :nullable-object) ($class :nullable-object))
  #@(local.set $class (call $subtype-class (local.get $type)))
  #@(if (i32.eqz (ref.is_null (local.get $class)))
    (then (local.set $type (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
      ,(clos-library-reference :name))))))
  #@(if (i32.ne (call $id (local.get $type)) (i32.const ,(runtime-symbol-id 'complex)))
    (then
      (if (i32.ne (call $operator (local.get $type)) (i32.const ,(runtime-symbol-id 'complex)))
        (then (return (i32.const 0))))
      (local.set $args (call $cdr (local.get $type)))
      (call $arity (local.get $args) (i32.const 0) (i32.const 1))))
  #@(if (ref.is_null (local.get $args)) (then (return (i32.const 7))))
  #@(local.set $part (call $car (local.get $args)))
  #@(if (i32.eq (call $id (local.get $part)) (i32.const ,(runtime-symbol-id '*)))
    (then (return (i32.const 7))))
  #@(local.set $part (call $upgraded-complex-part-type (local.get $part) (local.get $env)))
  (wat-symbol-case #@(call $id (local.get $part))
    ((nil) #@(return (i32.const 8)))
    (rational #@(return (i32.const 1)))
    (single-float #@(return (i32.const 2)))
    (double-float #@(return (i32.const 4)))
    (float #@(return (i32.const 6)))
    (real #@(return (i32.const 7))))
  #@(i32.const 0))

(defun $complex-part-type ()
  (runtime-function-definition '$complex-part-type
    (wat-fragment (miricl-cross:runtime-kernel-wat '$complex-part-type 'miricl-runtime:complex-component-type))))

(define-runtime-function $complex-subtypep
    (($a :nullable-object) ($b :nullable-object) ($env :nullable-object)) (i32)
  (:locals ($am i32) ($bm i32) ($class :nullable-object) ($base :nullable-object)
           ($result :nullable-object) ($other :nullable-object))
  #@(local.set $am (call $complex-type-mask (local.get $a) (local.get $env)))
  #@(local.set $bm (call $complex-type-mask (local.get $b) (local.get $env)))
  #@(if (i32.eq (local.get $am) (i32.const 8)) (then (return (i32.const 1))))
  #@(if (i32.and (i32.ne (local.get $am) (i32.const 0)) (i32.ne (local.get $bm) (i32.const 0)))
    (then
      (if (i32.eq (local.get $am) (local.get $bm)) (then (return (i32.const 1))))
      (if (i32.ne (i32.and (local.get $am) (local.get $bm)) (local.get $am))
        (then (return (i32.const 2))))
      ;; Unequal upgraded representations require containment of the ORIGINAL
      ;; part types (CLHS SUBTYPEP), not just containment of upgraded formats.
      (local.set $result (call $subtypep-native (call $complex-part-type (local.get $a))
        (call $complex-part-type (local.get $b)) (local.get $env)))
      (if (i32.eqz (ref.is_null (local.get $result))) (then (return (i32.const 1))))
      (return (select (i32.const 2) (i32.const 0)
        (i32.eqz (ref.is_null (call $car (global.get $extra-values))))))))
  #@(if (i32.eqz (i32.or (local.get $am) (local.get $bm))) (then (return (i32.const 0))))
  #@(local.set $other (if (result (ref null eq)) (local.get $am)
    (then (local.get $b)) (else (local.get $a))))
  #@(if (ref.is_null (local.get $other)) (then (return (i32.const 2))))
  #@(local.set $class (call $subtype-class (local.get $other)))
  #@(if (i32.eqz (ref.is_null (local.get $class)))
    (then
      (if (local.get $am)
        (then
          (local.set $base (call $named-class (call $symbol (i32.const ,(runtime-symbol-id 'complex)))))
          (return (select (i32.const 1) (i32.const 2)
            (call $member-eq (local.get $class) (call $car (call $property-cell
              (struct.get $obj $b (ref.cast (ref $obj) (local.get $base))) ,(clos-library-reference :cpl))))))))
      (if (i32.eqz (ref.is_null (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
        ,(clos-library-reference :finalized))))) (then (return (i32.const 2))))))
  #@(if (i32.eq (call $id (local.get $other)) (i32.const ,(runtime-symbol-id 'atom)))
    (then (return (select (i32.const 1) (i32.const 2) (local.get $am)))))
  ;; Numeric emptiness is important here: an empty real interval is a subtype
  ;; of every complex type, while every inhabited real interval is disjoint.
  #@(local.set $result (call $numeric-subtypep (local.get $other) (ref.null eq) (local.get $env)))
  #@(if (i32.eqz (ref.is_null (call $car (global.get $extra-values))))
    (then (return (select (i32.const 1) (i32.const 2)
      (i32.and (i32.eqz (local.get $am)) (i32.eqz (ref.is_null (local.get $result))))))))
  #@(i32.const 0))

(define-runtime-section complex-type-runtime-wat ()
  ($complex-type-mask) ($complex-part-type) ($complex-subtypep))
