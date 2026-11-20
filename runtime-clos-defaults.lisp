;;;; Standard CLOS method bodies. Calls to extensible MOP operations stay generic.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun clos-default-arguments (arguments &optional (tail #@(ref.null eq)))
  (reduce (lambda (argument rest)
            (wat-fragment #@(call $cons ,(if (stringp argument) (wat-fragment argument) argument) ,rest)))
    arguments :from-end t :initial-value (if (stringp tail) (wat-fragment tail) tail)))

(defun clos-default-call (name arguments &optional (tail #@(ref.null eq) tail-supplied-p))
  (if (and (not tail-supplied-p) (= (length arguments) 1))
      (wat-fragment #@(call $clos-default-call1 ,(clos-library-reference name)
        ,(let ((argument (first arguments)))
           (if (stringp argument) (wat-fragment argument) argument))))
      (wat-fragment #@(call $apply ,(clos-library-reference name) ,(clos-default-arguments arguments tail)))))

(define-runtime-function $clos-default-call1
    (($name :nullable-object) ($argument :nullable-object)) (:nullable-object)
  (:locals ($function :nullable-object) ($hit i32)
           ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object)
           ($restart-associations :nullable-object) ($catches :nullable-object) ($targets :nullable-object)
           ($thrown i32) ($result :nullable-object)
           ($backtrace-depth i32) ($backtrace-slot i32) ($backtrace-frame :nullable-object))
  ;; Evaluate the operand before resolving the name: it may call Lisp that
  ;; changes the global function binding. Debugger calls keep ordinary APPLY.
  #@(if (global.get $debug-evaluating)
    (then (return_call $apply (local.get $name) (call $cons (local.get $argument) (ref.null eq)))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  ;; Match APPLY0's frame and dynamic cleanup, including nonlocal exits.
  #@(local.set $backtrace-depth (global.get $backtrace-depth))
  #@(local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
  #@(local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
  #@(array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $name))
  #@(global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
  #@(call $clear-values)
  (wat-dynamic-call
    #@(block $done (result (ref null eq))
      (local.set $function (call $fdefinition (local.get $name)))
      (if (i32.eq (call $native-builtin-id (local.get $function)) (i32.const 2147483644))
        (then
          (call $generic-recent-call (local.get $function) (i32.const 1)
            (local.get $argument) (ref.null eq) (ref.null eq) (ref.null eq))
          (local.set $hit) (local.set $result)
          ;; The selected method owns its complete value group. Do not use
          ;; the scalar NATIVE-CALL1 ABI, which clears secondary values.
          (br_if $done (local.get $result) (local.get $hit))
          (drop)))
      ;; Retain the resolved binding across the selector's scheduler poll,
      ;; as ordinary generic APPLY does. Misses allocate the original packet.
      (call $apply1 (local.get $function) (call $cons (local.get $argument) (ref.null eq))))
    :cleanup #@(block
      (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
      (global.set $backtrace-depth (local.get $backtrace-depth)))))

(defun clos-default-get (object key)
  (wat-fragment #@(call $clos-meta-value ,(if (stringp object) (wat-fragment object) object) ,(clos-library-reference key))))

(define-runtime-function $clos-meta-value (($object :nullable-object) ($key :nullable-object)) (:nullable-object)
  ;; Private primitives are callable by Lisp too; reject bad inputs before
  ;; using the instance storage layout, rather than trapping on REF.CAST.
  #@(if (i32.and (i32.ne (call $tag (local.get $object)) (i32.const 23))
                (i32.ne (call $tag (local.get $object)) (i32.const 17)))
    (then (call $type-fail (local.get $object) (call $symbol (i32.const ,(runtime-symbol-id 'standard-object)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $object))) (local.get $key))))

(define-runtime-function $standard-slot-cell
    (($object :nullable-object) ($slot :nullable-object)) (:nullable-object)
  (:locals ($allocation :nullable-object) ($owner :nullable-object) ($name :nullable-object))
  #@(if (i32.eq (call $tag (local.get $object)) (i32.const 8))
    (then (if (i32.eq (call $value-n (local.get $object)) (i32.const 2147483645))
      (then (local.set $object (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))))
  #@(drop (call $clos-meta-value (local.get $object) ,(clos-library-reference '%layout)))
  #@(drop (call $ensure-instance-layout (local.get $object)))
  #@(local.set $allocation ,(clos-default-call 'slot-definition-allocation (list #@(local.get $slot))))
  #@(local.set $name ,(clos-default-call 'slot-definition-name (list #@(local.get $slot))))
  #@(if (ref.eq (local.get $allocation) ,(clos-library-reference :class))
    (then (local.set $owner ,(clos-default-get #@(local.get $slot) :owner))
      (return (call $property-cell ,(clos-default-get #@(local.get $owner) :shared-slots) (local.get $name)))))
  #@(call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $object))) (local.get $name)))

(define-runtime-function $standard-slot-call (($op i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($class :nullable-object) ($object :nullable-object) ($slot :nullable-object)
           ($cell :nullable-object) ($name :nullable-object) ($value :nullable-object) ($result :nullable-object))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-set-slot-value)))
    (then ,(wat-arity 4)
      (local.set $value (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args))))
    (else ,(wat-arity 3)))
  #@(local.set $class (call $car (local.get $args)))
  #@(local.set $object (call $car (call $cdr (local.get $args))))
  #@(local.set $slot (call $car (call $cdr (call $cdr (local.get $args)))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-set-slot-value)))
    (then (call $check-type (local.get $value)
      ,(clos-default-call 'slot-definition-type (list #@(local.get $slot))))))
  #@(local.set $cell (call $standard-slot-cell (local.get $object) (local.get $slot)))
  #@(block $done
    (if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-slot-value)))
      (then
        (if (i32.or (ref.is_null (local.get $cell))
              (ref.eq (call $car (local.get $cell)) (call $symbol-value ,(clos-library-reference '%unbound))))
          (then (return ,(clos-default-call 'slot-unbound
            (list #@(local.get $class) #@(local.get $object)
              (clos-default-call 'slot-definition-name (list #@(local.get $slot))))))))
        (local.set $result (call $car (local.get $cell))) (br $done)))
    (if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-slot-boundp)))
      (then (local.set $result (call $bool (i32.and (i32.eqz (ref.is_null (local.get $cell)))
        (i32.eqz (ref.eq (call $car (local.get $cell)) (call $symbol-value ,(clos-library-reference '%unbound)))))))
        (br $done)))
    (if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-slot-makunbound)))
      (then
        (if (i32.eqz (ref.is_null (local.get $cell)))
          (then (struct.set $obj $a (ref.cast (ref $obj) (local.get $cell))
            (call $symbol-value ,(clos-library-reference '%unbound)))))
        (local.set $result (local.get $object)) (br $done)))
    (if (ref.is_null (local.get $cell))
      (then
        (local.set $name ,(clos-default-call 'slot-definition-name (list #@(local.get $slot))))
        (drop ,(clos-default-call '%%put (list #@(local.get $object) #@(local.get $name) #@(local.get $value)))))
      (else (struct.set $obj $a (ref.cast (ref $obj) (local.get $cell)) (local.get $value))))
    (local.set $result (local.get $value)))
  #@(call $clear-values)
  #@(local.get $result))

(define-runtime-function $standard-allocate-storage (($class :nullable-object)) (:nullable-object)
  (:locals ($slots :nullable-object) ($slot :nullable-object) ($storage :nullable-object)
           ($tail :nullable-object) ($pair :nullable-object) ($name :nullable-object)
           ($layout :nullable-object) ($object :nullable-object) ($metaclass :nullable-object)
           ($locations (ref $objects)) ($location i32) ($indexed i32))
  #@(call $check-type (local.get $class) ,(clos-library-reference 'class))
  #@(local.set $slots ,(clos-default-call 'class-slots (list #@(local.get $class))))
  #@(call $check-apply-list (local.get $slots))
  #@(local.set $layout (local.get $slots))
  #@(if (ref.is_null (local.get $layout))
    (then (local.set $layout ,(clos-default-get #@(local.get $class) :empty-layout))))
  ;; Slot metadata methods may run Lisp. Keep their callbacks from changing
  ;; the validated iteration spine or exceeding the index allocation.
  #@(local.set $slots (call $copy-spine (local.get $slots) (i32.const 0)))
  #@(local.set $locations (array.new_default $objects (i32.add (i32.const 1) (call $length (local.get $slots)))))
  #@(array.set $objects (local.get $locations) (i32.const 0) (local.get $layout))
  #@(local.set $indexed (i32.const 1))
  #@(local.set $tail (call $cons (local.get $layout) (ref.null eq)))
  #@(local.set $storage (call $cons ,(clos-library-reference '%layout) (local.get $tail)))
  (wat-list-loop ($slots)
    #@(local.set $slot (call $car (local.get $slots)))
    #@(if (ref.eq ,(clos-default-call 'slot-definition-allocation (list #@(local.get $slot)))
          ,(clos-library-reference :instance))
      (then
        (local.set $name ,(clos-default-call 'slot-definition-name (list #@(local.get $slot))))
        (local.set $pair (call $cons (call $symbol-value ,(clos-library-reference '%unbound)) (ref.null eq)))
        ;; A metaclass may reorder effective slots after locations were
        ;; assigned. Keep the ordinary protocol when ordinal indexing is
        ;; not justified by the stored locations.
        (if (i32.eqz (call $eql ,(clos-default-get #@(local.get $slot) :location) (call $int (local.get $location))))
          (then (local.set $indexed (i32.const 0))))
        (local.set $location (i32.add (local.get $location) (i32.const 1)))
        (array.set $objects (local.get $locations) (local.get $location) (local.get $pair))
        (struct.set $obj $b (ref.cast (ref $obj) (local.get $tail)) (call $cons (local.get $name) (local.get $pair)))
        (local.set $tail (local.get $pair)))))
  ;; The index refers to the canonical value cells; it duplicates no values.
  ;; Borrowing or structurally replacing the plist clears the index.
  #@(if (i32.eq (call $tag (local.get $layout)) (i32.const 7))
    (then (call $check-apply-list (local.get $layout))))
  #@(local.set $object (call $node (i32.const 23) (i32.const 0) (local.get $class) (local.get $storage)
    (if (result (ref null eq)) (local.get $indexed) (then (local.get $locations)) (else (ref.null eq)))))
  #@(call $clear-values)
  #@(local.get $object))


(define-runtime-function $clos-method-metadata-function () (:nullable-object)
  #@(if (ref.is_null (global.get $clos-method-metadata-function))
    (then (global.set $clos-method-metadata-function
      (call $node (i32.const 8)
        (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%clos-method-metadata)))
        (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(global.get $clos-method-metadata-function))

;; One checked representation/protocol operation per callback. All selection,
;; argument traversal, copying and initialization order live in the CL workers.
(define-runtime-function $clos-method-metadata-call (($args :nullable-object)) (:nullable-object)
  #@(call $check-apply-list (local.get $args))
  (wat-arity 3)
  #@(return_call $clos-method-metadata-value (call $car (local.get $args))
    (call $car (call $cdr (local.get $args))) (call $car (call $cdr (call $cdr (local.get $args))))))

(define-runtime-function $clos-method-metadata-value
    (($selector-value :nullable-object) ($object :nullable-object) ($extra :nullable-object)) (:nullable-object)
  (:locals ($selector i32) ($count i32) ($data (ref $objects)))
  #@(local.set $selector (call $natural-count (local.get $selector-value)))
  #@(if (i32.eq (local.get $selector) (i32.const 0))
    (then (return ,(clos-default-call 'class-finalized-p (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 1))
    (then (return ,(clos-default-call 'finalize-inheritance (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 2))
    (then (return ,(clos-default-call 'class-default-initargs (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 3))
    (then (return ,(clos-default-call 'class-slots (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 15))
    (then (return (call $clos-meta-value (local.get $object)
      (if (result (ref null eq)) (ref.is_null (local.get $extra))
        (then ,(clos-library-reference :slots)) (else ,(clos-library-reference :default-initargs)))))))
  #@(if (i32.eq (local.get $selector) (i32.const 16))
    (then (return (call $bool (ref.eq (call $dispatch-class (local.get $object))
      ,(clos-default-call 'find-class (list (clos-library-reference 'forward-referenced-class))))))))
  #@(if (i32.eq (local.get $selector) (i32.const 17))
    (then (return ,(clos-default-call 'error
      (list (wat-text-literal "Class ~S has not been finalized.")
        (clos-default-call 'class-name (list #@(local.get $object))))))))
  #@(if (i32.eq (local.get $selector) (i32.const 4))
    (then (return ,(clos-default-call 'slot-definition-name (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 5))
    (then (return ,(clos-default-call 'slot-definition-initargs (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 7))
    (then (return ,(clos-default-call 'slot-definition-initfunction (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 14))
    (then (return (call $bool (i32.eq (call $tag (local.get $object)) (i32.const 8))))))
  #@(if (i32.eq (local.get $selector) (i32.const 11))
    (then (return (call $dispatch-class (local.get $object)))))
  #@(if (i32.or (i32.eq (local.get $selector) (i32.const 9))
                (i32.eq (local.get $selector) (i32.const 10)))
    (then
      ;; LENGTH validates dotted and cyclic spines while counting. Reuse that
      ;; checked count for both the initarg parity check and the result.
      (local.set $count (call $length (local.get $object)))
      ,(wat-fail-when #@(i32.and (i32.eq (local.get $selector) (i32.const 10))
                        (i32.and (local.get $count) (i32.const 1))) :wrong-arity)
      (return (call $int (local.get $count)))))
  #@(if (i32.eq (local.get $selector) (i32.const 12))
    (then (return ,(clos-default-call 'error
      (list (clos-library-reference 'simple-type-error) (clos-library-reference :datum) #@(local.get $object)
            (clos-library-reference :expected-type) #@(ref.null eq)
            (clos-library-reference :format-control) (wat-text-literal "Malformed CLOS initialization metadata.")
            (clos-library-reference :format-arguments) #@(ref.null eq))))))
  #@(if (i32.eq (local.get $selector) (i32.const 13))
    (then
      (if (ref.is_null (local.get $extra))
        (then (return ,(clos-default-call 'slot-definition-name (list #@(local.get $object))))))
      (return ,(clos-default-get #@(local.get $object) :name))))
  #@(if (i32.or (i32.eq (local.get $selector) (i32.const 6))
                (i32.eq (local.get $selector) (i32.const 8)))
    (then
      (if (i32.ne (call $tag (local.get $object)) (i32.const 34))
        (then (call $list-type-error (local.get $object) (i32.const ,(runtime-symbol-id 'simple-vector)))))
      (local.set $data (call $vector-data (local.get $object)))
      (if (i32.ne (array.len (local.get $data)) (i32.const 3))
        (then (call $type-fail (local.get $object)
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'simple-vector)))
            (call $cons (call $int (i32.const 3)) (ref.null eq)))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (if (i32.eq (local.get $selector) (i32.const 6))
        (then (return ,(clos-default-call 'slot-boundp-using-class
          (loop for i below 3 collect #@(array.get $objects (local.get $data) (i32.const ,i)))))))
      (return (call $apply
        (call $fdefinition ,(clos-default-arguments
          (list (clos-library-reference 'setf) (clos-library-reference 'slot-value-using-class))))
        ,(clos-default-arguments
          (cons #@(local.get $extra)
            (loop for i below 3 collect #@(array.get $objects (local.get $data) (i32.const ,i)))))))))
  (wat-fail :malformed-form)
  #@(unreachable))

;; The private WAT ingresses below supply the saved immutable metadata
;; callback. Prove its uses through the CL workers while preserving the
;; checked service and every dynamic MOP call inside it.
(defun $clos-class-slots-cl ()
  (runtime-function-definition '$clos-class-slots-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-class-slots-cl 'miricl-runtime:clos-class-slots-value
      :fixed-callbacks '((1 %%clos-method-metadata))))))
(defun $clos-class-default-initargs-cl ()
  (runtime-function-definition '$clos-class-default-initargs-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-class-default-initargs-cl 'miricl-runtime:clos-class-default-initargs-value
      :fixed-callbacks '((1 %%clos-method-metadata))))))
(defun $clos-default-initargs-cl ()
  (runtime-function-definition '$clos-default-initargs-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-default-initargs-cl 'miricl-runtime:clos-default-initargs-value
      :fixed-callbacks '((2 %%clos-method-metadata))))))
(defun $clos-shared-initialize-cl ()
  (runtime-function-definition '$clos-shared-initialize-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-shared-initialize-cl 'miricl-runtime:clos-shared-initialize-value
      :fixed-callbacks '((3 %%clos-method-metadata))))))
(defun $clos-find-slot-cl ()
  (runtime-function-definition '$clos-find-slot-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-find-slot-cl 'miricl-runtime:clos-find-slot-value
      :fixed-callbacks '((3 %%clos-method-metadata))))))

(define-runtime-function $standard-shared-initialize
    (($object :nullable-object) ($slot-names :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $clos-shared-initialize-cl
    (local.get $object) (local.get $slot-names) (local.get $args) (call $clos-method-metadata-function)))
  #@(call $clear-values)
  #@(local.get $result))

(define-runtime-function $standard-default-initargs
    (($class :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $clos-default-initargs-cl
    (local.get $class) (local.get $args) (call $clos-method-metadata-function)))
  #@(call $clear-values)
  #@(local.get $result))

(define-runtime-function $standard-check-initargs
    (($object :nullable-object) ($args :nullable-object) ($names :nullable-object)
     ($slot-names :nullable-object) ($previous :nullable-object)) (:nullable-object)
  (:locals ($state (ref $objects)) ($result :nullable-object))
  #@(local.set $state (array.new_default $objects (i32.const 3)))
  #@(array.set $objects (local.get $state) (i32.const 0) (local.get $names))
  #@(array.set $objects (local.get $state) (i32.const 1) (local.get $slot-names))
  #@(array.set $objects (local.get $state) (i32.const 2) (local.get $previous))
  #@(local.set $result (call $clos-check-initargs-cl (local.get $object) (local.get $args)
    (call $vector-new (local.get $state))
    (call $clos-initarg-metadata-function)))
  #@(call $clear-values)
  #@(local.get $result))

(define-runtime-function $standard-allocate-instance (($class :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $clos-allocate-instance-cl (local.get $class) (call $clos-initarg-metadata-function)))
  #@(call $clear-values)
  #@(local.get $result))

;; Constructor plans are process-private execution plans, not Lisp metadata.
;; The ordinary path establishes the selected methods before admission. ANSI
;; class redefinition advances the dispatch epoch; method edits replace each
;; generic's token. The supported slice needs no default or user initialization.
(define-runtime-function $constructor-property (($object :nullable-object) ($key :nullable-object)) (:nullable-object)
  #@(if (i32.eqz (ref.test (ref $obj) (local.get $object))) (then (return (ref.null eq))))
  #@(call $car (call $call-next-property-cell
    (struct.get $obj $b (ref.cast (ref $obj) (local.get $object))) (local.get $key))))

(define-runtime-function $constructor-symbol-p (($value :nullable-object)) (i32)
  #@(if (ref.is_null (local.get $value)) (then (return (i32.const 1))))
  #@(if (i32.eqz (ref.test (ref $obj) (local.get $value))) (then (return (i32.const 0))))
  #@(i32.eq (call $tag (local.get $value)) (i32.const 6)))

(define-runtime-function $constructor-default-binding
    (($name :nullable-object) ($args :nullable-object) ($opcode i32)) (:nullable-object)
  (:locals ($gf :nullable-object) ($info :nullable-object) ($properties :nullable-object)
           ($token :nullable-object) ($discriminator :nullable-object) ($profile :nullable-object)
           ($history :nullable-object) ($entry :nullable-object) ($scan :nullable-object)
           ($selected :nullable-object) ($method :nullable-object) ($body :nullable-object)
           ($adapter :nullable-object)
           ($count i32) ($remaining i32) ($token-cell :nullable-object) ($discriminator-cell :nullable-object)
           ($combination :nullable-object))
  #@(block $miss
    (local.set $gf (call $fdefinition (local.get $name)))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $gf))))
    (br_if $miss (ref.test (ref $native-function) (local.get $gf)))
    (br_if $miss (i32.ne (call $tag (local.get $gf)) (i32.const 8)))
    (br_if $miss (i32.ne (call $value-n (local.get $gf)) (i32.const 2147483645)))
    (local.set $info (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf))))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $info))))
    (local.set $properties (struct.get $obj $b (ref.cast (ref $obj) (local.get $info))))
    (local.set $combination (call $constructor-property (local.get $info) ,(clos-library-reference :method-combination-name)))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $combination))))
    (br_if $miss (i32.ne (call $tag (local.get $combination)) (i32.const 7)))
    (br_if $miss (i32.eqz (ref.eq (call $car (local.get $combination)) ,(clos-library-reference 'standard))))
    (br_if $miss (i32.eqz (ref.is_null (call $cdr (local.get $combination)))))
    (local.set $token-cell (call $call-next-property-cell (local.get $properties) ,(clos-library-reference :cache-token)))
    (local.set $discriminator-cell (call $call-next-property-cell (local.get $properties) ,(clos-library-reference :discriminating-function)))
    (br_if $miss (ref.is_null (local.get $token-cell)))
    (local.set $token (call $car (local.get $token-cell)))
    (local.set $discriminator (call $car (local.get $discriminator-cell)))
    (if (i32.eqz (ref.is_null (local.get $discriminator)))
      (then
        (br_if $miss (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator))))
        (br_if $miss (i32.eqz (ref.eq (struct.get $generic-discriminator $owner
          (ref.cast (ref $generic-discriminator) (local.get $discriminator))) (local.get $gf))))))
    (local.set $profile (call $constructor-property (local.get $info) ,(clos-library-reference :dispatch-profile)))
    ;; Only class keys are reusable for newly allocated objects. Validate the
    ;; entire bounded profile before the existing raw class-key query.
    (local.set $scan (local.get $profile)) (local.set $count (i32.const 0))
    (block $profile-done (loop $profile-loop
      (br_if $profile-done (ref.is_null (local.get $scan)))
      (br_if $miss (i32.eq (local.get $count) (i32.const 2)))
      (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $scan))))
      (br_if $miss (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
      (br_if $miss (i32.eqz (ref.test (ref i31) (call $car (local.get $scan)))))
      (br_if $miss (i32.gt_u (i31.get_u (ref.cast (ref i31) (call $car (local.get $scan)))) (i32.const 1)))
      (local.set $scan (call $cdr (local.get $scan)))
      (local.set $count (i32.add (local.get $count) (i32.const 1))) (br $profile-loop)))
    (local.set $history (call $constructor-property (local.get $info) ,(clos-library-reference :cache)))
    (local.set $remaining (i32.const 16))
    (loop $next
      (br_if $miss (i32.eqz (local.get $remaining)))
      (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $history))))
      (br_if $miss (i32.ne (call $tag (local.get $history)) (i32.const 7)))
      (local.set $entry (call $car (local.get $history)))
      (local.set $scan (local.get $entry)) (local.set $count (i32.const 0))
      (block $counted (loop $cells
        (br_if $counted (ref.is_null (local.get $scan)))
        (br_if $miss (i32.eq (local.get $count) (i32.const 6)))
        (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $scan))))
        (br_if $miss (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
        (local.set $scan (call $cdr (local.get $scan)))
        (local.set $count (i32.add (local.get $count) (i32.const 1))) (br $cells)))
      (br_if $miss (i32.ne (local.get $count) (i32.const 6)))
      ;; The class-key matcher is an unchecked internal query. Validate its
      ;; short spine before a raw tag read can see foreign GC data or a cycle.
      (local.set $scan (call $car (call $cdr (local.get $entry))))
      (local.set $count (i32.const 0))
      (block $key-checked (loop $key-cells
        (br_if $key-checked (ref.is_null (local.get $scan)))
        (br_if $miss (i32.eq (local.get $count) (i32.const 2)))
        (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $scan))))
        (br_if $miss (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
        (local.set $scan (call $cdr (local.get $scan)))
        (local.set $count (i32.add (local.get $count) (i32.const 1))) (br $key-cells)))
      (if (i32.and (ref.eq (call $car (local.get $entry))
                    (call $symbol-value ,(clos-library-reference '%dispatch-version)))
            (call $clos-applicable-class-key-p (call $car (call $cdr (local.get $entry)))
              (local.get $profile) (local.get $args)))
        (then
          ;; Rest-argument generics use a compiled effective wrapper. Inspect
          ;; its selected method set, not the wrapper's transport convention.
          (local.set $selected (call $car (call $cdr (call $cdr (call $cdr (call $cdr (local.get $entry)))))))
          (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $selected))))
          (br_if $miss (i32.ne (call $tag (local.get $selected)) (i32.const 7)))
          (br_if $miss (i32.eqz (ref.is_null (call $cdr (local.get $selected)))))
          (local.set $method (call $car (local.get $selected)))
          (br_if $miss (i32.eqz (ref.is_null (call $constructor-property (local.get $method) ,(clos-library-reference :qualifiers)))))
          (local.set $body (call $constructor-property (local.get $method) ,(clos-library-reference :function)))
          (br_if $miss (i32.eqz (ref.test (ref $direct-function) (local.get $body))))
          (br_if $miss (i32.ne (call $tag (local.get $body)) (i32.const 8)))
          (br_if $miss (i32.ne (call $value-n (local.get $body)) (i32.const 0)))
          (br_if $miss (i32.ne (struct.get $direct-function $mode
            (ref.cast (ref $direct-function) (local.get $body))) (i32.const 10)))
          (local.set $adapter (local.get $body))
          (local.set $body (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $body))))
          (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $body))))
          ;; APPLY dispatches a native entry before its numeric builtin field.
          (br_if $miss (ref.test (ref $native-function) (local.get $body)))
          (br_if $miss (i32.ne (call $tag (local.get $body)) (i32.const 8)))
          (br_if $miss (i32.ne (call $value-n (local.get $body)) (local.get $opcode)))
          (return (array.new_fixed $objects 11 (local.get $name) (local.get $gf) (local.get $info)
            (local.get $properties) (local.get $token-cell) (local.get $token)
            (local.get $discriminator-cell) (local.get $discriminator)
            (local.get $adapter) (local.get $body)
            (ref.i31 (i32.sub (local.get $opcode) (i32.const ,+clos-primitive-base+)))))))
      (local.set $history (call $cdr (local.get $history)))
      (local.set $remaining (i32.sub (local.get $remaining) (i32.const 1))) (br $next)))
  #@(ref.null eq))

(define-runtime-function $constructor-binding-current (($binding (ref $objects))) (i32)
  ;; N is mutable even on a fixed direct-function subtype. Bind permission to
  ;; the actual adapter and builtin dispatch that were authenticated at build.
  #@(if (i32.ne (call $value-n (array.get $objects (local.get $binding) (i32.const 8))) (i32.const 0))
    (then (return (i32.const 0))))
  #@(if (i32.ne (call $value-n (array.get $objects (local.get $binding) (i32.const 9)))
        (i32.add (i32.const ,+clos-primitive-base+)
          (i31.get_u (ref.cast (ref i31) (array.get $objects (local.get $binding) (i32.const 10))))))
    (then (return (i32.const 0))))
  #@(i32.and
    (ref.eq (call $fdefinition (array.get $objects (local.get $binding) (i32.const 0)))
      (array.get $objects (local.get $binding) (i32.const 1)))
    (i32.and
      (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (array.get $objects (local.get $binding) (i32.const 1))))
        (array.get $objects (local.get $binding) (i32.const 2)))
      (i32.and
        (ref.eq (struct.get $obj $b (ref.cast (ref $obj) (array.get $objects (local.get $binding) (i32.const 2))))
          (array.get $objects (local.get $binding) (i32.const 3)))
        (i32.and
          (ref.eq (call $car (array.get $objects (local.get $binding) (i32.const 4)))
            (array.get $objects (local.get $binding) (i32.const 5)))
          (ref.eq (call $car (array.get $objects (local.get $binding) (i32.const 6)))
            (array.get $objects (local.get $binding) (i32.const 7))))))))

(define-runtime-function $constructor-plan-build
    (($class :nullable-object) ($args :nullable-object) ($object :nullable-object)) ()
  (:locals ($key :nullable-object) ($slots :nullable-object) ($slot :nullable-object) ($keys :nullable-object)
           ($layout :nullable-object) ($names (ref $objects)) ($init :nullable-object) ($shared :nullable-object)
           ($count i32) ($mask i32) ($remaining i32) ($name :nullable-object))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $args))))
    (br_if $miss (i32.ne (call $tag (local.get $args)) (i32.const 7)))
    (local.set $key (call $car (local.get $args)))
    (br_if $miss (i32.eqz (call $constructor-symbol-p (local.get $key))))
    (br_if $miss (ref.eq (local.get $key) ,(clos-library-reference :allow-other-keys)))
    (local.set $args (call $cdr (local.get $args)))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $args))))
    (br_if $miss (i32.ne (call $tag (local.get $args)) (i32.const 7)))
    (br_if $miss (i32.eqz (ref.is_null (call $cdr (local.get $args)))))
    (br_if $miss (i32.eqz (ref.eq (call $dispatch-class-known (local.get $class))
      (call $named-class ,(clos-library-reference 'standard-class)))))
    (local.set $name (call $constructor-property (local.get $class) ,(clos-library-reference :name)))
    (br_if $miss (i32.eqz (call $constructor-symbol-p (local.get $name))))
    (br_if $miss (i32.eqz (ref.eq (call $named-class (local.get $name)) (local.get $class))))
    (br_if $miss (ref.is_null (call $constructor-property (local.get $class) ,(clos-library-reference :finalized))))
    (br_if $miss (i32.eqz (ref.is_null (call $constructor-property (local.get $class) ,(clos-library-reference :default-initargs)))))
    (local.set $slots (call $constructor-property (local.get $class) ,(clos-library-reference :slots)))
    (local.set $layout (local.get $slots))
    (local.set $names (array.new_default $objects (i32.const 8)))
    (block $slots-done (loop $slot-loop
      (br_if $slots-done (ref.is_null (local.get $slots)))
      (br_if $miss (i32.eq (local.get $count) (i32.const 8)))
      (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $slots))))
      (br_if $miss (i32.ne (call $tag (local.get $slots)) (i32.const 7)))
      (local.set $slot (call $car (local.get $slots)))
      (br_if $miss (i32.eqz (ref.eq (call $constructor-property (local.get $slot) ,(clos-library-reference :allocation)) ,(clos-library-reference :instance))))
      (br_if $miss (i32.eqz (ref.eq (call $constructor-property (local.get $slot) ,(clos-library-reference :type)) (call $symbol (i32.const 1)))))
      (br_if $miss (i32.eqz (ref.is_null (call $constructor-property (local.get $slot) ,(clos-library-reference :initfunction)))))
      (br_if $miss (i32.eqz (ref.eq (call $constructor-property (local.get $slot) ,(clos-library-reference :location)) (call $int (local.get $count)))))
      (local.set $name (call $constructor-property (local.get $slot) ,(clos-library-reference :name)))
      (br_if $miss (i32.eqz (call $constructor-symbol-p (local.get $name))))
      (array.set $objects (local.get $names) (local.get $count) (local.get $name))
      (local.set $keys (call $constructor-property (local.get $slot) ,(clos-library-reference :initargs)))
      (local.set $remaining (i32.const 16))
      (block $keys-done (loop $key-loop
        (br_if $keys-done (ref.is_null (local.get $keys)))
        (br_if $miss (i32.eqz (local.get $remaining)))
        (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $keys))))
        (br_if $miss (i32.ne (call $tag (local.get $keys)) (i32.const 7)))
        (br_if $miss (i32.eqz (call $constructor-symbol-p (call $car (local.get $keys)))))
        (if (ref.eq (call $car (local.get $keys)) (local.get $key))
          (then (local.set $mask (i32.or (local.get $mask) (i32.shl (i32.const 1) (local.get $count))))))
        (local.set $keys (call $cdr (local.get $keys)))
        (local.set $remaining (i32.sub (local.get $remaining) (i32.const 1))) (br $key-loop)))
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      (local.set $slots (call $cdr (local.get $slots))) (br $slot-loop)))
    (br_if $miss (i32.eqz (local.get $mask)))
    (local.set $args (call $cons (local.get $key) (local.get $args)))
    (local.set $init (call $constructor-default-binding ,(clos-library-reference 'initialize-instance)
      (call $cons (local.get $object) (local.get $args))
      (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%standard-initialize-instance)))))
    (br_if $miss (ref.is_null (local.get $init)))
    (local.set $shared (call $constructor-default-binding ,(clos-library-reference 'shared-initialize)
      (call $cons (local.get $object) (call $cons (call $symbol (i32.const 1)) (local.get $args)))
      (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%standard-shared-initialize)))))
    (br_if $miss (ref.is_null (local.get $shared)))
    (if (ref.is_null (global.get $constructor-plans))
      (then (global.set $constructor-plans (array.new_default $objects (i32.const 4)))))
    (array.set $objects (ref.as_non_null (global.get $constructor-plans)) (global.get $constructor-plan-next)
      (struct.new $constructor-plan (local.get $class) (local.get $key) (local.get $layout)
        (local.get $names) (local.get $count) (local.get $mask)
        (call $symbol-value ,(clos-library-reference '%dispatch-version))
        (ref.cast (ref $objects) (local.get $init)) (ref.cast (ref $objects) (local.get $shared))))
    (global.set $constructor-plan-next (i32.and (i32.add (global.get $constructor-plan-next) (i32.const 1)) (i32.const 3)))))

(define-runtime-function $constructor-plan-call (($class :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($key :nullable-object) ($value :nullable-object) ($entry :nullable-object) ($plan (ref $constructor-plan))
           ($position i32) ($index i32) ($storage :nullable-object) ($tail :nullable-object) ($cell :nullable-object)
           ($locations (ref $objects)))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (br_if $miss (ref.is_null (global.get $constructor-plans)))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $args))))
    (br_if $miss (i32.ne (call $tag (local.get $args)) (i32.const 7)))
    (local.set $key (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $args))))
    (br_if $miss (i32.ne (call $tag (local.get $args)) (i32.const 7)))
    (br_if $miss (i32.eqz (ref.is_null (call $cdr (local.get $args)))))
    (local.set $value (call $car (local.get $args)))
    (loop $entries
      (br_if $miss (i32.eq (local.get $position) (i32.const 4)))
      (local.set $entry (array.get $objects (ref.as_non_null (global.get $constructor-plans)) (local.get $position)))
      (if (ref.test (ref $constructor-plan) (local.get $entry))
        (then
          (local.set $plan (ref.cast (ref $constructor-plan) (local.get $entry)))
          (if (i32.and (ref.eq (struct.get $constructor-plan $class (local.get $plan)) (local.get $class))
                (ref.eq (struct.get $constructor-plan $key (local.get $plan)) (local.get $key)))
            (then
              (br_if $miss (i32.eqz (ref.eq (struct.get $constructor-plan $version (local.get $plan))
                (call $symbol-value ,(clos-library-reference '%dispatch-version)))))
              (br_if $miss (i32.eqz (call $constructor-binding-current (struct.get $constructor-plan $init (local.get $plan)))))
              (br_if $miss (i32.eqz (call $constructor-binding-current (struct.get $constructor-plan $shared (local.get $plan)))))
              (local.set $locations (array.new_default $objects (i32.add (struct.get $constructor-plan $count (local.get $plan)) (i32.const 1))))
              (array.set $objects (local.get $locations) (i32.const 0) (struct.get $constructor-plan $layout (local.get $plan)))
              (local.set $tail (call $cons (struct.get $constructor-plan $layout (local.get $plan)) (ref.null eq)))
              (local.set $storage (call $cons ,(clos-library-reference '%layout) (local.get $tail)))
              (block $done (loop $slots
                (br_if $done (i32.eq (local.get $index) (struct.get $constructor-plan $count (local.get $plan))))
                (local.set $cell (call $cons
                  (if (result (ref null eq)) (i32.and (struct.get $constructor-plan $mask (local.get $plan)) (i32.shl (i32.const 1) (local.get $index)))
                    (then (local.get $value)) (else (call $symbol-value ,(clos-library-reference '%unbound)))) (ref.null eq)))
                (struct.set $obj $b (ref.cast (ref $obj) (local.get $tail))
                  (call $cons (array.get $objects (struct.get $constructor-plan $names (local.get $plan)) (local.get $index)) (local.get $cell)))
                (array.set $objects (local.get $locations) (i32.add (local.get $index) (i32.const 1)) (local.get $cell))
                (local.set $tail (local.get $cell)) (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $slots)))
              (return (call $node (i32.const 23) (i32.const 0) (local.get $class) (local.get $storage) (local.get $locations)))))))
      (local.set $position (i32.add (local.get $position) (i32.const 1))) (br $entries)))
  #@(ref.null eq))

(define-runtime-function $standard-clos-call (($op i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($class :nullable-object) ($object :nullable-object) ($initargs :nullable-object) ($slots :nullable-object))
  #@(call $check-apply-list (local.get $args))
  #@(if (i32.or (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-class-slots)))
                (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-class-default-initargs))))
    (then ,(wat-arity 1)
      (local.set $class (call $car (local.get $args)))
      (if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-class-slots)))
        (then (local.set $object (call $clos-class-slots-cl
          (local.get $class) (call $clos-method-metadata-function))))
        (else (local.set $object (call $clos-class-default-initargs-cl
          (local.get $class) (call $clos-method-metadata-function)))))
      (call $clear-values)
      (return (local.get $object))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-default-initargs)))
    (then ,(wat-arity 2)
      (return_call $standard-default-initargs (call $car (local.get $args)) (call $car (call $cdr (local.get $args))))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-check-initargs)))
    (then ,(wat-arity 5)
      (return_call $standard-check-initargs (call $car (local.get $args)) (call $car (call $cdr (local.get $args)))
        (call $car (call $cdr (call $cdr (local.get $args))))
        (call $car (call $cdr (call $cdr (call $cdr (local.get $args)))))
        (call $car (call $cdr (call $cdr (call $cdr (call $cdr (local.get $args)))))))))
  #@(if (i32.ge_u (local.get $op) (i32.const ,(clos-primitive-op '%%standard-slot-value)))
    (then (return_call $standard-slot-call (local.get $op) (local.get $args))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-shared-initialize)))
    (then ,(wat-arity 2 2147483647)
      (return_call $standard-shared-initialize (call $car (local.get $args))
        (call $car (call $cdr (local.get $args))) (call $cdr (call $cdr (local.get $args))))))
  (wat-arity 1 2147483647)
  #@(local.set $class (call $car (local.get $args)))
  #@(local.set $initargs (call $cdr (local.get $args)))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-allocate-instance)))
    (then (return_call $standard-allocate-instance (local.get $class))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%standard-initialize-instance)))
    (then (return ,(clos-default-call 'shared-initialize
      (list #@(local.get $class) #@(call $symbol (i32.const 1))) #@(local.get $initargs)))))
  ;; Symbol designators redispatch on the class, so user class methods run.
  #@(if (call $symbolp (local.get $class))
    (then
      (local.set $object (call $named-class (local.get $class)))
      ;; FIND-CLASS is a standard operation. A registered hit needs no generic
      ;; function-call transport; misses retain lazy condition creation/errors.
      (local.set $class (if (result (ref null eq)) (ref.is_null (local.get $object))
        (then ,(clos-default-call 'find-class (list #@(local.get $class))))
        (else (local.get $object))))
      (return ,(clos-default-call 'make-instance (list #@(local.get $class)) #@(local.get $initargs)))))
  #@(call $check-type (local.get $class) ,(clos-library-reference 'class))
  #@(if (i32.eqz (ref.is_null ,(clos-default-get #@(local.get $class) :condition)))
    (then (return ,(clos-default-call 'make-condition
      (list (clos-default-call 'class-name (list #@(local.get $class)))) #@(local.get $initargs)))))
  ;; This is a new bounded implementation of the selected standard body.
  ;; Preserve its entry observation before reading any plan permission.
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $object (call $constructor-plan-call (local.get $class) (local.get $initargs)))
  #@(if (i32.eqz (ref.is_null (local.get $object))) (then (call $clear-values) (return (local.get $object))))
  #@(local.set $initargs ,(clos-default-call '%default-initargs (list #@(local.get $class) #@(local.get $initargs))))
  #@(local.set $object ,(clos-default-call 'allocate-instance (list #@(local.get $class)) #@(local.get $initargs)))
  #@(drop ,(clos-default-call '%check-initargs
    (list #@(local.get $object) #@(local.get $initargs)
      (clos-default-arguments (mapcar #'clos-library-reference '(initialize-instance shared-initialize allocate-instance)))
      #@(call $symbol (i32.const 1)) #@(ref.null eq))))
  #@(drop ,(clos-default-call 'initialize-instance (list #@(local.get $object)) #@(local.get $initargs)))
  #@(call $clear-values)
  #@(call $constructor-plan-build (local.get $class) (local.get $initargs) (local.get $object))
  #@(local.get $object))

(define-runtime-function $invalidate-slot-access (($name :nullable-object)) ()
  (:locals ($id i32))
  #@(if (ref.eq (local.get $name) (call $symbol (i32.const 1)))
    (then (global.set $slot-access-token (call $cons (ref.null eq) (ref.null eq))) (return)))
  #@(local.set $id (call $id (call $function-name-symbol (local.get $name))))
  (wat-symbol-case #@(local.get $id)
    ((class-slots class-finalized-p slot-definition-name slot-definition-allocation
      slot-definition-type slot-value-using-class compute-applicable-methods
      compute-effective-method compute-discriminating-function method-lambda-list)
      #@(global.set $slot-access-token (call $cons (ref.null eq) (ref.null eq)))))
  #@(nop))

(define-runtime-function $slot-protocols-standard-p
    (($class :nullable-object) ($object :nullable-object) ($slot :nullable-object)) (i32)
  (:locals ($protocols :nullable-object) ($entry :nullable-object) ($function :nullable-object)
           ($discriminator :nullable-object))
  #@(if (i32.eqz (call $boundp ,(clos-library-reference '%standard-slot-protocols)))
    (then (return (i32.const 0))))
  #@(local.set $protocols (call $symbol-value ,(clos-library-reference '%standard-slot-protocols)))
  #@(call $check-apply-list (local.get $protocols))
  (wat-list-loop ($protocols)
    #@(local.set $entry (call $car (local.get $protocols)))
    #@(local.set $function (call $fdefinition (call $car (local.get $entry))))
    #@(if (i32.eqz (ref.eq (local.get $function) (call $car (call $cdr (local.get $entry)))))
      (then (return (i32.const 0))))
    #@(if (i32.eqz (call $standard-property-generic-p (local.get $function)
          (call $car (call $cdr (call $cdr (local.get $entry))))))
      (then
        (if (i32.eqz (call $generic-cache-permitted-p (local.get $function))) (then (return (i32.const 0))))
        (local.set $discriminator (call $generic-property (local.get $function) ,(clos-library-reference :discriminating-function)))
        (if (i32.eqz (ref.is_null (local.get $discriminator)))
          (then
            (if (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator))) (then (return (i32.const 0))))
            (if (i32.eqz (ref.eq (struct.get $generic-discriminator $owner
                  (ref.cast (ref $generic-discriminator) (local.get $discriminator))) (local.get $function)))
              (then (return (i32.const 0))))))
        (if (ref.is_null ,(clos-default-call '%standard-slot-protocol-for-args-p
              (list #@(local.get $entry) #@(local.get $class) #@(local.get $object) #@(local.get $slot))))
          (then (return (i32.const 0)))))))
  #@(i32.const 1))

(define-runtime-function $specialize-accessor
    (($function :nullable-object) ($gf :nullable-object) ($body (ref $direct-function))
     ($args :nullable-object)) (:nullable-object)
  (:locals ($object :nullable-object) ($class :nullable-object) ($layout :nullable-object)
           ($slot :nullable-object) ($name :nullable-object) ($scan :nullable-object)
           ($entry :nullable-object) ($cell :nullable-object) ($token :nullable-object)
           ($plan (ref $objects)) ($writer i32) ($offset i32) ($found i32))
  #@(local.set $token (global.get $slot-access-token))
  #@(local.set $writer (i32.eq (struct.get $direct-function $mode (local.get $body)) (i32.const 12)))
  #@(local.set $object (if (result (ref null eq)) (local.get $writer)
    (then (call $car (call $cdr (local.get $args)))) (else (call $car (local.get $args)))))
  #@(if (i32.ne (call $tag (local.get $object)) (i32.const 23)) (then (return (ref.null eq))))
  #@(local.set $class (call $dispatch-class (local.get $object)))
  #@(if (ref.is_null ,(clos-default-get #@(local.get $class) :finalized)) (then (return (ref.null eq))))
  #@(local.set $layout ,(clos-default-get #@(local.get $class) :slots))
  #@(local.set $name (struct.get $direct-function $slot (local.get $body)))
  #@(local.set $slot (call $find-instance-slot (local.get $class) (local.get $name)))
  #@(if (ref.is_null (local.get $slot)) (then (return (ref.null eq))))
  #@(if (i32.eqz (call $slot-protocols-standard-p (local.get $class) (local.get $object) (local.get $slot)))
    (then (return (ref.null eq))))
  #@(if (ref.eq ,(clos-default-get #@(local.get $slot) :allocation) ,(clos-library-reference :class))
    (then
      (local.set $entry ,(clos-default-get #@(local.get $slot) :owner))
      (local.set $cell (call $property-cell ,(clos-default-get #@(local.get $entry) :shared-slots) (local.get $name)))
      (if (ref.is_null (local.get $cell)) (then (return (ref.null eq)))))
    (else
      (local.set $scan (local.get $layout))
      (call $check-apply-list (local.get $scan))
      (block $found (loop $slot
        (br_if $found (ref.is_null (local.get $scan)))
        (local.set $entry (call $car (local.get $scan)))
        (if (ref.eq ,(clos-default-get #@(local.get $entry) :allocation) ,(clos-library-reference :instance))
          (then
            (if (ref.eq ,(clos-default-get #@(local.get $entry) :name) (local.get $name))
              (then (local.set $found (i32.const 1)) (br $found)))
            (local.set $offset (i32.add (local.get $offset) (i32.const 1)))))
        (local.set $scan (call $cdr (local.get $scan))) (br $slot)))
      (if (i32.or (i32.eqz (local.get $found)) (i32.ge_u (local.get $offset) (i32.const 4096)))
        (then (return (ref.null eq))))))
  #@(if (i32.eqz (ref.eq (local.get $token) (global.get $slot-access-token))) (then (return (ref.null eq))))
  #@(local.set $plan (array.new_default $objects (i32.const 8)))
  #@(array.set $objects (local.get $plan) (i32.const 0) (local.get $token))
  #@(array.set $objects (local.get $plan) (i32.const 1) (local.get $class))
  #@(array.set $objects (local.get $plan) (i32.const 2) (local.get $layout))
  #@(array.set $objects (local.get $plan) (i32.const 3) (local.get $name))
  #@(array.set $objects (local.get $plan) (i32.const 4) (call $int (local.get $offset)))
  #@(array.set $objects (local.get $plan) (i32.const 5) ,(clos-default-get #@(local.get $slot) :type))
  #@(array.set $objects (local.get $plan) (i32.const 6) (local.get $cell))
  #@(array.set $objects (local.get $plan) (i32.const 7) (local.get $slot))
  #@(call $direct-function (local.get $function) (local.get $gf) (local.get $plan)
    (i32.add (i32.const 15) (local.get $writer))))

(define-runtime-function $specialized-slot-call
    (($function (ref $direct-function)) ($args :nullable-object)) (:nullable-object)
  (:locals ($object :nullable-object) ($value :nullable-object) ($writer i32))
  (wat-arity 1)
  #@(local.set $args (call $car (local.get $args)))
  #@(local.set $writer (i32.eq (struct.get $direct-function $mode (local.get $function)) (i32.const 16)))
  #@(call $arity (local.get $args) (i32.add (i32.const 1) (local.get $writer))
    (i32.add (i32.const 1) (local.get $writer)))
  #@(local.set $value (call $car (local.get $args)))
  #@(local.set $object (if (result (ref null eq)) (local.get $writer)
    (then (call $car (call $cdr (local.get $args)))) (else (local.get $value))))
  #@(return_call $specialized-slot-operation (local.get $function) (local.get $object) (local.get $value)))

(define-runtime-function $specialized-slot-operation
    (($function (ref $direct-function)) ($object :nullable-object) ($value :nullable-object)) (:nullable-object)
  (:locals ($plan (ref $objects)) ($cell :nullable-object) ($scan :nullable-object) ($name :nullable-object)
           ($slot :nullable-object) ($locations (ref null $objects)) ($writer i32) ($offset i32))
  #@(local.set $writer (i32.eq (struct.get $direct-function $mode (local.get $function)) (i32.const 16)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $objects) (struct.get $direct-function $slot (local.get $function)))) :malformed-form)
  #@(local.set $plan (ref.cast (ref $objects) (struct.get $direct-function $slot (local.get $function))))
  (wat-fail-when #@(i32.ne (array.len (local.get $plan)) (i32.const 8)) :malformed-form)
  #@(local.set $name (array.get $objects (local.get $plan) (i32.const 3)))
  #@(block $general
    (br_if $general (i32.eqz (ref.eq (array.get $objects (local.get $plan) (i32.const 0)) (global.get $slot-access-token))))
    (br_if $general (i32.ne (call $tag (local.get $object)) (i32.const 23)))
    (br_if $general (i32.eqz (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (local.get $object)))
      (array.get $objects (local.get $plan) (i32.const 1)))))
    (local.set $cell (array.get $objects (local.get $plan) (i32.const 6)))
    (if (ref.is_null (local.get $cell))
      (then
        (br_if $general (i32.eqz (ref.test (ref $objects) (struct.get $obj $c (ref.cast (ref $obj) (local.get $object))))))
        (local.set $locations (ref.cast (ref $objects) (struct.get $obj $c (ref.cast (ref $obj) (local.get $object)))))
        (br_if $general (i32.eqz (array.len (local.get $locations))))
        (br_if $general (i32.eqz (ref.eq (array.get $objects (local.get $locations) (i32.const 0))
          (array.get $objects (local.get $plan) (i32.const 2)))))
        (local.set $offset (call $natural-count (array.get $objects (local.get $plan) (i32.const 4))))
        (br_if $general (i32.ge_u (local.get $offset) (i32.const 4096)))
        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (br_if $general (i32.ge_u (local.get $offset) (array.len (local.get $locations))))
        (local.set $cell (array.get $objects (local.get $locations) (local.get $offset)))))
    (br_if $general (ref.is_null (local.get $cell)))
    (if (local.get $writer)
      (then
        (call $check-type (local.get $value) (array.get $objects (local.get $plan) (i32.const 5)))
        ;; SATISFIES and type expansion can run user code. Resume the selected
        ;; standard setter after validation, without repeating predicate effects.
        (if (i32.or
              (i32.eqz (ref.eq (array.get $objects (local.get $plan) (i32.const 0)) (global.get $slot-access-token)))
              (if (result i32) (ref.is_null (array.get $objects (local.get $plan) (i32.const 6)))
                (then (i32.eqz (ref.eq (struct.get $obj $c (ref.cast (ref $obj) (local.get $object)))
                  (local.get $locations)))) (else (i32.const 0))))
          (then
            (local.set $slot (array.get $objects (local.get $plan) (i32.const 7)))
            (local.set $cell (call $standard-slot-cell (local.get $object) (local.get $slot)))
            (if (ref.is_null (local.get $cell))
              (then
                (drop ,(clos-default-call '%%put (list #@(local.get $object)
                  (clos-default-call 'slot-definition-name (list #@(local.get $slot))) #@(local.get $value))))
                (call $clear-values) (return (local.get $value))))))
        (drop (call $car (local.get $cell)))
        (struct.set $obj $a (ref.cast (ref $obj) (local.get $cell)) (local.get $value))
        (call $clear-values) (return (local.get $value))))
    (local.set $value (call $car (local.get $cell)))
    (if (ref.eq (local.get $value) (call $symbol-value ,(clos-library-reference '%unbound)))
      (then (local.set $value ,(clos-default-call 'slot-unbound
        (list (wat-fragment #@(array.get $objects (local.get $plan) (i32.const 1))) #@(local.get $object) #@(local.get $name))))))
    (call $clear-values) (return (local.get $value)))
  #@(return_call $instance-slot-operation-general (local.get $writer) (local.get $object) (local.get $name) (local.get $value)))

(define-runtime-function $raw-instance-access (($op i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($object :nullable-object) ($storage :nullable-object) ($location :nullable-object)
           ($value :nullable-object) ($cell :nullable-object) ($owned :nullable-object)
           ($protocol :nullable-object) ($function :nullable-object)
           ($locations (ref null $objects)) ($writer i32) ($index i32))
  #@(local.set $writer (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%set-raw-instance-access))))
  #@(call $arity (local.get $args) (i32.add (i32.const 2) (local.get $writer))
    (i32.add (i32.const 2) (local.get $writer)))
  #@(if (local.get $writer)
    (then (local.set $value (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))))
  #@(local.set $object (call $car (local.get $args)))
  #@(local.set $location (call $car (call $cdr (local.get $args))))
  #@(local.set $storage (local.get $object))
  #@(if (if (result i32) (i32.eq (call $tag (local.get $storage)) (i32.const 8))
          (then (i32.eq (call $value-n (local.get $storage)) (i32.const 2147483645))) (else (i32.const 0)))
    (then (local.set $storage (struct.get $obj $a (ref.cast (ref $obj) (local.get $storage))))))
  #@(block $ordinary
    (br_if $ordinary (i32.eqz (call $boundp ,(clos-library-reference '%standard-slot-location-protocol))))
    (local.set $protocol (call $symbol-value ,(clos-library-reference '%standard-slot-location-protocol)))
    (local.set $function (call $fdefinition ,(clos-library-reference 'slot-definition-location)))
    (br_if $ordinary (i32.eqz (ref.eq (local.get $function) (call $car (local.get $protocol)))))
    (br_if $ordinary (i32.eqz (call $standard-property-generic-p (local.get $function) (call $cdr (local.get $protocol)))))
    (br_if $ordinary (i32.ne (call $tag (local.get $storage)) (i32.const 23)))
    (local.set $owned (struct.get $obj $c (ref.cast (ref $obj) (local.get $storage))))
    (br_if $ordinary (i32.eqz (ref.test (ref $objects) (local.get $owned))))
    (local.set $locations (ref.cast (ref $objects) (local.get $owned)))
    (br_if $ordinary (i32.eqz (array.len (local.get $locations))))
    (br_if $ordinary (i32.eqz (ref.eq (array.get $objects (local.get $locations) (i32.const 0))
      ,(clos-default-get #@(struct.get $obj $a (ref.cast (ref $obj) (local.get $storage))) :slots))))
    (local.set $index (call $natural-count (local.get $location)))
    (br_if $ordinary (i32.ge_u (local.get $index) (i32.sub (array.len (local.get $locations)) (i32.const 1))))
    (local.set $cell (array.get $objects (local.get $locations) (i32.add (local.get $index) (i32.const 1))))
    (br_if $ordinary (ref.is_null (local.get $cell)))
    (drop (call $car (local.get $cell)))
    (if (local.get $writer)
      (then (struct.set $obj $a (ref.cast (ref $obj) (local.get $cell)) (local.get $value)))
      (else
        (local.set $value (call $car (local.get $cell)))
        (br_if $ordinary (ref.eq (local.get $value) (call $symbol-value ,(clos-library-reference '%unbound))))))
    (call $clear-values) (return (local.get $value)))
  #@(if (local.get $writer)
    (then (return ,(clos-default-call '%set-raw-instance-access-general
      (list #@(local.get $value) #@(local.get $object) #@(local.get $location))))))
  #@(return ,(clos-default-call '%raw-instance-access-general (list #@(local.get $object) #@(local.get $location)))))

;; This bounded cache is derived process state, deliberately absent from the
;; saved core roots. Only this runtime owns its array and entries. Epoch changes
;; discard the complete table, so old layouts cannot accumulate without bound.
(defparameter *slot-read-cache-size* 1024)

(define-runtime-function $slot-read-probe-permitted () (i32)
  (:locals ($scan :nullable-object) ($entry :nullable-object) ($name :nullable-object)
           ($function :nullable-object))
  #@(if (i32.eqz (call $boundp ,(clos-library-reference '%standard-slot-protocols)))
    (then (return (i32.const 0))))
  #@(local.set $scan (call $symbol-value ,(clos-library-reference '%standard-slot-protocols)))
  #@(call $check-apply-list (local.get $scan))
  (wat-list-loop ($scan)
    #@(local.set $entry (call $car (local.get $scan)))
    #@(local.set $name (call $car (local.get $entry)))
    #@(local.set $function (call $function-binding (local.get $name)))
    #@(if (ref.is_null (local.get $function)) (then (return (i32.const 0))))
    #@(local.set $function (call $binding-value (local.get $function)))
    #@(if (ref.is_null (local.get $function)) (then (return (i32.const 0))))
    #@(if (i32.eqz (ref.eq (local.get $function) (call $car (call $cdr (local.get $entry)))))
      (then (return (i32.const 0))))
    (runtime-wat-forms
      (loop for name in '(class-slots class-finalized-p slot-definition-name slot-definition-allocation slot-definition-type)
            collect `(if (ref.eq (local.get $name) ,(clos-library-reference name))
              (then (if (i32.eqz (call $standard-property-generic-p (local.get $function)
                    (call $car (call $cdr (call $cdr (local.get $entry))))))
                (then (return (i32.const 0)))))))))
  #@(i32.const 1))

(define-runtime-function $instance-slot-operation
    (($mode i32) ($object :nullable-object) ($name :nullable-object) ($value :nullable-object)) (:nullable-object)
  (:locals ($class :nullable-object) ($entry :nullable-object) ($function :nullable-object)
           ($plan (ref null $objects)) ($index i32) ($cache (ref null $objects)) ($proof-token :nullable-object))
  (progn
    (assert (and (<= 1 *slot-read-cache-size* 4096)
                 (zerop (logand *slot-read-cache-size* (1- *slot-read-cache-size*)))))
    (wat-fragment ""))
  #@(block $general
    (br_if $general (local.get $mode))
    (br_if $general (i32.ne (call $tag (local.get $object)) (i32.const 23)))
    (br_if $general (i32.eqz (call $symbolp (local.get $name))))
    (local.set $class (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))
    (br_if $general (i32.and (i32.ne (call $tag (local.get $class)) (i32.const 17))
      (i32.ne (call $tag (local.get $class)) (i32.const 23))))
    (if (i32.or (ref.is_null (global.get $slot-read-cache))
          (i32.eqz (ref.eq (global.get $slot-read-epoch) (global.get $slot-access-token))))
      (then
        (global.set $slot-read-cache (array.new_default $objects (i32.const ,*slot-read-cache-size*)))
        (global.set $slot-read-epoch (global.get $slot-access-token))))
    (local.set $cache (global.get $slot-read-cache))
    (local.set $index (i32.and (i32.xor
      (i32.shr_u (struct.get $obj $tag (ref.cast (ref $obj) (local.get $class))) (i32.const 6))
      (call $id (local.get $name))) (i32.const ,(1- *slot-read-cache-size*))))
    (local.set $entry (array.get $objects (local.get $cache) (local.get $index)))
    (block $miss
      (br_if $miss (i32.eqz (ref.test (ref $direct-function) (local.get $entry))))
      (local.set $plan (ref.cast (ref $objects)
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $entry)))))
      (br_if $miss (i32.eqz (ref.eq (array.get $objects (local.get $plan) (i32.const 1)) (local.get $class))))
      (br_if $miss (i32.eqz (ref.eq (array.get $objects (local.get $plan) (i32.const 3)) (local.get $name))))
      (return (call $specialized-slot-operation (ref.cast (ref $direct-function) (local.get $entry))
        (local.get $object) (local.get $value))))
    (if (if (result i32) (i32.eq (call $tag (local.get $entry)) (i32.const 7))
          (then (ref.eq (call $car (local.get $entry)) (local.get $class))) (else (i32.const 0)))
      (then (br_if $general (ref.eq (call $cdr (local.get $entry)) (local.get $name)))))
    ;; Cold probing cannot invoke overridden metadata getters and then repeat
    ;; their effects on fallback. Validate those getters before consulting them.
    (if (i32.or (i32.lt_s (global.get $slot-read-probe-allowed) (i32.const 0))
          (i32.eqz (ref.eq (global.get $slot-read-probe-epoch) (global.get $slot-access-token))))
      (then
        (local.set $proof-token (global.get $slot-access-token))
        (global.set $slot-read-probe-allowed (i32.const 0))
        (global.set $slot-read-probe-epoch (local.get $proof-token))
        (global.set $slot-read-probe-allowed (call $slot-read-probe-permitted))
        (br_if $general (i32.eqz (ref.eq (local.get $proof-token) (global.get $slot-access-token))))
        (global.set $slot-read-probe-epoch (local.get $proof-token))))
    (if (i32.eqz (global.get $slot-read-probe-allowed))
      (then (array.set $objects (local.get $cache) (local.get $index)
        (call $cons (local.get $class) (local.get $name))) (br $general)))
    ;; Mark a miss before calling Lisp: recursive or cooperative reads use the
    ;; ordinary path until a complete, epoch-checked plan is available.
    (array.set $objects (local.get $cache) (local.get $index)
      (call $cons (local.get $class) (local.get $name)))
    ;; Direct accessor dispatch supplies all execution; no source closure is needed.
    (local.set $function (call $node (i32.const 8) (i32.const 0) (ref.null eq) (ref.null eq) (ref.null eq)))
    (local.set $entry (call $specialize-accessor (local.get $function) (ref.null eq)
      (ref.cast (ref $direct-function) (call $direct-function
        (local.get $function) (ref.null eq) (local.get $name) (i32.const 11)))
      (call $cons (local.get $object) (ref.null eq))))
    (if (ref.is_null (local.get $entry))
      (then (array.set $objects (local.get $cache) (local.get $index)
        (call $cons (local.get $class) (local.get $name))) (br $general)))
    ;; Shared value cells can be exposed through their owner's raw storage.
    ;; Cache only owned instance locations, which are checked on every hit.
    (br_if $general (i32.eqz (ref.is_null (array.get $objects
      (ref.cast (ref $objects) (struct.get $direct-function $slot
        (ref.cast (ref $direct-function) (local.get $entry)))) (i32.const 6)))))
    (array.set $objects (local.get $cache) (local.get $index) (local.get $entry))
    (return (call $specialized-slot-operation (ref.cast (ref $direct-function) (local.get $entry))
      (local.get $object) (local.get $value))))
  #@(return_call $instance-slot-operation-general (local.get $mode) (local.get $object) (local.get $name) (local.get $value)))

;; This service has its own selector space. The established method metadata
;; callback (including its invalid-selector contract) is unchanged.
(define-runtime-function $clos-initarg-metadata-function () (:nullable-object)
  #@(if (ref.is_null (global.get $clos-initarg-metadata-function))
    (then (global.set $clos-initarg-metadata-function
      (call $node (i32.const 8)
        (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%clos-initarg-metadata)))
        (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(global.get $clos-initarg-metadata-function))

(define-runtime-function $clos-initarg-metadata-call (($args :nullable-object)) (:nullable-object)
  #@(call $check-apply-list (local.get $args))
  (wat-arity 3)
  #@(return_call $clos-initarg-metadata-value (call $car (local.get $args))
    (call $car (call $cdr (local.get $args))) (call $car (call $cdr (call $cdr (local.get $args))))))

(define-runtime-function $clos-applicable-class-key-p
    (($key :nullable-object) ($profile :nullable-object) ($args :nullable-object)) (i32)
  (:locals ($count i32) ($mask i32) ($flag :nullable-object) ($class :nullable-object)
           ($ca :nullable-object) ($cb :nullable-object) ($cc :nullable-object) ($cd :nullable-object))
  ;; Intrinsic keys contain selected classes in reverse argument order. Keep
  ;; EQL profiles ordinary. This bounded probe neither calls Lisp nor polls.
  #@(block $miss
    (block $classified (loop $next
      (br_if $classified (ref.is_null (local.get $profile)))
      (br_if $miss (i32.ge_u (local.get $count) (i32.const 4)))
      (br_if $miss (i32.ne (call $tag (local.get $profile)) (i32.const 7)))
      (br_if $miss (i32.ne (call $tag (local.get $args)) (i32.const 7)))
      (local.set $flag (call $car (local.get $profile)))
      (if (call $eql (local.get $flag) (call $int (i32.const 1)))
        (then
          (local.set $class (call $dispatch-class-known (call $car (local.get $args))))
          (br_if $miss (ref.is_null (local.get $class)))
          (local.set $mask (i32.or (local.get $mask) (i32.shl (i32.const 1) (local.get $count))))
          (if (i32.eqz (local.get $count)) (then (local.set $ca (local.get $class))))
          (if (i32.eq (local.get $count) (i32.const 1)) (then (local.set $cb (local.get $class))))
          (if (i32.eq (local.get $count) (i32.const 2)) (then (local.set $cc (local.get $class))))
          (if (i32.eq (local.get $count) (i32.const 3)) (then (local.set $cd (local.get $class)))))
        (else (br_if $miss (i32.eqz (call $eql (local.get $flag) (call $int (i32.const 0)))))))
      (local.set $profile (call $cdr (local.get $profile)))
      (local.set $args (call $cdr (local.get $args)))
      (local.set $count (i32.add (local.get $count) (i32.const 1))) (br $next)))
    (block $matched (loop $keys
      (br_if $matched (i32.eqz (local.get $count)))
      (local.set $count (i32.sub (local.get $count) (i32.const 1)))
      (if (i32.and (local.get $mask) (i32.shl (i32.const 1) (local.get $count)))
        (then
          (br_if $miss (i32.ne (call $tag (local.get $key)) (i32.const 7)))
          (local.set $class (if (result (ref null eq)) (i32.eqz (local.get $count))
            (then (local.get $ca)) (else (if (result (ref null eq)) (i32.eq (local.get $count) (i32.const 1))
              (then (local.get $cb)) (else (if (result (ref null eq)) (i32.eq (local.get $count) (i32.const 2))
                (then (local.get $cc)) (else (local.get $cd))))))))
          (br_if $miss (i32.eqz (ref.eq (call $car (local.get $key)) (local.get $class))))
          (local.set $key (call $cdr (local.get $key)))))
      (br $keys)))
    (return (ref.is_null (local.get $key))))
  #@(i32.const 0))

(define-runtime-function $clos-applicable-cache-hit
    (($gf :nullable-object) ($args :nullable-object)) (:nullable-object i32)
  (:locals ($info (ref $obj)) ($owned :nullable-object) ($metadata (ref $objects))
           ($version :nullable-object) ($token :nullable-object) ($profile :nullable-object) ($parameters :nullable-object)
           ($cache :nullable-object) ($scan :nullable-object) ($entry :nullable-object)
           ($key :nullable-object) ($methods :nullable-object) ($remaining i32))
  #@(block $miss
    (br_if $miss (i32.ne (call $tag (local.get $gf)) (i32.const 8)))
    (br_if $miss (i32.ne (call $value-n (local.get $gf)) (i32.const 2147483645)))
    (br_if $miss (i32.eqz (ref.is_null (call $symbol-value ,(clos-library-reference '%booting)))))
    ;; Check arity before capturing proofs: checked workers can poll.
    (local.set $parameters (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)))
    (drop (call $check-generic-arity (local.get $parameters) (local.get $args)))
    (br_if $miss (i32.eqz (ref.eq (local.get $parameters)
      (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)))))
    (local.set $cache (call $generic-property (local.get $gf) ,(clos-library-reference :applicable-cache)))
    (br_if $miss (ref.is_null (local.get $cache)))
    (local.set $token (call $generic-property (local.get $gf) ,(clos-library-reference :cache-token)))
    (local.set $profile (call $generic-property (local.get $gf) ,(clos-library-reference :dispatch-profile)))
    (local.set $version (call $symbol-value ,(clos-library-reference '%dispatch-version)))
    (local.set $info (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf)))))
    (local.set $owned (struct.get $obj $c (local.get $info)))
    (br_if $miss (i32.eqz (ref.test (ref $objects) (local.get $owned))))
    (local.set $metadata (ref.cast (ref $objects) (local.get $owned)))
    (br_if $miss (i32.ne (array.len (local.get $metadata)) (i32.const ,*generic-property-cache-size*)))
    (br_if $miss (i32.eqz (ref.eq (array.get $objects (local.get $metadata) (i32.const 0)) (call $symbol (i32.const 1)))))
    (br_if $miss (i32.eqz (call $eql (call $car (call $car (local.get $cache))) (local.get $version))))
    (local.set $scan (local.get $cache)) (local.set $remaining (i32.const 16))
    (loop $next
      (br_if $miss (ref.is_null (local.get $scan)))
      (br_if $miss (i32.eqz (local.get $remaining)))
      (br_if $miss (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
      (local.set $remaining (i32.sub (local.get $remaining) (i32.const 1)))
      (local.set $entry (call $car (local.get $scan)))
      (br_if $miss (i32.eqz (call $eql (call $list-length-native (local.get $entry)) (call $int (i32.const 4)))))
      (local.set $key (call $car (call $cdr (call $cdr (local.get $entry)))))
      (if (i32.and (ref.eq (call $car (call $cdr (local.get $entry))) (local.get $token))
            (call $clos-applicable-class-key-p (local.get $key) (local.get $profile) (local.get $args)))
        (then
          (local.set $methods (call $car (call $cdr (call $cdr (call $cdr (local.get $entry))))))
          (call $check-apply-list (local.get $methods))
          ;; Revalidate after the final checked worker, then classify again:
          ;; CHANGE-CLASS and list edits need not change the dispatch epoch.
          (br_if $miss (i32.eqz (ref.eq (struct.get $obj $c (local.get $info)) (local.get $owned))))
          (br_if $miss (i32.eqz (call $eql (local.get $version)
            (call $symbol-value ,(clos-library-reference '%dispatch-version)))))
          (br_if $miss (i32.eqz (ref.eq (local.get $token)
            (call $generic-property (local.get $gf) ,(clos-library-reference :cache-token)))))
          (br_if $miss (i32.eqz (ref.eq (local.get $cache)
            (call $generic-property (local.get $gf) ,(clos-library-reference :applicable-cache)))))
          (br_if $miss (i32.eqz (ref.eq (local.get $profile)
            (call $generic-property (local.get $gf) ,(clos-library-reference :dispatch-profile)))))
          (br_if $miss (i32.eqz (ref.eq (local.get $parameters)
            (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)))))
          (br_if $miss (i32.eqz (call $clos-applicable-class-key-p
            (local.get $key) (local.get $profile) (local.get $args))))
          (call $clear-values)
          (call $copy-spine (local.get $methods) (i32.const 0)) (i32.const 1) (return)))
      (local.set $scan (call $cdr (local.get $scan))) (br $next)))
  #@(ref.null eq) #@(i32.const 0))

;; Owned definition-time keyword identities live with the method and therefore
;; survive image save/restore. Ordinary MOP lambda lists remain unchanged.
;; Return a fresh reverse-order key spine. Stored profiles use declaration
;; order, while the shared normalization service returns reverse order.
(define-runtime-function $clos-initarg-copy-profile (($profile :nullable-object)) (:nullable-object)
  (:locals ($scan :nullable-object) ($head :nullable-object) ($key :nullable-object))
  #@(local.set $scan (call $car (local.get $profile)))
  ;; Private descriptors also arrive through untrusted core images. Reject
  ;; malformed or cyclic profiles before the no-poll copy loop.
  #@(drop (call $length (local.get $scan)))
  (wat-list-loop ($scan)
    #@(local.set $key (call $car (local.get $scan)))
    #@(if (i32.eqz (call $symbolp (local.get $key)))
      (then (call $type-fail (local.get $key) (call $symbol (i32.const ,(runtime-symbol-id 'symbol)))
        (i32.const ,(runtime-error-code :expected-designator)))))
    #@(local.set $head (call $cons (local.get $key) (local.get $head))))
  #@(call $cons (local.get $head) (call $cdr (local.get $profile))))

(define-runtime-function $clos-initialize-method-profile
    (($method :nullable-object) ($canonical :nullable-object)) (:nullable-object)
  (:locals ($ll :nullable-object) ($profile :nullable-object) ($ownership :nullable-object)
           ($source :nullable-object) ($proof :nullable-object))
  #@(call $check-type (local.get $method) ,(clos-library-reference 'method))
  ;; Custom method classes retain their ordinary metadata protocol. Descriptor
  ;; installation never accepts a caller-provided normalized profile.
  #@(if (i32.eqz (ref.eq (call $dispatch-class (local.get $method))
        (call $named-class ,(clos-library-reference 'standard-method)))) (then (return (local.get $method))))
  #@(if (ref.test (ref $method-initarg-profile) (struct.get $obj $c (ref.cast (ref $obj) (local.get $method))))
    (then (return (local.get $method))))
  #@(local.set $ownership (struct.get $obj $c (ref.cast (ref $obj) (local.get $method))))
  #@(local.set $ll ,(clos-default-get #@(local.get $method) :lambda-list))
  #@(if (ref.is_null (local.get $canonical)) (then (local.set $canonical (local.get $ll)))
    (else
      (if (ref.is_null (call $clos-canonical-method-p-cl (local.get $ll) (local.get $canonical)
            (call $clos-initarg-metadata-function)))
        (then (drop (call $clos-method-metadata-value (call $int (i32.const 12)) (local.get $canonical) (ref.null eq)))))))
  #@(local.set $profile (call $clos-initarg-copy-profile
    (call $clos-initarg-profile-cl (local.get $canonical) (call $clos-initarg-metadata-function))))
  #@(local.set $source (call $clos-initarg-source-cl (local.get $ll) (call $clos-initarg-metadata-function)))
  ;; Semantic key identities survive raw borrowing. Only an unchanged owned
  ;; storage proof permits the cheap path; a borrower receives checked reuse.
  #@(if (ref.is_null (global.get $method-profile-proof))
    (then (global.set $method-profile-proof (call $cons (ref.null eq) (ref.null eq)))))
  #@(if (i32.and (i32.eqz (ref.is_null (local.get $ownership)))
        (ref.eq (local.get $ownership) (struct.get $obj $c (ref.cast (ref $obj) (local.get $method)))))
    (then (local.set $proof (global.get $method-profile-proof))))
  #@(if (i32.and (ref.eq (local.get $ll) ,(clos-default-get #@(local.get $method) :lambda-list))
        (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (local.get $method)))
          (call $named-class ,(clos-library-reference 'standard-method))))
    (then
      (if (i32.eqz (ref.test (ref $method-initarg-profile)
            (struct.get $obj $c (ref.cast (ref $obj) (local.get $method)))))
        (then (struct.set $obj $c (ref.cast (ref $obj) (local.get $method))
          (struct.new $method-initarg-profile (local.get $ll) (local.get $profile) (local.get $source) (local.get $proof)))))))
  #@(local.get $method))

(define-runtime-function $clos-allocate-method
    (($class :nullable-object) ($properties :nullable-object) ($canonical :nullable-object)) (:nullable-object)
  (:locals ($method :nullable-object) ($count i32))
  #@(call $check-type (local.get $class) ,(clos-library-reference 'class))
  #@(local.set $count (call $length (local.get $properties)))
  (wat-fail-when #@(i32.and (local.get $count) (i32.const 1)) :wrong-arity)
  ;; Own the property spine before attaching a private descriptor. No caller
  ;; can retain its cells through the allocation input.
  #@(local.set $properties (call $copy-spine (local.get $properties) (i32.const 0)))
  #@(local.set $method (call $node (i32.const 23) (i32.const 0)
    (local.get $class) (local.get $properties) (call $symbol (i32.const 1))))
  #@(if (ref.is_null (local.get $canonical)) (then (return (local.get $method))))
  #@(call $clos-initialize-method-profile (local.get $method) (local.get $canonical)))

(define-runtime-function $clos-initarg-profile
    (($method :nullable-object) ($ll :nullable-object)) (:nullable-object)
  (:locals ($protocol :nullable-object) ($function :nullable-object) ($token :nullable-object)
           ($owned :nullable-object) ($proof :nullable-object))
  #@(if (i32.ne (call $tag (local.get $method)) (i32.const 23)) (then (return (ref.null eq))))
  #@(if (i32.eqz (call $boundp ,(clos-library-reference '%standard-initarg-protocol)))
    (then (return (ref.null eq))))
  #@(local.set $protocol (call $symbol-value ,(clos-library-reference '%standard-initarg-protocol)))
  #@(local.set $token (global.get $slot-access-token))
  #@(local.set $function (call $fdefinition ,(clos-library-reference 'method-lambda-list)))
  #@(if (i32.eqz (ref.eq (local.get $function) (call $car (local.get $protocol))))
    (then (return (ref.null eq))))
  #@(if (i32.eqz (call $standard-property-generic-p (local.get $function) (call $car (call $cdr (local.get $protocol)))))
    (then (return (ref.null eq))))
  ;; The owned fast-hit checks and returned copy do not poll. Borrowed and
  ;; restored metadata instead use checked live matching below, which may poll.
  ;; The live reader already ran before this query.
  #@(if (i32.eqz (ref.eq (local.get $token) (global.get $slot-access-token))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq ,(clos-default-get #@(call $car (call $cdr (local.get $protocol))) :function)
        (call $car (call $cdr (call $cdr (local.get $protocol)))))) (then (return (ref.null eq))))
  #@(local.set $owned (struct.get $obj $c (ref.cast (ref $obj) (local.get $method))))
  #@(if (i32.eqz (ref.test (ref $method-initarg-profile) (local.get $owned))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (local.get $ll) ,(clos-default-get #@(local.get $method) :lambda-list)))
    (then (return (ref.null eq))))
  #@(if (ref.is_null (global.get $method-profile-proof))
    (then (global.set $method-profile-proof (call $cons (ref.null eq) (ref.null eq)))))
  #@(local.set $proof (struct.get $method-initarg-profile $proof (ref.cast (ref $method-initarg-profile) (local.get $owned))))
  #@(if (i32.and (ref.eq (local.get $proof) (global.get $method-profile-proof))
        (ref.eq (local.get $ll) (struct.get $method-initarg-profile $lambda-list
          (ref.cast (ref $method-initarg-profile) (local.get $owned)))))
    (then (return (call $clos-initarg-copy-profile
      (struct.get $method-initarg-profile $profile (ref.cast (ref $method-initarg-profile) (local.get $owned)))))))
  ;; Borrowed metadata is checked on every request. A restored non-null proof
  ;; is from a different process: validate its source mapping before reusing it.
  #@(if (ref.is_null (call $clos-initarg-profile-matches-cl (local.get $ll)
        (struct.get $method-initarg-profile $source (ref.cast (ref $method-initarg-profile) (local.get $owned)))
        (struct.get $method-initarg-profile $profile (ref.cast (ref $method-initarg-profile) (local.get $owned)))
        (call $clos-initarg-metadata-function))) (then (return (ref.null eq))))
  ;; The matcher may call checked workers. Never reinstate a proof revoked by
  ;; a borrower, setter, reader change, or class/storage change during it.
  #@(if (i32.and (i32.eqz (ref.is_null (local.get $proof)))
        (i32.and (ref.eq (local.get $token) (global.get $slot-access-token))
          (i32.and (ref.eq (local.get $owned) (struct.get $obj $c (ref.cast (ref $obj) (local.get $method))))
            (i32.and (ref.eq (local.get $ll) ,(clos-default-get #@(local.get $method) :lambda-list))
              (ref.eq (local.get $proof) (struct.get $method-initarg-profile $proof
                (ref.cast (ref $method-initarg-profile) (local.get $owned))))))))
    (then (struct.set $method-initarg-profile $proof (ref.cast (ref $method-initarg-profile) (local.get $owned))
      (global.get $method-profile-proof))))
  #@(call $clos-initarg-copy-profile
    (struct.get $method-initarg-profile $profile (ref.cast (ref $method-initarg-profile) (local.get $owned)))))

(define-runtime-function $clos-initarg-metadata-value
    (($selector-value :nullable-object) ($object :nullable-object) ($extra :nullable-object)) (:nullable-object)
  (:locals ($selector i32) ($class :nullable-object) ($metaclass :nullable-object) ($data (ref $objects))
           ($methods :nullable-object) ($hit i32) ($ll :nullable-object) ($profile :nullable-object))
  #@(local.set $selector (call $natural-count (local.get $selector-value)))
  #@(if (i32.eq (local.get $selector) (i32.const 31))
    (then
      (local.set $ll ,(clos-default-call 'method-lambda-list (list #@(local.get $object))))
      ;; Match the separate reader callback's primary-value cleanup and the
      ;; following profile callback's checkpoint before observing its proofs.
      ;; The live reader still runs exactly once, including custom methods.
      (call $clear-values)
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (local.set $profile (call $clos-initarg-profile (local.get $object) (local.get $ll)))
      (if (i32.eqz (ref.is_null (local.get $profile))) (then (return (local.get $profile))))
      (call $clear-values)
      (return_call $clos-initarg-profile-cl (local.get $ll) (call $clos-initarg-metadata-function))))
  #@(if (i32.eq (local.get $selector) (i32.const 30))
    (then (return_call $clos-initarg-profile-cl (local.get $object) (call $clos-initarg-metadata-function))))
  #@(if (i32.eq (local.get $selector) (i32.const 29))
    (then (return_call $clos-initialize-method-profile (local.get $object) (ref.null eq))))
  #@(if (i32.eq (local.get $selector) (i32.const 28))
    (then (return_call $clos-initarg-profile (local.get $object) (local.get $extra))))
  #@(if (i32.le_u (local.get $selector) (i32.const 14))
    (then (return_call $clos-method-metadata-value (local.get $selector-value) (local.get $object) (local.get $extra))))
  #@(if (i32.eq (local.get $selector) (i32.const 15))
    (then (return (call $fdefinition (local.get $object)))))
  #@(if (i32.eq (local.get $selector) (i32.const 16))
    (then
      (call $check-type (local.get $object) ,(clos-library-reference 'generic-function))
      (call $check-apply-list (local.get $extra))
      (call $clos-applicable-cache-hit (local.get $object) (local.get $extra))
      (local.set $hit) (local.set $methods)
      (if (local.get $hit) (then (return (local.get $methods))))
      (return ,(clos-default-call '%compute-applicable (list #@(local.get $object) #@(local.get $extra))))))
  #@(if (i32.eq (local.get $selector) (i32.const 17))
    (then (return ,(clos-default-call 'method-lambda-list (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 18))
    (then
      (if (i32.eqz (call $symbolp (local.get $object)))
        (then (call $type-fail (local.get $object) (call $symbol (i32.const ,(runtime-symbol-id 'symbol)))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (return ,(clos-default-call '%keyword (list #@(local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 19))
    (then
      (if (i32.ne (call $tag (local.get $extra)) (i32.const 7))
        (then (call $list-type-error (local.get $extra) (i32.const ,(runtime-symbol-id 'cons)))))
      (call $check-apply-list (call $car (local.get $extra)))
      (return ,(clos-default-call '%check-keys
        (list #@(local.get $object) #@(call $car (local.get $extra)) #@(call $cdr (local.get $extra)))))))
  #@(if (i32.eq (local.get $selector) (i32.const 20))
    (then
      (if (i32.ne (call $tag (local.get $object)) (i32.const 34))
        (then (call $list-type-error (local.get $object) (i32.const ,(runtime-symbol-id 'simple-vector)))))
      (local.set $data (call $vector-data (local.get $object)))
      (if (i32.ne (array.len (local.get $data)) (i32.const 3))
        (then (call $type-fail (local.get $object)
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'simple-vector)))
            (call $cons (call $int (i32.const 3)) (ref.null eq)))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (return (local.get $object))))
  #@(if (i32.eq (local.get $selector) (i32.const 21))
    (then (return (call $bool (call $symbolp (local.get $object))))))
  #@(if (i32.eq (local.get $selector) (i32.const 22))
    (then
      (if (call $symbolp (local.get $object))
        (then (local.set $object ,(clos-default-call 'find-class (list #@(local.get $object))))))
      (call $check-type (local.get $object) ,(clos-library-reference 'class))
      (return (local.get $object))))
  #@(if (i32.or (i32.eq (local.get $selector) (i32.const 23))
                (i32.eq (local.get $selector) (i32.const 25)))
    (then
      (call $check-type (local.get $object) ,(clos-library-reference 'class))
      (if (i32.eq (local.get $selector) (i32.const 23))
        (then
          (local.set $metaclass (call $dispatch-class (local.get $object)))
          ,(wat-fail-when #@(i32.or
            (call $class-subclassp (local.get $object) (call $named-class ,(clos-library-reference 'built-in-class)))
            (ref.eq (local.get $metaclass) (call $named-class ,(clos-library-reference 'built-in-class)))) :condition)
          (return (local.get $metaclass))))
      (call $check-type (local.get $extra) ,(clos-library-reference 'class))
      (return (call $bool (i32.or
        (call $class-subclassp (local.get $object) (call $named-class ,(clos-library-reference 'generic-function)))
        (call $class-subclassp (local.get $extra) (call $named-class ,(clos-library-reference 'funcallable-standard-class))))))))
  #@(if (i32.eq (local.get $selector) (i32.const 24))
    (then (return_call $standard-allocate-storage (local.get $object))))
  #@(if (i32.eq (local.get $selector) (i32.const 26))
    (then
      (if (i32.ne (call $tag (local.get $object)) (i32.const 23))
        (then (call $type-fail (local.get $object) ,(clos-library-reference 'standard-object)
          (i32.const ,(runtime-error-code :expected-designator)))))
      (return (call $node (i32.const 8) (i32.const 2147483645)
        (local.get $object) (ref.null eq) (ref.null eq)))))
  #@(if (i32.eq (local.get $selector) (i32.const 27))
    (then (return (call $bool (i32.and (call $symbolp (local.get $object))
      (i32.and (i32.and (i32.eqz (ref.is_null (local.get $object)))
          (i32.eqz (ref.eq (local.get $object) (call $symbol (i32.const 1)))))
        (i32.eqz (call $keywordp (local.get $object)))))))))
  (wat-fail :malformed-form)
  #@(unreachable))

(defun $clos-initarg-source-cl ()
  (runtime-function-definition '$clos-initarg-source-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-initarg-source-cl 'miricl-runtime:clos-initarg-source-value
      :fixed-callbacks '((1 %%clos-initarg-metadata))))))
(defun $clos-initarg-profile-matches-cl ()
  (runtime-function-definition '$clos-initarg-profile-matches-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-initarg-profile-matches-cl 'miricl-runtime:clos-initarg-profile-matches-value
      :fixed-callbacks '((3 %%clos-initarg-metadata))))))

(defun $clos-canonical-method-p-cl ()
  (runtime-function-definition '$clos-canonical-method-p-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-canonical-method-p-cl 'miricl-runtime:clos-canonical-method-metadata-p
      :fixed-callbacks '((2 %%clos-initarg-metadata))))))

(defun $clos-initarg-profile-cl ()
  (runtime-function-definition '$clos-initarg-profile-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-initarg-profile-cl 'miricl-runtime:clos-initarg-profile-value
      :fixed-callbacks '((1 %%clos-initarg-metadata))))))

(defun $clos-check-initargs-cl ()
  (runtime-function-definition '$clos-check-initargs-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-check-initargs-cl 'miricl-runtime:clos-check-initargs-value
      :fixed-callbacks '((3 %%clos-initarg-metadata))))))
(defun $clos-allocate-instance-cl ()
  (runtime-function-definition '$clos-allocate-instance-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$clos-allocate-instance-cl 'miricl-runtime:clos-allocate-instance-value
      :fixed-callbacks '((1 %%clos-initarg-metadata))))))

(define-runtime-section clos-defaults-runtime-wat ()
  ;; These roots are absent from saved Lisp roots. Restoring an image starts
  ;; with no executable constructor permissions, just like the slot-read cache.
  #@(type $constructor-plan (struct (field $class (ref null eq)) (field $key (ref null eq))
    (field $layout (ref null eq)) (field $names (ref $objects)) (field $count i32) (field $mask i32)
    (field $version (ref null eq)) (field $init (ref $objects)) (field $shared (ref $objects))))
  #@(global $constructor-plans (mut (ref null $objects)) (ref.null $objects))
  #@(global $constructor-plan-next (mut i32) (i32.const 0))
  ($constructor-property) ($constructor-symbol-p) ($constructor-default-binding) ($constructor-binding-current)
  ($constructor-plan-build) ($constructor-plan-call)
  ($clos-default-call1)
  #@(global $clos-method-metadata-function (mut (ref null eq)) (ref.null eq))
  ($clos-method-metadata-function) ($clos-method-metadata-call) ($clos-method-metadata-value)
  ($clos-class-slots-cl) ($clos-class-default-initargs-cl)
  ($clos-default-initargs-cl) ($clos-shared-initialize-cl) ($clos-find-slot-cl)
  #@(global $clos-initarg-metadata-function (mut (ref null eq)) (ref.null eq))
  ($clos-initarg-metadata-function) ($clos-initarg-metadata-call) ($clos-initarg-metadata-value)
  #@(type $method-initarg-profile (struct (field $lambda-list (ref null eq)) (field $profile (ref null eq))
    (field $source (ref null eq)) (field $proof (mut (ref null eq)))))
  ;; Process-only capability: deserialized profiles must validate their compact
  ;; source mapping before acquiring this process's fast-path proof.
  #@(global $method-profile-proof (mut (ref null eq)) (ref.null eq))
  ($clos-initarg-copy-profile) ($clos-initialize-method-profile) ($clos-allocate-method)
  ($clos-initarg-profile) ($clos-initarg-profile-cl) ($clos-canonical-method-p-cl)
  ($clos-initarg-source-cl) ($clos-initarg-profile-matches-cl)
  ($clos-check-initargs-cl) ($clos-allocate-instance-cl) ($standard-allocate-storage)
  ($clos-applicable-class-key-p) ($clos-applicable-cache-hit)
  #@(global $slot-access-token (mut (ref null eq)) (ref.null eq))
  #@(global $slot-read-cache (mut (ref null $objects)) (ref.null $objects))
  #@(global $slot-read-epoch (mut (ref null eq)) (ref.null eq))
  #@(global $slot-read-probe-epoch (mut (ref null eq)) (ref.null eq))
  #@(global $slot-read-probe-allowed (mut i32) (i32.const -1))
  ($slot-read-probe-permitted) ($instance-slot-operation)
  ($invalidate-slot-access) ($slot-protocols-standard-p) ($specialize-accessor) ($specialized-slot-call) ($specialized-slot-operation)
  ($raw-instance-access)
  ($clos-meta-value) ($standard-slot-cell) ($standard-slot-call)
  ($standard-allocate-instance) ($standard-shared-initialize) ($standard-default-initargs)
  ($standard-check-initargs) ($standard-clos-call))
