;;;; Native cache hits and method invocation. Cache misses retain the MOP.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $generic-discriminator (($gf :nullable-object) ($function :nullable-object)) (:object)
  (wat-fail-when #@(i32.ne (call $tag (local.get $function)) (i32.const 8)) :malformed-form)
  (wat-fail-when #@(call $value-n (local.get $function)) :malformed-form)
  #@(struct.new $generic-discriminator (call $allocate-object-tag (i32.const 8)) (i32.const 0)
    (struct.get $obj $f (ref.cast (ref $obj) (local.get $function))) (struct.get $obj $digits (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $a (ref.cast (ref $obj) (local.get $function))) (struct.get $obj $b (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $c (ref.cast (ref $obj) (local.get $function))) (local.get $gf)))

(defparameter *generic-property-cache-keys*
  '(:discriminating-function :cache :lambda-list :methods :cache-protocol :name :eql-dispatch-profile))

(define-runtime-function $generic-property (($gf :nullable-object) ($key :nullable-object)) (:nullable-object)
  (:locals ($info (ref $obj)) ($owned :nullable-object) ($cache (ref null $objects))
           ($cell :nullable-object) ($index i32))
  #@(local.set $info (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf)))))
  #@(block $ordinary
    (local.set $owned (struct.get $obj $c (local.get $info)))
    (if (ref.eq (local.get $owned) (call $symbol (i32.const 1)))
      (then
        (local.set $cache (array.new_default $objects (i32.const ,(1+ (length *generic-property-cache-keys*)))))
        (array.set $objects (local.get $cache) (i32.const 0) (local.get $owned))
        (struct.set $obj $c (local.get $info) (local.get $cache)))
      (else
        (br_if $ordinary (i32.eqz (ref.test (ref $objects) (local.get $owned))))
        (local.set $cache (ref.cast (ref $objects) (local.get $owned)))
        (br_if $ordinary (i32.ne (array.len (local.get $cache)) (i32.const ,(1+ (length *generic-property-cache-keys*)))))
        (br_if $ordinary (i32.eqz (ref.eq (array.get $objects (local.get $cache) (i32.const 0)) (call $symbol (i32.const 1)))))))
    (block $selected
      ,@(loop for key in *generic-property-cache-keys* for index from 1 collect
          `(if (ref.eq (local.get $key) ,(clos-library-reference key))
              (then (local.set $index (i32.const ,index)) (br $selected))))
      (br $ordinary))
    (local.set $cell (array.get $objects (local.get $cache) (local.get $index)))
    (if (ref.is_null (local.get $cell))
      (then
        (local.set $cell (call $property-cell (struct.get $obj $b (local.get $info)) (local.get $key)))
        (array.set $objects (local.get $cache) (local.get $index) (local.get $cell))))
    (return (call $car (local.get $cell))))
  #@(call $car (call $property-cell (struct.get $obj $b (local.get $info)) (local.get $key))))

;; The next-method context has lexical extent through the method's closures.
;; Pack the private context into one object while retaining the same dynamic
;; binding and lexical lifetime as the former five/six-element list.
(define-runtime-function $invoke-method
    (($gf :nullable-object) ($method :nullable-object) ($rest :nullable-object)
     ($args :nullable-object) ($continuation :nullable-object) ($forbidden i32)) (:nullable-object)
  (:locals ($saved :nullable-object) ($context :nullable-object) ($result :nullable-object) ($function :nullable-object) ($thrown i32))
  #@(local.set $context (struct.new $method-context
    (call $allocate-object-tag (i32.const 53)) (local.get $forbidden) (f64.const 0) (ref.null $digits)
    (local.get $gf) (local.get $method) (local.get $rest) (local.get $args) (local.get $continuation)))
  #@(local.set $saved (global.get $dynamic-variables))
  #@(global.set $dynamic-variables (call $bind (local.get $saved)
    ,(clos-library-reference '%next-context) (local.get $context)))
  #@(block $returned
    (block $thrown
      (try_table (catch $transfer $thrown)
        (local.set $function (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method)))
          ,(clos-library-reference :function))))
        (local.set $result (call $apply (local.get $function)
          (call $native-two-arguments (local.get $function) (local.get $args) (local.get $rest))))
        (br $returned)))
    (local.set $thrown (i32.const 1)))
  #@(global.set $dynamic-variables (local.get $saved))
  #@(if (local.get $thrown) (then (throw $transfer)))
  #@(local.get $result))

;; %NTH is private to the object system. Ordinary lists remain supported for
;; callers that construct effective-method contexts themselves.
(define-runtime-function $method-context-element (($index :nullable-object) ($context :nullable-object)) (:nullable-object)
  (:locals ($n i32) ($packed (ref null $method-context)))
  #@(if (ref.test (ref $method-context) (local.get $context))
    (then
      (local.set $n (call $natural-count (local.get $index)))
      (local.set $packed (ref.cast (ref $method-context) (local.get $context)))
      (if (i32.eqz (local.get $n)) (then (return (struct.get $obj $a (ref.cast (ref $obj) (local.get $context))))))
      (if (i32.eq (local.get $n) (i32.const 1)) (then (return (struct.get $obj $b (ref.cast (ref $obj) (local.get $context))))))
      (if (i32.eq (local.get $n) (i32.const 2)) (then (return (struct.get $obj $c (ref.cast (ref $obj) (local.get $context))))))
      (if (i32.eq (local.get $n) (i32.const 3)) (then (return (struct.get $method-context $args (local.get $packed)))))
      (if (i32.eq (local.get $n) (i32.const 4)) (then (return (struct.get $method-context $next (local.get $packed)))))
      (if (i32.eq (local.get $n) (i32.const 5))
        (then (return (if (result (ref null eq)) (call $value-n (local.get $context))
          (then (call $symbol (i32.const 1))) (else (ref.null eq))))))
      (return (ref.null eq))))
  #@(call $car (call $nthcdr-native (local.get $index) (local.get $context))))

;; Shared by CLASS-OF and cache lookup, including lazy condition class creation.
(define-runtime-function $dispatch-class (($object :nullable-object)) (:nullable-object)
  (:locals ($tag i32) ($id i32) ($length i32) ($high i32) ($class :nullable-object))
  #@(local.set $tag (call $tag (local.get $object)))
  #@(if (i32.eq (local.get $tag) (i32.const 47))
    (then (return (call $named-class ,(clos-library-reference 'miricl-js:value)))))
  #@(if (i32.eq (local.get $tag) (i32.const 23))
    (then (return (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))
  #@(if (i32.eq (local.get $tag) (i32.const 17))
    (then
      (local.set $class (call $named-class (struct.get $obj $a (ref.cast (ref $obj) (local.get $object)))))
      (if (i32.eqz (ref.is_null (local.get $class))) (then (return (local.get $class))))
      (return (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'find-class)))
        (call $cons (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))) (ref.null eq))))))
  #@(if (i32.eq (local.get $tag) (i32.const 8))
    (then (if (i32.eq (call $value-n (local.get $object)) (i32.const 2147483645))
      (then (return (struct.get $obj $a (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))))))
  #@(if (i32.eq (local.get $tag) (i32.const 35))
    (then
      (local.set $id (i32.const ,(runtime-symbol-id 'array)))
      (if (i32.eq (call $array-rank-value (local.get $object)) (i32.const 1))
        (then
          (local.set $high (call $array-kind (local.get $object)))
          (local.set $id (if (result i32) (i32.and (local.get $high) (i32.const 1))
            (then (i32.const ,(runtime-symbol-id 'string)))
            (else (if (result i32) (i32.eq (local.get $high) (i32.const 2))
              (then (i32.const ,(runtime-symbol-id 'bit-vector))) (else (i32.const ,(runtime-symbol-id 'vector)))))))))
      (return (call $named-class (call $symbol (local.get $id))))))
  #@(if (i32.eq (local.get $tag) (i32.const 28))
    (then (return (call $named-class (call $symbol
      (select (i32.const ,(runtime-symbol-id 'logical-pathname)) (i32.const ,(runtime-symbol-id 'pathname))
        (call $value-n (local.get $object))))))))
  #@(local.set $id (i32.const ,(runtime-symbol-id t)))
  (runtime-wat-forms
    (loop for (tag name) in '((0 null) (2 ratio) (3 single-float) (4 double-float) (5 complex)
                            (6 symbol) (7 cons) (8 function) (10 string) (12 package)
                            (16 character) (19 restart) (29 hash-table)
                            (34 vector) (36 bit-vector) (37 readtable) (39 string-stream)
                            (40 two-way-stream) (41 synonym-stream) (42 random-state) (43 broadcast-stream) (44 file-stream) (45 concatenated-stream) (46 echo-stream))
      collect `(if (i32.eq (local.get $tag) (i32.const ,tag))
        (then (return (call $named-class (call $symbol (i32.const ,(runtime-symbol-id name)))))))))
  #@(if (i32.eq (local.get $tag) (i32.const 22))
    (then (local.set $id (if (result i32) (call $console-stream-p (local.get $object))
      (then (i32.const ,(runtime-symbol-id 'stream))) (else (i32.const ,(runtime-symbol-id 'string-stream)))))))
  #@(if (i32.eq (local.get $tag) (i32.const 1))
    (then
      (local.set $id (i32.const ,(runtime-symbol-id 'bignum)))
      (if (call $scalar-integer-p (local.get $object))
        (then (local.set $id (i32.const ,(runtime-symbol-id 'fixnum)))))))
  #@(call $named-class (call $symbol (local.get $id))))

(define-runtime-function $dispatch-eql-key
    (($object :nullable-object) ($specializers :nullable-object)) (:nullable-object)
  (:locals ($specializer :nullable-object))
  #@(block $done (loop $next (br_if $done (ref.is_null (local.get $specializers)))
    (local.set $specializer (call $car (local.get $specializers)))
    (if (call $eql (local.get $object)
      (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $specializer))) ,(clos-library-reference :object))))
      (then (return (local.get $specializer))))
    (local.set $specializers (call $cdr (local.get $specializers))) (br $next)))
  #@(ref.null eq))

(define-runtime-function $generic-key-match
    (($key :nullable-object) ($gf :nullable-object) ($a :nullable-object)
     ($ca :nullable-object) ($cb :nullable-object) ($cc :nullable-object) ($cd :nullable-object) ($mode i32)) (i32)
  (:locals ($position i32) ($object :nullable-object) ($class :nullable-object) ($scan :nullable-object) ($index i32)
           ($profile :nullable-object))
  ;; Low three bits hold required arity; bit 3 marks the zero-arity native path.
  ;; Bits 4..7 select class tests; bits 8..11 add EQL tests at those positions.
  #@(if (i32.eqz (i32.and (local.get $mode) (i32.const 7)))
    (then (return (ref.is_null (local.get $key)))))
  #@(local.set $position (i32.sub (i32.and (local.get $mode) (i32.const 7)) (i32.const 1)))
  #@(loop $next
    (if (i32.and (local.get $mode) (i32.shl (i32.const 16) (local.get $position)))
      (then
        (local.set $scan (local.get $a)) (local.set $index (local.get $position))
        (block $argument (loop $step
          (br_if $argument (i32.eqz (local.get $index)))
          (local.set $scan (call $cdr (local.get $scan)))
          (local.set $index (i32.sub (local.get $index) (i32.const 1))) (br $step)))
        (local.set $object (call $car (local.get $scan)))
        (local.set $class (if (result (ref null eq)) (i32.eq (local.get $position) (i32.const 3))
          (then (local.get $cd)) (else
            (if (result (ref null eq)) (i32.eq (local.get $position) (i32.const 2))
              (then (local.get $cc)) (else
                (if (result (ref null eq)) (local.get $position)
                  (then (local.get $cb)) (else (local.get $ca))))))))
        (if (i32.and (local.get $mode) (i32.shl (i32.const 256) (local.get $position)))
          (then
            (local.set $profile (call $generic-property (local.get $gf) ,(clos-library-reference :eql-dispatch-profile)))
            (local.set $index (local.get $position))
            (block $profile (loop $step (br_if $profile (i32.eqz (local.get $index)))
              (local.set $profile (call $cdr (local.get $profile)))
              (local.set $index (i32.sub (local.get $index) (i32.const 1))) (br $step)))
            (if (i32.eqz (ref.eq (call $car (local.get $key))
                  (call $dispatch-eql-key (local.get $object) (call $car (local.get $profile)))))
              (then (return (i32.const 0))))
            (local.set $key (call $cdr (local.get $key)))))
        (if (i32.eqz (ref.eq (call $car (local.get $key)) (local.get $class)))
          (then (return (i32.const 0))))
        (local.set $key (call $cdr (local.get $key)))))
    (if (i32.eqz (local.get $position)) (then (return (ref.is_null (local.get $key)))))
    (local.set $position (i32.sub (local.get $position) (i32.const 1))) (br $next))
  #@(i32.const 0))

(define-runtime-function $generic-cache-permitted-p (($gf :nullable-object)) (i32)
  (:locals ($proof :nullable-object))
  #@(if (i32.eqz (ref.is_null (call $symbol-value ,(clos-library-reference '%cache-permitted))))
    (then (return (i32.const 1))))
  #@(local.set $proof (call $generic-property (local.get $gf) ,(clos-library-reference :cache-protocol)))
  #@(if (i32.eqz (ref.is_null (local.get $proof)))
    (then
      (if (call $eql (call $car (local.get $proof))
            (call $symbol-value ,(clos-library-reference '%dispatch-version)))
        (then (return (i32.eqz (ref.is_null (call $cdr (local.get $proof)))))))))
  #@(i32.eqz (ref.is_null (call $apply ,(clos-library-reference '%dispatch-cache-permitted-p)
    (call $cons (local.get $gf) (ref.null eq))))))

(define-runtime-function $generic-apply (($gf :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($function :nullable-object) ($cache :nullable-object) ($entry :nullable-object)
           ($version :nullable-object) ($key :nullable-object) ($mode-object :nullable-object)
           ($ca :nullable-object) ($cb :nullable-object) ($cc :nullable-object) ($cd :nullable-object)
           ($mode i32) ($computed i32))
  #@(local.set $function (call $generic-property (local.get $gf) ,(clos-library-reference :discriminating-function)))
  ;; Bootstrap generics use NIL for the same default dispatcher. Custom
  ;; discriminators and trace wrappers retain their own call path.
  #@(if (if (result i32) (ref.is_null (local.get $function)) (then (i32.const 1))
    (else (if (result i32) (ref.test (ref $generic-discriminator) (local.get $function))
      (then (ref.eq (struct.get $generic-discriminator $owner
        (ref.cast (ref $generic-discriminator) (local.get $function))) (local.get $gf)))
      (else (i32.const 0)))))
    (then
      (if (call $generic-cache-permitted-p (local.get $gf))
        (then
          (local.set $cache (call $generic-property (local.get $gf) ,(clos-library-reference :cache)))
          (local.set $version (call $symbol-value ,(clos-library-reference '%dispatch-version)))
          (block $miss (loop $next
            (br_if $miss (ref.is_null (local.get $cache)))
            (local.set $entry (call $car (local.get $cache)))
            (if (call $eql (call $car (local.get $entry)) (local.get $version))
              (then
                (local.set $mode-object (call $car (call $cdr (call $cdr (call $cdr (local.get $entry))))))
                (local.set $mode (call $natural-count (local.get $mode-object)))
                (br_if $miss (i32.eqz (local.get $mode)))
                (if (i32.and (local.get $mode) (i32.const 4096))
                  (then (br_if $miss (i32.eqz (ref.eq (global.get $slot-access-token)
                    (call $car (call $cdr (call $cdr (call $cdr (call $cdr (call $cdr (local.get $entry))))))))))))
                (if (i32.eqz (local.get $computed))
                  (then
                    (if (i32.and (local.get $mode) (i32.const 16))
                      (then (local.set $ca (call $dispatch-class (call $car (local.get $args))))))
                    (if (i32.and (local.get $mode) (i32.const 32))
                      (then (local.set $cb (call $dispatch-class (call $car (call $cdr (local.get $args)))))))
                    (if (i32.and (local.get $mode) (i32.const 64))
                      (then (local.set $cc (call $dispatch-class (call $car (call $cdr (call $cdr (local.get $args))))))))
                    (if (i32.and (local.get $mode) (i32.const 128))
                      (then (local.set $cd (call $dispatch-class (call $car (call $cdr (call $cdr (call $cdr (local.get $args)))))))))
                    (local.set $computed (i32.const 1))))
                (local.set $key (call $car (call $cdr (local.get $entry))))
                (if (call $generic-key-match (local.get $key) (local.get $gf) (local.get $args)
                      (local.get $ca) (local.get $cb) (local.get $cc) (local.get $cd) (local.get $mode))
                  (then
                    (drop (call $check-generic-arity
                      (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)) (local.get $args)))
                    (return (call $apply (call $car (call $cdr (call $cdr (local.get $entry))))
                      (call $cons (local.get $args) (ref.null eq))))))))
            (local.set $cache (call $cdr (local.get $cache))) (br $next)))))
      (return (call $apply ,(clos-library-reference '%invoke-effective)
        (call $cons (local.get $gf) (call $cons (local.get $args) (ref.null eq)))))))
  #@(call $apply ,(clos-library-reference '%dispatch-generic)
    (call $cons (local.get $gf) (call $cons (local.get $args) (ref.null eq)))))

(define-runtime-function $standard-property-generic-p
    (($function :nullable-object) ($method :nullable-object)) (i32)
  (:locals ($methods :nullable-object) ($discriminator :nullable-object))
  #@(if (i32.ne (call $tag (local.get $function)) (i32.const 8)) (then (return (i32.const 0))))
  #@(if (i32.ne (call $value-n (local.get $function)) (i32.const 2147483645)) (then (return (i32.const 0))))
  #@(if (i32.eqz (call $generic-cache-permitted-p (local.get $function)))
    (then (return (i32.const 0))))
  #@(local.set $methods (call $generic-property (local.get $function) ,(clos-library-reference :methods)))
  #@(if (i32.eqz (ref.eq (call $car (local.get $methods)) (local.get $method))) (then (return (i32.const 0))))
  #@(if (i32.eqz (ref.is_null (call $cdr (local.get $methods)))) (then (return (i32.const 0))))
  #@(local.set $discriminator (call $generic-property (local.get $function) ,(clos-library-reference :discriminating-function)))
  #@(if (ref.is_null (local.get $discriminator)) (then (return (i32.const 1))))
  #@(if (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator))) (then (return (i32.const 0))))
  #@(ref.eq (struct.get $generic-discriminator $owner (ref.cast (ref $generic-discriminator) (local.get $discriminator)))
    (local.get $function)))

(define-runtime-function $find-instance-slot (($class :nullable-object) ($name :nullable-object)) (:nullable-object)
  (:locals ($slots :nullable-object) ($slot :nullable-object) ($found :nullable-object)
           ($slot-name :nullable-object) ($direct-names i32) ($direct-slots i32))
  ;; The standard MOP getters expose class metadata. Keep custom methods and
  ;; discriminators observable, including changes made during finalization.
  #@(if (call $boundp ,(clos-library-reference '%standard-class-slots-method))
    (then (local.set $direct-slots (call $standard-property-generic-p
      (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'class-slots))))
      (call $symbol-value ,(clos-library-reference '%standard-class-slots-method))))))
  #@(if (i32.and (local.get $direct-slots) (i32.eqz (ref.is_null (call $car (call $property-cell
        (struct.get $obj $b (ref.cast (ref $obj) (local.get $class))) ,(clos-library-reference :finalized))))))
    (then (local.set $slots (call $car (call $property-cell
      (struct.get $obj $b (ref.cast (ref $obj) (local.get $class))) ,(clos-library-reference :slots)))))
    (else (local.set $slots (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'class-slots)))
      (call $cons (local.get $class) (ref.null eq))))))
  #@(if (call $boundp ,(clos-library-reference '%standard-slot-definition-name-method))
    (then (local.set $direct-names (call $standard-property-generic-p
      (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'slot-definition-name))))
      (call $symbol-value ,(clos-library-reference '%standard-slot-definition-name-method))))))
  (wat-list-loop ($slots)
    #@(local.set $slot (call $car (local.get $slots)))
    #@(local.set $slot-name (if (result (ref null eq)) (local.get $direct-names)
      (then (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $slot)))
        ,(clos-library-reference :name))))
      (else (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'slot-definition-name)))
        (call $cons (local.get $slot) (ref.null eq))))))
    #@(if (ref.eq (local.get $slot-name) (local.get $name))
      (then (local.set $found (local.get $slot)))))
  #@(call $clear-values)
  #@(local.get $found))

(define-runtime-function $instance-slot-operation
    (($mode i32) ($object :nullable-object) ($name :nullable-object) ($value :nullable-object)) (:nullable-object)
  (:locals ($storage :nullable-object) ($class :nullable-object) ($slot :nullable-object)
           ($function :nullable-object) ($args :nullable-object) ($result :nullable-object))
  #@(local.set $storage (local.get $object))
  #@(if (if (result i32) (i32.eq (call $tag (local.get $object)) (i32.const 8))
      (then (i32.eq (call $value-n (local.get $object)) (i32.const 2147483645))) (else (i32.const 0)))
    (then (local.set $storage (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))
  #@(drop (call $ensure-instance-layout (local.get $storage)))
  #@(local.set $class (call $dispatch-class (local.get $object)))
  #@(local.set $slot (call $find-instance-slot (local.get $class) (local.get $name)))
  #@(if (i32.eq (local.get $mode) (i32.const 3))
    (then (return (call $bool (i32.eqz (ref.is_null (local.get $slot)))))))
  #@(if (ref.is_null (local.get $slot))
    (then
      (local.set $function (call $symbol (i32.const ,(runtime-symbol-id 'slot-missing))))
      (local.set $args (call $cons (local.get $class) (call $cons (local.get $object)
        (call $cons (local.get $name) (call $cons
          (call $symbol (if (result i32) (i32.eqz (local.get $mode))
            (then (i32.const ,(runtime-symbol-id 'slot-value)))
            (else (if (result i32) (i32.eq (local.get $mode) (i32.const 1))
              (then (i32.const ,(runtime-symbol-id 'setf)))
              (else (select (i32.const ,(runtime-symbol-id 'slot-boundp)) (i32.const ,(runtime-symbol-id 'slot-makunbound))
                (i32.eq (local.get $mode) (i32.const 2))))))))
          (if (result (ref null eq)) (i32.eq (local.get $mode) (i32.const 1))
            (then (call $cons (local.get $value) (ref.null eq))) (else (ref.null eq)))))))))
    (else
      (local.set $function (call $symbol (if (result i32) (i32.le_u (local.get $mode) (i32.const 1))
        (then (i32.const ,(runtime-symbol-id 'slot-value-using-class)))
        (else (select (i32.const ,(runtime-symbol-id 'slot-boundp-using-class)) (i32.const ,(runtime-symbol-id 'slot-makunbound-using-class))
          (i32.eq (local.get $mode) (i32.const 2)))))))
      (local.set $args (call $cons (local.get $class) (call $cons (local.get $object) (call $cons (local.get $slot) (ref.null eq)))))
      (if (i32.eq (local.get $mode) (i32.const 1))
        (then (local.set $function (call $fdefinition (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'setf)))
          (call $cons (local.get $function) (ref.null eq)))))
          (local.set $args (call $cons (local.get $value) (local.get $args)))))))
  #@(local.set $result (call $apply (local.get $function) (local.get $args)))
  #@(call $clear-values)
  #@(if (i32.eq (local.get $mode) (i32.const 1)) (then (return (local.get $value))))
  #@(if (i32.eq (local.get $mode) (i32.const 2)) (then (return (call $bool (i32.eqz (ref.is_null (local.get $result)))))))
  #@(if (i32.eq (local.get $mode) (i32.const 4)) (then (return (local.get $object))))
  #@(local.get $result))

(define-runtime-function $property-effective
    (($function :nullable-object) ($form :nullable-object) ($gf :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($method :nullable-object) ($body :nullable-object) ($scan :nullable-object) ($next :nullable-object)
           ($specialized :nullable-object) ($probe i32))
  #@(local.set $probe (ref.is_null (local.get $function)))
  #@(if (local.get $probe)
    (then (local.set $function (call $closure (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(block $ordinary
    (br_if $ordinary (i32.ne (call $operator (local.get $form)) (i32.const ,(runtime-symbol-id 'call-method))))
    (br_if $ordinary (i32.eqz (ref.is_null (call $cdr (call $cdr (call $cdr (local.get $form)))))))
    (br_if $ordinary (ref.is_null (call $cdr (call $cdr (local.get $form)))))
    (local.set $next (call $car (call $cdr (call $cdr (local.get $form)))))
    (local.set $method (call $car (call $cdr (local.get $form))))
    (br_if $ordinary (i32.ne (call $tag (local.get $method)) (i32.const 23)))
    (local.set $body (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method))) ,(clos-library-reference :function))))
    (if (i32.and (ref.is_null (local.get $next)) (ref.test (ref $direct-function) (local.get $body)))
      (then
        (if (i32.or
              (i32.eq (struct.get $direct-function $mode (ref.cast (ref $direct-function) (local.get $body))) (i32.const 11))
              (i32.eq (struct.get $direct-function $mode (ref.cast (ref $direct-function) (local.get $body))) (i32.const 12)))
          (then
            (if (call $generic-cache-permitted-p (local.get $gf))
              (then (local.set $specialized (call $specialize-accessor (local.get $function) (local.get $gf)
                (ref.cast (ref $direct-function) (local.get $body)) (local.get $args)))
                (if (i32.eqz (ref.is_null (local.get $specialized))) (then (return (local.get $specialized))))))
            (return (call $direct-function (local.get $function)
            (struct.get $direct-function $owner (ref.cast (ref $direct-function) (local.get $body)))
            (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $body)))
            (i32.add (struct.get $direct-function $mode (ref.cast (ref $direct-function) (local.get $body))) (i32.const 2))))))
        (if (i32.eq (struct.get $direct-function $mode
                       (ref.cast (ref $direct-function) (local.get $body))) (i32.const 3))
        (then (return (call $direct-function (local.get $function) (ref.null eq)
          (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $body))) (i32.const 4)))))))
    ;; A CALL-METHOD plan can invoke the primary directly. Its next-method
    ;; continuation is shared by the plan; keyword validation stays general.
    (local.set $scan (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)))
    (block $checked (loop $parameter (br_if $checked (ref.is_null (local.get $scan)))
      (br_if $ordinary (i32.or
        (ref.eq (call $car (local.get $scan)) (call $symbol (i32.const ,(runtime-symbol-id '&key))))
        (ref.eq (call $car (local.get $scan)) (call $symbol (i32.const ,(runtime-symbol-id '&rest))))))
      (local.set $scan (call $cdr (local.get $scan))) (br $parameter)))
    (if (i32.eqz (ref.is_null (local.get $next)))
      (then
        (local.set $scan (local.get $next))
        (block $checked (loop $method
          (br_if $checked (ref.is_null (local.get $scan)))
          ;; Synthesized MAKE-METHOD forms retain their general expansion.
          (br_if $ordinary (i32.ne (call $tag (call $car (local.get $scan))) (i32.const 23)))
          (local.set $scan (call $cdr (local.get $scan))) (br $method)))
        (return (call $apply ,(clos-library-reference '%primary-effective)
          (call $cons (local.get $function) (call $cons (local.get $gf)
            (call $cons (local.get $method) (call $cons (local.get $next) (ref.null eq)))))))))
    (return (call $direct-function (local.get $function) (local.get $gf) (local.get $method) (i32.const 8))))
  #@(if (result (ref null eq)) (local.get $probe) (then (ref.null eq)) (else (local.get $function))))

(define-runtime-function $ensure-instance-layout (($object :nullable-object)) (:nullable-object)
  (:locals ($class :nullable-object) ($function :nullable-object) ($methods :nullable-object)
           ($discriminator :nullable-object) ($layout :nullable-object) ($expected :nullable-object))
  #@(if (i32.ne (call $tag (local.get $object)) (i32.const 23)) (then (return (local.get $object))))
  ;; The default CLASS-SLOTS method reads finalized class metadata. Comparing
  ;; the existing layout directly avoids an interpreted method call per slot
  ;; access. Custom methods and discriminators keep their ordinary path.
  #@(block $general
    (br_if $general (i32.eqz (call $boundp ,(clos-library-reference '%standard-class-slots-method))))
    (local.set $class (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))
    (br_if $general (ref.is_null (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
      ,(clos-library-reference :finalized)))))
    (local.set $function (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'class-slots)))))
    (br_if $general (i32.ne (call $tag (local.get $function)) (i32.const 8)))
    (br_if $general (i32.ne (call $value-n (local.get $function)) (i32.const 2147483645)))
    (br_if $general (i32.eqz (call $generic-cache-permitted-p (local.get $function))))
    (local.set $methods (call $generic-property (local.get $function) ,(clos-library-reference :methods)))
    (br_if $general (i32.eqz (ref.eq (call $car (local.get $methods))
      (call $symbol-value ,(clos-library-reference '%standard-class-slots-method)))))
    (br_if $general (i32.eqz (ref.is_null (call $cdr (local.get $methods)))))
    (local.set $discriminator (call $generic-property (local.get $function) ,(clos-library-reference :discriminating-function)))
    (if (i32.eqz (ref.is_null (local.get $discriminator)))
      (then
        (br_if $general (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator))))
        (br_if $general (i32.eqz (ref.eq
          (struct.get $generic-discriminator $owner (ref.cast (ref $generic-discriminator) (local.get $discriminator)))
          (local.get $function))))))
    (local.set $layout (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $object))) ,(clos-library-reference '%layout)))
    (br_if $general (ref.is_null (local.get $layout)))
    (local.set $expected (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class))) ,(clos-library-reference :slots))))
    (if (ref.is_null (local.get $expected))
      (then (local.set $expected (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $class)))
        ,(clos-library-reference :empty-layout))))))
    (br_if $general (i32.eqz (ref.eq (call $car (local.get $layout)) (local.get $expected))))
    (return (local.get $object)))
  #@(call $apply ,(clos-library-reference '%ensure-layout-general) (call $cons (local.get $object) (ref.null eq))))

(define-runtime-function $restore-saved-slots
    (($object :nullable-object) ($names :nullable-object) ($values :nullable-object) ($bound :nullable-object)) (:nullable-object)
  (:locals ($index i32) ($present i32))
  #@(block $done (loop $next
    (br_if $done (ref.is_null (local.get $names)))
    (local.set $present (i32.eqz (ref.is_null
      (call $logbitp-native (call $int (local.get $index)) (local.get $bound)))))
    ;; Retain the ordinary slot protocol, including class slots and MOP hooks.
    (drop (call $instance-slot-operation (select (i32.const 1) (i32.const 4) (local.get $present))
      (local.get $object) (call $car (local.get $names))
      (if (result (ref null eq)) (local.get $present)
        (then (call $car (local.get $values))) (else (ref.null eq)))))
    (local.set $names (call $cdr (local.get $names)))
    (local.set $values (call $cdr (local.get $values)))
    (local.set $index (i32.add (local.get $index) (i32.const 1)))
    (call $thread-checkpoint) (br $next)))
  #@(call $clear-values)
  #@(local.get $object))

(define-runtime-section generic-functions-runtime-wat ()
  #@(type $method-context (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $args (ref null eq)) (field $next (ref null eq)))))
  ($dispatch-eql-key) ($generic-discriminator) ($generic-property) ($invoke-method) ($method-context-element) ($dispatch-class) ($generic-key-match) ($generic-cache-permitted-p) ($generic-apply)
  ($standard-property-generic-p) ($find-instance-slot) ($instance-slot-operation) ($restore-saved-slots) ($property-effective) ($ensure-instance-layout))
