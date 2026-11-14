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
  '(:discriminating-function :cache :lambda-list :methods :cache-protocol :name :eql-dispatch-profile :cache-token))

;; The owned metadata vector also holds two recent flattened dispatch rows
;; and the call-history identity under which they were populated. Borrowing
;; RAW-SLOTS relinquishes ownership and disables these derived caches too.
(defparameter *generic-property-cache-size* (+ 4 (length *generic-property-cache-keys*)))
(defparameter *generic-recent-index* (1+ (length *generic-property-cache-keys*)))

(define-runtime-function $generic-property (($gf :nullable-object) ($key :nullable-object)) (:nullable-object)
  (:locals ($info (ref $obj)) ($owned :nullable-object) ($cache (ref null $objects))
           ($cell :nullable-object) ($index i32))
  #@(local.set $info (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf)))))
  #@(block $ordinary
    (local.set $owned (struct.get $obj $c (local.get $info)))
    (if (ref.eq (local.get $owned) (call $symbol (i32.const 1)))
      (then
        (local.set $cache (array.new_default $objects (i32.const ,*generic-property-cache-size*)))
        (array.set $objects (local.get $cache) (i32.const 0) (local.get $owned))
        (struct.set $obj $c (local.get $info) (local.get $cache)))
      (else
        (br_if $ordinary (i32.eqz (ref.test (ref $objects) (local.get $owned))))
        (local.set $cache (ref.cast (ref $objects) (local.get $owned)))
        (br_if $ordinary (i32.ne (array.len (local.get $cache)) (i32.const ,*generic-property-cache-size*)))
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
  #@(local.set $function (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method)))
    ,(clos-library-reference :function))))
  ;; Method bodies receive their lexical next-method context as an
  ;; explicit operand. No dynamic binding or restoration is needed here.
  #@(if (ref.test (ref $direct-function) (local.get $function))
    (then (if (i32.eq (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) (local.get $function))) (i32.const 18))
      (then (return_call $apply
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $function)))
        (call $cons (local.get $context) (local.get $args)))))))
  #@(local.set $saved (global.get $dynamic-variables))
  #@(global.set $dynamic-variables (call $bind (local.get $saved)
    ,(clos-library-reference '%next-context) (local.get $context)))
  #@(block $returned
    (block $thrown
      (try_table (catch $transfer $thrown)
        (if (if (result i32) (ref.test (ref $direct-function) (local.get $function))
              (then (i32.eq (struct.get $direct-function $mode
                (ref.cast (ref $direct-function) (local.get $function))) (i32.const 17)))
              (else (i32.const 0)))
          (then (local.set $result (call $apply
            (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $function)))
            (local.get $args))))
          (else (local.set $result (call $apply (local.get $function)
            (call $native-two-arguments (local.get $function) (local.get $args) (local.get $rest))))))
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

;; A cache probe cannot create classes or call user code. An uninitialized
;; condition class supplies no proof and must use ordinary dispatch.
(define-runtime-function $dispatch-class-known (($object :nullable-object)) (:nullable-object)
  (:locals ($tag i32) ($id i32) ($length i32) ($high i32))
  #@(local.set $tag (call $tag (local.get $object)))
  #@(if (i32.eq (local.get $tag) (i32.const 47))
    (then (return (call $named-class ,(clos-library-reference 'miricl-js:value)))))
  #@(if (i32.eq (local.get $tag) (i32.const 23))
    (then (return (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))
  #@(if (i32.eq (local.get $tag) (i32.const 17))
    (then (return (call $named-class (struct.get $obj $a (ref.cast (ref $obj) (local.get $object)))))))
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

;; CLASS-OF and ordinary dispatch retain lazy condition class creation.
(define-runtime-function $dispatch-class (($object :nullable-object)) (:nullable-object)
  (:locals ($class :nullable-object))
  #@(local.set $class (call $dispatch-class-known (local.get $object)))
  #@(if (i32.and (ref.is_null (local.get $class)) (i32.eq (call $tag (local.get $object)) (i32.const 17)))
    (then (return (call $apply (call $symbol (i32.const ,(runtime-symbol-id 'find-class)))
      (call $cons (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))) (ref.null eq))))))
  #@(local.get $class))

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

(define-runtime-function $generic-cache-row (($entry :nullable-object)) ((ref $objects))
  (:locals ($row (ref $objects)) ($scan :nullable-object))
  #@(local.set $row (array.new_default $objects (i32.const 6)))
  #@(local.set $scan (local.get $entry))
  (runtime-wat-forms
    (loop for index below 6 collect
      `(block
         (array.set $objects (local.get $row) (i32.const ,index) (call $car (local.get $scan)))
         (local.set $scan (call $cdr (local.get $scan))))))
  #@(local.get $row))

(define-runtime-function $make-generic-call-plan (($args :nullable-object)) (:nullable-object)
  (:locals ($gf :nullable-object) ($function :nullable-object) ($guards :nullable-object)
           ($discriminator :nullable-object) ($data (ref $objects)) ($copy (ref $objects))
           ($classes (ref $objects)) ($position i32)
           ($version :nullable-object) ($token :nullable-object) ($slot-token :nullable-object)
           ($mask i32) ($count i32))
  #@(call $arity (local.get $args) (i32.const 4) (i32.const 7))
  #@(local.set $gf (call $car (local.get $args)))
  (wat-fail-when #@(i32.ne (call $tag (local.get $gf)) (i32.const 8)) :malformed-form)
  (wat-fail-when #@(i32.ne (call $value-n (local.get $gf)) (i32.const 2147483645)) :malformed-form)
  #@(local.set $discriminator (call $generic-property (local.get $gf) ,(clos-library-reference :discriminating-function)))
  ;; Probe and construction both reject trace wrappers and custom callable
  ;; discriminators. A guarded call must preserve those on every invocation.
  #@(if (i32.eqz (ref.is_null (local.get $discriminator)))
    (then
      (if (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator)))
        (then (return (ref.null eq))))
      (if (i32.eqz (ref.eq (struct.get $generic-discriminator $owner
            (ref.cast (ref $generic-discriminator) (local.get $discriminator))) (local.get $gf)))
        (then (return (ref.null eq))))))
  #@(local.set $args (call $cdr (local.get $args)))
  #@(local.set $function (call $car (local.get $args)))
  #@(if (ref.is_null (local.get $function))
    (then (call $arity (local.get $args) (i32.const 3) (i32.const 3)) (return (local.get $gf))))
  #@(call $arity (local.get $args) (i32.const 6) (i32.const 6))
  (wat-fail-when #@(i32.ne (call $tag (local.get $function)) (i32.const 8)) :malformed-form)
  #@(local.set $args (call $cdr (local.get $args)))
  #@(local.set $guards (call $car (local.get $args)))
  (wat-fail-when #@(i32.ne (call $tag (local.get $guards)) (i32.const 34)) :malformed-form)
  #@(local.set $data (struct.get $vector $data (ref.cast (ref $vector) (local.get $guards))))
  #@(local.set $count (array.len (local.get $data)))
  (wat-fail-when #@(i32.gt_u (local.get $count) (i32.const 4)) :malformed-form)
  #@(local.set $mask (call $natural-count (call $car (call $cdr (local.get $args)))))
  (wat-fail-when #@(i32.ge_u (local.get $mask) (i32.shl (i32.const 1) (local.get $count))) :malformed-form)
  #@(local.set $copy (array.new_default $objects (local.get $count)))
  #@(array.copy $objects $objects (local.get $copy) (i32.const 0)
    (local.get $data) (i32.const 0) (local.get $count))
  #@(local.set $args (call $cdr (call $cdr (local.get $args))))
  #@(local.set $version (call $car (local.get $args)))
  #@(local.set $token (call $car (call $cdr (local.get $args))))
  #@(local.set $slot-token (call $car (call $cdr (call $cdr (local.get $args)))))
  #@(local.set $classes (array.new_default $objects (local.get $count)))
  #@(block $classes-done (loop $next
    (br_if $classes-done (i32.eq (local.get $position) (local.get $count)))
    (if (i32.and (local.get $mask) (i32.shl (i32.const 1) (local.get $position)))
      (then (array.set $objects (local.get $classes) (local.get $position)
        (call $dispatch-class (array.get $objects (local.get $copy) (local.get $position))))))
    (local.set $position (i32.add (local.get $position) (i32.const 1))) (br $next)))
  ;; A cooperative checkpoint can occur between the Lisp proof and this
  ;; constructor. Capture the proof's epoch, never relabel an old result.
  #@(if (i32.eqz (call $eql (local.get $version)
        (call $symbol-value ,(clos-library-reference '%dispatch-version)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (local.get $token)
        (call $generic-property (local.get $gf) ,(clos-library-reference :cache-token)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (local.get $slot-token) (global.get $slot-access-token))) (then (return (ref.null eq))))
  #@(call $clear-values)
  #@(struct.new $generic-call-plan
    (call $allocate-object-tag (i32.const 54)) (local.get $mask) (f64.const 0) (ref.null $digits)
    (local.get $gf) (local.get $function) (ref.null eq) (local.get $copy) (local.get $classes)
    (local.get $version) (local.get $token) (local.get $discriminator) (local.get $slot-token)))

(define-runtime-function $generic-call-plan-function
    (($plan :nullable-object) ($gf :nullable-object) ($args :nullable-object) ($known i32)) (:nullable-object)
  (:locals ($saved (ref null $generic-call-plan)) ($guards (ref $objects))
           ($guard :nullable-object) ($position i32) ($mask i32))
  #@(if (i32.eqz (ref.test (ref $generic-call-plan) (local.get $plan))) (then (return (ref.null eq))))
  #@(local.set $saved (ref.cast (ref $generic-call-plan) (local.get $plan)))
  #@(if (i32.eqz (ref.eq (local.get $gf) (struct.get $obj $a (local.get $saved)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (call $eql (struct.get $generic-call-plan $version (local.get $saved))
        (call $symbol-value ,(clos-library-reference '%dispatch-version)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (struct.get $generic-call-plan $token (local.get $saved))
        (call $generic-property (local.get $gf) ,(clos-library-reference :cache-token)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (struct.get $generic-call-plan $discriminator (local.get $saved))
        (call $generic-property (local.get $gf) ,(clos-library-reference :discriminating-function)))) (then (return (ref.null eq))))
  #@(if (i32.eqz (ref.eq (struct.get $generic-call-plan $slot-token (local.get $saved))
        (global.get $slot-access-token))) (then (return (ref.null eq))))
  #@(local.set $guards (struct.get $generic-call-plan $guards (local.get $saved)))
  #@(local.set $mask (struct.get $obj $n (local.get $saved)))
  ;; Only identity checks for EQL positions can be discharged by the compiler.
  ;; Keep class and invalidation checks even for an identical mutable object.
  #@(if (i32.or
      (i32.ge_u (local.get $known) (i32.shl (i32.const 1) (array.len (local.get $guards))))
      (i32.and (local.get $known) (i32.xor (local.get $mask) (i32.const -1))))
    (then (return (ref.null eq))))
  #@(loop $next
    (if (i32.eq (local.get $position) (array.len (local.get $guards)))
      (then
        (if (i32.eqz (ref.is_null (local.get $args))) (then (return (ref.null eq))))
        ;; DISPATCH-CLASS may lazily initialize a condition class.
        (if (i32.eqz (call $eql (struct.get $generic-call-plan $version (local.get $saved))
              (call $symbol-value ,(clos-library-reference '%dispatch-version)))) (then (return (ref.null eq))))
        (if (i32.eqz (ref.eq (struct.get $generic-call-plan $token (local.get $saved))
              (call $generic-property (local.get $gf) ,(clos-library-reference :cache-token)))) (then (return (ref.null eq))))
        (if (i32.eqz (ref.eq (struct.get $generic-call-plan $discriminator (local.get $saved))
              (call $generic-property (local.get $gf) ,(clos-library-reference :discriminating-function)))) (then (return (ref.null eq))))
        (if (i32.eqz (ref.eq (struct.get $generic-call-plan $slot-token (local.get $saved))
              (global.get $slot-access-token))) (then (return (ref.null eq))))
        (return (struct.get $obj $b (local.get $saved)))))
    (if (ref.is_null (local.get $args)) (then (return (ref.null eq))))
    (local.set $guard (array.get $objects (local.get $guards) (local.get $position)))
    (if (i32.and (local.get $mask) (i32.shl (i32.const 1) (local.get $position)))
      (then
        (if (i32.eqz (i32.and (local.get $known) (i32.shl (i32.const 1) (local.get $position))))
          (then
            (if (i32.eqz (call $eql (local.get $guard) (call $car (local.get $args))))
              (then (return (ref.null eq))))))
        ;; EQL objects can CHANGE-CLASS without changing their identity.
        (if (i32.eqz (ref.eq
              (array.get $objects (struct.get $generic-call-plan $eql-classes (local.get $saved)) (local.get $position))
              (call $dispatch-class (call $car (local.get $args))))) (then (return (ref.null eq)))))
      (else
        (if (i32.eqz (ref.is_null (local.get $guard)))
          (then
            (if (i32.eqz (ref.eq (local.get $guard) (call $dispatch-class (call $car (local.get $args)))))
              (then (return (ref.null eq))))))))
    (local.set $position (i32.add (local.get $position) (i32.const 1)))
    (local.set $args (call $cdr (local.get $args))) (br $next))
  #@(ref.null eq))

;; Fixed-argument callers can enter a recent executable plan without first
;; allocating an argument container or invoking the universal dispatcher.
(define-runtime-function $generic-fixed-arity-p (($params :nullable-object) ($count i32)) (i32)
  (:locals ($index i32) ($name :nullable-object) ($id i32))
  #@(block $done (loop $next
    (br_if $done (i32.eq (local.get $index) (local.get $count)))
    (if (i32.ne (call $tag (local.get $params)) (i32.const 7)) (then (return (i32.const 0))))
    (local.set $name (call $car (local.get $params)))
    (if (i32.eqz (call $symbolp (local.get $name))) (then (return (i32.const 0))))
    (local.set $id (call $id (local.get $name)))
    ,@(loop for marker in '(&optional &rest &body &key &allow-other-keys &aux &whole &environment) collect
      `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id marker))) (then (return (i32.const 0)))))
    (local.set $params (call $cdr (local.get $params)))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $next)))
  #@(ref.is_null (local.get $params)))

(define-runtime-function $generic-recent-call
    (($gf :nullable-object) ($count i32) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object)) (:nullable-object i32)
  (:locals ($info (ref $obj)) ($metadata :nullable-object) ($recent (ref $objects))
           ($row-object :nullable-object) ($row (ref $objects)) ($phase i32) ($mode i32) ($target-mode i32) ($position i32)
           ($version :nullable-object) ($history :nullable-object) ($discriminator :nullable-object)
           ($proof :nullable-object) ($key :nullable-object) ($profile :nullable-object)
           ($object :nullable-object) ($class :nullable-object) ($selected :nullable-object)
           ($method :nullable-object) ($rest :nullable-object) ($next :nullable-object)
           ($function :nullable-object) ($body (ref null $native-function)) ($args :nullable-object)
           ($context :nullable-object) ($result :nullable-object) ($index i32))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (if (global.get $thread-active) (then (call $thread-checkpoint)))
    (br_if $miss (i32.gt_u (local.get $count) (i32.const 3)))
    (local.set $info (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf)))))
    (local.set $metadata (struct.get $obj $c (local.get $info)))
    (br_if $miss (i32.eqz (ref.test (ref $objects) (local.get $metadata))))
    (local.set $recent (ref.cast (ref $objects) (local.get $metadata)))
    (br_if $miss (i32.ne (array.len (local.get $recent)) (i32.const ,*generic-property-cache-size*)))
    (br_if $miss (i32.eqz (ref.eq (array.get $objects (local.get $recent) (i32.const 0)) (call $symbol (i32.const 1)))))
    (local.set $history (call $car (array.get $objects (local.get $recent)
      (i32.const ,(1+ (position :cache *generic-property-cache-keys*))))))
    (br_if $miss (ref.is_null (local.get $history)))
    (br_if $miss (i32.eqz (ref.eq (local.get $history)
      (array.get $objects (local.get $recent) (i32.const ,(+ *generic-recent-index* 2))))))
    (local.set $discriminator (call $car (array.get $objects (local.get $recent)
      (i32.const ,(1+ (position :discriminating-function *generic-property-cache-keys*))))))
    (if (i32.eqz (ref.is_null (local.get $discriminator)))
      (then
        (br_if $miss (i32.eqz (ref.test (ref $generic-discriminator) (local.get $discriminator))))
        (br_if $miss (i32.eqz (ref.eq (struct.get $generic-discriminator $owner
          (ref.cast (ref $generic-discriminator) (local.get $discriminator))) (local.get $gf))))))
    (local.set $version (call $symbol-value ,(clos-library-reference '%dispatch-version)))
    ;; Do not run a MOP eligibility callback on a speculative fast-path miss.
    (if (ref.is_null (call $symbol-value ,(clos-library-reference '%cache-permitted)))
      (then
        (local.set $proof (call $car (array.get $objects (local.get $recent)
          (i32.const ,(1+ (position :cache-protocol *generic-property-cache-keys*))))))
        (br_if $miss (ref.is_null (local.get $proof)))
        (br_if $miss (i32.eqz (call $eql (call $car (local.get $proof)) (local.get $version))))
        (br_if $miss (ref.is_null (call $cdr (local.get $proof))))))
    (loop $rows
      (br_if $miss (i32.ge_u (local.get $phase) (i32.const 2)))
      (block $row-miss
        (local.set $row-object (array.get $objects (local.get $recent)
          (i32.add (i32.const ,*generic-recent-index*) (local.get $phase))))
        (br_if $row-miss (i32.eqz (ref.test (ref $objects) (local.get $row-object))))
        (local.set $row (ref.cast (ref $objects) (local.get $row-object)))
        (br_if $row-miss (i32.ne (array.len (local.get $row)) (i32.const 6)))
        (br_if $row-miss (i32.eqz (call $eql (array.get $objects (local.get $row) (i32.const 0)) (local.get $version))))
        (local.set $mode (call $natural-count (array.get $objects (local.get $row) (i32.const 3))))
        (br_if $row-miss (i32.eqz (local.get $mode)))
        (br_if $row-miss (i32.ne (i32.and (local.get $mode) (i32.const 7)) (local.get $count)))
        (if (i32.and (local.get $mode) (i32.const 4096))
          (then (br_if $row-miss (i32.eqz (ref.eq (global.get $slot-access-token)
            (array.get $objects (local.get $row) (i32.const 5)))))))
        ;; Reject unsupported plans before classifying operands on a miss.
        (local.set $selected (array.get $objects (local.get $row) (i32.const 2)))
        (br_if $row-miss (i32.eqz (ref.test (ref $direct-function) (local.get $selected))))
        (local.set $target-mode (struct.get $direct-function $mode (ref.cast (ref $direct-function) (local.get $selected))))
        ;; Standard metadata readers have no owner; other selected plans must
        ;; belong to this generic. Its owned cache and epoch guard both cases.
        (br_if $row-miss (i32.eqz (ref.eq (struct.get $direct-function $owner
          (ref.cast (ref $direct-function) (local.get $selected)))
          (if (result (ref null eq)) (i32.eq (local.get $target-mode) (i32.const 4))
            (then (ref.null eq)) (else (local.get $gf))))))
        (block $target-ready
          (br_if $target-ready (i32.and (i32.eq (local.get $target-mode) (i32.const 4)) (i32.eq (local.get $count) (i32.const 1))))
          (br_if $target-ready (i32.and (i32.eq (local.get $target-mode) (i32.const 15)) (i32.eq (local.get $count) (i32.const 1))))
          (br_if $target-ready (i32.and (i32.eq (local.get $target-mode) (i32.const 16)) (i32.eq (local.get $count) (i32.const 2))))
          (br_if $row-miss (i32.and (i32.ne (local.get $target-mode) (i32.const 8)) (i32.ne (local.get $target-mode) (i32.const 9))))
          (local.set $rest (ref.null eq)) (local.set $next (ref.null eq))
          (local.set $method (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $selected))))
          (if (i32.eq (local.get $target-mode) (i32.const 9))
            (then
              (local.set $rest (call $car (call $cdr (local.get $method))))
              (local.set $next (call $car (call $cdr (call $cdr (local.get $method)))))
              (local.set $method (call $car (local.get $method)))))
          (local.set $function (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method)))
            ,(clos-library-reference :function))))
          (br_if $row-miss (i32.eqz (ref.test (ref $direct-function) (local.get $function))))
          (br_if $row-miss (i32.ne (struct.get $direct-function $mode
            (ref.cast (ref $direct-function) (local.get $function))) (i32.const 18)))
          (local.set $function (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $function))))
          (br_if $row-miss (i32.eqz (ref.test (ref $native-function) (local.get $function))))
          (local.set $body (ref.cast (ref $native-function) (local.get $function)))
          (br_if $row-miss (f64.ne (struct.get $obj $f (ref.as_non_null (local.get $body)))
            (f64.convert_i32_u (i32.add (local.get $count) (i32.const 3)))))
          (br_if $row-miss (ref.is_null (struct.get $native-function $direct (ref.as_non_null (local.get $body)))))
        )
        (local.set $key (array.get $objects (local.get $row) (i32.const 1)))
        (local.set $position (local.get $count))
        (block $matched (loop $keys
          (br_if $matched (i32.eqz (local.get $position)))
          (local.set $position (i32.sub (local.get $position) (i32.const 1)))
          (if (i32.and (local.get $mode) (i32.shl (i32.const 16) (local.get $position)))
            (then
              (local.set $object (if (result (ref null eq)) (i32.eq (local.get $position) (i32.const 2))
                (then (local.get $c)) (else (if (result (ref null eq)) (local.get $position)
                  (then (local.get $b)) (else (local.get $a))))))
              (local.set $class (call $dispatch-class-known (local.get $object)))
              (br_if $row-miss (ref.is_null (local.get $class)))
              (if (i32.and (local.get $mode) (i32.shl (i32.const 256) (local.get $position)))
                (then
                  (local.set $profile (call $generic-property (local.get $gf) ,(clos-library-reference :eql-dispatch-profile)))
                  (local.set $index (local.get $position))
                  (block $at-profile (loop $profile
                    (br_if $at-profile (i32.eqz (local.get $index)))
                    (local.set $profile (call $cdr (local.get $profile)))
                    (local.set $index (i32.sub (local.get $index) (i32.const 1))) (br $profile)))
                  (br_if $row-miss (i32.eqz (ref.eq (call $car (local.get $key))
                    (call $dispatch-eql-key (local.get $object) (call $car (local.get $profile))))))
                  (local.set $key (call $cdr (local.get $key)))))
              (br_if $row-miss (i32.eqz (ref.eq (call $car (local.get $key)) (local.get $class))))
              (local.set $key (call $cdr (local.get $key)))))
          (br $keys)))
        (br_if $row-miss (i32.eqz (ref.is_null (local.get $key))))
        ;; Classification and EQL comparison above neither call Lisp nor poll.
        ;; The cache identities checked before them therefore remain valid.
        (br_if $miss (i32.eqz (call $generic-fixed-arity-p
          (call $car (array.get $objects (local.get $recent)
            (i32.const ,(1+ (position :lambda-list *generic-property-cache-keys*))))) (local.get $count))))
        (if (i32.eq (local.get $target-mode) (i32.const 4))
          (then
            (local.set $object (local.get $a))
            (if (i32.eq (call $tag (local.get $object)) (i32.const 8))
              (then (local.set $object (struct.get $obj $a (ref.cast (ref $obj) (local.get $object))))))
            (br_if $row-miss (i32.ne (call $tag (local.get $object)) (i32.const 23)))
            ;; Read the canonical cell on every hit, retaining borrowed-slot
            ;; mutation visibility without another argument list or APPLY.
            (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $object)))
              (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $selected)))))
            (i32.const 1) (return)))
        (if (i32.and (i32.eq (local.get $target-mode) (i32.const 15)) (i32.eq (local.get $count) (i32.const 1)))
          (then (call $specialized-slot-operation (ref.cast (ref $direct-function) (local.get $selected))
            (local.get $a) (local.get $a)) (i32.const 1) (return)))
        (if (i32.and (i32.eq (local.get $target-mode) (i32.const 16)) (i32.eq (local.get $count) (i32.const 2)))
          (then (call $specialized-slot-operation (ref.cast (ref $direct-function) (local.get $selected))
            (local.get $b) (local.get $a)) (i32.const 1) (return)))
        (local.set $index (struct.get $obj $n
          (ref.as_non_null (struct.get $native-function $direct (ref.as_non_null (local.get $body))))))
        (if (i32.or (i32.eqz (local.get $index))
              (i32.and (i32.sub (local.get $index) (i32.const 1)) (i32.const 1)))
          (then
        (local.set $position (local.get $count))
        (block $arguments (loop $argument
          (br_if $arguments (i32.eqz (local.get $position)))
          (local.set $position (i32.sub (local.get $position) (i32.const 1)))
          (local.set $args (call $cons (if (result (ref null eq)) (i32.eq (local.get $position) (i32.const 2))
            (then (local.get $c)) (else (if (result (ref null eq)) (local.get $position)
              (then (local.get $b)) (else (local.get $a))))) (local.get $args))) (br $argument)))
        (local.set $context (struct.new $method-context (call $allocate-object-tag (i32.const 53))
          (i32.const 0) (f64.const 0) (ref.null $digits) (local.get $gf) (local.get $method)
          (local.get $rest) (local.get $args) (local.get $next)))
          ))
        (call $native-direct-apply (ref.as_non_null (local.get $body)) (local.get $context)
          (local.get $a) (local.get $b) (local.get $c)) (i32.const 1) (return))
      (local.set $phase (i32.add (local.get $phase) (i32.const 1))) (br $rows)))
  #@(ref.null eq) #@(i32.const 0))

(define-runtime-function $generic-apply (($gf :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($function :nullable-object) ($cache :nullable-object) ($entry :nullable-object)
           ($version :nullable-object) ($key :nullable-object) ($mode-object :nullable-object)
           ($ca :nullable-object) ($cb :nullable-object) ($cc :nullable-object) ($cd :nullable-object)
           ($mode i32) ($computed i32) ($phase i32)
           ($metadata :nullable-object) ($recent (ref null $objects)) ($row (ref null $objects))
           ($selected :nullable-object) ($epoch :nullable-object) ($slot-token :nullable-object))
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
          (local.set $metadata (struct.get $obj $c
            (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $gf))))))
          (block $no-recent
            (br_if $no-recent (i32.eqz (ref.test (ref $objects) (local.get $metadata))))
            (br_if $no-recent (i32.ne (array.len (ref.cast (ref $objects) (local.get $metadata)))
              (i32.const ,*generic-property-cache-size*)))
            (br_if $no-recent (i32.eqz (ref.eq
              (array.get $objects (ref.cast (ref $objects) (local.get $metadata)) (i32.const 0))
              (call $symbol (i32.const 1)))))
            (local.set $recent (ref.cast (ref $objects) (local.get $metadata)))
            (if (i32.eqz (ref.eq (local.get $cache)
                  (array.get $objects (local.get $recent) (i32.const ,(+ *generic-recent-index* 2)))))
              (then
                (array.set $objects (local.get $recent) (i32.const ,*generic-recent-index*) (ref.null eq))
                (array.set $objects (local.get $recent) (i32.const ,(1+ *generic-recent-index*)) (ref.null eq))
                (array.set $objects (local.get $recent) (i32.const ,(+ *generic-recent-index* 2)) (local.get $cache)))))
          (block $miss (loop $next
            (local.set $row (ref.null $objects))
            (if (i32.lt_u (local.get $phase) (i32.const 2))
              (then
                (if (i32.eqz (ref.is_null (local.get $recent)))
                  (then
                    (local.set $metadata (array.get $objects (local.get $recent)
                      (i32.add (i32.const ,*generic-recent-index*) (local.get $phase))))
                    (if (ref.test (ref $objects) (local.get $metadata))
                      (then
                        (local.set $row (ref.cast (ref $objects) (local.get $metadata)))
                        (if (i32.ne (array.len (local.get $row)) (i32.const 6))
                          (then (local.set $row (ref.null $objects))))))))
                (if (ref.is_null (local.get $row))
                  (then (local.set $phase (i32.add (local.get $phase) (i32.const 1))) (br $next)))
                (local.set $epoch (array.get $objects (local.get $row) (i32.const 0)))
                (local.set $key (array.get $objects (local.get $row) (i32.const 1)))
                (local.set $selected (array.get $objects (local.get $row) (i32.const 2)))
                (local.set $mode-object (array.get $objects (local.get $row) (i32.const 3)))
                (local.set $slot-token (array.get $objects (local.get $row) (i32.const 5))))
              (else
                (br_if $miss (ref.is_null (local.get $cache)))
                (local.set $entry (call $car (local.get $cache)))
                (local.set $epoch (call $car (local.get $entry)))
                (local.set $key (call $car (call $cdr (local.get $entry))))
                (local.set $selected (call $car (call $cdr (call $cdr (local.get $entry)))))
                (local.set $mode-object (call $car (call $cdr (call $cdr (call $cdr (local.get $entry))))))
                (local.set $slot-token
                  (call $car (call $cdr (call $cdr (call $cdr (call $cdr (call $cdr (local.get $entry))))))))))
            (if (call $eql (local.get $epoch) (local.get $version))
              (then
                (local.set $mode (call $natural-count (local.get $mode-object)))
                (br_if $miss (i32.eqz (local.get $mode)))
                (if (i32.and (local.get $mode) (i32.const 4096))
                  (then (br_if $miss (i32.eqz (ref.eq (global.get $slot-access-token)
                    (local.get $slot-token))))))
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
                (if (call $generic-key-match (local.get $key) (local.get $gf) (local.get $args)
                      (local.get $ca) (local.get $cb) (local.get $cc) (local.get $cd) (local.get $mode))
                  (then
                    ;; Lazy condition-class creation can reenter Lisp. Never
                    ;; use an entry whose assumptions changed during lookup.
                    (br_if $miss (i32.eqz (call $eql (local.get $version)
                      (call $symbol-value ,(clos-library-reference '%dispatch-version)))))
                    (drop (call $check-generic-arity
                      (call $generic-property (local.get $gf) ,(clos-library-reference :lambda-list)) (local.get $args)))
                    (if (i32.eqz (ref.is_null (local.get $recent)))
                      (then
                        (if (i32.ge_u (local.get $phase) (i32.const 2))
                          (then (local.set $row (call $generic-cache-row (local.get $entry)))))
                        (if (i32.ne (local.get $phase) (i32.const 0))
                          (then
                            (array.set $objects (local.get $recent) (i32.const ,(1+ *generic-recent-index*))
                              (array.get $objects (local.get $recent) (i32.const ,*generic-recent-index*)))
                            (array.set $objects (local.get $recent) (i32.const ,*generic-recent-index*) (local.get $row))))))
                    (return (call $apply (local.get $selected)
                      (call $cons (local.get $args) (ref.null eq))))))))
            (if (i32.lt_u (local.get $phase) (i32.const 2))
              (then (local.set $phase (i32.add (local.get $phase) (i32.const 1))))
              (else (local.set $cache (call $cdr (local.get $cache)))))
            (br $next)))))
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
  #@(local.set $methods (call $generic-property (local.get $function) ,(clos-library-reference :method-combination-name)))
  #@(if (i32.eqz (ref.eq (call $car (local.get $methods)) ,(clos-library-reference 'standard)))
    (then (return (i32.const 0))))
  #@(if (i32.eqz (ref.is_null (call $cdr (local.get $methods)))) (then (return (i32.const 0))))
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

(define-runtime-function $instance-slot-operation-general
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
        (call $check-apply-list (local.get $next))
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
  (:locals ($class :nullable-object) ($function :nullable-object)
           ($layout :nullable-object) ($expected :nullable-object))
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
    (br_if $general (i32.eqz (call $standard-property-generic-p (local.get $function)
      (call $symbol-value ,(clos-library-reference '%standard-class-slots-method)))))
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
  #@(type $generic-call-plan (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $guards (ref $objects)) (field $eql-classes (ref $objects))
    (field $version (ref null eq)) (field $token (ref null eq))
    (field $discriminator (ref null eq)) (field $slot-token (ref null eq)))))
  #@(type $method-context (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $args (ref null eq)) (field $next (ref null eq)))))
  ($dispatch-eql-key) ($generic-discriminator) ($generic-property) ($invoke-method) ($method-context-element) ($dispatch-class-known) ($dispatch-class) ($generic-key-match) ($generic-fixed-arity-p) ($generic-recent-call) ($generic-cache-permitted-p) ($generic-cache-row) ($generic-apply)
  ($make-generic-call-plan) ($generic-call-plan-function)
  ($standard-property-generic-p) ($find-instance-slot) ($instance-slot-operation-general) ($restore-saved-slots) ($property-effective) ($ensure-instance-layout))
