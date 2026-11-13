;;;; Stable boxed entry convention for independently compiled Wasm modules.
;;;; Extern references cross the module boundary; values remain WasmGC objects.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun native-object (form) `(ref.cast (ref null eq) (any.convert_extern ,form)))
(defun native-external (form) `(extern.convert_any ,form))
(defun native-context (form) `(ref.cast (ref $native-context) (any.convert_extern ,form)))

(define-runtime-function $native-install
    (($code :nullable-object) ($literals :nullable-object) ($name :nullable-object)) (:nullable-object)
  (wat-fail-when #@(ref.is_null (table.get $native-services (i32.const 0))) :unsupported)
  ;; EMIT-NATIVE-MODULE returns its literal vector to callers. Own the pool's
  ;; container before installing code whose proofs depend on constant entries;
  ;; preserve the identity and mutability of the literal objects themselves.
  #@(if (i32.eqz (ref.is_null (local.get $literals)))
    (then
      (call $check-type (local.get $literals) (call $symbol (i32.const ,(runtime-symbol-id 'simple-vector))))
      (local.set $literals (call $vector-slice (local.get $literals) (i32.const 0)
        (call $array-sequence-length (local.get $literals))))))
  #@(ref.cast (ref null eq) (any.convert_extern
    (call_indirect $native-services (type $native-service)
      (extern.convert_any (local.get $code)) (extern.convert_any (local.get $literals))
      (extern.convert_any (local.get $name)) (i32.const 0)))))

(define-runtime-function $compiler-image-function
    (($index i32) ($literals :nullable-object) ($name :nullable-object)) (:nullable-object)
  (if *native-compiler-image*
      #@(ref.cast (ref null eq) (any.convert_extern
        (call $native-function (table.get $compiler-entries (local.get $index))
          (extern.convert_any (local.get $literals)) (extern.convert_any (local.get $name)) (i32.const 0))))
      (runtime-wat-sequence (wat-fail :unsupported) #@(unreachable))))

(define-runtime-function $native-function
    (($entry funcref) ($literals externref) ($name externref) ($size i32)) (externref)
  (:export-raw "native-function")
  #@(return_call $native-function-with-code (local.get $entry) (local.get $literals)
    (local.get $name) (local.get $size) (ref.null extern)))

(define-runtime-function $native-function-with-code
    (($entry funcref) ($literals externref) ($name externref) ($size i32) ($code externref)) (externref)
  (:export-raw "core-native-function")
  #@(extern.convert_any (struct.new $native-function (call $allocate-object-tag (i32.const 8))
    (i32.const 0) (f64.const 1) (ref.cast (ref null $digits) (any.convert_extern (local.get $code)))
    ,(native-object '(local.get $literals)) (ref.null eq) ,(native-object '(local.get $name))
    (ref.cast (ref $native-entry) (local.get $entry)) (local.get $size) (ref.null $native-direct-code))))

;; F=1 denotes a general native entry; F=2+arity permits packed fixed calls.
(define-runtime-function $native-close-with-arity
    (($entry funcref) ($parent externref) ($captures externref) ($arity i32)) (externref)
  (:locals ($values :nullable-object) ($vector :nullable-object) ($count i32))
  #@(local.set $values ,(native-object '(local.get $captures)))
  #@(local.set $count (call $length (local.get $values)))
  ;; The final private slot caches children that reuse this function's bindings.
  #@(local.set $vector (call $vector-from-list (local.get $values) (i32.add (local.get $count) (i32.const 1))))
  #@(array.set $objects (call $vector-data (local.get $vector)) (local.get $count) (ref.null eq))
  #@(extern.convert_any (struct.new $native-function (call $allocate-object-tag (i32.const 8))
    (i32.const 0) (f64.convert_i32_s (i32.add (local.get $arity) (i32.const 2)))
    (struct.get $obj $digits (ref.cast (ref $obj) ,(native-object '(local.get $parent))))
    (struct.get $obj $a (ref.cast (ref $obj) ,(native-object '(local.get $parent))))
    (local.get $vector) (ref.null eq)
    (ref.cast (ref $native-entry) (local.get $entry))
    (struct.get $native-function $code-size (ref.cast (ref $native-function) ,(native-object '(local.get $parent)))) (ref.null $native-direct-code))))

(define-runtime-function $native-close
    (($entry funcref) ($parent externref) ($captures externref)) (externref)
  (:export-raw "native-close")
  #@(return_call $native-close-with-arity (local.get $entry) (local.get $parent) (local.get $captures) (i32.const -1)))

(define-runtime-function $native-close-fixed
    (($entry funcref) ($parent externref) ($captures externref) ($arity i32)) (externref)
  (:export-raw "native-close-fixed")
  #@(return_call $native-close-with-arity (local.get $entry) (local.get $parent) (local.get $captures) (local.get $arity)))

(define-runtime-function $native-close-cached
    (($entry funcref) ($parent externref) ($captured externref) ($arity i32) ($slot i32)) (externref)
  (:export-raw "native-close-cached")
  (:locals ($function :nullable-object) ($captures :nullable-object) ($data (ref $objects))
           ($index i32) ($cached :nullable-object) ($value :nullable-object))
  #@(local.set $function ,(native-object '(local.get $parent)))
  #@(local.set $captures (struct.get $obj $b (ref.cast (ref $obj) (local.get $function))))
  #@(if (ref.is_null (local.get $captures))
    (then (local.set $captures (call $vector-from-list (ref.null eq) (i32.const 1)))
      (struct.set $obj $b (ref.cast (ref $obj) (local.get $function)) (local.get $captures))))
  #@(local.set $data (call $vector-data (local.get $captures)))
  #@(local.set $index (i32.sub (array.len (local.get $data)) (i32.const 1)))
  #@(local.set $cached (array.get $objects (local.get $data) (local.get $index)))
  #@(block $missing (loop $next (br_if $missing (ref.is_null (local.get $cached)))
    (if (i32.eq (call $value-n (local.get $cached)) (local.get $slot))
      (then (return (extern.convert_any (struct.get $obj $a (ref.cast (ref $obj) (local.get $cached)))))))
    (local.set $cached (struct.get $obj $b (ref.cast (ref $obj) (local.get $cached)))) (br $next)))
  #@(local.set $value ,(native-object '(call $native-close-with-arity
    (local.get $entry) (local.get $parent) (local.get $captured) (local.get $arity))))
  #@(array.set $objects (local.get $data) (local.get $index)
    (call $node (i32.const 9) (local.get $slot) (local.get $value)
      (array.get $objects (local.get $data) (local.get $index)) (ref.null eq)))
  #@(extern.convert_any (local.get $value)))

(define-runtime-function $native-close-static
    (($entry funcref) ($parent externref) ($arity i32) ($slot i32)) (externref)
  (:export-raw "native-close-static")
  #@(return_call $native-close-cached (local.get $entry) (local.get $parent) (ref.null extern)
    (local.get $arity) (local.get $slot)))

(define-runtime-function $native-with-arity (($function :nullable-object) ($arity i32)) (:nullable-object)
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $arity) (i32.const 4))
    (i32.eqz (ref.test (ref $native-function) (local.get $function)))) :expected-designator)
  #@(struct.new $native-function (call $allocate-object-tag (i32.const 8)) (i32.const 0)
    (f64.convert_i32_u (i32.add (local.get $arity) (i32.const 2)))
    (struct.get $obj $digits (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $a (ref.cast (ref $obj) (local.get $function))) (struct.get $obj $b (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $c (ref.cast (ref $obj) (local.get $function)))
    (struct.get $native-function $entry (ref.cast (ref $native-function) (local.get $function)))
    (struct.get $native-function $code-size (ref.cast (ref $native-function) (local.get $function)))
    (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $function)))))

(define-runtime-function $native-code-size (($function :nullable-object)) (:nullable-object)
  (:locals ($size i32))
  ;; Methods expose a MOP adapter while normal invocation uses
  ;; the retained body. Report that body's native code just as for an ordinary
  ;; compiled closure, including when the adapter itself has no module bytes.
  #@(if (ref.test (ref $direct-function) (local.get $function))
    (then (if (i32.ge_u (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) (local.get $function))) (i32.const 17))
      (then (return_call $native-code-size
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $function))))))))
  #@(if (ref.test (ref $native-function) (local.get $function))
    (then
      (local.set $size (struct.get $native-function $code-size (ref.cast (ref $native-function) (local.get $function))))
      (if (local.get $size) (then (return (call $int (local.get $size)))))))
  #@(ref.null eq))

(define-runtime-function $native-debug-wrap
    (($function :nullable-object) ($code :object) ($id i32)) (:object)
  ;; Only debug-enabled modules allocate this subtype. Ordinary functions keep
  ;; their existing layout, allocation path and calling convention.
  #@(struct.new $native-debug-function (call $allocate-object-tag (i32.const 8)) (i32.const 0)
    (struct.get $obj $f (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $digits (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $a (ref.cast (ref $obj) (local.get $function))) (struct.get $obj $b (ref.cast (ref $obj) (local.get $function)))
    (struct.get $obj $c (ref.cast (ref $obj) (local.get $function)))
    (struct.get $native-function $entry (ref.cast (ref $native-function) (local.get $function)))
    (struct.get $native-function $code-size (ref.cast (ref $native-function) (local.get $function)))
    (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $function)))
    (local.get $code) (local.get $id)))

(define-runtime-function $native-debug-attach
    (($function :nullable-object) ($descriptor :nullable-object) ($id i32)) (:object)
  (:locals ($slots (ref $objects)) ($code :nullable-object) ($data :digits)
           ($start i32) ($length i32) ($i i32) ($word i32))
  #@(local.set $slots (call $vector-data (local.get $descriptor)))
  #@(local.set $code (array.get $objects (local.get $slots) (i32.const 0)))
  #@(if (i32.ne (call $tag (local.get $code)) (i32.const 9))
    (then
      ;; Pack the byte vector once per module. The descriptor is shared by
      ;; entry points and closures; it never refers back to a function.
      (call $native-code-bytes (extern.convert_any (local.get $code)))
      (local.set $length) (local.set $start)
      (local.set $data (array.new_default $digits
        (i32.shr_u (i32.add (local.get $length) (i32.const 3)) (i32.const 2))))
      (block $done (loop $pack (br_if $done (i32.eq (local.get $i) (local.get $length)))
        (local.set $word (i32.shr_u (local.get $i) (i32.const 2)))
        (array.set $digits (local.get $data) (local.get $word)
          (i32.or (array.get $digits (local.get $data) (local.get $word))
            (i32.shl (i32.load8_u (i32.add (local.get $start) (local.get $i)))
              (i32.shl (i32.and (local.get $i) (i32.const 3)) (i32.const 3)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $pack)))
      (local.set $code ,(wat-new-object #@(i32.const 9) :n #@(local.get $length)
        :digits #@(local.get $data) :a #@(array.get $objects (local.get $slots) (i32.const 1))))
      (array.set $objects (local.get $slots) (i32.const 0) (local.get $code))))
  #@(call $native-debug-wrap (local.get $function) (ref.as_non_null (local.get $code)) (local.get $id)))

(define-runtime-function $native-debug-close
    (($function externref) ($parent externref) ($id i32)) (externref)
  (:export-raw "native-debug-close")
  (:locals ($owner :nullable-object))
  #@(local.set $owner ,(native-object '(local.get $parent)))
  #@(if (ref.test (ref $native-debug-function) (local.get $owner))
    (then (return (extern.convert_any (call $native-debug-wrap ,(native-object '(local.get $function))
      (struct.get $native-debug-function $debug-code (ref.cast (ref $native-debug-function) (local.get $owner)))
      (local.get $id))))))
  #@(local.get $function))

(define-runtime-function $native-debug-info (($function :nullable-object)) (:nullable-object)
  (:locals ($code :object) ($index :nullable-object) ($bytes (ref $objects))
           ($data :digits) ($length i32) ($i i32))
  #@(if (i32.eqz (ref.test (ref $native-debug-function) (local.get $function))) (then (return (ref.null eq))))
  #@(local.set $code (struct.get $native-debug-function $debug-code
    (ref.cast (ref $native-debug-function) (local.get $function))))
  #@(local.set $index (array.get $objects (call $vector-data (struct.get $obj $a (ref.cast (ref $obj) (local.get $code))))
    (struct.get $native-debug-function $debug-id (ref.cast (ref $native-debug-function) (local.get $function)))))
  #@(if (ref.is_null (local.get $index)) (then (return (ref.null eq))))
  ;; Expand only while DISASSEMBLE runs, for the existing Lisp byte reader.
  #@(local.set $length (call $value-n (local.get $code)))
  #@(local.set $data (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $code)))))
  #@(local.set $bytes (array.new_default $objects (local.get $length)))
  #@(block $done (loop $unpack (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (array.set $objects (local.get $bytes) (local.get $i)
      (call $int (i32.and (i32.shr_u
        (array.get $digits (local.get $data) (i32.shr_u (local.get $i) (i32.const 2)))
        (i32.shl (i32.and (local.get $i) (i32.const 3)) (i32.const 3))) (i32.const 255))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $unpack)))
  #@(call $cons (call $vector-new (local.get $bytes)) (local.get $index)))

(define-runtime-function $native-capture (($function externref) ($index i32)) (externref)
  (:export-raw "native-capture")
  #@(extern.convert_any (array.get $objects
    (call $vector-data (struct.get $obj $b (ref.cast (ref $obj) ,(native-object '(local.get $function))))) (local.get $index))))

(define-runtime-function $native-code-storage (($code externref)) (externref)
  (:export-raw "native-code-storage")
  (:locals ($value :nullable-object))
  #@(local.set $value ,(native-object '(local.get $code)))
  #@(extern.convert_any (if (result (ref null eq)) (i32.eq (call $tag (local.get $value)) (i32.const 7))
    (then (call $car (local.get $value))) (else (local.get $value)))))

(define-runtime-function $native-code-entry (($code externref)) (i32)
  (:export-raw "native-code-entry")
  (:locals ($value :nullable-object))
  #@(local.set $value ,(native-object '(local.get $code)))
  #@(if (result i32) (i32.eq (call $tag (local.get $value)) (i32.const 7))
    (then (call $natural-count (call $cdr (local.get $value)))) (else (i32.const -1))))

(define-runtime-function $native-code-bytes (($code externref)) (i32 i32)
  (:export-raw "native-code-bytes")
  (:locals ($value :nullable-object) ($size i32) ($i i32) ($text :digits) ($byte i32))
  #@(local.set $value ,(native-object '(local.get $code)))
  #@(if (i32.eq (call $tag (local.get $value)) (i32.const 7))
    (then (local.set $value (call $car (local.get $value)))))
  #@(if (i32.eq (call $tag (local.get $value)) (i32.const 10))
    (then (local.set $text (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $value)))))
      (local.set $size (array.len (local.get $text)))
      (call $reserve (local.get $size))
      (block $done (loop $copy
        (br_if $done (i32.eq (local.get $i) (local.get $size)))
        (local.set $byte (array.get $digits (local.get $text) (local.get $i)))
        ,(wat-fail-when #@(i32.gt_u (local.get $byte) (i32.const 255)) :malformed-form)
        (i32.store8 (i32.add (global.get $cursor) (local.get $i)) (local.get $byte))
        (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
      (return (global.get $cursor) (local.get $size))))
  #@(local.set $size (call $array-sequence-length (local.get $value)))
  #@(call $reserve (local.get $size))
  #@(block $done (loop $copy (br_if $done (i32.eq (local.get $i) (local.get $size)))
    (i32.store8 (i32.add (global.get $cursor) (local.get $i))
      (call $natural-count (call $sequence-value (local.get $value) (local.get $i))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
  #@(global.get $cursor) #@(local.get $size))

(define-runtime-function $native-literal (($function externref) ($index i32)) (externref)
  (:export-raw "native-literal")
  #@(extern.convert_any (array.get $objects
    (call $vector-data (struct.get $obj $a (ref.cast (ref $obj) ,(native-object '(local.get $function))))) (local.get $index))))

(define-runtime-function $native-two-arguments
    (($function :nullable-object) ($a :nullable-object) ($b :nullable-object)) (:nullable-object)
  #@(if (ref.test (ref $native-function) (local.get $function))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) (local.get $function))) (f64.const 4))
      (then (return (array.new_fixed $objects 2 (local.get $a) (local.get $b)))))))
  #@(call $cons (local.get $a) (call $cons (local.get $b) (ref.null eq))))

(define-runtime-function $native-packed-argument (($args (ref $objects)) ($index i32)) (:nullable-object)
  ;; Private argument arrays have exactly their live arguments, without the
  ;; Lisp object header and unused slots of a fixed-size packet.
  #@(if (i32.ge_u (local.get $index) (array.len (local.get $args))) (then (return (ref.null eq))))
  #@(array.get $objects (local.get $args) (local.get $index)))

(define-runtime-function $native-list-tail (($value :nullable-object) ($index i32)) (:nullable-object)
  ;; Parameter indices are compiler constants; traverse without boxing them.
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(block $done (loop $next
    (br_if $done (i32.or (i32.eqz (local.get $index)) (ref.is_null (local.get $value))))
    (local.set $value (call $cdr (local.get $value)))
    (local.set $index (i32.sub (local.get $index) (i32.const 1))) (br $next)))
  #@(local.get $value))

(define-runtime-function $native-rest-argument (($args externref) ($index i32)) (externref)
  (:export-raw "native-rest-argument")
  (:locals ($value :nullable-object) ($count i32) ($result :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (local.set $count (array.len (ref.cast (ref $objects) (local.get $value))))
      (block $done (loop $next (br_if $done (i32.le_s (local.get $count) (local.get $index)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1)))
        (local.set $result (call $cons (call $native-packed-argument
          (ref.cast (ref $objects) (local.get $value)) (local.get $count)) (local.get $result)))
        (br $next)))
      (return (extern.convert_any (local.get $result)))))
  #@(extern.convert_any (call $native-list-tail (local.get $value) (local.get $index))))

(define-runtime-function $native-argument (($args externref) ($index i32)) (externref)
  (:export-raw "native-argument")
  (:locals ($value :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then (return (extern.convert_any (call $native-packed-argument
      (ref.cast (ref $objects) (local.get $value)) (local.get $index))))))
  #@(extern.convert_any (call $car (call $native-list-tail (local.get $value) (local.get $index)))))

(define-runtime-function $native-check-arguments (($args externref) ($count i32) ($maximum i32)) ()
  (:export-raw "native-check-arguments")
  (:locals ($scan :nullable-object) ($i i32))
  #@(local.set $scan ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $scan))
    (then (local.set $i (array.len (ref.cast (ref $objects) (local.get $scan))))
      ,(wat-fail-when #@(i32.or (i32.lt_u (local.get $i) (local.get $count))
        (i32.and (i32.ge_s (local.get $maximum) (i32.const 0))
          (i32.gt_u (local.get $i) (local.get $maximum)))) :wrong-arity)
      (return)))
  ;; REST accepts dotted tails in the private destructuring ABI as before.
  #@(if (i32.lt_s (local.get $maximum) (i32.const 0))
    (then
      (block $done (loop $required (br_if $done (i32.eq (local.get $i) (local.get $count)))
        ,(wat-fail-when #@(i32.ne (call $tag (local.get $scan)) (i32.const 7)) :wrong-arity)
        (local.set $scan (call $cdr (local.get $scan)))
        (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $required)))
      (return)))
  #@(call $arity (local.get $scan) (local.get $count) (local.get $maximum)))

(define-runtime-function $native-check-apply-arguments (($args externref) ($count i32)) ()
  (:export-raw "native-check-apply-arguments")
  #@(drop (call $check-apply-arguments ,(native-object '(local.get $args)) (local.get $count))))

(define-runtime-function $native-check-apply-list (($args externref)) ()
  (:export-raw "native-check-apply-list")
  #@(call $check-apply-list ,(native-object '(local.get $args))))

(define-runtime-function $native-environment () (externref)
  (:export-raw "native-environment")
  (:locals ($context (ref null $native-context)))
  #@(local.set $context (global.get $native-environment-cache))
  #@(if (i32.eqz (ref.is_null (local.get $context)))
    (then (if (i32.and
      (ref.eq (struct.get $native-context $variables (local.get $context)) (global.get $dynamic-variables))
      (i32.and (ref.eq (struct.get $native-context $packages (local.get $context)) (global.get $dynamic-package))
        (ref.eq (struct.get $native-context $targets (local.get $context)) (global.get $targets))))
      (then (return (extern.convert_any (local.get $context)))))))
  #@(local.set $context (struct.new $native-context (global.get $dynamic-variables)
    (global.get $dynamic-package) (global.get $targets)))
  #@(global.set $native-environment-cache (local.get $context))
  #@(extern.convert_any (local.get $context)))

(define-runtime-function $native-restore-environment (($environment externref)) ()
  (:export-raw "native-restore-environment")
  (:locals ($context (ref $native-context)))
  #@(local.set $context ,(native-context '(local.get $environment)))
  #@(global.set $dynamic-variables (struct.get $native-context $variables (local.get $context)))
  #@(global.set $dynamic-package (struct.get $native-context $packages (local.get $context)))
  #@(global.set $targets (struct.get $native-context $targets (local.get $context)))
  #@(global.set $native-environment-cache (local.get $context)))

(define-runtime-function $native-bind
    (($environment externref) ($symbol externref) ($value externref)) (externref)
  (:export-raw "native-bind")
  (:locals ($context (ref $native-context)) ($name :nullable-object) ($binding :nullable-object)
           ($variables :nullable-object) ($packages :nullable-object))
  #@(call $native-restore-environment (local.get $environment))
  #@(local.set $context ,(native-context '(local.get $environment)))
  #@(local.set $name ,(native-object '(local.get $symbol)))
  #@(local.set $variables (call $bind (struct.get $native-context $variables (local.get $context))
    (local.get $name) ,(native-object '(local.get $value))))
  #@(local.set $binding (call $car (local.get $variables)))
  ;; A lexical SPECIAL declaration need not proclaim the name globally.
  #@(struct.set $obj $c (ref.cast (ref $obj) (local.get $binding)) (call $symbol (i32.const 1)))
  #@(local.set $packages (struct.get $native-context $packages (local.get $context)))
  #@(if (i32.eq (call $id (local.get $name)) (i32.const ,(runtime-symbol-id '*package*)))
    (then (drop (call $require-package ,(native-object '(local.get $value))))
      (local.set $packages (call $cons (struct.get $obj $a (ref.cast (ref $obj) (local.get $binding))) (local.get $packages)))))
  #@(local.set $context (struct.new $native-context (local.get $variables) (local.get $packages)
    (struct.get $native-context $targets (local.get $context))))
  #@(global.set $dynamic-variables (local.get $variables))
  #@(global.set $dynamic-package (local.get $packages))
  #@(global.set $native-environment-cache (local.get $context))
  #@(extern.convert_any (local.get $context)))

(define-runtime-function $native-global-read (($environment externref) ($name externref)) (externref)
  (:export-raw "native-global-read")
  #@(extern.convert_any (call $symbol-value ,(native-object '(local.get $name)))))

(define-runtime-function $native-global-write
    (($environment externref) ($name externref) ($value externref)) (externref)
  (:export-raw "native-global-write")
  #@(extern.convert_any (call $set-symbol-value ,(native-object '(local.get $name)) ,(native-object '(local.get $value)))))

(define-runtime-function $native-function-reference (($name externref)) (externref)
  (:export-raw "native-function-reference")
  (:locals ($symbol :nullable-object) ($binding :nullable-object) ($value :nullable-object) ($id i32))
  ;; Compiled global calls overwhelmingly name symbols. Read the current
  ;; binding directly; retain FDEFINITION for SETF names and undefined calls.
  #@(local.set $symbol ,(native-object '(local.get $name)))
  #@(if (call $symbolp (local.get $symbol))
    (then
      ;; SYMBOLP has established the representation. Read a non-NIL symbol's
      ;; ID directly instead of testing its tag again through ID/VALUE-N.
      (if (i32.eqz (ref.is_null (local.get $symbol)))
        (then (local.set $id (struct.get $obj $n (ref.cast (ref $obj) (local.get $symbol))))))
      (local.set $binding (call $symbol-index-get (global.get $function-bindings) (local.get $id)))
      (if (i32.eqz (ref.is_null (local.get $binding)))
        (then
          (local.set $value (struct.get $obj $a (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $binding))))))
          (if (i32.eqz (ref.is_null (local.get $value)))
            (then (return (extern.convert_any (local.get $value)))))))))
  #@(extern.convert_any (call $fdefinition ,(native-object '(local.get $name)))))

(define-runtime-function $native-cons (($a externref) ($b externref)) (externref)
  (:export-raw "native-cons")
  #@(extern.convert_any (call $cons ,(native-object '(local.get $a)) ,(native-object '(local.get $b)))))

(define-runtime-function $native-cell (($value externref)) (externref)
  (:export-raw "native-cell")
  #@(extern.convert_any (call $cons ,(native-object '(local.get $value)) (ref.null eq))))

(define-runtime-function $native-primary (($values externref)) (externref)
  (:export-raw "native-primary")
  #@(extern.convert_any (call $car ,(native-object '(local.get $values)))))

(define-runtime-function $native-cell-read (($cell externref)) (externref)
  (:export-raw "native-cell-read")
  #@(extern.convert_any (call $car ,(native-object '(local.get $cell)))))

(define-runtime-function $native-cell-write (($cell externref) ($value externref)) (externref)
  (:export-raw "native-cell-write")
  #@(struct.set $obj $a (ref.cast (ref $obj) ,(native-object '(local.get $cell))) ,(native-object '(local.get $value)))
  #@(local.get $value))

(define-runtime-function $native-block-enter (($environment externref)) (externref)
  (:export-raw "native-block-enter")
  (:locals ($context (ref $native-context)) ($target :nullable-object))
  #@(call $native-restore-environment (local.get $environment))
  #@(local.set $context ,(native-context '(local.get $environment)))
  ;; A regular Lisp object can travel through the existing closure capture
  ;; vector. Keep the environment fields here without boxing all native contexts.
  #@(local.set $target (call $node (i32.const 21) (i32.const 0)
    (struct.get $native-context $variables (local.get $context))
    (struct.get $native-context $packages (local.get $context)) (ref.null eq)))
  #@(global.set $targets (call $cons (local.get $target) (global.get $targets)))
  #@(struct.set $obj $c (ref.cast (ref $obj) (local.get $target)) (global.get $targets))
  #@(extern.convert_any (local.get $target)))

(define-runtime-function $native-block-environment (($token externref)) (externref)
  (:export-raw "native-block-environment")
  (:locals ($target :nullable-object))
  #@(local.set $target ,(native-object '(local.get $token)))
  #@(extern.convert_any (struct.new $native-context (struct.get $obj $a (ref.cast (ref $obj) (local.get $target)))
    (struct.get $obj $b (ref.cast (ref $obj) (local.get $target))) (struct.get $obj $c (ref.cast (ref $obj) (local.get $target))))))

(define-runtime-function $native-block-return
    (($token externref) ($values externref)) ()
  (:export-raw "native-block-return")
  (:locals ($target :nullable-object))
  #@(local.set $target ,(native-object '(local.get $token)))
  (wat-fail-when #@(i32.eqz (call $member-eq (local.get $target) (global.get $targets))) :control-error)
  #@(call $transfer-to (local.get $target) ,(native-object '(local.get $values))))

(define-runtime-function $native-block-match (($token externref)) (i32)
  (:export-raw "native-block-match")
  #@(if (ref.is_null (local.get $token)) (then (return (i32.const 0))))
  #@(ref.eq (global.get $transfer-target) ,(native-object '(local.get $token))))

(define-runtime-function $native-block-values () (externref)
  (:export-raw "native-block-values")
  #@(global.set $error (i32.const 0))
  #@(call $clear-values)
  #@(extern.convert_any (global.get $transfer-values)))

;; Shared dynamic-control paths keep exception plumbing out of every emitted
;; function. Bodies and handlers are native closures, never interpreter forms.
(define-runtime-function $native-control
    (($environment externref) ($mode i32) ($thunk externref) ($data externref) ($no-error externref)) (externref)
  (:export-raw "native-control")
  (:locals ($handlers :nullable-object) ($catches :nullable-object) ($target :nullable-object)
           ($scan :nullable-object) ($cluster :nullable-object) ($type :nullable-object)
           ($function :nullable-object) ($values :nullable-object) ($saved-target :nullable-object)
           ($saved-function :nullable-object) ($condition :nullable-object) ($error-code i32) ($symbol i32)
           ($restarts :nullable-object) ($associations :nullable-object) ($new :nullable-object) ($options :nullable-object)
           ($binding :nullable-object))
  #@(call $native-restore-environment (local.get $environment))
  #@(local.set $handlers (global.get $handlers)) #@(local.set $catches (global.get $catches))
  #@(local.set $restarts (global.get $restarts)) #@(local.set $associations (global.get $restart-associations))
  #@(if (i32.eqz (local.get $mode))
    (then
      (local.set $target (call $node (i32.const 21) (i32.const 0) ,(native-object '(local.get $data))
        (ref.null eq) (ref.null eq)))
      (global.set $catches (call $cons (local.get $target) (local.get $catches))))
    (else (if (i32.le_u (local.get $mode) (i32.const 2))
      (then
        (if (i32.eq (local.get $mode) (i32.const 2))
          (then (local.set $target (call $node (i32.const 21) (i32.const 0)
            (ref.null eq) (ref.null eq) (ref.null eq)))))
        (local.set $scan ,(native-object '(local.get $data)))
        (block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
          (local.set $type (call $car (local.get $scan))) (local.set $scan (call $cdr (local.get $scan)))
          (local.set $function (call $car (local.get $scan))) (local.set $scan (call $cdr (local.get $scan)))
          (local.set $cluster (call $cons (call $node (i32.const 20)
            (i32.eq (local.get $mode) (i32.const 2)) (local.get $type) (local.get $function) (local.get $target))
            (local.get $cluster))) (br $next)))
        (global.set $handlers (call $cons (call $reverse-onto (local.get $cluster) (ref.null eq) (i32.const 0))
          (local.get $handlers)))))))
  #@(if (i32.and (i32.ge_u (local.get $mode) (i32.const 4)) (i32.le_u (local.get $mode) (i32.const 5)))
    (then
      (if (i32.eq (local.get $mode) (i32.const 5))
        (then (local.set $target (call $node (i32.const 21) (i32.const 0)
          (ref.null eq) (ref.null eq) (ref.null eq)))))
      (local.set $scan ,(native-object '(local.get $data)))
      (block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
        (local.set $type (call $car (local.get $scan))) (local.set $scan (call $cdr (local.get $scan)))
        (local.set $function (call $car (local.get $scan))) (local.set $scan (call $cdr (local.get $scan)))
        (local.set $options (call $car (local.get $scan))) (local.set $scan (call $cdr (local.get $scan)))
        (call $check-apply-list (local.get $options))
        (local.set $new (call $cons (call $new-restart (local.get $type) (local.get $function)
          (local.get $target) (local.get $options) (i32.eq (local.get $mode) (i32.const 5))) (local.get $new)))
        (br $next)))
      (local.set $new (call $reverse-onto (local.get $new) (ref.null eq) (i32.const 0)))
      (global.set $restarts (call $prepend-list (local.get $new) (local.get $restarts)))))
  #@(if (i32.eq (local.get $mode) (i32.const 6))
    (then
      (local.set $scan ,(native-object '(local.get $data)))
      (local.set $condition (call $car (local.get $scan)))
      ,(wat-fail-when #@(i32.ne (call $tag (local.get $condition)) (i32.const 17)) :expected-designator)
      (local.set $new (call $car (call $cdr (local.get $scan))))
      (call $check-apply-list (local.get $new))
      (local.set $scan (local.get $new))
      (block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
        ,(wat-fail-when #@(i32.ne (call $tag (call $car (local.get $scan))) (i32.const 19)) :expected-designator)
        (local.set $scan (call $cdr (local.get $scan))) (br $next)))
      (global.set $restart-associations (call $cons (call $cons (local.get $condition) (local.get $new))
        (local.get $associations)))))
  #@(if (i32.eq (local.get $mode) (i32.const 7))
    (then
      (local.set $scan (call $car ,(native-object '(local.get $data))))
      (local.set $options (call $car (call $cdr ,(native-object '(local.get $data)))))
      (drop (call $proper-sequence-length (local.get $scan)))
      (drop (call $proper-sequence-length (local.get $options)))
      (call $require-list (local.get $scan)) (call $require-list (local.get $options))
      (block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
        (local.set $type (call $car (local.get $scan))) (call $check-variable (local.get $type))
        (local.set $binding (call $node (i32.const 9) (call $id (local.get $type))
          (call $node (i32.const 11) (ref.is_null (local.get $options))
            (call $car (local.get $options)) (ref.null eq) (ref.null eq))
          (call $symbol (i32.const 1)) (call $symbol (i32.const 1))))
        (global.set $dynamic-variables (call $cons (local.get $binding) (global.get $dynamic-variables)))
        (if (i32.eq (call $id (local.get $type)) (i32.const ,(runtime-symbol-id '*package*)))
          (then
            (if (i32.eqz (ref.is_null (local.get $options)))
              (then (drop (call $require-package (call $car (local.get $options))))))
            (global.set $dynamic-package (call $cons (struct.get $obj $a (ref.cast (ref $obj) (local.get $binding)))
              (global.get $dynamic-package)))))
        (local.set $scan (call $cdr (local.get $scan)))
        (local.set $options (call $cdr (local.get $options))) (br $next)))))
  #@(block $failed
    (try_table (catch $transfer $failed)
      (local.set $values (call $value-list (call $apply ,(native-object '(local.get $thunk))
        (if (result (ref null eq)) (i32.eq (local.get $mode) (i32.const 5))
          (then (call $cons (local.get $new) (ref.null eq))) (else (ref.null eq))))))
      (global.set $handlers (local.get $handlers)) (global.set $catches (local.get $catches))
      (global.set $restarts (local.get $restarts)) (global.set $restart-associations (local.get $associations))
      (if (i32.eq (local.get $mode) (i32.const 7))
        (then (call $native-restore-environment (local.get $environment))))
      (if (i32.eq (local.get $mode) (i32.const 3))
        (then (drop (call $apply ,(native-object '(local.get $data)) (ref.null eq)))))
      (if (i32.eqz (ref.is_null ,(native-object '(local.get $no-error))))
        (then (local.set $values (call $value-list
          (call $apply ,(native-object '(local.get $no-error)) (local.get $values))))))
      (call $clear-values) (return (extern.convert_any (local.get $values)))))
  #@(global.set $handlers (local.get $handlers)) #@(global.set $catches (local.get $catches))
  #@(global.set $restarts (local.get $restarts)) #@(global.set $restart-associations (local.get $associations))
  #@(if (i32.eq (local.get $mode) (i32.const 7))
    (then (call $native-restore-environment (local.get $environment))))
  #@(if (i32.eq (local.get $mode) (i32.const 3))
    (then
      (local.set $saved-target (global.get $transfer-target))
      (local.set $saved-function (global.get $transfer-function))
      (local.set $values (global.get $transfer-values)) (local.set $condition (global.get $last-condition))
      (local.set $error-code (global.get $error)) (local.set $symbol (global.get $unbound-symbol))
      (drop (call $apply ,(native-object '(local.get $data)) (ref.null eq)))
      (global.set $transfer-target (local.get $saved-target))
      (global.set $transfer-function (local.get $saved-function))
      (global.set $transfer-values (local.get $values)) (global.set $last-condition (local.get $condition))
      (global.set $error (local.get $error-code)) (global.set $unbound-symbol (local.get $symbol))
      (throw $transfer)))
  #@(if (i32.or (i32.or (i32.eq (local.get $mode) (i32.const 1))
                       (i32.or (i32.eq (local.get $mode) (i32.const 4))
                         (i32.ge_u (local.get $mode) (i32.const 6))))
                (i32.eqz (ref.eq (global.get $transfer-target) (local.get $target))))
    (then (throw $transfer)))
  #@(local.set $values (global.get $transfer-values))
  #@(global.set $error (i32.const 0))
  #@(if (i32.or (i32.eq (local.get $mode) (i32.const 2)) (i32.eq (local.get $mode) (i32.const 5)))
    (then (local.set $values (call $value-list (call $apply (global.get $transfer-function) (local.get $values))))))
  #@(call $clear-values) #@(extern.convert_any (local.get $values)))

(define-runtime-function $native-restart-signal
    (($environment externref) ($mode i32) ($arguments externref) ($restarts externref)) (externref)
  (:export-raw "native-restart-signal")
  (:locals ($args :nullable-object) ($control :nullable-object) ($condition :nullable-object)
           ($saved :nullable-object) ($values :nullable-object) ($thrown i32) ($operator i32))
  #@(call $native-restore-environment (local.get $environment))
  #@(local.set $args ,(native-object '(local.get $arguments)))
  #@(if (i32.eq (local.get $mode) (i32.const 3))
    (then (call $arity (local.get $args) (i32.const 2) (i32.const 2147483647))
      (local.set $control (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))))
  #@(call $arity (local.get $args) (i32.const 1) (i32.const 2147483647))
  #@(local.set $condition (call $coerce-condition (call $car (local.get $args)) (call $cdr (local.get $args))
    (call $symbol (if (result i32) (i32.eqz (local.get $mode))
      (then (i32.const ,(runtime-symbol-id 'simple-condition)))
      (else (if (result i32) (i32.eq (local.get $mode) (i32.const 2))
        (then (i32.const ,(runtime-symbol-id 'simple-warning)))
        (else (i32.const ,(runtime-symbol-id 'simple-error)))))))))
  #@(local.set $operator (if (result i32) (i32.eqz (local.get $mode))
    (then (i32.const ,(runtime-symbol-id 'signal)))
    (else (if (result i32) (i32.eq (local.get $mode) (i32.const 1))
      (then (i32.const ,(runtime-symbol-id 'error)))
      (else (if (result i32) (i32.eq (local.get $mode) (i32.const 2))
        (then (i32.const ,(runtime-symbol-id 'warn))) (else (i32.const ,(runtime-symbol-id 'cerror)))))))))
  #@(local.set $args (call $cons (local.get $condition) (ref.null eq)))
  #@(if (i32.eq (local.get $mode) (i32.const 3))
    (then (local.set $args (call $cons (local.get $control) (local.get $args)))))
  #@(local.set $saved (global.get $restart-associations))
  #@(global.set $restart-associations (call $cons (call $cons (local.get $condition)
    ,(native-object '(local.get $restarts))) (local.get $saved)))
  #@(block $returned
    (block $failed
      (try_table (catch $transfer $failed)
        (local.set $values (call $value-list (call $apply (call $symbol (local.get $operator)) (local.get $args))))
        (br $returned)))
    (local.set $thrown (i32.const 1)))
  #@(global.set $restart-associations (local.get $saved))
  #@(if (local.get $thrown) (then (throw $transfer)))
  #@(call $clear-values) #@(extern.convert_any (local.get $values)))

(define-runtime-function $native-throw (($environment externref) ($tag externref) ($values externref)) ()
  (:export-raw "native-throw")
  (:locals ($scan :nullable-object) ($target :nullable-object))
  #@(call $native-restore-environment (local.get $environment))
  #@(local.set $scan (global.get $catches))
  #@(block $missing (loop $next (br_if $missing (ref.is_null (local.get $scan)))
    (local.set $target (call $car (local.get $scan)))
    (if (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (local.get $target))) ,(native-object '(local.get $tag)))
      (then (call $transfer-to (local.get $target) ,(native-object '(local.get $values)))))
    (local.set $scan (call $cdr (local.get $scan))) (br $next)))
  (wat-fail :control-error))

(define-runtime-function $native-call
    (($environment externref) ($function externref) ($args externref)) (externref)
  (:export-raw "native-call")
  (:locals ($values :nullable-object))
  #@(local.set $values (call $value-list (call $apply
    ,(native-object '(local.get $function)) ,(native-object '(local.get $args)))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $values)))

(define-runtime-function $native-return (($values externref)) (externref)
  (:export-raw "native-return")
  #@(extern.convert_any (call $return-values ,(native-object '(local.get $values)))))

(define-runtime-function $native-return1 (($value externref)) (externref)
  (:export-raw "native-return1")
  #@(call $clear-values)
  #@(local.get $value))

(define-runtime-function $native-int64 (($value i64)) (externref)
  (:export-raw "native-int64")
  #@(extern.convert_any (call $int64 (local.get $value))))

(define-runtime-function $native-unbox-int64 (($value externref)) (i64)
  (:export-raw "native-unbox-int64")
  #@(call $scalar-integer-value (ref.as_non_null ,(native-object '(local.get $value)))))

(defun native-integer-fit-wat (function)
  ;; The computed result has wrapped i64 bits. Test whether the mathematical
  ;; result fits before boxing it; the ordinary arithmetic path promotes.
  (runtime-wat-sequence (ecase function
    (+ #@(i64.ge_s (i64.and
          (i64.xor (local.get $left) (local.get $computed))
          (i64.xor (local.get $right) (local.get $computed))) (i64.const 0)))
    (- #@(i64.ge_s (i64.and
          (i64.xor (local.get $left) (local.get $right))
          (i64.xor (local.get $left) (local.get $computed))) (i64.const 0)))
    (* ;; Unsigned magnitudes also represent ABS(MIN-I64). Guard zero before
       ;; division, and allow magnitude 2^63 only for a negative product.
       #@(if (result i32) (i64.eqz (local.get $right)) (then (i32.const 1))
         (else (i64.le_u
           (select (i64.sub (i64.const 0) (local.get $left)) (local.get $left)
             (i64.lt_s (local.get $left) (i64.const 0)))
           (i64.div_u
             (select (i64.const -9223372036854775808) (i64.const 9223372036854775807)
               (i64.lt_s (i64.xor (local.get $left) (local.get $right)) (i64.const 0)))
             (select (i64.sub (i64.const 0) (local.get $right)) (local.get $right)
               (i64.lt_s (local.get $right) (i64.const 0)))))))))))

(defun native-integer-entry-wat (function opcode result &optional checked)
  (runtime-wat-sequence
    #@(if (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
                  (i32.const ,(1+ (runtime-symbol-id function))))
      (then ,(runtime-wat-sequence (if checked
        #@(block $overflow
          (local.set $computed (,opcode (local.get $left) (local.get $right)))
          (br_if $overflow (i32.eqz ,(native-integer-fit-wat function)))
          (return (extern.convert_any (call ,result (local.get $computed)))))
        #@(return (extern.convert_any (call ,result (,opcode (local.get $left) (local.get $right)))))))))
    ;; Integer identity is EQL value identity. Only the redefined function path
    ;; and overflow need boxed arguments; the common path stays scalar.
    #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
      (call $cons (call $int64 (local.get $left))
        (call $cons (call $int64 (local.get $right)) (ref.null eq)))))))

(defmacro define-native-integer-entry (name export function opcode result &optional checked)
  `(define-runtime-function ,name (($function externref) ($left i64) ($right i64)) (externref)
     (:export-raw ,export)
     ,@(when checked '((:locals ($computed i64))))
     (native-integer-entry-wat ',function ',opcode ',result ,checked)))

(define-native-integer-entry $native-add-int64 "native-add-int64" + i64.add $int64)
(define-native-integer-entry $native-sub-int64 "native-sub-int64" - i64.sub $int64)
(define-native-integer-entry $native-mul-int64 "native-mul-int64" * i64.mul $int64)
(define-native-integer-entry $native-logand-int64 "native-logand-int64" logand i64.and $int64)
(define-native-integer-entry $native-logior-int64 "native-logior-int64" logior i64.or $int64)
(define-native-integer-entry $native-logxor-int64 "native-logxor-int64" logxor i64.xor $int64)

(define-runtime-function $native-ash-int64
    (($function externref) ($left i64) ($right i64)) (externref)
  (:export-raw "native-ash-int64")
  (:locals ($computed i64))
  #@(if (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
                (i32.const ,(1+ (runtime-symbol-id 'ash))))
    (then
      (if (i64.lt_s (local.get $right) (i64.const 0))
        (then
          ;; Wasm masks shift counts; Lisp saturates an arithmetic right
          ;; shift once every value bit has been shifted out.
          (return (extern.convert_any (call $int64
            (if (result i64) (i64.le_s (local.get $right) (i64.const -64))
              (then (select (i64.const -1) (i64.const 0) (i64.lt_s (local.get $left) (i64.const 0))))
              (else (i64.shr_s (local.get $left) (i64.sub (i64.const 0) (local.get $right))))))))))
      (if (i64.eqz (local.get $left)) (then (return (extern.convert_any (call $int (i32.const 0))))))
      (if (i64.lt_u (local.get $right) (i64.const 64))
        (then
          (local.set $computed (i64.shl (local.get $left) (local.get $right)))
          (if (i64.eq (i64.shr_s (local.get $computed) (local.get $right)) (local.get $left))
            (then (return (extern.convert_any (call $int64 (local.get $computed))))))))))
  ;; Positive shifts that overflow i64 use arbitrary-precision arithmetic.
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons (call $int64 (local.get $left))
      (call $cons (call $int64 (local.get $right)) (ref.null eq))))))
(define-native-integer-entry $native-add-int64-checked "native-add-int64-checked" + i64.add $int64 t)
(define-native-integer-entry $native-sub-int64-checked "native-sub-int64-checked" - i64.sub $int64 t)
(define-native-integer-entry $native-mul-int64-checked "native-mul-int64-checked" * i64.mul $int64 t)
(define-native-integer-entry $native-lt-int64 "native-lt-int64" < i64.lt_s $bool)
(define-native-integer-entry $native-gt-int64 "native-gt-int64" > i64.gt_s $bool)
(define-native-integer-entry $native-le-int64 "native-le-int64" <= i64.le_s $bool)
(define-native-integer-entry $native-ge-int64 "native-ge-int64" >= i64.ge_s $bool)
(define-native-integer-entry $native-eq-int64 "native-eq-int64" = i64.eq $bool)
(define-native-integer-entry $native-ne-int64 "native-ne-int64" /= i64.ne $bool)

(define-runtime-function $native-vector-ref
    (($function externref) ($vector externref) ($index i32) ($code i32)) (externref)
  (:export-raw "native-vector-ref")
  (:locals ($data (ref $objects)))
  ;; A declaration proves the vector representation and immediate index.
  ;; Bounds errors retain the ordinary Lisp condition and offending datum.
  ;; Share the redefinition guard rather than repeating it at each field read.
  #@(if (i32.ne (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (local.get $code))
    (then (return (extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
      (call $cons ,(native-object '(local.get $vector)) (call $cons (call $int (local.get $index)) (ref.null eq))))))))
  #@(local.set $data (call $vector-data ,(native-object '(local.get $vector))))
  #@(if (i32.ge_u (local.get $index) (array.len (local.get $data)))
    (then (return (extern.convert_any (call $vector-element ,(native-object '(local.get $vector))
      (call $int (local.get $index)) (ref.null eq) (i32.const 0))))))
  #@(extern.convert_any (array.get $objects (local.get $data) (local.get $index))))

(define-runtime-function $native-vector-ref-trusted
    (($function externref) ($vector externref) ($index i32) ($code i32)) (externref)
  (:export-raw "native-vector-ref-trusted")
  ;; SAFETY 0: declarations and the access preconditions are trusted. WasmGC
  ;; casts and intrinsic array bounds still trap rather than corrupt memory.
  #@(if (i32.ne (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (local.get $code))
    (then (return (extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
      (call $cons ,(native-object '(local.get $vector)) (call $cons (call $int (local.get $index)) (ref.null eq))))))))
  #@(extern.convert_any (array.get $objects
    (call $vector-data ,(native-object '(local.get $vector))) (local.get $index))))

(defun native-list-entry-wat (first second field)
  (runtime-wat-sequence
    #@(if (i32.or
      (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
        (i32.const ,(1+ (runtime-symbol-id first))))
      (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
        (i32.const ,(1+ (runtime-symbol-id second)))))
      (then
        (if (ref.is_null (local.get $list)) (then (return (ref.null extern))))
        (return (extern.convert_any
          (struct.get $obj ,field (ref.cast (ref $obj) ,(native-object '(local.get $list))))))))
    #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
      (call $cons ,(native-object '(local.get $list)) (ref.null eq))))))

(define-runtime-function $native-vector-ref-trusted64
    (($function externref) ($vector externref) ($index i64) ($code i32)) (externref)
  (:export-raw "native-vector-ref-trusted64")
  ;; Keep the entire integer for a redefined function's ordinary call. Only
  ;; the built-in unsafe access uses Wasm's unsigned i32 array index.
  #@(if (i32.ne (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (local.get $code))
    (then (return (extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
      (call $cons ,(native-object '(local.get $vector)) (call $cons (call $int64 (local.get $index)) (ref.null eq))))))))
  #@(extern.convert_any (array.get $objects
    (call $vector-data ,(native-object '(local.get $vector))) (i32.wrap_i64 (local.get $index)))))

(define-runtime-function $native-car-list (($function externref) ($list externref)) (externref)
  (:export-raw "native-car-list")
  (native-list-entry-wat 'car 'first '$a))

(defun native-checked-list-entry-wat (first second field)
  (runtime-wat-sequence
    #@(if (i32.or
        (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
          (i32.const ,(1+ (runtime-symbol-id first))))
        (i32.eq (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $function))))
          (i32.const ,(1+ (runtime-symbol-id second)))))
      (then
        (if (ref.is_null (local.get $list)) (then (return (ref.null extern))))
        (if (i32.eq (call $tag ,(native-object '(local.get $list))) (i32.const 7))
          (then (return (extern.convert_any
            (struct.get $obj ,field (ref.cast (ref $obj) ,(native-object '(local.get $list))))))))))
    ;; Erroneous operands and redefined functions retain ordinary Lisp
    ;; dispatch and conditions. The successful operation is one bounded read.
    #@(return_call $native-call1 (local.get $function) (local.get $list))))

(define-runtime-function $native-car-checked (($function externref) ($list externref)) (externref)
  (:export-raw "native-car-checked")
  (native-checked-list-entry-wat 'car 'first '$a))

(define-runtime-function $native-cdr-checked (($function externref) ($list externref)) (externref)
  (:export-raw "native-cdr-checked")
  (native-checked-list-entry-wat 'cdr 'rest '$b))

(define-runtime-function $native-vector-ref-inline (($vector externref) ($index i64)) (externref)
  (:export-raw "native-vector-ref-inline")
  ;; The compiler proved the representation and captured the canonical
  ;; built-in definition under INLINE. No function or argument dispatch.
  #@(extern.convert_any (array.get $objects
    (call $vector-data ,(native-object '(local.get $vector))) (i32.wrap_i64 (local.get $index)))))

(define-runtime-function $native-vector-set-inline
    (($value externref) ($vector externref) ($index i64)) (externref)
  (:export-raw "native-vector-set-inline")
  #@(array.set $objects (call $vector-data ,(native-object '(local.get $vector)))
    (i32.wrap_i64 (local.get $index)) ,(native-object '(local.get $value)))
  #@(local.get $value))

(define-runtime-function $native-cdr-list (($function externref) ($list externref)) (externref)
  (:export-raw "native-cdr-list")
  (native-list-entry-wat 'cdr 'rest '$b))

(define-runtime-function $native-car-list-inline (($list externref)) (externref)
  (:export-raw "native-car-list-inline")
  #@(if (ref.is_null ,(native-object '(local.get $list))) (then (return (ref.null extern))))
  #@(extern.convert_any (struct.get $obj $a
    (ref.cast (ref $obj) ,(native-object '(local.get $list))))))

(define-runtime-function $native-cdr-list-inline (($list externref)) (externref)
  (:export-raw "native-cdr-list-inline")
  #@(if (ref.is_null ,(native-object '(local.get $list))) (then (return (ref.null extern))))
  #@(extern.convert_any (struct.get $obj $b
    (ref.cast (ref $obj) ,(native-object '(local.get $list))))))

(define-runtime-function $native-boolean (($value i32)) (externref)
  (:export-raw "native-boolean")
  #@(extern.convert_any (call $bool (local.get $value))))

(define-runtime-function $native-eq-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-eq-inline")
  #@(extern.convert_any (call $bool (ref.eq ,(native-object '(local.get $left)) ,(native-object '(local.get $right))))))

(define-runtime-function $native-eql-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-eql-inline")
  #@(extern.convert_any (call $bool (call $eql ,(native-object '(local.get $left)) ,(native-object '(local.get $right))))))

(define-runtime-function $native-length-inline (($sequence externref)) (externref)
  (:export-raw "native-length-inline")
  #@(extern.convert_any (call $int (call $proper-sequence-length
    ,(native-object '(local.get $sequence))))))

(define-runtime-function $native-vector-length-inline (($vector externref)) (externref)
  (:export-raw "native-vector-length-inline")
  #@(extern.convert_any (call $int (array.len (call $vector-data
    ,(native-object '(local.get $vector)))))))

(define-runtime-function $native-gethash-inline
    (($key externref) ($table externref) ($default externref) ($full i32)) (externref)
  (:export-raw "native-gethash-inline")
  (:locals ($cell :nullable-object) ($value :nullable-object))
  #@(local.set $cell (call $hash-cell-native
    ,(native-object '(local.get $key)) ,(native-object '(local.get $table))))
  #@(local.set $value (if (result (ref null eq)) (ref.is_null (local.get $cell))
    (then ,(native-object '(local.get $default))) (else (call $cdr (local.get $cell)))))
  #@(if (local.get $full)
    (then (local.set $value (call $cons (local.get $value)
      (call $cons (call $bool (i32.eqz (ref.is_null (local.get $cell)))) (ref.null eq))))))
  #@(extern.convert_any (local.get $value)))

(define-runtime-function $native-gethash-primary-inline (($key externref) ($table externref)) (externref)
  (:export-raw "native-gethash-primary-inline")
  (:locals ($cell :nullable-object))
  #@(local.set $cell (call $hash-cell-native
    ,(native-object '(local.get $key)) ,(native-object '(local.get $table))))
  #@(extern.convert_any (if (result (ref null eq)) (ref.is_null (local.get $cell))
    (then (ref.null eq)) (else (call $cdr (local.get $cell))))))

(define-runtime-function $native-puthash-inline
    (($value externref) ($key externref) ($table externref)) (externref)
  (:export-raw "native-puthash-inline")
  #@(extern.convert_any (call $puthash-native ,(native-object '(local.get $key))
    ,(native-object '(local.get $table)) ,(native-object '(local.get $value)))))

(define-runtime-function $native-consp-inline (($value externref)) (externref)
  (:export-raw "native-consp-inline")
  #@(extern.convert_any (call $bool (i32.eq
    (call $tag ,(native-object '(local.get $value))) (i32.const 7)))))

(define-runtime-function $native-simple-vector-p-inline (($value externref)) (externref)
  (:export-raw "native-simple-vector-p-inline")
  #@(extern.convert_any (call $bool (i32.eq
    (call $tag ,(native-object '(local.get $value))) (i32.const 34)))))

(define-runtime-function $native-primary-apply
    (($function :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($value :nullable-object))
  #@(local.set $value (call $apply (local.get $function) (local.get $args)))
  #@(call $clear-values)
  #@(local.get $value))

(define-runtime-function $native-builtin-id (($function :nullable-object)) (i32)
  ;; Test the actual resolved function, so redefining a global function in a
  ;; development image still takes effect. Native and interpreted closures use
  ;; N=0 and cannot accidentally enter a builtin fast path.
  #@(if (i32.eq (call $tag (local.get $function)) (i32.const 8))
    (then (return (i32.sub
      (struct.get $obj $n (ref.cast (ref $obj) (local.get $function))) (i32.const 1)))))
  #@(i32.const -1))

(define-runtime-function $native-poll () ()
  (:export-raw "native-poll")
  #@(call $thread-checkpoint))

(define-runtime-function $native-tail-enter (($function externref)) (externref)
  (:export-raw "native-tail-enter")
  ;; Match the dispatcher's checkpoint, frame and multiple-value setup.
  #@(call $thread-checkpoint)
  #@(if (global.get $backtrace-depth)
    (then (array.set $objects (global.get $backtrace-frames)
      (i32.and (i32.sub (global.get $backtrace-depth) (i32.const 1)) (i32.const 63))
      ,(native-object '(local.get $function)))))
  #@(call $clear-values)
  #@(local.get $function))

(define-runtime-function $native-tail-direct (($function externref)) (i32)
  (:export-raw "native-tail-direct")
  #@(if (i32.or (global.get $debug-evaluating)
                (ref.test (ref $native-debug-function) ,(native-object '(local.get $function))))
    (then (return (i32.const 0))))
  #@(drop (call $native-tail-enter (local.get $function)))
  #@(i32.const 1))

(define-runtime-function $native-tail-call (($function externref) ($arguments externref)) (externref)
  (:export-raw "native-tail-call")
  (:locals ($name :nullable-object) ($args :nullable-object) ($id i32))
  #@(local.set $name ,(native-object '(local.get $function)))
  #@(local.set $args ,(native-object '(local.get $arguments)))
  #@(loop $dispatch
    (call $thread-checkpoint)
    (if (call $symbolp (local.get $name))
      (then (local.set $name (call $fdefinition (local.get $name)))))
    ;; APPLY and FUNCALL are dispatch adapters, not additional Lisp frames.
    ;; Inspect the actual function so global redefinitions still take effect.
    (local.set $id (call $native-builtin-id (local.get $name)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'funcall)))
      (then (call $arity (local.get $args) (i32.const 1) (i32.const 2147483647))
        (local.set $name (call $car (local.get $args)))
        (local.set $args (call $cdr (local.get $args))) (br $dispatch)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'apply)))
      (then (local.set $name (call $car (local.get $args)))
        (local.set $args (call $spread-arguments (local.get $args))) (br $dispatch)))
    ;; Preserve interactive debugger entry and frames for debug-enabled code.
    (if (i32.and (i32.eqz (global.get $debug-evaluating))
          (i32.and (ref.test (ref $native-function) (local.get $name))
                   (i32.eqz (ref.test (ref $native-debug-function) (local.get $name)))))
      (then
        (if (global.get $backtrace-depth)
          (then (array.set $objects (global.get $backtrace-frames)
            (i32.and (i32.sub (global.get $backtrace-depth) (i32.const 1)) (i32.const 63))
            (local.get $name))))
        (call $clear-values)
        (return_call_ref $native-entry
          (extern.convert_any (local.get $name)) (extern.convert_any (local.get $args))
          (struct.get $native-function $entry (ref.cast (ref $native-function) (local.get $name))))))
    (return (extern.convert_any (call $apply (local.get $name) (local.get $args)))))
  #@(unreachable))

(define-runtime-function $native-attach-direct (($function externref) ($entry funcref)) (externref)
  (:export-raw "native-attach-direct")
  (:locals ($fn (ref $native-function)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) ,(native-object '(local.get $function)))) :malformed-form)
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-direct-entry) (local.get $entry))) :malformed-form)
  #@(local.set $fn (ref.cast (ref $native-function) ,(native-object '(local.get $function))))
  (wat-fail-when #@(i32.or (f64.lt (struct.get $obj $f (local.get $fn)) (f64.const 2))
                         (f64.gt (struct.get $obj $f (local.get $fn)) (f64.const 6))) :malformed-form)
  ;; Static closure caches can return the same function object repeatedly.
  #@(if (i32.eqz (ref.is_null (struct.get $native-function $direct (local.get $fn))))
    (then (return (local.get $function))))
  #@(struct.set $native-function $direct (local.get $fn)
    (struct.new $native-direct-code (call $allocate-object-tag (i32.const 55)) (i32.const 0) (f64.const 0)
      (struct.get $obj $digits (local.get $fn)) (ref.null eq) (ref.null eq) (ref.null eq)
      (ref.cast (ref $native-direct-entry) (local.get $entry))))
  #@(local.get $function))

(define-runtime-function $native-attach-direct-root
    (($function externref) ($entry funcref) ($arity i32)) (externref)
  (:export-raw "native-attach-direct-root")
  #@(return_call $native-attach-direct
    (extern.convert_any (call $native-with-arity ,(native-object '(local.get $function)) (local.get $arity)))
    (local.get $entry)))

(define-runtime-function $native-direct-apply
    (($name (ref $native-function)) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object)) (:nullable-object)
  (:locals ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object) ($restart-associations :nullable-object)
           ($catches :nullable-object) ($targets :nullable-object) ($thrown i32)
           ($backtrace-depth i32) ($backtrace-slot i32) ($backtrace-frame :nullable-object)
           ($result :nullable-object))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $backtrace-depth (global.get $backtrace-depth))
  #@(local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
  #@(local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
  #@(array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $name))
  #@(global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
  #@(call $clear-values)
  (wat-dynamic-call
    #@(ref.cast (ref null eq) (any.convert_extern
      (call_ref $native-direct-entry (extern.convert_any (local.get $name))
        (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
        (extern.convert_any (local.get $c)) (extern.convert_any (local.get $d))
        (struct.get $native-direct-code $entry (ref.as_non_null (struct.get $native-function $direct (local.get $name)))))))
    :cleanup #@(block
      (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
      (global.set $backtrace-depth (local.get $backtrace-depth)))))

(define-runtime-function $native-set-argument-usage (($function externref) ($usage i32)) (externref)
  (:export-raw "native-set-argument-usage")
  (:locals ($body (ref $native-function)) ($arity i32))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) ,(native-object '(local.get $function)))) :malformed-form)
  #@(local.set $body (ref.cast (ref $native-function) ,(native-object '(local.get $function))))
  (wat-fail-when #@(ref.is_null (struct.get $native-function $direct (local.get $body))) :malformed-form)
  #@(local.set $arity (i32.sub (i32.trunc_f64_u (struct.get $obj $f (local.get $body))) (i32.const 2)))
  (wat-fail-when #@(i32.ge_u (local.get $usage) (i32.shl (i32.const 1) (local.get $arity))) :malformed-form)
  ;; Zero denotes an unknown usage mask for older compiled modules.
  #@(struct.set $obj $n (ref.as_non_null (struct.get $native-function $direct (local.get $body)))
    (i32.add (local.get $usage) (i32.const 1)))
  #@(local.get $function))

(define-runtime-function $native-shared-entry (($function externref) ($args externref)) (externref)
  (:export-raw "native-shared-entry")
  (:locals ($body (ref $native-function)) ($arity i32) ($usage i32))
  ;; The caller established its Lisp frame and cleanup. This shared checked
  ;; adapter must tail-call the worker without adding another Lisp frame.
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) ,(native-object '(local.get $function)))) :malformed-form)
  #@(local.set $body (ref.cast (ref $native-function) ,(native-object '(local.get $function))))
  (wat-fail-when #@(ref.is_null (struct.get $native-function $direct (local.get $body))) :malformed-form)
  #@(local.set $arity (i32.sub (i32.trunc_f64_u (struct.get $obj $f (local.get $body))) (i32.const 2)))
  #@(local.set $usage (struct.get $obj $n (ref.as_non_null (struct.get $native-function $direct (local.get $body)))))
  #@(call $native-check-arguments (local.get $args) (local.get $arity) (local.get $arity))
  #@(return_call_ref $native-direct-entry (local.get $function)
    ,@(loop for index below 4 collect
      `(if (result externref) (i32.and (i32.gt_u (local.get $arity) (i32.const ,index))
             (i32.or (i32.eqz (local.get $usage))
               (i32.ne (i32.and (i32.sub (local.get $usage) (i32.const 1)) (i32.const ,(ash 1 index))) (i32.const 0))))
         (then (call $native-argument (local.get $args) (i32.const ,index))) (else (ref.null extern))))
    (struct.get $native-direct-code $entry (ref.as_non_null (struct.get $native-function $direct (local.get $body))))))

(define-runtime-function $native-attach-direct-usage
    (($function externref) ($entry funcref) ($usage i32)) (externref)
  (:export-raw "native-attach-direct-usage")
  #@(return_call $native-set-argument-usage
    (call $native-attach-direct (local.get $function) (local.get $entry)) (local.get $usage)))

(defun native-direct-call-wat (arity &optional checked)
  ;; Reuse the caller's native-function and fixed-arity checks when present.
  (let* ((names (case arity (0 nil) (1 '($value)) (2 '($left $right))
                        (3 '($a $b $c)) (4 '($a $b $c $d))))
         (fn (native-object '(local.get $function)))
         (direct (wat-fragment #@(if (i32.eqz (ref.is_null
                       (struct.get $native-function $direct (ref.cast (ref $native-function) ,fn))))
                     (then
                       (local.set $direct-result
                         (call $native-direct-apply (ref.cast (ref $native-function) ,fn)
                           ,@(loop for index below 4 collect (if (< index arity)
                             (native-object (list 'local.get (nth index names))) '(ref.null eq)))))
                       (call $clear-values)
                       (return (extern.convert_any (local.get $direct-result))))))))
    (if checked direct
      (wat-fragment #@(if (ref.test (ref $native-function) ,fn)
        (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,fn)) (f64.const ,(+ arity 2)))
          (then ,direct))))))))

(defun native-generic-fast-call-wat (arity)
  (let* ((names (case arity (0 nil) (1 '($value)) (2 '($left $right)) (3 '($a $b $c))))
         (id (if (zerop arity) `(call $native-builtin-id ,(native-object '(local.get $function))) '(local.get $id)))
         (expected (if (= arity 3) -2147483646 2147483644)))
    (wat-fragment #@(if (i32.eq ,id (i32.const ,expected))
      (then
        (call $generic-recent-call ,(native-object '(local.get $function)) (i32.const ,arity)
          ,@(loop for index below 4 collect (if (< index arity)
               (native-object (list 'local.get (nth index names))) '(ref.null eq))))
        (local.set $hit) (local.set $direct-result)
        (if (local.get $hit)
          (then (call $clear-values) (return (extern.convert_any (local.get $direct-result))))))))))

(define-runtime-function $native-call0 (($function externref)) (externref)
  (:export-raw "native-call0")
  (:locals ($direct-result :nullable-object) ($hit i32))
  (native-direct-call-wat 0)
  (native-generic-fast-call-wat 0)
  #@(extern.convert_any (call $native-primary-apply
    ,(native-object '(local.get $function)) (ref.null eq))))

(define-runtime-function $native-generic-call
    (($plan externref) ($function externref) ($args externref) ($primary i32)) (externref)
  (:export-raw "native-generic-call")
  (:locals ($effective :nullable-object) ($result :nullable-object))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $effective (call $generic-call-plan-function
    ,(native-object '(local.get $plan)) ,(native-object '(local.get $function)) ,(native-object '(local.get $args))
    (i32.shr_u (local.get $primary) (i32.const 1))))
  #@(if (ref.is_null (local.get $effective))
    (then (local.set $result (call $apply ,(native-object '(local.get $function)) ,(native-object '(local.get $args)))))
    (else
      ;; The guarded plan selects the entire effective method, preserving
      ;; method combination and next-method context. Its compiled unary
      ;; entry accepts the original argument list directly.
      (if (if (result i32) (ref.test (ref $native-function) (local.get $effective))
            (then (i32.and
              (f64.eq (struct.get $obj $f (ref.cast (ref $obj) (local.get $effective))) (f64.const 3))
              (i32.eqz (ref.is_null (struct.get $native-function $direct
                (ref.cast (ref $native-function) (local.get $effective)))))))
            (else (i32.const 0)))
        (then (local.set $result (call $native-direct-apply
          (ref.cast (ref $native-function) (local.get $effective))
          ,(native-object '(local.get $args)) (ref.null eq) (ref.null eq) (ref.null eq))))
        (else (local.set $result (call $apply (local.get $effective)
          (call $cons ,(native-object '(local.get $args)) (ref.null eq))))))))
  #@(if (i32.eqz (i32.and (local.get $primary) (i32.const 1)))
    (then (local.set $result (call $value-list (local.get $result)))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-call1 (($function externref) ($value externref)) (externref)
  (:export-raw "native-call1")
  (:locals ($direct-result :nullable-object) ($hit i32) ($id i32) ($tag i32) ($accessor i32) ($a :nullable-object))
  #@(local.set $a ,(native-object '(local.get $value)))
  #@(local.set $id (call $native-builtin-id ,(native-object '(local.get $function))))
  #@(local.set $tag (call $tag (local.get $a)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 3))
      (then ,(native-direct-call-wat 1 t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 1 (local.get $a)))))))))
  (wat-symbol-case #@(local.get $id)
    ((not null) #@(return (extern.convert_any (call $bool (ref.is_null (local.get $a))))))
    (identity #@(return (local.get $value)))
    (list #@(return (extern.convert_any (call $cons (local.get $a) (ref.null eq)))))
    ((car first)
      #@(if (i32.or (ref.is_null (local.get $a)) (i32.eq (local.get $tag) (i32.const 7)))
        (then (return (extern.convert_any (call $car (local.get $a)))))))
    ((cdr rest)
      #@(if (i32.or (ref.is_null (local.get $a)) (i32.eq (local.get $tag) (i32.const 7)))
        (then (return (extern.convert_any (call $cdr (local.get $a)))))))
    (consp #@(return (extern.convert_any (call $bool (i32.eq (local.get $tag) (i32.const 7))))))
    (atom #@(return (extern.convert_any (call $bool (i32.ne (local.get $tag) (i32.const 7))))))
    (symbolp #@(return (extern.convert_any (call $bool (call $symbolp (local.get $a))))))
    ((listp stringp)
      #@(return (extern.convert_any (call $bool (call $simple-predicate-value (local.get $id) (local.get $a))))))
    (integerp #@(return (extern.convert_any (call $bool (i32.eq (local.get $tag) (i32.const 1))))))
    (numberp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 4))))))
    (rationalp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 1))))))
    (realp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 3))))))
    (complexp #@(return (extern.convert_any (call $bool (i32.eq (local.get $tag) (i32.const 5))))))
    (floatp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 3)) (i32.const 1))))))
    ((1+ 1-)
      #@(if (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 4))
        (then (return (extern.convert_any (call $op (i32.const 0) (local.get $a)
          (call $int (select (i32.const 1) (i32.const -1)
            (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '1+)))))))))))
    (length
      #@(if (call $vectorp-native (local.get $a))
        (then (return (extern.convert_any (call $int (call $array-sequence-length (local.get $a)))))))))
  #@(local.set $accessor (call $list-accessor-code (local.get $id)))
  #@(if (local.get $accessor)
    (then (return (extern.convert_any (call $list-accessor
      (local.get $a) (local.get $accessor) (i32.const 0) (ref.null eq))))))
  ;; DEFSTRUCT readers already carry their class and slot descriptor. Use
  ;; the same checked access without constructing an APPLY argument list.
  #@(if (ref.test (ref $direct-function) ,(native-object '(local.get $function)))
    (then (if (i32.eq (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) ,(native-object '(local.get $function)))) (i32.const 0))
      (then (local.set $a (call $structure-access (local.get $a)
        (struct.get $direct-function $owner (ref.cast (ref $direct-function) ,(native-object '(local.get $function))))
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) ,(native-object '(local.get $function))))
        (ref.null eq) (i32.const 0)))
        (call $clear-values)
        (return (extern.convert_any (local.get $a)))))))
  ;; Uncommon functions and exceptional arguments share the complete call
  ;; path, including its backtrace, arity checks and dynamic cleanup.
  (native-generic-fast-call-wat 1)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons (local.get $a) (ref.null eq)))))

(define-runtime-function $native-call2
    (($function externref) ($left externref) ($right externref)) (externref)
  (:export-raw "native-call2")
  (:locals ($direct-result :nullable-object) ($hit i32) ($id i32) ($at i32) ($bt i32) ($index i32) ($cmp i32)
           ($a :nullable-object) ($b :nullable-object) ($cell :nullable-object) ($vector (ref $array)))
  #@(local.set $a ,(native-object '(local.get $left)))
  #@(local.set $b ,(native-object '(local.get $right)))
  #@(local.set $id (call $native-builtin-id ,(native-object '(local.get $function))))
  #@(local.set $at (call $tag (local.get $a)))
  #@(local.set $bt (call $tag (local.get $b)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 4))
      (then ,(native-direct-call-wat 2 t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 2 (local.get $a) (local.get $b)))))))))
  (wat-symbol-case #@(local.get $id)
    (eq #@(return (extern.convert_any (call $bool (ref.eq (local.get $a) (local.get $b))))))
    (eql #@(return (extern.convert_any (call $bool (call $eql (local.get $a) (local.get $b))))))
    ((= /=)
      #@(if (i32.and (i32.le_u (i32.sub (local.get $at) (i32.const 1)) (i32.const 4))
                     (i32.le_u (i32.sub (local.get $bt) (i32.const 1)) (i32.const 4)))
        (then (return (extern.convert_any (call $bool
          (i32.xor (call $equal (local.get $a) (local.get $b))
            (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '/=))))))))))
    (cons #@(return (extern.convert_any (call $cons (local.get $a) (local.get $b)))))
    (list #@(return (extern.convert_any
      (call $cons (local.get $a) (call $cons (local.get $b) (ref.null eq))))))
    ((vector-push vector-push-extend)
      ;; The common append needs no argument list or general APPLY dispatch.
      ;; Retain the full path for missing fill pointers and capacity growth.
      #@(if (i32.eq (local.get $bt) (i32.const 35))
        (then
          (local.set $vector (ref.cast (ref $array) (local.get $b)))
          (local.set $index (struct.get $array $fill (local.get $vector)))
          (if (i32.and (i32.ge_s (local.get $index) (i32.const 0))
                      (i32.lt_u (local.get $index) (call $array-length (local.get $b))))
            (then
              (drop (call $array-raw-element (local.get $b) (local.get $index) (local.get $a) (i32.const 1)))
              (struct.set $array $fill (local.get $vector) (i32.add (local.get $index) (i32.const 1)))
              (return (extern.convert_any (call $int (local.get $index)))))))))
    (gethash
      #@(if (i32.eq (local.get $bt) (i32.const 29))
        (then (local.set $cell (call $hash-cell-native (local.get $a) (local.get $b)))
          (return (extern.convert_any (if (result (ref null eq)) (ref.is_null (local.get $cell))
            (then (ref.null eq)) (else (call $cdr (local.get $cell)))))))))
    ((bit sbit)
      #@(if (i32.and (i32.eq (local.get $at) (i32.const 36)) (i32.eq (local.get $bt) (i32.const 1)))
        (then
          (if (i32.le_u (call $integer-length (ref.as_non_null (local.get $b))) (i32.const 2))
            (then (local.set $index (call $small-integer-value (ref.as_non_null (local.get $b))))
              (if (i32.lt_u (local.get $index) (call $value-n (local.get $a)))
                (then (return (extern.convert_any (call $int
                  (i32.and (i32.shr_u
                    (array.get $digits (struct.get $obj $digits (ref.cast (ref $obj) (local.get $a)))
                      (i32.shr_u (local.get $index) (i32.const 5)))
                    (local.get $index)) (i32.const 1))))))))))))
    ((+ - *)
      #@(if (i32.and (i32.le_u (i32.sub (local.get $at) (i32.const 1)) (i32.const 4))
                     (i32.le_u (i32.sub (local.get $bt) (i32.const 1)) (i32.const 4)))
        (then (return (extern.convert_any (call $op
          (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id '+))) (local.get $a) (local.get $b)))))))
    ((< > <= >=)
      #@(if (i32.and (i32.le_u (i32.sub (local.get $at) (i32.const 1)) (i32.const 3))
                     (i32.le_u (i32.sub (local.get $bt) (i32.const 1)) (i32.const 3)))
        (then
          (local.set $cmp (call $compare (local.get $a) (local.get $b)))
          (return (extern.convert_any (call $bool
            (if (result i32) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '<)))
              (then (i32.lt_s (local.get $cmp) (i32.const 0)))
              (else (if (result i32) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '>)))
                (then (i32.gt_s (local.get $cmp) (i32.const 0)))
                (else (if (result i32) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '<=)))
                  (then (i32.le_s (local.get $cmp) (i32.const 0)))
                  (else (i32.ge_s (local.get $cmp) (i32.const 0))))))))))))))
    ((aref svref)
      #@(if (i32.and (i32.eq (local.get $at) (i32.const 34)) (i32.eq (local.get $bt) (i32.const 1)))
        (then
          (if (i32.le_u (call $integer-length (ref.as_non_null (local.get $b))) (i32.const 2))
            (then
              (local.set $index (call $small-integer-value (ref.as_non_null (local.get $b))))
              (if (i32.lt_u (local.get $index) (array.len (call $vector-data (local.get $a))))
                (then (return (extern.convert_any (array.get $objects
                  (call $vector-data (local.get $a)) (local.get $index))))))))))))
  #@(if (ref.test (ref $direct-function) ,(native-object '(local.get $function)))
    (then (if (i32.eq (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) ,(native-object '(local.get $function)))) (i32.const 1))
      (then (return (extern.convert_any (call $structure-access (local.get $b)
        (struct.get $direct-function $owner (ref.cast (ref $direct-function) ,(native-object '(local.get $function))))
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) ,(native-object '(local.get $function))))
        (local.get $a) (i32.const 1))))))))
  (native-generic-fast-call-wat 2)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons (local.get $a) (call $cons (local.get $b) (ref.null eq))))))

(define-runtime-function $native-call3
    (($function externref) ($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-call3")
  (:locals ($direct-result :nullable-object) ($hit i32) ($id i32) ($index i32) ($array :nullable-object) ($subscript :nullable-object))
  #@(local.set $id (i32.sub (i32.const -2)
    (call $native-builtin-id ,(native-object '(local.get $function)))))
  #@(local.set $array ,(native-object '(local.get $b)))
  #@(local.set $subscript ,(native-object '(local.get $c)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 5))
      (then ,(native-direct-call-wat 3 t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 3
          ,(native-object '(local.get $a)) ,(native-object '(local.get $b)) ,(native-object '(local.get $c))))))))))
  #@(if (i32.eq (local.get $id) (i32.const ,(- -2 (runtime-symbol-id 'list))))
    (then (return (extern.convert_any
      (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
        (call $cons ,(native-object '(local.get $c)) (ref.null eq))))))))
  (wat-symbol-case #@(local.get $id)
    ((bit sbit)
      #@(if (i32.and (i32.eq (call $tag (local.get $array)) (i32.const 36))
                     (i32.eq (call $tag (local.get $subscript)) (i32.const 1)))
        (then
          (if (i32.le_u (call $integer-length (ref.as_non_null (local.get $subscript))) (i32.const 2))
            (then (local.set $index (call $small-integer-value (ref.as_non_null (local.get $subscript))))
              (if (i32.lt_u (local.get $index) (call $value-n (local.get $array)))
                (then (return (extern.convert_any (call $array-raw-element
                  (local.get $array) (local.get $index) ,(native-object '(local.get $a)) (i32.const 1)))))))))))
    ((aref svref row-major-aref)
      #@(if (i32.and (i32.eq (call $tag (local.get $array)) (i32.const 34))
                     (i32.eq (call $tag (local.get $subscript)) (i32.const 1)))
        (then
          (if (i32.le_u (call $integer-length (ref.as_non_null (local.get $subscript))) (i32.const 2))
            (then
              (local.set $index (call $small-integer-value (ref.as_non_null (local.get $subscript))))
              (if (i32.lt_u (local.get $index) (array.len (call $vector-data (local.get $array))))
                (then (array.set $objects (call $vector-data (local.get $array)) (local.get $index)
                  ,(native-object '(local.get $a)))
                  (return (local.get $a))))))))))
  (native-generic-fast-call-wat 3)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
      (call $cons ,(native-object '(local.get $c)) (ref.null eq)))))))

(define-runtime-function $native-call4
    (($function externref) ($a externref) ($b externref) ($c externref) ($d externref)) (externref)
  (:export-raw "native-call4")
  (:locals ($direct-result :nullable-object))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 6))
      (then ,(native-direct-call-wat 4 t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 4
          ,(native-object '(local.get $a)) ,(native-object '(local.get $b))
          ,(native-object '(local.get $c)) ,(native-object '(local.get $d))))))))))
  #@(if (i32.eq (call $native-builtin-id ,(native-object '(local.get $function)))
                 (i32.const ,(runtime-symbol-id 'list)))
    (then (return (extern.convert_any
      (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
        (call $cons ,(native-object '(local.get $c)) (call $cons ,(native-object '(local.get $d)) (ref.null eq)))))))))
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
      (call $cons ,(native-object '(local.get $c)) (call $cons ,(native-object '(local.get $d)) (ref.null eq))))))))

(define-runtime-function $native-value-arguments (($groups externref)) (:nullable-object)
  (:locals ($scan :nullable-object) ($group :nullable-object) ($head :nullable-object)
           ($tail :nullable-object) ($cell :nullable-object))
  #@(local.set $scan ,(native-object '(local.get $groups)))
  (wat-list-loop ($scan)
    #@(local.set $group (call $car (local.get $scan)))
    (wat-list-loop ($group)
      #@(local.set $cell (call $cons (call $car (local.get $group)) (ref.null eq)))
      #@(if (ref.is_null (local.get $head)) (then (local.set $head (local.get $cell)))
        (else (struct.set $obj $b (ref.cast (ref $obj) (local.get $tail)) (local.get $cell))))
      #@(local.set $tail (local.get $cell))))
  #@(local.get $head))

(define-runtime-function $native-multiple-value-call
    (($environment externref) ($function externref) ($groups externref)) (externref)
  (:export-raw "native-multiple-value-call")
  (:locals ($values :nullable-object))
  #@(local.set $values (call $value-list (call $apply ,(native-object '(local.get $function))
    (call $native-value-arguments (local.get $groups)))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $values)))

(define-runtime-function $native-tail-multiple-value-call
    (($environment externref) ($function externref) ($groups externref)) (externref)
  (:export-raw "native-tail-multiple-value-call")
  #@(return_call $native-tail-call (local.get $function)
    (extern.convert_any (call $native-value-arguments (local.get $groups)))))

(define-runtime-section native-code-runtime-wat ()
  #@(type $native-context (struct (field $variables (ref null eq)) (field $packages (ref null eq))
    (field $targets (ref null eq))))
  #@(global $native-environment-cache (mut (ref null $native-context)) (ref.null $native-context))
  #@(type $native-entry (func (param externref externref) (result externref)))
  #@(type $native-service (func (param externref externref externref) (result externref)))
  #@(type $native-direct-entry (func (param externref externref externref externref externref) (result externref)))
  #@(type $native-direct-code (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-direct-entry)))))
  #@(type $native-function (sub $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-entry)) (field $code-size i32)
    (field $direct (mut (ref null $native-direct-code))))))
  #@(type $native-debug-function (sub final $native-function (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-entry)) (field $code-size i32)
    (field $direct (mut (ref null $native-direct-code)))
    (field $debug-code (ref eq)) (field $debug-id i32))))
  #@(table $native-services (export "native-services") 1 funcref)
  ($compiler-image-function)
  ($native-install) ($native-function) ($native-function-with-code) ($native-close-with-arity) ($native-close) ($native-close-fixed) ($native-close-cached) ($native-close-static) ($native-with-arity) ($native-code-size) ($native-capture) ($native-code-storage) ($native-code-entry) ($native-code-bytes) ($native-literal)
  ($native-debug-wrap) ($native-debug-attach) ($native-debug-close) ($native-debug-info)
  ($native-attach-direct) ($native-attach-direct-root) ($native-set-argument-usage) ($native-attach-direct-usage) ($native-direct-apply) ($native-shared-entry)
  ($native-two-arguments) ($native-packed-argument) ($native-list-tail) ($native-argument) ($native-rest-argument) ($native-check-arguments) ($native-check-apply-arguments) ($native-check-apply-list) ($native-environment)
  ($native-restore-environment) ($native-bind)
  ($native-block-enter) ($native-block-environment) ($native-block-return) ($native-block-match) ($native-block-values)
  ($native-control) ($native-restart-signal) ($native-throw)
  ($native-global-read) ($native-global-write) ($native-function-reference)
  ($native-cons) ($native-cell) ($native-primary) ($native-cell-read) ($native-cell-write)
  ($native-call) ($native-value-arguments) ($native-multiple-value-call) ($native-tail-multiple-value-call)
  ($native-return) ($native-return1)
  ($native-int64) ($native-unbox-int64) ($native-add-int64) ($native-sub-int64) ($native-mul-int64)
  ($native-logand-int64) ($native-logior-int64) ($native-logxor-int64) ($native-ash-int64)
  ($native-add-int64-checked) ($native-sub-int64-checked) ($native-mul-int64-checked)
  ($native-lt-int64) ($native-gt-int64) ($native-le-int64) ($native-ge-int64) ($native-eq-int64) ($native-ne-int64)
  ($native-vector-ref) ($native-boolean)
  ($native-eq-inline) ($native-eql-inline) ($native-length-inline) ($native-vector-length-inline)
  ($native-gethash-inline) ($native-gethash-primary-inline) ($native-puthash-inline)
  ($native-consp-inline) ($native-simple-vector-p-inline)
  ($native-vector-ref-trusted) ($native-car-list) ($native-cdr-list)
  ($native-car-checked) ($native-cdr-checked)
  ($native-car-list-inline) ($native-cdr-list-inline)
  ($native-vector-ref-trusted64)
  ($native-vector-ref-inline)
  ($native-vector-set-inline)
  ($native-primary-apply) ($native-builtin-id) ($native-poll) ($native-tail-enter) ($native-tail-direct) ($native-tail-call)
  ($native-call0) ($native-call1) ($native-call2) ($native-call3) ($native-call4) ($native-generic-call))
