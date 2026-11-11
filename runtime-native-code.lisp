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
    (ref.cast (ref $native-entry) (local.get $entry)) (local.get $size))))

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
    (struct.get $native-function $code-size (ref.cast (ref $native-function) ,(native-object '(local.get $parent)))))))

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
    (struct.get $native-function $code-size (ref.cast (ref $native-function) (local.get $function)))))

(define-runtime-function $native-code-size (($function :nullable-object)) (:nullable-object)
  (:locals ($size i32))
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
      (then (return (struct.new $native-arguments (call $allocate-object-tag (i32.const 52))
        (i32.const 2) (f64.const 0) (ref.null $digits)
        (local.get $a) (local.get $b) (ref.null eq) (ref.null eq)))))))
  #@(call $cons (local.get $a) (call $cons (local.get $b) (ref.null eq))))

(define-runtime-function $native-packed-argument (($args (ref $native-arguments)) ($index i32)) (:nullable-object)
  #@(if (i32.ge_u (local.get $index) (call $value-n (local.get $args))) (then (return (ref.null eq))))
  #@(if (i32.eqz (local.get $index)) (then (return (struct.get $obj $a (ref.cast (ref $obj) (local.get $args))))))
  #@(if (i32.eq (local.get $index) (i32.const 1)) (then (return (struct.get $obj $b (ref.cast (ref $obj) (local.get $args))))))
  #@(if (i32.eq (local.get $index) (i32.const 2)) (then (return (struct.get $obj $c (ref.cast (ref $obj) (local.get $args))))))
  #@(struct.get $native-arguments $d (local.get $args)))

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
  #@(local.set $value ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $native-arguments) (local.get $value))
    (then
      (local.set $count (call $value-n (local.get $value)))
      (block $done (loop $next (br_if $done (i32.le_s (local.get $count) (local.get $index)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1)))
        (local.set $result (call $cons (call $native-packed-argument
          (ref.cast (ref $native-arguments) (local.get $value)) (local.get $count)) (local.get $result)))
        (br $next)))
      (return (extern.convert_any (local.get $result)))))
  #@(extern.convert_any (call $native-list-tail (local.get $value) (local.get $index))))

(define-runtime-function $native-argument (($args externref) ($index i32)) (externref)
  (:export-raw "native-argument")
  (:locals ($value :nullable-object))
  #@(local.set $value ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $native-arguments) (local.get $value))
    (then (return (extern.convert_any (call $native-packed-argument
      (ref.cast (ref $native-arguments) (local.get $value)) (local.get $index))))))
  #@(extern.convert_any (call $car (call $native-list-tail (local.get $value) (local.get $index)))))

(define-runtime-function $native-check-arguments (($args externref) ($count i32) ($maximum i32)) ()
  (:export-raw "native-check-arguments")
  (:locals ($scan :nullable-object) ($i i32))
  #@(local.set $scan ,(native-object '(local.get $args)))
  #@(if (ref.test (ref $native-arguments) (local.get $scan))
    (then (local.set $i (call $value-n (local.get $scan)))
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
  (:locals ($symbol :nullable-object) ($binding :nullable-object) ($value :nullable-object))
  ;; Compiled global calls overwhelmingly name symbols. Read the current
  ;; binding directly; retain FDEFINITION for SETF names and undefined calls.
  #@(local.set $symbol ,(native-object '(local.get $name)))
  #@(if (call $symbolp (local.get $symbol))
    (then
      (local.set $binding (call $symbol-index-get (global.get $function-bindings) (call $id (local.get $symbol))))
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
      (call $require-list (local.get $new))
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

(define-runtime-function $native-boolean (($value i32)) (externref)
  (:export-raw "native-boolean")
  #@(extern.convert_any (call $bool (local.get $value))))

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
    (then (return (i32.sub (call $value-n (local.get $function)) (i32.const 1)))))
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

(define-runtime-function $native-call0 (($function externref)) (externref)
  (:export-raw "native-call0")
  #@(extern.convert_any (call $native-primary-apply
    ,(native-object '(local.get $function)) (ref.null eq))))

(define-runtime-function $native-call1 (($function externref) ($value externref)) (externref)
  (:export-raw "native-call1")
  (:locals ($id i32) ($tag i32) ($accessor i32) ($a :nullable-object))
  #@(local.set $a ,(native-object '(local.get $value)))
  #@(local.set $id (call $native-builtin-id ,(native-object '(local.get $function))))
  #@(local.set $tag (call $tag (local.get $a)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 3))
      (then (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (struct.new $native-arguments (call $allocate-object-tag (i32.const 52))
        (i32.const 1) (f64.const 0) (ref.null $digits) (local.get $a) (ref.null eq) (ref.null eq) (ref.null eq)))))))))
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
  ;; Uncommon functions and exceptional arguments share the complete call
  ;; path, including its backtrace, arity checks and dynamic cleanup.
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons (local.get $a) (ref.null eq)))))

(define-runtime-function $native-call2
    (($function externref) ($left externref) ($right externref)) (externref)
  (:export-raw "native-call2")
  (:locals ($id i32) ($at i32) ($bt i32) ($index i32) ($cmp i32)
           ($a :nullable-object) ($b :nullable-object) ($cell :nullable-object) ($vector (ref $array)))
  #@(local.set $a ,(native-object '(local.get $left)))
  #@(local.set $b ,(native-object '(local.get $right)))
  #@(local.set $id (call $native-builtin-id ,(native-object '(local.get $function))))
  #@(local.set $at (call $tag (local.get $a)))
  #@(local.set $bt (call $tag (local.get $b)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 4))
      (then (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (struct.new $native-arguments (call $allocate-object-tag (i32.const 52))
        (i32.const 2) (f64.const 0) (ref.null $digits) (local.get $a) (local.get $b) (ref.null eq) (ref.null eq)))))))))
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
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons (local.get $a) (call $cons (local.get $b) (ref.null eq))))))

(define-runtime-function $native-call3
    (($function externref) ($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-call3")
  (:locals ($id i32) ($index i32) ($array :nullable-object) ($subscript :nullable-object))
  #@(local.set $id (i32.sub (i32.const -2)
    (call $native-builtin-id ,(native-object '(local.get $function)))))
  #@(local.set $array ,(native-object '(local.get $b)))
  #@(local.set $subscript ,(native-object '(local.get $c)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 5))
      (then (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (struct.new $native-arguments (call $allocate-object-tag (i32.const 52))
        (i32.const 3) (f64.const 0) (ref.null $digits) (ref.cast (ref null eq) (any.convert_extern (local.get $a))) (ref.cast (ref null eq) (any.convert_extern (local.get $b))) (ref.cast (ref null eq) (any.convert_extern (local.get $c))) (ref.null eq)))))))))
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
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
      (call $cons ,(native-object '(local.get $c)) (ref.null eq)))))))

(define-runtime-function $native-call4
    (($function externref) ($a externref) ($b externref) ($c externref) ($d externref)) (externref)
  (:export-raw "native-call4")
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 6))
      (then (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (struct.new $native-arguments (call $allocate-object-tag (i32.const 52))
        (i32.const 4) (f64.const 0) (ref.null $digits) (ref.cast (ref null eq) (any.convert_extern (local.get $a))) (ref.cast (ref null eq) (any.convert_extern (local.get $b))) (ref.cast (ref null eq) (any.convert_extern (local.get $c))) (ref.cast (ref null eq) (any.convert_extern (local.get $d)))))))))))
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
  #@(type $native-function (sub $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-entry)) (field $code-size i32))))
  #@(type $native-debug-function (sub final $native-function (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-entry)) (field $code-size i32)
    (field $debug-code (ref eq)) (field $debug-id i32))))
  #@(type $native-arguments (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $d (ref null eq)))))
  #@(table $native-services (export "native-services") 1 funcref)
  ($compiler-image-function)
  ($native-install) ($native-function) ($native-function-with-code) ($native-close-with-arity) ($native-close) ($native-close-fixed) ($native-close-cached) ($native-close-static) ($native-with-arity) ($native-code-size) ($native-capture) ($native-code-storage) ($native-code-entry) ($native-code-bytes) ($native-literal)
  ($native-debug-wrap) ($native-debug-attach) ($native-debug-close) ($native-debug-info)
  ($native-two-arguments) ($native-packed-argument) ($native-list-tail) ($native-argument) ($native-rest-argument) ($native-check-arguments) ($native-check-apply-arguments) ($native-check-apply-list) ($native-environment)
  ($native-restore-environment) ($native-bind)
  ($native-block-enter) ($native-block-environment) ($native-block-return) ($native-block-match) ($native-block-values)
  ($native-control) ($native-restart-signal) ($native-throw)
  ($native-global-read) ($native-global-write) ($native-function-reference)
  ($native-cons) ($native-cell) ($native-primary) ($native-cell-read) ($native-cell-write)
  ($native-call) ($native-value-arguments) ($native-multiple-value-call) ($native-tail-multiple-value-call)
  ($native-return) ($native-return1)
  ($native-int64) ($native-unbox-int64) ($native-add-int64) ($native-sub-int64) ($native-mul-int64)
  ($native-add-int64-checked) ($native-sub-int64-checked) ($native-mul-int64-checked)
  ($native-lt-int64) ($native-gt-int64) ($native-le-int64) ($native-ge-int64) ($native-eq-int64) ($native-ne-int64)
  ($native-vector-ref) ($native-boolean)
  ($native-vector-ref-trusted) ($native-car-list) ($native-cdr-list)
  ($native-vector-ref-trusted64)
  ($native-vector-ref-inline)
  ($native-vector-set-inline)
  ($native-primary-apply) ($native-builtin-id) ($native-poll) ($native-tail-enter) ($native-tail-direct) ($native-tail-call)
  ($native-call0) ($native-call1) ($native-call2) ($native-call3) ($native-call4))
