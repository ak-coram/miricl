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
      (runtime-wat-sequence
        ;; Only the leading factory entries accept the packet ABI. The rest
        ;; of the table also retains typed workers for image serialization.
        (wat-fail-when #@(i32.ge_u (local.get $index)
          (i32.const ,(length (third *native-compiler-image*)))) :malformed-form)
        #@(ref.cast (ref null eq) (any.convert_extern
          (call $native-function (table.get $compiler-entries (local.get $index))
            (extern.convert_any (local.get $literals)) (extern.convert_any (local.get $name)) (i32.const 0)))))
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
  ;; The installer has already set the arity of a checked direct root.
  ;; Preserve that owner's identity. Incompatible copies acquire no grant.
  #@(if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) (local.get $function)))
    (f64.convert_i32_u (i32.add (local.get $arity) (i32.const 2))))
    (then (return (local.get $function))))
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
    (then (if (i32.and (i32.ge_u (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) (local.get $function))) (i32.const 17))
        (i32.le_u (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) (local.get $function))) (i32.const 19)))
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

;; Packet slots start with an inaccessible non-Lisp sentinel. The builder fills
;; each slot once, but a foreign canonical GC array can still alias its storage.
;; Public call ingress snapshots that storage before retaining or executing it.
(define-runtime-function $native-checked-object (($value externref)) (:nullable-object)
  (:locals ($object (ref null any)))
  #@(local.set $object (any.convert_extern (local.get $value)))
  (wat-fail-when #@(i32.eqz (ref.test (ref null eq) (local.get $object))) :malformed-form)
  (wat-fail-when #@(i32.eqz (i32.or (ref.is_null (local.get $object))
    (i32.or (ref.test (ref i31) (local.get $object))
      (i32.or (ref.test (ref $obj) (local.get $object)) (ref.test (ref $objects) (local.get $object)))))) :malformed-form)
  #@(ref.cast (ref null eq) (local.get $object)))

(define-runtime-function $native-checked-value (($value externref)) (:nullable-object)
  (:locals ($object (ref null any)))
  #@(local.set $object (any.convert_extern (local.get $value)))
  (wat-fail-when #@(i32.eqz (i32.or
    (ref.test (ref null i31) (local.get $object))
    (ref.test (ref $obj) (local.get $object)))) :malformed-form)
  #@(ref.cast (ref null eq) (local.get $object)))

(define-runtime-function $native-arguments (($count i32)) (externref)
  (:export-raw "native-arguments")
  (wat-fail-when #@(i32.ge_u (local.get $count) (i32.const 65536)) :malformed-form)
  #@(if (ref.is_null (global.get $native-argument-empty))
    (then (global.set $native-argument-empty (array.new_default $objects (i32.const 0)))))
  #@(extern.convert_any (array.new $objects (global.get $native-argument-empty) (local.get $count))))

(define-runtime-function $native-argument-set
    (($packet externref) ($index i32) ($element externref)) (externref)
  (:export-raw "native-argument-set")
  (:locals ($value (ref null any)) ($item (ref null any)) ($data (ref null $objects)))
  #@(local.set $value (any.convert_extern (local.get $packet)))
  #@(local.set $item (any.convert_extern (local.get $element)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $objects) (local.get $value))) :malformed-form)
  #@(local.set $data (ref.cast (ref $objects) (local.get $value)))
  (wat-fail-when #@(i32.ge_u (local.get $index) (array.len (local.get $data))) :malformed-form)
  (wat-fail-when #@(i32.eqz (i32.or (ref.is_null (local.get $item))
    (i32.or (ref.test (ref i31) (local.get $item)) (ref.test (ref $obj) (local.get $item))))) :malformed-form)
  (wat-fail-when #@(i32.or (ref.is_null (global.get $native-argument-empty))
    (i32.eqz (ref.eq (array.get $objects (local.get $data) (local.get $index))
      (global.get $native-argument-empty)))) :malformed-form)
  #@(array.set $objects (local.get $data) (local.get $index) (ref.cast (ref null eq) (local.get $item)))
  #@(local.get $packet))

(define-runtime-function $native-validate-packet (($args (ref $objects))) ()
  (:locals ($index i32) ($item :nullable-object))
  (wat-fail-when #@(i32.gt_u (array.len (local.get $args)) (i32.const 65536)) :malformed-form)
  #@(block $done (loop $next
    (br_if $done (i32.eq (local.get $index) (array.len (local.get $args))))
    (local.set $item (array.get $objects (local.get $args) (local.get $index)))
    ,(wat-fail-when #@(i32.eqz (i32.or (ref.is_null (local.get $item))
      (i32.or (ref.test (ref i31) (local.get $item)) (ref.test (ref $obj) (local.get $item))))) :malformed-form)
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $next))))

(define-runtime-function $native-ingress-arguments (($args externref)) (externref)
  (:export-raw "native-ingress-arguments")
  (:locals ($value :nullable-object) ($source (ref null $objects))
           ($packet (ref null $objects)) ($item :nullable-object) ($index i32) ($count i32))
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (i32.eqz (ref.test (ref $objects) (local.get $value)))
    (then (return (local.get $args))))
  #@(local.set $source (ref.cast (ref $objects) (local.get $value)))
  #@(local.set $count (array.len (local.get $source)))
  ;; Generated method entries may include one extra hidden context operand.
  ;; Keep the same bounded shape admission as private packet validation.
  (wat-fail-when #@(i32.gt_u (local.get $count) (i32.const 65536)) :malformed-form)
  #@(local.set $packet (array.new_default $objects (local.get $count)))
  ;; No checkpoint, callback or user-visible object precedes completion. Copy
  ;; slots only: referenced Lisp objects retain their ordinary shared identity.
  #@(block $done (loop $copy
    (br_if $done (i32.eq (local.get $index) (local.get $count)))
    (local.set $item (array.get $objects (local.get $source) (local.get $index)))
    ,(wat-fail-when #@(i32.eqz (i32.or (ref.is_null (local.get $item))
      (i32.or (ref.test (ref i31) (local.get $item)) (ref.test (ref $obj) (local.get $item))))) :malformed-form)
    (array.set $objects (local.get $packet) (local.get $index) (local.get $item))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy)))
  #@(extern.convert_any (local.get $packet)))

(define-runtime-function $native-packed-argument (($args (ref $objects)) ($index i32)) (:nullable-object)
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(if (result (ref null eq)) (i32.lt_u (local.get $index) (array.len (local.get $args)))
    (then (array.get $objects (local.get $args) (local.get $index))) (else (ref.null eq))))

;; Representation copies allocate owned packets. No caller spine is modified.
(define-runtime-function $native-prepend-argument (($head :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($source (ref null $objects)) ($data (ref null $objects)) ($index i32) ($count i32))
  #@(local.set $source (ref.cast (ref $objects) (local.get $args)))
  #@(local.set $count (array.len (local.get $source)))
  (wat-fail-when #@(i32.ge_u (local.get $count) (i32.const 65536)) :malformed-form)
  #@(local.set $data (array.new_default $objects (i32.add (local.get $count) (i32.const 1))))
  #@(array.set $objects (local.get $data) (i32.const 0) (local.get $head))
  #@(block $done (loop $copy
    (br_if $done (i32.eq (local.get $index) (local.get $count)))
    (array.set $objects (local.get $data) (i32.add (local.get $index) (i32.const 1))
      (array.get $objects (local.get $source) (local.get $index)))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy)))
  #@(local.get $data))

(define-runtime-function $native-packet-tail (($args :nullable-object) ($start i32)) (:nullable-object)
  (:locals ($source (ref null $objects)) ($data (ref null $objects)) ($index i32) ($count i32))
  #@(if (i32.eqz (ref.test (ref $objects) (local.get $args)))
    (then (call $native-check-private-spine (local.get $args)) (return (call $native-list-tail (local.get $args) (local.get $start)))))
  #@(local.set $source (ref.cast (ref $objects) (local.get $args)))
  #@(local.set $count (array.len (local.get $source)))
  #@(if (i32.gt_u (local.get $start) (local.get $count)) (then (local.set $start (local.get $count))))
  #@(local.set $count (i32.sub (local.get $count) (local.get $start)))
  #@(local.set $data (array.new_default $objects (local.get $count)))
  #@(block $done (loop $copy
    (br_if $done (i32.eq (local.get $index) (local.get $count)))
    (array.set $objects (local.get $data) (local.get $index)
      (array.get $objects (local.get $source) (i32.add (local.get $start) (local.get $index))))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy)))
  #@(local.get $data))

;; Representation adapters used only by sealed portable CL packet kernels.
(define-runtime-function $native-packet-length-cl (($args :nullable-object)) (:nullable-object)
  (:locals ($count :nullable-object))
  #@(local.set $args (call $argument-context-arguments (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $args))
    (then (return (call $int (array.len (ref.cast (ref $objects) (local.get $args)))))))
  ;; The checked walker already supplies the count. Keep its cycle rejection
  ;; here instead of validating and then traversing the same spine again.
  #@(local.set $count (call $list-length-native (local.get $args)))
  #@(if (ref.is_null (local.get $count))
    (then (call $type-fail (local.get $args) (call $symbol-value ,(clos-library-reference '%proper-list-type))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(local.get $count))

(define-runtime-function $native-packet-ref-cl (($args :nullable-object) ($index :nullable-object)) (:nullable-object)
  #@(local.set $args (call $argument-context-arguments (local.get $args)))
  #@(ref.cast (ref null eq) (any.convert_extern
    (call $native-argument (extern.convert_any (local.get $args)) (call $sequence-index (local.get $index))))))

(defun $native-list-tail-cl ()
  (runtime-function-definition '$native-list-tail-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-list-tail-cl 'miricl-runtime:native-list-tail-value))))

(define-runtime-function $native-list-tail (($value :nullable-object) ($index i32)) (:nullable-object)
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(call $native-list-tail-cl (local.get $value) (call $int (local.get $index))))

(defun $native-private-spine-count-cl ()
  (runtime-function-definition '$native-private-spine-count-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-private-spine-count-cl 'miricl-runtime:private-argument-spine-count))))

(define-runtime-function $native-check-private-spine (($value :nullable-object)) ()
  ;; %%APPLY also implements destructuring: unlike public APPLY, a private
  ;; REST binder can return an atomic tail. Validate finiteness independently.
  #@(drop (call $native-private-spine-count-cl (local.get $value) (ref.null eq))))

(defun $native-rest-argument-cl ()
  (runtime-function-definition '$native-rest-argument-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-rest-argument-cl 'miricl-runtime:rest-argument-values))))

(define-runtime-function $native-rest-argument (($args externref) ($index i32)) (externref)
  (:export-raw "native-rest-argument")
  (:locals ($value :nullable-object) ($result :nullable-object) ($count i32) ($data (ref null $objects)))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (local.set $data (ref.cast (ref $objects) (local.get $value)))
      (call $native-validate-packet (ref.as_non_null (local.get $data)))
      (local.set $count (array.len (local.get $data)))
      (block $done (loop $copy
        (br_if $done (i32.le_u (local.get $count) (local.get $index)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1)))
        (local.set $result (call $cons (array.get $objects (local.get $data) (local.get $count)) (local.get $result)))
        (br $copy)))
      (return (extern.convert_any (local.get $result)))))
  #@(call $native-check-private-spine (local.get $value))
  #@(extern.convert_any (call $native-rest-argument-cl (local.get $value) (call $int (local.get $index)))))

(define-runtime-function $native-prepare-dynamic-rest (($args externref) ($index i32)) (externref)
  (:export-raw "native-prepare-dynamic-rest")
  (:locals ($value :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (call $native-validate-packet (ref.cast (ref $objects) (local.get $value)))
      (return (local.get $args))))
  ;; Materialize other representations at the original binding observation,
  ;; preserving private-tail errors and aliases before later body effects.
  #@(return_call $native-rest-argument (local.get $args) (local.get $index)))

(define-runtime-function $native-dynamic-rest-length (($args externref) ($index i32)) (externref)
  (:export-raw "native-dynamic-rest-length")
  (:locals ($value :nullable-object) ($count i32))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (call $native-validate-packet (ref.cast (ref $objects) (local.get $value)))
      (local.set $count (array.len (ref.cast (ref $objects) (local.get $value))))
      (return (extern.convert_any (call $int
        (if (result i32) (i32.le_u (local.get $count) (local.get $index))
          (then (i32.const 0)) (else (i32.sub (local.get $count) (local.get $index)))))))))
  #@(return_call $native-length-inline (local.get $args)))

(define-runtime-function $native-arguments-list (($args :nullable-object)) (:nullable-object)
  #@(ref.cast (ref null eq) (any.convert_extern
    (call $native-rest-argument (extern.convert_any (local.get $args)) (i32.const 0)))))

(define-runtime-function $native-argument-present (($args externref) ($index i32)) (externref)
  (:export-raw "native-argument-present")
  (:locals ($value :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then (return (extern.convert_any (call $bool
      (i32.lt_u (local.get $index) (array.len (ref.cast (ref $objects) (local.get $value)))))))))
  #@(call $native-check-private-spine (local.get $value))
  #@(extern.convert_any (call $bool (i32.eqz (ref.is_null (call $native-list-tail (local.get $value) (local.get $index)))))))

(defun $native-argument-cl ()
  (runtime-function-definition '$native-argument-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-argument-cl 'miricl-runtime:argument-value))))

(define-runtime-function $native-argument (($args externref) ($index i32)) (externref)
  (:export-raw "native-argument")
  (:locals ($value :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $index) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    ;; Read a raw packet without allocating a Lisp vector or boxing the index.
    (then
      (if (i32.ge_u (local.get $index) (array.len (ref.cast (ref $objects) (local.get $value))))
        (then (return (ref.null extern))))
      (local.set $value (array.get $objects (ref.cast (ref $objects) (local.get $value)) (local.get $index))))
    (else
      (call $native-check-private-spine (local.get $value))
      (local.set $value (call $native-argument-cl (local.get $value) (call $int (local.get $index))))))
  ;; The container's shape does not establish the shape of its slots.
  (wat-fail-when #@(i32.eqz (i32.or (ref.test (ref null i31) (local.get $value))
    (ref.test (ref $obj) (local.get $value)))) :malformed-form)
  #@(extern.convert_any (local.get $value)))

(defun $native-check-arguments-cl ()
  (runtime-function-definition '$native-check-arguments-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-check-arguments-cl 'miricl-runtime:private-arguments-valid-p :result :truth))))

(define-runtime-function $native-check-arguments (($args externref) ($count i32) ($maximum i32)) ()
  (:export-raw "native-check-arguments")
  (:locals ($value (ref null any)) ($data (ref null $objects)) ($size i32))
  #@(local.set $value (call $native-checked-object (local.get $args)))
  (wat-fail-when #@(i32.or (i32.lt_s (local.get $count) (i32.const 0))
    (i32.and (i32.ge_s (local.get $maximum) (i32.const 0))
      (i32.lt_s (local.get $maximum) (local.get $count)))) :malformed-form)
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (local.set $data (ref.cast (ref $objects) (local.get $value)))
      (call $native-validate-packet (ref.as_non_null (local.get $data)))
      (local.set $size (array.len (local.get $data)))
      ,(wat-fail-when #@(i32.or (i32.lt_u (local.get $size) (local.get $count))
        (i32.and (i32.ge_s (local.get $maximum) (i32.const 0))
          (i32.gt_u (local.get $size) (local.get $maximum)))) :wrong-arity)
      (return)))
  (wat-fail-when #@(i32.eqz (ref.test (ref null eq) (local.get $value))) :malformed-form)
  (wat-fail-when #@(i32.eqz (call $native-check-arguments-cl
    (ref.cast (ref null eq) (local.get $value)) (call $int (local.get $count)) (call $int (local.get $maximum)))) :wrong-arity))

(defun $native-keyword-find-cl ()
  (runtime-function-definition '$native-keyword-find-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-keyword-find-cl 'miricl-runtime:packet-keyword-index))))
(defun $native-check-keywords-cl ()
  (runtime-function-definition '$native-check-keywords-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-check-keywords-cl 'miricl-runtime:check-packet-keywords))))

(define-runtime-function $native-keyword-find (($args externref) ($start i32) ($key externref)) (:nullable-object)
  (:locals ($value :nullable-object) ($name :nullable-object) ($packet (ref null $objects))
           ($count i32) ($index i32))
  (wat-fail-when #@(i32.lt_s (local.get $start) (i32.const 0)) :malformed-form)
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(local.set $name (call $native-checked-value (local.get $key)))
  (wat-fail-when #@(i32.eqz (call $symbolp (local.get $name))) :malformed-form)
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (local.set $packet (ref.cast (ref $objects) (local.get $value)))
      (local.set $count (array.len (local.get $packet)))
      ,(wat-fail-when #@(i32.gt_u (local.get $count) (i32.const 65536)) :malformed-form)
      (local.set $index (local.get $start))
      (block $done (loop $pairs
        ;; Defaults precede final keyword validation. Ignore an incomplete pair.
        (br_if $done (i32.ge_u (i32.add (local.get $index) (i32.const 1)) (local.get $count)))
        (local.set $value (call $native-checked-value
          (extern.convert_any (array.get $objects (local.get $packet) (local.get $index)))))
        (if (ref.eq (local.get $value) (local.get $name))
          (then (return (call $int (i32.add (local.get $index) (i32.const 1))))))
        (local.set $index (i32.add (local.get $index) (i32.const 2))) (br $pairs)))
      (return (ref.null eq))))
  #@(call $native-keyword-find-cl (local.get $value) (call $int (local.get $start)) (local.get $name)))

(define-runtime-function $native-keyword-present (($args externref) ($start i32) ($key externref)) (externref)
  (:export-raw "native-keyword-present")
  #@(extern.convert_any (call $bool (i32.eqz (ref.is_null
    (call $native-keyword-find (local.get $args) (local.get $start) (local.get $key)))))))

(define-runtime-function $native-keyword-argument (($args externref) ($start i32) ($key externref)) (externref)
  (:export-raw "native-keyword-argument")
  (:locals ($index :nullable-object))
  #@(local.set $index (call $native-keyword-find (local.get $args) (local.get $start) (local.get $key)))
  #@(if (result externref) (ref.is_null (local.get $index)) (then (ref.null extern))
    (else (call $native-argument (local.get $args) (call $natural-count (local.get $index))))))

(define-runtime-function $native-check-keywords (($args externref) ($start i32) ($keys externref) ($allow i32)) ()
  (:export-raw "native-check-keywords")
  (:locals ($names :nullable-object) ($name :nullable-object) ($value :nullable-object)
           ($packet (ref null $objects)) ($count i32))
  (wat-fail-when #@(i32.lt_s (local.get $start) (i32.const 0)) :malformed-form)
  #@(call $check-apply-list (call $native-checked-value (local.get $keys)))
  #@(local.set $names (call $native-checked-value (local.get $keys)))
  #@(block $done (loop $keys
    (br_if $done (ref.is_null (local.get $names)))
    (local.set $name (call $car (local.get $names)))
    ,(wat-fail-when #@(i32.eqz (call $symbolp (local.get $name))) :malformed-form)
    (local.set $names (call $cdr (local.get $names))) (br $keys)))
  #@(local.set $value (call $native-checked-object (local.get $args)))
  #@(if (ref.test (ref $objects) (local.get $value))
    (then
      (local.set $packet (ref.cast (ref $objects) (local.get $value)))
      (local.set $count (array.len (local.get $packet)))
      ,(wat-fail-when #@(i32.gt_u (local.get $count) (i32.const 65536)) :malformed-form)
      (if (i32.ge_u (local.get $start) (local.get $count)) (then (return)))
      (if (i32.eq (i32.sub (local.get $count) (local.get $start)) (i32.const 2))
        (then
          (local.set $name (call $native-checked-value
            (extern.convert_any (array.get $objects (local.get $packet) (local.get $start)))))
          (drop (call $native-checked-value (extern.convert_any
            (array.get $objects (local.get $packet) (i32.add (local.get $start) (i32.const 1))))))
          (if (call $keyword-single-pair-valid-p (local.get $name)
                (call $native-checked-value (local.get $keys)) (local.get $allow))
            (then (return)))))))
  #@(drop (call $native-check-keywords-cl (local.get $value)
    (call $int (local.get $start)) (call $native-checked-value (local.get $keys)) (call $bool (local.get $allow)))))

(define-runtime-function $native-check-apply-arguments (($args externref) ($count i32)) ()
  (:export-raw "native-check-apply-arguments")
  #@(drop (call $check-apply-arguments (call $native-checked-value (local.get $args)) (local.get $count))))

(define-runtime-function $native-check-apply-list (($args externref)) ()
  (:export-raw "native-check-apply-list")
  #@(call $check-apply-list (call $native-checked-value (local.get $args))))

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

(define-runtime-function $native-global-read (($name externref)) (externref)
  (:export-raw "native-global-read")
  #@(extern.convert_any (call $symbol-value ,(native-object '(local.get $name)))))

(define-runtime-function $native-global-write
    (($name externref) ($value externref)) (externref)
  (:export-raw "native-global-write")
  #@(extern.convert_any (call $set-symbol-value ,(native-object '(local.get $name)) ,(native-object '(local.get $value)))))

(define-runtime-function $native-function-reference (($name externref)) (externref)
  (:export-raw "native-function-reference")
  (:locals ($object (ref null any)))
  #@(local.set $object (any.convert_extern (local.get $name)))
  (wat-fail-when #@(i32.eqz (ref.test (ref null eq) (local.get $object))) :malformed-form)
  #@(extern.convert_any (call $fdefinition (ref.cast (ref null eq) (local.get $object)))))

(define-runtime-function $native-standard-function-reference (($name externref)) (externref)
  (:export-raw "native-standard-function-reference")
  #@(extern.convert_any (call $standard-function-reference
    (call $native-checked-value (local.get $name)))))

(define-runtime-function $native-literal-function (($function externref) ($index i32)) (externref)
  (:export-raw "native-literal-function")
  (:locals ($owner (ref null any)) ($pool :nullable-object) ($data (ref $objects)))
  #@(local.set $owner (any.convert_extern (local.get $function)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) (local.get $owner))) :malformed-form)
  #@(local.set $pool (struct.get $obj $a (ref.cast (ref $native-function) (local.get $owner))))
  (wat-fail-when #@(i32.eqz (ref.test (ref $vector) (local.get $pool))) :malformed-form)
  #@(local.set $data (struct.get $vector $data (ref.cast (ref $vector) (local.get $pool))))
  (wat-fail-when #@(i32.ge_u (local.get $index) (array.len (local.get $data))) :malformed-form)
  #@(extern.convert_any (call $fdefinition (array.get $objects (local.get $data) (local.get $index)))))

(define-runtime-function $native-entry-valid (($entry funcref) ($direct i32)) (i32)
  (:export-raw "native-entry-valid")
  #@(if (result i32) (local.get $direct)
    (then (ref.test (ref $native-direct-entry) (local.get $entry)))
    (else (ref.test (ref $native-entry) (local.get $entry)))))

(define-runtime-function $native-module-error (($message externref)) (externref)
  (:export-raw "native-module-error")
  #@(drop (call $apply
    (call $canonical-builtin-function (i32.const ,(runtime-symbol-id 'error)) (i32.const 0))
    (call $cons ,(wat-text-literal "~A")
      (call $cons ,(native-object '(local.get $message)) (ref.null eq)))))
  #@(unreachable))

(define-runtime-function $native-cons (($a externref) ($b externref)) (externref)
  (:export-raw "native-cons")
  #@(extern.convert_any (call $cons ,(native-object '(local.get $a)) ,(native-object '(local.get $b)))))

(define-runtime-function $native-cons2 (($a externref) ($b externref) ($tail externref)) (externref)
  (:export-raw "native-cons2")
  #@(extern.convert_any (call $cons2 ,(native-object '(local.get $a))
    ,(native-object '(local.get $b)) ,(native-object '(local.get $tail)))))

(define-runtime-function $native-list2 (($a externref) ($b externref)) (externref)
  (:export-raw "native-list2")
  #@(extern.convert_any (call $cons2 ,(native-object '(local.get $a))
    ,(native-object '(local.get $b)) (ref.null eq))))

(define-runtime-function $native-cell (($value externref)) (externref)
  (:export-raw "native-cell")
  #@(extern.convert_any (call $cons ,(native-object '(local.get $value)) (ref.null eq))))

(define-runtime-function $native-primary (($values externref)) (externref)
  (:export-raw "native-primary")
  ;; Value packets, boxed cells and checked CAR calls share this checked read.
  #@(extern.convert_any (call $car ,(native-object '(local.get $values)))))

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
  (wat-fail-when #@(i32.ne (call $tag (local.get $target)) (i32.const 21)) :control-error)
  ;; Source TAGBODY entries retain the resume spine and their shared owner.
  ;; HIR closures can capture these tokens while their enclosing source frame
  ;; is still active; use its ordinary transfer packet and cleanup machinery.
  #@(if (i32.eq (call $value-n (local.get $target)) (i32.const 1))
    (then
      ,(wat-fail-when #@(i32.eqz (call $member-eq
        (struct.get $obj $c (ref.cast (ref $obj) (local.get $target))) (global.get $targets))) :control-error)
      (call $transfer-to (struct.get $obj $c (ref.cast (ref $obj) (local.get $target))) (local.get $target))
      (unreachable)))
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
    (call $native-checked-value (local.get $function))
    ,(native-object '(call $native-ingress-arguments (local.get $args))))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $values)))

(define-runtime-function $native-primary-call
    (($environment externref) ($function externref) ($args externref)) (externref)
  (:export-raw "native-primary-call")
  ;; Keep ordinary application checks and frames, but discard secondary values
  ;; without constructing a result list for a primary-only consumer.
  #@(extern.convert_any (call $native-primary-apply
    (call $native-checked-value (local.get $function))
    ,(native-object '(call $native-ingress-arguments (local.get $args))))))

(defun native-primary-call5-packet-wat ()
  (wat-fragment #@(array.new_fixed $objects 5
    (local.get $a) (local.get $b) (local.get $c) (local.get $d) (local.get $e))))

(define-runtime-function $native-primary-call5-ready
    (($name :nullable-object) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object) ($e :nullable-object)) (externref)
  (:locals ($result :nullable-object))
  #@(local.set $result
    (call $apply0-ready (local.get $name) ,(native-primary-call5-packet-wat)))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-primary-call5
    (($function externref) ($first externref) ($second externref) ($third externref)
     ($fourth externref) ($fifth externref)) (externref)
  (:export-raw "native-primary-call5")
  (:locals ($name :nullable-object) ($a :nullable-object) ($b :nullable-object)
           ($c :nullable-object) ($d :nullable-object) ($e :nullable-object))
  #@(local.set $name (call $native-checked-value (local.get $function)))
  #@(local.set $a (call $native-checked-value (local.get $first)))
  #@(local.set $b (call $native-checked-value (local.get $second)))
  #@(local.set $c (call $native-checked-value (local.get $third)))
  #@(local.set $d (call $native-checked-value (local.get $fourth)))
  #@(local.set $e (call $native-checked-value (local.get $fifth)))
  ;; Match APPLY0's entry decision before the checkpoint. The debugging path
  ;; manages checkpoint suppression and restoration inside ordinary application.
  #@(if (global.get $debug-evaluating)
    (then (return (extern.convert_any
      (call $native-primary-apply (local.get $name) ,(native-primary-call5-packet-wat))))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(return_call $native-primary-call5-ready
    (local.get $name) (local.get $a) (local.get $b) (local.get $c) (local.get $d) (local.get $e)))

(define-runtime-function $native-return (($values externref)) (externref)
  (:export-raw "native-return")
  #@(extern.convert_any (call $return-values ,(native-object '(local.get $values)))))

(define-runtime-function $native-return1 (($value externref)) (externref)
  (:export-raw "native-return1")
  #@(call $clear-values)
  #@(local.get $value))

(define-runtime-function $native-return2 (($a externref) ($b externref)) (externref)
  (:export-raw "native-return2")
  #@(extern.convert_any (call $two-values
    ,(native-object '(local.get $a)) ,(native-object '(local.get $b)))))

(define-runtime-function $native-return3 (($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-return3")
  #@(extern.convert_any (call $three-values
    ,(native-object '(local.get $a)) ,(native-object '(local.get $b))
    ,(native-object '(local.get $c)))))

(define-runtime-function $native-return-counted8
    (($a externref) ($b externref) ($c externref) ($d externref)
     ($e externref) ($f externref) ($g externref) ($h externref) ($count i32)) (externref)
  (:export-raw "native-return-counted8")
  (:locals ($first :nullable-object) ($second :nullable-object) ($third :nullable-object)
           ($payload (ref $secondary-values)))
  ;; Check every externally supplied slot before publishing anything, even
  ;; padding. Allocation and publication have no intervening callback.
  (wat-fail-when #@(i32.ge_u (i32.sub (local.get $count) (i32.const 4)) (i32.const 5)) :malformed-form)
  #@(local.set $first (call $native-checked-value (local.get $a)))
  #@(local.set $second (call $native-checked-value (local.get $b)))
  #@(local.set $third (call $native-checked-value (local.get $c)))
  #@(local.set $payload (array.new_fixed $secondary-values 7
    (local.get $second) (local.get $third)
    (call $native-checked-value (local.get $d)) (call $native-checked-value (local.get $e))
    (call $native-checked-value (local.get $f)) (call $native-checked-value (local.get $g))
    (call $native-checked-value (local.get $h))))
  #@(call $restore-values (local.get $second) (local.get $third)
    (local.get $payload) (local.get $count))
  #@(extern.convert_any (local.get $first)))

(define-runtime-function $native-check-result-spine (($list :nullable-object) ($count i32)) ()
  (wat-fail-when #@(i64.ne
    (call $scalar-integer-value (call $checked-list-length (local.get $list) (i32.const 1)))
    (i64.extend_i32_u (i32.sub (local.get $count) (i32.const 1)))) :malformed-form))

(define-runtime-function $native-captured-result-ref
    (($payload externref) ($index i32) ($count i32)) (externref)
  (:export-raw "native-captured-result-ref")
  (:locals ($value (ref null any)) ($array (ref $secondary-values)) ($list :nullable-object))
  ;; INDEX counts the primary value. Reserve the first three result slots for
  ;; caller locals. Bounds and admission are independent of array identity.
  (wat-fail-when #@(i32.or (i32.lt_u (local.get $index) (i32.const 3))
    (i32.lt_s (local.get $count) (i32.const 4))) :malformed-form)
  #@(local.set $value (any.convert_extern (local.get $payload)))
  #@(if (ref.test (ref $secondary-values) (local.get $value))
    (then
      (local.set $array (ref.cast (ref $secondary-values) (local.get $value)))
      ,(wat-fail-when #@(i32.or
        (i32.ge_u (i32.sub (local.get $count) (i32.const 4)) (i32.const 5))
        (i32.ne (array.len (local.get $array)) (i32.const 7))) :malformed-form)
      (if (i32.ge_u (local.get $index) (local.get $count)) (then (return (ref.null extern))))
      (return (extern.convert_any (call $native-checked-value
        (extern.convert_any (array.get $secondary-values (local.get $array)
          (i32.sub (local.get $index) (i32.const 1)))))))))
  #@(local.set $list (call $native-checked-value (local.get $payload)))
  #@(call $native-check-result-spine (local.get $list) (local.get $count))
  #@(if (i32.ge_u (local.get $index) (local.get $count)) (then (return (ref.null extern))))
  ;; A long projection can service interrupts. Recheck each observed cell
  ;; and the selected value after any callback can have edited a shared spine.
  #@(loop $next
    ,(wat-fail-when #@(i32.eqz (call $checked-consp (local.get $list))) :malformed-form)
    (if (i32.eq (local.get $index) (i32.const 1))
      (then (return (extern.convert_any (call $native-checked-value
        (extern.convert_any (struct.get $obj $a (ref.cast (ref $obj) (local.get $list)))))))))
    (local.set $list (struct.get $obj $b (ref.cast (ref $obj) (local.get $list))))
    (local.set $index (i32.sub (local.get $index) (i32.const 1)))
    (if (i32.gt_u (local.get $count) (i32.const 8)) (then (call $native-poll)))
    (br $next))
  #@(unreachable))

(define-runtime-function $native-captured-extra-list
    (($payload externref) ($count i32)) (externref)
  (:export-raw "native-captured-extra-list")
  (:locals ($value (ref null any)) ($list :nullable-object))
  (wat-fail-when #@(i32.lt_s (local.get $count) (i32.const 4)) :malformed-form)
  #@(local.set $value (any.convert_extern (local.get $payload)))
  #@(if (ref.test (ref $secondary-values) (local.get $value))
    (then (return (extern.convert_any (call $secondary-values-list
      (ref.cast (ref $secondary-values) (local.get $value)) (local.get $count))))))
  #@(local.set $list (call $native-checked-value (local.get $payload)))
  #@(call $native-check-result-spine (local.get $list) (local.get $count))
  #@(extern.convert_any (local.get $list)))

(define-runtime-function $native-int64 (($value i64)) (externref)
  (:export-raw "native-int64")
  #@(extern.convert_any (call $box-int64 (local.get $value))))

(define-runtime-function $native-unbox-int64 (($value externref)) (i64)
  (:export-raw "native-unbox-int64")
  #@(call $scalar-integer-value (ref.as_non_null ,(native-object '(local.get $value)))))

(define-runtime-function $native-check-int64 (($value externref)) (externref)
  (:export-raw "native-check-int64")
  (:locals ($object :nullable-object))
  #@(local.set $object (call $native-checked-value (local.get $value)))
  #@(if (i32.eqz (call $scalar-integer-p (local.get $object)))
    (then (call $list-type-error (local.get $object) (i32.const ,(runtime-symbol-id 'fixnum)))))
  #@(local.get $value))

(defun native-object-integer-entry-wat (worker)
  (runtime-wat-sequence
     ;; Keep actual immediate operands immediate without constructing boxes
     ;; merely to call the raw-i64 ABI. This entry adds no poll or values reset.
     #@(if (i32.and (ref.test (ref i31) (any.convert_extern (local.get $left)))
                    (ref.test (ref i31) (any.convert_extern (local.get $right))))
       (then (return (extern.convert_any (call ,worker
         (ref.as_non_null ,(native-object '(local.get $left)))
         (ref.as_non_null ,(native-object '(local.get $right))))))))
     #@(local.set $a (call $native-checked-value (local.get $left)))
     #@(if (i32.eqz (call $scalar-integer-p (local.get $a)))
       (then (call $list-type-error (local.get $a) (i32.const ,(runtime-symbol-id 'fixnum)))))
     #@(local.set $b (call $native-checked-value (local.get $right)))
     #@(if (i32.eqz (call $scalar-integer-p (local.get $b)))
       (then (call $list-type-error (local.get $b) (i32.const ,(runtime-symbol-id 'fixnum)))))
     #@(extern.convert_any (call ,worker (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))))

(defmacro define-native-object-integer-entry (name export worker)
  `(define-runtime-function ,name (($left externref) ($right externref)) (externref)
     (:export-raw ,export)
     (:locals ($a :nullable-object) ($b :nullable-object))
     (native-object-integer-entry-wat ',worker)))

(define-native-object-integer-entry $native-add-integer "native-add-integer" $iadd)
(define-native-object-integer-entry $native-sub-integer "native-sub-integer" $isub)
(define-native-object-integer-entry $native-mul-integer "native-mul-integer" $imul)

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
  ;; These scalar entries are selected only for exact standard CL operations.
  ;; Overflow uses the same checked standard arithmetic worker directly.
  (if checked
    (runtime-wat-sequence
      #@(block $overflow
        (local.set $computed (,opcode (local.get $left) (local.get $right)))
        (br_if $overflow (i32.eqz ,(native-integer-fit-wat function)))
        (return (extern.convert_any (call ,result (local.get $computed)))))
      #@(extern.convert_any (call $op (i32.const ,(ecase function (+ 0) (- 1) (* 2)))
        (call $box-int64 (local.get $left)) (call $box-int64 (local.get $right)))))
    #@(extern.convert_any (call ,result (,opcode (local.get $left) (local.get $right))))))

(defmacro define-native-integer-entry (name export function opcode result &optional checked)
  `(define-runtime-function ,name (($left i64) ($right i64)) (externref)
     (:export-raw ,export)
     ,@(when checked '((:locals ($computed i64))))
     (native-integer-entry-wat ',function ',opcode ',result ,checked)))

(define-native-integer-entry $native-add-int64 "native-add-int64" + i64.add $box-int64)
(define-native-integer-entry $native-sub-int64 "native-sub-int64" - i64.sub $box-int64)
(define-native-integer-entry $native-mul-int64 "native-mul-int64" * i64.mul $box-int64)
(define-native-integer-entry $native-logand-int64 "native-logand-int64" logand i64.and $int64)
(define-native-integer-entry $native-logior-int64 "native-logior-int64" logior i64.or $int64)
(define-native-integer-entry $native-logxor-int64 "native-logxor-int64" logxor i64.xor $int64)

(define-runtime-function $native-ash-int64
    (($left i64) ($right i64)) (externref)
  (:export-raw "native-ash-int64")
  (:locals ($computed i64))
  #@(if (i64.lt_s (local.get $right) (i64.const 0))
    (then
      ;; Wasm masks shift counts; Lisp saturates an arithmetic right
      ;; shift once every value bit has been shifted out.
      (return (extern.convert_any (call $int64
        (if (result i64) (i64.le_s (local.get $right) (i64.const -64))
          (then (select (i64.const -1) (i64.const 0) (i64.lt_s (local.get $left) (i64.const 0))))
          (else (i64.shr_s (local.get $left) (i64.sub (i64.const 0) (local.get $right))))))))))
  #@(if (i64.eqz (local.get $left)) (then (return (extern.convert_any (call $int (i32.const 0))))))
  #@(if (i64.lt_u (local.get $right) (i64.const 64))
    (then
      (local.set $computed (i64.shl (local.get $left) (local.get $right)))
      (if (i64.eq (i64.shr_s (local.get $computed) (local.get $right)) (local.get $left))
        (then (return (extern.convert_any (call $int64 (local.get $computed))))))))
  ;; Positive shifts that overflow i64 use arbitrary-precision arithmetic.
  #@(extern.convert_any (call $ash-integer-digits
    (call $int64 (local.get $left)) (call $int64 (local.get $right)))))
(define-native-integer-entry $native-add-int64-checked "native-add-int64-checked" + i64.add $box-int64 t)
(define-native-integer-entry $native-sub-int64-checked "native-sub-int64-checked" - i64.sub $box-int64 t)
(define-native-integer-entry $native-mul-int64-checked "native-mul-int64-checked" * i64.mul $box-int64 t)
(define-native-integer-entry $native-lt-int64 "native-lt-int64" < i64.lt_s $bool)
(define-native-integer-entry $native-gt-int64 "native-gt-int64" > i64.gt_s $bool)
(define-native-integer-entry $native-le-int64 "native-le-int64" <= i64.le_s $bool)
(define-native-integer-entry $native-ge-int64 "native-ge-int64" >= i64.ge_s $bool)
(define-native-integer-entry $native-eq-int64 "native-eq-int64" = i64.eq $bool)
(define-native-integer-entry $native-ne-int64 "native-ne-int64" /= i64.ne $bool)

(define-runtime-function $native-vector-ref
    (($vector externref) ($index i32)) (externref)
  (:export-raw "native-vector-ref")
  (:locals ($data (ref $objects)))
  ;; A declaration proves the vector representation and immediate index.
  ;; Bounds errors retain the ordinary Lisp condition and offending datum.
  #@(local.set $data (call $vector-data ,(native-object '(local.get $vector))))
  #@(if (i32.ge_u (local.get $index) (array.len (local.get $data)))
    (then (return (extern.convert_any (call $vector-element ,(native-object '(local.get $vector))
      (call $int (local.get $index)) (ref.null eq) (i32.const 0))))))
  #@(extern.convert_any (array.get $objects (local.get $data) (local.get $index))))

(define-runtime-function $native-vector-ref-trusted
    (($vector externref) ($index i32)) (externref)
  (:export-raw "native-vector-ref-trusted")
  ;; SAFETY 0: declarations and the access preconditions are trusted. WasmGC
  ;; casts and intrinsic array bounds still trap rather than corrupt memory.
  #@(extern.convert_any (array.get $objects
    (call $vector-data ,(native-object '(local.get $vector))) (local.get $index))))

(define-runtime-function $native-vector-ref-inline (($vector externref) ($index i64)) (externref)
  (:export-raw "native-vector-ref-inline")
  ;; Both SAFETY 0 vector-read paths share the scalar worker. The compiler
  ;; proves the representation and canonical standard operation without
  ;; NOTINLINE; access preconditions are trusted. Wasm wraps the wide index
  ;; to its unsigned i32 array index and retains intrinsic cast/bounds traps.
  #@(extern.convert_any (array.get $objects
    (call $vector-data ,(native-object '(local.get $vector))) (i32.wrap_i64 (local.get $index)))))

(define-runtime-function $checked-sbit-ref
    (($array :nullable-object) ($index :nullable-object)) (:nullable-object)
  (:locals ($position i32))
  ;; A flat simple bit-vector has a packed backing array and rank one. Keep
  ;; the complete slow boundary for multidimensional/wrong-type/large indices.
  #@(if (i32.and (i32.eq (call $tag (local.get $array)) (i32.const 36))
            (ref.test (ref i31) (local.get $index)))
    (then
      (local.set $position (i31.get_s (ref.cast (ref i31) (local.get $index))))
      (if (i32.lt_u (local.get $position) (call $value-n (local.get $array)))
        (then (return (call $array-raw-element (local.get $array) (local.get $position)
          (ref.null eq) (i32.const 0)))))))
  #@(call $array-bit-check (local.get $array) (i32.const 1))
  (wat-fail-when #@(i32.ne (call $array-rank-value (local.get $array)) (i32.const 1)) :wrong-arity)
  #@(call $array-element (local.get $array) (local.get $index) (ref.null eq) (i32.const 0)))

(define-runtime-function $native-sbit-ref-inline (($array externref) ($index externref)) (externref)
  (:export-raw "native-sbit-ref-inline")
  #@(extern.convert_any (call $checked-sbit-ref ,(native-object '(local.get $array))
    ,(native-object '(local.get $index)))))

(define-runtime-function $native-row-major-ref-inline (($array externref) ($index externref)) (externref)
  (:export-raw "native-row-major-ref-inline")
  ;; ARRAY-ELEMENT checks the original array and its physical capacity, then
  ;; resolves displacement live. Fill-pointer changes do not narrow storage.
  #@(extern.convert_any (call $array-element ,(native-object '(local.get $array))
    ,(native-object '(local.get $index)) (ref.null eq) (i32.const 0))))

(define-runtime-function $checked-row-major-set
    (($value :nullable-object) ($array :nullable-object) ($index :nullable-object)) (:nullable-object)
  ;; Retain the checked array/index boundary and live displacement resolution.
  ;; The ordinary representation store validates specialized element values
  ;; before writing, and returns exactly the supplied value.
  #@(call $array-element (local.get $array) (local.get $index)
    (local.get $value) (i32.const 1)))

(define-runtime-function $native-row-major-set-inline
    (($value externref) ($array externref) ($index externref)) (externref)
  (:export-raw "native-row-major-set-inline")
  #@(extern.convert_any (call $checked-row-major-set
    ,(native-object '(local.get $value)) ,(native-object '(local.get $array))
    ,(native-object '(local.get $index)))))

(define-runtime-function $native-character-code-inline (($character externref)) (externref)
  (:export-raw "native-character-code-inline")
  #@(if (i32.eq (call $tag ,(native-object '(local.get $character))) (i32.const 16))
    (then (return (extern.convert_any (call $int (call $value-n
      ,(native-object '(local.get $character))))))))
  #@(extern.convert_any (call $int (call $character-code ,(native-object '(local.get $character))))))

(define-runtime-function $native-character-ref-inline (($string externref) ($index externref)) (externref)
  (:export-raw "native-character-ref-inline")
  (:locals ($position i32) ($text :nullable-object) ($subscript :nullable-object) ($data (ref null $digits)))
  ;; A bounded flat-string read is a representation operation. Displaced,
  ;; adjustable and fill-pointer strings use the ordinary array implementation;
  ;; CHAR must use the capacity rather than the fill pointer.
  #@(local.set $text (call $native-checked-value (local.get $string)))
  #@(local.set $subscript (call $native-checked-value (local.get $index)))
  #@(if (i32.and (i32.eq (call $tag (local.get $text)) (i32.const 10))
            (ref.test (ref i31) (local.get $subscript)))
        (then
          (local.set $position (i31.get_s (ref.cast (ref i31) (local.get $subscript))))
          (local.set $data (struct.get $obj $digits (ref.cast (ref $obj) (local.get $text))))
          ,(wat-fail-when #@(ref.is_null (local.get $data)) :malformed-form)
          (if (i32.lt_u (local.get $position) (array.len (local.get $data)))
            (then (return (extern.convert_any
              (call $node (i32.const 16)
                (call $array-checked-character-code (array.get $digits (local.get $data) (local.get $position)))
                (ref.null eq) (ref.null eq) (ref.null eq))))))))
  #@(call $native-check-aref-miss (local.get $text) (local.get $subscript))
  #@(extern.convert_any (call $character-element (local.get $text)
    (local.get $subscript) (ref.null eq) (i32.const 0))))

(define-runtime-function $native-make-character-vector-inline (($size externref)) (externref)
  (:export-raw "native-make-character-vector-inline")
  (:locals ($n i32))
  #@(if (ref.test (ref i31) ,(native-object '(local.get $size)))
    (then
      (local.set $n (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $size)))))
      (if (i32.le_u (local.get $n) (i32.const 536870910))
        (then (return (extern.convert_any (call $text
          (array.new_default $digits (local.get $n)))))))))
  #@(extern.convert_any (call $make-string-native ,(native-object '(local.get $size))
    (call $node (i32.const 16) (i32.const 0) (ref.null eq) (ref.null eq) (ref.null eq)) (i32.const 0))))

(define-runtime-function $native-make-string-inline (($size externref)) (externref)
  (:export-raw "native-make-string-inline")
  ;; MIRICL's chosen MAKE-STRING default is #\Space. Unlike uninitialized
  ;; MAKE-ARRAY storage, these characters are observable immediately.
  #@(extern.convert_any (call $make-string-native ,(native-object '(local.get $size))
    (call $node (i32.const 16) (i32.const 32) (ref.null eq) (ref.null eq) (ref.null eq)) (i32.const 0))))

(define-runtime-function $native-make-vector-inline (($size externref) ($initial externref)) (externref)
  (:export-raw "native-make-vector-inline")
  ;; The default MAKE-ARRAY element type and absent layout keywords produce a
  ;; simple general vector. Share the ordinary checked allocation primitive.
  #@(extern.convert_any (call $make-vector-native
    ,(native-object '(local.get $size)) ,(native-object '(local.get $initial)))))

(define-runtime-function $native-code-character-inline (($code externref)) (externref)
  (:export-raw "native-code-character-inline")
  (:locals ($n i32))
  #@(block $validated
    (if (ref.test (ref i31) ,(native-object '(local.get $code)))
      (then
        (local.set $n (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $code)))))
        (br_if $validated (i32.le_u (local.get $n) (i32.const 1114111)))))
    (local.set $n (call $bounded-index ,(native-object '(local.get $code)) (i32.const 1114111))))
  #@(if (i32.and (i32.ge_u (local.get $n) (i32.const 55296))
                 (i32.le_u (local.get $n) (i32.const 57343)))
    (then (return (ref.null extern))))
  #@(extern.convert_any (call $node (i32.const 16) (local.get $n)
    (ref.null eq) (ref.null eq) (ref.null eq))))

(define-runtime-function $native-simple-character-set-inline
    (($value externref) ($string externref) ($index i64)) (externref)
  (:export-raw "native-simple-character-set-inline")
  (:locals ($data (ref null $digits)))
  #@(local.set $data (struct.get $obj $digits
    (ref.cast (ref $obj) ,(native-object '(local.get $string)))))
  ;; The proof removes representation checks, while the unsigned comparison
  ;; keeps negative, large and out-of-bounds indexes on the Lisp error path.
  ;; Empty NIL-element strings are simple strings with no character backing.
  #@(if (i32.eqz (ref.is_null (local.get $data)))
    (then
      (if (i64.lt_u (local.get $index) (i64.extend_i32_u (array.len (ref.as_non_null (local.get $data)))))
        (then
          (array.set $digits (ref.as_non_null (local.get $data)) (i32.wrap_i64 (local.get $index))
            (struct.get $obj $n (ref.cast (ref $obj) ,(native-object '(local.get $value)))))
          (return (local.get $value))))))
  #@(extern.convert_any (call $character-element ,(native-object '(local.get $string))
    (call $int64 (local.get $index)) ,(native-object '(local.get $value)) (i32.const 1))))

(define-runtime-function $native-character-set-inline
    (($value externref) ($string externref) ($index externref)) (externref)
  (:export-raw "native-character-set-inline")
  (:locals ($text :nullable-object) ($data (ref $digits)) ($position i32))
  #@(local.set $text ,(native-object '(local.get $string)))
  #@(if (i32.and (i32.eq (call $tag (local.get $text)) (i32.const 10))
                 (ref.test (ref i31) ,(native-object '(local.get $index))))
    (then
      (local.set $position (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $index)))))
      (local.set $data (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $text)))))
      (if (i32.lt_u (local.get $position) (array.len (local.get $data)))
        (then
          (array.set $digits (local.get $data) (local.get $position)
            (if (result i32) (i32.eq (call $tag ,(native-object '(local.get $value))) (i32.const 16))
              (then (call $value-n ,(native-object '(local.get $value))))
              (else (call $character-code ,(native-object '(local.get $value))))))
          (return (local.get $value))))))
  #@(extern.convert_any (call $character-element
    ,(native-object '(local.get $string)) ,(native-object '(local.get $index))
    ,(native-object '(local.get $value)) (i32.const 1))))

(define-runtime-function $native-character-set-code-inline
    (($code i32) ($string externref) ($index externref)) ()
  (:export-raw "native-character-set-code-inline")
  (:locals ($text :nullable-object) ($subscript :nullable-object)
           ($data (ref null $digits)) ($position i64))
  #@(local.set $text (call $native-checked-value (local.get $string)))
  #@(local.set $subscript (call $native-checked-value (local.get $index)))
  #@(local.set $code (call $array-checked-character-code (local.get $code)))
  #@(block $miss
    (br_if $miss (i32.ne (call $tag (local.get $text)) (i32.const 10)))
    ,(wat-decode-scalar-integer '$subscript '$position '$miss)
    (local.set $data (struct.get $obj $digits (ref.cast (ref $obj) (local.get $text))))
    (br_if $miss (ref.is_null (local.get $data)))
    (br_if $miss (i64.ge_u (local.get $position) (i64.extend_i32_u (array.len (local.get $data)))))
    (if (ref.test (ref $fixnum) (local.get $subscript))
      (then (call $validate-bit-integer-layout (local.get $subscript))))
    (array.set $digits (local.get $data) (i32.wrap_i64 (local.get $position)) (local.get $code))
    (return))
  #@(call $native-check-aref-miss (local.get $text) (local.get $subscript))
  #@(drop (call $character-element (local.get $text) (local.get $subscript)
    (call $node (i32.const 16) (local.get $code) (ref.null eq) (ref.null eq) (ref.null eq)) (i32.const 1))))

(define-runtime-function $native-vector-set-inline
    (($value externref) ($vector externref) ($index i64)) (externref)
  (:export-raw "native-vector-set-inline")
  #@(array.set $objects (call $vector-data ,(native-object '(local.get $vector)))
    (i32.wrap_i64 (local.get $index)) ,(native-object '(local.get $value)))
  #@(local.get $value))

(define-runtime-function $native-character-code-at-inline (($string externref) ($index externref)) (externref)
  (:export-raw "native-character-code-at-inline")
  #@(extern.convert_any (call $int64
    (call $native-character-code-at-unboxed-inline (local.get $string) (local.get $index)))))

(define-runtime-function $native-character-code-at64-inline (($string externref) ($index i64)) (externref)
  (:export-raw "native-character-code-at64-inline")
  #@(extern.convert_any (call $int64
    (call $native-character-code-at64-unboxed-inline (local.get $string) (local.get $index)))))

(define-runtime-function $native-character-code-at-unboxed-inline (($string externref) ($index externref)) (i64)
  (:export-raw "native-character-code-at-unboxed-inline")
  (:locals ($subscript :nullable-object))
  #@(local.set $subscript (call $native-checked-value (local.get $index)))
  #@(if (ref.test (ref i31) (local.get $subscript))
    (then (return_call $native-character-code-at64-unboxed-inline (local.get $string)
      (i64.extend_i32_s (i31.get_s (ref.cast (ref i31) (local.get $subscript)))))))
  #@(call $native-unbox-int64
    (call $native-character-code-inline
      (call $native-character-ref-inline (local.get $string) (local.get $index)))))

(define-runtime-function $native-character-code-at64-unboxed-inline (($string externref) ($index i64)) (i64)
  (:export-raw "native-character-code-at64-unboxed-inline")
  (:locals ($text :nullable-object) ($data (ref null $digits)))
  ;; Canonical CHAR followed only by canonical CHAR-CODE needs no character
  ;; object. Keep CHAR's checked capacity/index behavior and the general view
  ;; fallback, including errors, at the original element-read instruction.
  #@(local.set $text (call $native-checked-value (local.get $string)))
  #@(if (i32.eq (call $tag (local.get $text)) (i32.const 10))
        (then
          (local.set $data (struct.get $obj $digits (ref.cast (ref $obj) (local.get $text))))
          ,(wat-fail-when #@(ref.is_null (local.get $data)) :malformed-form)
          ;; Full-width unsigned comparison rejects negatives and oversized
          ;; values before the narrowing conversion used for array indexing.
          (if (i64.lt_u (local.get $index) (i64.extend_i32_u (array.len (local.get $data))))
            (then (return (i64.extend_i32_u
              (call $array-checked-character-code
                (array.get $digits (local.get $data) (i32.wrap_i64 (local.get $index))))))))))
  #@(call $native-unbox-int64
    (call $native-character-code-inline
      (call $native-character-ref-inline (local.get $string) (extern.convert_any (call $int64 (local.get $index)))))))

(define-runtime-function $native-car-list-inline (($list externref)) (externref)
  (:export-raw "native-car-list-inline")
  #@(if (ref.is_null ,(native-object '(local.get $list))) (then (return (ref.null extern))))
  #@(extern.convert_any (struct.get $obj $a
    (ref.cast (ref $obj) ,(native-object '(local.get $list))))))

(define-runtime-function $native-cdr-inline-checked (($list externref)) (externref)
  (:export-raw "native-cdr-inline-checked")
  #@(extern.convert_any (call $cdr ,(native-object '(local.get $list)))))

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

(define-runtime-function $native-eq-test (($left externref) ($right externref)) (i32)
  (:export-raw "native-eq-test")
  #@(ref.eq ,(native-object '(local.get $left)) ,(native-object '(local.get $right))))

(define-runtime-function $native-eql-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-eql-inline")
  #@(extern.convert_any (call $bool (call $eql ,(native-object '(local.get $left)) ,(native-object '(local.get $right))))))

(define-runtime-function $native-eql-test (($left externref) ($right externref)) (i32)
  (:export-raw "native-eql-test")
  #@(call $eql ,(native-object '(local.get $left)) ,(native-object '(local.get $right))))

(define-runtime-function $native-length-inline (($sequence externref)) (externref)
  (:export-raw "native-length-inline")
  #@(if (i32.eq (call $tag ,(native-object '(local.get $sequence))) (i32.const 10))
    (then (return (extern.convert_any (call $int (array.len
      (ref.as_non_null (struct.get $obj $digits
        (ref.cast (ref $obj) ,(native-object '(local.get $sequence)))))))))))
  #@(extern.convert_any (call $int (call $proper-sequence-length
    ,(native-object '(local.get $sequence))))))

(define-runtime-function $native-list-length-inline (($value externref)) (externref)
  (:export-raw "native-list-length-inline")
  ;; Reuse the checked CL spine/cycle worker, including TYPE-ERROR recovery.
  #@(extern.convert_any (call $list-length-native ,(native-object '(local.get $value)))))

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

(define-runtime-function $checked-gethash-primary (($key :nullable-object) ($table :nullable-object)) (:nullable-object)
  (:locals ($cell :nullable-object) ($values :nullable-object) ($count i32))
  ;; User hash predicates can return extra values. A primary-only lookup must
  ;; not expose those through a raw runtime caller's pending value packet.
  #@(local.set $values (call $extra-value-list))
  #@(local.set $count (global.get $value-count))
  #@(local.set $cell (call $hash-cell-native (local.get $key) (local.get $table)))
  #@(global.set $extra-values (local.get $values))
  #@(global.set $value-count (local.get $count))
  #@(if (result (ref null eq)) (ref.is_null (local.get $cell))
    (then (ref.null eq)) (else (call $cdr (local.get $cell)))))

(define-runtime-function $native-gethash-primary-inline (($key externref) ($table externref)) (externref)
  (:export-raw "native-gethash-primary-inline")
  #@(extern.convert_any (call $checked-gethash-primary
    ,(native-object '(local.get $key)) ,(native-object '(local.get $table)))))

(define-runtime-function $native-puthash-inline
    (($value externref) ($key externref) ($table externref)) (externref)
  (:export-raw "native-puthash-inline")
  #@(extern.convert_any (call $puthash-native ,(native-object '(local.get $key))
    ,(native-object '(local.get $table)) ,(native-object '(local.get $value)))))

(define-runtime-function $native-consp-inline (($value externref)) (externref)
  (:export-raw "native-consp-inline")
  #@(extern.convert_any (call $bool (call $checked-consp
    (call $native-checked-value (local.get $value))))))

(define-runtime-function $native-consp-test (($value externref)) (i32)
  (:export-raw "native-consp-test")
  #@(call $checked-consp (call $native-checked-value (local.get $value))))

(define-runtime-function $native-simple-vector-p-inline (($value externref)) (externref)
  (:export-raw "native-simple-vector-p-inline")
  #@(extern.convert_any (call $bool (i32.eq
    (call $tag ,(native-object '(local.get $value))) (i32.const 34)))))

(define-runtime-function $native-vector-p-inline (($value externref)) (externref)
  (:export-raw "native-vector-p-inline")
  ;; Rank is fixed for an array's lifetime, including across ADJUST-ARRAY.
  ;; The canonical predicate is total, so arbitrary Lisp objects are valid.
  #@(extern.convert_any (call $bool (call $vectorp-native
    ,(native-object '(local.get $value))))))

(define-runtime-function $native-string-p-inline (($value externref)) (externref)
  (:export-raw "native-string-p-inline")
  ;; This uses the same representation predicate as ordinary STRINGP, covering
  ;; displaced/adjustable strings and the empty NIL-element string subtype.
  #@(extern.convert_any (call $bool (call $stringp-native
    ,(native-object '(local.get $value))))))

(define-runtime-function $native-integer-length-inline (($value externref)) (externref)
  (:export-raw "native-integer-length-inline")
  #@(extern.convert_any (call $integer-bit-measure
    ,(native-object '(local.get $value)) (i32.const 1))))

(define-runtime-function $native-ash-general-inline (($value externref) ($count externref)) (externref)
  (:export-raw "native-ash-general-inline")
  #@(extern.convert_any (call $ash-native
    (any.convert_extern (local.get $value)) (any.convert_extern (local.get $count)))))

(define-runtime-function $native-logand-general-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-logand-general-inline")
  #@(extern.convert_any (call $boole-native (i32.const 8)
    ,(native-object '(local.get $left)) ,(native-object '(local.get $right)) (i32.const 0))))

(define-runtime-function $native-zero-inline (($value externref)) (externref)
  (:export-raw "native-zero-inline")
  #@(if (ref.test (ref i31) ,(native-object '(local.get $value)))
    (then (return (extern.convert_any (call $bool (i32.eqz
      (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $value))))))))))
  #@(extern.convert_any (call $bool (call $zero
    (call $number ,(native-object '(local.get $value)))))))

(define-runtime-function $ordered-compare-native
    (($left :nullable-object) ($right :nullable-object) ($mode i32)) (:nullable-object)
  (:locals ($comparison i32))
  #@(local.set $comparison (call $compare (local.get $left) (local.get $right)))
  #@(call $bool
    (if (result i32) (i32.eq (local.get $mode) (i32.const 0))
      (then (i32.lt_s (local.get $comparison) (i32.const 0)))
      (else (if (result i32) (i32.eq (local.get $mode) (i32.const 1))
        (then (i32.gt_s (local.get $comparison) (i32.const 0)))
        (else (if (result i32) (i32.eq (local.get $mode) (i32.const 2))
          (then (i32.le_s (local.get $comparison) (i32.const 0)))
          (else (i32.ge_s (local.get $comparison) (i32.const 0))))))))))

(define-runtime-function $native-ordered-compare-inline
    (($left externref) ($right externref) ($mode i32)) (externref)
  (:export-raw "native-ordered-compare-inline")
  (:locals ($a i32) ($b i32))
  ;; Actual i31 operands are real integers throughout their signed range.
  ;; Other representations and modes retain the checked general comparison.
  #@(if (i32.and (i32.le_u (local.get $mode) (i32.const 3))
          (i32.and (ref.test (ref i31) ,(native-object '(local.get $left)))
                   (ref.test (ref i31) ,(native-object '(local.get $right)))))
    (then
      (local.set $a (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $left)))))
      (local.set $b (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $right)))))
      (return (extern.convert_any (call $bool
        (if (result i32) (i32.eq (local.get $mode) (i32.const 0))
          (then (i32.lt_s (local.get $a) (local.get $b)))
          (else (if (result i32) (i32.eq (local.get $mode) (i32.const 1))
            (then (i32.gt_s (local.get $a) (local.get $b)))
            (else (if (result i32) (i32.eq (local.get $mode) (i32.const 2))
              (then (i32.le_s (local.get $a) (local.get $b)))
              (else (i32.ge_s (local.get $a) (local.get $b)))))))))))))
  #@(extern.convert_any (call $ordered-compare-native
    ,(native-object '(local.get $left)) ,(native-object '(local.get $right)) (local.get $mode))))

(define-runtime-function $native-mod-general-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-mod-general-inline")
  (:locals ($quotient :object) ($remainder :object) ($a i32) ($b i32) ($r i32))
  ;; Immediate integers fit signed i32, including their quotient. Keep zero
  ;; on the checked general path so its Lisp condition retains both operands.
  #@(if (i32.and (ref.test (ref i31) ,(native-object '(local.get $left)))
                 (ref.test (ref i31) ,(native-object '(local.get $right))))
    (then
      (local.set $a (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $left)))))
      (local.set $b (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $right)))))
      (if (local.get $b)
        (then
          (local.set $r (i32.rem_s (local.get $a) (local.get $b)))
          (if (i32.and (i32.ne (local.get $r) (i32.const 0))
                        (i32.lt_s (i32.xor (local.get $r) (local.get $b)) (i32.const 0)))
            (then (local.set $r (i32.add (local.get $r) (local.get $b)))))
          (return (extern.convert_any (call $int (local.get $r))))))))
  #@(call $rounding-native (call $real ,(native-object '(local.get $left)))
    (call $real ,(native-object '(local.get $right))) (i32.const 0) (i32.const 0))
  #@(local.set $remainder) #@(local.set $quotient)
  #@(extern.convert_any (local.get $remainder)))

(define-runtime-function $native-rounding-primary-inline
    (($left externref) ($right externref) ($mode i32)) (externref)
  (:locals ($quotient :object) ($remainder :object) ($a i32) ($b i32)
           ($q i32) ($r i32) ($negative i32) ($adjust i32) ($twice i32) ($divisor i32))
  #@(if (i32.and (ref.test (ref i31) ,(native-object '(local.get $left)))
                 (ref.test (ref i31) ,(native-object '(local.get $right))))
    (then
      (local.set $a (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $left)))))
      (local.set $b (i31.get_s (ref.cast (ref i31) ,(native-object '(local.get $right)))))
      ;; i31 operands cannot overflow signed i32 division, including -2^30/-1.
      ;; INT still promotes that quotient rather than wrapping it to i31.
      (if (local.get $b)
        (then
          (local.set $q (i32.div_s (local.get $a) (local.get $b)))
          (local.set $r (i32.rem_s (local.get $a) (local.get $b)))
          (if (local.get $r)
            (then
              (local.set $negative (i32.lt_s (i32.xor (local.get $a) (local.get $b)) (i32.const 0)))
              (if (i32.eq (local.get $mode) (i32.const 0))
                (then (local.set $adjust (local.get $negative))))
              (if (i32.eq (local.get $mode) (i32.const 1))
                (then (local.set $adjust (i32.eqz (local.get $negative)))))
              (if (i32.eq (local.get $mode) (i32.const 3))
                (then
                  ;; Twice an absolute i31 remainder fits signed i32.
                  (local.set $twice (i32.shl
                    (select (i32.sub (i32.const 0) (local.get $r)) (local.get $r)
                      (i32.lt_s (local.get $r) (i32.const 0))) (i32.const 1)))
                  (local.set $divisor
                    (select (i32.sub (i32.const 0) (local.get $b)) (local.get $b)
                      (i32.lt_s (local.get $b) (i32.const 0))))
                  (local.set $adjust (i32.or (i32.gt_s (local.get $twice) (local.get $divisor))
                    (i32.and (i32.eq (local.get $twice) (local.get $divisor))
                      (i32.and (local.get $q) (i32.const 1)))))))
              (if (local.get $adjust)
                (then (local.set $q (i32.add (local.get $q)
                  (select (i32.const -1) (i32.const 1) (local.get $negative))))))))
          (return (extern.convert_any (call $int (local.get $q))))))))
  ;; Only the primary quotient is observed. Other reals and zero divisors
  ;; retain ordinary exact rounding, contagion and checked Lisp conditions.
  #@(call $rounding-native (call $real ,(native-object '(local.get $left)))
    (call $real ,(native-object '(local.get $right))) (local.get $mode) (i32.const 0))
  #@(local.set $remainder) #@(local.set $quotient)
  #@(extern.convert_any (local.get $quotient)))

(define-runtime-function $native-truncate-primary-inline (($left externref) ($right externref)) (externref)
  (:export-raw "native-truncate-primary-inline")
  #@(return_call $native-rounding-primary-inline
    (local.get $left) (local.get $right) (i32.const 2)))

(define-runtime-function $native-character-p-inline (($value externref)) (externref)
  (:export-raw "native-character-p-inline")
  #@(extern.convert_any (call $bool (i32.eq
    (call $tag ,(native-object '(local.get $value))) (i32.const 16)))))

(define-runtime-function $native-integer-p-inline (($value externref)) (externref)
  (:export-raw "native-integer-p-inline")
  ;; TAG normalizes immediate integers and all scalar/limb integer objects.
  #@(extern.convert_any (call $bool (i32.eq
    (call $tag ,(native-object '(local.get $value))) (i32.const 1)))))

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
  (miricl-cross::thread-checkpoint-wat))

(define-runtime-function $native-frame-enter (($function externref)) (i32 externref)
  (:export-raw "native-frame-enter")
  (:locals ($name :nullable-object) ($depth i32) ($frame :nullable-object))
  ;; A suspended/throwing checkpoint has not acquired this activation's frame.
  #@(call $thread-checkpoint)
  #@(local.set $name (call $native-checked-value (local.get $function)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) (local.get $name))) :malformed-form)
  #@(local.set $depth (global.get $backtrace-depth))
  #@(local.set $frame (array.get $objects (global.get $backtrace-frames)
    (i32.and (local.get $depth) (i32.const 63))))
  #@(array.set $objects (global.get $backtrace-frames)
    (i32.and (local.get $depth) (i32.const 63)) (local.get $name))
  #@(global.set $backtrace-depth (i32.add (local.get $depth) (i32.const 1)))
  #@(call $clear-values)
  #@(local.get $depth)
  #@(extern.convert_any (local.get $frame)))

(define-runtime-function $native-frame-leave
    (($value externref) ($depth i32) ($saved externref) ($normal i32)) (externref)
  (:export-raw "native-frame-leave")
  (:locals ($frame :nullable-object))
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $normal) (i32.const 1))
    (i32.or (i32.eqz (global.get $backtrace-depth))
      (i32.ne (i32.add (local.get $depth) (i32.const 1)) (global.get $backtrace-depth)))) :malformed-form)
  #@(local.set $frame (call $native-checked-value (local.get $saved)))
  ;; A failed result check transfers while the generated catch still owns the
  ;; frame. Its exceptional leave restores without changing the transfer group.
  #@(if (local.get $normal)
    (then (drop (call $native-checked-value (local.get $value))) (call $clear-values)))
  #@(array.set $objects (global.get $backtrace-frames)
    (i32.and (local.get $depth) (i32.const 63)) (local.get $frame))
  #@(global.set $backtrace-depth (local.get $depth))
  #@(local.get $value))

(define-runtime-function $native-tail-enter (($function externref)) (externref)
  (:export-raw "native-tail-enter")
  #@(drop (call $native-checked-value (local.get $function)))
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
  #@(drop (call $native-checked-value (local.get $function)))
  #@(if (i32.or (global.get $debug-evaluating)
                (ref.test (ref $native-debug-function) ,(native-object '(local.get $function))))
    (then (return (i32.const 0))))
  #@(drop (call $native-tail-enter (local.get $function)))
  #@(i32.const 1))

(define-runtime-function $native-tail-call (($function externref) ($arguments externref)) (externref)
  (:export-raw "native-tail-call")
  #@(return_call $native-tail-dispatch (local.get $function)
    (call $native-ingress-arguments (local.get $arguments)) (i32.const 0)))

;; Allocate only for the ordinary adapter; never share a mutable argument spine.
(define-runtime-function $native-tail-arguments
    (($count i32) ($a :nullable-object) ($b :nullable-object) ($c :nullable-object) ($d :nullable-object)) (:nullable-object)
  (:locals ($args :nullable-object))
  #@(if (i32.ge_u (local.get $count) (i32.const 4))
    (then (local.set $args (call $cons (local.get $d) (local.get $args)))))
  #@(if (i32.ge_u (local.get $count) (i32.const 3))
    (then (local.set $args (call $cons (local.get $c) (local.get $args)))))
  #@(if (i32.ge_u (local.get $count) (i32.const 2))
    (then (local.set $args (call $cons (local.get $b) (local.get $args)))))
  #@(if (local.get $count) (then (local.set $args (call $cons (local.get $a) (local.get $args)))))
  #@(local.get $args))

;; Fixed operands enter the same guarded workers without an argument packet.
;; Unknown arities, debugger entries and adapters retain the ordinary path.
(define-runtime-function $native-fixed-tail-call
    (($name :nullable-object) ($count i32) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object)) (externref)
  (:locals ($body (ref null $native-function)))
  #@(call $thread-checkpoint)
  #@(if (call $symbolp (local.get $name)) (then (local.set $name (call $fdefinition (local.get $name)))))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (if (ref.test (ref $native-function) (local.get $name))
      (then
        (block $not-direct
          (br_if $not-direct (ref.test (ref $native-debug-function) (local.get $name)))
          (local.set $body (ref.cast (ref $native-function) (local.get $name)))
          (br_if $not-direct (f64.ne (struct.get $obj $f (ref.as_non_null (local.get $body)))
            (f64.convert_i32_u (i32.add (local.get $count) (i32.const 2)))))
          (br_if $not-direct (ref.is_null (struct.get $native-function $direct (ref.as_non_null (local.get $body)))))
          (if (global.get $backtrace-depth)
            (then (array.set $objects (global.get $backtrace-frames)
              (i32.and (i32.sub (global.get $backtrace-depth) (i32.const 1)) (i32.const 63)) (local.get $name))))
          (call $clear-values)
          (return_call_ref $native-direct-entry
            (extern.convert_any (local.get $name)) (extern.convert_any (local.get $a))
            (extern.convert_any (local.get $b)) (extern.convert_any (local.get $c)) (extern.convert_any (local.get $d))
            (struct.get $native-direct-code $entry
              (ref.as_non_null (struct.get $native-function $direct (ref.as_non_null (local.get $body)))))))))
    (if (i32.and (i32.le_u (local.get $count) (i32.const 3))
          (i32.eq (call $native-builtin-id (local.get $name)) (i32.const 2147483644)))
      (then (return_call $native-generic-fixed-tail-call (local.get $name) (ref.null eq) (local.get $count)
        (local.get $a) (local.get $b) (local.get $c)))))
  #@(return_call $native-tail-dispatch (extern.convert_any (local.get $name))
    (extern.convert_any (call $native-tail-arguments (local.get $count)
      (local.get $a) (local.get $b) (local.get $c) (local.get $d))) (i32.const 1)))

(define-runtime-function $native-tail-dispatch (($function externref) ($arguments externref) ($ready i32)) (externref)
  (:locals ($name :nullable-object) ($args :nullable-object) ($id i32))
  #@(local.set $name (call $native-checked-value (local.get $function)))
  #@(local.set $args (call $native-checked-object (local.get $arguments)))
  ;; The fixed-operand entry already supplied its checkpoint. Adapter loops
  ;; still checkpoint before reading each subsequently selected mutable spine.
  #@(loop $dispatch
    (if (local.get $ready) (then (local.set $ready (i32.const 0))) (else (call $thread-checkpoint)))
    (if (call $symbolp (local.get $name))
      (then (local.set $name (call $fdefinition (local.get $name)))))
    ;; APPLY and FUNCALL are dispatch adapters, not additional Lisp frames.
    ;; Inspect the actual function so global redefinitions still take effect.
    (local.set $id (call $native-builtin-id (local.get $name)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'funcall)))
      (then (call $native-check-arguments (extern.convert_any (local.get $args)) (i32.const 1) (i32.const -1))
        (local.set $name (call $native-packet-ref-cl (local.get $args) (call $int (i32.const 0))))
        (local.set $args (call $native-packet-tail (local.get $args) (i32.const 1))) (br $dispatch)))
    (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'apply)))
      (then (call $native-check-arguments (extern.convert_any (local.get $args)) (i32.const 2) (i32.const -1))
        (local.set $name (call $native-packet-ref-cl (local.get $args) (call $int (i32.const 0))))
        (local.set $args (call $native-spread-arguments (local.get $args))) (br $dispatch)))
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
    ;; Generic functions need an actual tail worker entry, not APPLY cleanup.
    (if (i32.and (i32.eqz (global.get $debug-evaluating))
          (i32.eq (local.get $id) (i32.const 2147483644)))
      (then (return_call $native-generic-tail-call
        (extern.convert_any (local.get $name)) (extern.convert_any (local.get $args)))))
    (return (extern.convert_any (call $apply (local.get $name) (local.get $args)))))
  #@(unreachable))

;; Checked packet ingress shares the guarded selector and worker entry below.
(define-runtime-function $native-generic-tail-call
    (($function externref) ($arguments externref)) (externref)
  (:locals ($gf :nullable-object) ($args :nullable-object) ($scan :nullable-object)
           ($a :nullable-object) ($b :nullable-object) ($c :nullable-object) ($count i32))
  #@(local.set $gf ,(native-object '(local.get $function)))
  #@(local.set $args ,(native-object '(local.get $arguments)))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (br_if $ordinary (ref.test (ref $objects) (local.get $args)))
    ;; Read the mutable spine only after the dispatcher's checkpoint.
    (local.set $scan (local.get $args))
    (block $arguments (loop $argument
      (br_if $arguments (ref.is_null (local.get $scan)))
      (br_if $ordinary (i32.ge_u (local.get $count) (i32.const 3)))
      (br_if $ordinary (i32.ne (call $tag (local.get $scan)) (i32.const 7)))
      (if (i32.eqz (local.get $count))
        (then (local.set $a (call $car (local.get $scan))))
        (else (if (i32.eq (local.get $count) (i32.const 1))
          (then (local.set $b (call $car (local.get $scan))))
          (else (local.set $c (call $car (local.get $scan)))))))
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      (local.set $scan (call $cdr (local.get $scan))) (br $argument)))
    (return_call $native-generic-fixed-tail-call
      (local.get $gf) (local.get $args) (local.get $count) (local.get $a) (local.get $b) (local.get $c)))
  #@(extern.convert_any (call $apply (local.get $gf) (local.get $args))))

(define-runtime-function $native-generic-fixed-tail-call
    (($gf :nullable-object) ($args :nullable-object) ($count i32)
     ($a :nullable-object) ($b :nullable-object) ($c :nullable-object)) (externref)
  (:locals ($selected :nullable-object) ($body-object :nullable-object) ($body (ref null $native-function))
           ($method :nullable-object) ($rest :nullable-object) ($next :nullable-object) ($context :nullable-object))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (call $generic-recent-plan-ready (local.get $gf) (local.get $count)
      (local.get $a) (local.get $b) (local.get $c) (ref.null eq) (i32.const 0))
    (local.set $next) (local.set $rest) (local.set $method)
    (local.set $body-object) (local.set $selected)
    ;; Native methods enter their direct worker in tail position. Scalar plans
    ;; share their existing executor; other cases keep ordinary APPLY.
    ;; Callbacks within scalar operations have no added TCO guarantee.
    (br_if $ordinary (ref.is_null (local.get $selected)))
    (if (ref.is_null (local.get $body-object))
      (then
        ;; A committed scalar plan needs neither a second probe nor APPLY.
        ;; Match native tail entry's frame/value contract before callbacks.
        (if (global.get $backtrace-depth)
          (then (array.set $objects (global.get $backtrace-frames)
            (i32.and (i32.sub (global.get $backtrace-depth) (i32.const 1)) (i32.const 63))
            (local.get $gf))))
        (call $clear-values)
        (return (extern.convert_any (call $generic-recent-scalar-call
          (ref.cast (ref $direct-function) (local.get $selected)) (local.get $a) (local.get $b))))))
    (local.set $body (ref.cast (ref $native-function) (local.get $body-object)))
    (local.set $context (call $generic-recent-context (local.get $gf) (ref.as_non_null (local.get $body))
      (local.get $method) (local.get $rest) (local.get $next) (local.get $count)
      (local.get $a) (local.get $b) (local.get $c)))
    ;; Selection, context assembly and frame replacement are one nonpolling
    ;; interval. The dispatcher already supplied the cooperative checkpoint.
    (if (global.get $backtrace-depth)
      (then (array.set $objects (global.get $backtrace-frames)
        (i32.and (i32.sub (global.get $backtrace-depth) (i32.const 1)) (i32.const 63))
        (local.get $body-object))))
    (call $clear-values)
    (return_call_ref $native-direct-entry
      (extern.convert_any (local.get $body-object)) (extern.convert_any (local.get $context))
      (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b)) (extern.convert_any (local.get $c))
      (struct.get $native-direct-code $entry
        (ref.as_non_null (struct.get $native-function $direct (ref.as_non_null (local.get $body)))))))
  ;; This fallback intentionally retains the existing APPLY cleanup contract.
  #@(extern.convert_any (call $apply (local.get $gf) (if (result (ref null eq)) (ref.is_null (local.get $args))
      (then (call $native-tail-arguments (local.get $count) (local.get $a) (local.get $b) (local.get $c) (ref.null eq)))
      (else (local.get $args))))))

(define-runtime-function $native-tail-call0
    (($function externref)) (externref)
  (:export-raw "native-tail-call0")
  #@(return_call $native-fixed-tail-call (call $native-checked-value (local.get $function)) (i32.const 0)
    (ref.null eq) (ref.null eq) (ref.null eq) (ref.null eq)))

(define-runtime-function $native-tail-call1
    (($function externref) ($a externref)) (externref)
  (:export-raw "native-tail-call1")
  #@(return_call $native-fixed-tail-call (call $native-checked-value (local.get $function)) (i32.const 1)
    (call $native-checked-value (local.get $a)) (ref.null eq) (ref.null eq) (ref.null eq)))

(define-runtime-function $native-tail-call2
    (($function externref) ($a externref) ($b externref)) (externref)
  (:export-raw "native-tail-call2")
  #@(return_call $native-fixed-tail-call (call $native-checked-value (local.get $function)) (i32.const 2)
    (call $native-checked-value (local.get $a)) (call $native-checked-value (local.get $b)) (ref.null eq) (ref.null eq)))

(define-runtime-function $native-tail-call3
    (($function externref) ($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-tail-call3")
  #@(return_call $native-fixed-tail-call (call $native-checked-value (local.get $function)) (i32.const 3)
    (call $native-checked-value (local.get $a)) (call $native-checked-value (local.get $b)) (call $native-checked-value (local.get $c)) (ref.null eq)))

(define-runtime-function $native-tail-call4
    (($function externref) ($a externref) ($b externref) ($c externref) ($d externref)) (externref)
  (:export-raw "native-tail-call4")
  #@(return_call $native-fixed-tail-call (call $native-checked-value (local.get $function)) (i32.const 4)
    (call $native-checked-value (local.get $a)) (call $native-checked-value (local.get $b)) (call $native-checked-value (local.get $c)) (call $native-checked-value (local.get $d))))

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
      (ref.cast (ref $native-direct-entry) (local.get $entry)) (i32.const 0)))
  #@(local.get $function))

(define-runtime-function $native-attach-direct-root
    (($function externref) ($entry funcref) ($arity i32)) (externref)
  (:export-raw "native-attach-direct-root")
  #@(return_call $native-attach-direct
    (extern.convert_any (call $native-with-arity ,(native-object '(local.get $function)) (local.get $arity)))
    (local.get $entry)))

(define-runtime-function $native-attach-scalar (($function externref) ($entry funcref)) (externref)
  (:export-raw "native-attach-scalar")
  ;; The adapter can grant scalar authority after an audited attachment.
  ;; Calling this import alone only installs the ordinary boxed entry.
  #@(return_call $native-attach-direct (local.get $function) (local.get $entry)))

(define-runtime-function $native-attach-certified-root
    (($function externref) ($entry funcref) ($arity i32)) (externref)
  ;; Only the trusted adapter may call this after auditing the final worker.
  ;; CORE exports are deliberately absent from native module imports.
  (:export-raw "core-native-attach-certified-root")
  (:locals ($fn (ref $native-function)) ($code (ref $native-direct-code)) ($slot i32))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) ,(native-object '(local.get $function)))) :malformed-form)
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-direct-entry) (local.get $entry))) :malformed-form)
  (wat-fail-when #@(i32.gt_u (local.get $arity) (i32.const 4)) :malformed-form)
  #@(local.set $fn (ref.cast (ref $native-function) ,(native-object '(local.get $function))))
  (wat-fail-when #@(i32.eqz (ref.is_null (struct.get $native-function $direct (local.get $fn)))) :malformed-form)
  #@(local.set $fn (ref.cast (ref $native-function)
    (call $native-with-arity (local.get $fn) (local.get $arity))))
  #@(local.set $slot (global.get $native-call-proof-next))
  #@(local.set $code (struct.new $native-direct-code
    (call $allocate-object-tag (i32.const 55)) (i32.const 0) (f64.const 0)
    (struct.get $obj $digits (local.get $fn)) (ref.null eq) (ref.null eq) (ref.null eq)
    (ref.cast (ref $native-direct-entry) (local.get $entry))
    (i32.or (i32.shl (local.get $slot) (i32.const 3)) (i32.add (local.get $arity) (i32.const 1)))))
  ;; Canonical WasmGC types can be reproduced by supplied modules. The hint
  ;; alone is not authority: own this exact immutable code object privately.
  #@(array.set $objects (global.get $native-call-proofs) (local.get $slot) (local.get $code))
  #@(global.set $native-call-proof-next (i32.and (i32.add (local.get $slot) (i32.const 1)) (i32.const 15)))
  #@(struct.set $native-function $direct (local.get $fn) (local.get $code))
  #@(extern.convert_any (local.get $fn)))

(define-runtime-function $native-direct-apply
    (($name (ref $native-function)) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object) ($primary i32)) (externref)
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(return_call $native-direct-apply-ready
    (local.get $name) (local.get $a) (local.get $b) (local.get $c) (local.get $d) (local.get $primary)))

;; The caller owns the checkpoint and has not called Lisp or polled since
;; checking the selected entry. Keep frame and dynamic cleanup in one body.
(define-runtime-function $native-direct-apply-ready
    (($name (ref $native-function)) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object) ($primary i32)) (externref)
  (:locals ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object) ($restart-associations :nullable-object)
           ($catches :nullable-object) ($targets :nullable-object) ($thrown i32)
           ($backtrace-depth i32) ($backtrace-slot i32) ($backtrace-frame :nullable-object)
           ($result :nullable-object) ($direct (ref $native-direct-code)) ($proof i32))
  #@(local.set $direct (ref.as_non_null (struct.get $native-function $direct (local.get $name))))
  #@(local.set $proof (struct.get $native-direct-code $call-proof (local.get $direct)))
  #@(if (i32.and (i32.ne (local.get $proof) (i32.const 0))
        (i32.and (i32.eqz (global.get $debug-evaluating))
          (i32.eqz (ref.test (ref $native-debug-function) (local.get $name)))))
    (then
      (if (i32.and
            (ref.eq (local.get $direct) (array.get $objects (global.get $native-call-proofs)
              (i32.and (i32.shr_u (local.get $proof) (i32.const 3)) (i32.const 15))))
            (f64.eq (struct.get $obj $f (local.get $name))
              (f64.convert_i32_u (i32.add (i32.and (local.get $proof) (i32.const 7)) (i32.const 1)))))
        (then
          ;; The audited worker cannot observe or change dynamic roots, poll,
          ;; trap or signal, and its final RETURN1 normalizes exactly one value.
          (return_call_ref $native-direct-entry (extern.convert_any (local.get $name))
              (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
              (extern.convert_any (local.get $c)) (extern.convert_any (local.get $d))
              (struct.get $native-direct-code $entry (local.get $direct)))))))
  #@(local.set $backtrace-depth (global.get $backtrace-depth))
  #@(local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
  #@(local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
  #@(array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $name))
  #@(global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
  #@(call $clear-values)
  ;; Ordinary attached modules need not have passed the executable audit.
  ;; Admit their result before releasing the frame and dynamic cleanup owner.
  #@(drop ,(wat-dynamic-call
    #@(call $native-checked-value
      (call_ref $native-direct-entry (extern.convert_any (local.get $name))
        (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
        (extern.convert_any (local.get $c)) (extern.convert_any (local.get $d))
        (struct.get $native-direct-code $entry (local.get $direct))))
    :cleanup #@(block
      (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
      (global.set $backtrace-depth (local.get $backtrace-depth)))))
  ;; Clear only after successful scope/frame restoration and transfer handling.
  ;; Full consumers retain the complete group until their own capture boundary.
  #@(if (local.get $primary) (then (call $clear-values)))
  #@(extern.convert_any (local.get $result)))

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

(defun native-direct-call-wat (arity &optional checked function arguments typed-function ready)
  ;; Reuse the caller's native-function and fixed-arity checks when present.
  ;; Callers may also reuse object conversions and a proven native-function cast.
  (let* ((names (case arity (0 nil) (1 '($value)) (2 '($left $right))
                        (3 '($a $b $c)) (4 '($a $b $c $d))))
         (fn (or function (native-object '(local.get $function))))
         (native-fn (or typed-function `(ref.cast (ref $native-function) ,fn)))
         (direct (wat-fragment #@(if (i32.eqz (ref.is_null
                       (struct.get $native-function $direct ,native-fn)))
                     (then
                       (return_call ,(if ready '$native-direct-apply-ready '$native-direct-apply) ,native-fn
                         ,@(loop for index below 4 collect (if (< index arity)
                           (if arguments (nth index arguments)
                             (native-object (list 'local.get (nth index names)))) '(ref.null eq)))
                         (i32.const 1)))))))
    (if checked direct
      (wat-fragment #@(if (ref.test (ref $native-function) ,fn)
        (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,fn)) (f64.const ,(+ arity 2)))
          (then ,direct))))))))

;; Checked callers already own the application checkpoint. Resolve the live
;; binding and admit its exact attached worker without polling or calling Lisp.
(define-runtime-function $native-symbol-fixed-function
    (($name :nullable-object) ($count i32)) (:nullable-object)
  (:locals ($binding :nullable-object) ($value :nullable-object) ($body (ref $native-function)))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (br_if $miss (i32.eqz (call $symbolp (local.get $name))))
    (local.set $binding (call $function-binding (local.get $name)))
    (br_if $miss (ref.is_null (local.get $binding)))
    (local.set $value (call $binding-value (local.get $binding)))
    (br_if $miss (i32.eqz (ref.test (ref $native-function) (local.get $value))))
    (local.set $body (ref.cast (ref $native-function) (local.get $value)))
    (br_if $miss (ref.test (ref $native-debug-function) (local.get $body)))
    (br_if $miss (f64.ne (struct.get $obj $f (local.get $body))
      (f64.convert_i32_u (i32.add (local.get $count) (i32.const 2)))))
    (br_if $miss (ref.is_null (struct.get $native-function $direct (local.get $body))))
    (return (local.get $body)))
  #@(ref.null eq))

(defun native-symbol-fixed-call-wat (arity &optional (function-object (native-object '(local.get $function))))
  ;; Keep actual-function and builtin fast paths ahead of this speculative
  ;; fallback. DIRECT-RESULT is scratch until either this hit or ordinary APPLY.
  (let ((arguments (case arity (1 '((local.get $a)))
                     (2 '((local.get $a) (local.get $b)))
                     (3 (mapcar (lambda (name) (native-object `(local.get ,name))) '($a $b $c)))
                     (4 (mapcar (lambda (name) (native-object `(local.get ,name))) '($a $b $c $d))))))
    (wat-fragment #@(block $symbol-miss
      (local.set $direct-result (call $native-symbol-fixed-function
        ,function-object (i32.const ,arity)))
      (br_if $symbol-miss (ref.is_null (local.get $direct-result)))
      (return_call $native-direct-apply-ready
        (ref.cast (ref $native-function) (local.get $direct-result))
        ,@(loop for index below 4 collect (if (< index arity) (nth index arguments) '(ref.null eq)))
        (i32.const 1))))))

(defun native-generic-fast-call-wat (arity &optional ready
                                    (function-object '(call $native-checked-value (local.get $function)))
                                    arguments)
  (let* ((names (case arity (0 nil) (1 '($value)) (2 '($left $right))
                         (3 '($a $b $c)) (4 '($a $b $c $d))))
         (id (if (zerop arity) `(call $native-builtin-id ,(native-object '(local.get $function))) '(local.get $id)))
         (expected (if (= arity 3) -2147483646 2147483644)))
    (wat-fragment #@(if (i32.and (i32.eq ,id (i32.const ,expected))
                                (i32.eqz (global.get $debug-evaluating)))
      (then
        ,@(unless ready '((if (global.get $thread-active) (then (call $thread-checkpoint)))))
        (local.set $direct-result (call $native-fixed-apply-ready
          ,function-object
          ,@(loop for index below 4 collect (if (< index arity)
               (if arguments (nth index arguments)
                   `(call $native-checked-value (local.get ,(nth index names)))) '(ref.null eq)))
          (i32.const ,arity) (i32.const 0)))
        (call $clear-values)
        (return (extern.convert_any (local.get $direct-result))))))))

(defun native-fixed-builtin-functions (arity)
  "Conservative checked builtin capabilities of NATIVE-CALL0..4.
This records implementations, not signatures or successful result counts."
  (case arity
    (0 '(vector))
    ;; The accessor decoder already provides checked fixed entries for this
    ;; shared implementation family; the contract join still proves arity
    ;; and exact value count independently for every member.
    (1 (append '(car cdr not null identity consp atom symbolp listp stringp
                 integerp numberp rationalp realp complexp floatp 1+ 1- length list vector
                 string symbol-name char-upcase char-downcase alphanumericp oddp evenp abs signum)
               *list-accessors*))
    (2 '(eq eql cons + - * / = /= < > <= >= list vector string=
         vector-push vector-push-extend bit sbit aref svref))
    ((3 4) '(list vector))))

(defun native-standard-fixed-functions (arity)
  "Join checked implementation capabilities with shared ANSI contracts.
Unknown identity, arity or result count leaves the ordinary evaluator path."
  (when (and (integerp arity) (<= 0 arity 4))
    (remove-if-not
      (lambda (name)
        (and (member name *runtime-functions* :test #'eq)
             (not (assoc name *native-public-functions* :test #'eq))
             (member (miricl-compiler::standard-function-property name :kind)
                     '(:function :accessor) :test #'eq)
             (let ((limits (miricl-compiler::standard-function-property name :arity)))
               (and limits (<= (car limits) arity)
                    (or (null (cdr limits)) (<= arity (cdr limits)))))
             (eql (miricl-compiler::standard-function-result-count name arity) 1)))
      (native-fixed-builtin-functions arity))))

(defun native-standard-fixed-dispatch-wat (value arity hit miss)
  ;; Resolve contracts during runtime generation. No guest table or query is
  ;; added; eligible entries keep the existing compact symbol-ID dispatcher.
  ;; These facts do not prove operand types or replace checked native entries.
  ;; The caller checked ARITY in 0..4. Group identical capability intersections
  ;; so constructor-only arities share their dispatcher, retaining the common
  ;; one/two-argument order without widening their symbol lookup trees.
  (let ((groups nil))
    (dolist (count '(1 2 0 3 4))
      (let* ((eligible (native-standard-fixed-functions count))
             (group (find eligible groups :key #'cdr :test #'equal)))
        (if group
            (setf (car group) (append (car group) (list count)))
            (setf groups (append groups (list (cons (list count) eligible)))))))
    (apply #'runtime-wat-sequence
      (loop for ((counts . eligible) . rest) on groups
            for dispatch = (if eligible
                (wat-symbol-dispatch (runtime-wat-sequence value)
                  (list (cons eligible (runtime-wat-sequence hit))))
                (runtime-wat-sequence))
            collect (if rest
                (wat-integer-branch (runtime-wat-sequence arity) counts
                  (runtime-wat-sequence dispatch miss))
                dispatch)))))

(defun native-fixed-primitive-entries ()
  ;; Only checked object-argument entries belong here. Their fixed ABI accepts
  ;; already evaluated Lisp values and never exposes the caller's argument
  ;; packet. These descriptors do not change private primitive ordinals.
  '((%%list-length 1 $list-length-native)
    (%%oddp 1 $oddp-native)
    (%%copy-list 1 $copy-list-native)
    (%%nthcdr 2 $nthcdr-native)
    (%%nth 2 $public-nth-cl)
    (%%slot-value 2 $slot-value-native)
    (%%list-copy 2 $list-copy-fixed)
    (%cell 2 $property-cell)
    (%list-eq 2 $list-eq)
    (%%hash-cell 2 $hash-cell-native)
    (%%package-present-entry 2 $present)
    (%%package-inherited-entry 2 $inherited)
    (%%package-node-ref 3 $package-node-ref-value)
    (%%array-type-metadata 3 $array-type-metadata-value)
    (%%stream-sequence-metadata 3 $stream-sequence-metadata-value)
    (%%clos-method-metadata 3 $clos-method-metadata-value)
    (%%real-type-metadata 3 $real-type-metadata-value)
    (%%array-upgrade-metadata 3 $array-upgrade-metadata-value)
    (%%clos-initarg-metadata 3 $clos-initarg-metadata-value)
    (%%complex-type-metadata 3 $complex-type-metadata-value)
    (%%generic-dispatch-metadata 3 $generic-dispatch-metadata-value)
    (%%compound-type-metadata 3 $compound-type-metadata-value)
    (%%array-adjust-metadata 3 $array-adjust-metadata-value)
    (%%slot-operation 4 $slot-operation-fixed)))

(defun native-fixed-primitive-call-wat (arity &optional arguments)
  ;; NATIVE-BUILTIN-ID reads the actual function object's code, independently
  ;; of its current name. Interpreted/native closures (code zero), replacements
  ;; and other arities continue through ordinary APPLY, including its errors.
  (let ((operands (or arguments
                      (mapcar (lambda (name) (native-object `(local.get ,name)))
                        (ecase arity (1 '($value)) (2 '($left $right))
                                     (3 '($a $b $c)) (4 '($a $b $c $d))))))
        (id '(local.get $id)))
    (runtime-wat-forms
      (loop for (name count target) in (native-fixed-primitive-entries)
            when (= count arity) collect
        `(if (i32.eq ,id
                     (i32.const ,(let ((code (+ +clos-primitive-base+ (clos-primitive-op name))))
                                   (if (= arity 3) (- -1 code) (1- code)))))
           (then
             (local.set $direct-result
               (call ,target ,@operands))
             ;; These entry points can call CL workers and leave transient
             ;; multiple values. This ABI promises only the primary value.
             (call $clear-values)
             (return (extern.convert_any (local.get $direct-result)))))))))

(defun native-fixed-vector-call-wat (arity)
  ;; VECTOR accepts arbitrary objects and returns a fresh simple general
  ;; vector. Recognize the saved builtin's immutable code, never its current
  ;; global binding. The fixed ABI already owns evaluated argument values;
  ;; allocating its backing array directly avoids an APPLY argument list.
  (let* ((arguments (ecase arity
                      (0 nil) (1 '($a)) (2 '($a $b))
                      (3 '($a $b $c)) (4 '($a $b $c $d))))
         (id (if (member arity '(1 2 3 4)) '(local.get $id)
                 `(call $native-builtin-id ,(native-object '(local.get $function)))))
         (expected (runtime-symbol-id 'vector)))
    (wat-fragment #@(if (i32.eq ,id (i32.const ,(if (= arity 3) (- -2 expected) expected)))
      (then
        ;; CALL1..4 checkpoint before their builtin cases. CALL0 normally
        ;; reaches APPLY's checkpoint, which this constructor now bypasses.
        ,@(when (zerop arity)
            '((if (global.get $thread-active) (then (call $thread-checkpoint)))))
        (local.set $direct-result
          (call $vector-new (array.new_fixed $objects ,arity
            ,@(mapcar (lambda (name)
                        (if (member arity '(1 2)) `(local.get ,name)
                            (native-object `(local.get ,name)))) arguments))))
        (call $clear-values)
        (return (extern.convert_any (local.get $direct-result))))))))

(defun native-fixed-fallback-arguments-wat (arity
                                            &optional (function-object '(local.get $function-object)))
  ;; Checked native functions, HIR and generic functions accept owned packets.
  ;; This selects storage only; ordinary application owns frames and cleanup,
  ;; and native entry ingress still snapshots the packet before callbacks.
  (let* ((arguments (ecase arity
                      (1 '((local.get $a)))
                      (2 '((local.get $a) (local.get $b)))
                      (3 (mapcar (lambda (name) (native-object `(local.get ,name))) '($a $b $c)))
                      (4 (mapcar (lambda (name) (native-object `(local.get ,name))) '($a $b $c $d)))))
         (id '(local.get $id))
         (hir-id (if (= arity 3) (- -2 2147483640) 2147483640)))
    `(if (result (ref null eq))
         (i32.or (ref.test (ref $native-function) ,function-object)
           (i32.or (i32.eq ,id (i32.const ,hir-id))
             (i32.eq ,id (i32.const ,(if (= arity 3) -2147483646 2147483644)))))
       (then (array.new_fixed $objects ,arity ,@arguments))
       (else ,(reduce (lambda (argument tail) `(call $cons ,argument ,tail))
                      arguments :from-end t :initial-value '(ref.null eq))))))

;; Checked callers own the operation-entry checkpoint and fixed operands.
;; Selection and protected invocation share this nonpolling ready interval.
(define-runtime-function $native-fixed-apply-ready
    (($name :nullable-object) ($a :nullable-object) ($b :nullable-object)
     ($c :nullable-object) ($d :nullable-object) ($count i32) ($generic-probe i32)) (:nullable-object)
  (:locals ($body (ref $native-function)) ($args :nullable-object) ($packet (ref $objects))
           ($result :nullable-object) ($id i32) ($hit i32) ($index i32))
  #@(block $called
    (block $direct-miss
      (br_if $direct-miss (global.get $debug-evaluating))
      (local.set $body (if (result (ref $native-function))
        (ref.test (ref $native-function) (local.get $name))
        (then (ref.cast (ref $native-function) (local.get $name)))
        (else (ref.cast (ref $native-function)
          (br_on_null $direct-miss (call $native-symbol-fixed-function
            (local.get $name) (local.get $count)))))))
      (br_if $direct-miss (ref.test (ref $native-debug-function) (local.get $body)))
      (br_if $direct-miss (f64.ne (struct.get $obj $f (local.get $body))
        (f64.convert_i32_u (i32.add (local.get $count) (i32.const 2)))))
      (br_if $direct-miss (ref.is_null (struct.get $native-function $direct (local.get $body))))
      (local.set $result (ref.cast (ref null eq) (any.convert_extern
        (call $native-direct-apply-ready (local.get $body)
          (local.get $a) (local.get $b) (local.get $c) (local.get $d) (i32.const 0)))))
      (br $called))
    (local.set $id (call $native-builtin-id (local.get $name)))
    (block $generic-miss
      ;; 0 needs validation, 1 can retry with a materialized operand, and 2
      ;; has already probed the original boxed arguments.
      (br_if $generic-miss (i32.eq (local.get $generic-probe) (i32.const 2)))
      (br_if $generic-miss (global.get $debug-evaluating))
      (br_if $generic-miss (i32.ne (local.get $id) (i32.const 2147483644)))
      ;; A generic dispatch code alone does not validate its metadata. The
      ;; checked property boundary may yield; select only after it returns.
      (if (i32.eqz (local.get $generic-probe))
        (then (drop (call $generic-dispatch-metadata-value (call $int (i32.const 2))
          (local.get $name) (ref.null eq)))))
      (call $generic-recent-call-ready (local.get $name) (local.get $count)
        (local.get $a) (local.get $b) (local.get $c) (local.get $d))
      (local.set $hit) (local.set $result)
      (br_if $generic-miss (i32.eqz (local.get $hit)))
      (br $called))
    ;; Binary misses use the same storage selection as ordinary CALL2.
    ;; List consumers can use their fresh spine without converting a packet.
    (local.set $args
      (if (result (ref null eq)) (i32.eq (local.get $count) (i32.const 2))
        (then ,(native-fixed-fallback-arguments-wat 2 '(local.get $name)))
        (else
          (local.set $packet (array.new_default $objects (local.get $count)))
          (block $copied (loop $copy
            (br_if $copied (i32.eq (local.get $index) (local.get $count)))
            (array.set $objects (local.get $packet) (local.get $index)
              (if (result (ref null eq)) (i32.eq (local.get $index) (i32.const 0))
                (then (local.get $a))
                (else (if (result (ref null eq)) (i32.eq (local.get $index) (i32.const 1))
                  (then (local.get $b))
                  (else (if (result (ref null eq)) (i32.eq (local.get $index) (i32.const 2))
                    (then (local.get $c)) (else (local.get $d))))))))
            (local.set $index (i32.add (local.get $index) (i32.const 1)))
            (br $copy)))
          (local.get $packet))))
    ;; Debugger calls keep ordinary entry handling. Other callers already
    ;; own the checkpoint.
    (local.set $result (if (result (ref null eq)) (global.get $debug-evaluating)
      (then (call $apply (local.get $name) (local.get $args)))
      (else (call $apply0-ready (local.get $name) (local.get $args))))))
  #@(local.get $result))

(define-runtime-function $native-fixed-call
    (($function externref) ($a externref) ($b externref) ($c externref) ($d externref) ($flags i32)) (externref)
  (:export-raw "native-fixed-call")
  (:locals ($name :nullable-object) ($left :nullable-object) ($right :nullable-object)
           ($third :nullable-object) ($fourth :nullable-object) ($result :nullable-object))
  ;; Validate every raw operand, including padding, independently of proofs.
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $flags) (i32.const 12))
    (i32.gt_u (i32.and (local.get $flags) (i32.const 7)) (i32.const 4))) :malformed-form)
  #@(local.set $name (call $native-checked-value (local.get $function)))
  #@(local.set $left (call $native-checked-value (local.get $a)))
  #@(local.set $right (call $native-checked-value (local.get $b)))
  #@(local.set $third (call $native-checked-value (local.get $c)))
  #@(local.set $fourth (call $native-checked-value (local.get $d)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $result (call $native-fixed-apply-ready (local.get $name)
    (local.get $left) (local.get $right) (local.get $third) (local.get $fourth)
    (i32.and (local.get $flags) (i32.const 7)) (i32.const 0)))
  ;; Primary demand supplies no result-count proof. Capture before clearing.
  #@(if (i32.eqz (i32.and (local.get $flags) (i32.const 8)))
    (then (local.set $result (call $value-list (local.get $result)))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-scalar-slot (($owner (ref $native-function))) (i32)
  (:locals ($first i32) ($offset i32) ($slot i32) ($worker (ref null $native-scalar-worker)))
  #@(local.set $first
    (i32.and (i32.shr_u (struct.get $obj $tag (local.get $owner)) (i32.const 6)) (i32.const 63)))
  #@(loop $probe
    (local.set $slot (i32.and (i32.add (local.get $first) (local.get $offset)) (i32.const 63)))
    (local.set $worker (array.get $native-scalar-registry (global.get $native-scalar-workers) (local.get $slot)))
    (if (ref.is_null (local.get $worker)) (then (return (local.get $slot))))
    (if (ref.eq (struct.get $native-scalar-worker $owner (ref.as_non_null (local.get $worker))) (local.get $owner))
      (then (return (local.get $slot))))
    (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
    (br_if $probe (i32.lt_u (local.get $offset) (i32.const 4))))
  #@(local.get $first))

(define-runtime-function $native-register-scalar
    (($function externref) ($raw funcref) ($arity i32) ($arguments i32) ($result-kind i32)) ()
  (:export-raw "core-native-register-scalar")
  (:locals ($value :nullable-object) ($owner (ref $native-function)) ($direct (ref $native-direct-code)))
  ;; The host calls this only after checking both exact boxed wrappers and
  ;; binding the raw target from the same instance. Native imports cannot
  ;; create authority. Neither the registry nor an index is in saved objects.
  #@(local.set $value (call $native-checked-value (local.get $function)))
  (wat-fail-when #@(i32.eqz (ref.test (ref $native-function) (local.get $value))) :malformed-form)
  #@(local.set $owner (ref.cast (ref $native-function) (local.get $value)))
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $arity) (i32.const 4))
    (i32.eqz (ref.test (ref $native-scalar-entry) (local.get $raw)))) :malformed-form)
  ;; Four three-bit argument kinds are followed by the loader's omission mask.
  (wat-fail-when #@(i32.ge_u (i32.and (local.get $arguments) (i32.const 4095))
    (i32.shl (i32.const 1) (i32.mul (local.get $arity) (i32.const 3)))) :malformed-form)
  (wat-fail-when #@(i32.ge_u (i32.shr_u (local.get $arguments) (i32.const 12))
    (i32.shl (i32.const 1) (local.get $arity))) :malformed-form)
  (wat-fail-when #@(i32.gt_u (i32.sub (local.get $result-kind) (i32.const 1)) (i32.const 3)) :malformed-form)
  #@(if (i32.or (ref.test (ref $native-debug-function) (local.get $owner))
    (i32.or (f64.ne (struct.get $obj $f (local.get $owner))
              (f64.convert_i32_u (i32.add (local.get $arity) (i32.const 2))))
            (ref.is_null (struct.get $native-function $direct (local.get $owner)))))
    (then (return)))
  #@(local.set $direct (ref.as_non_null (struct.get $native-function $direct (local.get $owner))))
  #@(array.set $native-scalar-registry (global.get $native-scalar-workers)
    (call $native-scalar-slot (local.get $owner))
    (struct.new $native-scalar-worker (local.get $owner) (local.get $direct)
      (struct.get $obj $a (local.get $owner)) (struct.get $obj $b (local.get $owner))
      (ref.cast (ref $native-scalar-entry) (local.get $raw)) (local.get $arguments) (local.get $result-kind))))

(define-runtime-function $native-scalar-decode
    (($object :nullable-object) ($kind i32)) (i64 i32)
  (:locals ($bits i64))
  ;; 0 is boxed. 1 is a signed i64, 2 a character, 3/4 the runtime float
  ;; tags, and 5/6 require physical i31 storage (the latter also checks BIT).
  #@(block $miss
    (if (i32.eq (local.get $kind) (i32.const 1))
      (then ,(wat-decode-scalar-integer '$object '$bits '$miss)
        (return (local.get $bits) (i32.const 1))))
    (if (i32.le_u (i32.sub (local.get $kind) (i32.const 5)) (i32.const 1))
      (then
        (br_if $miss (i32.eqz (ref.test (ref i31) (local.get $object))))
        (local.set $bits (i64.extend_i32_s (i31.get_s (ref.cast (ref i31) (local.get $object)))))
        (br_if $miss (i32.and (i32.eq (local.get $kind) (i32.const 6))
                             (i64.gt_u (local.get $bits) (i64.const 1))))
        (return (local.get $bits) (i32.const 1))))
    (br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $object))))
    (if (i32.eq (local.get $kind) (i32.const 2))
      (then
        (br_if $miss (i32.ne (call $tag (local.get $object)) (i32.const 16)))
        (return (i64.extend_i32_u (call $character-code (local.get $object))) (i32.const 1))))
    (br_if $miss (i32.gt_u (i32.sub (local.get $kind) (i32.const 3)) (i32.const 1)))
    (br_if $miss (i32.ne (call $tag (local.get $object)) (local.get $kind)))
    (return (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $object))))
            (i32.const 1)))
  #@(i64.const 0) #@(i32.const 0))

(define-runtime-function $native-scalar-unpack
    (($a externref) ($b externref) ($c externref) ($d externref) ($arguments i32))
    (i64 i64 i64 i64 i32)
  (:export-raw "native-scalar-unpack")
  (:locals ($status i32) ($hit i32))
  (apply #'runtime-wat-sequence
    (loop for value in '($a $b $c $d) for index from 0 collect
      (runtime-wat-sequence
      #@(call $native-scalar-decode (call $native-checked-value (local.get ,value))
        (i32.and (i32.shr_u (local.get $arguments) (i32.const ,(* 3 index))) (i32.const 7)))
      #@(local.set $hit)
      #@(local.set $status (i32.or (local.get $status)
        (i32.shl (local.get $hit) (i32.const ,index)))))))
  #@(local.get $status))

(define-runtime-function $native-scalar-box
    (($bits i64) ($kind i32) ($boxed externref) ($expected i32)) (externref)
  (:export-raw "native-scalar-box")
  (wat-fail-when #@(i32.and (i32.ne (local.get $kind) (i32.const 0))
    (i32.ne (local.get $kind) (local.get $expected))) :malformed-form)
  #@(if (i32.eq (local.get $kind) (i32.const 1))
    (then (return (extern.convert_any (call $int64 (local.get $bits))))))
  #@(if (i32.eq (local.get $kind) (i32.const 2))
    (then
      ,(wat-fail-when #@(i64.gt_u (local.get $bits) (i64.const 1114111)) :malformed-form)
      (return (extern.convert_any (call $node (i32.const 16)
        (call $array-checked-character-code (i32.wrap_i64 (local.get $bits)))
        (ref.null eq) (ref.null eq) (ref.null eq))))))
  #@(if (i32.le_u (i32.sub (local.get $kind) (i32.const 3)) (i32.const 1))
    (then (return (extern.convert_any (call $float (local.get $kind)
      (f64.reinterpret_i64 (local.get $bits)))))))
  (wat-fail-when #@(local.get $kind) :malformed-form)
  #@(extern.convert_any (call $native-checked-value (local.get $boxed))))

(define-runtime-function $native-scalar-call2
    (($function externref) ($left externref) ($right externref)) (i64 i32 externref)
  (:export-raw "native-scalar-call2")
  #@(return_call $native-scalar-call (local.get $function) (local.get $left) (local.get $right)
    (ref.null extern) (ref.null extern) (i32.const 2) (i32.const 1)))

(define-runtime-function $native-scalar-call
    (($function externref) ($left externref) ($right externref) ($third externref) ($fourth externref)
     ($arity i32) ($kind i32)) (i64 i32 externref)
  (:export-raw "native-scalar-call")
  #@(return_call $native-scalar-call-with-operands (local.get $function)
    (local.get $left) (local.get $right) (local.get $third) (local.get $fourth)
    (local.get $arity) (local.get $kind) (i64.const 0) (i64.const 0) (i64.const 0) (i32.const 0)))

(define-runtime-function $native-scalar-validate-operand
    (($kind i32) ($bits i64)) (i64)
  (wat-fail-when #@(i32.gt_u (local.get $kind) (i32.const 6)) :malformed-form)
  #@(if (i32.eq (local.get $kind) (i32.const 2))
    (then
      ,(wat-fail-when #@(i64.gt_u (local.get $bits) (i64.const 1114111)) :malformed-form)
      (drop (call $array-checked-character-code (i32.wrap_i64 (local.get $bits))))))
  #@(if (i32.le_u (i32.sub (local.get $kind) (i32.const 3)) (i32.const 1))
    (then (return (i64.reinterpret_f64
      (call $checked-float (local.get $kind) (f64.reinterpret_i64 (local.get $bits)))))))
  #@(if (i32.eq (local.get $kind) (i32.const 5))
    (then ,(wat-fail-when #@(i64.gt_u (i64.add (local.get $bits) (i64.const 1073741824))
      (i64.const 2147483647)) :malformed-form)))
  #@(if (i32.eq (local.get $kind) (i32.const 6))
    (then ,(wat-fail-when #@(i64.gt_u (local.get $bits) (i64.const 1)) :malformed-form)))
  #@(local.get $bits))

(define-runtime-function $native-scalar-materialize-operand (($kind i32) ($bits i64)) (:nullable-object)
  #@(if (i32.eq (local.get $kind) (i32.const 1))
    (then (return (call $box-int64 (local.get $bits)))))
  #@(if (i32.ge_u (local.get $kind) (i32.const 5))
    (then (return (ref.i31 (i32.wrap_i64 (local.get $bits))))))
  #@(call $native-checked-value (call $native-scalar-box
    (local.get $bits) (local.get $kind) (ref.null extern) (local.get $kind))))

(defun native-scalar-public-operands-wat (body)
  (apply #'runtime-wat-sequence
    (loop for position from 0
          for object in '($a $b $c)
          for bits in '($input-a $input-b $input-c)
          for raw in '($raw-b $raw-c $raw-d)
          collect (funcall body position object bits raw))))

(define-runtime-function $native-scalar-materialize-operands
    (($a :nullable-object) ($b :nullable-object) ($c :nullable-object)
     ($input-a i64) ($input-b i64) ($input-c i64) ($input i32) ($omitted i32))
    (:nullable-object :nullable-object :nullable-object)
  (native-scalar-public-operands-wat
    (lambda (position object bits raw)
      (declare (ignore raw))
      #@(if (result (ref null eq)) (i32.and (local.get $omitted) (i32.const ,(ash 1 position)))
          (then (call $native-scalar-materialize-operand
            (i32.and (i32.shr_u (local.get $input) (i32.const ,(* 3 position))) (i32.const 7))
            (local.get ,bits)))
          (else (local.get ,object))))))

(defun native-scalar-materialize-call-wat ()
  (runtime-wat-sequence
    #@(call $native-scalar-materialize-operands (local.get $a) (local.get $b) (local.get $c)
      (local.get $input-a) (local.get $input-b) (local.get $input-c)
      (local.get $input) (local.get $omitted))
    #@(local.set $c) #@(local.set $b) #@(local.set $a)))

(define-runtime-function $native-scalar-call-with-operands
    (($function externref) ($left externref) ($right externref) ($third externref) ($fourth externref)
     ($arity i32) ($kind i32) ($input-a i64) ($input-b i64) ($input-c i64) ($input i32)) (i64 i32 externref)
  (:export-raw "native-scalar-call-with-operands")
  (:locals ($name :nullable-object) ($a :nullable-object) ($b :nullable-object)
           ($c :nullable-object) ($d :nullable-object)
           ($selected :nullable-object) ($body :nullable-object) ($method :nullable-object)
           ($rest :nullable-object) ($next :nullable-object) ($generic-probe i32) ($worker-arity i32)
           ($input-kind i32) ($omitted i32) ($arguments i32) ($worker-kind i32)
           ($raw-a i64) ($raw-b i64) ($raw-c i64) ($raw-d i64) ($valid i32)
           ($owner (ref $native-function)) ($worker (ref $native-scalar-worker))
           ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object) ($restart-associations :nullable-object)
           ($catches :nullable-object) ($targets :nullable-object) ($thrown i32)
           ($backtrace-depth i32) ($backtrace-slot i32) ($backtrace-frame :nullable-object)
           ($result :nullable-object) ($raw i64) ($status i32) ($boxed externref))
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $arity) (i32.const 4))
    (i32.gt_u (i32.sub (local.get $kind) (i32.const 1)) (i32.const 3))) :malformed-form)
  (apply #'runtime-wat-sequence
    (loop for value in '($left $right $third $fourth) for index from 0 collect
      (wat-fail-when #@(i32.and (i32.le_u (local.get $arity) (i32.const ,index))
                       (i32.eqz (ref.is_null (local.get ,value)))) :malformed-form)))
  #@(local.set $name (call $native-checked-value (local.get $function)))
  #@(local.set $a (call $native-checked-value (local.get $left)))
  #@(local.set $b (call $native-checked-value (local.get $right)))
  #@(local.set $c (call $native-checked-value (local.get $third)))
  #@(local.set $d (call $native-checked-value (local.get $fourth)))
  ;; Three three-bit public kinds, with zero for a boxed slot. Public slots
  ;; start at worker parameter one, after the hidden method context.
  (wat-fail-when #@(i32.ge_u (local.get $input) (i32.shl (i32.const 1)
    (i32.mul (i32.const 3) (local.get $arity)))) :malformed-form)
  (wat-fail-when #@(i32.and (i32.eq (local.get $arity) (i32.const 4))
    (i32.ne (local.get $input) (i32.const 0))) :malformed-form)
  (native-scalar-public-operands-wat
    (lambda (position object bits raw)
      (declare (ignore raw))
      (runtime-wat-sequence
        #@(local.set $input-kind (i32.and (i32.shr_u (local.get $input)
          (i32.const ,(* 3 position))) (i32.const 7)))
        #@(if (local.get $input-kind)
          (then
            ,(wat-fail-when #@(i32.eqz (ref.is_null (local.get ,object))) :malformed-form)
            (local.set ,bits (call $native-scalar-validate-operand (local.get $input-kind) (local.get ,bits)))
            (local.set $omitted (i32.or (local.get $omitted) (i32.const ,(ash 1 position)))))
          (else ,(wat-fail-when #@(i64.ne (local.get ,bits) (i64.const 0)) :malformed-form))))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  ;; If the captured callable is an ordinary native function, reconstruct
  ;; its input objects and retain its independently admitted result path.
  #@(if (i32.and (i32.ne (local.get $omitted) (i32.const 0))
        (ref.test (ref $native-function) (local.get $name)))
    (then
      ,(native-scalar-materialize-call-wat)
      (local.set $left (extern.convert_any (local.get $a)))
      (local.set $right (extern.convert_any (local.get $b)))
      (local.set $third (extern.convert_any (local.get $c)))
      (local.set $omitted (i32.const 0))
      (local.set $input (i32.const 0))))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (local.set $owner (if (result (ref $native-function)) (ref.test (ref $native-function) (local.get $name))
      (then (ref.cast (ref $native-function) (local.get $name)))
      (else
        (br_if $ordinary (i32.ne (call $native-builtin-id (local.get $name)) (i32.const 2147483644)))
        (br_if $ordinary (i32.ge_u (local.get $arity) (i32.const 4)))
        ;; Validation may yield. Select from the live cache afterwards and
        ;; keep selection, worker admission and entry in one ready interval.
        (drop (call $generic-dispatch-metadata-value (call $int (i32.const 2))
          (local.get $name) (ref.null eq)))
        (local.set $generic-probe (select (i32.const 1) (i32.const 2) (local.get $input)))
        (call $generic-recent-plan-ready (local.get $name) (local.get $arity)
          (local.get $a) (local.get $b) (local.get $c) (local.get $d) (local.get $omitted))
        (local.set $next) (local.set $rest) (local.set $method)
        (local.set $body) (local.set $selected)
        (br_if $ordinary (ref.is_null (local.get $selected)))
        ;; Mode 8 denotes an effective plan consisting of one primary.
        (br_if $ordinary (i32.ne (struct.get $direct-function $mode
          (ref.cast (ref $direct-function) (local.get $selected))) (i32.const 8)))
        (ref.cast (ref $native-function) (local.get $body)))))
    (local.set $worker (br_on_null $ordinary
      (array.get $native-scalar-registry (global.get $native-scalar-workers)
        (call $native-scalar-slot (local.get $owner)))))
    (br_if $ordinary (i32.eqz (ref.eq (local.get $owner) (struct.get $native-scalar-worker $owner (local.get $worker)))))
    (br_if $ordinary (i32.ne (local.get $kind) (struct.get $native-scalar-worker $result-kind (local.get $worker))))
    (local.set $worker-arity (i32.add (local.get $arity) (i32.eqz (ref.is_null (local.get $selected)))))
    (br_if $ordinary (f64.ne (struct.get $obj $f (local.get $owner))
      (f64.convert_i32_u (i32.add (local.get $worker-arity) (i32.const 2)))))
    (br_if $ordinary (i32.eqz (ref.eq (struct.get $native-function $direct (local.get $owner))
      (struct.get $native-scalar-worker $direct (local.get $worker)))))
    (br_if $ordinary (i32.eqz (ref.eq (struct.get $obj $a (local.get $owner))
      (struct.get $native-scalar-worker $literals (local.get $worker)))))
    (br_if $ordinary (i32.eqz (ref.eq (struct.get $obj $b (local.get $owner))
      (struct.get $native-scalar-worker $captures (local.get $worker)))))
    (local.set $arguments (struct.get $native-scalar-worker $arguments (local.get $worker)))
    (br_if $ordinary (i32.and (i32.shl (local.get $omitted) (i32.const 13))
      (i32.xor (local.get $arguments) (i32.const -1))))
    ,(native-scalar-public-operands-wat
      (lambda (position object bits raw)
        (declare (ignore object bits raw))
        #@(if (i32.and (local.get $omitted) (i32.const ,(ash 1 position)))
          (then
            (local.set $worker-kind (i32.and (i32.shr_u (local.get $arguments)
              (i32.const ,(* 3 (1+ position)))) (i32.const 7)))
            ;; Kind zero is admissible only for an audited unused parameter.
            (br_if $ordinary (i32.and (i32.ne (local.get $worker-kind) (i32.const 0))
              (i32.ne (local.get $worker-kind) (i32.and (i32.shr_u (local.get $input)
                (i32.const ,(* 3 position))) (i32.const 7)))))))))
    (if (i32.eqz (ref.is_null (local.get $selected)))
      (then
        ;; Method entries put the lexical next-method context before the
        ;; public operands. Its construction neither calls Lisp nor polls.
        (local.set $function (extern.convert_any (local.get $owner)))
        (local.set $fourth (local.get $third))
        (local.set $third (local.get $right))
        (local.set $right (local.get $left))
        (local.set $left (if (result externref) (local.get $input)
          (then (ref.null extern))
          (else (extern.convert_any (call $generic-recent-context
            (local.get $name) (local.get $owner) (local.get $method) (local.get $rest)
            (local.get $next) (local.get $arity)
            (local.get $a) (local.get $b) (local.get $c))))))))
    (local.set $backtrace-depth (global.get $backtrace-depth))
    (local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
    (local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
    (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $owner))
    (global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
    (call $clear-values)
    (drop ,(wat-dynamic-call
      #@(block (result (ref null eq))
        (call_ref $native-scalar-entry
          (local.get $function) (local.get $left) (local.get $right) (local.get $third) (local.get $fourth)
          (if (result i64 i64 i64 i64 i32) (local.get $input)
            (then
              (call $native-scalar-unpack (local.get $left) (local.get $right)
                (local.get $third) (local.get $fourth) (local.get $arguments))
              (local.set $valid) (local.set $raw-d) (local.set $raw-c)
              (local.set $raw-b) (local.set $raw-a)
              (local.get $raw-a)
              ,(native-scalar-public-operands-wat
                (lambda (position object bits raw)
                  (declare (ignore object))
                  #@(select (local.get ,bits) (local.get ,raw)
                    (i32.and (local.get $omitted) (i32.const ,(ash 1 position))))))
              (i32.or (local.get $valid) (i32.shl (local.get $omitted) (i32.const 1))))
            (else (call $native-scalar-unpack (local.get $left) (local.get $right)
              (local.get $third) (local.get $fourth) (local.get $arguments))))
          (struct.get $native-scalar-worker $entry (local.get $worker)))
        (local.set $boxed) (local.set $status) (local.set $raw)
        (if (i32.ne (local.get $kind) (local.get $status))
          (then
            (local.set $boxed (call $native-scalar-box (local.get $raw) (local.get $status)
              (local.get $boxed) (local.get $kind)))
            (local.set $status (i32.const 0)))
          (else
            ;; A bytecode claim authenticates the wrapper relationship. A
            ;; malformed raw result must still fail wherever boxing would.
            (if (i32.eq (local.get $status) (i32.const 2))
              (then
                ,(wat-fail-when #@(i64.gt_u (local.get $raw) (i64.const 1114111)) :malformed-form)
                (drop (call $array-checked-character-code (i32.wrap_i64 (local.get $raw)))))
              (else
                (if (i32.ge_u (local.get $status) (i32.const 3))
                  (then (local.set $raw (i64.reinterpret_f64
                    (call $checked-float (local.get $status) (f64.reinterpret_i64 (local.get $raw)))))))))))
        (ref.null eq))
      :cleanup #@(block
        (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
        (global.set $backtrace-depth (local.get $backtrace-depth)))))
    (call $clear-values)
    (if (i32.eq (local.get $status) (local.get $kind))
      (then (return (local.get $raw) (i32.const 1) (ref.null extern))))
    (return (i64.const 0) (i32.const 0) (local.get $boxed)))
  ;; This producer is admitted only for primary demand. Use ordinary
  ;; invocation, including arity handling, with the same evaluated operands.
  #@(if (local.get $omitted)
    (then
      ,(native-scalar-materialize-call-wat)
      ;; A raw miss resumes live boxed dispatch with the original values.
      (local.set $selected (ref.null eq))))
  #@(local.set $result (if (result (ref null eq)) (ref.is_null (local.get $selected))
    (then (call $native-fixed-apply-ready (local.get $name) (local.get $a) (local.get $b)
      (local.get $c) (local.get $d) (local.get $arity) (local.get $generic-probe)))
    (else (call $generic-recent-invoke-ready (local.get $name) (local.get $arity)
      (local.get $a) (local.get $b) (local.get $c)
      (ref.cast (ref $direct-function) (local.get $selected))
      (local.get $body) (local.get $method) (local.get $rest) (local.get $next)))))
  #@(call $clear-values)
  #@(i64.const 0)
  #@(i32.const 0)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-call0 (($function externref)) (externref)
  (:export-raw "native-call0")
  (:locals ($direct-result :nullable-object) ($hit i32))
  #@(drop (call $native-checked-value (local.get $function)))
  (native-direct-call-wat 0)
  (native-generic-fast-call-wat 0)
  (native-fixed-vector-call-wat 0)
  #@(extern.convert_any (call $native-primary-apply
    ,(native-object '(local.get $function)) (ref.null eq))))

(define-runtime-function $native-call-values1-ready
    (($function externref) ($value externref)) (:nullable-object)
  (:locals ($function-object :nullable-object) ($a :nullable-object) ($result :nullable-object)
           ($id i32) ($hit i32) ($native-function (ref $native-function)))
  ;; This raw export has the same checked Lisp-value boundary as NATIVE-CALL.
  ;; Preserve the public scalar worker ABI and its ordinary frame/cleanup owner.
  #@(local.set $function-object (call $native-checked-value (local.get $function)))
  #@(local.set $a (call $native-checked-value (local.get $value)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(block $called
    (if (ref.test (ref $native-function) (local.get $function-object))
      (then
        (local.set $native-function (ref.cast (ref $native-function) (local.get $function-object)))
        (if (f64.eq (struct.get $obj $f (local.get $native-function)) (f64.const 3))
          (then
            (if (ref.is_null (struct.get $native-function $direct (local.get $native-function)))
              (then (local.set $result (call $apply (local.get $function-object)
                (array.new_fixed $objects 1 (local.get $a)))))
              (else (local.set $result (ref.cast (ref null eq) (any.convert_extern
                (call $native-direct-apply-ready (local.get $native-function)
                  (local.get $a) (ref.null eq) (ref.null eq) (ref.null eq) (i32.const 0)))))))
            (br $called)))))
    (local.set $id (call $native-builtin-id (local.get $function-object)))
    (if (i32.eq (local.get $id) (i32.const 2147483644))
      (then
        (block $skip-recent
          ;; A cached EQL profile can only skip this optional probe. Unknown
          ;; shapes retain the full selector. RESULT is scratch until either
          ;; the selector or ordinary APPLY overwrites it with a call result.
          (block $profile-unknown
            (local.set $result (struct.get $obj $a (ref.cast (ref $obj) (local.get $function-object))))
            (br_if $profile-unknown (i32.eqz (ref.test (ref $obj) (local.get $result))))
            (local.set $result (struct.get $obj $c (ref.cast (ref $obj) (local.get $result))))
            (br_if $profile-unknown (i32.eqz (ref.test (ref $objects) (local.get $result))))
            (br_if $profile-unknown (i32.ne (array.len (ref.cast (ref $objects) (local.get $result)))
              (i32.const ,*generic-property-cache-size*)))
            (br_if $profile-unknown (i32.eqz (ref.eq
              (array.get $objects (ref.cast (ref $objects) (local.get $result)) (i32.const 0))
              (call $symbol (i32.const 1)))))
            (local.set $result (array.get $objects (ref.cast (ref $objects) (local.get $result))
              (i32.const ,(1+ (position :eql-dispatch-profile *generic-property-cache-keys*)))))
            (br_if $profile-unknown (i32.eqz (ref.test (ref $obj) (local.get $result))))
            (br_if $profile-unknown (i32.ne (call $tag (local.get $result)) (i32.const 7)))
            (local.set $result (struct.get $obj $a (ref.cast (ref $obj) (local.get $result))))
            (br_if $profile-unknown (i32.eqz (ref.test (ref $obj) (local.get $result))))
            (br_if $profile-unknown (i32.ne (call $tag (local.get $result)) (i32.const 7)))
            (br_if $skip-recent (i32.eqz (ref.is_null
              (struct.get $obj $a (ref.cast (ref $obj) (local.get $result)))))))
          (call $generic-recent-call-ready (local.get $function-object) (i32.const 1)
            (local.get $a) (ref.null eq) (ref.null eq) (ref.null eq))
          (local.set $hit) (local.set $result)
          (if (local.get $hit) (then (br $called))))))
    (local.set $result (call $apply (local.get $function-object)
      ,(native-fixed-fallback-arguments-wat 1))))
  #@(local.get $result))

(define-runtime-function $native-capture-result
    (($result :nullable-object) ($compact i32)) (externref externref externref externref i32)
  (:locals ($b :nullable-object) ($c :nullable-object) ($extra :nullable-object) ($count i32))
  #@(if (result (ref null eq) (ref null eq) (ref null eq) i32) (local.get $compact)
    (then (call $capture-values))
    (else (call $capture-values-list)))
  #@(local.set $count) #@(local.set $extra) #@(local.set $c) #@(local.set $b)
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result))
  #@(extern.convert_any (local.get $b)) #@(extern.convert_any (local.get $c))
  #@(extern.convert_any (local.get $extra)) #@(local.get $count))

(define-runtime-function $native-call-values1
    (($function externref) ($value externref)) (externref externref externref externref i32)
  (:export-raw "native-call-values1")
  #@(call $native-capture-result
    (call $native-call-values1-ready (local.get $function) (local.get $value)) (i32.const 0)))

(define-runtime-function $native-call-captured1
    (($function externref) ($value externref)) (externref externref externref externref i32)
  (:export-raw "native-call-captured1")
  #@(call $native-capture-result
    (call $native-call-values1-ready (local.get $function) (local.get $value)) (i32.const 1)))

(define-runtime-function $native-call1 (($function externref) ($value externref)) (externref)
  (:export-raw "native-call1")
  (:locals ($direct-result :nullable-object) ($hit i32) ($id i32) ($tag i32) ($accessor i32)
           ($a :nullable-object) ($function-object :nullable-object) ($native-function (ref $native-function)))
  #@(local.set $a (call $native-checked-value (local.get $value)))
  #@(local.set $function-object (call $native-checked-value (local.get $function)))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (ref.test (ref $native-function) (local.get $function-object))
    (then (if (f64.eq (struct.get $obj $f
        (local.tee $native-function (ref.cast (ref $native-function) (local.get $function-object)))) (f64.const 3))
      (then ,(native-direct-call-wat 1 t '(local.get $function-object) '((local.get $a)) '(local.get $native-function) t)
        (return (extern.convert_any (call $native-primary-apply
        (local.get $function-object) (array.new_fixed $objects 1 (local.get $a)))))))))
  #@(local.set $id (call $native-builtin-id (local.get $function-object)))
  #@(local.set $tag (call $tag (local.get $a)))
  (native-fixed-vector-call-wat 1)
  (wat-symbol-case #@(local.get $id)
    ((not null) #@(return (extern.convert_any (call $bool (ref.is_null (local.get $a))))))
    (identity #@(return (local.get $value)))
    ;; Dispatch on the actual saved builtin, retaining replacement closures.
    ;; STRING preserves string identity, including displaced/fill-pointer views.
    (string #@(return (extern.convert_any (call $string-designator (local.get $a)))))
    (symbol-name #@(return (extern.convert_any (call $symbol-name (local.get $a)))))
    (list #@(return (extern.convert_any (call $cons (local.get $a) (ref.null eq)))))
    (char-upcase #@(return (extern.convert_any
      (call $character-case-native (local.get $a) (i32.const 0)))))
    (char-downcase #@(return (extern.convert_any
      (call $character-case-native (local.get $a) (i32.const 1)))))
    (unicode-char-upcase #@(return (extern.convert_any
      (call $character-case-native (local.get $a) (i32.const 2)))))
    (unicode-char-downcase #@(return (extern.convert_any
      (call $character-case-native (local.get $a) (i32.const 3)))))
    (alphanumericp #@(return (extern.convert_any
      (call $character-property-apply (local.get $id) (local.get $a)))))
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
    ((oddp evenp)
      ;; Dispatch on the saved builtin function object, rather than its current
      ;; global binding. The ordinary checked predicate handles all integers
      ;; and reports noninteger arguments before EVENP inverts its result.
      #@(return (extern.convert_any (call $bool (i32.xor
        (i32.eqz (ref.is_null (call $oddp-native (local.get $a))))
        (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'evenp))))))))
    (numberp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 4))))))
    (rationalp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 1))))))
    (realp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 3))))))
    (complexp #@(return (extern.convert_any (call $bool (i32.eq (local.get $tag) (i32.const 5))))))
    (abs
      ;; Validate before the shared magnitude helpers inspect numeric fields.
      ;; REAL-ABSOLUTE preserves the target's real type and signed-zero rules.
      #@(local.set $a (call $number (local.get $a)))
      #@(local.set $direct-result
        (if (result (ref eq)) (i32.eq (local.get $tag) (i32.const 5))
          (then (call $complex-absolute (ref.as_non_null (local.get $a))))
          (else (call $real-absolute (ref.as_non_null (local.get $a))))))
      #@(call $clear-values)
      #@(return (extern.convert_any (local.get $direct-result))))
    (signum
      #@(local.set $direct-result (call $root-signum-native (local.get $a) (i32.const 1)))
      #@(call $clear-values)
      #@(return (extern.convert_any (local.get $direct-result))))
    ((floor ceiling truncate round)
      #@(if (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 3))
        (then
          (call $native-rounding-primary-inline (local.get $value)
            (extern.convert_any (call $int (i32.const 1)))
            (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'floor))))
          (call $clear-values)
          (return))))
    (floatp #@(return (extern.convert_any (call $bool
      (i32.le_u (i32.sub (local.get $tag) (i32.const 3)) (i32.const 1))))))
    ((1+ 1-)
      #@(if (i32.le_u (i32.sub (local.get $tag) (i32.const 1)) (i32.const 4))
        (then (return (extern.convert_any (call $number-step (local.get $a)
          (select (i32.const 1) (i32.const -1)
            (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '1+))))))))))
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
  (native-fixed-primitive-call-wat 1)
  (native-generic-fast-call-wat 1 t)
  (native-symbol-fixed-call-wat 1)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    ,(native-fixed-fallback-arguments-wat 1))))

;; A canonical builtin and two actual i31 values prove these operations exact.
;; Signed i31 addition fits i32; INT retains promotion beyond the i31 range.
;; Misses leave the ordinary checked dispatcher, including its poll, in place.
(defun native-small-number-call-wat ()
  (runtime-wat-sequence
    #@(block $small-miss (result (ref null eq))
      (br_if $small-miss (ref.null eq)
        (i32.and (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id '+)))
                 (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id '<)))))
      (local.set $small-left
        (br_on_cast_fail $small-miss (ref null eq) (ref i31) (local.get $a)))
      (local.set $small-right
        (br_on_cast_fail $small-miss (ref null eq) (ref i31) (local.get $b)))
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '+)))
        (then (return (extern.convert_any (call $int
          (i32.add (i31.get_s (local.get $small-left)) (i31.get_s (local.get $small-right)))))))
        (else (return (extern.convert_any (call $bool
          (i32.lt_s (i31.get_s (local.get $small-left)) (i31.get_s (local.get $small-right)))))))))
    #@(drop)))

(defun native-fixed-integer-bits-wat (arity)
  (wat-integer-branch '(local.get $id)
    (loop for (name) in *variadic-integer-bit-operations*
          for id = (runtime-symbol-id name)
          collect (if (= arity 2) id (- -2 id)))
    (runtime-wat-sequence
      #@(local.set $direct-result (call $fixed-scalar-integer-bits (local.get $function-object)
        ,(if (= arity 2) '(local.get $id) '(i32.sub (i32.const -2) (local.get $id)))
        ,@(if (= arity 2) '((local.get $a) (local.get $b) (ref.null eq))
              (list (native-object '(local.get $a)) '(local.get $array) '(local.get $subscript)))
        (i32.const ,(if (= arity 2) 0 1))))
      #@(if (i32.eqz (ref.is_null (local.get $direct-result)))
        (then (return (extern.convert_any (local.get $direct-result))))))))

(define-runtime-function $native-call2
    (($function externref) ($left externref) ($right externref)) (externref)
  (:export-raw "native-call2")
  #@(return_call $native-call2-checked
    (call $native-checked-value (local.get $function))
    (call $native-checked-value (local.get $left))
    (call $native-checked-value (local.get $right))))

;; Each entry has checked the Lisp-value shape of all three references.
;; The actual callable's ID is read before polling, while dispatch observes
;; live bindings, frames and array storage after the checkpoint.
(define-runtime-function $native-call2-checked
    (($function-object :nullable-object) ($a :nullable-object) ($b :nullable-object)) (externref)
  (:locals ($id i32) ($small-left (ref i31)) ($small-right (ref i31)))
  #@(local.set $id (call $native-builtin-id (local.get $function-object)))
  (native-small-number-call-wat)
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(return_call $native-call2-ready
    (local.get $function-object) (local.get $a) (local.get $b) (local.get $id) (i32.const 0)))

(define-runtime-function $native-call2-ready
    (($function-object :nullable-object) ($a :nullable-object) ($b :nullable-object) ($id i32) ($check-array i32))
    (externref)
  (:locals ($direct-result :nullable-object) ($hit i32) ($at i32) ($bt i32) ($index i32) ($cmp i32)
           ($cell :nullable-object) ($vector (ref $array)))
  #@(if (ref.test (ref $native-function) (local.get $function-object))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) (local.get $function-object))) (f64.const 4))
      (then ,(native-direct-call-wat 2 t '(local.get $function-object) '((local.get $a) (local.get $b)) nil t)
        (return (extern.convert_any (call $native-primary-apply
        (local.get $function-object) (array.new_fixed $objects 2 (local.get $a) (local.get $b)))))))))
  (wat-symbol-branch '(local.get $id) '(byte ldb ldb-test mask-field)
    (wat-fragment
      #@(if (i32.and (i32.eqz (global.get $debug-evaluating))
          (ref.eq (local.get $function-object)
            (array.get $objects (global.get $canonical-builtin-functions) (i32.shl (local.get $id) (i32.const 1)))))
        (then (return_call $fixed-byte-apply (local.get $function-object)
          (local.get $id) (local.get $a) (local.get $b))))))
  ;; Exact native workers need only checked Lisp values, not builtin tags.
  #@(local.set $at (call $tag (local.get $a)))
  #@(local.set $bt (call $tag (local.get $b)))
  (native-fixed-vector-call-wat 2)
  (wat-symbol-case #@(local.get $id)
    (eq #@(return (extern.convert_any (call $bool (ref.eq (local.get $a) (local.get $b))))))
    (eql #@(return (extern.convert_any (call $bool (call $eql (local.get $a) (local.get $b))))))
    (string= #@(return (extern.convert_any (call $bool (call $string-equal
      (call $string-contents (call $string-designator (local.get $a)))
      (call $string-contents (call $string-designator (local.get $b))))))))
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
    (/
      #@(if (i32.and (i32.le_u (i32.sub (local.get $at) (i32.const 1)) (i32.const 4))
                     (i32.le_u (i32.sub (local.get $bt) (i32.const 1)) (i32.const 4)))
        (then
          (local.set $direct-result (call $op (i32.const 3) (local.get $a) (local.get $b)))
          (call $clear-values)
          (return (extern.convert_any (local.get $direct-result))))))
    ((floor ceiling truncate round)
      ;; This ABI consumes only the primary value. Reuse the ordinary exact
      ;; rounding operation, avoiding APPLY's argument and second-value packets.
      ;; Complex/non-number inputs retain the checked general call path.
      #@(if (i32.and (i32.le_u (i32.sub (local.get $at) (i32.const 1)) (i32.const 3))
                     (i32.le_u (i32.sub (local.get $bt) (i32.const 1)) (i32.const 3)))
        (then
          (call $native-rounding-primary-inline
            (extern.convert_any (local.get $a)) (extern.convert_any (local.get $b))
            (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'floor))))
          (call $clear-values)
          (return))))
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
  #@(if (ref.test (ref $direct-function) (local.get $function-object))
    (then (if (i32.eq (struct.get $direct-function $mode
        (ref.cast (ref $direct-function) (local.get $function-object))) (i32.const 1))
      (then (return (extern.convert_any (call $structure-access (local.get $b)
        (struct.get $direct-function $owner (ref.cast (ref $direct-function) (local.get $function-object)))
        (struct.get $direct-function $slot (ref.cast (ref $direct-function) (local.get $function-object)))
        (local.get $a) (i32.const 1))))))))
  ;; Specialized rank-one arrays retain their live dimension and backing.
  ;; SVREF's SIMPLE-VECTOR contract stays on its ordinary path.
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'aref)))
    (then
      (local.set $direct-result (call $array-owned-rank1-fixnum-element
        (local.get $a) (local.get $b) (ref.null eq) (i32.const 0)))
      (if (i32.eqz (ref.is_null (local.get $direct-result)))
        (then (call $clear-values) (return (extern.convert_any (local.get $direct-result)))))
      (local.set $index (call $array-rank1-index (local.get $a) (local.get $b)))
      (if (i32.ge_s (local.get $index) (i32.const 0))
        (then
          (local.set $direct-result (call $array-resolve-element (local.get $a)
            (local.get $index) (ref.null eq) (i32.const 0) (local.get $check-array)))
          (call $clear-values)
          (return (extern.convert_any (local.get $direct-result)))))))
  (native-fixed-primitive-call-wat 2 '((local.get $a) (local.get $b)))
  (native-fixed-integer-bits-wat 2)
  (native-generic-fast-call-wat 2 t '(local.get $function-object) '((local.get $a) (local.get $b)))
  (native-symbol-fixed-call-wat 2 '(local.get $function-object))
  #@(extern.convert_any (call $native-primary-apply (local.get $function-object)
    ,(native-fixed-fallback-arguments-wat 2))))

(define-runtime-function $fixed-deposit-apply
    (($function :nullable-object) ($id i32)
     ($new :nullable-object) ($spec :nullable-object) ($base :nullable-object)) (externref)
  (:locals ($result :nullable-object))
  ;; CALL3 has completed its entry checkpoint and selected ordinary APPLY.
  ;; APPLY0's checkpoint precedes the live bytespec reads and any fast result.
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(if (i32.and (i32.eqz (global.get $debug-evaluating))
      (i32.and (ref.eq (local.get $function)
        (array.get $objects (global.get $canonical-builtin-functions) (i32.shl (local.get $id) (i32.const 1))))
        (i32.eq (call $native-builtin-id (local.get $function)) (local.get $id))))
    (then
      (local.set $result (call $try-deposit-scalar (local.get $new) (local.get $spec)
        (local.get $base) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'deposit-field)))))))
  #@(if (ref.is_null (local.get $result))
    (then (local.set $result (call $apply0-ready (local.get $function)
      (call $cons (local.get $new) (call $cons2 (local.get $spec) (local.get $base) (ref.null eq)))))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-call3
    (($function externref) ($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-call3")
  (:locals ($function-object :nullable-object))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $function-object (call $native-checked-value (local.get $function)))
  #@(drop (call $native-checked-value (local.get $a)))
  #@(drop (call $native-checked-value (local.get $b)))
  #@(drop (call $native-checked-value (local.get $c)))
  #@(return_call $native-call3-ready (local.get $function-object)
    (local.get $function) (local.get $a) (local.get $b) (local.get $c) (i32.const 0)))

(define-runtime-function $native-call3-ready
    (($function-object :nullable-object) ($function externref) ($a externref) ($b externref) ($c externref)
     ($check-array i32))
    (externref)
  (:locals ($direct-result :nullable-object) ($hit i32) ($id i32) ($index i32)
           ($array :nullable-object) ($subscript :nullable-object))
  #@(local.set $id (i32.sub (i32.const -2)
    (call $native-builtin-id (local.get $function-object))))
  (wat-integer-branch '(local.get $id)
    (mapcar (lambda (name) (- -2 (runtime-symbol-id name))) '(dpb deposit-field))
    (runtime-wat-sequence
      #@(if (i32.and (i32.eqz (global.get $debug-evaluating))
          (ref.eq (local.get $function-object) (array.get $objects
            (global.get $canonical-builtin-functions)
            (i32.shl (i32.sub (i32.const -2) (local.get $id)) (i32.const 1)))))
        (then (return_call $fixed-deposit-apply (local.get $function-object)
          (i32.sub (i32.const -2) (local.get $id))
          ,(native-object '(local.get $a)) ,(native-object '(local.get $b)) ,(native-object '(local.get $c)))))))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 5))
      (then ,(native-direct-call-wat 3 t nil nil nil t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 3
          ,(native-object '(local.get $a)) ,(native-object '(local.get $b)) ,(native-object '(local.get $c))))))))))
  (native-fixed-vector-call-wat 3)
  ;; Ordinary AREF has the negative identity after this dispatcher's setter
  ;; transform. Probe only after the checkpoint, since adjustment can replace
  ;; dimensions and backing storage there. Misses keep the complete old path.
  #@(if (i32.eq (local.get $id) (i32.const ,(- -2 (runtime-symbol-id 'aref))))
    (then
      (local.set $index (call $array-rank2-index ,(native-object '(local.get $a))
        ,(native-object '(local.get $b)) ,(native-object '(local.get $c))))
      (if (i32.ge_s (local.get $index) (i32.const 0))
        (then
          (local.set $direct-result (call $array-raw-element ,(native-object '(local.get $a))
            (local.get $index) (ref.null eq) (i32.const 0)))
          (call $clear-values)
          (return (extern.convert_any (local.get $direct-result)))))))
  #@(if (i32.eq (local.get $id) (i32.const ,(- -2 (runtime-symbol-id 'list))))
    (then (return (extern.convert_any
      (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
        (call $cons ,(native-object '(local.get $c)) (ref.null eq))))))))
  ;; All raw operands were admitted before any fixed worker or builtin path.
  #@(local.set $array ,(native-object '(local.get $b)))
  #@(local.set $subscript ,(native-object '(local.get $c)))
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
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'aref)))
    (then
      (local.set $direct-result (call $array-owned-rank1-fixnum-element
        (local.get $array) (local.get $subscript) ,(native-object '(local.get $a)) (i32.const 1)))
      (if (i32.eqz (ref.is_null (local.get $direct-result)))
        (then (call $clear-values) (return (local.get $a))))
      (local.set $index (call $array-rank1-index (local.get $array) (local.get $subscript)))
      (if (i32.ge_s (local.get $index) (i32.const 0))
        (then
          (drop (call $array-resolve-element (local.get $array) (local.get $index)
            ,(native-object '(local.get $a)) (i32.const 1) (local.get $check-array)))
          (call $clear-values)
          (return (local.get $a))))))
  (native-fixed-primitive-call-wat 3)
  (native-fixed-integer-bits-wat 3)
  (native-generic-fast-call-wat 3 t)
  (native-symbol-fixed-call-wat 3)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    ,(native-fixed-fallback-arguments-wat 3))))

(define-runtime-function $native-call4
    (($function externref) ($a externref) ($b externref) ($c externref) ($d externref)) (externref)
  (:export-raw "native-call4")
  (:locals ($direct-result :nullable-object) ($id i32) ($index i32) ($function-object :nullable-object))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $function-object (call $native-checked-value (local.get $function)))
  #@(drop (call $native-checked-value (local.get $a)))
  #@(drop (call $native-checked-value (local.get $b)))
  #@(drop (call $native-checked-value (local.get $c)))
  #@(drop (call $native-checked-value (local.get $d)))
  #@(if (ref.test (ref $native-function) ,(native-object '(local.get $function)))
    (then (if (f64.eq (struct.get $obj $f (ref.cast (ref $obj) ,(native-object '(local.get $function)))) (f64.const 6))
      (then ,(native-direct-call-wat 4 t nil nil nil t)
        (return (extern.convert_any (call $native-primary-apply
        ,(native-object '(local.get $function)) (array.new_fixed $objects 4
          ,(native-object '(local.get $a)) ,(native-object '(local.get $b))
          ,(native-object '(local.get $c)) ,(native-object '(local.get $d))))))))))
  ;; Failed fixed guards only read this actual function; successful ones return.
  ;; Share its ID from the original first query until the ordinary fallback.
  #@(local.set $id (call $native-builtin-id (local.get $function-object)))
  (native-fixed-vector-call-wat 4)
  #@(if (i32.eq (local.get $id) (i32.const ,(- -2 (runtime-symbol-id 'aref))))
    (then
      (local.set $index (call $array-rank2-index ,(native-object '(local.get $b))
        ,(native-object '(local.get $c)) ,(native-object '(local.get $d))))
      (if (i32.ge_s (local.get $index) (i32.const 0))
        (then
          (drop (call $array-raw-element ,(native-object '(local.get $b))
            (local.get $index) ,(native-object '(local.get $a)) (i32.const 1)))
          (call $clear-values)
          (return (local.get $a))))))
  #@(if (i32.eq (local.get $id)
                 (i32.const ,(runtime-symbol-id 'list)))
    (then (return (extern.convert_any
      (call $cons ,(native-object '(local.get $a)) (call $cons ,(native-object '(local.get $b))
        (call $cons ,(native-object '(local.get $c)) (call $cons ,(native-object '(local.get $d)) (ref.null eq)))))))))
  (native-fixed-primitive-call-wat 4)
  (native-generic-fast-call-wat 4 t)
  (native-symbol-fixed-call-wat 4)
  #@(extern.convert_any (call $native-primary-apply ,(native-object '(local.get $function))
    ,(native-fixed-fallback-arguments-wat 4))))

;; Fixed primary-value calls use exact standard identities independently of
;; inline expansion. The retained helpers are also suitable for NOTINLINE.
;; Lookup uses implementation-owned installed entries, so Lisp functions and
;; live generic protocols keep the same checked invocation paths as builtins.
(define-runtime-function $native-standard-call0 (($code i32)) (externref)
  (:export-raw "native-standard-call0")
  #@(return_call $native-call0
    (extern.convert_any (call $standard-function-code (local.get $code)))))

(define-runtime-function $native-standard-call1 (($code i32) ($value externref)) (externref)
  (:export-raw "native-standard-call1")
  (:locals ($function :nullable-object) ($a :nullable-object) ($id i32) ($result :nullable-object))
  ;; Actual scalar storage proves this checked successor/predecessor path.
  ;; The shared worker promotes at both immediate and signed-i64 endpoints.
  #@(if (i32.and
      (i32.or
        (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '1+))))
        (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '1-)))))
      (i32.or (ref.test (ref i31) (any.convert_extern (local.get $value)))
        (ref.test (ref $fixnum) (any.convert_extern (local.get $value)))))
    (then
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (return (extern.convert_any (call $number-step
        (ref.cast (ref eq) (any.convert_extern (local.get $value)))
        (select (i32.const 1) (i32.const -1)
          (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '1+))))))))))
  #@(local.set $function (call $standard-function-code (local.get $code)))
  #@(local.set $a (call $native-checked-value (local.get $value)))
  #@(local.set $id (i32.sub (local.get $code) (i32.const 1)))
  ;; These operations have no signaling/callback path after ingress checks.
  ;; Shared semantic workers cover the other fixed operations without copies
  ;; of their checks, allocation loops or arbitrary-precision arithmetic.
  #@(block $ordinary
    (block $done
      ,(wat-symbol-case #@(local.get $id)
        (identity #@(local.set $result (local.get $a)) #@(br $done))
        (functionp #@(local.set $result (call $bool
          (i32.eq (call $tag (local.get $a)) (i32.const 8)))) #@(br $done))
        ((atom listp numberp) #@(local.set $result (call $bool
          (call $simple-predicate-value (local.get $id) (local.get $a)))) #@(br $done)))
      (br $ordinary))
    (if (global.get $thread-active) (then (call $thread-checkpoint)))
    (call $clear-values)
    (return (extern.convert_any (local.get $result))))
  #@(return_call $native-call1 (extern.convert_any (local.get $function)) (local.get $value)))

(define-runtime-function $native-standard-call2
    (($code i32) ($left externref) ($right externref)) (externref)
  (:export-raw "native-standard-call2")
  (:locals ($a :nullable-object) ($b :nullable-object))
  ;; The fixed standard identity and actual immediate operands establish both
  ;; the operation and valid Lisp input without repeating callable ingress.
  ;; Keep the ordinary checkpoint before arithmetic and preserve promotion.
  #@(if (i32.and
      (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '+))))
      (i32.and (ref.test (ref i31) (any.convert_extern (local.get $left)))
               (ref.test (ref i31) (any.convert_extern (local.get $right)))))
    (then
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (return (extern.convert_any (call $int64
        (i64.add
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $left)))))
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $right)))))))))))
  ;; A difference of two signed i31 values also fits in i64. Keep this
  ;; separate so addition retains its existing immediate path.
  #@(if (i32.and
      (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '-))))
      (i32.and (ref.test (ref i31) (any.convert_extern (local.get $left)))
               (ref.test (ref i31) (any.convert_extern (local.get $right)))))
    (then
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (return (extern.convert_any (call $int64
        (i64.sub
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $left)))))
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $right)))))))))))
  ;; The product of two signed i31 values fits in signed i64 as well.
  #@(if (i32.and
      (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id '*))))
      (i32.and (ref.test (ref i31) (any.convert_extern (local.get $left)))
               (ref.test (ref i31) (any.convert_extern (local.get $right)))))
    (then
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (return (extern.convert_any (call $int64
        (i64.mul
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $left)))))
          (i64.extend_i32_s (i31.get_s (ref.cast (ref i31)
            (any.convert_extern (local.get $right)))))))))))
  #@(local.set $a (call $native-checked-value (local.get $left)))
  #@(local.set $b (call $native-checked-value (local.get $right)))
  ;; Lookup validates the signed identity. The selected slot still needs
  ;; the same Lisp-value shape check as a raw callable argument.
  #@(return_call $native-call2-checked
    (call $native-checked-value (extern.convert_any (call $standard-function-code (local.get $code))))
    (local.get $a) (local.get $b)))

(define-runtime-function $native-float-pair
    (($op i32) ($kind i32) ($mode i32) ($bits i64) ($left externref) ($right externref))
    (i64 i32 externref)
  (:export-raw "native-float-pair")
  #@(return_call $native-float-transport (local.get $op) (local.get $kind) (local.get $mode)
    (local.get $bits) (local.get $left) (local.get $right) (i64.const 0) (i32.const 0)))

(define-runtime-function $native-float-raw
    (($kind i32) ($tag i32) ($bits i64)) (f64)
  #@(if (i32.eq (local.get $tag) (i32.const 1))
    (then
      (return (if (result f64) (i32.eq (local.get $kind) (i32.const 3))
        (then (f64.promote_f32 (f32.convert_i64_s (local.get $bits))))
        (else (f64.convert_i64_s (local.get $bits)))))))
  #@(call $checked-float (local.get $tag) (f64.reinterpret_i64 (local.get $bits))))

(define-runtime-function $native-float-transport
    (($op i32) ($kind i32) ($mode i32) ($bits i64) ($left externref) ($right externref)
     ($right-bits i64) ($input-mask i32))
    (i64 i32 externref)
  (:export-raw "native-float-transport")
  (:locals ($id i32) ($function :nullable-object) ($a :nullable-object) ($b :nullable-object)
           ($raw-a i32) ($raw-b i32) ($kind-a i32) ($kind-b i32)
           ($x f64) ($y f64) ($result f64) ($tag-a i32) ($tag-b i32))
  ;; Mode 0 produces a snapshot, mode 1 consumes its raw left operand, and
  ;; mode 2 consumes its boxed miss. Even a native caller supplies untrusted
  ;; ingress. Validate the mode and precision independently of the compiler.
  (wat-fail-when #@(i32.or (i32.gt_u (local.get $op) (i32.const 2))
    (i32.or (i32.gt_u (local.get $mode) (i32.const 2))
      (i32.or (i32.gt_u (local.get $input-mask) (i32.const 255))
        (i32.gt_u (i32.sub (local.get $kind) (i32.const 3)) (i32.const 1))))) :malformed-form)
  ;; The low two bits report valid input snapshots. Optional three-bit
  ;; kinds describe raw operands with no boxed object. A zero kind uses the
  ;; result precision, as in the ordinary producer/consumer convention.
  #@(local.set $kind-a (i32.and (i32.shr_u (local.get $input-mask) (i32.const 2)) (i32.const 7)))
  #@(local.set $kind-b (i32.shr_u (local.get $input-mask) (i32.const 5)))
  #@(local.set $input-mask (i32.and (local.get $input-mask) (i32.const 3)))
  #@(local.set $id (i32.add (local.get $op) (i32.const ,(runtime-symbol-id '+))))
  #@(if (i32.eq (local.get $mode) (i32.const 2))
    (then (return (i64.const 0) (i32.const 0)
      (call $native-standard-call2 (i32.add (local.get $id) (i32.const 1))
        (local.get $left) (local.get $right)))))
  #@(local.set $a (call $native-checked-value (local.get $left)))
  #@(local.set $b (call $native-checked-value (local.get $right)))
  ;; Validate omitted payloads before polling or calling Lisp. Keep their
  ;; input kind for reconstruction if the fast arithmetic path misses.
  #@(if (i32.eqz (local.get $mode))
    (then
      (local.set $raw-a (i32.and (ref.is_null (local.get $a)) (local.get $input-mask)))
      (local.set $raw-b (i32.and (ref.is_null (local.get $b))
        (i32.shr_u (local.get $input-mask) (i32.const 1))))
      ,(apply #'runtime-wat-sequence
        (loop for raw in '($raw-a $raw-b) for kind in '($kind-a $kind-b)
              for bits in '($bits $right-bits) collect
          #@(if (local.get ,raw)
              (then
                (local.set ,kind (select (local.get ,kind) (local.get $kind) (local.get ,kind)))
                (local.set ,bits (call $native-scalar-validate-operand (local.get ,kind) (local.get ,bits)))))))))
  #@(local.set $function (call $native-checked-value
    (extern.convert_any (call $standard-function-code (i32.add (local.get $id) (i32.const 1))))))
  #@(if (local.get $mode)
    (then (local.set $x (call $checked-float (local.get $kind) (f64.reinterpret_i64 (local.get $bits))))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $id (call $native-builtin-id (local.get $function)))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    ;; APPLY1 checks the native subtype first and dispatches other function
    ;; objects by code. Restored builtin objects have that same executable
    ;; identity even when their address differs from the bootstrap cache.
    (br_if $miss (ref.test (ref $native-function) (local.get $function)))
    (br_if $miss (i32.ne (local.get $id)
      (i32.add (local.get $op) (i32.const ,(runtime-symbol-id '+)))))
    (local.set $tag-b (select (select (i32.const 1) (local.get $kind-b) (i32.ge_u (local.get $kind-b) (i32.const 5)))
      (call $tag (local.get $b)) (local.get $raw-b)))
    (local.set $tag-a (select (select (i32.const 1) (local.get $kind-a) (i32.ge_u (local.get $kind-a) (i32.const 5)))
      (call $tag (local.get $a)) (local.get $raw-a)))
    (if (local.get $input-mask)
      (then
        (br_if $miss (i32.or (local.get $mode)
          (i32.ne (select (local.get $tag-a) (local.get $tag-b)
                          (i32.gt_u (local.get $tag-a) (local.get $tag-b))) (local.get $kind))))
        (br_if $miss (i32.and (i32.ne (local.get $tag-a) (i32.const 1))
          (i32.gt_u (i32.sub (local.get $tag-a) (i32.const 3)) (i32.const 1))))
        (br_if $miss (i32.and (i32.ne (local.get $tag-b) (i32.const 1))
          (i32.gt_u (i32.sub (local.get $tag-b) (i32.const 3)) (i32.const 1))))))
    (if (i32.and (local.get $input-mask) (i32.const 2))
      (then (local.set $y (call $native-float-raw (local.get $kind) (local.get $tag-b)
        (local.get $right-bits))))
      (else
        (br_if $miss (i32.ne (local.get $tag-b) (local.get $kind)))
        (local.set $y (call $coerce-float (ref.as_non_null (local.get $b)) (local.get $kind)))))
    (if (i32.eqz (local.get $mode))
      (then
        (if (i32.and (local.get $input-mask) (i32.const 1))
          (then (local.set $x (call $native-float-raw (local.get $kind) (local.get $tag-a)
            (local.get $bits))))
          (else
            (br_if $miss (i32.ne (local.get $tag-a) (local.get $kind)))
            (local.set $x (call $coerce-float (ref.as_non_null (local.get $a)) (local.get $kind)))))))
    (local.set $result (if (result f64) (i32.eqz (local.get $op))
      (then (f64.add (local.get $x) (local.get $y)))
      (else (if (result f64) (i32.eq (local.get $op) (i32.const 1))
        (then (f64.sub (local.get $x) (local.get $y)))
        (else (f64.mul (local.get $x) (local.get $y)))))))
    (if (local.get $mode)
      (then (return (i64.const 0) (i32.const 0)
        (extern.convert_any (call $float (local.get $kind) (local.get $result))))))
    ;; The producer rounds and signals before the consumer's operands run.
    (return (i64.reinterpret_f64 (call $checked-float (local.get $kind) (local.get $result)))
      (i32.const 1) (ref.null extern)))
  #@(if (local.get $mode)
    (then (local.set $a (call $float (local.get $kind) (local.get $x)))))
  #@(if (local.get $raw-a)
    (then (local.set $a (call $native-scalar-materialize-operand (local.get $kind-a) (local.get $bits)))))
  #@(if (local.get $raw-b)
    (then (local.set $b (call $native-scalar-materialize-operand (local.get $kind-b) (local.get $right-bits)))))
  #@(i64.const 0)
  #@(i32.const 0)
  #@(call $native-call2-ready (local.get $function) (local.get $a) (local.get $b) (local.get $id) (i32.const 0)))

(defun native-low-field-binding-wat (name)
  (let ((id (runtime-symbol-id name)))
    (runtime-wat-sequence
      #@(local.set $binding (array.get $objects (local.get $bindings) (i32.const ,id)))
      #@(br_if $miss (i32.eqz (ref.test (ref $obj) (local.get $binding))))
      #@(br_if $miss (i32.eqz (ref.eq (local.get $binding)
        (array.get $objects (global.get $canonical-builtin-functions) (i32.const ,(* 2 id))))))
      #@(br_if $miss (i32.ne (call $native-builtin-id (local.get $binding)) (i32.const ,id))))))

(defun native-low-field-operand-wat (raw result)
  (runtime-wat-sequence
    #@(local.set $operand (any.convert_extern (local.get ,raw)))
    #@(br_if $miss (i32.eqz (ref.test (ref null eq) (local.get $operand))))
    #@(local.set $object (ref.cast (ref null eq) (local.get $operand)))
    (wat-decode-scalar-integer '$object result '$miss)))

(defun native-low-field-guards-wat (&optional (check-width t))
  (runtime-wat-sequence
    (if check-width #@(br_if $miss (i32.gt_u (local.get $width) (i32.const 63))) "")
    #@(br_if $miss (global.get $debug-evaluating))
    #@(local.set $bindings (struct.get $symbol-index $dense (global.get $standard-function-bindings)))
    (native-low-field-binding-wat 'byte)
    (native-low-field-binding-wat 'ldb)))

(defun native-low-field-commit-wat (value)
  ;; BYTE and LDB each visit CALL2 and fixed APPLY. The admitted arithmetic
  ;; adds no checkpoint. Commit only if none of the four can service the queue.
  (runtime-wat-sequence
    (wat-fragment (miricl-cross::thread-work-gate-wat
      '(br_if $miss (i32.or (i32.le_s (global.get $thread-budget) (i32.const 4))
                           (i32.eqz (ref.is_null (global.get $thread-pending)))))
      '(global.set $thread-budget (i32.sub (global.get $thread-budget) (i32.const 4)))))
    #@(call $clear-values)
    #@(return (extern.convert_any (call $int64 (i64.and (local.get ,value)
      (i64.sub (i64.shl (i64.const 1) (i64.extend_i32_u (local.get $width))) (i64.const 1))))))))

(defun native-low-field-fallback-wat (arithmetic)
  ;; Resolve BYTE first, then LDB after the checked arithmetic. BYTE can
  ;; service pending work, so the second call must do its own live lookup.
  (runtime-wat-sequence
    #@(local.set $spec (call $native-standard-call2 (i32.const ,(1+ (runtime-symbol-id 'byte)))
      (extern.convert_any (call $int (local.get $width))) (extern.convert_any (ref.i31 (i32.const 0)))))
    arithmetic
    #@(return_call $native-standard-call2
      (i32.const ,(1+ (runtime-symbol-id 'ldb))) (local.get $spec) (local.get $result))))

(define-runtime-function $validate-low-field-box (($operand (ref null any))) ()
  #@(block $other (result (ref null any))
    ,(wat-fail-when
      #@(i32.ne (i32.and (struct.get $obj $tag
        (br_on_cast_fail $other (ref null any) (ref $fixnum) (local.get $operand)))
        (i32.const 63)) (i32.const 1)) :malformed-form)
    (return))
  #@(drop))

(define-runtime-function $native-low-field-add
    (($width i32) ($raw-left externref) ($raw-right externref)) (externref)
  (:export-raw "native-low-field-add")
  (:locals ($bindings (ref $objects)) ($binding :nullable-object)
           ($operand (ref null any)) ($object :nullable-object)
           ($left i64) ($right i64) ($computed i64)
           ($spec externref) ($result externref))
  #@(block $miss
    ,(native-low-field-guards-wat)
    ,(native-low-field-operand-wat '$raw-left '$left)
    ,(native-low-field-operand-wat '$raw-right '$right)
    (local.set $computed (i64.add (local.get $left) (local.get $right)))
    ;; The ordinary signed overflow path promotes and uses framed LDB.
    (br_if $miss (i32.eqz ,(native-integer-fit-wat '+)))
    ,(native-low-field-commit-wat '$computed))
  ;; The raw ABI also admits foreign references. Check boxed tags after
  ;; BYTE and leave left-before-right argument errors to the ordinary ADD.
  (native-low-field-fallback-wat
    (runtime-wat-sequence
      #@(local.set $operand (any.convert_extern (local.get $raw-left)))
      #@(call $validate-low-field-box (local.get $operand))
      #@(if (i32.or (ref.test (ref i31) (local.get $operand))
                    (ref.test (ref $fixnum) (local.get $operand)))
        (then (call $validate-low-field-box (any.convert_extern (local.get $raw-right)))))
      #@(local.set $result (call $native-add-integer (local.get $raw-left) (local.get $raw-right))))))

(define-runtime-function $native-low-field-ash
    (($width i32) ($raw-value externref) ($raw-count externref)) (externref)
  (:export-raw "native-low-field-ash")
  (:locals ($bindings (ref $objects)) ($binding :nullable-object)
           ($operand (ref null any)) ($object :nullable-object)
           ($value i64) ($shift i64) ($shifted i64)
           ($spec externref) ($result externref))
  #@(block $miss
    ,(native-low-field-guards-wat)
    ,(native-low-field-operand-wat '$raw-value '$value)
    ,(native-low-field-operand-wat '$raw-count '$shift)
    ;; Even width zero checks both operands and requires the exact signed
    ;; intermediate. Overflow and wide operands retain general ASH behavior.
    (local.set $shifted (local.get $value))
    (if (i32.and (i64.ne (local.get $value) (i64.const 0))
                 (i64.ne (local.get $shift) (i64.const 0)))
      (then ,(wat-checked-scalar-shift '$value '$shift '$shifted '$miss)))
    ,(native-low-field-commit-wat '$shifted))
  (native-low-field-fallback-wat
    #@(local.set $result (call $native-ash-general-inline (local.get $raw-value) (local.get $raw-count)))))

(define-runtime-function $native-literal-byte-extract
    (($width i32) ($position i32) ($raw-value externref)) (externref)
  (:export-raw "native-literal-byte-extract")
  (:locals ($bindings (ref $objects)) ($binding :nullable-object)
           ($operand (ref null any)) ($object :nullable-object)
           ($value i64) ($shifted i64) ($spec externref))
  ;; The unsigned bounds also reject negative raw metadata before narrowing.
  (wat-fail-when #@(i32.or
    (i32.gt_u (local.get $width) (i32.const 63))
    (i32.gt_u (local.get $position) (i32.const 1073741823))) :malformed-form)
  #@(block $miss
    ,(native-low-field-guards-wat nil)
    ,(native-low-field-operand-wat '$raw-value '$value)
    ;; Shifting by 63 produces the sign bits for every higher position.
    (local.set $shifted
      (i64.shr_s (local.get $value) (i64.extend_i32_u
        (select (local.get $position) (i32.const 63)
          (i32.lt_u (local.get $position) (i32.const 63))))))
    ,(native-low-field-commit-wat '$shifted))
  ;; The probe has left the checkpoint budget and Lisp values alone.
  ;; Resolve LDB after BYTE returns, with LDB's ordinary checked ingress.
  #@(local.set $spec (call $native-standard-call2
    (i32.const ,(1+ (runtime-symbol-id 'byte)))
    (extern.convert_any (ref.i31 (local.get $width)))
    (extern.convert_any (ref.i31 (local.get $position)))))
  #@(return_call $native-standard-call2 (i32.const ,(1+ (runtime-symbol-id 'ldb)))
    (local.get $spec) (local.get $raw-value)))

(define-runtime-function $native-standard-call3
    (($code i32) ($a externref) ($b externref) ($c externref)) (externref)
  (:export-raw "native-standard-call3")
  #@(drop (call $native-checked-value (local.get $a)))
  #@(drop (call $native-checked-value (local.get $b)))
  #@(drop (call $native-checked-value (local.get $c)))
  #@(return_call $native-call3
    (extern.convert_any (call $standard-function-code (local.get $code)))
    (local.get $a) (local.get $b) (local.get $c)))

;; Check the selected callable after polling before relying on its builtin body.
(define-runtime-function $native-canonical-aref-p (($function :nullable-object) ($write i32)) (i32)
  (:locals ($id i32))
  #@(local.set $id (i32.const ,(runtime-symbol-id 'aref)))
  #@(i32.and
    (i32.eq (call $native-builtin-id (local.get $function))
      (select (i32.sub (i32.const -2) (local.get $id)) (local.get $id) (local.get $write)))
    (ref.eq (local.get $function) (array.get $objects (global.get $canonical-builtin-functions)
      (i32.add (i32.shl (local.get $id) (i32.const 1)) (local.get $write))))))

(define-runtime-function $native-check-aref-miss
    (($array :nullable-object) ($subscript :nullable-object)) ()
  #@(call $array-check-raw-read-miss (local.get $array))
  #@(call $validate-bit-integer-layout (local.get $subscript)))

(define-runtime-function $native-primary-apply-ready
    (($function :nullable-object) ($args :nullable-object)) (externref)
  (:locals ($result :nullable-object))
  #@(local.set $result (call $apply-ready (local.get $function) (local.get $args)))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-aref-snapshot
    (($array externref) ($subscript externref)) (i64 i32 externref)
  (:export-raw "native-aref-snapshot")
  #@(return_call $native-aref-snapshot-worker
    (local.get $array) (local.get $subscript) (i64.const 0) (i32.const 0)))

(define-runtime-function $native-aref-snapshot64
    (($array externref) ($subscript i64)) (i64 i32 externref)
  (:export-raw "native-aref-snapshot64")
  #@(return_call $native-aref-snapshot-worker
    (local.get $array) (ref.null extern) (local.get $subscript) (i32.const 1)))

(define-runtime-function $native-aref-snapshot-transport
    (($array externref) ($subscript externref) ($wide i64) ($valid i32)) (i64 i32 externref)
  (:export-raw "native-aref-snapshot-transport")
  #@(return_call $native-aref-snapshot-worker
    (local.get $array) (local.get $subscript) (local.get $wide)
    (select (i32.const 2) (i32.const 0) (local.get $valid))))

(define-runtime-function $native-flat-bit-backing
    (($array :nullable-object) ($index i64)) ((ref null $digits))
  (:locals ($data (ref null $digits)) ($length i32))
  #@(if (i32.eqz (ref.test (ref $obj) (local.get $array))) (then (return (ref.null $digits))))
  #@(if (i32.ne (call $tag (local.get $array)) (i32.const 36)) (then (return (ref.null $digits))))
  #@(local.set $length (call $value-n (local.get $array)))
  #@(if (i64.ge_u (local.get $index) (i64.extend_i32_u (local.get $length)))
    (then (return (ref.null $digits))))
  #@(local.set $data (struct.get $obj $digits (ref.cast (ref $obj) (local.get $array))))
  #@(if (i32.eqz (ref.is_null (local.get $data)))
    (then (call $array-check-bit-capacity (local.get $length) (array.len (local.get $data)))))
  #@(local.get $data))

(define-runtime-function $native-aref-snapshot-worker
    (($array externref) ($subscript externref) ($wide i64) ($raw-index i32)) (i64 i32 externref)
  (:locals ($a :nullable-object) ($b :nullable-object) ($function :nullable-object)
           ($id i32) ($canonical i32) ($data (ref null $array-i64-data)) ($boxed externref)
           ($bits (ref $digits)) ($index i32))
  #@(local.set $a (call $native-checked-value (local.get $array)))
  #@(if (i32.ne (local.get $raw-index) (i32.const 1))
    (then (local.set $b (call $native-checked-value (local.get $subscript)))))
  #@(local.set $function (call $native-checked-value
    (extern.convert_any (call $standard-function-code (i32.const ,(1+ (runtime-symbol-id 'aref)))))))
  #@(local.set $id (call $native-builtin-id (local.get $function)))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'aref)))
    (then
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (local.set $canonical (i32.and (i32.eqz (global.get $debug-evaluating))
        (call $native-canonical-aref-p (local.get $function) (i32.const 0))))
      (block $miss
        (br_if $miss (i32.eqz (local.get $canonical)))
        (if (i32.eqz (local.get $raw-index))
          (then ,(wat-decode-scalar-integer '$b '$wide '$miss)))
        (local.set $data (call $array-owned-rank1-fixnum-backing (local.get $a) (local.get $wide)))
        (if (i32.eqz (ref.is_null (local.get $data)))
          (then
            (call $clear-values)
            (return (array.get $array-i64-data (local.get $data) (i32.wrap_i64 (local.get $wide)))
              (i32.const 1) (ref.null extern))))
        (block $not-bit
          (local.set $bits (br_on_null $not-bit
            (call $native-flat-bit-backing (local.get $a) (local.get $wide))))
          (if (i32.and (i32.ne (local.get $raw-index) (i32.const 1))
                       (ref.test (ref $fixnum) (local.get $b)))
            (then (call $validate-bit-integer-layout (local.get $b))))
          (local.set $index (i32.wrap_i64 (local.get $wide)))
          (call $clear-values)
          (return
            (i64.extend_i32_u (i32.and (i32.const 1)
              (i32.shr_u (array.get $digits (local.get $bits) (i32.shr_u (local.get $index) (i32.const 5)))
                (local.get $index))))
            (i32.const 1) (ref.null extern)))
        (if (i32.eq (local.get $raw-index) (i32.const 1))
          (then (return (i64.const 0) (i32.const 0) (call $native-aref-scalar-miss
            (local.get $a) (local.get $function) (local.get $wide) (i32.const 0))))))
      (if (i32.eq (local.get $raw-index) (i32.const 1))
        (then (local.set $b (call $box-int64 (local.get $wide)))))
      (call $native-check-aref-miss (local.get $a) (local.get $b))
      (local.set $boxed (if (result externref) (local.get $canonical)
        (then (call $native-call2-ready
          (local.get $function) (local.get $a) (local.get $b) (local.get $id) (i32.const 1)))
        (else (call $native-primary-apply-ready (local.get $function)
          (call $cons2 (local.get $a) (local.get $b) (ref.null eq)))))))
    (else
      (if (i32.eq (local.get $raw-index) (i32.const 1))
        (then (local.set $b (call $box-int64 (local.get $wide)))))
      (local.set $boxed (call $native-call2-checked (local.get $function) (local.get $a) (local.get $b)))))
  ;; A boxed NIL is a valid observation, distinguished by the separate status.
  #@(drop (call $native-checked-value (local.get $boxed)))
  #@(i64.const 0)
  #@(i32.const 0)
  #@(local.get $boxed))

(define-runtime-function $native-set-aref-int64
    (($function externref) ($value i64) ($array externref) ($subscript externref)) ()
  (:export-raw "native-set-aref-int64")
  (:locals ($function-object :nullable-object) ($a :nullable-object) ($b :nullable-object)
           ($data (ref null $array-i64-data)) ($wide i64) ($boxed :nullable-object) ($checked i32)
           ($bits (ref $digits)) ($index i32) ($word i32) ($mask i32))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(local.set $function-object (call $native-checked-value (local.get $function)))
  #@(local.set $a (call $native-checked-value (local.get $array)))
  #@(local.set $b (call $native-checked-value (local.get $subscript)))
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (br_if $miss (i32.eqz (call $native-canonical-aref-p (local.get $function-object) (i32.const 1))))
    ,(wat-decode-scalar-integer '$b '$wide '$miss)
    (local.set $data (call $array-owned-rank1-fixnum-backing (local.get $a) (local.get $wide)))
    (if (ref.is_null (local.get $data))
      (then
        (br_if $miss (i64.gt_u (local.get $value) (i64.const 1)))
        (local.set $bits (br_on_null $miss
          (call $native-flat-bit-backing (local.get $a) (local.get $wide))))
        (if (ref.test (ref $fixnum) (local.get $b))
          (then (call $validate-bit-integer-layout (local.get $b))))
        (local.set $index (i32.wrap_i64 (local.get $wide)))
        (local.set $word (i32.shr_u (local.get $index) (i32.const 5)))
        (local.set $mask (i32.shl (i32.const 1) (local.get $index)))
        ;; Check and update the live word after the operation's checkpoint.
        (array.set $digits (local.get $bits) (local.get $word)
          (i32.or (i32.and (array.get $digits (local.get $bits) (local.get $word))
                           (i32.xor (local.get $mask) (i32.const -1)))
                  (i32.mul (local.get $mask) (i32.wrap_i64 (local.get $value))))))
      (else
        (array.set $array-i64-data (local.get $data) (i32.wrap_i64 (local.get $wide)) (local.get $value))))
    (call $clear-values)
    (return))
  ;; The ordinary continuation invokes the already captured setter. Only the
  ;; miss needs a Lisp box because this entry consumes no result values.
  #@(local.set $boxed (call $int64 (local.get $value)))
  #@(if (i32.eq (call $native-builtin-id (local.get $function-object))
        (i32.const ,(- -2 (runtime-symbol-id 'aref))))
    (then
      (call $native-check-aref-miss (local.get $a) (local.get $b))
      (if (i32.or (global.get $debug-evaluating)
          (i32.eqz (call $native-canonical-aref-p (local.get $function-object) (i32.const 1))))
        (then
          (drop (call $native-primary-apply-ready (local.get $function-object)
            (call $cons (local.get $boxed) (call $cons2 (local.get $a) (local.get $b) (ref.null eq)))))
          (return)))
      (local.set $checked (i32.const 1))))
  #@(drop (call $native-call3-ready (local.get $function-object) (local.get $function)
    (extern.convert_any (local.get $boxed)) (local.get $array) (local.get $subscript) (local.get $checked))))

(define-runtime-function $native-aref-scalar-index (($subscript i64) ($normalize i32)) (:object)
  ;; The raw i64 result tier keeps its box on a miss. The separate i32
  ;; entry preserves the immediate tier whenever the full value fits i31.
  #@(if (local.get $normalize)
    (then (return_call $int64 (local.get $subscript))))
  #@(return_call $box-int64 (local.get $subscript)))

(define-runtime-function $native-aref-int64 (($array externref) ($subscript i64)) (externref)
  (:export-raw "native-aref-int64")
  #@(return_call $native-aref-scalar (local.get $array) (local.get $subscript) (i32.const 0)))

(define-runtime-function $native-aref-i31-index (($array externref) ($subscript i32)) (externref)
  (:export-raw "native-aref-i31-index")
  ;; Treat every incoming signed i32 as its full integer, including values
  ;; outside the intended i31 range. The shared worker owns checked ingress.
  #@(return_call $native-aref-scalar (local.get $array)
    (i64.extend_i32_s (local.get $subscript)) (i32.const 1)))

(define-runtime-function $native-aref-scalar
    (($array externref) ($subscript i64) ($normalize i32)) (externref)
  (:locals ($a :nullable-object) ($function :nullable-object) ($result :nullable-object))
  #@(local.set $a (call $native-checked-value (local.get $array)))
  #@(local.set $function (call $standard-function-code (i32.const ,(1+ (runtime-symbol-id 'aref)))))
  #@(if (i32.ne (call $native-builtin-id (local.get $function)) (i32.const ,(runtime-symbol-id 'aref)))
    (then
      (call $array-check-raw-read-miss (local.get $a))
      (return_call $native-call2 (extern.convert_any (local.get $function))
        (local.get $array) (extern.convert_any (call $native-aref-scalar-index (local.get $subscript) (local.get $normalize))))))
  (miricl-cross::thread-checkpoint-wat)
  #@(if (if (result i32) (local.get $normalize)
          (then (i32.or (global.get $debug-evaluating)
            (i32.eqz (call $native-canonical-aref-p (local.get $function) (i32.const 0)))))
          (else (i32.const 0)))
    (then
      (local.set $result (call $int64 (local.get $subscript)))
      (call $native-check-aref-miss (local.get $a) (local.get $result))
      (return_call $native-primary-apply-ready (local.get $function)
        (call $cons2 (local.get $a) (local.get $result) (ref.null eq)))))
  ;; The checkpoint can change array shape or backing. Read both live.
  #@(local.set $result (call $array-owned-rank1-fixnum-element64
    (local.get $a) (local.get $subscript) (ref.null eq) (i32.const 0)))
  #@(if (ref.is_null (local.get $result))
    (then (return_call $native-aref-scalar-miss
      (local.get $a) (local.get $function) (local.get $subscript) (local.get $normalize))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-aref-scalar-miss
    (($a :nullable-object) ($function :nullable-object) ($subscript i64) ($normalize i32)) (externref)
  (:locals ($result :nullable-object) ($index i32))
  #@(local.set $index (call $array-rank1-index64
    (call $array-rank1-dimension (local.get $a)) (local.get $subscript)))
  #@(if (i32.ge_s (local.get $index) (i32.const 0))
    (then
      (local.set $result (call $array-resolve-element (local.get $a) (local.get $index)
        (ref.null eq) (i32.const 0) (i32.const 1))))
    (else
      ;; Ordinary application owns the rank/index errors and the frame.
      ;; Only here does the original scalar snapshot need its Lisp box.
      (call $array-check-raw-read-miss (local.get $a))
      (local.set $result (call $apply-ready (local.get $function)
        (call $cons2 (local.get $a) (call $native-aref-scalar-index (local.get $subscript) (local.get $normalize))
          (ref.null eq))))))
  #@(drop (call $native-checked-value (extern.convert_any (local.get $result))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-standard-call4
    (($code i32) ($a externref) ($b externref) ($c externref) ($d externref)) (externref)
  (:export-raw "native-standard-call4")
  #@(drop (call $native-checked-value (local.get $a)))
  #@(drop (call $native-checked-value (local.get $b)))
  #@(drop (call $native-checked-value (local.get $c)))
  #@(drop (call $native-checked-value (local.get $d)))
  #@(return_call $native-call4
    (extern.convert_any (call $standard-function-code (local.get $code)))
    (local.get $a) (local.get $b) (local.get $c) (local.get $d)))

(defun $native-value-arguments-cl ()
  (runtime-function-definition '$native-value-arguments-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$native-value-arguments-cl 'miricl-runtime:flatten-value-groups))))

(define-runtime-function $native-value-arguments (($groups externref)) (:nullable-object)
  #@(call $check-apply-list ,(native-object '(local.get $groups)))
  #@(call $native-value-arguments-cl ,(native-object '(local.get $groups))))

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
  #@(type $native-scalar-entry (func (param externref externref externref externref externref i64 i64 i64 i64 i32)
    (result i64 i32 externref)))
  #@(type $native-scalar-worker (struct
    (field $owner (ref eq)) (field $direct (ref eq)) (field $literals (ref null eq))
    (field $captures (ref null eq)) (field $entry (ref $native-scalar-entry)) (field $arguments i32)
    (field $result-kind i32)))
  #@(type $native-scalar-registry (array (mut (ref null $native-scalar-worker))))
  #@(global $native-scalar-workers (ref $native-scalar-registry)
    (array.new_default $native-scalar-registry (i32.const 64)))
  #@(type $native-direct-code (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64) (field $digits (ref null $digits))
    (field $a (mut (ref null eq))) (field $b (mut (ref null eq))) (field $c (mut (ref null eq)))
    (field $entry (ref $native-direct-entry)) (field $call-proof i32))))
  ;; A bounded, process-local identity authority. Eviction only loses a proof;
  ;; neither this registry nor its hints survive image serialization.
  #@(global $native-call-proofs (ref $objects) (array.new_default $objects (i32.const 16)))
  #@(global $native-call-proof-next (mut i32) (i32.const 0))
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
  ($native-attach-direct) ($native-attach-scalar) ($native-attach-direct-root) ($native-attach-certified-root) ($native-set-argument-usage) ($native-attach-direct-usage) ($native-direct-apply) ($native-direct-apply-ready) ($native-shared-entry)
  #@(global $native-argument-empty (mut (ref null eq)) (ref.null eq))
  ($native-checked-object) ($native-checked-value) ($native-arguments) ($native-argument-set) ($native-validate-packet) ($native-ingress-arguments) ($native-prepend-argument) ($native-packet-tail)
  ($native-packet-length-cl) ($native-packet-ref-cl) ($native-arguments-list)
  ($native-argument-present) ($native-keyword-find-cl) ($native-check-keywords-cl)
  ($native-keyword-find) ($native-keyword-present) ($native-keyword-argument) ($native-check-keywords)
  ($native-two-arguments) ($native-packed-argument) ($native-list-tail-cl) ($native-list-tail) ($native-private-spine-count-cl) ($native-check-private-spine) ($native-argument-cl) ($native-argument) ($native-rest-argument-cl) ($native-rest-argument) ($native-prepare-dynamic-rest) ($native-dynamic-rest-length) ($native-check-arguments-cl) ($native-check-arguments) ($native-check-apply-arguments) ($native-check-apply-list) ($native-environment)
  ($native-restore-environment) ($native-bind)
  ($native-block-enter) ($native-block-environment) ($native-block-return) ($native-block-match) ($native-block-values)
  ($native-control) ($native-restart-signal) ($native-throw)
  ($native-global-read) ($native-global-write) ($native-function-reference) ($native-standard-function-reference) ($native-literal-function) ($native-entry-valid) ($native-module-error)
  ($native-cons) ($native-cons2) ($native-list2) ($native-cell) ($native-primary) ($native-cell-write)
  ($native-call) ($native-primary-call) ($native-primary-call5-ready) ($native-primary-call5)
  ($native-value-arguments-cl) ($native-value-arguments) ($native-multiple-value-call) ($native-tail-multiple-value-call)
  ($native-return) ($native-return1) ($native-return2) ($native-return3)
  ($native-return-counted8) ($native-check-result-spine) ($native-captured-result-ref) ($native-captured-extra-list)
  ($native-int64) ($native-unbox-int64) ($native-check-int64) ($native-add-int64) ($native-sub-int64) ($native-mul-int64)
  ($native-add-integer) ($native-sub-integer) ($native-mul-integer)
  ($native-logand-int64) ($native-logior-int64) ($native-logxor-int64) ($native-ash-int64)
  ($native-add-int64-checked) ($native-sub-int64-checked) ($native-mul-int64-checked)
  ($native-lt-int64) ($native-gt-int64) ($native-le-int64) ($native-ge-int64) ($native-eq-int64) ($native-ne-int64)
  ($native-vector-ref) ($native-boolean)
  ($native-eq-inline) ($native-eq-test) ($native-eql-inline) ($native-eql-test) ($native-length-inline) ($native-list-length-inline) ($native-vector-length-inline)
  ($native-gethash-inline) ($checked-gethash-primary) ($native-gethash-primary-inline) ($native-puthash-inline)
  ($native-consp-inline) ($native-consp-test) ($native-simple-vector-p-inline) ($native-vector-p-inline)
  ($native-string-p-inline) ($native-integer-p-inline) ($native-character-p-inline)
  ($native-integer-length-inline) ($native-ash-general-inline) ($native-logand-general-inline)
  ($native-mod-general-inline) ($native-rounding-primary-inline) ($native-truncate-primary-inline)
  ($ordered-compare-native) ($native-ordered-compare-inline)
  ($native-zero-inline)
  ($native-vector-ref-trusted)
  ($native-cdr-inline-checked)
  ($native-car-list-inline) ($native-cdr-list-inline)
  ($native-vector-ref-inline)
  ($checked-sbit-ref) ($native-sbit-ref-inline) ($native-row-major-ref-inline)
  ($checked-row-major-set) ($native-row-major-set-inline)
  ($native-character-ref-inline) ($native-character-code-inline) ($native-character-set-inline)
  ($native-simple-character-set-inline)
  ($native-character-set-code-inline)
  ($native-code-character-inline)
  ($native-make-character-vector-inline)
  ($native-make-string-inline)
  ($native-make-vector-inline)
  ($native-character-code-at-inline)
  ($native-character-code-at64-inline)
  ($native-character-code-at-unboxed-inline)
  ($native-character-code-at64-unboxed-inline)
  ($native-vector-set-inline)
  ($native-primary-apply) ($native-builtin-id) ($native-poll) ($native-frame-enter) ($native-frame-leave)
  ($native-tail-enter) ($native-tail-direct) ($native-tail-call) ($native-generic-tail-call)
  ($native-symbol-fixed-function) ($native-fixed-apply-ready) ($native-fixed-call)
  ($native-call-values1-ready) ($native-capture-result) ($native-call-values1) ($native-call-captured1)
  ($native-call0) ($native-call1) ($native-call2) ($native-call2-checked) ($native-call2-ready)
  ($fixed-deposit-apply) ($native-call3) ($native-call3-ready) ($native-call4)
  ($native-standard-call0) ($native-standard-call1) ($native-standard-call2) ($native-standard-call3) ($native-standard-call4)
  ($native-float-pair) ($native-float-raw) ($native-float-transport)
  ($native-scalar-slot) ($native-register-scalar) ($native-scalar-decode) ($native-scalar-unpack)
  ($native-scalar-box) ($native-scalar-call) ($native-scalar-call2) ($native-scalar-validate-operand)
  ($native-scalar-materialize-operand) ($native-scalar-materialize-operands) ($native-scalar-call-with-operands)
  ($validate-low-field-box) ($native-low-field-add) ($native-low-field-ash) ($native-literal-byte-extract)
  ($native-canonical-aref-p) ($native-check-aref-miss) ($native-primary-apply-ready)
  ($native-aref-snapshot) ($native-aref-snapshot64) ($native-aref-snapshot-transport)
  ($native-aref-snapshot-worker) ($native-set-aref-int64)
  ($native-flat-bit-backing)
  ($native-aref-scalar-index) ($native-aref-scalar) ($native-aref-scalar-miss) ($native-aref-int64) ($native-aref-i31-index)
  ($native-tail-dispatch) ($native-tail-arguments) ($native-fixed-tail-call)
  ($native-generic-fixed-tail-call)
  ($native-tail-call0) ($native-tail-call1) ($native-tail-call2) ($native-tail-call3) ($native-tail-call4))
