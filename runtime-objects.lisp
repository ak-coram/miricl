;;;; Core WasmGC object layout and error handling.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $error-code
    ()
    (i32)
  (:export "error-code")
  #@(global.get $error))

(define-runtime-function $tag
    (($x :nullable-object))
    (i32)
  #@(if (result i32) (ref.is_null (local.get $x)) (then (i32.const 0))
      (else (i32.and (struct.get $obj $tag (local.get $x)) (i32.const 63)))))

(define-runtime-function $allocate-object-tag (($tag i32)) (i32)
  ;; Reuse the tag word's spare bits for a stable identity hash. Wrapping can
  ;; create collisions, which the table handles; it never changes an existing ID.
  #@(global.set $object-serial (i32.add (global.get $object-serial) (i32.const 64)))
  #@(i32.or (local.get $tag) (global.get $object-serial)))

(define-runtime-function $node
    (($tag i32) ($n i32) ($a :nullable-object) ($b :nullable-object) ($c :nullable-object))
    (:object)
  (:locals ($page (ref null $objects)) ($cached :nullable-object) ($character :object))
  ;; Character identities are canonical across readers, strings, CODE-CHAR and
  ;; compiled constants. Lazy pages avoid rooting an entire Unicode table.
  #@(if (i32.eq (local.get $tag) (i32.const 16))
    (then
      (local.set $page (array.get $object-pages (global.get $character-pages)
        (i32.shr_u (local.get $n) (i32.const 8))))
      (if (ref.is_null (local.get $page))
        (then
          (local.set $page (array.new_default $objects (i32.const 256)))
          (array.set $object-pages (global.get $character-pages)
            (i32.shr_u (local.get $n) (i32.const 8)) (local.get $page))))
      (local.set $cached (array.get $objects (ref.as_non_null (local.get $page))
        (i32.and (local.get $n) (i32.const 255))))
      (if (i32.eqz (ref.is_null (local.get $cached)))
        (then (return (ref.as_non_null (local.get $cached)))))
      (local.set $character ,(wat-new-object :character :n #@(local.get $n)))
      (array.set $objects (ref.as_non_null (local.get $page))
        (i32.and (local.get $n) (i32.const 255)) (local.get $character))
      (return (local.get $character))))
  (wat-new-object #@(local.get $tag)
                  :n #@(local.get $n)
                  :a #@(local.get $a)
                  :b #@(local.get $b)
                  :c #@(local.get $c)))

(define-runtime-function $id
    (($x :nullable-object))
    (i32)
  #@(if (ref.is_null (local.get $x)) (then (return (i32.const 0))))
  #@(if (i32.ne (call $tag (local.get $x)) (i32.const 6)) (then (return (i32.const -1))))
  #@(struct.get $obj $n (local.get $x)))

(define-runtime-function $symbol
    (($id i32))
    (:object)
  #@(if (i32.lt_u (local.get $id) (i32.const ,(length *runtime-symbols*)))
    (then (return (ref.as_non_null (array.get $objects (global.get $symbols) (local.get $id))))))
  #@(ref.as_non_null (array.get $objects
    (ref.as_non_null (array.get $object-pages (global.get $symbol-pages) (i32.shr_u (local.get $id) (i32.const 8))))
    (i32.and (local.get $id) (i32.const 255)))))

(define-runtime-function $bool
    (($x i32))
    (:nullable-object)
  #@(if (result (ref null $obj)) (local.get $x)
      (then (call $symbol (i32.const 1))) (else (ref.null $obj))))

;; Bootstrap identities are dense and hot. Later symbol IDs may be separated
;; by millions of ephemeral gensyms, so their bindings use lazy 256-cell pages.
(define-runtime-function $symbol-index-get (($index (ref $symbol-index)) ($id i32)) (:nullable-object)
  (:locals ($pages (ref $object-pages)) ($page (ref null $objects)))
  #@(if (i32.lt_u (local.get $id) (i32.const ,(length *runtime-symbols*)))
    (then (return (array.get $objects (struct.get $symbol-index $dense (local.get $index)) (local.get $id)))))
  #@(local.set $pages (struct.get $symbol-index $pages (local.get $index)))
  #@(if (i32.ge_u (i32.shr_u (local.get $id) (i32.const 8)) (array.len (local.get $pages)))
    (then (return (ref.null $obj))))
  #@(local.set $page (array.get $object-pages (local.get $pages) (i32.shr_u (local.get $id) (i32.const 8))))
  #@(if (ref.is_null (local.get $page)) (then (return (ref.null $obj))))
  #@(array.get $objects (local.get $page) (i32.and (local.get $id) (i32.const 255))))

(define-runtime-function $symbol-index-set
    (($index (ref $symbol-index)) ($id i32) ($value :nullable-object)) ()
  (:locals ($pages (ref $object-pages)) ($new (ref $object-pages)) ($page (ref null $objects)) ($slot i32))
  #@(if (i32.lt_u (local.get $id) (i32.const ,(length *runtime-symbols*)))
    (then (array.set $objects (struct.get $symbol-index $dense (local.get $index)) (local.get $id) (local.get $value))
      (return)))
  #@(local.set $pages (struct.get $symbol-index $pages (local.get $index)))
  #@(local.set $slot (i32.shr_u (local.get $id) (i32.const 8)))
  #@(if (i32.ge_u (local.get $slot) (array.len (local.get $pages)))
    (then
      (if (ref.is_null (local.get $value)) (then (return)))
      (local.set $new (array.new_default $object-pages (i32.mul (i32.add (local.get $slot) (i32.const 1)) (i32.const 2))))
      (array.copy $object-pages $object-pages (local.get $new) (i32.const 0) (local.get $pages) (i32.const 0)
        (array.len (local.get $pages)))
      (local.set $pages (local.get $new))
      (struct.set $symbol-index $pages (local.get $index) (local.get $pages))))
  #@(local.set $page (array.get $object-pages (local.get $pages) (local.get $slot)))
  #@(if (ref.is_null (local.get $page))
    (then
      (if (ref.is_null (local.get $value)) (then (return)))
      (local.set $page (array.new_default $objects (i32.const 256)))
      (array.set $object-pages (local.get $pages) (local.get $slot) (local.get $page))))
  #@(array.set $objects (local.get $page) (i32.and (local.get $id) (i32.const 255)) (local.get $value)))

(defun empty-symbol-index-wat ()
  (wat-fragment #@(struct.new $symbol-index
    (array.new_default $objects (i32.const ,(length *runtime-symbols*)))
    (array.new_default $object-pages (i32.const 0)))))

(define-runtime-section object-runtime-wat (&key wasi)
  ;; Minimal recursive groups are part of the compiled-code ABI. A module
  ;; defining DIGITS and OBJ alone gets the same canonical WasmGC types; it
  ;; must not reproduce every unrelated runtime subtype to access a cons.
  #@(type $digits (array (mut i32)))
  #@(rec (type $obj (sub (struct
                  (field $tag i32) (field $n (mut i32)) (field $f f64)
                  (field $digits (ref null $digits))
                  (field $a (mut (ref null $obj)))
                  (field $b (mut (ref null $obj)))
                  (field $c (mut (ref null $obj)))))))
  #@(type $objects (array (mut (ref null $obj))))
  #@(type $source-entry (sub final $obj (struct
        (field $tag i32) (field $n (mut i32)) (field $f f64)
        (field $digits (ref null $digits))
        (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj)))
        (field $c (mut (ref null $obj))) (field $modified (mut f64)))))
  #@(type $object-pages (array (mut (ref null $objects))))
  #@(type $symbol-index (struct (field $dense (ref $objects)) (field $pages (mut (ref $object-pages)))))
      ;; Public tables retain constructor hints without changing the trie fields.
  #@(type $hash-table (sub final $obj (struct
        (field $tag i32) (field $n (mut i32)) (field $f f64)
        (field $digits (ref null $digits))
        (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj)))
        (field $c (mut (ref null $obj)))
        (field $size-hint (ref null $obj)) (field $rehash-size (ref null $obj))
        (field $rehash-threshold (ref null $obj)))))
      ;; Only vectors carry a reference array; ordinary objects keep their layout.
  #@(type $vector (sub final $obj (struct
        (field $tag i32) (field $n (mut i32)) (field $f f64)
        (field $digits (ref null $digits))
        (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj)))
        (field $c (mut (ref null $obj))) (field $data (ref $objects)))))
      ;; General arrays own mutable shape/view metadata, not extra fields in OBJ.
  #@(type $array (sub final $obj (struct
        (field $tag i32) (field $n (mut i32)) (field $f f64)
        (field $digits (ref null $digits))
        (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj)))
        (field $c (mut (ref null $obj)))
        (field $shape (mut (ref $digits))) (field $offset (mut i32)) (field $fill (mut i32)))))
  #@(type $text-stream (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64)
    (field $digits (ref null $digits))
    (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj))) (field $c (mut (ref null $obj)))
    (field $data (mut (ref $digits))) (field $length (mut i32)) (field $closed (mut i32)))))
  ;; Structure and metadata method wrappers retain their interpreted source
  ;; and environment, with direct call metadata confined to this subtype.
  #@(type $direct-function (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64)
    (field $digits (ref null $digits))
    (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj))) (field $c (mut (ref null $obj)))
    (field $owner (ref null $obj)) (field $slot (ref null $obj)) (field $mode i32))))
  #@(type $generic-discriminator (sub final $obj (struct
    (field $tag i32) (field $n (mut i32)) (field $f f64)
    (field $digits (ref null $digits))
    (field $a (mut (ref null $obj))) (field $b (mut (ref null $obj))) (field $c (mut (ref null $obj)))
    (field $owner (ref null $obj)))))
  #@(memory (export "memory") 2)
  #@(global $object-serial (mut i32) (i32.const 0))
  #@(global $character-pages (ref $object-pages)
    (array.new_default $object-pages (i32.const 4352)))
  #@(global $error (export "error") (mut i32) (i32.const 0))
  #@(tag $transfer (export "native-transfer"))
  #@(func $trap
      ,(if wasi '(throw $guest-error) '(unreachable)))
  ($error-code)
  ($tag)
  ($allocate-object-tag)
  ($node)
  ($id)
  ($symbol)
  ($symbol-index-get) ($symbol-index-set)
  ($bool))
