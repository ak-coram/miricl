;;;; Symbol identities and object equality.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $symbolp
    (($x :nullable-object))
    (i32)
  #@(i32.or (ref.is_null (local.get $x)) (i32.eq (call $tag (local.get $x)) (i32.const 6))))

(define-runtime-function $symbol-name
    (($x :nullable-object))
    (:nullable-object)
  #@(if (ref.is_null (local.get $x)) (then (return (global.get $nil-name))))
  #@(call $require-symbol (local.get $x))
  #@(struct.get $obj $a (ref.cast (ref $obj) (local.get $x))))

(define-runtime-function $symbol-home
    (($x :nullable-object))
    (:nullable-object)
  #@(if (ref.is_null (local.get $x)) (then (return (global.get $nil-package))))
  #@(call $require-symbol (local.get $x))
  #@(struct.get $obj $b (ref.cast (ref $obj) (local.get $x))))

(define-runtime-function $set-home
    (($x :nullable-object) ($package :nullable-object))
    ()
  #@(if (ref.is_null (local.get $x)) (then (global.set $nil-package (local.get $package)))
      (else (struct.set $obj $b (ref.cast (ref $obj) (local.get $x)) (local.get $package)))))

(define-runtime-function $keywordp
    (($x :nullable-object))
    (i32)
  #@(if (result i32) (call $symbolp (local.get $x))
      (then (ref.eq (call $symbol-home (local.get $x)) (global.get $keyword-package))) (else (i32.const 0))))

(define-runtime-function $symbol-object
    (($id i32))
    (:nullable-object)
  #@(if (i32.eqz (local.get $id)) (then (return (ref.null eq))))
  (wat-fail-when #@(i32.ge_u (local.get $id) (global.get $next-symbol)) :malformed-form)
  #@(call $symbol (local.get $id)))

(define-runtime-function $register-symbol
    (($symbol :object))
    ()
  (:locals ($new (ref $object-pages)) ($page (ref null $objects)) ($index i32) ($id i32))
  #@(local.set $id (call $value-n (local.get $symbol)))
  #@(if (i32.lt_u (local.get $id) (i32.const ,(length *runtime-symbols*)))
    (then (array.set $objects (global.get $symbols) (local.get $id) (local.get $symbol)) (return)))
  ;; Symbol IDs include transient gensyms. Only allocate pages containing an
  ;; identity that the host transport or unbound-variable protocol retains.
  #@(local.set $index (i32.shr_u (local.get $id) (i32.const 8)))
  #@(if (i32.ge_u (local.get $index) (array.len (global.get $symbol-pages)))
    (then (local.set $new (array.new_default $object-pages
      (i32.mul (i32.add (local.get $index) (i32.const 1)) (i32.const 2))))
      (array.copy $object-pages $object-pages (local.get $new) (i32.const 0)
        (global.get $symbol-pages) (i32.const 0) (array.len (global.get $symbol-pages)))
      (global.set $symbol-pages (local.get $new))))
  #@(local.set $page (array.get $object-pages (global.get $symbol-pages) (local.get $index)))
  #@(if (ref.is_null (local.get $page))
    (then (local.set $page (array.new_default $objects (i32.const 256)))
      (array.set $object-pages (global.get $symbol-pages) (local.get $index) (local.get $page))))
  #@(array.set $objects (local.get $page) (i32.and (local.get $id) (i32.const 255)) (local.get $symbol)))

(define-runtime-function $new-symbol
    (($name :nullable-object) ($package :nullable-object))
    (:object)
  (:locals ($symbol :object))
  #@(local.set $name (call $string-contents (local.get $name)))
  ;; Uninterned internal operators retain conservative EVAL scope boundaries.
  ;; Interned ordinary calls share APPLY's boundary instead.
  #@(local.set $symbol ,(wat-new-object :symbol :n #@(global.get $next-symbol)
    :a #@(local.get $name) :b #@(local.get $package)
    :f #@(f64.convert_i32_u (ref.is_null (local.get $package)))))
  #@(global.set $next-symbol (i32.add (global.get $next-symbol) (i32.const 1)))
  ;; Guest references own ordinary symbols. Register only bootstrap identities,
  ;; host-exported symbols, and names saved by the unbound-variable protocol;
  ;; retaining every internal gensym here pins entire macro expansions.
  #@(if (ref.eq (local.get $package) (global.get $keyword-package))
      (then (call $install-keyword (local.get $symbol))))
  #@(local.get $symbol))

(define-runtime-section symbol-runtime-wat ()
  #@(global $nil-package (mut (ref null eq)) (ref.null eq))
  #@(global $nil-name (mut (ref null eq)) (ref.null eq))
  #@(global $next-symbol (mut i32) (i32.const ,(length *runtime-symbols*)))
  #@(global $symbols (ref $objects) (array.new_default $objects (i32.const ,(length *runtime-symbols*))))
  #@(global $symbol-pages (mut (ref $object-pages)) (array.new_default $object-pages (i32.const 0)))
  ($symbolp)
  ($symbol-name)
  ($symbol-home)
  ($set-home)
  ($keywordp)
  ($symbol-object)
  ($register-symbol)
  ($new-symbol))

(define-runtime-function $eql
    (($a :nullable-object) ($b :nullable-object))
    (i32)
  #@(if (ref.eq (local.get $a) (local.get $b)) (then (return (i32.const 1))))
  #@(if (i32.ne (call $tag (local.get $a)) (call $tag (local.get $b))) (then (return (i32.const 0))))
  #@(if (i32.eq (call $tag (local.get $a)) (i32.const 1))
    (then (return (i32.and (i32.eq (call $value-n (local.get $a)) (call $value-n (local.get $b)))
      (i32.eqz (call $cmpabs (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))))))
  #@(if (i32.eq (call $tag (local.get $a)) (i32.const 2))
    (then (return (call $equal (local.get $a) (local.get $b)))))
  #@(if (i32.or (i32.eq (call $tag (local.get $a)) (i32.const 3)) (i32.eq (call $tag (local.get $a)) (i32.const 4)))
      (then (return (i64.eq (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $a))))
                           (i64.reinterpret_f64 (struct.get $obj $f (ref.cast (ref $obj) (local.get $b))))))))
  #@(if (i32.eq (call $tag (local.get $a)) (i32.const 5))
      (then (return (i32.and (call $eql (struct.get $obj $a (ref.cast (ref $obj) (local.get $a))) (struct.get $obj $a (ref.cast (ref $obj) (local.get $b))))
                             (call $eql (struct.get $obj $b (ref.cast (ref $obj) (local.get $a))) (struct.get $obj $b (ref.cast (ref $obj) (local.get $b))))))))
  #@(if (i32.eq (call $tag (local.get $a)) (i32.const 16))
      (then (return (i32.eq (call $value-n (local.get $a)) (call $value-n (local.get $b))))))
  #@(i32.const 0))

(define-runtime-function $object-equal
    (($a :nullable-object) ($b :nullable-object))
    (i32)
  #@(loop $tail
    (if (call $eql (local.get $a) (local.get $b)) (then (return (i32.const 1))))
    (if (i32.or (i32.le_u (i32.sub (call $tag (local.get $a)) (i32.const 35)) (i32.const 1))
                 (i32.le_u (i32.sub (call $tag (local.get $b)) (i32.const 35)) (i32.const 1)))
      (then (return (call $array-equal-native (local.get $a) (local.get $b) (i32.const 0)))))
    (if (i32.ne (call $tag (local.get $a)) (call $tag (local.get $b))) (then (return (i32.const 0))))
    (if (i32.eq (call $tag (local.get $a)) (i32.const 10)) (then (return (call $string-equal (local.get $a) (local.get $b)))))
    (if (i32.eq (call $tag (local.get $a)) (i32.const 28))
      (then (return (call $object-equal (call $pathname-parts (local.get $a)) (call $pathname-parts (local.get $b))))))
    (if (i32.eq (call $tag (local.get $a)) (i32.const 7))
      (then
        (if (i32.eqz (call $object-equal (call $car (local.get $a)) (call $car (local.get $b))))
          (then (return (i32.const 0))))
        (local.set $a (call $cdr (local.get $a)))
        (local.set $b (call $cdr (local.get $b)))
        (br $tail)))
    (return (i32.const 0)))
  #@(unreachable))

(define-runtime-function $hash-equalp
    (($a :nullable-object) ($b :nullable-object)) (i32)
  (:locals ($scan :nullable-object) ($entry :nullable-object) ($other :nullable-object))
  #@(if (i32.eqz (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (local.get $a))) (struct.get $obj $a (ref.cast (ref $obj) (local.get $b)))))
    (then (return (i32.const 0))))
  #@(if (i32.eqz (call $eql (call $hash-count (local.get $a)) (call $hash-count (local.get $b))))
    (then (return (i32.const 0))))
  #@(local.set $scan (call $hash-entries (local.get $a)))
  #@(block $done (loop $next (br_if $done (ref.is_null (local.get $scan)))
    (local.set $entry (call $car (local.get $scan)))
    (local.set $other (call $hash-cell-native (call $car (local.get $entry)) (local.get $b)))
    (if (ref.is_null (local.get $other)) (then (return (i32.const 0))))
    (if (i32.eqz (call $object-equalp (call $cdr (local.get $entry)) (call $cdr (local.get $other))))
      (then (return (i32.const 0))))
    (local.set $scan (call $cdr (local.get $scan))) (br $next)))
  #@(i32.const 1))

(define-runtime-function $object-equalp
    (($a :nullable-object) ($b :nullable-object)) (i32)
  (:locals ($tag-a i32) ($tag-b i32))
  #@(loop $tail
    (if (ref.eq (local.get $a) (local.get $b)) (then (return (i32.const 1))))
    (local.set $tag-a (call $tag (local.get $a)))
    (local.set $tag-b (call $tag (local.get $b)))
    (if (i32.or (i32.le_u (i32.sub (local.get $tag-a) (i32.const 35)) (i32.const 1))
                 (i32.le_u (i32.sub (local.get $tag-b) (i32.const 35)) (i32.const 1)))
      (then (return (call $array-equal-native (local.get $a) (local.get $b) (i32.const 1)))))
    (if (i32.and (i32.eq (local.get $tag-a) (i32.const 34))
                 (i32.or (i32.eq (local.get $tag-b) (i32.const 34)) (i32.eq (local.get $tag-b) (i32.const 10))))
      (then (return (call $vector-equalp (local.get $a) (local.get $b)))))
    (if (i32.and (i32.eq (local.get $tag-a) (i32.const 10)) (i32.eq (local.get $tag-b) (i32.const 34)))
      (then (return (call $vector-equalp (local.get $b) (local.get $a)))))
    (if (i32.and (i32.le_u (i32.sub (local.get $tag-a) (i32.const 1)) (i32.const 4))
                 (i32.le_u (i32.sub (local.get $tag-b) (i32.const 1)) (i32.const 4)))
      (then (return (call $equal (local.get $a) (local.get $b)))))
    (if (i32.ne (local.get $tag-a) (local.get $tag-b)) (then (return (i32.const 0))))
    (if (i32.eq (local.get $tag-a) (i32.const 16))
      (then (return (i32.eq
        (call $string-upcase-code (call $value-n (local.get $a)))
        (call $string-upcase-code (call $value-n (local.get $b)))))))
    (if (i32.eq (local.get $tag-a) (i32.const 10))
      (then (return (call $string-fold-equal (local.get $a) (local.get $b)))))
    (if (i32.eq (local.get $tag-a) (i32.const 7))
      (then
        (if (i32.eqz (call $object-equalp (call $car (local.get $a)) (call $car (local.get $b))))
          (then (return (i32.const 0))))
        (local.set $a (call $cdr (local.get $a)))
        (local.set $b (call $cdr (local.get $b))) (br $tail)))
    (if (i32.eq (local.get $tag-a) (i32.const 23))
      (then
        (if (i32.eqz (ref.eq (struct.get $obj $a (ref.cast (ref $obj) (local.get $a))) (struct.get $obj $a (ref.cast (ref $obj) (local.get $b)))))
          (then (return (i32.const 0))))
        ;; Standard instances share this tag, but only DEFSTRUCT classes
        ;; carry :STRUCT-INITARGS. Skip the private layout header.
        (if (ref.is_null (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (struct.get $obj $a (ref.cast (ref $obj) (local.get $a)))))
              ,(clos-library-reference :struct-initargs)))
          (then (return (i32.const 0))))
        (local.set $a (call $cdr (call $cdr (struct.get $obj $b (ref.cast (ref $obj) (local.get $a))))))
        (local.set $b (call $cdr (call $cdr (struct.get $obj $b (ref.cast (ref $obj) (local.get $b))))))
        (br $tail)))
    (if (i32.eq (local.get $tag-a) (i32.const 29))
      (then (return (call $hash-equalp (local.get $a) (local.get $b)))))
    (return (call $object-equal (local.get $a) (local.get $b))))
  #@(unreachable))

(define-runtime-section equality-runtime-wat ()
  ($eql)
  ($object-equal)
  ($hash-equalp)
  ($object-equalp))
