;;;; Bounded Lisp backtraces, independent of native Wasm debugging metadata.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

;; Only resolve names on error. Active slots may hold function objects, but
;; snapshots retain names alone, never a closure's arguments or environment.
(define-runtime-function $backtrace-name (($function :nullable-object)) (:nullable-object)
  (:locals ($id i32) ($body :nullable-object))
  #@(if (call $symbolp (local.get $function)) (then (return (local.get $function))))
  #@(if (i32.eq (call $tag (local.get $function)) (i32.const 8))
    (then
      (local.set $id (struct.get $obj $n (local.get $function)))
      (if (i32.lt_u (i32.sub (local.get $id) (i32.const 1)) (i32.const ,(length *runtime-symbols*)))
        (then (return (call $symbol (i32.sub (local.get $id) (i32.const 1))))))
      (if (i32.eq (local.get $id) (i32.const 2147483645))
        (then
          (local.set $body (call $generic-property (local.get $function) ,(clos-library-reference :name)))
          (if (call $symbolp (local.get $body)) (then (return (local.get $body))))))
      (if (i32.eqz (local.get $id))
        (then
          (local.set $body (struct.get $obj $b (local.get $function)))
          (block $done (loop $declarations
            (br_if $done (i32.ne (call $operator (call $car (local.get $body)))
              (i32.const ,(runtime-symbol-id 'declare))))
            (local.set $body (call $cdr (local.get $body))) (br $declarations)))
          (if (i32.eq (call $operator (call $car (local.get $body))) (i32.const ,(runtime-symbol-id 'block)))
            (then (return (call $car (call $cdr (call $car (local.get $body)))))))))))
  #@(call $symbol (i32.const ,(runtime-symbol-id 'lambda))))

(define-runtime-function $capture-backtrace () ()
  (:export "capture-backtrace")
  (:locals ($i i32) ($count i32))
  #@(global.set $backtrace-saved-depth (global.get $backtrace-depth))
  #@(local.set $count (select (global.get $backtrace-depth) (i32.const 64)
    (i32.lt_u (global.get $backtrace-depth) (i32.const 64))))
  #@(array.fill $objects (global.get $backtrace-saved) (i32.const 0) (ref.null $obj) (i32.const 64))
  #@(block $done (loop $next (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
    (array.set $objects (global.get $backtrace-saved) (local.get $i)
      (call $backtrace-name (array.get $objects (global.get $backtrace-frames)
        (i32.and (i32.sub (i32.sub (global.get $backtrace-depth) (i32.const 1)) (local.get $i)) (i32.const 63)))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next))))

(define-runtime-function $print-backtrace () ()
  (:locals ($i i32) ($name :nullable-object) ($home :nullable-object) ($entry :nullable-object))
  #@(if (i32.eqz (global.get $backtrace-saved-depth)) (then (return)))
  #@(block ,(emit-text (format nil "Backtrace (most recent call first):~%")))
  #@(block $done (loop $next
    (br_if $done (i32.ge_u (local.get $i) (global.get $backtrace-saved-depth)))
    (br_if $done (i32.ge_u (local.get $i) (i32.const 64)))
    ,(emit-text "  ")
    (local.set $name (array.get $objects (global.get $backtrace-saved) (local.get $i)))
    ;; Always qualify names, including those accessible in *PACKAGE*. Keep
    ;; ordinary symbol printing unchanged and avoid user-defined printers.
    (local.set $home (call $symbol-home (local.get $name)))
    (if (ref.is_null (local.get $home)) (then ,(emit-text "#:"))
      (else
        (if (ref.eq (local.get $home) (global.get $keyword-package))
          (then (call $emit-char (i32.const 58)))
          (else
            (if (ref.is_null (struct.get $obj $a (local.get $home)))
              (then ,(emit-text "#<DELETED-PACKAGE>"))
              (else (call $print-name (call $car (struct.get $obj $a (local.get $home))))))
            (call $emit-char (i32.const 58))
            (local.set $entry (call $present (call $symbol-name (local.get $name)) (local.get $home)))
            (if (ref.is_null (local.get $entry)) (then (call $emit-char (i32.const 58)))
              (else (if (i32.ne (struct.get $obj $n (local.get $entry)) (i32.const 2))
                (then (call $emit-char (i32.const 58))))))))))
    (call $print-name (call $symbol-name (local.get $name))) (call $emit-char (i32.const 10))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(if (i32.gt_u (global.get $backtrace-saved-depth) (i32.const 64))
    (then ,(emit-text (format nil "  ... older calls omitted~%")))))

(define-runtime-function $format-backtrace () (i32)
  (:export "format-backtrace")
  #@(global.set $cursor (i32.const 0))
  #@(call $print-backtrace)
  #@(global.get $cursor))

(define-runtime-section backtrace-runtime-wat ()
  #@(global $backtrace-depth (mut i32) (i32.const 0))
  #@(global $backtrace-saved-depth (mut i32) (i32.const 0))
  #@(global $backtrace-frames (mut (ref $objects)) (array.new_default $objects (i32.const 64)))
  #@(global $backtrace-saved (mut (ref $objects)) (array.new_default $objects (i32.const 64)))
  ($backtrace-name) ($capture-backtrace) ($print-backtrace) ($format-backtrace))
