;;;; Wasmtime suspension adapter. All scheduling and Lisp state are shared
;;;; with the browser; this file only keeps and resumes native continuations.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $native-thread-yield () ()
  #@(suspend $native-thread-yield-tag))

(define-runtime-function $native-thread-entry () ()
  #@(call $thread-run (call $thread-id)))

(define-runtime-function $native-thread-store (($id i32) ($continuation (ref $native-thread-cont))) ()
  (:locals ($new (ref $native-thread-conts)))
  #@(if (i32.ge_u (local.get $id) (array.len (global.get $native-thread-continuations)))
    (then
      (local.set $new (array.new_default $native-thread-conts (i32.mul (i32.add (local.get $id) (i32.const 1)) (i32.const 2))))
      (array.copy $native-thread-conts $native-thread-conts (local.get $new) (i32.const 0)
        (global.get $native-thread-continuations) (i32.const 0) (array.len (global.get $native-thread-continuations)))
      (global.set $native-thread-continuations (local.get $new))))
  #@(array.set $native-thread-conts (global.get $native-thread-continuations) (local.get $id) (local.get $continuation)))

(defun thread-native-wat ()
  (runtime-wat-sequence
    #@(type $native-thread-function (func))
    #@(type $native-thread-cont (cont $native-thread-function))
    #@(type $native-thread-conts (array (mut (ref null $native-thread-cont))))
    #@(tag $native-thread-yield-tag)
    #@(global $native-thread-continuations (mut (ref $native-thread-conts))
      (array.new_default $native-thread-conts (i32.const 16)))
    #@(elem declare func $native-thread-yield $native-thread-entry $terminal-start)
    ($native-thread-yield) ($native-thread-entry) ($native-thread-store)
    ;; The function is emitted below to make selection's block explicit.
    (runtime-function-definition '$native-thread-start
      (wat-fragment #@(func $native-thread-start (export "_start")
        (local $continuation (ref $native-thread-cont)) (local $candidate (ref null $native-thread-cont))
        (local $id i32) (local $delay f64)
        (table.set $thread-services (i32.const 0) (ref.func $native-thread-yield))
        (local.set $continuation (cont.new $native-thread-cont (ref.func $terminal-start)))
        (loop $dispatch
          (block $next
            (block $yielded (result (ref $native-thread-cont))
              (resume $native-thread-cont (on $native-thread-yield-tag $yielded) (local.get $continuation))
              (if (i32.eqz (local.get $id)) (then (return))
                (else (call $thread-retire (local.get $id))))
              (br $next))
            (local.set $continuation)
            (call $native-thread-store (local.get $id) (local.get $continuation)))
          ;; The suspended stack has already saved its scratch memory.
          (global.set $cursor (i32.const 0)) (call $flush-output) (call $terminal-write)
          (block $ready
            (loop $wait
              (if (global.get $terminal-waiting) (then (drop (call $terminal-poll (f64.const 0)))))
              (local.set $id (call $thread-next))
              (br_if $ready (i32.ge_s (local.get $id) (i32.const 0)))
              (local.set $delay (call $thread-delay))
              (if (global.get $terminal-waiting)
                (then (drop (call $terminal-poll (local.get $delay))))
                (else
                  (if (f64.lt (local.get $delay) (f64.const 0)) (then (return)))
                  (drop (call $wasi-host-service (i32.const 3) (local.get $delay))))) (br $wait)))
          (call $thread-enter (local.get $id))
          (if (i32.lt_u (local.get $id) (array.len (global.get $native-thread-continuations)))
            (then
              (local.set $candidate (array.get $native-thread-conts (global.get $native-thread-continuations) (local.get $id)))
              (array.set $native-thread-conts (global.get $native-thread-continuations) (local.get $id) (ref.null $native-thread-cont))))
          (if (ref.is_null (local.get $candidate))
            (then (local.set $continuation (cont.new $native-thread-cont (ref.func $native-thread-entry))))
            (else (local.set $continuation (ref.as_non_null (local.get $candidate)))))
          (local.set $candidate (ref.null $native-thread-cont))
          (br $dispatch)))))))
