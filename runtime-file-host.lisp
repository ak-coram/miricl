;;;; Optional read-through filesystem; writes remain in the mounted filesystem.
;;;; Operations: 0 stat -> (kind . date), 1 read -> source entry,
;;;; 2 directory -> ((path . directory-p) ...), -1 poll, -2 cancel.
;;;; Service results carry status 0 (done), 1 (waiting) or 2 (file error).
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $host-file
    (($operation i32) ($path :nullable-object)) (:nullable-object)
  (:locals ($value externref) ($request externref) ($status i32) ($thrown i32)
           ($cleanup externref) ($cleanup-status i32))
  #@(if (ref.is_null (table.get $filesystem-services (i32.const 0)))
    (then (return (ref.null $obj))))
  ;; Browser I/O runs through the existing asynchronous thread driver.
  #@(if (i32.or (i32.eqz (global.get $thread-active)) (global.get $javascript-depth))
    (then (call $file-error (local.get $path))))
  #@(call_indirect $filesystem-services (type $filesystem-service-type)
    (local.get $operation) (extern.convert_any (local.get $path))
    (extern.convert_any (call $thread-current)) (i32.const 0))
  #@(local.set $status)
  #@(local.set $value)
  #@(if (i32.eq (local.get $status) (i32.const 1))
    (then
      (local.set $request (local.get $value))
      (local.set $thrown (i32.const 1))
      (block $failed
        (try_table (catch $transfer $failed)
          (loop $wait
            (call $thread-park (f64.const -1))
            (call_indirect $filesystem-services (type $filesystem-service-type)
              (i32.const -1) (extern.convert_any (local.get $path)) (local.get $request) (i32.const 0))
            (local.set $status) (local.set $value)
            (br_if $wait (i32.eq (local.get $status) (i32.const 1))))
          (local.set $thrown (i32.const 0))))
      ;; Cancel before a nonlocal exit can retire the waiting thread.
      (call_indirect $filesystem-services (type $filesystem-service-type)
        (i32.const -2) (extern.convert_any (local.get $path)) (local.get $request) (i32.const 0))
      (local.set $cleanup-status) (local.set $cleanup)
      (if (local.get $thrown) (then (throw $transfer)))))
  #@(if (i32.ne (local.get $status) (i32.const 0)) (then (call $file-error (local.get $path))))
  #@(ref.cast (ref null $obj) (any.convert_extern (local.get $value))))

(define-runtime-function $filesystem-wake (($thread (ref $green-thread))) ()
  (:export-raw "filesystem-wake")
  ;; Retain the native thread object, since numeric thread ids can be reused.
  #@(if (i32.eq (struct.get $green-thread $state (local.get $thread)) (i32.const 1))
    (then (struct.set $green-thread $state (local.get $thread) (i32.const 0)))))

(define-runtime-function $filesystem-file
    (($path :nullable-object) ($length i32) ($date f64)) (:nullable-object)
  (:export-raw "filesystem-file")
  (:locals ($entry :nullable-object) ($buffer (ref $text-stream)) ($bytes :digits) ($i i32))
  #@(local.set $bytes (array.new_default $digits (local.get $length)))
  #@(block $done (loop $copy
    (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (array.set $digits (local.get $bytes) (local.get $i)
      (i32.load8_u (i32.add (global.get $cursor) (local.get $i))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
  #@(local.set $buffer (ref.cast (ref $text-stream) (call $report-stream)))
  #@(struct.set $text-stream $data (local.get $buffer) (local.get $bytes))
  #@(struct.set $text-stream $length (local.get $buffer) (local.get $length))
  #@(local.set $entry (call $new-source-file (local.get $path) (ref.null $obj)))
  #@(struct.set $obj $c (local.get $entry) (local.get $buffer))
  #@(struct.set $source-entry $modified (ref.cast (ref $source-entry) (local.get $entry)) (local.get $date))
  #@(local.get $entry))

(define-runtime-function $source-file-p (($path :nullable-object)) (i32)
  #@(if (i32.eqz (ref.is_null (call $local-source-file (local.get $path))))
    (then (return (i32.const 1))))
  #@(call $eql (call $car (call $host-file (i32.const 0) (local.get $path))) (call $int (i32.const 5))))

(define-runtime-section file-host-runtime-wat ()
  #@(type $filesystem-service-type (func (param i32 externref externref) (result externref i32)))
  #@(table $filesystem-services (export "filesystem-services") 1 1 funcref)
  ($host-file) ($filesystem-wake) ($filesystem-file) ($source-file-p))
