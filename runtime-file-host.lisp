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
  #@(if (call $source-deleted-p (local.get $path)) (then (return (i32.const 0))))
  #@(call $eql (call $car (call $host-file (i32.const 0) (local.get $path))) (call $int (i32.const 5))))

(define-runtime-function $filesystem-path (($path externref)) (i32 i32)
  (:export-raw "filesystem-path")
  (:locals ($start i32) ($length i32))
  #@(local.set $start (global.get $cursor))
  #@(call $print-text (ref.cast (ref $obj) (any.convert_extern (local.get $path))))
  #@(local.set $length (i32.sub (global.get $cursor) (local.get $start)))
  #@(global.set $cursor (local.get $start))
  #@(local.get $start) #@(local.get $length))

(define-runtime-function $filesystem-result
    (($op i32) ($path externref) ($length i32) ($date f64) ($previous externref)) (externref)
  (:export-raw "filesystem-result")
  (:locals ($value :nullable-object) ($name :nullable-object) ($prefix :digits) ($child :digits)
           ($joined :digits) ($end i32))
  #@(local.set $value (ref.cast (ref null $obj) (any.convert_extern (local.get $path))))
  #@(if (i32.eqz (local.get $op))
    (then (return (extern.convert_any (call $cons (call $int (local.get $length))
      (if (result (ref null $obj)) (f64.lt (local.get $date) (f64.const 0))
        (then (ref.null $obj)) (else (call $float (i32.const 4) (local.get $date)))))))))
  #@(if (i32.eq (local.get $op) (i32.const 1))
    (then (return (extern.convert_any (call $filesystem-file
      (local.get $value) (local.get $length) (local.get $date))))))
  #@(local.set $name (call $decode-text (global.get $cursor) (local.get $length)))
  #@(local.set $child (ref.as_non_null (struct.get $obj $digits (local.get $name))))
  #@(local.set $prefix (ref.as_non_null (struct.get $obj $digits (local.get $value))))
  #@(local.set $end (call $path-directory-end (local.get $prefix)))
  #@(local.set $joined (array.new_default $digits (i32.add (local.get $end) (array.len (local.get $child)))))
  #@(array.copy $digits $digits (local.get $joined) (i32.const 0) (local.get $prefix) (i32.const 0) (local.get $end))
  #@(array.copy $digits $digits (local.get $joined) (local.get $end) (local.get $child) (i32.const 0) (array.len (local.get $child)))
  #@(extern.convert_any (call $cons
    (call $cons (call $text (local.get $joined)) (call $bool (f64.eq (local.get $date) (f64.const 2))))
    (ref.cast (ref null $obj) (any.convert_extern (local.get $previous))))))

(define-runtime-function $filesystem-data (($path externref)) (i32 i32)
  (:export-raw "filesystem-data")
  (:locals ($entry :nullable-object) ($buffer (ref $text-stream)) ($bytes :digits) ($i i32) ($length i32))
  #@(local.set $entry (call $local-source-file (ref.cast (ref $obj) (any.convert_extern (local.get $path)))))
  #@(local.set $buffer (call $file-buffer (local.get $entry)))
  #@(local.set $length (struct.get $text-stream $length (local.get $buffer)))
  #@(local.set $bytes (struct.get $text-stream $data (local.get $buffer)))
  #@(call $reserve (local.get $length))
  #@(block $done (loop $copy (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (i32.store8 (i32.add (global.get $cursor) (local.get $i)) (array.get $digits (local.get $bytes) (local.get $i)))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
  #@(global.get $cursor) #@(local.get $length))

(define-runtime-function $filesystem-name (($path externref) ($merge i32)) (externref)
  (:locals ($value :nullable-object) ($extra :nullable-object) ($count i32) ($mask i32) ($cursor i32) ($failed i32))
  #@(local.set $extra (global.get $extra-values)) #@(local.set $count (global.get $value-count))
  #@(local.set $mask (global.get $thread-mask)) #@(local.set $cursor (global.get $cursor))
  #@(global.set $thread-mask (i32.add (local.get $mask) (i32.const 1)))
  #@(block $done
    (block $transfer
        (try_table (catch $transfer $transfer)
          (local.set $value (call $apply
            (if (result (ref null $obj)) (local.get $merge)
              (then ,(clos-library-reference '%source-path)) (else ,(clos-library-reference '%canonical-path)))
            (call $cons (ref.cast (ref $obj) (any.convert_extern (local.get $path))) (ref.null $obj))))
          (br $done)))
    (local.set $failed (i32.const 1)))
  #@(global.set $extra-values (local.get $extra)) #@(global.set $value-count (local.get $count))
  #@(global.set $thread-mask (local.get $mask)) #@(global.set $cursor (local.get $cursor))
  #@(if (local.get $failed) (then (throw $transfer)))
  #@(extern.convert_any (local.get $value)))

;; Transfer file bytes directly through scratch memory. Parsing or printing a
;; Lisp vector for each byte makes multi-megabyte application bundles costly.
(define-runtime-function $filesystem-mount (($path externref) ($length i32)) ()
  (:export-raw "filesystem-mount")
  (:locals ($entry :nullable-object) ($name :nullable-object))
  #@(local.set $entry (call $filesystem-file
    (ref.cast (ref $obj) (any.convert_extern (local.get $path))) (local.get $length) (call $file-time)))
  #@(local.set $name (ref.cast (ref $obj) (any.convert_extern (call $filesystem-name (local.get $path) (i32.const 0)))))
  #@(call $file-unlink (call $local-source-file (local.get $name)))
  #@(struct.set $obj $a (local.get $entry) (local.get $name))
  #@(global.set $source-files (call $cons (local.get $entry) (global.get $source-files))))

(define-runtime-function $filesystem-local-data (($path externref)) (i32 i32)
  (:export-raw "filesystem-local-data")
  (:locals ($name externref))
  #@(local.set $name (call $filesystem-name (local.get $path) (i32.const 1)))
  #@(if (ref.is_null (call $local-source-file (ref.cast (ref $obj) (any.convert_extern (local.get $name)))))
    (then (return (i32.const -1) (i32.const 0))))
  #@(call $filesystem-data (local.get $name)))

(define-runtime-section file-host-runtime-wat ()
  #@(type $filesystem-service-type (func (param i32 externref externref) (result externref i32)))
  #@(table $filesystem-services (export "filesystem-services") 1 1 funcref)
  ($host-file) ($filesystem-wake) ($filesystem-file)
  ($filesystem-name) ($filesystem-mount) ($filesystem-local-data)
  ($filesystem-path) ($filesystem-result) ($filesystem-data) ($source-file-p))
