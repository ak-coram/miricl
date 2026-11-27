;;;; Closure application and builtin function dispatch.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun wat-apply-frame (expression)
  ;; Reuse a bounded ring, restoring overwritten entries on return as well as
  ;; nonlocal exits. No frame objects or argument lists are allocated here.
  (runtime-wat-sequence
    #@(local.set $backtrace-depth (global.get $backtrace-depth))
    #@(local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
    #@(local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
    #@(array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $name))
    #@(global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
    #@(call $clear-values)
    (wat-dynamic-call expression
      :cleanup #@(block
        (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
        (global.set $backtrace-depth (local.get $backtrace-depth))))))

(define-runtime-function $apply0
    (($name :nullable-object) ($args :nullable-object))
    (:nullable-object)
  ;; Internal APPLY owns raw slot storage. Exported native/JS call adapters
  ;; snapshot foreign raw arrays before arriving here or reaching a checkpoint.
  #@(if (ref.test (ref $objects) (local.get $args))
    (then (call $native-validate-packet (ref.cast (ref $objects) (local.get $args)))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(return_call $apply0-ready (local.get $name) (local.get $args)))

;; Only checked internal callers enter after raw admission and the application
;; checkpoint. Keeping the ordinary owner here also serves speculative misses.
(define-runtime-function $apply0-ready
    (($name :nullable-object) ($args :nullable-object))
    (:nullable-object)
  (:locals ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object)
           ($restart-associations :nullable-object) ($catches :nullable-object) ($targets :nullable-object)
           ($thrown i32) ($direct-arity i32) ($direct-usage i32)
           ($backtrace-depth i32) ($backtrace-slot i32) ($backtrace-frame :nullable-object)
           ($result :nullable-object))
  ;; Resolved native functions already carry a typed entry. Preserve the same
  ;; dynamic cleanup and backtrace frame while bypassing generic dispatch.
  (wat-apply-frame
    #@(if (result (ref null eq)) (ref.test (ref $native-function) (local.get $name))
      (then
        (if (result (ref null eq))
            (ref.is_null (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $name))))
          (then (ref.cast (ref null eq) (any.convert_extern
            (call_ref $native-entry (extern.convert_any (local.get $name)) (extern.convert_any (local.get $args))
              (struct.get $native-function $entry (ref.cast (ref $native-function) (local.get $name)))))))
          (else
            (local.set $direct-arity (i32.sub (i32.trunc_f64_u (struct.get $obj $f (ref.cast (ref $obj) (local.get $name)))) (i32.const 2)))
            (local.set $direct-usage (struct.get $obj $n (ref.as_non_null
              (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $name))))))
            (call $native-check-arguments (extern.convert_any (local.get $args))
              (local.get $direct-arity) (local.get $direct-arity))
            (ref.cast (ref null eq) (any.convert_extern
              (call_ref $native-direct-entry (extern.convert_any (local.get $name))
                (if (result externref) (i32.and (i32.gt_u (local.get $direct-arity) (i32.const 0))
                    (i32.or (i32.eqz (local.get $direct-usage))
                      (i32.ne (i32.and (i32.sub (local.get $direct-usage) (i32.const 1)) (i32.const 1)) (i32.const 0))))
                  (then (call $native-argument (extern.convert_any (local.get $args)) (i32.const 0)))
                  (else (ref.null extern)))
                (if (result externref) (i32.and (i32.gt_u (local.get $direct-arity) (i32.const 1))
                    (i32.or (i32.eqz (local.get $direct-usage))
                      (i32.ne (i32.and (i32.sub (local.get $direct-usage) (i32.const 1)) (i32.const 2)) (i32.const 0))))
                  (then (call $native-argument (extern.convert_any (local.get $args)) (i32.const 1)))
                  (else (ref.null extern)))
                (if (result externref) (i32.and (i32.gt_u (local.get $direct-arity) (i32.const 2))
                    (i32.or (i32.eqz (local.get $direct-usage))
                      (i32.ne (i32.and (i32.sub (local.get $direct-usage) (i32.const 1)) (i32.const 4)) (i32.const 0))))
                  (then (call $native-argument (extern.convert_any (local.get $args)) (i32.const 2)))
                  (else (ref.null extern)))
                (if (result externref) (i32.and (i32.gt_u (local.get $direct-arity) (i32.const 3))
                    (i32.or (i32.eqz (local.get $direct-usage))
                      (i32.ne (i32.and (i32.sub (local.get $direct-usage) (i32.const 1)) (i32.const 8)) (i32.const 0))))
                  (then (call $native-argument (extern.convert_any (local.get $args)) (i32.const 3)))
                  (else (ref.null extern)))
                (struct.get $native-direct-code $entry (ref.as_non_null
                  (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $name)))))))))))
      (else (call $apply1 (local.get $name) (local.get $args))))))

(define-runtime-function $fixed-byte-apply
    (($name :nullable-object) ($id i32) ($a :nullable-object) ($b :nullable-object)) (externref)
  (:locals ($spec (ref $obj)) ($field i64) ($result :nullable-object))
  ;; CALL2 has checked the operands, run its checkpoint and selected the
  ;; ordinary APPLY entry. Retain APPLY0's checkpoint before the frame and
  ;; any reads of the bytespec. The admitted ID bounds the canonical lookup.
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (br_if $ordinary (i32.eqz (ref.eq (local.get $name)
      (array.get $objects (global.get $canonical-builtin-functions) (i32.shl (local.get $id) (i32.const 1))))))
    (br_if $ordinary (i32.ne (struct.get $obj $n (ref.cast (ref $obj) (local.get $name)))
      (i32.add (local.get $id) (i32.const 1))))
    (block $framed
      ;; Both admitted paths do bounded work with no guest calls or polls.
      ;; Clear values before constructing the result, including a fresh cons
      ;; for BYTE or a boxed integer when LDB needs one.
      (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'byte)))
        (then
          (br_if $framed (i32.eqz (i32.and
            (ref.test (ref i31) (local.get $a)) (ref.test (ref i31) (local.get $b)))))
          (br_if $framed (i32.lt_s (i32.or
            (i31.get_s (ref.cast (ref i31) (local.get $a)))
            (i31.get_s (ref.cast (ref i31) (local.get $b)))) (i32.const 0)))
          (call $clear-values)
          (return (extern.convert_any (call $cons (local.get $a) (local.get $b))))))
      ,(wat-symbol-branch '(local.get $id) '(ldb ldb-test)
        (runtime-wat-sequence
          ;; Read both live slots only after the second checkpoint. A miss
          ;; leaves the original arguments for the signaling worker.
          #@(br_if $framed (i32.eqz (ref.test (ref $obj) (local.get $a))))
          #@(local.set $spec (ref.cast (ref $obj) (local.get $a)))
          #@(br_if $framed (i32.ne (i32.and (struct.get $obj $tag (local.get $spec)) (i32.const 63)) (i32.const 7)))
          #@(call $try-scalar-integer-field (local.get $b)
            (struct.get $obj $a (local.get $spec)) (struct.get $obj $b (local.get $spec)))
          #@(br_if $framed)
          #@(local.set $field)
          #@(call $clear-values)
          #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'ldb-test)))
            (then (return (extern.convert_any (call $bool (i64.ne (local.get $field) (i64.const 0)))))))
          #@(return (extern.convert_any (call $int64 (local.get $field)))))))
    (return_call $fixed-byte-apply-framed (local.get $name) (local.get $id) (local.get $a) (local.get $b)))
  ;; Resume the original ordinary entry after its checkpoint.
  #@(local.set $result (call $apply0-ready (local.get $name)
    (call $cons2 (local.get $a) (local.get $b) (ref.null eq))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $fixed-byte-apply-framed
    (($name :nullable-object) ($id i32) ($a :nullable-object) ($b :nullable-object)) (externref)
  (:locals ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object)
           ($restart-associations :nullable-object) ($catches :nullable-object) ($targets :nullable-object)
           ($thrown i32) ($backtrace-depth i32) ($backtrace-slot i32)
           ($backtrace-frame :nullable-object) ($result :nullable-object))
  #@(drop ,(wat-apply-frame #@(call $binary-byte-operation (local.get $id) (local.get $a) (local.get $b))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(defun $spread-arguments ()
  (runtime-function-definition '$spread-arguments
    (wat-fragment (miricl-cross:runtime-kernel-wat '$spread-arguments 'miricl-runtime:spread-call-arguments))))

;; Physical APPLY adaptation copies a validated public final spine into owned
;; storage. Its prefix may already be raw; no temporary Lisp wrapper is used.
(define-runtime-function $native-spread-arguments (($args :nullable-object)) (:nullable-object)
  (:locals ($source (ref null $objects)) ($count i32))
  #@(if (i32.eqz (ref.test (ref $objects) (local.get $args)))
    (then (return (call $spread-arguments (local.get $args)))))
  #@(local.set $source (ref.cast (ref $objects) (local.get $args)))
  #@(call $native-validate-packet (ref.as_non_null (local.get $source)))
  #@(local.set $count (array.len (local.get $source)))
  (wat-fail-when #@(i32.lt_u (local.get $count) (i32.const 2)) :wrong-arity)
  #@(return_call $native-spread-list (local.get $source)
    (i32.sub (local.get $count) (i32.const 2))
    (array.get $objects (local.get $source) (i32.sub (local.get $count) (i32.const 1)))))

;; SOURCE is an owned checked packet, or NIL when PREFIX is zero. Validate and
;; copy the public tail in one nonpolling interval, before the selected call.
(define-runtime-function $native-spread-list
    (($source (ref null $objects)) ($prefix i32) ($tail :nullable-object)) (:nullable-object)
  (:locals ($tail-count :nullable-object) ($total i32))
  ;; Reuse the proper-list walk's count immediately, before any callback or
  ;; checkpoint can mutate the public final spine. A cycle keeps the same
  ;; CHECK-APPLY-LIST condition; dotted tails fail inside the checked walker.
  #@(local.set $tail-count (call $checked-apply-list-length (local.get $tail)))
  #@(local.set $total (i32.add (local.get $prefix) (call $natural-count (local.get $tail-count))))
  (wat-fail-when #@(i32.ge_u (local.get $total) (i32.const 65536)) :malformed-form)
  #@(return_call $copy-apply-arguments
    (local.get $source) (local.get $prefix) (local.get $tail) (local.get $total)))

;; The caller admitted exactly TOTAL minus PREFIX conses and owns the interval
;; until this copy ends. A dotted tail leaves callers on their list path.
(define-runtime-function $copy-apply-arguments
    (($source (ref null $objects)) ($prefix i32) ($tail :nullable-object) ($total i32)) ((ref null $objects))
  (:locals ($data (ref $objects)) ($index i32))
  #@(local.set $data (array.new_default $objects (local.get $total)))
  #@(block $prefix-done (loop $copy-prefix
    (br_if $prefix-done (i32.eq (local.get $index) (local.get $prefix)))
    (array.set $objects (local.get $data) (local.get $index)
      (array.get $objects (local.get $source) (i32.add (local.get $index) (i32.const 1))))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy-prefix)))
  #@(block $done (loop $copy-tail
    (br_if $done (i32.eq (local.get $index) (local.get $total)))
    (array.set $objects (local.get $data) (local.get $index)
      (struct.get $obj $a (ref.cast (ref $obj) (local.get $tail))))
    (local.set $tail (struct.get $obj $b (ref.cast (ref $obj) (local.get $tail))))
    (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy-tail)))
  #@(if (result (ref null $objects)) (ref.is_null (local.get $tail))
    (then (local.get $data)) (else (ref.null $objects))))

;; Select only after P3: a public list may have changed since validation, while
;; a packet snapshot is privately owned. Failed bounded extraction keeps the
;; original arguments and ordinary postcheckpoint binder, including its errors.
(define-runtime-function $native-apply-list-selected
    (($name :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($function (ref $native-function)) ($data (ref null $objects))
           ($scan :nullable-object) ($item :nullable-object) ($count :nullable-object) ($index i32) ($size i32)
           ($a :nullable-object) ($b :nullable-object) ($c :nullable-object) ($d :nullable-object))
  #@(if (global.get $debug-evaluating)
    (then (return_call $apply (local.get $name) (local.get $args))))
  #@(if (ref.test (ref $objects) (local.get $args))
    (then (call $native-validate-packet (ref.cast (ref $objects) (local.get $args)))))
  #@(if (global.get $thread-active) (then (call $thread-checkpoint)))
  #@(block $ordinary
    (br_if $ordinary (global.get $debug-evaluating))
    (br_if $ordinary (i32.eqz (ref.test (ref $native-function) (local.get $name))))
    (br_if $ordinary (ref.test (ref $native-debug-function) (local.get $name)))
    (local.set $function (ref.cast (ref $native-function) (local.get $name)))
    (br_if $ordinary (ref.is_null (struct.get $native-function $direct (local.get $function))))
    (if (ref.test (ref $objects) (local.get $args))
      (then
        (local.set $data (ref.cast (ref $objects) (local.get $args)))
        (local.set $size (array.len (local.get $data)))
        (br_if $ordinary (i32.gt_u (local.get $size) (i32.const 4))))
      (else (local.set $scan (local.get $args))))
    (block $done (loop $extract
      (if (ref.is_null (local.get $data))
        (then
          (br_if $done (ref.is_null (local.get $scan)))
          (br_if $ordinary (i32.eq (local.get $index) (i32.const 4)))
          (br_if $ordinary (i32.eqz (ref.test (ref $obj) (local.get $scan))))
          (br_if $ordinary (i32.ne (i32.and
            (struct.get $obj $tag (ref.cast (ref $obj) (local.get $scan))) (i32.const 63)) (i32.const 7)))
          (local.set $item (struct.get $obj $a (ref.cast (ref $obj) (local.get $scan))))
          (local.set $scan (struct.get $obj $b (ref.cast (ref $obj) (local.get $scan)))))
        (else
          (br_if $done (i32.eq (local.get $index) (local.get $size)))
          (local.set $item (array.get $objects (local.get $data) (local.get $index)))))
      (br_if $ordinary (i32.eqz (i32.or (ref.is_null (local.get $item))
        (i32.or (ref.test (ref i31) (local.get $item)) (ref.test (ref $obj) (local.get $item))))))
      (if (i32.eqz (local.get $index)) (then (local.set $a (local.get $item)))
        (else (if (i32.eq (local.get $index) (i32.const 1)) (then (local.set $b (local.get $item)))
          (else (if (i32.eq (local.get $index) (i32.const 2)) (then (local.set $c (local.get $item)))
            (else (local.set $d (local.get $item))))))))
      (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $extract)))
    (br_if $ordinary (f64.ne (struct.get $obj $f (local.get $function))
      (f64.convert_i32_u (i32.add (local.get $index) (i32.const 2)))))
    ;; No callback or poll separates extraction, current-entry admission and
    ;; the shared executor's independently authenticated leaf permission.
    (return (ref.cast (ref null eq) (any.convert_extern
      (call $native-direct-apply-ready (local.get $function)
        (local.get $a) (local.get $b) (local.get $c) (local.get $d) (i32.const 0))))))
  ;; A short proper spine can use the boxed native binder's indexed packet
  ;; after P3, when the shared input's current values have been observed.
  #@(if (i32.eqz (ref.test (ref $objects) (local.get $args)))
    (then
      ;; The postcheckpoint boundary admits finite dotted private REST tails.
      ;; The packet copy below declines those tails and keeps the original.
      (local.set $count (call $checked-list-length (local.get $args) (i32.const 3)))
      (if (i64.lt_s (call $scalar-integer-value (local.get $count)) (i64.const 0))
        (then (call $type-fail (local.get $args) (call $symbol-value ,(clos-library-reference '%proper-list-type))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (block $ordinary-spine
        (br_if $ordinary-spine (global.get $debug-evaluating))
        (br_if $ordinary-spine (i64.gt_u
          (call $scalar-integer-value (local.get $count)) (i64.const 8)))
        (br_if $ordinary-spine (i32.eqz (ref.test (ref $native-function) (local.get $name))))
        (br_if $ordinary-spine (ref.test (ref $native-debug-function) (local.get $name)))
        (br_if $ordinary-spine (i32.eqz (ref.is_null
          (struct.get $native-function $direct (ref.cast (ref $native-function) (local.get $name))))))
        (local.set $data (call $copy-apply-arguments (ref.null $objects) (i32.const 0)
          (local.get $args) (call $natural-count (local.get $count))))
        (br_if $ordinary-spine (ref.is_null (local.get $data)))
        (local.set $args (local.get $data)))))
  #@(return_call $apply0-ready (local.get $name) (local.get $args)))

;; No-prefix APPLY has two existing transport observations: fixed CALL2 shares
;; its public tail and supplies P1; generic packet calls snapshot it without P1.
;; MODE bit zero selects primary demand; bit one preserves that fixed ABI.
(define-runtime-function $native-apply-list-ready
    (($function externref) ($arguments externref) ($mode i32)) (:nullable-object)
  (:locals ($name :nullable-object) ($args :nullable-object) ($outer :nullable-object)
           ($dynamic-package :nullable-object) ($dynamic-variables :nullable-object)
           ($handlers :nullable-object) ($restarts :nullable-object)
           ($restart-associations :nullable-object) ($catches :nullable-object) ($targets :nullable-object)
           ($thrown i32) ($backtrace-depth i32) ($backtrace-slot i32)
           ($backtrace-frame :nullable-object) ($result :nullable-object))
  (wat-fail-when #@(i32.gt_u (local.get $mode) (i32.const 3)) :malformed-form)
  #@(local.set $name (call $native-checked-value (local.get $function)))
  #@(local.set $args (call $native-checked-value (local.get $arguments)))
  #@(local.set $outer (call $canonical-builtin-function (i32.const ,(runtime-symbol-id 'apply)) (i32.const 0)))
  ;; P1 belongs only to the original fixed CALL2 route.
  #@(if (i32.and (local.get $mode) (i32.const 2))
    (then (if (global.get $thread-active) (then (call $thread-checkpoint)))))
  #@(if (global.get $debug-evaluating)
    (then
      ;; Debugger evaluation retains the ordinary adapter and its entry owner.
      (local.set $result (call $apply (local.get $outer)
        (if (result (ref null eq)) (i32.and (local.get $mode) (i32.const 2))
          (then (call $cons (local.get $name) (call $cons (local.get $args) (ref.null eq))))
          (else (array.new_fixed $objects 2 (local.get $name) (local.get $args)))))))
    (else
      ;; P2 precedes installation of the outer APPLY frame, as in APPLY0.
      (if (global.get $thread-active) (then (call $thread-checkpoint)))
      (local.set $backtrace-depth (global.get $backtrace-depth))
      (local.set $backtrace-slot (i32.and (local.get $backtrace-depth) (i32.const 63)))
      (local.set $backtrace-frame (array.get $objects (global.get $backtrace-frames) (local.get $backtrace-slot)))
      (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $outer))
      (global.set $backtrace-depth (i32.add (local.get $backtrace-depth) (i32.const 1)))
      (call $clear-values)
      ,(wat-dynamic-call
        #@(block (result (ref null eq))
          (if (i32.and (local.get $mode) (i32.const 2))
            (then (call $check-apply-list (local.get $args)))
            (else (local.set $args (call $native-spread-list (ref.null $objects) (i32.const 0) (local.get $args)))))
          (call $native-apply-list-selected (local.get $name) (local.get $args)))
        :cleanup #@(block
          (array.set $objects (global.get $backtrace-frames) (local.get $backtrace-slot) (local.get $backtrace-frame))
          (global.set $backtrace-depth (local.get $backtrace-depth))))
      (drop)))
  #@(local.get $result))

(define-runtime-function $native-apply-list
    (($function externref) ($arguments externref) ($mode i32)) (externref)
  (:export-raw "native-apply-list")
  (:locals ($result :nullable-object))
  #@(local.set $result
    (call $native-apply-list-ready (local.get $function) (local.get $arguments) (local.get $mode)))
  #@(if (i32.eqz (i32.and (local.get $mode) (i32.const 1)))
    (then (local.set $result (call $value-list (local.get $result)))))
  #@(call $clear-values)
  #@(extern.convert_any (local.get $result)))

(define-runtime-function $native-apply-captured-list
    (($function externref) ($arguments externref)) (externref externref externref externref i32)
  (:export-raw "native-apply-captured-list")
  #@(call $native-capture-result
    (call $native-apply-list-ready (local.get $function) (local.get $arguments) (i32.const 0))
    (i32.const 1)))

(define-runtime-function $apply-source (($name :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($params :nullable-object) ($param :nullable-object) ($env :nullable-object) ($stop :nullable-object))
  #@(block (result (ref null eq))
        (local.set $params (struct.get $obj $a (ref.cast (ref $obj) (local.get $name))))
        ;; Successful binding checks names and allocates private cells without a
        ;; callback or checkpoint. Retain its original environment as STOP.
        (local.set $env (local.tee $stop (struct.get $obj $c (ref.cast (ref $obj) (local.get $name)))))
        (block $done (loop $next
          (br_if $done (ref.is_null (local.get $params)))
          (local.set $param (call $car (local.get $params)))
          (if (i32.eq (call $id (local.get $param)) (i32.const ,(runtime-symbol-id '&rest)))
            (then
              (local.set $env (call $bind (local.get $env) (call $car (call $cdr (local.get $params))) (local.get $args)))
              (local.set $args (ref.null eq)) (br $done)))
          ,(wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
          (local.set $env (call $bind (local.get $env) (local.get $param) (call $car (local.get $args))))
          ;; Each checked CAR above proved its current non-NIL cell. Successful
          ;; binding cannot callback or checkpoint before these same-cell reads.
          (local.set $args (struct.get $obj $b (ref.cast (ref $obj) (local.get $args))))
          (local.set $params (struct.get $obj $b (ref.cast (ref $obj) (local.get $params)))) (br $next)))
        ,(wat-fail-when #@(i32.eqz (ref.is_null (local.get $args))) :wrong-arity)
        (call $activate-bindings (local.get $env) (local.get $stop))
        (local.set $args (struct.get $obj $b (ref.cast (ref $obj) (local.get $name))))
        ;; Activation may checkpoint while resolving *PACKAGE*. Reload the
        ;; closure environment after it, including live documentation changes.
        (return_call $body (local.get $args) (local.get $env) (struct.get $obj $c (ref.cast (ref $obj) (local.get $name))))
  ))

(define-runtime-function $apply1
    (($name :nullable-object) ($args :nullable-object))
    (:nullable-object)
  (:locals ($code i32) ($id i32)
           ($n i32)
           ($op i32)
           ($cmp i32)
           ($a :nullable-object)
           ($b :nullable-object)
           ($result :nullable-object)
           ($rest :nullable-object)
           ($scan :nullable-object)
           ($env :nullable-object)
           ($params :nullable-object)
           ($binding :nullable-object)
           ($power :object)
           ($r :object)
           ($negative i32)
           ($bit-packet (ref $objects)) ($bit-operation i32) ($bit-count i32)
           ($bit-index i32) ($bit-value :nullable-object) ($bit-word i64) ($bit-next i64))
  #@(if (call $symbolp (local.get $name))
      (then (local.set $name (call $fdefinition (local.get $name)))))
  #@(if (i32.ne (call $tag (local.get $name)) (i32.const 8))
      (then (global.set $undefined-name (local.get $name)) ,(wat-fail :undefined-function)))
  #@(if (ref.test (ref $native-function) (local.get $name))
    (then (return (ref.cast (ref null eq) (any.convert_extern
      (call_ref $native-entry (extern.convert_any (local.get $name)) (extern.convert_any (local.get $args))
        (struct.get $native-function $entry (ref.cast (ref $native-function) (local.get $name)))))))))
  ;; The resolved function has one immutable dispatch code.
  #@(local.set $code (struct.get $obj $n (ref.cast (ref $obj) (local.get $name))))
  #@(if (i32.eq (local.get $code) (i32.const 2147483645))
    (then (return_call $generic-apply (local.get $name) (local.get $args))))
  #@(if (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id 'funcall))))
    (then
      (call $native-check-arguments (extern.convert_any (local.get $args)) (i32.const 1) (i32.const -1))
      (return_call $apply (call $native-packet-ref-cl (local.get $args) (call $int (i32.const 0)))
        (call $native-packet-tail (local.get $args) (i32.const 1)))))
  #@(if (i32.eq (local.get $code) (i32.const ,(1+ (runtime-symbol-id 'apply))))
    (then
      (call $native-check-arguments (extern.convert_any (local.get $args)) (i32.const 2) (i32.const -1))
      (return_call $apply (call $native-packet-ref-cl (local.get $args) (call $int (i32.const 0)))
        (call $native-spread-arguments (local.get $args)))))
  ;; HIR argument binders accept owned raw packets as well as Lisp lists.
  ;; APPLY0 has validated every raw slot before its checkpoint, independently
  ;; of whether the public HIR body contains CHECK-ARGUMENTS.
  #@(if (i32.eq (local.get $code) (i32.const 2147483641))
    (then (return_call $hir-apply (local.get $name) (local.get $args))))
  #@(if (ref.test (ref $objects) (local.get $args))
    (then
      (block $bit-packet-miss
        ;; APPLY0 admitted the owned slots before its checkpoint. Match the
        ;; actual resolved function after that checkpoint and frame setup.
        (br_if $bit-packet-miss
          (i32.gt_u (local.tee $bit-operation
            (i32.sub (local.get $code) (i32.const ,(1+ (runtime-symbol-id 'logand)))))
            (i32.const ,(1- (length *variadic-integer-bit-operations*)))))
        (br_if $bit-packet-miss (global.get $debug-evaluating))
        (local.set $bit-packet (ref.cast (ref $objects) (local.get $args)))
        ;; Bound the fold to four through eight slots, with no guest calls or polls.
        (br_if $bit-packet-miss
          (i32.gt_u (i32.sub (local.tee $bit-count (array.len (local.get $bit-packet)))
            (i32.const 4)) (i32.const 4)))
        (br_if $bit-packet-miss (i32.eqz (ref.eq (local.get $name)
          (array.get $objects (global.get $canonical-builtin-functions)
            (i32.shl (i32.sub (local.get $code) (i32.const 1)) (i32.const 1))))))
        (local.set $bit-operation (i32.and (i32.const 15)
          (i32.shr_u (i32.const ,(loop with first = (runtime-symbol-id 'logand)
                                      for (name op) in *variadic-integer-bit-operations*
                                      for i from 0
                                      do (assert (= (runtime-symbol-id name) (+ first i)))
                                      sum (ash op (* i 4))))
            (i32.shl (local.get $bit-operation) (i32.const 2)))))
        ;; Check every slot in order, even after an absorbing value. A decoding
        ;; miss leaves the original arguments and values for normal dispatch.
        (local.set $bit-value (array.get $objects (local.get $bit-packet) (i32.const 0)))
        ,(wat-decode-scalar-integer '$bit-value '$bit-word '$bit-packet-miss)
        (local.set $bit-index (i32.const 1))
        (block $bit-packet-done
          (loop $bit-packet-next
            (br_if $bit-packet-done (i32.eq (local.get $bit-index) (local.get $bit-count)))
            (local.set $bit-value (array.get $objects (local.get $bit-packet) (local.get $bit-index)))
            ,(wat-decode-scalar-integer '$bit-value '$bit-next '$bit-packet-miss)
            (local.set $bit-word (call $boole64-word (local.get $bit-operation)
              (local.get $bit-word) (local.get $bit-next)))
            (local.set $bit-index (i32.add (local.get $bit-index) (i32.const 1)))
            (br $bit-packet-next)))
        ;; The application frame already cleared secondary values.
        (return_call $int64 (local.get $bit-word)))
      ;; Source, remaining builtins and public MOP protocols consume Lisp lists.
      (local.set $args (call $native-arguments-list (local.get $args)))))
  ;; Source and direct closures need no builtin dispatch. Keep the checked
  ;; packet-to-list conversion above this early dispatch.
  #@(if (i32.eqz (local.get $code))
      (then
        (if (ref.test (ref $direct-function) (local.get $name))
          (then (return (call $direct-function-call
            (ref.cast (ref $direct-function) (local.get $name)) (local.get $args)))))
        (if (ref.test (ref $generic-discriminator) (local.get $name))
          (then (return_call $apply
            (struct.get $obj $a (ref.cast (ref $obj) (local.get $name))) (local.get $args))))
        ,(if *source-bootstrap-p*
             '(return_call $apply-source (local.get $name) (local.get $args))
             (wat-fail :malformed-form))))
  #@(if (i32.eq (local.get $code) (i32.const 2147483642))
    (then ,(wat-arity 1 2147483647)
      (return (call $formatter-call (call $car (local.get $args))
        (struct.get $obj $a (ref.cast (ref $obj) (local.get $name))) (call $cdr (local.get $args))))))
  #@(if (i32.eq (local.get $code) (i32.const 2147483643))
    (then (local.set $result (call $apply (struct.get $obj $a (ref.cast (ref $obj) (local.get $name))) (local.get $args)))
      (call $clear-values)
      (return (call $bool (ref.is_null (local.get $result))))))
  ;; CONSTANTLY and constant macro expanders retain one immutable value.
  #@(if (i32.eq (local.get $code) (i32.const 2147483644))
    (then (if (i32.eqz (ref.is_null (struct.get $obj $b (ref.cast (ref $obj) (local.get $name)))))
        (then ,(wat-arity 2)
          (if (i32.eq (call $natural-count (struct.get $obj $b (ref.cast (ref $obj) (local.get $name)))) (i32.const 1))
            (then (call $arity (call $cdr (call $car (local.get $args))) (i32.const 0) (i32.const 0))))))
      (return (struct.get $obj $a (ref.cast (ref $obj) (local.get $name))))))
  ;; A current-format FASL function shell is not callable until initialization.
  #@(if (i32.eq (local.get $code) (i32.const 2147483640)) (then ,(wat-fail :malformed-form)))
  #@(if (i32.and (i32.ge_u (local.get $code) (i32.const ,+clos-primitive-base+))
                 (i32.lt_u (local.get $code) (i32.const 2147483641)))
      (then (return (call $clos-primitive
        (i32.sub (local.get $code) (i32.const ,+clos-primitive-base+)) (local.get $args)))))
  #@(if (i32.ge_u (local.get $code) (i32.const 2147483646))
      (then (if (i32.le_u (local.get $code) (i32.const 2147483647))
        (then (return (call $condition-reader (local.get $name) (local.get $args)))))))
  #@(local.set $id (i32.sub (local.get $code) (i32.const 1)))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'string)))
    (then ,(wat-arity 1) (return (call $string-designator (call $car (local.get $args))))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'string=)))
    (then (return (call $string-comparison (local.get $args) (i32.const 0)))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'string-equal)))
    (then (return (call $string-comparison (local.get $args) (i32.const 6)))))
  #@(if (i32.and (i32.ge_u (local.get $id) (i32.const ,(runtime-symbol-id 'string/=)))
                (i32.le_u (local.get $id) (i32.const ,(runtime-symbol-id 'string-not-lessp))))
    (then (return (call $string-comparison (local.get $args)
      (i32.add (i32.sub (local.get $id) (i32.const ,(1- (runtime-symbol-id 'string/=))))
        (i32.ge_u (local.get $id) (i32.const ,(runtime-symbol-id 'string-not-equal))))))))
  (wat-symbol-case #@(local.get $id)
    (float-bits
      (wat-arity 1)
      #@(return (call $float-bits (call $car (local.get $args)))))
    (bits-float
      (wat-arity 2)
      #@(return (call $bits-float (call $car (local.get $args)) (call $car (call $cdr (local.get $args))))))
    (readtable-macro-characters
      (wat-arity 1 2)
      #@(return (call $readtable-macro-characters (call $car (local.get $args))
        (call $car (call $cdr (local.get $args))))))
    (subtypep
      (wat-arity 2 3)
      #@(return (call $subtypep-native (call $car (local.get $args))
        (call $car (call $cdr (local.get $args)))
        (call $car (call $cdr (call $cdr (local.get $args)))))))
    (rationalize
      (wat-arity 1)
      #@(return (call $rationalize-native (call $real (call $car (local.get $args))))))
    (upgraded-complex-part-type
      (wat-arity 1 2)
      #@(return (call $upgraded-complex-part-type (call $car (local.get $args))
        (call $car (call $cdr (local.get $args))))))
    ((decode-float scale-float float-radix float-sign float-digits float-precision integer-decode-float)
      #@(return (call $float-property-apply (local.get $id) (local.get $args))))
    ((make-concatenated-stream concatenated-stream-streams make-echo-stream echo-stream-input-stream echo-stream-output-stream
      make-broadcast-stream broadcast-stream-streams file-length file-position file-string-length stream-external-format read-byte write-byte
      make-two-way-stream two-way-stream-input-stream two-way-stream-output-stream
      make-synonym-stream synonym-stream-symbol streamp open-stream-p input-stream-p
      output-stream-p stream-element-type interactive-stream-p)
      #@(return (call $constructed-stream-apply (local.get $id) (local.get $args)))))
  (wat-symbol-case #@(local.get $id)
    (list*
      #@(return_call $list-star-native (local.get $args)))
    (fresh-line
      (wat-arity 0 1)
      #@(return (call $fresh-line-native (call $car (local.get $args)))))
    (terpri
      (wat-arity 0 1)
      #@(call $terpri-native (call $car (local.get $args)))
      #@(return (ref.null eq))))
  #@(if (i32.and (i32.ge_s (local.get $id) (i32.const ,(runtime-symbol-id 'eval)))
                (i32.le_s (local.get $id) (i32.const ,(runtime-symbol-id 'sxhash))))
    (then (return (call $development-apply (local.get $id) (local.get $args)))))
  #@(if (i32.ge_s (local.get $id) (i32.const ,(runtime-symbol-id 'append)))
      (then (return (call $sequence-apply (local.get $id) (local.get $args)))))
  #@(if (i32.ge_s (local.get $id) (i32.const ,(runtime-symbol-id 'condition)))
      (then (return (call $condition-apply (local.get $id) (local.get $args)))))
  #@(if (i32.lt_s (local.get $code) (i32.const 0))
      (then (return (call $symbol-setter (i32.sub (i32.const -1) (local.get $code)) (local.get $args)))))
  #@(if (i32.and (i32.ge_u (local.get $id) (i32.const ,(runtime-symbol-id 'make-package)))
               (i32.le_u (local.get $id) (i32.const ,(runtime-symbol-id 'values))))
      (then (return (call $package-apply (local.get $id) (local.get $args)))))
  #@(if (i32.and (i32.ge_u (local.get $id) (i32.const ,(runtime-symbol-id 'copy-symbol)))
               (i32.le_u (local.get $id) (i32.const ,(runtime-symbol-id 'special-operator-p))))
      (then (return (call $symbol-apply (local.get $id) (local.get $args)))))
  ;; Validate once before reading operands. Every arity check below reuses N
  ;; before callbacks or mutations; builtin loops may consume ARGS afterwards.
  #@(local.set $n (call $length (local.get $args)))
  #@(local.set $a (call $car (local.get $args)))
  #@(local.set $rest (call $cdr (local.get $args)))
  #@(local.set $b (call $car (local.get $rest)))
  #@(if (i32.and (i32.ge_s (local.get $id) (i32.const ,(runtime-symbol-id '+)))
               (i32.le_s (local.get $id) (i32.const ,(runtime-symbol-id '/))))
      (then
        (local.set $op (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id '+))))
        (if (i32.or (i32.eq (local.get $op) (i32.const 1)) (i32.eq (local.get $op) (i32.const 3)))
            (then ,(wat-arity 1 2147483647 #@(local.get $n))
                  (local.set $result (call $number (local.get $a)))
                  (local.set $args (local.get $rest))
                  (if (ref.is_null (local.get $args))
                      (then
                        (if (i32.eq (local.get $op) (i32.const 1))
                          (then (return (call $negate-number (ref.as_non_null (local.get $result))))))
                        (return (call $op (local.get $op) (call $int (i32.const 1)) (local.get $result))))))
            (else (local.set $result (call $int (select (i32.const 0) (i32.const 1) (i32.eqz (local.get $op)))))))
        (block $done (loop $next (br_if $done (ref.is_null (local.get $args)))
          (local.set $result (call $op (local.get $op) (local.get $result) (call $car (local.get $args))))
          (local.set $args (call $cdr (local.get $args))) (br $next))) (return (local.get $result))))
  #@(if (i32.and (i32.ge_s (local.get $id) (i32.const ,(runtime-symbol-id '=)))
               (i32.le_s (local.get $id) (i32.const ,(runtime-symbol-id '/=))))
      (then
        ,(wat-arity 1 2147483647 #@(local.get $n))
        (local.set $op (i32.const 1))
        (block $done (loop $next
          (br_if $done (ref.is_null (local.get $args)))
          (local.set $a (call $car (local.get $args)))
          (if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '=)))
                      (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '/=))))
              (then (drop (call $number (local.get $a)))) (else (drop (call $real (local.get $a)))))
          (local.set $scan (call $cdr (local.get $args)))
          (block $scan-done (loop $pair
            (br_if $scan-done (ref.is_null (local.get $scan)))
            (local.set $b (call $car (local.get $scan)))
            (if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '=)))
                        (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '/=))))
                (then (local.set $cmp (call $equal (local.get $a) (local.get $b)))
                      (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '/=)))
                          (then (local.set $cmp (i32.eqz (local.get $cmp))))))
                (else (local.set $cmp (call $compare (local.get $a) (local.get $b)))
                  (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '<))) (then (local.set $cmp (i32.lt_s (local.get $cmp) (i32.const 0)))))
                  (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '>))) (then (local.set $cmp (i32.gt_s (local.get $cmp) (i32.const 0)))))
                  (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '<=))) (then (local.set $cmp (i32.le_s (local.get $cmp) (i32.const 0)))))
                  (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '>=))) (then (local.set $cmp (i32.ge_s (local.get $cmp) (i32.const 0)))))))
            (local.set $op (i32.and (local.get $op) (local.get $cmp)))
            (if (i32.ne (local.get $id) (i32.const ,(runtime-symbol-id '/=))) (then (br $scan-done)))
            (local.set $scan (call $cdr (local.get $scan))) (br $pair)))
          (local.set $args (call $cdr (local.get $args))) (br $next)))
        (return (call $bool (local.get $op)))))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '1+)))
              (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '1-))))
      (then ,(wat-arity 1 1 #@(local.get $n))
            (return (call $number-step (local.get $a)
              (select (i32.const 1) (i32.const -1) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id '1+))))))))
  (wat-symbol-case #@(local.get $id)
    (abs
      (wat-arity 1 1 #@(local.get $n))
      #@(drop (call $number (local.get $a)))
      #@(if (i32.eq (call $tag (local.get $a)) (i32.const 5))
          (then (return (call $complex-absolute (ref.as_non_null (local.get $a))))))
      #@(if (i32.ge_u (call $tag (local.get $a)) (i32.const 3))
          (then (return (call $float (call $tag (local.get $a)) (f64.abs (struct.get $obj $f (ref.cast (ref $obj) (local.get $a))))))))
      #@(return (if (result (ref eq)) (i32.lt_s (call $compare (local.get $a) (call $int (i32.const 0))) (i32.const 0))
                  (then (call $negate-number (ref.as_non_null (local.get $a))))
                  (else (ref.as_non_null (local.get $a)))))))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'numerator)))
              (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'denominator))))
      (then ,(wat-arity 1 1 #@(local.get $n))
            ,(wat-fail-when #@(i32.eqz (call $typep (local.get $a) (call $symbol (i32.const ,(runtime-symbol-id 'rational))))) :expected-number)
            (return (if (result (ref eq)) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'numerator)))
                        (then (call $numerator (ref.as_non_null (local.get $a)))) (else (call $denominator (ref.as_non_null (local.get $a))))))))
  (wat-symbol-case #@(local.get $id)
    (complex
      (wat-arity 1 2 #@(local.get $n))
      #@(if (ref.is_null (local.get $rest)) (then (local.set $b (call $int (i32.const 0)))))
      #@(return (call $complex (call $real (local.get $a)) (call $real (local.get $b))))))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'realpart)))
              (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'imagpart))))
      (then ,(wat-arity 1 1 #@(local.get $n))
            (return (call $part (call $number (local.get $a)) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'imagpart)))))))
  (wat-symbol-case #@(local.get $id)
    (rational
      (wat-arity 1 1 #@(local.get $n))
      #@(return (call $rational (call $real (local.get $a)))))
    (float
      (wat-arity 1 2 #@(local.get $n))
      #@(drop (call $real (local.get $a)))
      #@(local.set $op (select (call $tag (local.get $a)) (i32.const 3) (i32.ge_u (call $tag (local.get $a)) (i32.const 3))))
      #@(if (i32.eqz (ref.is_null (local.get $rest)))
          (then ,(wat-fail-when #@(i32.eqz (call $typep (local.get $b) (call $symbol (i32.const ,(runtime-symbol-id 'float))))) :expected-number)
                (local.set $op (call $tag (local.get $b)))))
      #@(return (call $float (local.get $op) (call $coerce-float (ref.as_non_null (local.get $a)) (local.get $op)))))
    (typep
      (wat-arity 2 3 #@(local.get $n))
      #@(return (call $bool (call $typep-environment (local.get $a) (local.get $b)
        (call $car (call $cdr (call $cdr (local.get $args)))))))))
  #@(if (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'zerop)))
      (i32.or (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'plusp)))
              (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'minusp)))))
      (then ,(wat-arity 1 1 #@(local.get $n))
            (if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'zerop)))
                (then (return (call $bool (call $zero (call $number (local.get $a)))))))
            (local.set $cmp (call $compare (local.get $a) (call $int (i32.const 0))))
            (return (call $bool (if (result i32) (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'plusp)))
                 (then (i32.gt_s (local.get $cmp) (i32.const 0))) (else (i32.lt_s (local.get $cmp) (i32.const 0))))))))
  (wat-symbol-case #@(local.get $id)
    ((not null)
      (wat-arity 1 1 #@(local.get $n))
      #@(return (call $bool (ref.is_null (local.get $a))))))
  (runtime-wat-forms (loop for predicate in '(numberp integerp rationalp realp complexp floatp)
          for type in '(number integer rational real complex float)
          collect `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id predicate)))
                       (then ,(wat-arity 1 1 #@(local.get $n))
                             (return (call $bool (call $typep (local.get $a) (call $symbol (i32.const ,(runtime-symbol-id type))))))))))
  (wat-symbol-case #@(local.get $id)
    (cons
      (wat-arity 2 2 #@(local.get $n))
      #@(return (call $cons (local.get $a) (local.get $b))))
    (car
      (wat-arity 1 1 #@(local.get $n))
      #@(return (call $car (local.get $a))))
    (cdr
      (wat-arity 1 1 #@(local.get $n))
      #@(return (call $cdr (local.get $a))))
    (list
      #@(return (local.get $args)))
    (gcd
      #@(local.set $power (call $int (i32.const 0)))
      #@(block $done (loop $next (br_if $done (ref.is_null (local.get $args)))
        (local.set $a (call $car (local.get $args)))
        (if (i32.ne (call $tag (local.get $a)) (i32.const 1))
          (then (call $list-type-error (local.get $a) (i32.const ,(runtime-symbol-id 'integer)))))
        (local.set $power (call $gcd (local.get $power) (ref.as_non_null (local.get $a))))
        (local.set $args (call $cdr (local.get $args))) (br $next)))
      #@(return (local.get $power)))
    (expt
      (wat-arity 2 2 #@(local.get $n))
      #@(drop (call $number (local.get $a)))
      #@(drop (call $number (local.get $b)))
      #@(if (i32.ne (call $tag (local.get $b)) (i32.const 1))
        (then
          (if (call $zero (ref.as_non_null (local.get $b)))
            (then (return (call $expt-one (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))))
          (if (call $zero (ref.as_non_null (local.get $a)))
            (then (if (i32.gt_s (call $compare (call $part (ref.as_non_null (local.get $b)) (i32.const 0))
              (call $int (i32.const 0))) (i32.const 0))
              (then (return (call $op (i32.const 2) (local.get $a) (local.get $b)))))))
          (if (i32.gt_u (i32.sub (call $tag (local.get $b)) (i32.const 3)) (i32.const 1))
            (then (return (call $expt-transcendental
              (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))))
          (local.set $power (call $rational (ref.as_non_null (local.get $b))))
          (if (i32.ne (call $tag (local.get $power)) (i32.const 1))
            (then (return (call $expt-transcendental
              (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))))
          ;; Float integral powers retain contagion while using exact integer
          ;; exponent bits. Convert the base before squaring to bound storage.
          (local.set $a (call $op (i32.const 2) (local.get $a)
            (call $float (call $tag (local.get $b)) (f64.const 1))))
          (local.set $b (local.get $power))))
      #@(if (i32.le_u (i32.sub (call $tag (local.get $a)) (i32.const 3)) (i32.const 1))
        (then (return (call $expt-float-integer (ref.as_non_null (local.get $a))
          (ref.as_non_null (local.get $b)) (local.get $args)))))
      #@(local.set $negative (i32.lt_s (call $value-n (local.get $b)) (i32.const 0)))
      #@(local.set $power (call $iabs (ref.as_non_null (local.get $b))))
      #@(local.set $result (call $expt-one (ref.as_non_null (local.get $a)) (ref.as_non_null (local.get $b))))
      #@(block $done (loop $next
        (br_if $done (call $zero (local.get $power)))
        (call $idiv (local.get $power) (call $int (i32.const 2))) (local.set $r) (local.set $power)
        (if (i32.eqz (call $zero (local.get $r))) (then (local.set $result (call $op (i32.const 2) (local.get $result) (local.get $a)))))
        (if (i32.eqz (call $zero (local.get $power))) (then (local.set $a (call $op (i32.const 2) (local.get $a) (local.get $a)))))
        (br $next)))
      #@(if (local.get $negative) (then (local.set $result (call $op (i32.const 3) (call $int (i32.const 1)) (local.get $result)))))
      #@(return (local.get $result))))
  (wat-fail :undefined-function)
  #@(ref.null eq))

(define-runtime-section apply-runtime-wat ()
  ($apply0) ($apply0-ready) ($fixed-byte-apply) ($fixed-byte-apply-framed)
  (if *source-bootstrap-p* ($apply-source) "")
  ($spread-arguments) ($native-spread-arguments) ($native-spread-list) ($copy-apply-arguments)
  ($native-apply-list-selected) ($native-apply-list-ready) ($native-apply-list) ($native-apply-captured-list)
  ($apply1))
