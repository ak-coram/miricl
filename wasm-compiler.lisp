;;;; Native Wasm backend with immediate integer values and scalar arithmetic;
;;;; runtime primitives handle generic Common Lisp operations and overflow.
(in-package :miricl-compiler)

;; Shared instruction records must be known while the host compiles this
;; emitter, not only when it subsequently loads the bytecode optimizer.
(defmacro native-op-code (op) (list 'svref (list 'the 'simple-vector op) 0))
(defmacro native-op-argument (op) (list 'svref (list 'the 'simple-vector op) 1))
(defmacro native-op-start (op) (list 'svref (list 'the 'simple-vector op) 2))
(defmacro native-op-end (op) (list 'svref (list 'the 'simple-vector op) 3))
(defmacro native-op-successors (op) (list 'svref (list 'the 'simple-vector op) 4))
(defmacro native-op-live-in (op) (list 'svref (list 'the 'simple-vector op) 5))
(defmacro native-op-live-out (op) (list 'svref (list 'the 'simple-vector op) 6))
(defmacro native-op-uses (op) (list 'svref (list 'the 'simple-vector op) 7))
(defmacro native-op-definitions (op) (list 'svref (list 'the 'simple-vector op) 8))

(defvar *compile-evaluations* nil
  "When true, EVAL, source loading and local REPL input use the native compiler.
Bind to NIL to interpret forms, including forms without native lowering.")
(defvar *compiling-evaluation* nil)

(defvar *wasm-bytes*)
(defvar *wasm-literals*)
(defvar *wasm-literal-index*)
(defvar *wasm-literal-locals* nil)
(defvar *wasm-literal-groups* nil)
(defvar *native-functions*)
(defvar *wasm-debug* nil)
(defvar *native-function-indices*)
(defvar *wasm-function-relocations* nil)
(defvar *wasm-current-function*)
(defvar *wasm-static-closures*)
(defvar *wasm-label-depths*)
(defvar *wasm-next-blocks*)
(defvar *wasm-body-keys*)
(defvar *wasm-function-plans* nil)
(defvar *wasm-direct-workers* nil)
(defvar *wasm-local-call-functions* nil)
(defvar *wasm-scoped-call-functions* nil)
(defvar *wasm-named-recursion* nil)
(defvar *wasm-named-single-result* nil)
(defvar *wasm-named-call-plan* nil)
(defvar *wasm-call-frame-locals* nil)
;; Host runtime linkage may erase call-only references to verified closed
;; workers. Ordinary guest/application emission never enables this contract.
(defvar *wasm-closed-worker-functions* nil)
;; Host runtime adapters may lower a proved immutable fixed callback. Ordinary
;; guest emission leaves this NIL; declarations never establish its identity.
(defvar *wasm-fixed-call-emitter* nil)
(defvar *native-call-stack-effects* nil)
(defvar *native-import-stack-effects* nil)
(defvar *native-scalar-stack-effects* nil)
(defvar *wasm-worker-origins* nil)
(defvar *wasm-private-scalar-enabled* t)
(defvar *wasm-private-scalar-arguments-enabled* t)
(defvar *wasm-private-scalar-worker* nil)
(defvar *wasm-reusable-scalars* nil)
(defvar *wasm-reusable-scalar-result* nil)
(defvar *wasm-reusable-scalar-arguments* nil)
(defvar *wasm-reusable-scalar-identity* nil)
(defvar *wasm-private-scalar-framed* nil)
(defvar *wasm-private-scalar-sites* nil)
(defvar *wasm-private-scalar-argument* nil)
(defvar *wasm-private-scalar-ingress* nil)
(defvar *wasm-private-scalar-width* nil)
(defvar *wasm-private-scalar-type* nil)
(defvar *wasm-private-scalar-return* nil)
(defvar *wasm-private-scalar-forward* nil)
(defvar *wasm-snapshot-mask-functions* nil)
(defvar *wasm-direct-raw-bodies* nil)
(defvar *wasm-shared-entries* nil)
(defvar *wasm-inline-constants* nil)
(defvar *wasm-last-local* nil)
(defvar *wasm-last-restore* nil)
(defvar *wasm-single-use-entry* nil)
(defvar *compiler-bootstrapping* nil)
(defvar *wasm-bootstrap-direct-workers* nil)
(defvar *wasm-import-counts* nil)
(defvar *native-import-indices* nil)
(defvar *wasm-referenced-functions*)
(defvar *wasm-local-cells*)
(defvar *wasm-live-values*)
(defvar *wasm-locals*)
(defvar *wasm-single-calls*)
(defvar *wasm-scalar-groups*)
(defvar *wasm-full-groups*)
(defvar *wasm-object-types*)
(defvar *wasm-dispatch-depth*)
(defvar *wasm-dispatch-indices*)
(defvar *wasm-dynamic-bindings*)
(defvar *wasm-entry-environment*)
(defvar *wasm-exceptions*)
(defvar *wasm-tail-calls* nil)
(defvar *wasm-integer-widths* nil)
(defvar *wasm-inferred-types* nil)
(defvar *wasm-type-guards* nil)
(defvar *wasm-inline-vector-calls* nil)
(defvar *wasm-inline-simple-calls* nil)
(defvar *wasm-dynamic-rests* nil)
(defvar *wasm-proven-integer-arithmetic* nil)
(defvar *wasm-proven-cell-types* nil)
(defvar *wasm-inline-references* nil)
(defvar *wasm-branch-tests* nil)
(defvar *wasm-reused-values* nil)
(defvar *wasm-checked-integer-copies* nil)
(defvar *wasm-cons-forward-blocks* nil)
(defvar *wasm-proper-list-checks* nil)
(defvar *wasm-unboxed-values* nil)
(defvar *wasm-vector-snapshot-prefixes* nil)
(defvar *wasm-vector-snapshot-operations* nil)
(defvar *wasm-low-field* nil)
(defvar *wasm-low-field-ash-candidates* nil)
(defvar *wasm-literal-byte-extract-owners* nil)
(defvar *wasm-tiered-values* nil)
(defvar *wasm-tiered-comparisons* nil)
(defvar *wasm-hoisted-values* nil)
(defvar *wasm-loop-hoists* nil)
(defvar *wasm-structured-labels*)
(defvar *wasm-loop-regions*)
(defvar *wasm-control-depth*)
(defvar *wasm-optimization-barriers* nil)
(defvar *wasm-result-arities* nil)
(defvar *wasm-result-callees* nil)
(defvar *wasm-multiple-result-functions* nil)
(defvar *wasm-frameless-multiple-functions* nil)
(defvar *wasm-boxed-result-workers* nil)
(defvar *wasm-multiple-result-locals* nil)
(defvar *wasm-multiple-result-count* nil)
(defvar *wasm-captured-result-locals* nil)
(defvar *wasm-captured-result-p* nil)
;; The result analysis leaves the interpreted-to-native bootstrap and host
;; linker on the ordinary ABI. These switches permit matched comparisons of
;; independently compiled producers and consumers in the dynamic compiler.
(defvar *wasm-counted-result-producer* t)
(defvar *wasm-counted-result-consumer* t)
(defvar *wasm-native-multiple-results* t)
(defvar *wasm-argument-usages* nil)
(defvar *wasm-primary-functions* nil)

;; IDs are dense within one function. Slot zero represents the NIL key used
;; by instructions without outputs; value zero starts at slot one. Keeping
;; this as a generalized place also supports SETF, PUSH and INCF directly.
(defmacro native-value-slot (table value)
  (let ((entry (gensym "HIR-VALUE")))
    (list 'svref (list 'the 'simple-vector table)
      (list 'the 'fixnum
        (list 'let (list (list entry value))
          (list 'if entry (list '1+ (list 'hir-value-id entry)) 0))))))

(defmacro native-value-table (function &optional initial)
  (list 'the 'simple-vector
    (list 'make-array (list '1+ (list 'hir-function-value-count function))
          :initial-element initial)))

(setf (get 'native-value-slot 'miricl::runtime-inline-macro) t
      (get 'native-value-table 'miricl::runtime-inline-macro) t)
(defparameter *native-imports*
  '(("native-literal" (:object :i32) :object)
    ("native-literal-function" (:object :i32) :object)
    ("native-tail-call" (:object :object) :object)
    ("native-tail-direct" (:object) :i32)
    ("native-tail-enter" (:object) :object)
    ("native-tail-multiple-value-call" (:object :object :object) :object)
    ("native-int64" (:i64) :object)
    ("native-unbox-int64" (:object) :i64)
    ("native-float-pair" (:i32 :i32 :i32 :i64 :object :object) (:i64 :i32 :object))
    ("native-float-transport" (:i32 :i32 :i32 :i64 :object :object :i64 :i32) (:i64 :i32 :object))
    ("native-scalar-call2" (:object :object :object) (:i64 :i32 :object))
    ("native-scalar-call" (:object :object :object :object :object :i32 :i32) (:i64 :i32 :object))
    ("native-scalar-box" (:i64 :i32 :object :i32) :object)
    ("native-scalar-unpack" (:object :object :object :object :i32) (:i64 :i64 :i64 :i64 :i32))
    ("native-check-int64" (:object) :object)
    ("native-add-integer" (:object :object) :object)
    ("native-sub-integer" (:object :object) :object)
    ("native-mul-integer" (:object :object) :object)
    ("native-add-int64" (:i64 :i64) :object)
    ("native-sub-int64" (:i64 :i64) :object)
    ("native-mul-int64" (:i64 :i64) :object)
    ("native-logand-int64" (:i64 :i64) :object)
    ("native-logior-int64" (:i64 :i64) :object)
    ("native-logxor-int64" (:i64 :i64) :object)
    ("native-ash-int64" (:i64 :i64) :object)
    ("native-add-int64-checked" (:i64 :i64) :object)
    ("native-sub-int64-checked" (:i64 :i64) :object)
    ("native-mul-int64-checked" (:i64 :i64) :object)
    ("native-lt-int64" (:i64 :i64) :object)
    ("native-gt-int64" (:i64 :i64) :object)
    ("native-le-int64" (:i64 :i64) :object)
    ("native-ge-int64" (:i64 :i64) :object)
    ("native-eq-int64" (:i64 :i64) :object)
    ("native-ne-int64" (:i64 :i64) :object)
    ("native-vector-ref" (:object :i32) :object)
    ("native-vector-ref-trusted" (:object :i32) :object)
    ("native-vector-ref-inline" (:object :i64) :object)
    ("native-vector-set-inline" (:object :object :i64) :object)
    ("native-sbit-ref-inline" (:object :object) :object)
    ("native-row-major-ref-inline" (:object :object) :object)
    ("native-row-major-set-inline" (:object :object :object) :object)
    ("native-character-ref-inline" (:object :object) :object)
    ("native-character-set-inline" (:object :object :object) :object)
    ("native-character-set-code-inline" (:i32 :object :object) nil)
    ("native-simple-character-set-inline" (:object :object :i64) :object)
    ("native-code-character-inline" (:object) :object)
    ("native-make-character-vector-inline" (:object) :object)
    ("native-make-string-inline" (:object) :object)
    ("native-make-vector-inline" (:object :object) :object)
    ("native-ordered-compare-inline" (:object :object :i32) :object)
    ("native-zero-inline" (:object) :object)
    ("native-character-code-inline" (:object) :object)
    ("native-character-code-at-inline" (:object :object) :object)
    ("native-character-code-at64-inline" (:object :i64) :object)
    ("native-character-code-at-unboxed-inline" (:object :object) :i64)
    ("native-character-code-at64-unboxed-inline" (:object :i64) :i64)
    ("native-cdr-inline-checked" (:object) :object)
    ("native-list-path" (:object :i32) :object)
    ("native-set-list-path" (:object :object :i32) :object)
    ("native-car-list-inline" (:object) :object)
    ("native-cdr-list-inline" (:object) :object)
    ("native-eq-inline" (:object :object) :object)
    ("native-eql-inline" (:object :object) :object)
    ("native-eql-test" (:object :object) :i32)
    ("native-boolean" (:i32) :object)
    ("native-length-inline" (:object) :object)
    ("native-list-length-inline" (:object) :object)
    ("native-vector-length-inline" (:object) :object)
    ("native-gethash-inline" (:object :object :object :i32) :object)
    ("native-gethash-primary-inline" (:object :object) :object)
    ("native-puthash-inline" (:object :object :object) :object)
    ("native-consp-inline" (:object) :object)
    ("native-simple-vector-p-inline" (:object) :object)
    ("native-vector-p-inline" (:object) :object)
    ("native-string-p-inline" (:object) :object)
    ("native-integer-p-inline" (:object) :object)
    ("native-character-p-inline" (:object) :object)
    ("native-integer-length-inline" (:object) :object)
    ("native-ash-general-inline" (:object :object) :object)
    ("native-logand-general-inline" (:object :object) :object)
    ("native-mod-general-inline" (:object :object) :object)
    ("native-truncate-primary-inline" (:object :object) :object)
    ("native-argument" (:object :i32) :object)
    ("native-arguments" (:i32) :object)
    ("native-argument-set" (:object :i32 :object) :object)
    ("native-ingress-arguments" (:object) :object)
    ("native-argument-present" (:object :i32) :object)
    ("native-keyword-present" (:object :i32 :object) :object)
    ("native-keyword-argument" (:object :i32 :object) :object)
    ("native-check-keywords" (:object :i32 :object :i32) nil)
    ("native-rest-argument" (:object :i32) :object)
    ("native-prepare-dynamic-rest" (:object :i32) :object)
    ("native-dynamic-rest-length" (:object :i32) :object)
    ("native-check-arguments" (:object :i32 :i32) nil)
    ("native-check-apply-arguments" (:object :i32) nil)
    ("native-check-apply-list" (:object) nil)
    ("native-environment" nil :object)
    ("native-global-read" (:object) :object)
    ("native-global-write" (:object :object) :object)
    ("native-function-reference" (:object) :object)
    ("native-standard-function-reference" (:object) :object)
    ("native-cons" (:object :object) :object)
    ("native-cons2" (:object :object :object) :object)
    ("native-list2" (:object :object) :object)
    ("native-cell" (:object) :object)
    ("native-primary" (:object) :object)
    ("native-cell-write" (:object :object) :object)
    ("native-call" (:object :object :object) :object)
    ("native-primary-call" (:object :object :object) :object)
    ("native-primary-call5" (:object :object :object :object :object :object) :object)
    ("native-return" (:object) :object)
    ("native-attach-direct" (:object :function) :object)
    ("native-attach-scalar" (:object :function) :object)
    ("native-attach-direct-usage" (:object :function :i32) :object)
    ("native-shared-entry" (:object :object) :object)
    ("native-close" (:function :object :object) :object)
    ("native-close-fixed" (:function :object :object :i32) :object)
    ("native-close-cached" (:function :object :object :i32 :i32) :object)
    ("native-debug-close" (:object :object :i32) :object)
    ("native-capture" (:object :i32) :object)
    ("native-multiple-value-call" (:object :object :object) :object)
    ("native-fixed-call" (:object :object :object :object :object :i32) :object)
    ("native-call0" (:object) :object)
    ("native-call1" (:object :object) :object)
    ("native-call-values1" (:object :object) (:object :object :object :object :i32))
    ("native-call-captured1" (:object :object) (:object :object :object :object :i32))
    ("native-call2" (:object :object :object) :object)
    ("native-apply-list" (:object :object :i32) :object)
    ("native-apply-captured-list" (:object :object) (:object :object :object :object :i32))
    ("native-captured-result-ref" (:object :i32 :i32) :object)
    ("native-captured-extra-list" (:object :i32) :object)
    ("native-return-counted8" (:object :object :object :object :object :object :object :object :i32) :object)
    ("native-call3" (:object :object :object :object) :object)
    ("native-call4" (:object :object :object :object :object) :object)
    ("native-standard-call0" (:i32) :object)
    ("native-standard-call1" (:i32 :object) :object)
    ("native-standard-call2" (:i32 :object :object) :object)
    ("native-standard-call3" (:i32 :object :object :object) :object)
    ("native-standard-call4" (:i32 :object :object :object :object) :object)
    ("native-aref-int64" (:object :i64) :object)
    ("native-aref-i31-index" (:object :i32) :object)
    ("native-aref-snapshot" (:object :object) (:i64 :i32 :object))
    ("native-aref-snapshot64" (:object :i64) (:i64 :i32 :object))
    ("native-aref-snapshot-transport" (:object :object :i64 :i32) (:i64 :i32 :object))
    ("native-set-aref-int64" (:object :i64 :object :object) nil)
    ("native-search-list" (:object :object :i32) :object)
    ("native-poll" nil nil)
    ("native-frame-enter" (:object) (:i32 :object))
    ("native-frame-leave" (:object :i32 :object :i32) :object)
    ("native-return1" (:object) :object)
    ("native-return2" (:object :object) :object)
    ("native-return3" (:object :object :object) :object)
    ("native-bind" (:object :object :object) :object)
    ("native-restore-environment" (:object) nil)
    ("native-control" (:object :i32 :object :object :object) :object)
    ("native-throw" (:object :object :object) nil)
    ("native-block-enter" (:object) :object)
    ("native-block-environment" (:object) :object)
    ("native-block-return" (:object :object) nil)
    ("native-block-match" (:object) :i32)
    ("native-block-values" nil :object)
    ("native-restart-signal" (:object :i32 :object :object) :object)
    ("native-eq-test" (:object :object) :i32)
    ("native-consp-test" (:object) :i32)
    ("native-tail-call0" (:object) :object)
    ("native-tail-call1" (:object :object) :object)
    ("native-tail-call2" (:object :object :object) :object)
    ("native-tail-call3" (:object :object :object :object) :object)
    ("native-tail-call4" (:object :object :object :object :object) :object)
    ("native-work-limit" (:i32) :i32)
    ("native-work-complete" (:i32) nil)
    ("native-poll-token" nil :i64)
    ("native-low-field-add" (:i32 :object :object) :object)
    ("native-low-field-ash" (:i32 :object :object) :object)
    ("native-literal-byte-extract" (:i32 :i32 :object) :object)))

(defun wasm-byte (byte)
  (declare (type (unsigned-byte 8) byte))
  ;; VECTOR-PUSH-EXTEND returns exactly one index. Keep the byte writer on
  ;; the scalar call path even when its caller could observe multiple values.
  (values (vector-push-extend byte *wasm-bytes*)))
(defun wasm-append (bytes &optional (start 0) (end (length bytes)))
  (declare (type vector bytes) (type (unsigned-byte 32) start end))
  ;; Append whole byte ranges using the runtime's sequence copy loop. Emission
  ;; and relocation otherwise dispatch WASM-BYTE once per already encoded byte.
  (let* ((position (length *wasm-bytes*)) (size (+ position (- end start))))
    (declare (type (unsigned-byte 32) position size) (type vector *wasm-bytes*))
    (when (> size (array-total-size *wasm-bytes*))
      (setq *wasm-bytes* (adjust-array *wasm-bytes*
                          (max size (* 2 (array-total-size *wasm-bytes*))))))
    (setf (fill-pointer *wasm-bytes*) size)
    (replace *wasm-bytes* bytes :start1 position :start2 start :end2 end)))
(defun wasm-u32 (number)
  (declare (type (unsigned-byte 32) number))
  ;; Five bytes cover every valid operand. An internal negative branch depth
  ;; must signal a compiler condition instead of growing the byte buffer forever.
  (loop repeat 5 do
    (wasm-byte (logior (logand number 127) (if (> number 127) 128 0)))
    (setq number (ash number -7)) (when (zerop number) (return-from wasm-u32 nil)))
  (compiler-input-fail "Wasm unsigned operand" number "operand did not terminate within five bytes"))
(defun wasm-u32-width (number)
  ;; Callers establish the unsigned format range before pricing. Four shifts
  ;; suffice even when bit 31 is set; no host fixnum representation is assumed.
  (declare (type (unsigned-byte 32) number))
  (let ((width 1))
    (declare (type (integer 1 5) width))
    (tagbody next
       (if (< number 128) (go done))
       (setq number (ash number -7)
             width (the (integer 1 5) (1+ width)))
       (go next)
     done)
    width))
(defun wasm-s32 (number)
  (declare (type (signed-byte 32) number))
  (loop
    (let ((byte (logand number 127)))
      (declare (type (unsigned-byte 7) byte))
      (setq number (ash number -7))
      (let ((done (or (and (= number 0) (< byte 64)) (and (= number -1) (>= byte 64)))))
        (wasm-byte (logior byte (if done 0 128))) (when done (return))))))
(defun wasm-name (name)
  (let ((bytes (utf8-bytes name))) (wasm-u32 (length bytes))
    (wasm-append bytes)))
(defun wasm-type (type)
  (ecase type (:object (wasm-byte #x6f)) (:i32 (wasm-byte #x7f))
    (:i64 (wasm-byte #x7e)) (:i31 (wasm-byte #x6c)) (:function (wasm-byte #x70))))
(defun wasm-section (kind bytes)
  (wasm-byte kind) (wasm-u32 (length bytes)) (wasm-append bytes))
(defmacro with-wasm-bytes (&body body)
  (list* 'let (list (list '*wasm-bytes*
    '(make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (append body (list '*wasm-bytes*))))
(defun wasm-call (name)
  (declare (type string name) (type list *native-imports*))
  ;; Imports keep their order throughout body emission. Rebuild when a caller
  ;; supplies another ABI list, including after reachability compacts it.
  (unless (and *native-import-indices* (eq (car *native-import-indices*) *native-imports*))
    (let ((indices (make-hash-table :test 'equal)))
      (loop for import in *native-imports* for index from 0 do
        (unless (gethash (first import) indices)
          (setf (gethash (first import) indices) index)))
      (setq *native-import-indices* (cons *native-imports* indices))))
  ;; Counts guide import ordering and shared-helper fusion. Reachability is
  ;; derived from decoded bodies rather than a separately retained use list.
  (when *wasm-import-counts*
    (incf (gethash name *wasm-import-counts* 0)))
  (let ((index (gethash name (cdr *native-import-indices*))))
    (unless index (error "Unknown native ABI operation ~S." name))
    (wasm-byte #x10) (wasm-u32 index)))
(defun native-signature-stack-effect (signature)
  ;; The kernel's compact effect keeps its scalar ABI. Multi-result signatures
  ;; use the general pair, including mixed reference/integer result tuples.
  (let* ((result (second signature)) (count (if (listp result) (length result) 1))
         (arguments (length (first signature))))
    (if (<= count 1) (+ (* 2 arguments) count) (cons arguments count))))

(defun wasm-remember-local (index start kind)
  (declare (type (unsigned-byte 32) index start) (type (unsigned-byte 8) kind))
  ;; A function owns one mutable lookbehind record. Recording each local
  ;; access used to allocate a fresh five-cons list in this emission loop.
  (let ((previous (or *wasm-last-local* (setq *wasm-last-local* (make-array 5)))))
    (declare (type simple-vector previous))
    (setf (svref previous 0) *wasm-bytes* (svref previous 1) (length *wasm-bytes*)
          (svref previous 2) index (svref previous 3) start (svref previous 4) kind)))

(defun emit-native-integer-box (value)
  (declare (type simple-vector value) (ignore value))
  ;; A mathematical result range does not recover the tier of raw arithmetic
  ;; or a private cell snapshot. Materialization preserves the i64 tier.
  (wasm-call "native-int64"))

(defun wasm-get (value)
  (declare (type simple-vector value *wasm-locals*) (type (or null simple-vector) *wasm-last-local*))
  (let ((captured (and *wasm-captured-result-locals*
                       (native-value-slot *wasm-captured-result-locals* value))))
    (when captured
      (emit-native-captured-list captured)
      (return-from wasm-get nil)))
  (let ((locals (and *wasm-multiple-result-locals*
                     (native-value-slot *wasm-multiple-result-locals* value))))
    (when locals
      ;; A packet is needed only at an ordinary full-value boundary.
      (dolist (local locals) (wasm-local local))
      (wasm-null)
      (dotimes (i (length locals)) (wasm-call "native-cons"))
      (return-from wasm-get nil)))
  (when *wasm-reused-values*
    (setq value (or (native-value-slot *wasm-reused-values* value) value)))
  (let ((tiered (and *wasm-tiered-values* (native-value-slot *wasm-tiered-values* value))))
    (when tiered (emit-native-tiered-materialize tiered) (return-from wasm-get nil)))
  (let ((raw (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* value))))
    (when (and raw (eq (svref raw 4) :character-result))
      ;; Only the checked producer, copies and CHAR-CODE own this recipe.
      (throw 'native-multiple-result-fallback :retry-sealed-scalar))
    (when (and raw (eq (svref raw 4) :result))
      (when (eq (svref raw 1) *wasm-private-scalar-ingress*)
        (throw 'native-multiple-result-fallback :retry-sealed-argument))
      (if (> (svref raw 5) 1)
          (let ((*wasm-unboxed-values* nil))
            ;; Boxed aliases keep their own ordinary local lifetimes. The
            ;; producer's reference slot may have been reused after a COPY.
            (wasm-get value))
          (progn (compiler-statistic :optimization/unboxed-result-box)
                 (wasm-local (svref raw 0)) (emit-native-raw-result-box raw)))
      (return-from wasm-get nil)))
  (cond
    ((and *wasm-inline-constants* (native-value-slot *wasm-inline-constants* value))
     (wasm-literal (first (hir-instruction-attributes (hir-value-producer value)))))
    ((and *wasm-last-local* (eq (svref *wasm-last-local* 0) *wasm-bytes*)
          (= (svref *wasm-last-local* 1) (length *wasm-bytes*))
          (= (svref *wasm-last-local* 4) #x21)
          (eql (svref *wasm-last-local* 2) (native-value-slot *wasm-locals* value)))
     ;; LOCAL.SET followed immediately by LOCAL.GET is one LOCAL.TEE.
     (setf (aref *wasm-bytes* (svref *wasm-last-local* 3)) #x22
           (svref *wasm-last-local* 4) 0))
    (t (wasm-local (or (native-value-slot *wasm-locals* value) (error "Missing native operand local."))))))
(defun wasm-set (value)
  (declare (type (or null simple-vector) value *wasm-last-local*) (type simple-vector *wasm-locals*))
  (let ((captured (and *wasm-captured-result-locals*
                       (native-value-slot *wasm-captured-result-locals* value))))
    (when captured
      (dolist (local (reverse captured)) (wasm-byte #x21) (wasm-u32 local))
      (return-from wasm-set nil)))
  (let ((locals (and *wasm-multiple-result-locals*
                     (native-value-slot *wasm-multiple-result-locals* value))))
    (when locals
      (dolist (local (reverse locals)) (wasm-byte #x21) (wasm-u32 local))
      (return-from wasm-set nil)))
  (let ((local (native-value-slot *wasm-locals* value)))
    (if local
        (if (and *wasm-last-local* (eq (svref *wasm-last-local* 0) *wasm-bytes*)
                 (= (svref *wasm-last-local* 1) (length *wasm-bytes*))
                 (= (svref *wasm-last-local* 4) #x20) (= (svref *wasm-last-local* 2) local))
            ;; A value's last use can share the result's local. Copying that
            ;; local to itself has no effect on either the stack or the local.
            (setf (fill-pointer *wasm-bytes*) (svref *wasm-last-local* 3)
                  (svref *wasm-last-local* 4) 0)
            (let ((start (length *wasm-bytes*)))
              (wasm-byte #x21) (wasm-u32 local)
              (wasm-remember-local local start #x21)))
        (wasm-byte #x1a))))

(defun emit-native-restore (environment)
  (declare (type simple-vector environment))
  ;; Keep the first checked restore, including its possible trap. Only an
  ;; adjacent restore of the same unchanged raw local can repeat its writes.
  ;; The local lookbehind also excludes constants, boxing and SET-to-TEE edits.
  (let ((start (length *wasm-bytes*)))
    (declare (type (unsigned-byte 32) start))
    (wasm-get environment)
    (let* ((local *wasm-last-local*)
           (exact (and local (eq (svref local 0) *wasm-bytes*)
                       (= (svref local 1) (length *wasm-bytes*))
                       (= (svref local 3) start) (= (svref local 4) #x20)))
           (previous *wasm-last-restore*))
      (declare (type (or null simple-vector) local previous))
      (when (and exact previous (eq (svref previous 0) *wasm-bytes*)
                 (= (svref previous 1) start) (= (svref previous 2) (svref local 2)))
        (setf (fill-pointer *wasm-bytes*) start)
        (setf (svref local 4) 0)
        (return-from emit-native-restore nil))
      (wasm-call "native-restore-environment")
      (if exact
          (let ((record (or previous (make-array 3))))
            (setf (svref record 0) *wasm-bytes* (svref record 1) (length *wasm-bytes*)
                  (svref record 2) (svref local 2))
            (setq *wasm-last-restore* record))
          (setq *wasm-last-restore* nil)))))

(defun wasm-integer (value)
  (declare (type (signed-byte 32) value))
  (wasm-byte #x41) (wasm-s32 value))
(defun wasm-local (index)
  (declare (type (unsigned-byte 32) index))
  (let ((start (length *wasm-bytes*)))
    (wasm-byte #x20) (wasm-u32 index)
    (wasm-remember-local index start #x20)))
(defun wasm-null () (wasm-byte #xd0) (wasm-byte #x6f))
(defun wasm-gc (opcode &rest immediates)
  (declare (type (unsigned-byte 32) opcode) (type list immediates))
  (wasm-byte #xfb) (wasm-u32 opcode) (dolist (immediate immediates) (wasm-u32 immediate)))
(defun wasm-as-object (value)
  (declare (type simple-vector value))
  (wasm-get value) (wasm-gc #x1a) (wasm-gc #x17 1))

(defun emit-native-tag-test (value tag &optional invariant nullp compactp boxedp)
  (declare (type simple-vector value))
  (when (and compactp (= tag 7))
    (if invariant (emit-native-invariant-value value) (wasm-get value))
    ;; Select the consumer's Boolean representation without a round trip.
    (wasm-call (if boxedp "native-consp-inline" "native-consp-test"))
    (return-from emit-native-tag-test nil))
  (setq *wasm-object-types* t)
  (if invariant (emit-native-invariant-value value) (wasm-get value))
  (wasm-gc #x1a) (wasm-gc #x14 1)
  (wasm-byte #x04) (wasm-type :i32)
    (if invariant (emit-native-invariant-value value) (wasm-get value))
    (wasm-gc #x1a) (wasm-gc #x16 1) (wasm-gc 2 1 0)
    (wasm-integer 63) (wasm-byte #x71) (wasm-integer tag) (wasm-byte #x46)
  (wasm-byte #x05)
    (if nullp
        (progn (if invariant (emit-native-invariant-value value) (wasm-get value))
               (wasm-byte #xd1))
        (wasm-integer 0))
  (wasm-byte #x0b)
  (when boxedp (wasm-call "native-boolean")))

(defun native-canonical-builtin-code (name)
  ;; Match the function object's N field, independently of its current binding.
  ;; Ordinary builtins use ID+1; SETF functions use -ID-1. Zero denotes a closure.
  (multiple-value-bind (code native) (compiler-primitive 37 name)
    (if native code
        (if (fboundp 'miricl::runtime-symbol-id)
            (if (consp name) (- -1 (miricl::runtime-symbol-id (second name)))
                (1+ (miricl::runtime-symbol-id name))) 0))))

(defun native-standard-function-name-p (name)
  ;; Method-local names have no fixed global entry. SETF place metadata alone
  ;; likewise does not establish a standardized SETF function definition.
  (and (standard-function-name-p name)
       (not (eq (standard-function-property name :kind) :local-function))))

(defun native-standard-operation (value)
  ;; Semantic provenance never permits removing the computed designator or
  ;; expanding its call. Its producer and ordinary value consumers stay live.
  (or (and *hir-standard-operations* (gethash value *hir-standard-operations*))
      (hir-standard-operation-leaf value)))

(defun native-search-list-code (name inputs)
  "Select a shared checked EQ/EQL search from statically known controls."
  (unless (and (member name '(member assoc)) (>= (length inputs) 4)
               (evenp (length inputs)))
    (return-from native-search-list-code nil))
  (labels ((test-name (value)
             (or (native-standard-operation value)
                 (multiple-value-bind (literal known) (hir-primary-constant value)
                   (and known (member literal '(eq eql)) literal)))))
    (let ((test 'eql) (seen nil) (negate nil))
      (loop for (key value) on (nthcdr 4 inputs) by #'cddr do
        (multiple-value-bind (keyword known) (hir-primary-constant key)
          (unless (and known (member keyword '(:key :test :test-not :allow-other-keys)))
            (return-from native-search-list-code nil))
          (unless (member keyword seen)
            (push keyword seen)
            (case keyword
              (:key
               (multiple-value-bind (key known) (hir-primary-constant value)
                 (unless (and known (null key)) (return-from native-search-list-code nil))))
              ((:test :test-not)
               (setq test (test-name value) negate (eq keyword :test-not))
               (unless (member test '(eq eql)) (return-from native-search-list-code nil)))))))
      ;; Supplying both is an error even when either value is NIL.
      (unless (and (member :test seen) (member :test-not seen))
        (+ (if (eq name 'assoc) 2 0) (if (eq test 'eql) 1 0) (if negate 4 0))))))

(defun native-list-path-code (name)
  ;; The semantic contract supplies steps in evaluation order. Its bounded
  ;; paths become the shared runtime worker's low-bit-first encoding.
  (let ((code 0) (bit 1))
    (dolist (step (standard-function-list-path name))
      (when (eq step :cdr) (incf code bit))
      (setq bit (* bit 2)))
    (+ code bit)))

(defun native-standard-effect-free-p (name arity)
  ;; All four negative facts and the accepted arity must be explicit. ANSI
  ;; inventory membership by itself supplies none of these guarantees.
  (let ((limits (standard-function-property name :arity)))
    (and limits (>= arity (car limits))
         (or (null (cdr limits)) (<= arity (cdr limits)))
         (every (lambda (property)
                  (multiple-value-bind (value known) (standard-function-property name property)
                    (and known (null value))))
                '(:reads :writes :callbacks :may-signal)))))

(defun emit-native-object-types ()
  ;; Match runtime-objects.lisp's two minimal canonical type groups exactly.
  ;; DIGITS is an independent i32 array. OBJ is a nonfinal recursive struct.
  (dolist (byte '(#x5e #x7f 1
                 #x4e 1 #x50 0 #x5f 7
                 #x7f 0 #x7f 1 #x7c 0 #x63 0 0
                 #x6d 1 #x6d 1 #x6d 1))
    (wasm-byte byte)))
(defun wasm-literal (value)
  (cond ((null value) (wasm-null))
    ((and (integerp value) (<= -1073741824 value 1073741823))
     ;; Fresh literals retain immediate storage independently of the presence
     ;; of the computed-i64 boxing import.
     (wasm-integer value) (wasm-gc #x1c) (wasm-gc #x1b))
    ((and *wasm-literal-locals* (gethash value *wasm-literal-locals*))
     (wasm-local (gethash value *wasm-literal-locals*)))
    ((eq value t) (wasm-integer 1) (wasm-call "native-boolean"))
    (t
      (let ((index (multiple-value-bind (index found) (gethash value *wasm-literal-index*)
                     (if found index (setf (gethash value *wasm-literal-index*)
                                      (vector-push-extend value *wasm-literals*))))))
        (wasm-local 0) (wasm-integer index) (wasm-call "native-literal")))))
(defun wasm-list (values tailp)
  (declare (type list values))
  (dolist (value values) (wasm-get value))
  ;; Operands are already evaluated left to right. Build the same fresh
  ;; spine from its tail, sharing pairs of allocations across call packets,
  ;; value groups and canonical inline LIST/LIST* calls. LIST* leaves its
  ;; final operand as the shared tail instead of adding an empty tail.
  (let ((count (length values)))
    (cond (tailp (decf count))
          ((>= count 2) (wasm-call "native-list2") (decf count 2))
          (t (wasm-null)))
    (dotimes (i (ash count -1)) (wasm-call "native-cons2"))
    (when (oddp count) (wasm-call "native-cons"))))

(defun wasm-arguments (values &optional usage)
  ;; All operand effects have already run in HIR order. Build one owned raw
  ;; array, filling its slots before publishing it to any application adapter.
  ;; Setters return the packet, so construction needs no extra local or GC
  ;; opcode/type declaration in the emitted module.
  (declare (type list values))
  (wasm-integer (length values)) (wasm-call "native-arguments")
  (loop for value in values for index from 0 do
    (wasm-integer index)
    (if (or (null usage) (logbitp index usage)) (wasm-get value) (wasm-null))
    (wasm-call "native-argument-set")))

(defun native-call-usage (inputs)
  ;; Cached before final liveness; never recursively plan a callee here.
  (let ((target (and *wasm-argument-usages* (native-known-callee (second inputs)))))
    (when (and target (gethash target *wasm-direct-workers*)
               (eql (length (cddr inputs)) (native-fixed-arity target)))
      (gethash target *wasm-argument-usages*))))

(defun emit-native-call-operands (inputs)
  (wasm-get (second inputs))
  (let ((usage (native-call-usage inputs)))
    (loop for input in (cddr inputs) for index from 0 do
      (if (or (null usage) (logbitp index usage)) (wasm-get input) (wasm-null)))))

(defun native-removable-p (instruction)
  (declare (type simple-vector instruction))
  (when (and *wasm-checked-integer-copies*
             (gethash (hir-instruction-output instruction) *wasm-checked-integer-copies*))
    (return-from native-removable-p nil))
  ;; These arithmetic plans retain their original cooperative checkpoint,
  ;; even when no consumer observes the arithmetic result.
  (when (and *wasm-proven-integer-arithmetic*
             (let ((proof (gethash instruction *wasm-proven-integer-arithmetic*)))
               (and proof (svref proof 3))))
    (return-from native-removable-p nil))
  (when (and *wasm-inline-simple-calls*
             (eq (gethash instruction *wasm-inline-simple-calls*) :endp-list))
    (return-from native-removable-p nil))
  ;; The compound LDB performs the covered BYTE and arithmetic effects. The other
  ;; cases below can disappear when unused without transferring effects.
  (or (and *wasm-low-field*
           (or (eq instruction (second *wasm-low-field*))
               (eq instruction (third *wasm-low-field*))))
      (and *wasm-proper-list-checks* (gethash instruction *wasm-proper-list-checks*))
      (and *wasm-reused-values*
           ;; A forwarded checked field access is nontrapping: its operand
           ;; was proved to be a fresh cons. Its now-unused result is dead too.
           (native-value-slot *wasm-reused-values* (hir-instruction-output instruction)))
      (and *wasm-inline-references*
           (gethash (hir-instruction-output instruction) *wasm-inline-references*))
      ;; These plans identify the canonical captured implementation, not a
      ;; possibly redefined global call. Their valid operands cannot signal.
      ;; Allocation is removable when its fresh result is unobserved, but is
      ;; not interchangeable with an existing allocation or safe to hoist.
      (and *wasm-inline-simple-calls* (eq (hir-instruction-opcode instruction) :call)
           (let ((kind (gethash instruction *wasm-inline-simple-calls*)))
             (or (integerp kind)
                 (and kind (not (eq kind :standard-call))
                      (native-standard-effect-free-p (hir-inline-primitive instruction)
                        (- (length (hir-instruction-inputs instruction)) 2)))
                 (member kind '(:true :false
                                :left :right :cons :list :list-star :car :cdr :car-cons :cdr-cons :vector-length :evenp :oddp)))))
      (member (hir-instruction-opcode instruction)
        ;; The checked entry validates arguments independently. An unused
        ;; parameter read or closure allocation has no additional observable
        ;; effect; initializer calls and global lookups remain live.
        '(:constant :copy :primary :truth :value-ref :values :list :capture
          :argument :argument-present :keyword-present :keyword-argument :rest-argument :dynamic-rest-argument
          :closure :cell-new :cell-read :block-enter :block-environment))))

(defmacro native-low-field-inputs (instruction)
  (list 'and '*wasm-low-field* (list 'eq instruction '(first *wasm-low-field*))
        '(nthcdr 6 *wasm-low-field*)))

(defun native-live-inputs (instruction)
  (declare (type simple-vector instruction))
  (let ((inputs (native-low-field-inputs instruction)))
    (when inputs (return-from native-live-inputs inputs)))
  (let ((inputs (hir-instruction-inputs instruction))
        (test (and (eq (hir-instruction-opcode instruction) :branch)
                   (native-active-branch-test instruction)))
        (reused (and *wasm-reused-values*
                     (native-value-slot *wasm-reused-values* (hir-instruction-output instruction))))
        (hoisted (and *wasm-hoisted-values*
                      (native-value-slot *wasm-hoisted-values* (hir-instruction-output instruction)))))
    (when (and *wasm-closed-worker-functions*
               (eq (hir-instruction-opcode instruction) :closure)
               (gethash (first (hir-instruction-attributes instruction)) *wasm-closed-worker-functions*))
      (return-from native-live-inputs nil))
    (when reused (return-from native-live-inputs (list reused)))
    (when hoisted (return-from native-live-inputs (svref hoisted 5)))
    (when (and (eq (hir-instruction-opcode instruction) :values)
               (native-value-slot *wasm-scalar-groups* (hir-instruction-output instruction)))
      (return-from native-live-inputs (and inputs (list (first inputs)))))
    (when (eq (hir-instruction-opcode instruction) :call)
      (let ((usage (native-call-usage inputs)))
        (when usage
          (return-from native-live-inputs
            (append (if (or (native-value-slot *wasm-single-calls* (hir-instruction-output instruction))
                            (member instruction *wasm-tail-calls* :test #'eq))
                        (list (second inputs)) (list (first inputs) (second inputs)))
              (loop for input in (cddr inputs) for index from 0
                    when (logbitp index usage) collect input))))))
    ;; An uncaptured lexical exit lowers to an ordinary edge. Only its value
    ;; crosses that edge; the interpreter's environment and token are unused.
    (when (and (eq (hir-instruction-opcode instruction) :block-return)
               (hir-instruction-attributes instruction))
      (return-from native-live-inputs (list (third inputs))))
    (when test
      (return-from native-live-inputs
        (if (second test)
            (let ((record (and *wasm-hoisted-values*
                              (native-value-slot *wasm-hoisted-values* (hir-instruction-output (second test))))))
              (if (and record (svref record 6)) (svref record 5)
                  (cddr (hir-instruction-inputs (second test)))))
            (list (first test)))))
    (let ((kind (and *wasm-inline-simple-calls* (gethash instruction *wasm-inline-simple-calls*))))
      (if (or (and *wasm-inline-vector-calls* (gethash instruction *wasm-inline-vector-calls*)) kind)
          ;; These primitives use neither the function lookup nor the hidden
          ;; dynamic environment. Their explicit operands retain evaluation order.
          ;; A singleton shift count is encoded in the instruction. Dropping
          ;; its value does not drop effectful or potentially signaling producers.
          (if (and (member kind '(#x86 #x87))
                   (multiple-value-bind (low high known)
                       (hir-integer-bounds
                         (if *wasm-inferred-types* (gethash (fourth inputs) *wasm-inferred-types* t)
                             (hir-value-type (fourth inputs))))
                     (and known (= low high))))
              (list (third inputs)) (cddr inputs))
          ;; Scalar runtime calls use the active dynamic state directly. Their
          ;; emitter does not read the HIR environment operand, so it must not
          ;; keep an otherwise unused entry snapshot alive.
          (if (and (eq (hir-instruction-opcode instruction) :call)
                   (or (native-value-slot *wasm-single-calls* (hir-instruction-output instruction))
                       (member instruction *wasm-tail-calls* :test #'eq)))
              (cdr inputs) inputs)))))

(defun native-direct-cells (function)
  (declare (type simple-vector function))
  ;; Uncaptured bindings use mutable Wasm locals. Unassigned captured
  ;; bindings carry their value directly through closure environments, since
  ;; the private binding cell has no observable identity. Mutation of the
  ;; value itself remains visible through the same object reference.
  (let ((cells (native-value-table function)))
    (dolist (binding (hir-function-bindings function))
      (when (and (hir-binding-cell binding)
                 (or (not (hir-binding-captured-p binding))
                     (not (hir-binding-assigned-p binding))))
        (setf (native-value-slot cells (hir-binding-cell binding)) t)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (%hir-instruction-p producer)
                   (eq (hir-instruction-opcode producer) :capture))
          (let ((binding (second (hir-instruction-attributes producer))))
            (when (and (%hir-binding-p binding) (not (hir-binding-assigned-p binding)))
              (setf (native-value-slot cells value) t))))))
    cells))

(defun native-forward-blocks (function)
  (declare (type simple-vector function))
  ;; Share the HIR solver with constant propagation. Edge multiplicity and
  ;; the reachable-only order are identical to the original backend pass.
  (hir-forward-blocks function))

(defun native-reachable-blocks (function targets)
  (declare (type simple-vector function) (type list targets))
  (let ((seen (make-array (hir-function-block-count function) :initial-element nil))
        (work (cons (hir-function-entry function) (mapcar (lambda (target) (hir-target-block target)) targets))))
    (declare (type simple-vector seen) (type list work))
    (loop while work do
      (let* ((block (pop work)) (id (hir-block-id block)))
        (declare (type (unsigned-byte 32) id))
        (unless (svref seen id)
          (setf (svref seen id) t)
          (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
            (push successor work)))))
    (loop for block in (hir-function-blocks function)
          when (svref seen (the (unsigned-byte 32) (hir-block-id block))) collect block)))

(defun native-loop-region-order (function blocks region regions)
  (declare (type simple-vector function) (type list blocks regions)
           (type (or null simple-vector) region))
  ;; Collapse immediate child loops. Ignore this region's return edges and
  ;; exits; its parent's ordering accounts for those exits. Remaining cycles
  ;; reject the whole structured layout.
  (let* ((members (and region (svref region 1))) (header (and region (svref region 0)))
         (nodes (make-array (hir-function-block-count function) :initial-element nil))
         (edges (make-array (hir-function-block-count function) :initial-element nil))
         (incoming (make-array (hir-function-block-count function) :initial-element 0))
         (entry (or header (hir-function-entry function))) (count 0) (order nil) (work nil))
    (declare (type simple-vector nodes edges incoming entry) (type list order work)
             (type (unsigned-byte 32) count))
    (dolist (child regions)
      (when (eq (svref child 2) region)
        (dolist (block blocks)
          (let ((id (hir-block-id block)))
            (declare (type (unsigned-byte 32) id))
            (when (svref (the simple-vector (svref child 1)) id)
              (setf (svref nodes id) (svref child 0)))))))
    (dolist (block blocks)
      (let ((id (hir-block-id block)))
        (declare (type (unsigned-byte 32) id))
        (when (or (null members) (svref (the simple-vector members) id))
          (let ((from (or (svref nodes id) block)))
            (when (eq from block) (incf count))
            (dolist (target (hir-instruction-attributes (hir-block-terminator block)))
              (let* ((next (hir-block-id target)) (to (or (svref nodes next) target)))
                (declare (type (unsigned-byte 32) next))
                (when (and (or (null members) (svref (the simple-vector members) next))
                           (not (eq target header))
                           (not (and (svref nodes id) (eq from to))))
                  (push to (svref edges (the (unsigned-byte 32) (hir-block-id from))))
                  (incf (svref incoming (the (unsigned-byte 32) (hir-block-id to)))))))))))
    (when (and (or (null (svref nodes (the (unsigned-byte 32) (hir-block-id entry))))
                   (eq entry (svref nodes (the (unsigned-byte 32) (hir-block-id entry)))))
               (zerop (the (unsigned-byte 33) (svref incoming (the (unsigned-byte 32) (hir-block-id entry))))))
      (push entry work))
    (loop while work do
      (let ((block (pop work)))
        (push block order)
        (dolist (target (svref edges (the (unsigned-byte 32) (hir-block-id block))))
          (let* ((id (hir-block-id target)) (n (svref incoming id)))
            (declare (type (unsigned-byte 32) id) (type (unsigned-byte 33) n))
            (let ((next (1- n)))
              (declare (type (unsigned-byte 33) next))
              (setf (svref incoming id) next) (when (zerop next) (push target work)))))))
    (when (= count (length order)) (nreverse order))))

(defun native-loop-plan (function blocks)
  (declare (type simple-vector function) (type list blocks))
  ;; Natural loops must form a proper nesting forest. Numeric backward edges
  ;; also include ordinary exits, so each latch needs a dominance proof.
  (when (or (>= (hir-function-debug-level function) 2)
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*))
            (> (hir-function-block-count function) 16384))
    (return-from native-loop-plan nil))
  (let ((alive (make-array (hir-function-block-count function) :initial-element nil))
        (seen (make-array (hir-function-block-count function) :initial-element 0))
        (candidates (make-array (hir-function-block-count function) :initial-element nil))
        (work nil) (backward nil) (targets nil) (groups nil) (regions nil) (epoch 0) (fuel 65536))
    (declare (type simple-vector alive seen candidates) (type list work backward targets groups regions)
             (type (unsigned-byte 32) epoch) (type (integer -1 65536) fuel))
    (dolist (block blocks)
      (setf (svref alive (the (unsigned-byte 32) (hir-block-id block))) t)
      (dolist (target (hir-instruction-attributes (hir-block-terminator block)))
        (when (<= (hir-block-id target) (hir-block-id block))
          (push (cons block target) backward)
          (unless (svref candidates (the (unsigned-byte 32) (hir-block-id target)))
            (setf (svref candidates (the (unsigned-byte 32) (hir-block-id target))) t) (push target targets)))))
    (dolist (candidate targets)
      (incf epoch) (setq work (list (hir-function-entry function)))
      (loop while work do
        (when (minusp (decf fuel)) (return-from native-loop-plan nil))
        (let* ((block (pop work)) (id (hir-block-id block)))
          (declare (type simple-vector block) (type (unsigned-byte 32) id))
          (unless (or (eq block candidate) (= (the (unsigned-byte 32) (svref seen id)) epoch))
            (setf (svref seen id) epoch)
            (dolist (target (hir-instruction-attributes (hir-block-terminator block))) (push target work)))))
      (let ((latches nil))
        (dolist (edge backward)
          (when (minusp (decf fuel)) (return-from native-loop-plan nil))
          (when (and (eq (cdr edge) candidate)
                     (/= (the (unsigned-byte 32) (svref seen (the (unsigned-byte 32) (hir-block-id (car edge))))) epoch))
            (push (car edge) latches)))
        (when latches (push (cons candidate latches) groups))))
    ;; Bound region storage, pair comparisons, ordering work and recursion.
    (when (or (null groups) (> (length groups) 16)
              (> (* (1+ (length groups)) (length blocks)) 32768))
      (return-from native-loop-plan nil))
    (dolist (group groups)
      (let ((header (car group))
            (members (make-array (hir-function-block-count function) :initial-element nil)))
        (declare (type simple-vector members header))
        (setq work (cdr group))
        (loop while work do
          (when (minusp (decf fuel)) (return-from native-loop-plan nil))
          (let* ((block (pop work)) (id (hir-block-id block)))
            (declare (type simple-vector block) (type (unsigned-byte 32) id))
            (unless (svref members id)
              (setf (svref members id) t)
              (unless (eq block header)
                (dolist (previous (hir-block-predecessors block))
                  (when (svref alive (the (unsigned-byte 32) (hir-block-id previous))) (push previous work)))))))
        ;; Header, membership, parent, ordered immediate body nodes.
        (push (vector header members nil nil) regions)))
    (dolist (region regions)
      (dolist (other regions)
        (unless (eq region other)
          (let ((overlap nil) (only-region nil) (only-other nil))
            (dolist (block blocks)
              (when (minusp (decf fuel)) (return-from native-loop-plan nil))
              (let* ((id (hir-block-id block))
                     (left (svref (the simple-vector (svref region 1)) id))
                     (right (svref (the simple-vector (svref other 1)) id)))
                (declare (type (unsigned-byte 32) id))
                (cond ((and left right) (setq overlap t))
                      (left (setq only-region t)) (right (setq only-other t)))))
            (when overlap
              (when (or (and only-region only-other) (not (or only-region only-other)))
                (return-from native-loop-plan nil))
              (cond
                ((not only-region)
                 (when (svref (the simple-vector (svref region 1))
                              (the (unsigned-byte 32) (hir-block-id (svref other 0))))
                   (return-from native-loop-plan nil))
                 (let ((parent (svref region 2)))
                   (when (or (null parent)
                             (svref (the simple-vector (svref parent 1))
                                    (the (unsigned-byte 32) (hir-block-id (svref other 0)))))
                     (setf (svref region 2) other))))
                ((svref (the simple-vector (svref other 1))
                        (the (unsigned-byte 32) (hir-block-id (svref region 0))))
                 (return-from native-loop-plan nil))))))))
    (let ((outer (native-loop-region-order function blocks nil regions))
          (by-header (make-array (hir-function-block-count function) :initial-element nil)))
      (declare (type simple-vector by-header))
      (unless outer (return-from native-loop-plan nil))
      (dolist (region regions)
        (let ((order (native-loop-region-order function blocks region regions)))
          (unless order (return-from native-loop-plan nil))
          (setf (svref region 3) order
                (svref by-header (the (unsigned-byte 32) (hir-block-id (svref region 0)))) region)))
      ;; Emission needs only headers and ordered bodies. Do not retain proof
      ;; bitsets or parent links in the module's cached function plans.
      (dolist (region regions) (setf (svref region 1) nil (svref region 2) nil))
      (cons outer by-header))))

(defun native-live-values (function &optional dynamic-bindings blocks)
  (declare (type simple-vector function) (type list blocks))
  (let ((live (native-value-table function)) (work nil))
    ;; Mark when scheduling, so shared inputs enter the worklist only once.
    ;; The dependency query uses fixed lowering plans, not this live table.
    (dolist (block (or blocks (hir-function-blocks function)))
      (let ((environment (and dynamic-bindings (hir-block-environment block))))
        (when (and environment (not (native-value-slot live environment)))
          (setf (native-value-slot live environment) t) (push environment work)))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (unless (native-removable-p instruction)
          (dolist (input (native-live-inputs instruction))
            (unless (native-value-slot live input)
              (setf (native-value-slot live input) t) (push input work))))))
    (loop while work do
      (let ((producer (hir-value-producer (pop work))))
        (when (%hir-instruction-p producer)
          (dolist (input (native-live-inputs producer))
            (unless (native-value-slot live input)
              (setf (native-value-slot live input) t) (push input work))))))
    live))

(defun native-local-layout (function blocks offset)
  (declare (type simple-vector function) (type list blocks) (type (unsigned-byte 32) offset))
  ;; Reuse temporary locals whose entire lifetime is inside one basic block.
  ;; Bindings, dynamic scopes, block arguments and cross-block values keep
  ;; dedicated locals, so loops and nonlocal exits need no interval heuristics.
  ;; Compounded LDB reads the original arithmetic operands at its own position.
  (let ((owners (native-value-table function)) (last-use (native-value-table function))
        (locals (native-value-table function)) (aliases (native-value-table function)) (count 0))
    (declare (type simple-vector owners last-use locals aliases) (type (unsigned-byte 32) count))
    (dolist (block blocks)
      (let ((position 0))
        (declare (type (unsigned-byte 32) position))
        (dolist (instruction (append (hir-block-instructions block) (list (hir-block-terminator block))))
          (let* ((reused (and *wasm-reused-values*
                              (native-value-slot *wasm-reused-values* (hir-instruction-output instruction))))
                 (inputs (unless reused
                           (or (native-low-field-inputs instruction)
                               (hir-instruction-inputs instruction)))))
          (dolist (original (cons (unless reused (hir-instruction-output instruction)) inputs))
            (let ((value (if (and original *wasm-reused-values*)
                             (or (native-value-slot *wasm-reused-values* original) original) original)))
            (when value
              (let ((owner (native-value-slot owners value)))
                (setf (native-value-slot owners value) (if (or (null owner) (eq owner block)) block :shared))))))
          (dolist (input inputs)
            (let ((value (if *wasm-reused-values* (or (native-value-slot *wasm-reused-values* input) input) input)))
              (setf (native-value-slot last-use value) position))))
          (incf position))))
    (dolist (value (hir-function-values function))
      (when (and (native-value-slot *wasm-live-values* value) (not (native-value-slot *wasm-inline-constants* value))
                 (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value))))
        (let* ((producer (hir-value-producer value))
               (alias (and (%hir-instruction-p producer)
                        (member (hir-instruction-opcode producer) '(:block-enter :block-environment))
                        (not (hir-target-captured-p (first (hir-instruction-attributes producer))))
                        (first (hir-instruction-inputs producer)))))
          (cond (alias (setf (native-value-slot aliases value) alias))
                ((or (member (hir-value-kind value) '(:dynamic :cell)) (native-value-slot *wasm-local-cells* value) (%hir-block-p producer)
                     (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* value))
                     (eq value (hir-function-environment function))
                     (eq (native-value-slot owners value) :shared))
                 (setf (native-value-slot locals value) (+ offset count)) (incf count))))))
    (let ((pinned count))
      (declare (type (unsigned-byte 32) pinned))
      (dolist (block blocks)
        (let* ((instructions (append (hir-block-instructions block) (list (hir-block-terminator block))))
               (even (make-array (1+ (length instructions)) :initial-element nil))
               (odd (make-array (1+ (length instructions)) :initial-element nil))
               (free (loop for i from pinned below count collect (+ offset i))) (position 0))
          (declare (type simple-vector even odd) (type (unsigned-byte 32) position)
                   (type list instructions free))
          (dolist (instruction instructions)
            ;; The old active-list scan reversed survivors each instruction.
            ;; Birth-parity buckets reproduce its exact free-list order while
            ;; touching only locals that expire here, not all survivors.
            (setq free (if (evenp position)
                           (nconc (aref even position) (nreconc (aref odd position) free))
                           (nconc (aref odd position) (nreconc (aref even position) free))))
            (setf (aref even position) nil (aref odd position) nil)
            (let ((output (hir-instruction-output instruction)))
              (when (and output (native-value-slot *wasm-live-values* output)
                         (not (native-value-slot *wasm-inline-constants* output))
                         (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output)))
                         (not (native-value-slot locals output)) (not (native-value-slot aliases output)))
                (let ((index (if free (pop free) (prog1 (+ offset count) (incf count)))))
                  (setf (native-value-slot locals output) index)
                  ;; Inputs expire before this instruction stores its result.
                  ;; An unused result cannot expire before its own creation.
                  (push index (aref (if (evenp position) even odd)
                               (max (1+ position) (or (native-value-slot last-use output) position)))))))
            (incf position)))))
    (dolist (value (hir-function-values function))
      (let* ((alias (native-value-slot aliases value))
             (local (when alias (native-value-slot locals alias))))
        (when alias
          (if local
            (setf (native-value-slot locals value) local)
            (progn
              ;; HIR IDs need not follow definition order. Resolve the complete
              ;; environment-alias chain before assigning a local, then compress
              ;; that path so later resolutions do not repeat the traversal.
              (loop repeat (length aliases)
                    for next = (native-value-slot aliases alias)
                    while next do (setq alias next)
                    finally (when next (error "Circular native local aliases.")))
              (loop for current = value then next
                    for next = (native-value-slot aliases current)
                    while next do
                      (setf (native-value-slot aliases current) alias
                            (native-value-slot locals current) (native-value-slot locals alias))))))))
    (values locals count)))

(defun native-inline-constants (function blocks)
  (declare (type simple-vector function) (type list blocks))
  ;; Literal lookup is pure and always returns the same object. A single use
  ;; needs no temporary local; NIL is cheaper to recreate at any number of uses.
  (let ((uses (native-value-table function 0)) (constants (native-value-table function)))
    (dolist (block blocks)
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (dolist (input (hir-instruction-inputs instruction))
          ;; Direct cell writes can read their value twice in the emitter.
          (incf (native-value-slot uses input) (if (eq (hir-instruction-opcode instruction) :cell-write) 2 1)))))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :constant)
                   (or (null (first (hir-instruction-attributes instruction)))
                       (= (native-value-slot uses (hir-instruction-output instruction)) 1)))
          (setf (native-value-slot constants (hir-instruction-output instruction)) t))))
    constants))

(defun native-single-calls (function scalar)
  (declare (type simple-vector function scalar))
  ;; Multiple-value packets are only needed when a consumer observes the
  ;; group. Do not allocate a packet merely to extract its primary value.
  ;; Keep this representation choice local to emission: HIR remains reusable.
  (let ((calls (native-value-table function)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :call)
                   (<= (length (hir-instruction-inputs producer)) 6) (native-value-slot scalar value))
          (setf (native-value-slot calls value) t))))
    calls))

(defun native-active-branch-test (end &optional consumed)
  ;; Liveness and emission agree when the terminator evaluates its test.
  ;; Check exact consumed identity before spending a proof query per instruction.
  (let ((test (and end *wasm-branch-tests*
                   (eq (hir-instruction-opcode end) :branch)
                   (gethash end *wasm-branch-tests*))))
    (when (and test (or (null consumed) (member consumed (fifth test) :test #'eq))
               (not (eq (first (hir-instruction-attributes end))
                        (second (hir-instruction-attributes end))))
               (not (nth-value 1
                      (native-proven-truth (first (hir-instruction-inputs end))
                                           (hir-instruction-safety-level end)))))
      test)))

(defun native-plan-branch-tests (function blocks)
  (declare (type simple-vector function) (type list blocks))
  ;; The bounded suffix belongs to this block, and every erased result has
  ;; exactly one use. NOT/NULL are the canonical total :NULL inline plan.
  (when (or (>= (hir-function-debug-level function) 2)
            (not (some (lambda (block)
                         (eq (hir-instruction-opcode (hir-block-terminator block)) :branch)) blocks)))
    (return-from native-plan-branch-tests nil))
  (let ((uses (native-value-table function 0)) (tests (make-hash-table :test 'eq)))
    (declare (type simple-vector uses) (type hash-table tests))
    (dolist (block blocks)
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (dolist (input (hir-instruction-inputs instruction))
          (incf (native-value-slot uses input)))))
    (dolist (block blocks)
      (let ((end (hir-block-terminator block)))
        (when (and (eq (hir-instruction-opcode end) :branch)
                   (not (eq (first (hir-instruction-attributes end))
                            (second (hir-instruction-attributes end))))
                   (not (nth-value 1
                          (native-proven-truth (first (hir-instruction-inputs end))
                                               (hir-instruction-safety-level end)))))
          (let* ((original (first (hir-instruction-inputs end))) (value original)
                 (call nil) (kind nil) (inverted nil) (consumed nil) (fuel 32))
            (dolist (instruction (reverse (hir-block-instructions block)))
              (when (zerop fuel) (return))
              (decf fuel)
              (unless (and (eq (hir-instruction-output instruction) value)
                           (= (native-value-slot uses value) 1)) (return))
              (let ((opcode (hir-instruction-opcode instruction))
                    (inputs (hir-instruction-inputs instruction)))
                (cond
                  ((or (member opcode '(:copy :truth))
                       (and (eq opcode :primary)
                            (or (native-value-slot *wasm-scalar-groups* (first inputs))
                                (native-value-slot *wasm-single-calls* (first inputs)))))
                   (setq value (first inputs)))
                  ((and (eq opcode :call)
                        (eq (gethash instruction *wasm-inline-simple-calls*) :null)
                        (= (length inputs) 3))
                   (push instruction consumed)
                   (setq inverted (not inverted) value (third inputs)))
                  ((and (eq opcode :call)
                        (let ((plan (gethash instruction *wasm-inline-simple-calls*)))
                          (when (or (and (integerp plan) (< plan #x60))
                                    (member plan '(:true :false :evenp :oddp :eq :eql :consp :symbolp))
                                    ;; Do not move a potentially signaling compare
                                    ;; past the terminator's existing backedge poll.
                                    (and (member plan '(:less-real :greater-real :not-greater-real :not-less-real
                                                       :proven-less-int64 :proven-greater-int64
                                                       :proven-not-greater-int64 :proven-not-less-int64))
                                         (every (lambda (target)
                                                  (> (hir-block-id target) (hir-block-id block)))
                                                (hir-instruction-attributes end))))
                            (setq call instruction kind plan)
                            (push instruction consumed))))
                   (return))
                  (t (return)))))
            (when (or call (not (eq value original)))
              (setf (gethash end tests) (list value call kind inverted consumed)))))))
    tests))

(defun native-stable-predicate-p (kind)
  ;; These canonical plans inspect immutable values or object identity/tags.
  ;; Mutable contents, allocation, general calls and checked accesses are out.
  (or (and (integerp kind) (<= #x50 kind #x59))
      ;; Integer-only parity plans carry their own checked type proof.
      (member kind '(:evenp :oddp))
      (let* ((entry (assoc kind '((:eq eq 2) (:eql eql 2) (:null null 1)
                                  (:consp consp 1) (:atom atom 1) (:listp listp 1)
                                  (:functionp functionp 1) (:simple-vector-p simple-vector-p 1)
                                  (:vectorp vectorp 1) (:stringp stringp 1) (:symbolp symbolp 1)
                                  (:integerp integerp 1) (:characterp characterp 1))))
             (name (second entry)))
        (and entry (standard-function-property name :truth-result)
             (native-standard-effect-free-p name (third entry))))))

(defun native-predicate-source (value reused)
  (declare (type simple-vector value) (type (or null simple-vector) reused))
  (loop repeat 32 do
    (let* ((previous (and reused (native-value-slot reused value)))
           (producer (hir-value-producer value)))
      (cond (previous (setq value previous))
            ((and (%hir-instruction-p producer)
                  (or (eq (hir-instruction-opcode producer) :copy)
                      (and (eq (hir-instruction-opcode producer) :primary)
                           (native-value-slot *wasm-scalar-groups* (first (hir-instruction-inputs producer))))))
             (setq value (first (hir-instruction-inputs producer))))
            (t (return)))))
  value)

(defun native-predicate-key (scope instruction constants reused)
  (declare (type simple-vector instruction) (type hash-table constants))
  (when (eq (hir-instruction-opcode instruction) :capture)
    (return-from native-predicate-key (list* scope :capture (hir-instruction-attributes instruction))))
  (let ((key (list scope (gethash instruction *wasm-inline-simple-calls*))))
    (dolist (input (cddr (hir-instruction-inputs instruction)))
      (let* ((source (native-predicate-source input reused)) (producer (hir-value-producer source))
             (id (hir-value-id source)))
        (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
          ;; EQ keys preserve literal identity without traversing mutable or
          ;; cyclic quoted objects. Their canonical value ID is function-local.
          (let ((literal (first (hir-instruction-attributes producer))))
            (setq id (or (gethash literal constants) (setf (gethash literal constants) id)))))
        (push id key)))
    key))

(defun native-plan-cons-forwarding (function blocks)
  "Forward fields of fresh conses within a block, before any possible mutation.
Only canonical INLINE allocations and accesses participate. Unknown effects
discard the field facts; transparent SSA copies preserve their observations.
Liveness can then remove an allocation whose identity is never observed."
  (declare (type simple-vector function) (type list blocks))
  (unless (and *wasm-cons-forward-blocks* (plusp (hash-table-count *wasm-cons-forward-blocks*)))
    (return-from native-plan-cons-forwarding nil))
  (let ((reused nil) (fuel 4096))
    (declare (type (or null simple-vector) reused) (type (integer -1 4096) fuel))
    (dolist (block blocks)
      (when (gethash block *wasm-cons-forward-blocks*)
      (let ((origins nil))
        (declare (type (or null hash-table) origins))
        (dolist (instruction (hir-block-instructions block))
          (when (minusp (decf fuel)) (return-from native-plan-cons-forwarding reused))
          (let* ((opcode (hir-instruction-opcode instruction)) (inputs (hir-instruction-inputs instruction))
                 (output (hir-instruction-output instruction))
                 (kind (and (eq opcode :call) (gethash instruction *wasm-inline-simple-calls*)))
                 (origin (and origins
                           (gethash (if (eq opcode :call) (third inputs) (first inputs)) origins))))
            (cond
              ((eq kind :cons)
               (unless origins (setq origins (make-hash-table :test 'eq)))
               (setf (gethash output origins) (cddr inputs)))
              ((and origin (member opcode '(:primary :copy)))
               (setf (gethash output origins) origin))
              ((and origin (member kind '(:car :cdr :car-cons :cdr-cons :checked-car :checked-cdr)))
               (let ((value (if (member kind '(:car :car-cons :checked-car)) (first origin) (second origin))))
                 (unless reused (setq reused (native-value-table function)))
                 ;; Reuse tables have direct targets. If a long chain exceeds
                 ;; this bound, leave the ordinary access in place.
                 (loop repeat 32 for previous = (native-value-slot reused value)
                       while previous do (setq value previous))
                 (unless (native-value-slot reused value)
                   (compiler-statistic :planning/cons-field-forward kind)
                   (setf (native-value-slot reused output) value)
                   (let ((nested (gethash value origins)))
                     (when nested (setf (gethash output origins) nested))))))
              ((not (native-removable-p instruction))
               (when origins (clrhash origins)))))))))
    reused))

(defun native-plan-predicate-reuse (function blocks)
  (declare (type simple-vector function) (type list blocks))
  (when (or (>= (hir-function-debug-level function) 2)
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*)))
    (return-from native-plan-predicate-reuse nil))
  (let ((seen nil) (constants nil) (reused (native-plan-cons-forwarding function blocks)) (fuel 4096))
    (declare (type (or null hash-table) seen constants) (type (or null simple-vector) reused)
             (type (integer 0 4096) fuel))
    (dolist (block blocks)
      (let ((first nil) (first-epoch 0) (started nil) (epoch 0))
        (declare (type (integer 0 4096) epoch first-epoch))
        (dolist (instruction (hir-block-instructions block))
          ;; Cheap tag tests are rematerialized across effectful operations:
          ;; keeping their result alive can cost more code than recomputing it.
          (unless (native-removable-p instruction)
            (when (= epoch 4096) (return-from native-plan-predicate-reuse reused))
            (incf epoch))
          (let* ((output (hir-instruction-output instruction))
                 (capture (and output (eq (hir-instruction-opcode instruction) :capture)
                               (eq (hir-value-type output) t)
                               (not (eq (hir-value-kind output) :multiple))))
                 (kind (if capture :capture
                           (and (eq (hir-instruction-opcode instruction) :call)
                                (gethash instruction *wasm-inline-simple-calls*)))))
            ;; A closure's binding slots are immutable; a captured cell's
            ;; contents are not. Reuse only the slot read, leaving every cell
            ;; read/write and its ordering intact. Public HIR validation keeps
            ;; capture indices out of the private child-cache slot.
            (when (and kind (or capture (native-stable-predicate-p kind))
                       (native-value-slot *wasm-live-values* (hir-instruction-output instruction))
                       (or capture (native-value-slot *wasm-scalar-groups* (hir-instruction-output instruction))))
              (when (zerop fuel) (return-from native-plan-predicate-reuse reused))
              (decf fuel)
              (if (null first)
                  (setq first instruction first-epoch epoch)
                  (progn
                    ;; Most blocks have at most one live predicate. Allocate no
                    ;; keys/table until a second candidate makes reuse possible.
                    (unless started
                      (unless seen (setq seen (make-hash-table :test 'equal)
                                         constants (make-hash-table :test 'eq)))
                      (let ((scope (if (member (gethash first *wasm-inline-simple-calls*)
                                                '(:null :consp :simple-vector-p :vectorp :stringp :symbolp :integerp :characterp))
                                       (cons (hir-block-id block) first-epoch) (hir-block-id block))))
                        (setf (gethash (native-predicate-key scope first constants reused) seen)
                              (hir-instruction-output first)))
                      (setq started t))
                    (let* ((scope (if (member kind '(:null :consp :simple-vector-p :vectorp :stringp :symbolp :integerp :characterp))
                                      (cons (hir-block-id block) epoch) (hir-block-id block)))
                           (key (native-predicate-key scope instruction constants reused))
                           (previous (gethash key seen)))
                      (if previous
                          (progn
                            (compiler-statistic :planning/predicate-reuse kind)
                            (unless reused (setq reused (native-value-table function)))
                            (setf (native-value-slot reused (hir-instruction-output instruction)) previous))
                          (setf (gethash key seen) (hir-instruction-output instruction)))))))))))
    reused))


(defun native-predicate-aliases (function blocks reused)
  (declare (type simple-vector function reused) (type list blocks))
  ;; Flatten transparent copies of reused boolean results to the original
  ;; scalar value. Keep aliases within its block: their execution counts and
  ;; definition order then agree, including when the block is in a loop. All
  ;; targets remain original scalar results, so alias lookups are one-hop.
  (let ((roots (make-hash-table :test 'eq)))
    (dolist (value (hir-function-values function))
      (let ((previous (native-value-slot reused value)))
        (when previous (setf (gethash previous roots) t))))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (let ((output (hir-instruction-output instruction)) (inputs (hir-instruction-inputs instruction)))
          (when (gethash output roots) (setf (gethash output roots) block))
          (when (and (native-value-slot *wasm-live-values* output)
                     (not (and *wasm-checked-integer-copies*
                               (gethash output *wasm-checked-integer-copies*)))
                     (or (eq (hir-instruction-opcode instruction) :copy)
                         (and (eq (hir-instruction-opcode instruction) :primary)
                              (native-value-slot *wasm-scalar-groups* (first inputs)))))
            (let ((source (native-predicate-source (first inputs) reused)))
              (when (eq block (gethash source roots))
                (setf (native-value-slot reused output) source)))))))
    reused))


(defun native-full-groups (function)
  (declare (type simple-vector function))
  ;; Propagate multiple-value demand backwards through joins. An IF used in
  ;; a single-value context must not force each arm through the full protocol.
  (let ((full (native-value-table function)) (sources (native-value-table function)) (work nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (let* ((opcode (hir-instruction-opcode instruction))
               (target (and (member opcode '(:jump :block-return))
                            (first (hir-instruction-attributes instruction))))
               (parameter (and target (first (hir-block-parameters target)))))
          (cond
            (parameter
             (push (if (eq opcode :jump) (first (hir-instruction-inputs instruction))
                       (third (hir-instruction-inputs instruction))) (native-value-slot sources parameter)))
            ((not (or (eq opcode :primary)
                      (and (eq opcode :return) *wasm-primary-functions*
                           (gethash function *wasm-primary-functions*))))
             (dolist (input (hir-instruction-inputs instruction))
               (when (eq (hir-value-kind input) :multiple) (push input work))))))))
    (loop while work do
      (let ((value (pop work)))
        (unless (native-value-slot full value)
          (setf (native-value-slot full value) t)
          (dolist (source (native-value-slot sources value)) (push source work)))))
    full))

(defun native-proven-truth (value safety)
  "Return primary truth and a proof flag, without evaluating user code."
  (declare (type simple-vector value) (type (integer 0 3) safety))
  ;; Declarations can justify removing this test only under its safety
  ;; contract. The inferred facts already account for writes, escaping
  ;; functions and closed calls; opaque types supply no proof.
  (when (and (zerop safety) *wasm-inferred-types*)
    (let ((type (gethash value *wasm-inferred-types* t)))
      (unless (eq type t)
        (cond ((hir-type-proves-p type '(not null)) (return-from native-proven-truth (values t t)))
              ((hir-type-proves-p type 'null) (return-from native-proven-truth (values nil t)))))))
  (loop repeat 32 do
    ;; These premises come from an executed branch, independently of any
    ;; unchecked declaration. Private-cell writes, capture and joins already
    ;; limit their extent in HIR-TYPE-BRANCH-GUARDS. They are usable at every
    ;; safety level; an ordinary replaceable predicate supplies no premise.
    (dolist (premise (and *wasm-type-guards* (gethash value *wasm-type-guards*)))
      (let ((restriction (cdr premise)))
        (cond ((eq restriction t) (return-from native-proven-truth (values t t)))
              ((null restriction) (return-from native-proven-truth (values nil t)))
              ((and (symbolp restriction) (hir-type-proves-p restriction '(not null)))
               (return-from native-proven-truth (values t t))))))
    (let ((producer (hir-value-producer value)))
      (unless (%hir-instruction-p producer) (return))
      (case (hir-instruction-opcode producer)
        (:constant (return (values (not (null (first (hir-instruction-attributes producer)))) t)))
        ((:primary :copy :truth) (setq value (first (hir-instruction-inputs producer))))
        (:call
         (let ((kind (gethash producer *wasm-inline-simple-calls*)))
           (return (values (eq kind :true) (and (member kind '(:true :false)) t)))))
        (otherwise (return))))))

(defun native-proof-reachable-blocks (function)
  "Reachability under this emission's canonical INLINE proofs, or NIL if unknown.
Keep HIR and ordinary CFG layout unchanged. Only value-count joins consume this
view; operand effects remain in their original blocks. Captured entries and
exhausted work budgets keep every incoming value group."
  (declare (type simple-vector function))
  (when (or (>= (hir-function-debug-level function) 2)
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*)))
    (return-from native-proof-reachable-blocks nil))
  ;; Most functions have no constant predicate plan. Avoid allocating and
  ;; traversing another CFG when there is no edge this query can remove.
  (unless (some (lambda (block)
                  (let ((end (hir-block-terminator block)))
                    (and (eq (hir-instruction-opcode end) :branch)
                         (nth-value 1 (native-proven-truth (first (hir-instruction-inputs end))
                                                          (hir-instruction-safety-level end))))))
                (hir-function-blocks function))
    (return-from native-proof-reachable-blocks nil))
  (let ((alive (make-array (hir-function-block-count function) :initial-element nil))
        (pending (list (hir-function-entry function))) (fuel 65536))
    (declare (type simple-vector alive) (type list pending) (type (integer -1 65536) fuel))
    (loop while pending do
      (when (minusp (decf fuel)) (return-from native-proof-reachable-blocks nil))
      (let* ((block (pop pending)) (id (hir-block-id block)))
        (declare (type (unsigned-byte 32) id))
        (unless (svref alive id)
          (setf (svref alive id) t)
          (let* ((end (hir-block-terminator block)) (arms (hir-instruction-attributes end)))
            (if (eq (hir-instruction-opcode end) :branch)
                (multiple-value-bind (truth known)
                    (native-proven-truth (first (hir-instruction-inputs end)) (hir-instruction-safety-level end))
                  (if known
                      (progn (compiler-statistic :proof/branch-reachability truth)
                             (push (if truth (first arms) (second arms)) pending))
                      (dolist (arm arms) (push arm pending))))
                (dolist (arm arms) (push arm pending)))))))
    alive))

(defun native-scalar-groups (function full)
  (declare (type simple-vector function))
  ;; Value cardinality is independent of object type. Carry groups known to
  ;; contain exactly one value as the value itself, including through joins.
  (let ((scalar (native-value-table function)) (incoming (make-hash-table :test 'eq)) (changed t)
        (alive (native-proof-reachable-blocks function)))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (or (and (eq (hir-instruction-opcode instruction) :values)
                       (= (length (hir-instruction-inputs instruction)) 1))
                  (and (eq (hir-instruction-opcode instruction) :call)
                       (or (native-named-single-call-p instruction function)
                           (eql (standard-function-result-count (hir-inline-primitive instruction)
                                   (- (length (hir-instruction-inputs instruction)) 2)) 1)
                           ;; Current executable callee facts also describe
                           ;; exact cardinality across retained out-of-line
                           ;; calls. Primary demand is not this proof.
                           (and *wasm-result-arities*
                                (let* ((inputs (hir-instruction-inputs instruction))
                                       (target (hir-result-callee (second inputs) *wasm-result-callees*)))
                                  (and target
                                       (eql (- (length inputs) 2) (native-fixed-arity target))
                                       (eql (gethash target *wasm-result-arities*) 1))))
                           (gethash instruction *wasm-inline-vector-calls*)
                           (let ((kind (gethash instruction *wasm-inline-simple-calls*)))
                             ;; Canonical inline operations have known value
                             ;; counts even in a full multiple-value context.
                             ;; GETHASH retains its two-value representation.
                             (or (integerp kind)
                                 (member kind '(:eq :eql :consp :simple-vector-p :vectorp :stringp :symbolp :integerp :characterp :null :true :false
                                                :left :right :cons :list :list-star :car :cdr :car-cons :cdr-cons
                                                :checked-car :checked-cdr :rplaca-cons :rplacd-cons :length :list-length :vector-length
                                                :evenp :oddp :puthash :character-ref :character-set :simple-character-set :code-character :make-character-vector :make-string :make-simple-vector :character-code :character-code-at
                                                :integer-length :ash-general :logand-general :mod-general :truncate-primary
                                                :sbit-ref :row-major-ref :row-major-set
                                                :less-real :greater-real :not-greater-real :not-less-real :zero-number
                                                :proven-less-int64 :proven-greater-int64
                                                :proven-not-greater-int64 :proven-not-less-int64)))))))
          (setf (native-value-slot scalar (hir-instruction-output instruction)) t)))
      (let* ((terminator (hir-block-terminator block)) (opcode (hir-instruction-opcode terminator))
             (target (first (hir-instruction-attributes terminator))))
        (when (and (member opcode '(:jump :block-return)) target (hir-block-parameters target)
                   (or (null alive) (svref alive (the (unsigned-byte 32) (hir-block-id block)))))
          (push (if (eq opcode :jump) (first (hir-instruction-inputs terminator))
                    (third (hir-instruction-inputs terminator)))
                (gethash target incoming)))))
    ;; A captured block also has incoming value groups from other functions.
    (dolist (target (native-block-targets function))
      (push nil (gethash (hir-target-block target) incoming)))
    (loop while changed do
      (setq changed nil)
      (dolist (block (hir-function-blocks function))
        (let ((parameter (first (hir-block-parameters block))) (values (gethash block incoming)))
          (when (and parameter values (not (native-value-slot scalar parameter))
                     (every (lambda (value) (native-value-slot scalar value)) values))
            (compiler-statistic :optimization/single-value-join)
            (setf (native-value-slot scalar parameter) t) (setq changed t)))))
    (dolist (value (hir-function-values function))
      (when (and (eq (hir-value-kind value) :multiple) (not (native-value-slot full value)))
        (setf (native-value-slot scalar value) t)))
    scalar))

(defun native-named-call-p (inputs function)
  (when (and *wasm-named-recursion* (eq function (car *wasm-named-recursion*)))
    (let* ((reference (second inputs))
           (call (gethash reference (cdr *wasm-named-recursion*))))
      (and (%hir-instruction-p call) (eq (hir-instruction-opcode call) :call)
           (eq inputs (hir-instruction-inputs call))
           (let ((producer (hir-value-producer reference)))
             (and (%hir-instruction-p producer)
                  (eq (hir-instruction-opcode producer) :function-reference)
                  (eq (first (hir-instruction-attributes producer)) (hir-function-name function))))))))

(defun native-named-call-mode (call plan)
  (if (member call (svref plan 13) :test #'eq) :tail
      (when (native-value-slot (svref plan 8) (hir-instruction-output call)) :scoped)))

(defun native-named-single-call-p (call function)
  (and *wasm-named-single-result* (eq function (car *wasm-named-recursion*))
       (eq call (car (gethash (second (hir-instruction-inputs call)) *wasm-named-single-result*)))))

(defun native-plan-named-call-sites (function plan)
  ;; Only surviving calls authorize erasing their exact function lookup.
  ;; The scoped-body pass already checked all uses of these references.
  (when (and *wasm-scoped-call-functions* (eq function (car *wasm-named-recursion*)))
    (let ((arity (gethash function *wasm-scoped-call-functions*)) (sites nil))
      (when arity
        (dolist (block (svref plan 17))
          (dolist (call (hir-block-instructions block))
            (when (eq (hir-instruction-opcode call) :call)
              (let ((inputs (hir-instruction-inputs call)))
                (when (and (eql (- (length inputs) 2) arity) (native-named-call-p inputs function))
                  (let ((mode (native-named-call-mode call plan)))
                    (unless mode (throw 'native-multiple-result-fallback :retry-named-recursion))
                    (unless sites (setq sites (make-hash-table :test 'eq)))
                    (setf (gethash (second inputs) sites) (cons call mode)))))))))
      (when *wasm-named-single-result*
        (unless (and sites (= (hash-table-count sites) (hash-table-count *wasm-named-single-result*)))
          (throw 'native-multiple-result-fallback :retry-named-recursion))
        (maphash (lambda (reference site)
                   (unless (equal site (gethash reference sites))
                     (throw 'native-multiple-result-fallback :retry-named-recursion)))
                 *wasm-named-single-result*))
      sites)))

(defun native-named-single-result-p (function sites)
  ;; Close the exact-one query over all forwarding sources. An admitted
  ;; self-call returns through this same equation. A finite normal return
  ;; must reach a concrete producer. Unknown joins and sources decline it.
  (let ((incoming (native-value-table function)) (visited (native-value-table function))
        (pending nil) (seed nil) (fuel 2048))
    (dolist (block (hir-function-blocks function))
      (when (minusp (decf fuel)) (return-from native-named-single-result-p nil))
      (let* ((end (hir-block-terminator block))
             (opcode (hir-instruction-opcode end))
             (inputs (hir-instruction-inputs end))
             (target (first (hir-instruction-attributes end))))
        (case opcode
          (:return (push (first inputs) pending))
          ((:jump :block-return)
           (when (and target (hir-block-parameters target))
             (push (if (eq opcode :jump) (first inputs) (third inputs))
                   (native-value-slot incoming (first (hir-block-parameters target)))))))))
    (loop while pending do
      (when (minusp (decf fuel)) (return-from native-named-single-result-p nil))
      (let ((value (pop pending)))
        (unless (native-value-slot visited value)
          (setf (native-value-slot visited value) t)
          (let ((producer (hir-value-producer value)))
            (cond ((or (eq (hir-value-kind value) :single)
                       (eql (gethash value *wasm-result-arities*) 1))
                   (setq seed t))
                  ((not (%hir-instruction-p producer))
                   (let ((sources (native-value-slot incoming value)))
                     (unless sources (return-from native-named-single-result-p nil))
                     (dolist (source sources) (push source pending))))
                  (t
                   (let ((inputs (hir-instruction-inputs producer)))
                     (case (hir-instruction-opcode producer)
                       (:copy (push (first inputs) pending))
                       (:call
                        (unless (eq producer (car (gethash (second inputs) sites)))
                          (return-from native-named-single-result-p nil)))
                       (otherwise (return-from native-named-single-result-p nil))))))))))
    seed))

(defun native-simple-body-p (function &optional multiple-callees scalar-callees scoped-callees)
  ;; Bound tuple body lowering independently of permission to omit a frame.
  ;; These bodies neither establish dynamic scope nor use scalar tail paths.
  (let ((budget 2048) (scalar nil))
    (dolist (block (hir-function-blocks function) t)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf budget)) (return-from native-simple-body-p nil))
        (case (hir-instruction-opcode instruction)
          ((:constant :copy :argument :checked-argument :check-arguments :truth
            :function-reference :global-read :values :values-list :list :value-ref
            :cell-new :cell-read :cell-write :primary :capture :closure))
          ((:block-enter :block-environment)
           (when (hir-target-captured-p (first (hir-instruction-attributes instruction)))
             (return-from native-simple-body-p nil)))
          (:call
           ;; Canonical inline primitives lower before tail-call setup. An
           ;; arbitrary call could replace the caller's backtrace frame with
           ;; NATIVE-TAIL-ENTER when this callee's own frame was omitted. A
           ;; certified tuple call lowers before that tail-frame setup.
           (let ((kind (gethash instruction *wasm-inline-simple-calls*))
                 (inputs (hir-instruction-inputs instruction)))
             (unless (or (gethash instruction *wasm-inline-vector-calls*)
                         (integerp kind)
                         (member kind '(:eq :eql :consp :simple-vector-p :vectorp :stringp :symbolp :integerp :null :true :false
                                        :left :right :cons :list :list-star :car :cdr :car-cons :cdr-cons
                                        :checked-car :checked-cdr :length :vector-length :evenp :oddp))
                         ;; These checked operations lower before tail-frame
                         ;; setup. They need an owned frame, not dynamic-root
                         ;; snapshots. Their arithmetic/value checks stay live.
                         (and scoped-callees
                              (case (- (length inputs) 2)
                                (1 (and (eq kind :standard-call)
                                        (member (native-standard-operation (second inputs)) '(1+ 1-))))
                                (2 (or (member kind '(:less-real :greater-real :not-greater-real :not-less-real))
                                       (and (eq kind :standard-call)
                                            (member (native-standard-operation (second inputs)) '(+ -)))))))
                         (and scoped-callees (native-named-call-p inputs function)
                              (eql (length (cddr inputs)) (gethash function scoped-callees))
                              (let ((plan (or (gethash function *wasm-function-plans*)
                                              (setf (gethash function *wasm-function-plans*)
                                                    (native-function-plan function)))))
                                (native-named-call-mode instruction plan)))
                         (let* ((target (and (or multiple-callees scalar-callees scoped-callees)
                                             (hir-result-callee (second inputs) *wasm-result-callees*)))
                                (tuple (and target multiple-callees (gethash target multiple-callees)))
                                (mode (and target scalar-callees (gethash target scalar-callees))))
                           (and target (or tuple (member mode '(:single :primary))
                                           (and scoped-callees (gethash target scoped-callees)))
                                ;; A frameless body must not reach a framed
                                ;; scalar tail adapter. DEBUG 1 dependencies
                                ;; need call-site permission and an owned frame.
                                (or scoped-callees (zerop (hir-function-debug-level target)))
                                ;; Check certificate membership before scanning
                                ;; the target's entry, and bound a wide proper
                                ;; call before forcing any caller lowering plan.
                                (null (nthcdr 4 (cddr inputs)))
                                (eql (length (cddr inputs))
                                     (if scoped-callees (gethash target scoped-callees)
                                         (native-fixed-arity target)))
                                (or tuple
                                    ;; Only primary/discard uses take the fixed
                                    ;; scalar path before tail setup. :PRIMARY
                                    ;; supplies no complete return-count proof.
                                    (native-value-slot
                                      (or scalar
                                        (setq scalar
                                          (svref (or (gethash function *wasm-function-plans*)
                                                     (setf (gethash function *wasm-function-plans*)
                                                       (native-function-plan function))) 7)))
                                      (hir-instruction-output instruction))))))
               (return-from native-simple-body-p nil))))
          (otherwise (return-from native-simple-body-p nil))))
      (let* ((end (hir-block-terminator block)) (opcode (hir-instruction-opcode end)))
        (unless (or (member opcode '(:return :jump :branch))
                    (and (eq opcode :block-return) (hir-instruction-attributes end)))
          (return-from native-simple-body-p nil))))))

(defun native-simple-callee-p (function &optional multiple-callees scalar-callees)
  ;; This certifies the body, not permission to omit its frame at every call.
  (and (< (hir-function-debug-level function) 2)
       (native-simple-body-p function multiple-callees scalar-callees)))

(defun native-multiple-result-functions (unit used)
  ;; Grow tuple body certificates from leaves. Only previously proved DEBUG 0
  ;; tuple callees can discharge an ordinary CALL, so recursive components
  ;; cannot seed a proof. DEBUG 1 bodies additionally need site permission.
  (let ((functions (make-hash-table :test 'eq)) (pending nil) (fuel 65536))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from native-multiple-result-functions functions))
      (let ((count (gethash function *wasm-result-arities*)))
        (when (and count (gethash function used) (<= 2 count 4)
                   (gethash function *wasm-direct-workers*)
                   (not (and *wasm-primary-functions* (gethash function *wasm-primary-functions*))))
          (push function pending))))
    (loop while pending do
      (let ((next nil) (changed nil))
        (dolist (function pending)
          ;; Charge every repeated body scan, including failed candidates.
          (when (minusp (decf fuel)) (return-from native-multiple-result-functions functions))
          (dolist (block (hir-function-blocks function))
            (when (minusp (decf fuel)) (return-from native-multiple-result-functions functions))
            (dolist (instruction (hir-block-instructions block))
              (declare (ignore instruction))
              (when (minusp (decf fuel)) (return-from native-multiple-result-functions functions))))
          (if (native-simple-callee-p function functions)
              (setf (gethash function functions) (gethash function *wasm-result-arities*) changed t)
              (push function next)))
        (unless changed (return))
        (setq pending next)))
    functions))

(defun native-multiple-result-bodies (unit frameless)
  ;; Ordinary runtime entry retains the Lisp frame while its scalar adapter
  ;; calls the tuple body. Only the separate FRAMELESS map licenses bypassing
  ;; that entry at a permitted call site. New bodies may call its DEBUG 0
  ;; certified subset only.
  (let ((functions (make-hash-table :test 'eq)) (fuel 65536))
    (maphash (lambda (function count) (setf (gethash function functions) count)) frameless)
    (dolist (function (hir-unit-functions unit) functions)
      (when (minusp (decf fuel)) (return-from native-multiple-result-bodies functions))
      (let ((count (gethash function *wasm-result-arities*)))
        (when (and count (<= 2 count 3)
                   (gethash function *wasm-direct-workers*)
                   (not (gethash function functions))
                   (not (and *wasm-primary-functions* (gethash function *wasm-primary-functions*))))
          (dolist (block (hir-function-blocks function))
            (when (minusp (decf fuel)) (return-from native-multiple-result-bodies functions))
            (dolist (instruction (hir-block-instructions block))
              (declare (ignore instruction))
              (when (minusp (decf fuel)) (return-from native-multiple-result-bodies functions))))
          (when (native-simple-body-p function frameless)
            (setf (gethash function functions) count)))))))

(defun native-local-call-functions (unit)
  (unless *wasm-debug*
    (let ((functions nil) (pending nil) (fuel 65536))
      (labels ((record (function)
                 (unless functions (setq functions (make-hash-table :test 'eq)))
                 (setf (gethash function functions)
                   (or (and *wasm-frameless-multiple-functions* (gethash function *wasm-frameless-multiple-functions*))
                       (let* ((plan (or (gethash function *wasm-function-plans*)
                                      (setf (gethash function *wasm-function-plans*) (native-function-plan function))))
                              (scalar (svref plan 7)))
                         (if (every (lambda (block)
                               (let ((end (hir-block-terminator block)))
                                 (or (not (eq (hir-instruction-opcode end) :return))
                                     (native-value-slot scalar (first (hir-instruction-inputs end))))))
                               (hir-function-blocks function)) :single :primary))))))
        (dolist (function (hir-unit-functions unit))
          (when (minusp (decf fuel)) (return-from native-local-call-functions functions))
          (when (gethash function *wasm-direct-workers*)
            (cond ((or (and *wasm-closed-worker-functions* (gethash function *wasm-closed-worker-functions*))
                       (and *wasm-frameless-multiple-functions* (gethash function *wasm-frameless-multiple-functions*)))
                   ;; Existing trusted host/tuple seeds retain their contract.
                   ;; The work budget below bounds new scalar proof propagation.
                   (record function))
                  ((and (not (and *wasm-multiple-result-functions* (gethash function *wasm-multiple-result-functions*)))
                        (zerop (hir-function-debug-level function)))
                   (push function pending)))))
        (loop while pending do
          (let ((next nil) (changed nil))
            (dolist (function pending)
              ;; Validated counts price dense tables before a lowering plan is
              ;; forced. Charge descriptors absent from instruction operands too.
              (when (minusp (decf fuel (+ 1 (hir-function-value-count function)
                                           (hir-function-block-count function))))
                (return-from native-local-call-functions functions))
              (dolist (binding (hir-function-bindings function))
                (declare (ignore binding))
                (when (minusp (decf fuel)) (return-from native-local-call-functions functions)))
              (dolist (capture (hir-function-captures function))
                (declare (ignore capture))
                (when (minusp (decf fuel)) (return-from native-local-call-functions functions)))
              ;; Repeated failed scans consume the same aggregate budget as
              ;; successful ones; recursive dependencies cannot seed a proof.
              (dolist (block (hir-function-blocks function))
                (when (minusp (decf fuel)) (return-from native-local-call-functions functions))
                (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
                  (when (minusp (decf fuel)) (return-from native-local-call-functions functions))
                  ;; A single valid VALUES/LIST/CALL can have a wide operand
                  ;; spine. Charge it before forcing the caller's scalar plan.
                  (dolist (input (hir-instruction-inputs instruction))
                    (declare (ignore input))
                    (when (minusp (decf fuel)) (return-from native-local-call-functions functions)))))
              (if (native-simple-callee-p function nil
                    (unless (or *wasm-fixed-call-emitter* *wasm-closed-worker-functions*) functions))
                  (progn (record function) (setq changed t))
                  (push function next)))
            (unless changed (return))
            (setq pending next)))
        functions))))

(defun native-scoped-call-functions (unit)
  ;; This proves only absence of changes to this activation's dynamic roots.
  ;; It never licenses frame omission, result-count claims or unchecked values.
  ;; Start with a closed candidate graph and remove invalid dependencies to a
  ;; completed fixed point. Exhausting aggregate work supplies no certificate.
  (unless (or *wasm-debug* *wasm-fixed-call-emitter* *wasm-closed-worker-functions*)
    (let ((functions (make-hash-table :test 'eq)) (fuel 65536) (changed t))
      (dolist (function (hir-unit-functions unit))
        (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil))
        (when (and (gethash function *wasm-direct-workers*)
                   (< (hir-function-debug-level function) 2)
                   (not (gethash function *wasm-multiple-result-functions*))
                   (not (gethash function *wasm-optimization-barriers*)))
          ;; Cache entry arity once. Public HIR need not put its argument
          ;; check first; charge that scan instead of repeating it at callers.
          (dolist (instruction (hir-block-instructions (hir-function-entry function)))
            (declare (ignore instruction))
            (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil)))
          (setf (gethash function functions) (native-fixed-arity function))))
      (loop while changed do
        (setq changed nil)
        (dolist (function (hir-unit-functions unit))
          (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil))
          (when (gethash function functions)
            (when (minusp (decf fuel (+ (hir-function-value-count function)
                                       (hir-function-block-count function))))
              (return-from native-scoped-call-functions nil))
            (dolist (binding (hir-function-bindings function))
              (declare (ignore binding))
              (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil)))
            (dolist (capture (hir-function-captures function))
              (declare (ignore capture))
              (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil)))
            (let ((named (and *wasm-named-recursion* (eq function (car *wasm-named-recursion*))
                              (cdr *wasm-named-recursion*)))
                  (valid t))
              (dolist (block (hir-function-blocks function))
                (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
                  (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil))
                  (loop for input in (hir-instruction-inputs instruction) for index from 0 do
                    (when (minusp (decf fuel)) (return-from native-scoped-call-functions nil))
                    (when named
                      (let ((original-call (gethash input named)))
                        (when (and original-call
                                   (not (and (= index 1) (eq instruction original-call)
                                             (eq (hir-instruction-opcode instruction) :call))))
                          (setq valid nil)))))))
              (unless (and valid (native-simple-body-p function nil nil functions))
                (remhash function functions) (setq changed t))))))
      functions)))

(defun native-result-signature (function)
  (let* ((source (and *wasm-worker-origins* (gethash function *wasm-worker-origins*)))
         (reusable (eq function (first (and *wasm-reusable-scalars*
                                           (gethash source *wasm-reusable-scalars*)))))
         (count (and source (eq function (gethash source *wasm-direct-workers*))
                     *wasm-multiple-result-functions* (gethash source *wasm-multiple-result-functions*))))
    (list (cond (reusable '(:object :object :object :object :object :i64 :i64 :i64 :i64 :i32))
                ((and (eq function *wasm-private-scalar-worker*) *wasm-private-scalar-argument*)
                 (let ((parameters (make-list 5 :initial-element :object)))
                   (setf (nth (1+ *wasm-private-scalar-argument*) parameters) :i64)
                   parameters))
                (source '(:object :object :object :object :object))
                (t '(:object :object)))
          (cond (reusable '(:i64 :i32 :object))
                ((eq function *wasm-private-scalar-worker*)
                 (if (eq *wasm-private-scalar-type* 'character) :i32 :i64))
                (count (make-list count :initial-element :object))
                (t :object)))))

(defun native-no-prefix-apply-p (inputs function)
  (and (= (length inputs) 4)
       (< (hir-function-debug-level function) 2)
       (null *wasm-fixed-call-emitter*)
       (eq (native-standard-operation (second inputs)) 'apply)
       (let ((producer (hir-value-producer (second inputs))))
         (and (%hir-instruction-p producer)
              (eq (hir-instruction-opcode producer) :function-reference)
              (eq (first (hir-instruction-attributes producer)) 'apply)
              (not (eq (second (hir-instruction-attributes producer)) 'notinline))))))

(defun native-captured-call-p (instruction)
  ;; Match every earlier :CALL lowering before allocating heterogeneous locals.
  ;; The existing result-analysis switch excludes bootstrap, host closed-worker
  ;; linkage and high DEBUG policies; no frame-omission certificate is needed.
  (let ((output (hir-instruction-output instruction)))
    (and *wasm-result-arities* (not *wasm-fixed-call-emitter*)
         (not *wasm-closed-worker-functions*)
         (eq (hir-instruction-opcode instruction) :call)
         (or (= (length (hir-instruction-inputs instruction)) 3)
             (and *wasm-counted-result-consumer*
                  (native-no-prefix-apply-p (hir-instruction-inputs instruction) *wasm-current-function*)))
         (not (native-value-slot *wasm-scalar-groups* output))
         (not (native-value-slot *wasm-single-calls* output))
         (not (gethash instruction *wasm-inline-simple-calls*))
         (not (gethash instruction *wasm-inline-vector-calls*))
         (not (native-multiple-call-count (hir-instruction-inputs instruction) instruction))
         (not (member instruction *wasm-tail-calls* :test #'eq)))))

(defun native-captured-result-groups (function blocks)
  ;; Direct MVB/SETQ projections need no new value-count or effect inference.
  ;; Current valid HIR COPY operands are single values. Local joins, landing
  ;; pads and all other producers retain their existing packet representation.
  (when (or (null *wasm-result-arities*) *wasm-fixed-call-emitter*
            *wasm-closed-worker-functions*)
    (return-from native-captured-result-groups nil))
  (let ((groups (native-value-table function)) (fuel 4096) (found nil))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel)) (return-from native-captured-result-groups nil))
        (when (and (member (hir-instruction-opcode instruction) '(:primary :value-ref))
                   (native-value-slot *wasm-live-values* (hir-instruction-output instruction)))
          (let* ((value (first (hir-instruction-inputs instruction)))
                 (origin (hir-value-producer value)))
            (when (and (%hir-instruction-p origin) (native-captured-call-p origin))
              (setf (native-value-slot groups value) t found t))))))
    (and found groups)))

(defun emit-native-captured-test (locals index)
  (wasm-local (fifth locals))
  ;; Public HIR component indices are u32; I32.CONST uses signed LEB spelling.
  (wasm-integer (if (> index #x7fffffff) (- index #x100000000) index))
  (wasm-byte #x4b)) ; I32.GT_U: component is present independently of its value

(defun emit-native-captured-list (locals)
  ;; Materialize only at an ordinary full-group boundary. The count, rather
  ;; than NIL components, distinguishes no values from one or more NIL values.
  (emit-native-captured-test locals 0)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (first locals))
  (emit-native-captured-test locals 3)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (fourth locals))
  (when *wasm-counted-result-consumer*
    (wasm-local (fifth locals)) (wasm-call "native-captured-extra-list"))
  (wasm-byte #x05)
  (emit-native-captured-test locals 1)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (second locals))
  (emit-native-captured-test locals 2)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (third locals)) (wasm-null) (wasm-call "native-cons")
  (wasm-byte #x05) (wasm-null) (wasm-byte #x0b)
  (wasm-call "native-cons")
  (wasm-byte #x05) (wasm-null) (wasm-byte #x0b)
  (wasm-byte #x0b)
  (wasm-call "native-cons")
  (wasm-byte #x05) (wasm-null) (wasm-byte #x0b))

(defun emit-native-result (value index)
  (let ((captured (and *wasm-captured-result-locals*
                       (native-value-slot *wasm-captured-result-locals* value))))
    (when captured
      (emit-native-captured-test captured index)
      (wasm-byte #x04) (wasm-type :object)
      (cond ((< index 3) (wasm-local (nth index captured)))
            (*wasm-counted-result-consumer*
             (wasm-local (fourth captured))
             (wasm-integer (if (> index #x7fffffff) (- index #x100000000) index))
             (wasm-local (fifth captured))
             (wasm-call "native-captured-result-ref"))
            (t (wasm-local (fourth captured))
               (wasm-integer (if (> index #x80000000) (- index #x100000001) (1- index)))
               (wasm-call "native-argument")))
      (wasm-byte #x05) (wasm-null) (wasm-byte #x0b)
      (return-from emit-native-result nil)))
  (let ((locals (and *wasm-multiple-result-locals*
                     (native-value-slot *wasm-multiple-result-locals* value))))
    (cond (locals (if (< index (length locals)) (wasm-local (nth index locals)) (wasm-null)))
          ((native-value-slot *wasm-scalar-groups* value) (if (zerop index) (wasm-get value) (wasm-null)))
          (t (wasm-get value) (wasm-integer index) (wasm-call "native-argument")))))

(defun emit-native-results (value count)
  (dotimes (index count) (emit-native-result value index)))

(defun native-counted-result-return-p (value)
  ;; Share the exact-count proof used for ordinary local tuple joins. In a
  ;; named function even a single VALUES body ends at its implicit BLOCK join.
  (when (and *wasm-counted-result-producer* *wasm-result-arities*
             (not *wasm-fixed-call-emitter*) (not *wasm-closed-worker-functions*))
    (let ((arity (gethash value *wasm-result-arities*)))
      (and arity (<= 4 arity 8)))))

(defun native-local-result-groups (function blocks)
  ;; An immediate component consumer also benefits from tuple storage when
  ;; there is no private multiple-result worker. Follow its local joins back
  ;; to VALUES, keeping unknown calls and captured landing pads on the ordinary
  ;; packet path. Exact counts come from the shared, validated HIR analysis.
  (declare (type simple-vector function) (type list blocks))
  (unless *wasm-result-arities* (return-from native-local-result-groups nil))
  (let ((work nil) (fuel 4096))
    (declare (type list work) (type (integer -1 4096) fuel))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel)) (return-from native-local-result-groups nil))
        (when (and (member (hir-instruction-opcode instruction) '(:primary :value-ref))
                   (native-value-slot *wasm-live-values* (hir-instruction-output instruction)))
          (let* ((value (first (hir-instruction-inputs instruction)))
                 (arity (gethash value *wasm-result-arities*)))
            (when (and arity (<= 2 arity 8) (%hir-block-p (hir-value-producer value))
                       (not (native-value-slot *wasm-scalar-groups* value)))
              (push value work)))))
      (let ((end (hir-block-terminator block)))
        (when (and (eq (hir-instruction-opcode end) :return)
                   (native-counted-result-return-p (first (hir-instruction-inputs end))))
          (when (minusp (decf fuel)) (return-from native-local-result-groups nil))
          (push (first (hir-instruction-inputs end)) work))))
    (unless work (return-from native-local-result-groups nil))
    (let ((sources (native-value-table function)) (groups (native-value-table function)))
      (declare (type simple-vector sources groups))
      (dolist (block blocks)
        (when (minusp (decf fuel)) (return-from native-local-result-groups nil))
        (let* ((end (hir-block-terminator block)) (opcode (hir-instruction-opcode end))
               (target (and (member opcode '(:jump :block-return)) (first (hir-instruction-attributes end))))
               (parameter (and target (first (hir-block-parameters target)))))
          (when parameter
            (push (if (eq opcode :jump) (first (hir-instruction-inputs end))
                      (third (hir-instruction-inputs end))) (native-value-slot sources parameter)))))
      (loop while work do
        (when (minusp (decf fuel)) (return-from native-local-result-groups nil))
        (let* ((value (pop work)) (arity (gethash value *wasm-result-arities*))
               (origin (hir-value-producer value)))
          (when (and arity (<= 2 arity 8) (not (native-value-slot groups value))
                     (not (native-value-slot *wasm-scalar-groups* value))
                     (or (%hir-block-p origin)
                         (and (%hir-instruction-p origin) (eq (hir-instruction-opcode origin) :values))))
            (setf (native-value-slot groups value) t)
            (dolist (source (native-value-slot sources value)) (push source work)))))
      groups)))

(defun native-call-frame-elidable-p (target instruction)
  ;; DEBUG 0 permits ordinary frame omission. At DEBUG 1 only a proven tail
  ;; site may reuse the caller's frame. A worker callable without its own frame
  ;; must retain the framed adapter: replacing its inherited slot would erase
  ;; the actual live non-tail owner, and omitting setup would lose the callee's
  ;; checked-error frame. Body certificates alone supply neither permission.
  (or (zerop (hir-function-debug-level target))
      (and (= (hir-function-debug-level target) 1)
           instruction (member instruction *wasm-tail-calls* :test #'eq)
           (not *wasm-fixed-call-emitter*)
           (not (and *wasm-local-call-functions*
                     (gethash *wasm-current-function* *wasm-local-call-functions*))))))

(defun native-multiple-call-count (inputs &optional instruction)
  ;; Match the actual call lowering, not just the target body's signature.
  (let* ((target (hir-result-callee (second inputs) *wasm-result-callees*))
         (count (and target *wasm-frameless-multiple-functions*
                     (gethash target *wasm-frameless-multiple-functions*))))
    (and count *wasm-local-call-functions* (gethash target *wasm-local-call-functions*)
         (native-call-frame-elidable-p target instruction)
         (eql (length (cddr inputs)) (native-fixed-arity target)) count)))

(defun emit-native-multiple-call (inputs output instruction)
  (let* ((target (hir-result-callee (second inputs) *wasm-result-callees*))
         (count (native-multiple-call-count inputs instruction))
         (worker (and count (gethash target *wasm-direct-workers*)))
         (arity (length (cddr inputs))))
    (when (and worker (= arity (native-fixed-arity target)))
      (compiler-statistic :optimization/native-multiple-results count)
      (if (zerop (hir-function-debug-level target))
          (wasm-call "native-poll")
          ;; The site proof establishes that this is our own tail frame.
          ;; Reuse the existing checkpoint/frame/value protocol before entry.
          ;; As in the scalar lexical tail path, HIR operand effects precede
          ;; setup and operand materialization follows it. The repeated function
          ;; read is a local or a pure literal lookup, preserving closure context.
          (progn (wasm-get (second inputs)) (wasm-call "native-tail-enter") (wasm-byte #x1a)))
      (emit-native-call-operands inputs)
      (dotimes (i (- 4 arity)) (wasm-null))
      (pushnew worker *wasm-referenced-functions*)
      (wasm-byte #x10)
      (push (list (length *wasm-bytes*) worker)
            (gethash *wasm-current-function* *wasm-function-relocations*))
      (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
      (cond ((native-value-slot *wasm-scalar-groups* output)
             (dotimes (i (1- count)) (wasm-byte #x1a))
             (wasm-call "native-return1"))
            ((not (and *wasm-multiple-result-locals*
                       (native-value-slot *wasm-multiple-result-locals* output)))
             (wasm-null) (dotimes (i count) (wasm-call "native-cons"))))
      t)))

(defun emit-native-local-call (inputs)
  (let* ((target (and *wasm-local-call-functions* (native-known-callee (second inputs))))
         (mode (and target (gethash target *wasm-local-call-functions*)))
         (worker (and mode (gethash target *wasm-direct-workers*)))
         (arity (length (cddr inputs))))
    (when (and worker (not (integerp mode)) (= arity (native-fixed-arity target)))
      (compiler-statistic :optimization/direct-private-call mode)
      ;; Every admitted operation reads explicit HIR operands, not ambient
      ;; multiple-value state. A single-valued return already clears that state;
      ;; primary-only callers clear it here after other value counts. DEBUG 0
      ;; permits frame omission, and the body needs no dynamic-scope cleanup.
      (wasm-call "native-poll")
      (emit-native-call-operands inputs)
      (dotimes (i (- 4 arity)) (wasm-null))
      (pushnew worker *wasm-referenced-functions*)
      (wasm-byte #x10)
      (push (list (length *wasm-bytes*) worker)
        (gethash *wasm-current-function* *wasm-function-relocations*))
      (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
      (unless (eq mode :single) (wasm-call "native-return1"))
      t)))

(defun native-scoped-call-target (inputs)
  (let ((target (and *wasm-scoped-call-functions*
                     (if (native-named-call-p inputs *wasm-current-function*) *wasm-current-function*
                         (native-known-callee (second inputs))))))
    (and target (gethash target *wasm-scoped-call-functions*)
         ;; Keep the compact default selective: recursive sites repay this
         ;; retained-frame envelope without cloning workers or growing leaves.
         (eq target *wasm-current-function*)
         (not (and *wasm-local-call-functions*
                   (member (gethash target *wasm-local-call-functions*) '(:single :primary))))
         (eql (length (cddr inputs)) (gethash target *wasm-scoped-call-functions*)) target)))

(defun native-scoped-call-sites-p (function plan)
  (some (lambda (block)
          (some (lambda (instruction)
                  (and (eq (hir-instruction-opcode instruction) :call)
                       (not (member instruction (svref plan 13) :test #'eq))
                       (native-value-slot (svref plan 8) (hir-instruction-output instruction))
                       (native-scoped-call-target (hir-instruction-inputs instruction))))
                (hir-block-instructions block)))
        (hir-function-blocks function)))

(defun emit-native-call-frame-enter (inputs type)
  ;; Enter owns the checkpoint; a transfer before its push needs no cleanup.
  (wasm-get (second inputs)) (wasm-call "native-frame-enter")
  (wasm-byte #x21) (wasm-u32 (second *wasm-call-frame-locals*))
  (wasm-byte #x21) (wasm-u32 (first *wasm-call-frame-locals*))
  (setq *wasm-exceptions* t)
  (wasm-byte #x02) (wasm-type type)
  (wasm-byte #x02) (wasm-byte #x40)
  (dolist (byte '(#x1f #x40 1 0 0 0)) (wasm-byte byte)))

(defun emit-native-call-frame-leave (raw)
  ;; An object result is checked inside the owned transfer scope. For a
  ;; certified raw result, pass NIL to cleanup and leave the scalar stacked.
  (when raw (wasm-null))
  (wasm-local (first *wasm-call-frame-locals*))
  (wasm-local (second *wasm-call-frame-locals*))
  (wasm-integer 1) (wasm-call "native-frame-leave")
  (when raw (wasm-byte #x1a))
  (wasm-byte #x0c) (wasm-u32 2)
  (wasm-byte #x0b) (wasm-byte #x0b)
  (wasm-null)
  (wasm-local (first *wasm-call-frame-locals*))
  (wasm-local (second *wasm-call-frame-locals*))
  (wasm-integer 0) (wasm-call "native-frame-leave") (wasm-byte #x1a)
  (wasm-byte #x08) (wasm-u32 0)
  (wasm-byte #x0b))

(defun emit-native-scoped-call (inputs)
  (let* ((target (and *wasm-call-frame-locals* (native-scoped-call-target inputs)))
         (worker (and target (gethash target *wasm-direct-workers*))))
                 (when worker
      (compiler-statistic :optimization/direct-scoped-call)
      (emit-native-call-frame-enter inputs :object)
      (emit-native-call-operands inputs)
      (dotimes (i (- 4 (length (cddr inputs)))) (wasm-null))
      (pushnew worker *wasm-referenced-functions*)
      (wasm-byte #x10)
      (push (list (length *wasm-bytes*) worker)
        (gethash *wasm-current-function* *wasm-function-relocations*))
      (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
      (emit-native-call-frame-leave nil)
      t)))

(defun emit-native-fixed-value-call (inputs output)
  ;; Transport is independent of target identity, result cardinality and
  ;; effects. The shared checked runtime selects the actual live callee.
  (let ((arity (length (cddr inputs))))
    (when (and (not *wasm-fixed-call-emitter*)
               (not *wasm-closed-worker-functions*) (<= arity 4))
      (let ((primary (or (native-value-slot *wasm-scalar-groups* output)
                         (not (native-value-slot *wasm-live-values* output)))))
        (compiler-statistic :optimization/fixed-value-transport
          (if primary :primary :full))
        (emit-native-call-operands inputs)
        (dotimes (i (- 4 arity)) (wasm-null))
        (wasm-integer (logior arity (if primary 8 0)))
        (wasm-call "native-fixed-call")
        t))))

(defun emit-native-fixed-call (inputs)
  (when (and *wasm-fixed-call-emitter* (funcall *wasm-fixed-call-emitter* inputs))
    (return-from emit-native-fixed-call nil))
  (when (emit-native-local-call inputs) (return-from emit-native-fixed-call nil))
  (when (emit-native-scoped-call inputs) (return-from emit-native-fixed-call nil))
  (compiler-statistic :lowering/ordinary-call (length (cddr inputs)))
  (emit-native-call-operands inputs)
  (wasm-call (nth (length (cddr inputs))
               '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4"))))

(defun native-type-integer-width (type)
  ;; One target CL type domain drives declarations, module summaries and
  ;; representation selection. Never ask the host about target FIXNUMs.
  (if (and (consp type) (eq (car type) 'values))
      (unless (member (second type) lambda-list-keywords)
        (native-type-integer-width (second type)))
      (multiple-value-bind (low high bounded) (hir-integer-bounds type)
        (when bounded (1+ (max (integer-length low) (integer-length high)))))))

(defun native-compute-integer-width (value)
  "A proven signed integer width of at most 64, or NIL when unknown."
  (declare (type simple-vector value))
  (let* ((producer (hir-value-producer value))
         (type (if *wasm-inferred-types* (gethash value *wasm-inferred-types* t) (hir-value-type value)))
         (width (unless (eq type t) (native-type-integer-width type))))
    (or (and width (<= 1 width 64) width)
        (and (%hir-instruction-p producer)
          (case (hir-instruction-opcode producer)
            (:constant (let ((literal (first (hir-instruction-attributes producer))))
                         (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807)
                              (1+ (integer-length literal)))))
            (:primary (native-integer-width (first (hir-instruction-inputs producer)))))))))

(defun native-integer-width (value)
  (declare (type simple-vector value))
  ;; Cache unknown widths as well as known ones within an emission, without
  ;; retaining facts between emissions
  ;; of an HIR graph which a client may have subsequently rewritten.
  (if *wasm-integer-widths*
      (let ((width (native-value-slot *wasm-integer-widths* value)))
        (if (eq width :unknown)
            (setf (native-value-slot *wasm-integer-widths* value) (native-compute-integer-width value)) width))
      (native-compute-integer-width value)))

(defun emit-native-unboxed-integer (operand width)
  (declare (type simple-vector operand) (type (integer 1 64) width))
  (let* ((cached (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* operand)))
         (producer (hir-value-producer operand))
         (literal (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant)
                       (first (hir-instruction-attributes producer)))))
    (cond ((and cached (or (eq (svref cached 4) :result) (svref cached 3)))
           (compiler-statistic (if (eq (svref cached 4) :result)
                                   :optimization/unboxed-result-read :optimization/unboxed-argument-read))
           (wasm-local (svref cached 0)))
          ((and (integerp literal) (<= -2147483648 literal 2147483647))
           ;; Signed LEB32 is also the canonical LEB64 encoding for this range.
           (wasm-byte #x42) (wasm-s32 literal))
          (t
           (wasm-get operand)
           ;; Logical ranges and checked producer origins permit scalar
           ;; extraction, but do not distinguish immediate from boxed i64.
           (wasm-call "native-unbox-int64")
           (when (and cached (not (eq (svref cached 4) :checked-copy)))
             ;; The planner proves a real first use in the entry block. Cache
             ;; at that original read, preserving checks and effects before it.
             (compiler-statistic :optimization/unboxed-argument-initialize)
             (wasm-byte #x22) (wasm-u32 (svref cached 0))
             (setf (svref cached 3) t))))))

(defun native-integer-result-fits-p (name inputs)
  (declare (type symbol name) (type list inputs))
  ;; Derive this from operand types, rather than treating a result assertion
  ;; as evidence that the arithmetic itself cannot overflow.
  (let ((result (hir-inline-result-type name
                  (mapcar (lambda (value)
                    (if *wasm-inferred-types* (gethash value *wasm-inferred-types* t)
                        (hir-value-type value))) (cddr inputs)))))
    (multiple-value-bind (low high bounded) (hir-integer-bounds result)
      (and bounded (<= -9223372036854775808 low high 9223372036854775807)))))

(defun native-inline-shift-opcode (inputs)
  (let* ((left (third inputs)) (count (fourth inputs))
         (width (native-integer-width left))
         (type (if *wasm-inferred-types* (gethash count *wasm-inferred-types* t) (hir-value-type count))))
    (multiple-value-bind (low high known) (hir-integer-bounds type)
      (when (and width known)
        (cond ((or (and (= low high) (minusp low)) (<= -63 low high 0))
               (compiler-statistic :proof/integer-shift-range :right) #x87)
              ((and (<= 0 low high 63) (native-integer-result-fits-p 'ash inputs))
               (compiler-statistic :proof/integer-shift-range :left) #x86))))))

(defun native-inline-nonnegative-division-opcode (name inputs)
  "Prove integer division cannot trap, overflow, or require a floor adjustment."
  (when (and (= (length inputs) 4) (member name '(mod truncate)))
    (multiple-value-bind (a b left)
        (hir-integer-bounds (if *wasm-inferred-types*
          (gethash (third inputs) *wasm-inferred-types* t) (hir-value-type (third inputs))))
      (multiple-value-bind (c d right)
          (hir-integer-bounds (if *wasm-inferred-types*
            (gethash (fourth inputs) *wasm-inferred-types* t) (hir-value-type (fourth inputs))))
        (and left right (<= 0 a b 9223372036854775807) (<= 1 c d 9223372036854775807)
             (if (eq name 'mod) #x82 #x80))))))

(defun native-inline-integer-opcode (name inputs)
  (declare (type symbol name) (type list inputs))
  ;; Ordered comparisons relate adjacent operands. All argument expressions
  ;; have already run in source order before this HIR call; every scalar proof
  ;; is required, even if an earlier comparison would be false. Bound expansion
  ;; so a large rest call keeps the compact shared implementation.
  (when (and (<= 5 (length inputs) 10) (member name '(< > <= >=))
             (every #'native-integer-width (cddr inputs)))
    (return-from native-inline-integer-opcode
      (case name (< #x53) (> #x55) (<= #x57) (>= #x59))))
  (when (and (eq name 'lognot) (= (length inputs) 3))
    (return-from native-inline-integer-opcode (and (native-integer-width (third inputs)) #x85)))
  (when (and (= (length inputs) 3) (member name '(zerop minusp plusp evenp oddp)))
    (let* ((value (third inputs))
           (type (if *wasm-inferred-types* (gethash value *wasm-inferred-types* t) (hir-value-type value)))
           (result (hir-integer-predicate-type name type)))
      (return-from native-inline-integer-opcode
        (cond ((equal result '(eql t)) :true) ((eq result 'null) :false)
              ((native-integer-width value)
               (case name (zerop #x50) (minusp #x53) (plusp #x55) (evenp :evenp) (oddp :oddp)))))))
  (when (and (= (length inputs) 3) (member name '(1+ 1-)))
    (let ((width (native-integer-width (third inputs))))
      (return-from native-inline-integer-opcode
        (and width (or (<= width 63) (native-integer-result-fits-p name inputs))
             (if (eq name '1+) #x7c #x7d)))))
  (when (= (length inputs) 4)
    (let ((identity (hir-integer-identity name
                      (if *wasm-inferred-types* (gethash (third inputs) *wasm-inferred-types* t)
                          (hir-value-type (third inputs)))
                      (if *wasm-inferred-types* (gethash (fourth inputs) *wasm-inferred-types* t)
                          (hir-value-type (fourth inputs))))))
      (when identity (return-from native-inline-integer-opcode identity)))
    (when (eq name 'ash)
      (return-from native-inline-integer-opcode (native-inline-shift-opcode inputs)))
    (let ((left (native-integer-width (third inputs)))
          (right (native-integer-width (fourth inputs))))
      (when (and left right)
        (locally (declare (type (integer 1 64) left right))
          (case name
            (+ (and (or (and (<= left 63) (<= right 63))
                        (native-integer-result-fits-p name inputs)) #x7c))
            (- (and (or (and (<= left 63) (<= right 63))
                        (native-integer-result-fits-p name inputs)) #x7d))
            (* (and (or (<= (+ left right) 64)
                        (native-integer-result-fits-p name inputs)) #x7e))
            (logand #x83) (logior #x84) (logxor #x85)
            ((= eql) #x51) (/= #x52) (< #x53) (> #x55) (<= #x57) (>= #x59)))))))

(defun emit-native-integer-chain (arguments widths opcode &optional invariant)
  (declare (type list arguments widths) (type (unsigned-byte 8) opcode))
  (labels ((operand (value width)
             (if invariant
                 (let* ((producer (hir-value-producer value))
                        (literal (and (%hir-instruction-p producer)
                                      (eq (hir-instruction-opcode producer) :constant)
                                      (first (hir-instruction-attributes producer)))))
                   (if (integerp literal)
                       (progn (wasm-byte #x42) (wasm-s64 literal))
                       (progn (wasm-get value)
                         (wasm-call "native-unbox-int64"))))
                 (emit-native-unboxed-integer value width))))
    (loop for tail on arguments for sizes on widths while (cdr tail)
          for first = t then nil do
      (operand (first tail) (first sizes))
      (operand (second tail) (second sizes))
      (wasm-byte opcode)
      (unless first (wasm-byte #x71)))))

(defun emit-native-inline-integer (inputs opcode &optional raw)
  (declare (type list inputs) (type (unsigned-byte 8) opcode))
  ;; Planning proved scalar operands and, for arithmetic, absence of overflow.
  ;; INLINE selects the canonical definition at compilation. Values remain
  ;; boxed at generic HIR boundaries and retain the ordinary value count.
  (when (and (> (length inputs) 4) (member opcode '(#x53 #x55 #x57 #x59)))
    (emit-native-integer-chain (cddr inputs) (mapcar #'native-integer-width (cddr inputs)) opcode)
    (unless raw (wasm-call "native-boolean"))
    (return-from emit-native-inline-integer nil))
  (if (member opcode '(#x86 #x87))
      (let* ((count (fourth inputs))
             (type (if *wasm-inferred-types* (gethash count *wasm-inferred-types* t) (hir-value-type count))))
        (multiple-value-bind (low high known) (hir-integer-bounds type)
          (unless (and known
                       (if (= opcode #x87)
                           (or (and (= low high) (minusp low)) (<= -63 low high 0))
                           (<= 0 low high 63)))
            (error "A native shift lost its count range proof."))
          (compiler-statistic :optimization/inline-shift (if (= opcode #x87) :right :left))
          (emit-native-unboxed-integer (third inputs) (native-integer-width (third inputs)))
          ;; Wasm masks the shift count. Arithmetic right shifts of signed
          ;; i64 by 63 or more instead saturate to the sign bit, as CL requires.
          (if (= low high)
              (progn (wasm-byte #x42) (wasm-s32 (if (= opcode #x87) (min 63 (- low)) low)))
              (progn
                (when (= opcode #x87) (wasm-byte #x42) (wasm-s32 0))
                (emit-native-unboxed-integer count (native-integer-width count))
                (when (= opcode #x87) (wasm-byte #x7d))))))
      (dolist (operand (cddr inputs))
        (emit-native-unboxed-integer operand (native-integer-width operand))))
  (when (and (= (length inputs) 3) (/= opcode #x50))
    (wasm-byte #x42) (wasm-s32 (cond ((member opcode '(#x7c #x7d)) 1) ((= opcode #x85) -1) (t 0))))
  (wasm-byte opcode)
  (unless raw (wasm-call (if (< opcode #x60) "native-boolean" "native-int64"))))

(defun emit-native-i31-operand (value &optional testp)
  ;; A literal is emitted in the target's immediate representation. Types
  ;; alone never prove that an arbitrary operand has that representation.
  (let* ((producer (hir-value-producer value))
         (literal (and (%hir-instruction-p producer)
                       (eq (hir-instruction-opcode producer) :constant)
                       (first (hir-instruction-attributes producer)))))
    (if (and (integerp literal) (<= -1073741824 literal 1073741823))
        (wasm-integer (if testp 1 literal))
        (progn
          (wasm-get value) (wasm-gc #x1a)
          (if testp (wasm-gc #x14 #x6c)
              (progn (wasm-gc #x16 #x6c) (wasm-gc #x1d)))))))

(defun emit-native-i31-step (inputs &optional binary-integer-p)
  "Inline immediate successor/predecessor; retain the original fallback."
  ;; The shared scalar entries already handle actual i31 inputs. Duplicate
  ;; their representation/endpoint checks only when the caller favors speed.
  (let ((policy (hir-function-optimization-policy *wasm-current-function*)))
    (when (> (logand policy 3) (logand 3 (ash policy -2)))
      (emit-native-i31-step-worker inputs binary-integer-p))))

(defun emit-native-i31-step-worker (inputs binary-integer-p)
  (let* ((producer (hir-value-producer (second inputs)))
         (attributes (and (%hir-instruction-p producer)
                          (eq (hir-instruction-opcode producer) :function-reference)
                          (hir-instruction-attributes producer)))
         (name (first attributes)) (value (third inputs))
         (literal-producer (and binary-integer-p (= (length inputs) 4)
                                (hir-value-producer (fourth inputs)))))
    ;; Binary admission comes only from the existing boxed integer arm,
    ;; after its width checks and exclusion of reusable raw SSA results.
    (unless (and (not (eq (second attributes) 'notinline))
                 (if binary-integer-p
                     (and (= (length inputs) 4) (member name '(+ -))
                          (%hir-instruction-p literal-producer)
                          (eq (hir-instruction-opcode literal-producer) :constant)
                          (eql (first (hir-instruction-attributes literal-producer)) 1))
                     (and (= (length inputs) 3) (member name '(1+ 1-)))))
      (return-from emit-native-i31-step-worker nil))
    ;; Only actual i31 values enter the cast. The directional endpoint
    ;; keeps the original worker so promotion never wraps the immediate.
    (emit-native-i31-operand value t)
    (wasm-byte #x04) (wasm-type :i32)
    (emit-native-i31-operand value)
    (wasm-integer (if (member name '(1+ +)) 1073741823 -1073741824))
    (wasm-byte #x46) (wasm-byte #x45)
    (wasm-byte #x05) (wasm-integer 0) (wasm-byte #x0b)
    (wasm-byte #x04) (wasm-type :object)
    ;; The binary integer entry has neither a poll nor a values reset.
    ;; The unary standard entry polls, and its inline arm must do so too.
    (unless binary-integer-p (wasm-call "native-poll"))
    (emit-native-i31-operand value) (wasm-integer 1)
    (wasm-byte (if (member name '(1+ +)) #x6a #x6b))
    (wasm-gc #x1c) (wasm-gc #x1b)
    (wasm-byte #x05)
    (if binary-integer-p
        (progn
          (dolist (operand (cddr inputs)) (wasm-get operand))
          (wasm-call (if (eq name '+) "native-add-integer" "native-sub-integer")))
        (progn
          (wasm-integer (native-canonical-builtin-code name)) (wasm-get value)
          (wasm-call "native-standard-call1")))
    (wasm-byte #x0b)
    (compiler-statistic :optimization/i31-step name)
    t))

(defun emit-native-i31-comparison (inputs kind &optional raw)
  ;; The guard supplies an executable representation fact even for a mutable
  ;; counter or an outside caller. Every non-i31 value keeps the real-number
  ;; comparison, including boxed i64, bignums, numeric contagion and errors.
  (let ((policy (hir-function-optimization-policy *wasm-current-function*)))
    (unless (> (logand policy 3) (logand 3 (ash policy -2)))
      (wasm-get (third inputs)) (wasm-get (fourth inputs))
      (wasm-integer (position kind '(:less-real :greater-real :not-greater-real :not-less-real)))
      (wasm-call "native-ordered-compare-inline")
      (when raw (wasm-byte #xd1) (wasm-byte #x45))
      (return-from emit-native-i31-comparison nil)))
  (emit-native-i31-comparison-worker inputs kind raw))

(defun emit-native-i31-comparison-worker (inputs kind raw)
  (emit-native-i31-operand (third inputs) t)
  (emit-native-i31-operand (fourth inputs) t) (wasm-byte #x71)
  (wasm-byte #x04) (wasm-type (if raw :i32 :object))
  (emit-native-i31-operand (third inputs))
  (emit-native-i31-operand (fourth inputs))
  (wasm-byte (ecase kind (:less-real #x48) (:greater-real #x4a)
                         (:not-greater-real #x4c) (:not-less-real #x4e)))
  (unless raw (wasm-call "native-boolean"))
  (wasm-byte #x05)
  (wasm-get (third inputs)) (wasm-get (fourth inputs))
  (wasm-integer (position kind '(:less-real :greater-real :not-greater-real :not-less-real)))
  (wasm-call "native-ordered-compare-inline")
  (when raw (wasm-byte #xd1) (wasm-byte #x45))
  (wasm-byte #x0b))

(defun emit-native-integer-call (inputs &optional output (safety 1))
  (declare (type list inputs) (type (or null simple-vector) output)
           (type (integer 0 3) safety))
  ;; Proven i64 operands use scalar entries. Narrow ranges need no overflow
  ;; test; wider arithmetic uses checked entries which promote to bignums.
  ;; Exact standard names below are stable; ordinary user functions and
  ;; dynamic function values do not enter this path.
  (unless (= (length inputs) 4) (return-from emit-native-integer-call nil))
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (and (%hir-instruction-p producer)
                 (eq (hir-instruction-opcode producer) :function-reference)
                 (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                 (first (hir-instruction-attributes producer))))
         (opcode (cdr (assoc name '((+ . "native-add-int64") (- . "native-sub-int64") (* . "native-mul-int64")
                                   (logand . "native-logand-int64") (logior . "native-logior-int64")
                                   (logxor . "native-logxor-int64") (ash . "native-ash-int64")
                                   (< . "native-lt-int64") (> . "native-gt-int64")
                                   (<= . "native-le-int64") (>= . "native-ge-int64")
                                   (= . "native-eq-int64") (/= . "native-ne-int64")))))
         (arithmetic (member name '(+ - *)))
         (widths (and opcode (mapcar #'native-integer-width (cddr inputs)))))
    (when (member name '(svref aref row-major-aref))
      (return-from emit-native-integer-call (emit-native-vector-call inputs safety)))
    (unless (and opcode (every #'identity widths))
      (return-from emit-native-integer-call nil))
    (when (and arithmetic
               (every (lambda (operand)
                        ;; Match WASM-GET's reuse and boxing rules. A raw SSA
                        ;; value with no saved reference stays on the raw ABI.
                        (and (not (and *wasm-multiple-result-locals*
                                       (native-value-slot *wasm-multiple-result-locals* operand)))
                             (let* ((value (or (and *wasm-reused-values*
                                                  (native-value-slot *wasm-reused-values* operand)) operand))
                                    (raw (and *wasm-unboxed-values*
                                              (native-value-slot *wasm-unboxed-values* value))))
                               (not (and raw (eq (svref raw 4) :result) (<= (svref raw 5) 1))))))
                      (cddr inputs)))
      (unless (emit-native-i31-step inputs t)
        (dolist (operand (cddr inputs)) (wasm-get operand))
        (wasm-call (ecase name (+ "native-add-integer") (- "native-sub-integer") (* "native-mul-integer"))))
      (return-from emit-native-integer-call t))
    (when (and arithmetic
               (not (if (eq name '*) (<= (+ (first widths) (second widths)) 64)
                        (<= (max (first widths) (second widths)) 63)))
               (not (native-integer-result-fits-p name inputs))
               (not (and (zerop safety) output
                         (let ((result-width (native-type-integer-width (hir-value-type output))))
                           (and result-width (<= result-width 64))))))
      (setq opcode (concatenate 'string opcode "-checked")))
    (loop for operand in (cddr inputs) for width in widths do
      (emit-native-unboxed-integer operand width))
    (wasm-call opcode)
    t))

(defun native-simple-vector-value-p (value)
  (declare (type simple-vector value))
  (let ((type (if *wasm-inferred-types* (gethash value *wasm-inferred-types* t) (hir-value-type value)))
        (producer (hir-value-producer value)))
    (or (hir-type-proves-p type 'simple-vector)
        (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :primary)
             (native-simple-vector-value-p (first (hir-instruction-inputs producer)))))))

(defun native-vector-call-width (inputs safety)
  (declare (type list inputs) (type (integer 0 3) safety))
  (unless (= (length inputs) 4) (return-from native-vector-call-width nil))
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :function-reference)
                    (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                    (first (hir-instruction-attributes producer))))
         (width (and (member name '(svref aref row-major-aref))
                     (native-simple-vector-value-p (third inputs)) (native-integer-width (fourth inputs)))))
    (when (and width (<= width (if (zerop safety) 64 31))) width)))

(defun emit-native-vector-call (inputs &optional (safety 1))
  (let ((width (native-vector-call-width inputs safety)))
    (unless width (return-from emit-native-vector-call nil))
    (wasm-get (third inputs))
    (emit-native-unboxed-integer (fourth inputs) width)
    (when (<= width 31) (wasm-byte #xa7))
    (wasm-call (cond ((not (zerop safety)) "native-vector-ref")
                     ((<= width 31) "native-vector-ref-trusted")
                     (t "native-vector-ref-inline")))
    t))

(defun native-list-value-p (value)
  (declare (type simple-vector value))
  (let ((type (if *wasm-inferred-types* (gethash value *wasm-inferred-types* t) (hir-value-type value))))
    (hir-type-proves-p type 'list)))

(defun emit-native-character-call (inputs)
  ;; Exact standard calls share the checked character implementation.
  (declare (type list inputs))
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (and (%hir-instruction-p producer)
                    (eq (hir-instruction-opcode producer) :function-reference)
                    (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                    (first (hir-instruction-attributes producer))))
         (target (cond ((and (= (length inputs) 3) (eq name 'char-code)) "native-character-code-inline")
                       ((and (= (length inputs) 4) (eq name 'char)) "native-character-ref-inline"))))
    (when target
      ;; Retain the function-reference producer's ordinary evaluation while
      ;; sharing the checked worker without an unused function operand.
      (dolist (value (cddr inputs)) (wasm-get value))
      (wasm-call target)
      t)))

(defun emit-native-list-call (inputs safety)
  (declare (type list inputs) (type (integer 0 3) safety))
  (unless (= (length inputs) 3) (return-from emit-native-list-call nil))
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (and (%hir-instruction-p producer)
                    (eq (hir-instruction-opcode producer) :function-reference)
                    (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                    (first (hir-instruction-attributes producer))))
         (listp (native-list-value-p (third inputs))))
    ;; LIST includes NIL and arbitrary conses, including dotted lists. These
    ;; declarations justify removing the generic argument tag dispatch.
    (when (member name '(car first cdr rest))
      ;; These exact standard entries ignore the captured function object.
      ;; Its producer retains normal evaluation; only the unused ABI operand
      ;; disappears. Reuse the same single-argument helpers as inline plans.
      (wasm-get (third inputs))
      (wasm-call (if (and (zerop safety) listp)
                    (if (member name '(car first)) "native-car-list-inline" "native-cdr-list-inline")
                    (if (member name '(car first)) "native-primary" "native-cdr-inline-checked")))
      t)))

(defun native-plan-dynamic-rests (unit)
  ;; DX selects a bounded private-use audit, not an alias or effect promise.
  ;; Raw packets may occupy only plain Wasm locals between preparation and
  ;; LENGTH. Unsupported/debug-visible uses retain the ordinary Lisp list.
  (let ((plans (make-hash-table :test 'eq)) (fuel 8192))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
      (when (and (< (hir-function-debug-level function) 2)
                 (< (ash (hir-function-optimization-policy function) -4) 3)
                 (not (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*))))
        (let ((uses (make-hash-table :test 'eq)) (candidates nil))
          (dolist (block (hir-function-blocks function))
            (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
            (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
              (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
              (dolist (input (hir-instruction-inputs instruction))
                (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
                (push instruction (gethash input uses)))
              (when (eq (hir-instruction-opcode instruction) :dynamic-rest-argument)
                (push instruction candidates))))
          (dolist (producer candidates)
            (let* ((root (hir-instruction-output producer)) (work (list (cons root nil)))
                   (seen (make-hash-table :test 'eq)) (cell nil) (calls nil) (valid t))
              (loop while (and work valid) do
                (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
                (let* ((entry (pop work)) (value (car entry)) (cellp (cdr entry)))
                  (unless (gethash value seen)
                    (setf (gethash value seen) t)
                    (unless (and (eq (hir-value-kind value) :single) (eq (hir-value-type value) t))
                      (setq valid nil))
                    (dolist (use (gethash value uses))
                      (when (minusp (decf fuel)) (return-from native-plan-dynamic-rests nil))
                      (let* ((opcode (hir-instruction-opcode use)) (inputs (hir-instruction-inputs use))
                             (output (hir-instruction-output use)))
                        (cond
                          ((and cellp (eq opcode :cell-read) (eq (first inputs) value))
                           (push (cons output nil) work))
                          ((and (not cellp) (eq opcode :copy) (eq (first inputs) value))
                           (push (cons output nil) work))
                          ((and (not cellp) (eq value root) (eq opcode :cell-new) (null cell)
                                (let ((binding (first (hir-instruction-attributes use))))
                                  (and (eq (hir-binding-owner binding) function)
                                       (eq (hir-binding-cell binding) output)
                                       ;; Exactly the existing direct-cell admission.
                                       (or (not (hir-binding-captured-p binding))
                                           (not (hir-binding-assigned-p binding))))))
                           (setq cell output) (push (cons output t) work))
                          ((and (not cellp) (eq opcode :call) (= (length inputs) 3)
                                (eq (third inputs) value) (not (member value (butlast inputs)))
                                (eq (gethash use *wasm-inline-simple-calls*) :length))
                           (push use calls))
                          (t (setq valid nil))))))))
              (when (and valid calls)
                (let ((index (first (hir-instruction-attributes producer))))
                  (setf (gethash producer plans) (cons index cell))
                  (dolist (call calls) (setf (gethash call plans) index)))))))))
    plans))

(defun native-low-field-ash-chain (ldb fuel)
  ;; An inexpensive producer walk also serves candidate collection. The
  ;; final plan checks the same path against the active reference lowering.
  (let ((consumer ldb) (value (fourth (hir-instruction-inputs ldb))) (uses nil))
    (dotimes (depth 32)
      (when (minusp (decf (car fuel))) (return nil))
      (let ((producer (hir-value-producer value)))
        (unless (%hir-instruction-p producer) (return nil))
        (push (cons value consumer) uses)
        (case (hir-instruction-opcode producer)
          ((:copy :primary) (setq consumer producer value (first (hir-instruction-inputs producer))))
          (:call (return (and (eq (gethash producer *wasm-inline-simple-calls*) :ash-general) uses)))
          (otherwise (return nil)))))))

(defun native-plan-low-field-ash-candidate (function ldb fuel)
  (when (and (plusp (car fuel))
             (null (cdr (hir-function-blocks function)))
             (or (null *wasm-low-field-ash-candidates*)
                 (not (gethash function *wasm-low-field-ash-candidates*)))
             (native-low-field-ash-chain ldb fuel)
             (eql (nth-value 2 (hir-literal-byte-field-chain
                                ldb function *wasm-optimization-barriers* fuel)) 0))
    (unless *wasm-low-field-ash-candidates*
      (setq *wasm-low-field-ash-candidates* (make-hash-table :test 'eq)))
    (setf (gethash function *wasm-low-field-ash-candidates*) ldb)))

(defun native-plan-inline-simple (unit &optional sealed-entries certificates)
  (declare (type hir-unit unit))
  ;; Each CASE arm names an exact standard CL operation. These bounded plans
  ;; reuse compact checked helpers or replace the ordinary argument packet;
  ;; NOTINLINE still vetoes expansion. Never classify a private builtin by ID.
  (let ((arithmetic nil) (list-accesses nil) (fuel 4096) (low-field-fuel (list 4096)))
    (declare (type (integer -1 4096) fuel))
    (dolist (function (hir-unit-functions unit))
      ;; Only inline TRUNCATE needs full multiple-value demand here. Compute
      ;; it on first use, then share the table within this function's plan.
      (let ((full nil)
            (arithmeticp (and *wasm-proven-integer-arithmetic*
                              (< (hir-function-debug-level function) 2)
                              (< (ash (hir-function-optimization-policy function) -4) 3)
                              (not (and *wasm-optimization-barriers*
                                        (gethash function *wasm-optimization-barriers*))))))
      (dolist (block (hir-function-blocks function))
        (let ((allocated nil))
        (dolist (instruction (hir-block-instructions block))
          ;; Share the ordinary traversal, including the old selection budget
          ;; for instructions in eligible functions. Exhaustion discards the
          ;; arithmetic/list candidates; ordinary and comparison plans continue.
          (when (and arithmeticp (not (minusp fuel)) (minusp (decf fuel)))
            (setq arithmetic nil list-accesses nil))
          (when (eq (hir-instruction-opcode instruction) :call)
            (let* ((inputs (hir-instruction-inputs instruction)) (reference (second inputs))
                   (producer (hir-value-producer reference))
                   (attributes (and (%hir-instruction-p producer)
                     (eq (hir-instruction-opcode producer) :function-reference)
                     (hir-instruction-attributes producer)))
                   (standard-name (native-standard-operation reference))
                   (name (and (not (eq (second attributes) 'notinline)) (first attributes)))
                   (kind (case name
                     ((1+ 1- + - * logand logior logxor lognot ash < > <= >= = /= zerop minusp plusp evenp oddp)
                      (or (and (zerop (hir-instruction-safety-level instruction))
                               (native-inline-integer-opcode name inputs))
                          ;; Canonical exact primitives can bypass function
                          ;; dispatch without claiming finite scalar operands.
                          ;; Their shared implementations retain every check
                          ;; and arbitrary-precision operation at all safeties.
                          (and (= (length inputs) 4)
                               (case name (ash :ash-general) (logand :logand-general)
                                 (< :less-real) (> :greater-real)
                                 (<= :not-greater-real) (>= :not-less-real)))
                          (and (= (length inputs) 3) (eq name 'zerop) :zero-number)))
                     (integer-length (and (= (length inputs) 3) :integer-length))
                     (characterp (and (= (length inputs) 3) :characterp))
                     (mod (and (= (length inputs) 4)
                       (or (and (zerop (hir-instruction-safety-level instruction))
                                (native-inline-nonnegative-division-opcode name inputs))
                           :mod-general)))
                     (truncate
                      (and (= (length inputs) 4)
                           (not (native-value-slot
                                  (or full (setq full (native-full-groups function)))
                                  (hir-instruction-output instruction)))
                           (or (and (zerop (hir-instruction-safety-level instruction))
                                    (native-inline-nonnegative-division-opcode name inputs))
                               :truncate-primary)))
                     ((car first cdr rest)
                      (and (= (length inputs) 3)
                           (if (and (zerop (hir-instruction-safety-level instruction))
                                    (native-list-value-p (third inputs)))
                               (if (hir-type-proves-p
                                     (if *wasm-inferred-types*
                                         (gethash (third inputs) *wasm-inferred-types* t)
                                         (hir-value-type (third inputs))) 'cons)
                                   (if (member name '(car first)) :car-cons :cdr-cons)
                                   (if (member name '(car first)) :car :cdr))
                               (if (member name '(car first)) :checked-car :checked-cdr))))
                     ((rplaca rplacd)
                      (and (= (length inputs) 4)
                           (zerop (hir-instruction-safety-level instruction))
                           (hir-type-proves-p
                             (if *wasm-inferred-types*
                                 (gethash (third inputs) *wasm-inferred-types* t)
                                 (hir-value-type (third inputs))) 'cons)
                           (if (eq name 'rplaca) :rplaca-cons :rplacd-cons)))
                     (eq (and (= (length inputs) 4) :eq))
                     (eql (and (= (length inputs) 4)
                       ;; Proven scalar integers have the same EQL identity
                       ;; exactly when their mathematical integer values agree.
                       (or (and (zerop (hir-instruction-safety-level instruction))
                                (native-inline-integer-opcode name inputs))
                       ;; EQL differs from EQ only for numbers and characters.
                       ;; A nonnumeric, noncharacter literal proves identity
                       ;; comparison sufficient, even when the other type is T.
                       (if (some (lambda (input)
                             (let ((producer (hir-value-producer input)))
                               (or (and (%hir-instruction-p producer)
                                    (eq (hir-instruction-opcode producer) :constant)
                                    (let ((literal (first (hir-instruction-attributes producer))))
                                      (not (or (numberp literal) (characterp literal)))))
                                   (and (zerop (hir-instruction-safety-level instruction))
                                        *wasm-inferred-types*
                                        (hir-type-proves-p (gethash input *wasm-inferred-types* t)
                                          '(not (or number character))))))) (cddr inputs))
                           :eq :eql))))
                     (cons (and (= (length inputs) 4) :cons))
                     ;; Canonical LIST needs only its fresh result spine.
                     ;; Large ordinary calls already emit that same spine as
                     ;; argument transport, so this never expands their code.
                     ;; HIR operands preserve source evaluation order.
                     (list :list)
                     ;; Reuse the transport spine emitter with the final
                     ;; operand as its tail. This allocates fewer conses than
                     ;; an ordinary call packet at every supported arity.
                     ;; Binary LIST* is CONS, including its field proofs.
                     (list* (and (cddr inputs)
                                 (case (length inputs)
                                   (3 :left) (4 :cons) (otherwise :list-star))))
                     (consp (and (= (length inputs) 3) :consp))
                     (atom (and (= (length inputs) 3) :atom))
                     (listp (and (= (length inputs) 3) :listp))
                     (functionp (and (= (length inputs) 3) :functionp))
                     (identity (and (= (length inputs) 3) :left))
                     (simple-vector-p (and (= (length inputs) 3) :simple-vector-p))
                     (vectorp (and (= (length inputs) 3) :vectorp))
                     (stringp (and (= (length inputs) 3) :stringp))
                     (symbolp (and (= (length inputs) 3) :symbolp))
                     (integerp (and (= (length inputs) 3) :integerp))
                     (char (and (= (length inputs) 4) :character-ref))
                     (char-code (and (= (length inputs) 3) :character-code))
                     (code-char (and (= (length inputs) 3) :code-character))
                     (make-string
                      (and (= (length inputs) 3) (native-integer-width (third inputs))
                           :make-string))
                     (list-length (and (= (length inputs) 3) :list-length))
                     (make-array
                      (and (member (length inputs) '(3 5))
                           (native-integer-width (third inputs))
                        (cond
                          ((= (length inputs) 3) :make-simple-vector)
                          ((= (length inputs) 5)
                           (let* ((key (hir-value-producer (fourth inputs)))
                                  (initial (hir-value-producer (fifth inputs))))
                             (when (and (%hir-instruction-p key)
                                        (eq (hir-instruction-opcode key) :constant))
                               (case (first (hir-instruction-attributes key))
                                 (:initial-element :make-simple-vector)
                                 (:element-type
                                  (and (%hir-instruction-p initial)
                                       (eq (hir-instruction-opcode initial) :constant)
                                       (eq (first (hir-instruction-attributes initial)) 'character)
                                       :make-character-vector)))))))))
                     ;; Checked shared array reads retain general element kinds
                     ;; and physical capacity. Only one-subscript SBIT has the
                     ;; rank-one contract used by this compact entry.
                     (sbit (and (= (length inputs) 4) :sbit-ref))
                     (row-major-aref (and (= (length inputs) 4) :row-major-ref))
                     (gethash (and (member (length inputs) '(4 5)) :gethash))
                     (length (and (= (length inputs) 3)
                       (if (and (zerop (hir-instruction-safety-level instruction))
                                (native-simple-vector-value-p (third inputs)))
                           :vector-length :length)))
                     ((null not) (and (= (length inputs) 3) :null))
                     (otherwise (and (equal name '(setf gethash))
                                     (member (length inputs) '(5 6)) :puthash)))))
              ;; These are compact checked calls, including under NOTINLINE.
              ;; The shared worker preserves NIL and checks each intermediate
              ;; CAR/CDR rather than expanding every composite path in callers.
              (when (and (null kind) (null *wasm-fixed-call-emitter*) (= (length inputs) 3)
                         (standard-function-list-path standard-name))
                (setq kind :list-path))
              (when (and (null kind) (null *wasm-fixed-call-emitter*)
                         (native-search-list-code standard-name inputs))
                (setq kind :search-list))
              ;; Exact one-value standard operations can use a checked fixed
              ;; entry without carrying a global function lookup. No purity,
              ;; valid-argument or fixed-method claim follows from this plan.
              (when (and (null kind) (null *wasm-fixed-call-emitter*) (<= 2 (length inputs) 6)
                         (native-standard-function-name-p standard-name)
                         (eql (standard-function-result-count standard-name (- (length inputs) 2)) 1))
                (setq kind :standard-call))
              (when (and (equal name '(setf char)) (= (length inputs) 5))
                (setq kind
                  (if (and (zerop (hir-instruction-safety-level instruction))
                           (native-integer-width (fifth inputs))
                           (hir-type-proves-p
                             (if *wasm-inferred-types* (gethash (fourth inputs) *wasm-inferred-types* t)
                                 (hir-value-type (fourth inputs))) 'simple-string)
                           (hir-type-proves-p
                             (if *wasm-inferred-types* (gethash (third inputs) *wasm-inferred-types* t)
                                 (hir-value-type (third inputs))) 'character))
                      :simple-character-set :character-set)))
              ;; Keep the existing proven simple-vector store when available.
              ;; All other canonical ROW-MAJOR-AREF stores use the checked
              ;; physical array operation, including at nonzero SAFETY.
              (when (and (equal name '(setf row-major-aref)) (= (length inputs) 5)
                         (not (gethash instruction *wasm-inline-vector-calls*)))
                (setq kind :row-major-set))
              (when (and *wasm-inferred-types* (zerop (hir-instruction-safety-level instruction))
                         (member name '(consp atom listp functionp simple-vector-p vectorp stringp symbolp integerp characterp null not eq eql = /= < <= > >=)))
                (let ((type (gethash (hir-instruction-output instruction) *wasm-inferred-types* t)))
                  (cond ((equal type '(eql t)) (setq kind :true))
                        ((eq type 'null) (setq kind :false)))))
              (when kind
                    (when (and arithmeticp (not (minusp fuel))
                               (= (hir-instruction-safety-level instruction) 1)
                               (or (and (eq kind :standard-call)
                                        (or (and (= (length inputs) 4) (member name '(+ - *)))
                                            (and (= (length inputs) 3) (member name '(1+ 1-)))))
                                   (and (eq kind :logand-general) (eq name 'logand))))
                      ;; NAME already requires a direct canonical reference
                      ;; with expansion permission, including NOTINLINE.
                      (push (cons instruction name) arithmetic))
                    (when (and arithmeticp (not (minusp fuel))
                               (= (hir-instruction-safety-level instruction) 1)
                               (= (length inputs) 3)
                               (member name '(car first cdr rest endp))
                               *wasm-inferred-types*
                               (hir-type-proves-p (gethash (third inputs) *wasm-inferred-types* t) 'list))
                      ;; Mixed facts only select roots. The shared executable
                      ;; query below independently certifies every operand.
                      (push (cons instruction name) list-accesses))
                    (when (member kind '(:cons :list :list-star)) (setq allocated t))
                    (when (and allocated *wasm-cons-forward-blocks*
                               (member kind '(:car :cdr :car-cons :cdr-cons :checked-car :checked-cdr)))
                      (setf (gethash block *wasm-cons-forward-blocks*) t))
                    (compiler-statistic :planning/inline-primitive kind)
                    (setf (gethash instruction *wasm-inline-simple-calls*) kind)
                    (when (and *wasm-snapshot-mask-functions* (eq kind :logand-general))
                      ;; Direct entries can copy the function record. The
                      ;; copies still share the same HIR entry block.
                      (setf (gethash (hir-function-entry function) *wasm-snapshot-mask-functions*) t))
                    (when (and (eq kind :standard-call) (eq standard-name 'ldb)
                               (cdddr inputs) (null (cddddr inputs))
                               *wasm-optimization-barriers* (not *wasm-closed-worker-functions*))
                      (when (null (cdr (hir-function-blocks function)))
                        (unless *wasm-literal-byte-extract-owners*
                          (setq *wasm-literal-byte-extract-owners* (make-hash-table :test 'eq)))
                        (setf (gethash function *wasm-literal-byte-extract-owners*) t))
                      (native-plan-low-field-ash-candidate function instruction low-field-fuel))
                    ;; Only an immediate canonical reference can disappear.
                    ;; Computed designators retain their evaluation and identity.
                    (when attributes
                      (setf (gethash reference *wasm-inline-references*) t))))))))))
  (native-plan-proven-integers unit arithmetic list-accesses sealed-entries certificates)
  (native-plan-character-code-fusion unit)
  *wasm-inline-simple-calls*))

(defun native-cell-storage-roots (unit)
  "Bound the extra executable query for private SAFETY 1 integer storage."
  (let ((cells (make-hash-table :test 'eq)) (roots nil) (fuel 4096))
    ;; Count operands as well as instructions before the second graph scan.
    ;; Exhaustion supplies no storage roots, including partially found cells.
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (when (minusp (decf fuel)) (return-from native-cell-storage-roots nil))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel)) (return-from native-cell-storage-roots nil))
          (dolist (input (hir-instruction-inputs instruction))
            (declare (ignore input))
            (when (minusp (decf fuel)) (return-from native-cell-storage-roots nil)))
          (when (and (eq (hir-instruction-opcode instruction) :cell-new)
                     (= (hir-instruction-safety-level instruction) 1)
                     (< (hir-function-debug-level function) 2)
                     *wasm-optimization-barriers*
                     (not (gethash function *wasm-optimization-barriers*))
                     (not (gethash (hir-instruction-output instruction) *wasm-optimization-barriers*))
                     (native-compute-integer-width (first (hir-instruction-inputs instruction))))
            ;; Mixed facts only select roots. The query proves each value anew.
            (setf (gethash (hir-instruction-output instruction) cells) t)
            (push (first (hir-instruction-inputs instruction)) roots)))))
    (when (zerop (hash-table-count cells)) (return-from native-cell-storage-roots nil))
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (and (eq (hir-instruction-opcode instruction) :cell-write)
                     (gethash (first (hir-instruction-inputs instruction)) cells))
            ;; Include every actual store, even when its mixed type is unknown.
            (push (second (hir-instruction-inputs instruction)) roots)))))
    roots))

(defun native-plan-checked-integer-copies (comparisons arithmetic)
  "Select declared snapshots for checked scalar lowering, without moving them."
  (let ((copies nil) (fuel 4096))
    (dolist (comparison comparisons)
      (dolist (operand (cddr (hir-instruction-inputs comparison)))
        (loop repeat 32 do
          (when (minusp (decf fuel)) (return-from native-plan-checked-integer-copies nil))
          (let ((producer (hir-value-producer operand)))
            (unless (and (%hir-instruction-p producer)
                         (eq (hir-instruction-opcode producer) :copy)) (return))
            (let* ((input (first (hir-instruction-inputs producer)))
                   (origin (hir-value-producer input))
                   (function (hir-value-owner operand)))
              (when (and (= (hir-instruction-safety-level producer) 1)
                         (%hir-instruction-p origin) (eq (hir-instruction-opcode origin) :argument)
                         (null (hir-block-predecessors (hir-function-entry function)))
                         (member producer (hir-block-instructions (hir-function-entry function)) :test #'eq)
                         (not (gethash function *wasm-optimization-barriers*))
                         (hir-value-type operand) (hir-type-proves-p (hir-value-type operand) 'fixnum)
                         (some (lambda (entry)
                                 (eq function (hir-value-owner (hir-instruction-output (car entry))))) arithmetic))
                (unless copies (setq copies (make-hash-table :test 'eq)))
                (setf (gethash operand copies) comparison))
              (setq operand input))))))
    copies))

(defun native-plan-proven-integers (unit candidates &optional list-accesses sealed-entries certificates)
  "Share one executable-fact query between integer and list-access plans."
  (let* ((cell-roots (native-cell-storage-roots unit))
         (arithmetic nil) (comparisons nil) (roots cell-roots))
    (dolist (entry (nreverse candidates))
      (let* ((instruction (car entry)) (name (cdr entry))
             (operands (cddr (hir-instruction-inputs instruction)))
             (types (and *wasm-inferred-types*
                         (mapcar (lambda (operand) (gethash operand *wasm-inferred-types* t)) operands))))
        ;; Mixed facts only reject candidates. Reject unknown operand widths
        ;; before deriving a result, so untyped calls need no interval work.
        (when (and types
                   (every (lambda (type)
                            (let ((width (native-type-integer-width type)))
                              (and width (<= width 64)))) types)
                   (let ((width (native-type-integer-width (hir-inline-result-type name types))))
                     (and width (<= width 64))))
          (push entry arithmetic)
          (dolist (operand operands) (push operand roots)))))
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (and (= (hir-instruction-safety-level instruction) 1)
                     (member (gethash instruction *wasm-inline-simple-calls*)
                       '(:less-real :greater-real :not-greater-real :not-less-real))
                     (< (hir-function-debug-level function) 2))
            (push instruction comparisons)
            (dolist (operand (cddr (hir-instruction-inputs instruction))) (push operand roots))))))
    (dolist (entry list-accesses)
      (push (third (hir-instruction-inputs (car entry))) roots))
    (when roots
      ;; These maps describe the current owned HIR, independently of inferred
      ;; types and declarations. Only a bounded audit of source-owned sealed
      ;; closures can narrow executable ingress. Facts stay in this emission.
      (let* ((copies (native-plan-checked-integer-copies comparisons arithmetic))
             (guards *wasm-type-guards*)
             (entries (or sealed-entries (hir-unit-functions unit)))
             (proven nil))
        (when copies
          (setq guards (make-hash-table :test 'eq))
          (maphash (lambda (value premises) (setf (gethash value guards) premises)) *wasm-type-guards*)
          (maphash (lambda (value dependent)
                     (declare (ignore dependent))
                     (push (cons value 'fixnum) (gethash value guards))) copies))
        (setq proven (infer-hir-types-checked unit roots t entries t
                       *wasm-result-callees* *hir-standard-operations* guards certificates
                       *wasm-optimization-barriers*))
        ;; Speculation becomes authority only alongside actual bounded scalar
        ;; consumers. Drop all seeds and recompute before publishing any plan
        ;; if a check lacks a dependent comparison or a bounded arithmetic
        ;; consumer in its function.
        (when (and copies
                   (loop for value being the hash-keys of copies using (hash-value dependent)
                     thereis (not (and
                       (every (lambda (operand)
                                (let ((width (native-type-integer-width (gethash operand proven t))))
                                  (and width (<= width 64))))
                         (cddr (hir-instruction-inputs dependent)))
                       (some (lambda (entry)
                               (and (eq (hir-value-owner value)
                                        (hir-value-owner (hir-instruction-output (car entry))))
                                    (let* ((types (mapcar (lambda (operand) (gethash operand proven t))
                                                    (cddr (hir-instruction-inputs (car entry)))))
                                           (width (native-type-integer-width (hir-inline-result-type (cdr entry) types))))
                                      (and width (<= width 64)
                                           (every (lambda (type)
                                                    (let ((size (native-type-integer-width type)))
                                                      (and size (<= size 64)))) types))))) arithmetic)))))
          (setq copies nil
                proven (infer-hir-types-checked unit roots t entries t
                         *wasm-result-callees* *hir-standard-operations* *wasm-type-guards* certificates
                         *wasm-optimization-barriers*)))
        (setq *wasm-checked-integer-copies* copies)
        (setq *wasm-proven-cell-types* (and cell-roots proven))
        (native-plan-proven-integer-arithmetic arithmetic proven)
        (native-plan-proven-integer-comparisons comparisons proven)
        (dolist (entry list-accesses)
          (let* ((instruction (car entry)) (name (cdr entry))
                 (type (gethash (third (hir-instruction-inputs instruction)) proven t)))
            (when (and (hir-standard-property-negative-p name
                         (cddr (hir-instruction-inputs instruction)) :may-signal proven)
                       ;; ENDP's discarded body is removable only when the
                       ;; independent effect/allocation obligations hold too.
                       (or (not (eq name 'endp))
                           (every (lambda (property)
                                    (hir-standard-property-negative-p name
                                      (cddr (hir-instruction-inputs instruction)) property proven))
                             '(:reads :writes :callbacks :allocation))))
              (compiler-statistic :proof/list-access name)
              (setf (gethash instruction *wasm-inline-simple-calls*)
                (cond ((eq name 'endp) :endp-list)
                      ((hir-type-proves-p type 'cons)
                       (if (member name '(car first)) :car-cons :cdr-cons))
                      (t (if (member name '(car first)) :car :cdr)))))))))))

(defun native-plan-proven-integer-arithmetic (pending proven)
  "Choose exact scalar arithmetic from executable operand facts at SAFETY 1."
  ;; Apply the same mixed operand-width classification as the ordinary
  ;; integer call emitter. A caller's function-local width cache is unrelated.
  (let ((*wasm-integer-widths* nil))
    (dolist (entry pending)
      (let* ((instruction (car entry)) (name (cdr entry))
             (operands (cddr (hir-instruction-inputs instruction)))
             (types (mapcar (lambda (operand) (gethash operand proven t)) operands))
             (widths (mapcar #'native-type-integer-width types))
             ;; A result assertion is never evidence of nonoverflow.
             (width (native-type-integer-width (hir-inline-result-type name types))))
        (when (and width (<= width 64)
                   (eql (standard-function-result-count name (length operands)) 1)
                   (every (lambda (size) (and size (<= size 64))) widths))
          (let* ((opcode (ecase name ((+ 1+) #x7c) ((- 1-) #x7d) (* #x7e) (logand #x83)))
                 (narrow (and (member name '(+ - 1+ 1-)) (<= width 32)
                              (every (lambda (size) (<= size 32)) widths)))
                 ;; Direct LOGAND and typed multiply do not poll. The
                 ;; generic multiply fallback and existing add/subtract
                 ;; proof plans retain their operation-entry checkpoints.
                 (poll (case name (logand nil) (* (not (every #'native-integer-width operands))) (t t))))
            (compiler-statistic (if narrow :proof/integer-arithmetic-i32 :proof/integer-arithmetic-i64) name)
            (setf (gethash instruction *wasm-proven-integer-arithmetic*) (vector widths width name poll narrow)
                  (gethash instruction *wasm-inline-simple-calls*) opcode)))))))

(defun emit-native-proven-integer-arithmetic (inputs opcode proof &optional raw)
  (let ((narrow (svref proof 4)))
    (compiler-statistic (if narrow :optimization/integer-arithmetic-i32
                            :optimization/integer-arithmetic-i64) (svref proof 2))
    ;; Operand and result intervals independently prove the operation exact.
    ;; Keep the narrow operation for i32 proofs and use the existing raw i64
    ;; snapshot ABI for wider proofs. The scheduling point remains in place.
    (when (svref proof 3) (wasm-call "native-poll"))
    (loop for operand in (cddr inputs) for width in (svref proof 0) do
      (let* ((producer (hir-value-producer operand))
             (literal (and (%hir-instruction-p producer)
                           (eq (hir-instruction-opcode producer) :constant)
                           (first (hir-instruction-attributes producer)))))
        (cond ((integerp literal)
               (if narrow (wasm-integer literal)
                   (progn (wasm-byte #x42) (wasm-s64 literal))))
              (t (emit-native-unboxed-integer operand width)
                 (when narrow (wasm-byte #xa7))))))
    (when (= (length inputs) 3)
      (if narrow (wasm-integer 1) (progn (wasm-byte #x42) (wasm-s32 1))))
    (wasm-byte (if narrow (if (= opcode #x7c) #x6a #x6b) opcode))
    (when narrow (wasm-byte #xac))
    (unless raw (wasm-call "native-int64"))))

(defun native-plan-proven-integer-comparisons (pending proven)
  ;; Only exact binary ordered comparisons at SAFETY 1 enter
  ;; this path; the ordinary planner already honored NOTINLINE. Declarations
  ;; alone cannot justify either operand's extraction.
  (dolist (instruction pending)
    ;; Executed checks and flow establish logical scalar integers. Declared
    ;; ranges and a producer's historical opcode alone supply no authority.
    (when (and (hir-standard-property-negative-p (hir-inline-primitive instruction)
                  (cddr (hir-instruction-inputs instruction)) :may-signal proven)
                (every (lambda (operand)
                         (multiple-value-bind (low high bounded)
                             (hir-integer-bounds (gethash operand proven t))
                           (and bounded (<= -9223372036854775808 low high 9223372036854775807))))
                  (cddr (hir-instruction-inputs instruction))))
      (let ((kind (case (gethash instruction *wasm-inline-simple-calls*)
                    (:less-real :proven-less-int64) (:greater-real :proven-greater-int64)
                    (:not-greater-real :proven-not-greater-int64)
                    (:not-less-real :proven-not-less-int64))))
        (compiler-statistic :proof/integer-comparison-checked-origin kind)
        (setf (gethash instruction *wasm-inline-simple-calls*) kind)))))

(defun emit-native-proven-integer-comparison (inputs kind &optional raw)
  ;; Read accepted raw SSA snapshots directly. Other operands retain the full
  ;; scalar extractor: a range alone never licenses an i31 representation cast.
  ;; Producers have already run in source order, including all their checks.
  (dolist (operand (cddr inputs))
    (let ((record (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* operand))))
      (if (and record (eq (svref record 4) :checked-copy))
          (wasm-local (svref record 0))
          (multiple-value-bind (literal known) (hir-primary-constant operand)
            (if (and known (integerp literal) (<= -9223372036854775808 literal 9223372036854775807))
                (progn (wasm-byte #x42) (wasm-s64 literal))
                (emit-native-unboxed-integer operand 64))))))
  (wasm-byte (case kind (:proven-less-int64 #x53) (:proven-greater-int64 #x55)
                       (:proven-not-greater-int64 #x57) (:proven-not-less-int64 #x59)))
  (unless raw (wasm-call "native-boolean")))

(defun native-plan-character-code-fusion (unit)
  "Eliminate an unobserved canonical CHAR object without moving its checked read."
  (declare (type hir-unit unit))
  (dolist (function (hir-unit-functions unit))
    ;; Debugger-visible lexical values keep their source representation.
    (when (and (< (hir-function-debug-level function) 2)
               (some (lambda (block)
                       (some (lambda (instruction)
                               (eq (gethash instruction *wasm-inline-simple-calls*) :character-code))
                             (hir-block-instructions block)))
                     (hir-function-blocks function)))
      (let ((uses (native-value-table function 0)) (owners (make-hash-table :test 'eq)))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
            (setf (gethash instruction owners) block)
            (dolist (input (hir-instruction-inputs instruction))
              (incf (native-value-slot uses input)))))
        (dolist (block (hir-function-blocks function))
          (dolist (consumer (hir-block-instructions block))
            (when (eq (gethash consumer *wasm-inline-simple-calls*) :character-code)
              (let ((value (third (hir-instruction-inputs consumer))))
                ;; Use counts cover every instruction, terminator and capture.
                ;; Restrict aliases to one basic block and bound planner work.
                (loop repeat 32 do
                  (unless (= (native-value-slot uses value) 1) (return))
                  (let ((producer (hir-value-producer value)))
                    (unless (and (%hir-instruction-p producer) (eq (gethash producer owners) block))
                      (return))
                    (cond
                      ((and (member (hir-instruction-opcode producer) '(:copy :primary))
                            (= (length (hir-instruction-inputs producer)) 1))
                       (setq value (first (hir-instruction-inputs producer))))
                      ((eq (gethash producer *wasm-inline-simple-calls*) :character-ref)
                       ;; The CHAR read, checks and errors stay at their original
                       ;; instruction. Only its unobserved character allocation
                       ;; disappears; CHAR-CODE later forwards the saved code.
                       (setf (gethash producer *wasm-inline-simple-calls*) :character-code-at
                             (gethash consumer *wasm-inline-simple-calls*) :left)
                       (compiler-statistic :optimization/character-code-fusion)
                       (return))
                      (t (return)))))))))))))

(defun native-keep-shared-references (unit)
  (declare (type hir-unit unit))
  ;; An INLINE call can share its function-valued operand with an ordinary
  ;; call or a value consumer in client-supplied HIR. Keep that lookup alive.
  (dolist (function (hir-unit-functions unit))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (loop for input in (hir-instruction-inputs instruction) for index from 0 do
          (when (and (gethash input *wasm-inline-references*)
                     (not (and (= index 1) (eq (hir-instruction-opcode instruction) :call)
                               (or (gethash instruction *wasm-inline-vector-calls*)
                                   (gethash instruction *wasm-inline-simple-calls*)))))
            (remhash input *wasm-inline-references*)))))))

(defun native-plan-inline-vectors (unit)
  (declare (type hir-unit unit))
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (and (eq (hir-instruction-opcode instruction) :call)
                     (zerop (hir-instruction-safety-level instruction)))
            (let* ((inputs (hir-instruction-inputs instruction)) (reference (second inputs))
                   (producer (hir-value-producer reference))
                   (name (and (%hir-instruction-p producer)
                              (eq (hir-instruction-opcode producer) :function-reference)
                              (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                              (first (hir-instruction-attributes producer))))
                   (setter (and (consp name) (eq (car name) 'setf) (= (length name) 2)))
                   (operator (if setter (second name) name))
                   (operands (if setter (cdddr inputs) (cddr inputs))))
              (when (and (zerop (hir-instruction-safety-level instruction))
                         (= (length inputs) (if setter 5 4))
                         (member operator '(svref aref row-major-aref))
                         (native-simple-vector-value-p (first operands))
                         (native-integer-width (second operands)))
                    (compiler-statistic :planning/inline-vector operator)
                    (setf (gethash instruction *wasm-inline-vector-calls*) t
                          (gethash reference *wasm-inline-references*) t)))))))
    *wasm-inline-vector-calls*)

(defun emit-native-inline-vector (inputs)
  (declare (type list inputs))
  (compiler-statistic :optimization/inline-vector (if (= (length inputs) 5) :write :read))
  (let* ((setter (= (length inputs) 5))
         (operands (if setter (cdddr inputs) (cddr inputs))))
    (when setter (wasm-get (third inputs)))
    (wasm-get (first operands))
    (let ((index (second operands)))
      (emit-native-unboxed-integer index (native-integer-width index)))
    (wasm-call (if setter "native-vector-set-inline" "native-vector-ref-inline"))))

(defun native-tail-forward-p (block)
  (declare (type simple-vector block))
  ;; Follow value-preserving, effect-free joins to a full multiple-value return.
  (let ((seen nil))
    (loop
      (when (or (member block seen :test #'eq) (hir-block-instructions block)) (return nil))
      (push block seen)
      (let* ((parameter (first (hir-block-parameters block)))
             (terminator (hir-block-terminator block)))
        (unless (and parameter (eq parameter
                   (if (eq (hir-instruction-opcode terminator) :block-return)
                       (third (hir-instruction-inputs terminator))
                       (first (hir-instruction-inputs terminator))))) (return nil))
        (case (hir-instruction-opcode terminator)
          (:return (return t))
          ((:jump :block-return)
           (unless (hir-instruction-attributes terminator) (return nil))
           (setq block (first (hir-instruction-attributes terminator))))
          (otherwise (return nil)))))))

(defun native-tail-environment-p (environment function)
  (declare (type simple-vector environment function))
  ;; A binding or captured exit in another branch must not inhibit this
  ;; call. Uncaptured BLOCKs add HIR tokens but no runtime dynamic extent.
  (loop
    (when (eq environment (hir-function-environment function)) (return t))
    (let ((producer (hir-value-producer environment)))
      (unless (and (%hir-instruction-p producer)
                   (eq (hir-instruction-opcode producer) :block-environment)
                   (not (hir-target-captured-p (first (hir-instruction-attributes producer)))))
        (return nil))
      (let ((token (hir-value-producer (first (hir-instruction-inputs producer)))))
        (unless (and (%hir-instruction-p token) (eq (hir-instruction-opcode token) :block-enter))
          (return nil))
        (setq environment (first (hir-instruction-inputs token)))))))

(defun native-tail-calls (function)
  (declare (type simple-vector function))
  (let ((calls nil))
    (when (< (hir-function-debug-level function) 2)
      (dolist (block (hir-function-blocks function))
        (let* ((instruction (car (last (hir-block-instructions block))))
               (terminator (hir-block-terminator block))
               (output (and instruction (hir-instruction-output instruction))))
          (when (and instruction (not (gethash instruction *wasm-inline-vector-calls*))
                  (member (hir-instruction-opcode instruction) '(:call :multiple-value-call))
                  (native-tail-environment-p (first (hir-instruction-inputs instruction)) function)
                  (or (not (native-value-slot *wasm-single-calls* output))
                      (native-named-single-call-p instruction function))
                  (eq output (if (eq (hir-instruction-opcode terminator) :block-return)
                                 (third (hir-instruction-inputs terminator))
                                 (first (hir-instruction-inputs terminator))))
                  (case (hir-instruction-opcode terminator)
                    (:return t)
                    ((:jump :block-return)
                     (and (hir-instruction-attributes terminator)
                          (native-tail-forward-p (first (hir-instruction-attributes terminator)))))))
            (push instruction calls)))))
    calls))

(defun native-known-callee (value)
  (declare (type simple-vector value))
  (hir-result-callee value *wasm-result-callees*))

(defun emit-native-tail-builtin (inputs safety)
  ;; These exact standard functions return one value. Preserve the tail-call
  ;; checkpoint, frame setup and value cleanup without a generic second arm.
  (let* ((function (second inputs)) (producer (hir-value-producer function))
           (name (and (%hir-instruction-p producer)
                      (eq (hir-instruction-opcode producer) :function-reference)
                      (first (hir-instruction-attributes producer))))
           (arity (- (length inputs) 2))
           (eligible (eql (standard-function-result-count name arity) 1)))
      (when eligible
        (wasm-get function) (wasm-call "native-tail-enter") (wasm-byte #x1a)
        (unless (or (emit-native-integer-call inputs nil safety) (emit-native-list-call inputs safety))
          (if (<= arity 4)
              (emit-native-fixed-call inputs)
              ;; The primary adapter uses active dynamic state; its legacy
              ;; environment slot is ignored, as for other scalar calls.
              (progn (wasm-null) (wasm-get function) (wasm-arguments (cddr inputs))
                     (wasm-call "native-primary-call"))))
        (wasm-call "native-return1") (wasm-byte #x0f)
        t)))

(defun emit-native-tail-call (inputs &optional (safety 1))
  (when (emit-native-tail-builtin inputs safety)
    (return-from emit-native-tail-call nil))
  (let ((target (if (and *wasm-named-call-plan* (gethash (second inputs) *wasm-named-call-plan*)) *wasm-current-function*
                    (native-known-callee (second inputs)))))
    (when (and target (< (hir-function-debug-level target) 2))
      ;; Lexical function bindings are immutable. Their known entry can be a
      ;; direct RETURN_CALL while retaining the actual closure as its context.
      ;; Debugger evaluation keeps the runtime dispatch path.
      (when *wasm-debug*
        (wasm-get (second inputs)) (wasm-call "native-tail-direct")
        (wasm-byte #x04) (wasm-byte #x40))
      (wasm-get (second inputs))
      ;; A module with no debug descriptor never wraps its lexical closures
      ;; as debug entries. Their context can pass straight through setup.
      (unless *wasm-debug* (wasm-call "native-tail-enter"))
      (let* ((worker (gethash target *wasm-direct-workers*))
             (direct (and worker (= (length (cddr inputs)) (native-fixed-arity target))))
             (entry (if direct (or (gethash target *wasm-boxed-result-workers*) worker) target)))
        ;; Immutable lexical bindings and the verified fixed arity establish
        ;; the worker's calling contract without a list or checked adapter.
        (if direct
            (let ((usage (native-call-usage inputs)))
              (dotimes (i 4)
                (if (and (< i (length (cddr inputs))) (or (null usage) (logbitp i usage)))
                    (wasm-get (nth (+ i 2) inputs)) (wasm-null))))
            (wasm-arguments (cddr inputs)))
        (wasm-byte #x12)
        (when *wasm-function-relocations*
          (push (list (length *wasm-bytes*) entry)
                (gethash *wasm-current-function* *wasm-function-relocations*)))
        (wasm-u32 (+ (length *native-imports*) (gethash entry *native-function-indices*))))
      (if *wasm-debug* (wasm-byte #x0b) (return-from emit-native-tail-call nil))))
  ;; Small calls share a guarded runtime helper. The ordinary adapter only
  ;; allocates a packet when the selected target cannot use its fixed worker.
  (let ((count (length (cddr inputs))))
    (if (<= count 4)
        (emit-native-call-operands inputs)
        (progn (wasm-get (second inputs)) (wasm-arguments (cddr inputs))))
    (let ((start (length *wasm-bytes*)))
      (wasm-call (if (<= count 4) (format nil "native-tail-call~D" count) "native-tail-call"))
      (setf (aref *wasm-bytes* start) #x12))))

(defun native-fixed-arity (function)
  (dolist (instruction (hir-block-instructions (hir-function-entry function)))
    (when (eq (hir-instruction-opcode instruction) :check-arguments)
      (let ((limits (hir-instruction-attributes instruction)))
        (return-from native-fixed-arity
          (and (= (first limits) (second limits)) (<= 0 (first limits) 4) (first limits)))))))

(defun native-immediate-integer-result-p (record)
  (member (gethash (hir-value-producer (svref record 1)) *wasm-inline-simple-calls*)
          '(:character-code-at :character-code)))

(defun native-raw-aref-index (instruction inputs kind table &optional immediate)
  ;; Match the boxed-use scan and emitter. The raw call can reconstruct an
  ;; arithmetic or cell snapshot using its i64 box on a miss.
  (when (and table (eq kind :standard-call) (= (length inputs) 4))
    (let ((record (native-value-slot table (fourth inputs)))
          (producer (hir-value-producer (second inputs))))
      (when (and record (eq (svref record 4) :result)
                 (%hir-instruction-p producer)
                 (eq (hir-instruction-opcode producer) :function-reference)
                 (eq (first (hir-instruction-attributes producer)) 'aref)
                 (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                 (or immediate (not (native-immediate-integer-result-p record)))
                 (not (native-vector-call-width inputs (hir-instruction-safety-level instruction))))
        record))))

(defun native-scalar-call-name (inputs)
  (let ((reference (hir-value-producer (second inputs))))
    (when (and (%hir-instruction-p reference)
               (eq (hir-instruction-opcode reference) :function-reference))
      (let* ((attributes (hir-instruction-attributes reference))
             (name (first attributes)))
        (unless (or (eq (second attributes) 'notinline)
                    (and (null (second attributes))
                         (boundp 'miricl::%global-inline-policy)
                         miricl::%global-inline-policy
                         (eq (gethash name miricl::%global-inline-policy) 'notinline)))
          name)))))

(defun native-raw-array-store (instruction table)
  ;; This is a transport change to the existing fixed primary call. Keep
  ;; the captured setter, including when the live binding is a user function.
  (when (and table (not *wasm-fixed-call-emitter*) (not *wasm-closed-worker-functions*)
             (eq (hir-instruction-opcode instruction) :call))
    (let ((inputs (hir-instruction-inputs instruction))
          (output (hir-instruction-output instruction)))
      (when (and (= (length inputs) 5)
                 (not (native-value-slot *wasm-live-values* output))
                 (native-value-slot *wasm-single-calls* output)
                 (not (member instruction *wasm-tail-calls* :test #'eq))
                 (not (gethash instruction *wasm-inline-simple-calls*))
                 (not (gethash instruction *wasm-inline-vector-calls*))
                 (equal (native-scalar-call-name inputs) '(setf aref)))
        (let ((record (native-value-slot table (third inputs))))
          (when (and record (eq (svref record 4) :result))
            record))))))

(defun emit-native-snapshot-arithmetic (plan)
  (destructuring-bind (opcode poll shift sources) plan
    (when poll (wasm-call "native-poll"))
    (dolist (source sources)
      (if (integerp source)
          (progn (wasm-byte #x42) (wasm-s64 source))
          (wasm-local (svref source 0))))
    (when shift (wasm-byte #x42) (wasm-s32 shift))
    (wasm-byte opcode)))

(defun emit-native-snapshot-mask (inputs plan)
  (let ((snapshot (first plan)) (mask (second plan)))
    (when snapshot
      (wasm-local (svref snapshot 6))
      (wasm-byte #x04) (wasm-type :i64)
      (wasm-local (svref snapshot 0))
      (wasm-byte #x42) (wasm-s64 mask) (wasm-byte #x83)
      (wasm-byte #x05))
    ;; The boxed arm includes NIL and arbitrary precision integers. The
    ;; ordinary operation checks the original operands in their original order.
    (dolist (input (cddr inputs)) (wasm-get input))
    (wasm-call "native-logand-general-inline")
    (wasm-call "native-unbox-int64")
    (when snapshot (wasm-byte #x0b))))

(defvar *wasm-float-pairs* nil)

(defun emit-native-retained-scalar-call (inputs kind)
  (dolist (input (cdr inputs)) (wasm-get input))
  (if (and (= kind 1) (= (length inputs) 4))
      (wasm-call "native-scalar-call2")
      (progn
        (loop repeat (- 6 (length inputs)) do (wasm-null))
        (wasm-integer (- (length inputs) 2)) (wasm-integer kind)
        (wasm-call "native-scalar-call"))))

(defun native-scalar-argument-index (value &optional kind)
  (let* ((origin (native-predicate-source value *wasm-reused-values*))
         (producer (hir-value-producer origin)))
    (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :argument))
      (let ((index (first (hir-instruction-attributes producer))))
        (when (or (null kind)
                  (let ((actual (ldb (byte 3 (* 3 index)) *wasm-reusable-scalar-arguments*)))
                    (if (eq kind :number) (member actual '(1 3 4 5 6)) (= kind actual))))
          index)))))

(defun emit-native-scalar-argument (index)
  ;; These pseudo-local reads exist only in the unpublished packet body.
  ;; The direct rewrite moves them into the scalar entry's parameter slots.
  (if index (wasm-local (+ #x10000000 index))
      (progn (wasm-byte #x42) (wasm-s64 0))))

(defun emit-native-scalar-argument-status (index bit)
  (if index
      (progn
        (wasm-local #x10000004)
        (wasm-integer (ash 1 index)) (wasm-byte #x71)
        (unless (= bit index)
          (wasm-integer 0) (wasm-byte #x47)
          (when (plusp bit) (wasm-integer (ash 1 bit)) (wasm-byte #x6c))))
      (wasm-integer 0)))

(defun emit-native-scalar-pair (instruction inputs output record)
  (let* ((producer (eq output (svref record 1))) (kind (svref record 2))
         (transport (and producer *wasm-reusable-scalar-arguments* (>= kind 3)))
         (left (and transport (native-scalar-argument-index (third inputs) :number)))
         (right (and transport (native-scalar-argument-index (fourth inputs) :number))))
    (cond ((and producer (svref record 7))
           (emit-native-retained-scalar-call inputs kind))
          ((eql kind 2)
           (wasm-local (svref record 6))
           (wasm-byte #x04) (wasm-type :object)
           (wasm-local (svref record 0)) (wasm-call "native-int64")
           (wasm-byte #x05)
           (wasm-get (third inputs)) (wasm-call "native-character-code-inline")
           (wasm-byte #x0b))
          (t
           (wasm-integer (native-float-call-op instruction))
           (wasm-integer kind)
           (if producer
               (progn (wasm-integer 0) (emit-native-scalar-argument left))
               (progn (wasm-integer 2) (wasm-local (svref record 6)) (wasm-byte #x6b)
                      (wasm-local (svref record 0))))
           (wasm-get (third inputs))
           (wasm-get (fourth inputs))
           (if (or left right)
               (progn (emit-native-scalar-argument right)
                      (emit-native-scalar-argument-status left 0)
                      (emit-native-scalar-argument-status right 1) (wasm-byte #x6a)
                      (wasm-call "native-float-transport"))
               (wasm-call "native-float-pair"))))
    (wasm-set output)
    (cond (producer (wasm-byte #x21) (wasm-u32 (svref record 6))
                    (wasm-byte #x21) (wasm-u32 (svref record 0)))
          ((/= kind 2) (wasm-byte #x1a) (wasm-byte #x1a)))))

(defun emit-native-scalar-snapshot (instruction inputs output raw)
  (cond
    ((and raw (member (svref raw 4) '(:array-snapshot :call-snapshot)))
     (if (eq (svref raw 4) :call-snapshot)
         (emit-native-retained-scalar-call inputs 1)
         (progn
           (wasm-get (third inputs))
           (let ((index (native-raw-aref-index instruction inputs :standard-call *wasm-unboxed-values*))
                 (argument (and *wasm-reusable-scalar-arguments*
                                (native-scalar-argument-index (fourth inputs) 1))))
             (cond (argument (wasm-get (fourth inputs))
                             (emit-native-scalar-argument argument)
                             (emit-native-scalar-argument-status argument 0)
                             (wasm-call "native-aref-snapshot-transport"))
                   (index (wasm-local (svref index 0)) (wasm-call "native-aref-snapshot64"))
                   (t (wasm-get (fourth inputs)) (wasm-call "native-aref-snapshot"))))))
     (wasm-set output)
     (wasm-byte #x21) (wasm-u32 (svref raw 6))
     (wasm-byte #x21) (wasm-u32 (svref raw 0))
     t)
    ((and raw (eq output (svref raw 1)) (> (length raw) 7))
     (let ((plan (svref raw 7)))
       (if (eq (first plan) :mask)
           (emit-native-snapshot-mask inputs (cdr plan))
           (emit-native-snapshot-arithmetic plan)))
     (emit-native-raw-result-store raw output)
     t)
    (t
     (let ((value (native-raw-array-store instruction *wasm-unboxed-values*)))
       (when value
         (wasm-get (second inputs))
         (wasm-local (svref value 0))
         (wasm-get (fourth inputs)) (wasm-get (fifth inputs))
         (wasm-call "native-set-aref-int64")
         t)))))

(defun emit-native-character-code-at (inputs unboxed)
  ;; STRING is already on the stack. Only reuse an already computed scalar or
  ;; a literal index; a merely declared argument still uses the checked object
  ;; path before any unboxing. Both result ABIs share those checked reads.
  (declare (type list inputs))
  (let* ((index (fourth inputs))
         (cached (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* index)))
         (producer (hir-value-producer index))
         (literal (and (%hir-instruction-p producer)
                       (eq (hir-instruction-opcode producer) :constant)
                       (first (hir-instruction-attributes producer))))
         (raw (or (and cached (or (eq (svref cached 4) :result) (svref cached 3)))
                  (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807)))))
    (if raw
        (progn (emit-native-unboxed-integer index
                 (or (native-integer-width index) (and cached (svref cached 2))))
               (wasm-call (if unboxed "native-character-code-at64-unboxed-inline"
                              "native-character-code-at64-inline")))
        (progn (wasm-get index)
               (wasm-call (if unboxed "native-character-code-at-unboxed-inline"
                              "native-character-code-at-inline"))))))

(defun emit-native-raw-result-store (raw output)
  (declare (type simple-vector raw output))
  (if (> (svref raw 5) 1)
      (progn (wasm-byte #x22) (wasm-u32 (svref raw 0))
        (compiler-statistic :optimization/unboxed-result-box)
        (emit-native-raw-result-box raw) (wasm-set output))
      (progn (wasm-byte #x21) (wasm-u32 (svref raw 0)))))

(defun emit-native-raw-result-box (raw)
  ;; Materializing an arithmetic result or SSA snapshot keeps the i64 tier.
  ;; Its mathematical width does not establish an immediate representation.
  (declare (type simple-vector raw))
  (if (native-immediate-integer-result-p raw)
      ;; A checked character read produces a fresh Unicode code, bounded by
      ;; the target character representation, independently of numeric inputs.
      (progn (wasm-byte #xa7) (wasm-gc #x1c) (wasm-gc #x1b))
      (wasm-call "native-int64")))

(defun emit-native-unused-context (value)
  ;; Global access uses the active runtime state, but HIR retains its context
  ;; dependency and producer effects. Discard a plain context local load
  ;; while preserving any materialization in WASM-GET.
  (declare (type simple-vector value))
  (let* ((source (or (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value)) value))
         (raw (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* source))))
    (when (or (and *wasm-multiple-result-locals* (native-value-slot *wasm-multiple-result-locals* value))
              (and raw (eq (svref raw 4) :result))
              (and *wasm-inline-constants* (native-value-slot *wasm-inline-constants* source)))
      (wasm-get value) (wasm-byte #x1a))))

(defun emit-native-vector-snapshot-operation (record instruction inputs)
  (emit-native-vector-snapshot-mode record :object)
  (cond ((eq instruction (svref record 3))
         (compiler-statistic :optimization/vector-snapshot :step)
         (wasm-call "native-poll")
         (wasm-local (svref record 6)) (wasm-integer 1) (wasm-byte #x6a)
         (wasm-gc #x1c) (wasm-gc #x1b))
        (t
         (compiler-statistic :optimization/vector-snapshot :aref)
         ;; Array and original index have executed. Only the proven
         ;; current snapshot changes transport. The worker resolves and
         ;; checks the array at the existing operation entry.
         (wasm-get (third inputs)) (wasm-local (svref record 6))
         (wasm-call "native-aref-i31-index")))
  (wasm-byte #x05))

(defun native-boxed-local-p (value)
  ;; Match WASM-GET's precedence. Captures and multiple results are checked
  ;; before reuse, while materialization and constants use the reused value.
  (and (not (and *wasm-captured-result-locals*
                 (native-value-slot *wasm-captured-result-locals* value)))
       (not (and *wasm-multiple-result-locals*
                 (native-value-slot *wasm-multiple-result-locals* value)))
       (let* ((source (or (and *wasm-reused-values*
                              (native-value-slot *wasm-reused-values* value))
                         value))
              (raw (and *wasm-unboxed-values*
                        (native-value-slot *wasm-unboxed-values* source))))
         (and (not (and *wasm-tiered-values*
                        (native-value-slot *wasm-tiered-values* source)))
              (not (and raw (eq (svref raw 4) :result)))
              (not (and *wasm-inline-constants*
                        (native-value-slot *wasm-inline-constants* source)))
              (native-value-slot *wasm-locals* source)))))

(defun emit-native-primary-call5 (inputs usage output)
  (declare (type list inputs))
  (when (and (null usage) (= (length inputs) 7)
             (not *wasm-debug*) (not *wasm-fixed-call-emitter*)
             (not *wasm-closed-worker-functions*)
             (not (native-value-slot *wasm-full-groups* output)))
    (let ((producer (hir-value-producer (second inputs))))
      (when (and (%hir-instruction-p producer)
                 (eq (hir-instruction-opcode producer) :function-reference)
                 (destructuring-bind (name &optional policy) (hir-instruction-attributes producer)
                   (not (eq (or policy
                                (and (boundp 'miricl::%global-inline-policy)
                                     miricl::%global-inline-policy
                                     (gethash name miricl::%global-inline-policy)))
                            'notinline)))
                 (every #'native-boxed-local-p inputs))
        ;; The environment read is unused by the ordinary adapter. Keep the
        ;; original producers and forward the saved callable and arguments.
        (dolist (value (cdr inputs)) (wasm-get value))
        (wasm-call "native-primary-call5")
        t))))

(defun emit-native-instruction (instruction &optional end)
  (declare (type simple-vector instruction) (type (or null simple-vector) end))
  ;; Suppress only exact calls consumed by the active branch plan. In
  ;; particular, checked comparisons remain globally nonremovable.
  (when (native-active-branch-test end instruction)
    (return-from emit-native-instruction nil))
  (let ((inputs (hir-instruction-inputs instruction)) (attributes (hir-instruction-attributes instruction))
        (output (hir-instruction-output instruction)))
    ;; This executable assertion belongs exactly here, even if later value
    ;; forwarding or dead-result elimination no longer needs its reference.
    (when (and *wasm-checked-integer-copies* (gethash output *wasm-checked-integer-copies*))
      (wasm-get (first inputs)) (wasm-call "native-check-int64") (wasm-set output)
      (let ((record (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* output))))
        (when (and record (eq (svref record 4) :checked-copy))
          ;; The check returned this same object after admitting i31 or
          ;; the immutable wide integer layout. Keep its ordinary box.
          (wasm-get output) (wasm-call "native-unbox-int64")
          (wasm-byte #x21) (wasm-u32 (svref record 0))))
      (return-from emit-native-instruction nil))
    (when (native-value-slot *wasm-inline-constants* output)
      (return-from emit-native-instruction nil))
    (let ((raw (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* output))))
      (when (and raw (member (svref raw 4) '(:result :character-result)) (<= (svref raw 5) 1)
                 (not (and *wasm-checked-integer-copies*
                           (gethash output *wasm-checked-integer-copies*)))
                 (not (eq output (svref raw 1)))
                 (or (member (hir-instruction-opcode instruction) '(:copy :primary))
                     (and (eq (hir-instruction-opcode instruction) :call)
                          (eq (gethash instruction *wasm-inline-simple-calls*) :left))))
        ;; These values name the same scalar observation. Integer uses can
        ;; materialize it, and the raw-only character path admits CHAR-CODE.
        (return-from emit-native-instruction nil)))
    (when (and (native-removable-p instruction) (not (native-value-slot *wasm-live-values* output)))
      (return-from emit-native-instruction nil))
    (let ((reused (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output))))
      (when reused
        (compiler-statistic :optimization/value-forward
          (or (gethash instruction *wasm-inline-simple-calls*) (hir-instruction-opcode instruction)))
        (return-from emit-native-instruction nil)))
    (when (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output))
      (return-from emit-native-instruction nil))
    (when (and (member (hir-instruction-opcode instruction) '(:block-enter :block-environment))
               (not (hir-target-captured-p (first attributes))))
      ;; Uncaptured blocks introduce no runtime scope or value copy.
      (return-from emit-native-instruction nil))
    (when (emit-native-tiered-instruction instruction) (return-from emit-native-instruction nil))
    (case (hir-instruction-opcode instruction)
      (:constant (wasm-literal (first attributes)))
      (:copy
       (let ((locals (and *wasm-multiple-result-locals* (native-value-slot *wasm-multiple-result-locals* output))))
         (if locals (emit-native-results (first inputs) (length locals)) (wasm-get (first inputs)))))
      ((:argument :rest-argument :dynamic-rest-argument :argument-present)
       (wasm-local 1) (wasm-integer (first attributes))
       (wasm-call (case (hir-instruction-opcode instruction)
                    (:argument "native-argument") (:argument-present "native-argument-present")
                    (:dynamic-rest-argument
                     (let ((plan (and *wasm-dynamic-rests* (gethash instruction *wasm-dynamic-rests*))))
                       (if (and plan (or (null (cdr plan))
                                         (native-value-slot *wasm-local-cells* (cdr plan))))
                           "native-prepare-dynamic-rest" "native-rest-argument")))
                    (otherwise "native-rest-argument")))
       (when (eq output *wasm-private-scalar-ingress*)
         ;; The fixed-worker rewrite replaces this packet read with its
         ;; typed parameter before the body is optimized or emitted.
         (let ((raw (native-value-slot *wasm-unboxed-values* output)))
           (unless (and raw (zerop (svref raw 5)))
             (throw 'native-multiple-result-fallback :retry-sealed-argument))
           (emit-native-raw-result-store raw output))
         (return-from emit-native-instruction nil)))
      ((:keyword-present :keyword-argument)
       (wasm-local 1) (wasm-integer (first attributes)) (wasm-literal (second attributes))
       (wasm-call (if (eq (hir-instruction-opcode instruction) :keyword-present)
                      "native-keyword-present" "native-keyword-argument")))
      (:check-keywords
       (wasm-local 1) (wasm-integer (first attributes)) (wasm-literal (second attributes))
       (wasm-integer (if (third attributes) 1 0)) (wasm-call "native-check-keywords"))
      (:check-arguments
       (wasm-local 1) (wasm-integer (first attributes)) (wasm-integer (second attributes))
       (wasm-call "native-check-arguments"))
      (:check-apply-arguments
       (wasm-get (first inputs)) (wasm-integer (first attributes))
       (wasm-call "native-check-apply-arguments"))
      (:check-apply-list
       (wasm-get (first inputs)) (wasm-call "native-check-apply-list"))
      (:checked-argument
       (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-call "native-argument"))
      (:truth
       (wasm-get (first inputs)) (wasm-byte #xd1)
       (wasm-byte #x04) (wasm-type :object) (wasm-null)
       (wasm-byte #x05) (wasm-literal t) (wasm-byte #x0b))
      (:block-enter
       (wasm-get (first inputs))
       (when (hir-target-captured-p (first attributes)) (wasm-call "native-block-enter")))
      (:block-environment
       (wasm-get (first inputs))
       (when (hir-target-captured-p (first attributes)) (wasm-call "native-block-environment")))
      (:function-reference
       (if (and *wasm-named-call-plan* (gethash output *wasm-named-call-plan*))
           ;; Pass the actual executing closure and its literal bank.
           ;; This exact surviving CALL must take its admitted lowering.
           (wasm-local 0)
           (progn
             (wasm-literal (first attributes))
             ;; Host kernel adapters own early-bootstrap reference resolution.
             (wasm-call (if (and (null *wasm-fixed-call-emitter*)
                                 (native-standard-function-name-p (first attributes)))
                            "native-standard-function-reference" "native-function-reference")))))
      (:global-read
       (emit-native-unused-context (first inputs))
       (wasm-literal (first attributes)) (wasm-call "native-global-read"))
      (:global-write
       (emit-native-unused-context (first inputs))
       (wasm-literal (first attributes)) (wasm-get (second inputs))
       (wasm-call "native-global-write"))
      (:dynamic-bind
       (wasm-get (first inputs)) (wasm-literal (first attributes)) (wasm-get (second inputs))
       (wasm-call "native-bind"))
      (:environment-restore
       (emit-native-restore (first inputs)))
      (:values (cond ((and *wasm-multiple-result-locals* (native-value-slot *wasm-multiple-result-locals* output))
                      (dolist (input inputs) (wasm-get input)))
                     ((native-value-slot *wasm-scalar-groups* output)
                      (if inputs (wasm-get (first inputs)) (wasm-null)))
                     (t (wasm-list inputs nil))))
      (:values-list
       (wasm-get (first inputs))
       (when (native-value-slot *wasm-scalar-groups* (first inputs)) (wasm-null) (wasm-call "native-cons")))
      (:list (wasm-list inputs nil))
      (:value-ref
       (emit-native-result (first inputs) (first attributes)))
      ((:cell-new :cell-read :cell-write :primary)
       (let* ((opcode (hir-instruction-opcode instruction))
              (cell (and *wasm-unboxed-values*
                        (native-value-slot *wasm-unboxed-values*
                          (if (eq opcode :cell-new) output (first inputs))))))
         (when (and cell (eq (svref cell 4) :cell))
           (compiler-statistic :optimization/unboxed-cell opcode)
           (case opcode
             (:cell-new
              (emit-native-unboxed-integer (first inputs)
                (if (svref cell 6) (svref cell 2) (native-integer-width (first inputs))))
              (wasm-byte #x21) (wasm-u32 (svref cell 0)))
             (:cell-read
              (wasm-local (svref cell 0))
              (emit-native-raw-result-store (native-value-slot *wasm-unboxed-values* output) output))
             (:cell-write
              (emit-native-unboxed-integer (second inputs)
                (if (svref cell 6) (svref cell 2) (native-integer-width (second inputs))))
              (wasm-byte #x21) (wasm-u32 (svref cell 0))
              (when (native-value-slot *wasm-live-values* output)
                (wasm-local (svref cell 0))
                (emit-native-raw-result-store (native-value-slot *wasm-unboxed-values* output) output))))
           (return-from emit-native-instruction nil)))
       (cond
         ((and (eq (hir-instruction-opcode instruction) :primary)
               (or (and *wasm-captured-result-locals*
                        (native-value-slot *wasm-captured-result-locals* (first inputs)))
                   (and *wasm-multiple-result-locals*
                        (native-value-slot *wasm-multiple-result-locals* (first inputs)))))
          (emit-native-result (first inputs) 0))
         ((and (eq (hir-instruction-opcode instruction) :primary)
               (or (native-value-slot *wasm-single-calls* (first inputs))
                   (native-value-slot *wasm-scalar-groups* (first inputs))))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-new) (native-value-slot *wasm-local-cells* output))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-read) (native-value-slot *wasm-local-cells* (first inputs)))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-write) (native-value-slot *wasm-local-cells* (first inputs)))
          (wasm-get (second inputs)) (wasm-set (first inputs)) (wasm-get (second inputs)))
         (t
          (dolist (input inputs) (wasm-get input))
          (wasm-call (ecase (hir-instruction-opcode instruction)
                    (:cell-new "native-cell") (:cell-read "native-primary")
                    (:cell-write "native-cell-write") (:primary "native-primary"))))))
      (:call
       (when *wasm-float-pairs*
         (let ((record (gethash instruction *wasm-float-pairs*)))
           (when record
             (emit-native-scalar-pair instruction inputs output record)
             (return-from emit-native-instruction nil))))
       (when *wasm-named-call-plan*
         (let ((site (gethash (second inputs) *wasm-named-call-plan*)))
           (when (eq instruction (car site))
             (ecase (cdr site)
               (:tail
                (compiler-statistic :optimization/direct-named-tail)
                (emit-native-tail-call inputs (hir-instruction-safety-level instruction)))
               (:scoped
                (unless (emit-native-scoped-call inputs)
                  (throw 'native-multiple-result-fallback :retry-named-recursion))
                (wasm-set output)))
             (return-from emit-native-instruction nil))))
       (when (emit-native-tiered-comparison instruction)
         (unless (native-value-slot *wasm-scalar-groups* output) (wasm-null) (wasm-call "native-cons"))
         (wasm-set output) (return-from emit-native-instruction nil))
       ;; The host closed-worker linker supports primary calls and direct
       ;; tail propagation. A full-value consumer needs a packet collector;
       ;; reject it before erasing the lexical function object.
       (when (and *wasm-closed-worker-functions*
                  (gethash (native-known-callee (second inputs)) *wasm-closed-worker-functions*)
                  (not (native-value-slot *wasm-single-calls* output))
                  (not (member instruction *wasm-tail-calls* :test #'eq)))
         (compiler-input-fail "closed worker call" :multiple-values
           "a non-tail full-value consumer requires the ordinary closure ABI"))
       (let ((kind (gethash instruction *wasm-inline-simple-calls*))
             (raw (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* output))))
         (when (and *wasm-unboxed-values*
                    (or raw (and (null kind) (fifth inputs)))
                    (emit-native-scalar-snapshot instruction inputs output raw))
           (return-from emit-native-instruction nil))
         (when kind
           (compiler-statistic :optimization/inline-primitive kind)
           (when (and (eq kind :character-set) *wasm-unboxed-values*
                      (not (native-value-slot *wasm-live-values* output)))
             (let ((character (native-value-slot *wasm-unboxed-values* (third inputs))))
               (when (and character (eq (svref character 4) :character-result))
                 (wasm-local (svref character 0)) (wasm-byte #xa7)
                 (wasm-get (fourth inputs)) (wasm-get (fifth inputs))
                 (wasm-call "native-character-set-code-inline")
                 (return-from emit-native-instruction nil))))
           (when (and raw (eq output *wasm-private-scalar-return*)
                      (eq (svref raw 4) :character-result))
             (wasm-get (third inputs))
             (emit-native-character-code-at inputs t)
             (emit-native-raw-result-store raw output)
             (return-from emit-native-instruction nil))
           (when (and raw (eq kind :character-code) (eq (svref raw 1) output))
             (let ((character (native-value-slot *wasm-unboxed-values* (third inputs))))
               (unless (and character (eq (svref character 4) :character-result))
                 (throw 'native-multiple-result-fallback :retry-sealed-scalar))
               (wasm-local (svref character 0))
               (emit-native-raw-result-store raw output)
               (return-from emit-native-instruction nil)))
           (when (and raw (eq (svref raw 4) :result)
                      (eq output (svref raw 1))
                      (or (and (integerp kind) (>= kind #x60)) (eq kind :character-code-at)))
             (compiler-statistic :optimization/unboxed-result kind)
             (if (eq kind :character-code-at)
                 (progn (wasm-get (third inputs))
                        (emit-native-character-code-at inputs t))
                 (let ((proof (and *wasm-proven-integer-arithmetic*
                                   (gethash instruction *wasm-proven-integer-arithmetic*))))
                   (if proof (emit-native-proven-integer-arithmetic inputs kind proof t)
                       (emit-native-inline-integer inputs kind t))))
             (emit-native-raw-result-store raw output)
             (return-from emit-native-instruction nil))
           (cond
             ((integerp kind)
              (let ((proof (and *wasm-proven-integer-arithmetic*
                                (gethash instruction *wasm-proven-integer-arithmetic*))))
                (if proof (emit-native-proven-integer-arithmetic inputs kind proof)
                    (emit-native-inline-integer inputs kind))))
             ((member kind '(:proven-less-int64 :proven-greater-int64
                             :proven-not-greater-int64 :proven-not-less-int64))
              (emit-native-proven-integer-comparison inputs kind))
             ((member kind '(:less-real :greater-real :not-greater-real :not-less-real))
              (emit-native-i31-comparison inputs kind))
             ((eq kind :true) (wasm-literal t))
             ((eq kind :false) (wasm-null))
             ((eq kind :left) (wasm-get (third inputs)))
             ((eq kind :right) (wasm-get (fourth inputs)))
             ((eq kind :endp-list)
              ;; The checked standard call polls after operand evaluation.
              ;; Keep that checkpoint even when its result is unused.
              (wasm-call "native-poll")
              ;; Admission proved the operation body separately. All operand
              ;; producers stay live at their original positions. A dead result
              ;; needs no boolean or value packet, while the poll stays here.
              (unless (native-value-slot *wasm-live-values* output)
                (return-from emit-native-instruction nil))
              (wasm-get (third inputs)) (wasm-byte #xd1) (wasm-call "native-boolean"))
             ((eq kind :list) (wasm-list (cddr inputs) nil))
             ((eq kind :list-star) (wasm-list (cddr inputs) t))
             ((eq kind :consp)
              (emit-native-tag-test (third inputs) 7 nil nil (null (hir-instruction-attributes instruction)) t))
             ((eq kind :atom)
              (emit-native-tag-test (third inputs) 7 nil nil t)
              (wasm-byte #x45) (wasm-call "native-boolean"))
             ((eq kind :listp)
              (emit-native-tag-test (third inputs) 7 nil t)
              (wasm-call "native-boolean"))
             ((eq kind :functionp)
              (emit-native-tag-test (third inputs) 8)
              (wasm-call "native-boolean"))
             ((eq kind :symbolp)
              (emit-native-tag-test (third inputs) 6 nil t) (wasm-call "native-boolean"))
             ((and *wasm-low-field* (eq instruction (first *wasm-low-field*))
                   (eq kind :standard-call))
              (wasm-integer (fourth *wasm-low-field*))
              (when (eq (fifth *wasm-low-field*) :extract)
                (wasm-integer (sixth *wasm-low-field*)))
              (dolist (operand (native-low-field-inputs instruction)) (wasm-get operand))
              (ecase (fifth *wasm-low-field*)
                (:add (wasm-call "native-low-field-add")
                      (compiler-statistic :optimization/low-field-add))
                (:ash (wasm-call "native-low-field-ash")
                      (compiler-statistic :optimization/low-field-ash))
                (:extract (wasm-call "native-literal-byte-extract")
                          (compiler-statistic :optimization/literal-byte-extract))))
             ((eq kind :standard-call)
              (let ((snapshot (and *wasm-vector-snapshot-operations*
                                   (gethash instruction *wasm-vector-snapshot-operations*))))
                (when snapshot (emit-native-vector-snapshot-operation snapshot instruction inputs))
                ;; Retain proven typed workers before the general checked
                ;; linkage. Their ABI no longer transports a function object.
                (let ((raw (native-raw-aref-index instruction inputs kind *wasm-unboxed-values* t)))
                  (cond (raw
                         (wasm-get (third inputs)) (wasm-local (svref raw 0))
                         (compiler-statistic :optimization/raw-aref-index)
                         (if (native-immediate-integer-result-p raw)
                             (progn (wasm-byte #xa7) (wasm-call "native-aref-i31-index"))
                             (wasm-call "native-aref-int64")))
                        ((or (emit-native-integer-call inputs output (hir-instruction-safety-level instruction))
                             (emit-native-i31-step inputs)))
                        (t
                         (let ((name (native-standard-operation (second inputs))))
                           (wasm-integer (native-canonical-builtin-code name))
                           (dolist (input (cddr inputs)) (wasm-get input))
                           (wasm-call (nth (- (length inputs) 2)
                             '("native-standard-call0" "native-standard-call1" "native-standard-call2"
                               "native-standard-call3" "native-standard-call4")))))))
                (when snapshot (wasm-byte #x0b))))
             ((eq kind :search-list)
              (wasm-get (third inputs)) (wasm-get (fourth inputs))
              (wasm-integer (native-search-list-code
                (native-standard-operation (second inputs)) inputs))
              (wasm-call "native-search-list"))
             ((member kind '(:evenp :oddp))
              (emit-native-unboxed-integer (third inputs) (native-integer-width (third inputs)))
              (wasm-byte #x42) (wasm-s32 1) (wasm-byte #x83)
              (wasm-byte (if (eq kind :evenp) #x50 #xa7))
              (wasm-call "native-boolean"))
             (t
              (wasm-get (third inputs))
              (case kind
                (:car (wasm-call "native-car-list-inline"))
                (:cdr (wasm-call "native-cdr-list-inline"))
                ((:car-cons :cdr-cons)
                 (setq *wasm-object-types* t)
                 (wasm-gc #x1a) (wasm-gc #x16 1)
                 (wasm-gc 2 1 (if (eq kind :car-cons) 4 5)) (wasm-gc #x1b))
                (:checked-car (wasm-call "native-primary"))
                (:checked-cdr (wasm-call "native-cdr-inline-checked"))
                (:list-path
                 (wasm-integer (native-list-path-code
                   (native-standard-operation (second inputs))))
                 (wasm-call "native-list-path"))
                ((:rplaca-cons :rplacd-cons)
                 ;; The proven CONS is mutable; the operation returns that
                 ;; same cons. Keep this store live even if its result is dead.
                 (setq *wasm-object-types* t)
                 (wasm-gc #x1a) (wasm-gc #x16 1)
                 (wasm-get (fourth inputs)) (wasm-gc #x1a) (wasm-gc #x17 #x6d)
                 (wasm-gc 5 1 (if (eq kind :rplaca-cons) 4 5))
                 (wasm-get (third inputs)))
                (:eq (wasm-get (fourth inputs)) (wasm-call "native-eq-inline"))
                (:eql (wasm-get (fourth inputs)) (wasm-call "native-eql-inline"))
                (:cons (wasm-get (fourth inputs)) (wasm-call "native-cons"))
                (:simple-vector-p (wasm-call "native-simple-vector-p-inline"))
                (:vectorp (wasm-call "native-vector-p-inline"))
                (:stringp (wasm-call "native-string-p-inline"))
                (:integerp (wasm-call "native-integer-p-inline"))
                (:characterp (wasm-call "native-character-p-inline"))
                (:integer-length (wasm-call "native-integer-length-inline"))
                (:zero-number (wasm-call "native-zero-inline"))
                (:ash-general (wasm-get (fourth inputs)) (wasm-call "native-ash-general-inline"))
                (:logand-general (wasm-get (fourth inputs)) (wasm-call "native-logand-general-inline"))
                (:mod-general (wasm-get (fourth inputs)) (wasm-call "native-mod-general-inline"))
                (:truncate-primary (wasm-get (fourth inputs)) (wasm-call "native-truncate-primary-inline"))
                ((:less-real :greater-real :not-greater-real :not-less-real)
                 (wasm-get (fourth inputs))
                 (wasm-integer (position kind '(:less-real :greater-real :not-greater-real :not-less-real)))
                 (wasm-call "native-ordered-compare-inline"))
                (:length
                 (let ((index (and *wasm-dynamic-rests* (gethash instruction *wasm-dynamic-rests*))))
                   (if index (progn (wasm-integer index) (wasm-call "native-dynamic-rest-length"))
                       (wasm-call "native-length-inline"))))
                (:list-length (wasm-call "native-list-length-inline"))
                (:vector-length (wasm-call "native-vector-length-inline"))
                (:character-ref
                 (wasm-get (fourth inputs)) (wasm-call "native-character-ref-inline"))
                (:character-set
                 (wasm-get (fourth inputs)) (wasm-get (fifth inputs))
                 (wasm-call "native-character-set-inline"))
                (:simple-character-set
                 (wasm-get (fourth inputs))
                 (emit-native-unboxed-integer (fifth inputs) (native-integer-width (fifth inputs)))
                 (wasm-call "native-simple-character-set-inline"))
                (:sbit-ref
                 (wasm-get (fourth inputs)) (wasm-call "native-sbit-ref-inline"))
                (:row-major-ref
                 (wasm-get (fourth inputs)) (wasm-call "native-row-major-ref-inline"))
                (:row-major-set
                 (wasm-get (fourth inputs)) (wasm-get (fifth inputs))
                 (wasm-call "native-row-major-set-inline"))
                (:character-code-at
                 (emit-native-character-code-at inputs nil))
                (:character-code (wasm-call "native-character-code-inline"))
                (:code-character (wasm-call "native-code-character-inline"))
                (:make-string (wasm-call "native-make-string-inline"))
                (:make-simple-vector
                 (if (= (length inputs) 5) (wasm-get (fifth inputs)) (wasm-null))
                 (wasm-call "native-make-vector-inline"))
                (:make-character-vector (wasm-call "native-make-character-vector-inline"))
                (:gethash
                 (wasm-get (fourth inputs))
                 (if (and (= (length inputs) 4) (native-value-slot *wasm-scalar-groups* output))
                     (wasm-call "native-gethash-primary-inline")
                     (progn
                       (if (fifth inputs) (wasm-get (fifth inputs)) (wasm-null))
                       (wasm-integer (if (native-value-slot *wasm-scalar-groups* output) 0 1))
                       (wasm-call "native-gethash-inline"))))
                (:puthash
                 (wasm-get (fourth inputs)) (wasm-get (fifth inputs))
                 (wasm-call "native-puthash-inline"))
                (:null (wasm-byte #xd1) (wasm-call "native-boolean")))))
           (unless (or (eq kind :gethash) (native-value-slot *wasm-scalar-groups* output))
             (wasm-null) (wasm-call "native-cons"))
           (wasm-set output)
           (return-from emit-native-instruction nil)))
       (when (gethash instruction *wasm-inline-vector-calls*)
         (emit-native-inline-vector inputs)
         (unless (native-value-slot *wasm-scalar-groups* output)
           (wasm-null) (wasm-call "native-cons"))
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (when (member instruction *wasm-private-scalar-sites* :test #'eq)
         (let ((raw (native-value-slot *wasm-unboxed-values* output))
               (worker *wasm-private-scalar-worker*))
           (unless (and raw (eq (svref raw 1) output) (zerop (svref raw 5))
                        (eq (svref raw 4) (if (eq *wasm-private-scalar-type* 'character)
                                             :character-result :result)))
             (throw 'native-multiple-result-fallback
               (if *wasm-private-scalar-argument* :retry-sealed-argument :retry-sealed-scalar)))
           (if *wasm-private-scalar-framed*
               (emit-native-call-frame-enter inputs
                 (if (eq (svref raw 4) :character-result) :i32 :i64))
               (wasm-call "native-poll"))
           (if *wasm-private-scalar-argument*
               (let ((usage (native-call-usage inputs)))
                 (wasm-get (second inputs))
                 (loop for input in (cddr inputs) for index from 0 do
                   (if (eql index *wasm-private-scalar-argument*)
                       (let ((argument (native-value-slot *wasm-unboxed-values* input)))
                         (unless (and argument (eq (svref argument 4) :result))
                           (throw 'native-multiple-result-fallback :retry-sealed-argument))
                         (wasm-local (svref argument 0)))
                       (if (or (null usage) (logbitp index usage)) (wasm-get input) (wasm-null)))))
               (emit-native-call-operands inputs))
           (dotimes (i (- 4 (length (cddr inputs)))) (wasm-null))
           (wasm-byte #x10)
           (push (list (length *wasm-bytes*) worker)
                 (gethash *wasm-current-function* *wasm-function-relocations*))
           (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
           (when *wasm-private-scalar-framed*
             (emit-native-call-frame-leave t))
           (when (eq (svref raw 4) :character-result)
             (wasm-byte #xad))
           (emit-native-raw-result-store raw output)
           (compiler-statistic :optimization/private-scalar-call (svref raw 4))
           (return-from emit-native-instruction nil)))
       (when (emit-native-multiple-call inputs output instruction)
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (when (and *wasm-captured-result-locals*
                  (native-value-slot *wasm-captured-result-locals* output)
                  (native-captured-call-p instruction))
         (if (= (length inputs) 3)
             (progn
               (wasm-get (second inputs)) (wasm-get (third inputs))
               (wasm-call (if *wasm-counted-result-consumer* "native-call-captured1" "native-call-values1")))
             (progn
               ;; Full capture retains the existing generic snapshot route.
               (wasm-get (third inputs)) (wasm-get (fourth inputs))
               (wasm-call "native-apply-captured-list")))
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (when (member instruction *wasm-tail-calls* :test #'eq)
         (emit-native-tail-call inputs (hir-instruction-safety-level instruction))
         (return-from emit-native-instruction nil))
       (when (native-no-prefix-apply-p inputs *wasm-current-function*)
         ;; Demand and the original transport ABI are independent: discarded
         ;; generic calls still snapshot the public list before callee entry.
         (wasm-get (third inputs)) (wasm-get (fourth inputs))
         (wasm-integer (+ (if (or (native-value-slot *wasm-scalar-groups* output)
                                  (not (native-value-slot *wasm-live-values* output))) 1 0)
                           (if (native-value-slot *wasm-single-calls* output) 2 0)))
         (wasm-call "native-apply-list")
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (when (and (not (native-value-slot *wasm-single-calls* output))
                  (emit-native-fixed-value-call inputs output))
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (if (native-value-slot *wasm-single-calls* output)
           ;; Keep argument checks in shared native primitives rather than
           ;; repeating CAR/CDR type dispatch at every call.
           (unless (or (emit-native-character-call inputs)
                       (emit-native-integer-call inputs output (hir-instruction-safety-level instruction))
                       (emit-native-list-call inputs (hir-instruction-safety-level instruction)))
             (emit-native-fixed-call inputs))
           (let ((usage (native-call-usage inputs))
                 (primary (or (native-value-slot *wasm-scalar-groups* output)
                              (not (native-value-slot *wasm-live-values* output)))))
             ;; Primary-only and discarded results need no value list,
             ;; even when the resolved callee's value count is unknown.
             (unless (and primary (emit-native-primary-call5 inputs usage output))
               (wasm-get (first inputs)) (wasm-get (second inputs))
               (wasm-arguments (cddr inputs) usage)
               (wasm-call (if primary "native-primary-call" "native-call"))))))
      (:control-call
       (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-get (second inputs))
       (if (member (first attributes) '(1 2 4 5 6 7)) (wasm-list (nthcdr 4 inputs) nil) (wasm-get (fourth inputs)))
       (wasm-get (third inputs)) (wasm-call "native-control")
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:restart-signal
       (wasm-get (first inputs)) (wasm-integer (first attributes))
       (wasm-list (cddr inputs) nil) (wasm-get (second inputs)) (wasm-call "native-restart-signal")
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:multiple-value-call
       (wasm-get (first inputs)) (wasm-get (second inputs))
       (dolist (group (cddr inputs))
         (wasm-get group)
         (when (native-value-slot *wasm-scalar-groups* group) (wasm-null) (wasm-call "native-cons")))
       (wasm-null) (dotimes (i (length (cddr inputs))) (wasm-call "native-cons"))
       (if (member instruction *wasm-tail-calls* :test #'eq)
           (let ((start (length *wasm-bytes*)))
             (wasm-call "native-tail-multiple-value-call")
             (setf (aref *wasm-bytes* start) #x12)
             (return-from emit-native-instruction nil))
           (wasm-call "native-multiple-value-call"))
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:capture
       (wasm-local 0) (wasm-integer (first attributes)) (wasm-call "native-capture"))
      (:closure
       (when (and *wasm-closed-worker-functions*
                  (gethash (first attributes) *wasm-closed-worker-functions*))
         ;; The adapter has proved all uses are calls of this closed target.
         ;; Its context is unused, and no function identity can escape.
         (wasm-null)
         (wasm-set output)
         (return-from emit-native-instruction nil))
       (pushnew (first attributes) *wasm-referenced-functions*)
       (wasm-byte #xd2)
       (when *wasm-function-relocations*
         (push (list (length *wasm-bytes*) (first attributes))
               (gethash *wasm-current-function* *wasm-function-relocations*)))
       (wasm-u32 (+ (length *native-imports*) (gethash (first attributes) *native-function-indices*)))
       (wasm-local 0) (wasm-list inputs nil)
       (let ((arity (native-fixed-arity (first attributes))))
         (if (and *wasm-static-closures*
                  (every (lambda (input)
                           (let ((producer (hir-value-producer input)))
                             (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :capture)))) inputs))
             (progn
               ;; Existing captures retain the same lexical binding instances
               ;; on every invocation of this parent function object.
               (wasm-integer (or arity -1))
               (wasm-integer (or (gethash (first attributes) *wasm-static-closures*)
                 (setf (gethash (first attributes) *wasm-static-closures*) (hash-table-count *wasm-static-closures*))))
               (wasm-call "native-close-cached"))
             (if arity (progn (wasm-integer arity) (wasm-call "native-close-fixed"))
                 (wasm-call "native-close"))))
       (let ((worker (and *wasm-direct-workers*
                          (or (gethash (first attributes) *wasm-boxed-result-workers*)
                              (gethash (first attributes) *wasm-direct-workers*)))))
         (when worker
           (pushnew worker *wasm-referenced-functions*)
           (wasm-byte #xd2)
           (push (list (length *wasm-bytes*) worker) (gethash *wasm-current-function* *wasm-function-relocations*))
           (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
           (let ((usage (native-argument-usage (first attributes))))
             (cond ((and *wasm-reusable-scalars* (gethash (first attributes) *wasm-reusable-scalars*))
                    (wasm-call "native-attach-scalar"))
                   (usage (wasm-integer usage) (wasm-call "native-attach-direct-usage"))
                   (t (wasm-call "native-attach-direct"))))))
       (when *wasm-debug*
         (wasm-local 0)
         ;; This stable HIR ordinal is resolved through the retained map after
         ;; body sharing, function ordering and module pruning have finished.
         (wasm-integer (position (first attributes) *native-functions* :test #'eq))
         (wasm-call "native-debug-close")))
      (otherwise (error "Native lowering is not implemented for ~S." (hir-instruction-opcode instruction))))
    (when output (wasm-set output))))

(defun emit-native-jump-value (block value)
  (when value
    (let* ((parameter (first (hir-block-parameters block)))
           (raw (and *wasm-unboxed-values* (native-value-slot *wasm-unboxed-values* parameter)))
           (locals (and *wasm-multiple-result-locals*
                        (native-value-slot *wasm-multiple-result-locals* parameter))))
      (when (and raw (eq (svref raw 1) parameter)
                 (member (svref raw 4) '(:result :array-snapshot)))
        (let ((source (native-value-slot *wasm-unboxed-values* value)))
          (if (eq (svref raw 4) :result)
              (progn
                (wasm-local (svref source 0))
                (emit-native-raw-result-store raw parameter))
              (let ((exact (native-unboxed-join-input-p source)))
                (if exact
                    (progn
                      (wasm-local (svref source 0))
                      (wasm-byte #x21) (wasm-u32 (svref raw 0))
                      (wasm-null))
                    (wasm-get value))
                (wasm-set parameter)
                (wasm-integer (if exact 1 0))
                (wasm-byte #x21) (wasm-u32 (svref raw 6)))))
        (return-from emit-native-jump-value nil))
      (when locals
        (emit-native-results value (length locals))
        (wasm-set (first (hir-block-parameters block)))
        (return-from emit-native-jump-value nil)))
    (wasm-get value)
    (when (and (native-value-slot *wasm-scalar-groups* value)
               (not (native-value-slot *wasm-scalar-groups* (first (hir-block-parameters block)))))
      (wasm-null) (wasm-call "native-cons"))
    (wasm-set (first (hir-block-parameters block)))))

(defun native-dispatch-targets (function targets &optional (blocks (hir-function-blocks function)))
  (declare (type simple-vector function) (type list targets blocks))
  ;; Most blocks are reached directly. Give only entry, backward and landing
  ;; targets dense dispatch indices, retaining their block IDs for layout.
  (let* ((indices (make-array (hir-function-block-count function) :initial-element nil))
         (depths nil) (count 0))
    (declare (type simple-vector indices) (type (unsigned-byte 32) count))
    (setf (svref indices (hir-block-id (hir-function-entry function))) t)
    (dolist (target targets) (setf (svref indices (hir-block-id (hir-target-block target))) t))
    (dolist (block blocks)
      (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
        (when (<= (hir-block-id successor) (hir-block-id block))
          (setf (svref indices (hir-block-id successor)) t))))
    (dotimes (id (length indices))
      (when (svref indices id)
        (setf (svref indices id) count) (incf count) (push id depths)))
    (cons indices (nreverse depths))))

(defun native-block-label-depths (function blocks dispatch)
  (declare (type simple-vector function) (type list blocks dispatch))
  ;; Fallthrough needs no label. Count only labels that an explicit edge or
  ;; the dispatcher can name; the prefix counts also give branch depths.
  ;; An acyclic ordering can omit unreachable blocks without renumbering IDs.
  (let ((depths (make-array (length (hir-function-blocks function)) :initial-element 0)) (count 0))
    (declare (type simple-vector depths) (type (unsigned-byte 32) count))
    (dolist (id dispatch) (setf (svref depths id) 1))
    (loop for tail on blocks for block = (car tail) for next = (cadr tail) do
      (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
        (unless (eq successor next)
          (setf (svref depths (hir-block-id successor)) 1))))
    (dolist (block blocks)
      (incf count (svref depths (hir-block-id block)))
      (setf (svref depths (hir-block-id block)) count))
    depths))

(defun emit-native-dispatch-target (block from)
  (declare (type simple-vector block from *wasm-next-blocks*))
  ;; The dispatcher's remaining nested labels already name every forward
  ;; block. Only backward edges need to reload the program counter.
  (let ((distance (- (hir-block-id block) (hir-block-id from))))
    (if (plusp distance)
        (unless (eq block (svref *wasm-next-blocks* (the (unsigned-byte 32) (hir-block-id from))))
          (wasm-byte #x0c)
          (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id block))
                       (svref *wasm-label-depths* (hir-block-id from)) 1)))
        (progn
          (wasm-integer (svref *wasm-dispatch-indices* (hir-block-id block)))
          (wasm-byte #x21) (wasm-u32 2)
          (wasm-byte #x0c) (wasm-u32 *wasm-dispatch-depth*)))))

(defun emit-native-jump (block value from)
  (emit-native-jump-value block value)
  ;; Every cycle has a backward/self edge under this total block ordering.
  ;; Inlining all calls in a loop must not remove its scheduling safepoint.
  (when (<= (hir-block-id block) (hir-block-id from))
    ;; Restore before yielding on a backedge. Forward transfers arrive at
    ;; the target's restore without an intervening call or scheduling point.
    (when *wasm-dynamic-bindings*
      (emit-native-restore (hir-block-environment block)))
    (wasm-call "native-poll"))
  (emit-native-dispatch-target block from))

(defun emit-native-condition (end truep)
  (declare (type simple-vector end))
  (multiple-value-bind (truth known)
      (native-proven-truth (first (hir-instruction-inputs end)) (hir-instruction-safety-level end))
    (when known
      (compiler-statistic :optimization/proven-condition (hir-instruction-safety-level end))
      (wasm-integer (if (eq truth truep) 1 0))
      (return-from emit-native-condition nil)))
  (let* ((test (native-active-branch-test end))
         (call (second test)) (kind (third test)))
    (when (fourth test) (setq truep (not truep)))
    (if call
        (let ((record (and *wasm-hoisted-values*
                           (native-value-slot *wasm-hoisted-values* (hir-instruction-output call)))))
          (compiler-statistic :optimization/raw-predicate-branch kind)
          (if (and record (svref record 6))
              (progn (wasm-local (svref record 6)) (unless truep (wasm-byte #x45)))
        (let ((inputs (hir-instruction-inputs call)))
          (cond
            ((and *wasm-vector-snapshot-operations*
                  (emit-native-vector-snapshot-comparison call inputs kind)))
            ((emit-native-tiered-comparison call t))
            ((integerp kind) (emit-native-inline-integer inputs kind t))
            ((member kind '(:less-real :greater-real :not-greater-real :not-less-real))
             (emit-native-i31-comparison inputs kind t))
            ((member kind '(:proven-less-int64 :proven-greater-int64
                            :proven-not-greater-int64 :proven-not-less-int64))
             (emit-native-proven-integer-comparison inputs kind t))
            ((member kind '(:true :false)) (wasm-integer (if (eq kind :true) 1 0)))
            ((eq kind :null) (wasm-get (third inputs)) (wasm-byte #xd1))
            ((eq kind :eq)
             (if (null (hir-instruction-attributes call))
                 (progn (wasm-get (third inputs)) (wasm-get (fourth inputs))
                   (wasm-call "native-eq-test"))
                 (progn (wasm-get (third inputs)) (wasm-gc #x1a) (wasm-gc #x17 #x6d)
                   (wasm-get (fourth inputs)) (wasm-gc #x1a) (wasm-gc #x17 #x6d) (wasm-byte #xd3))))
            ((eq kind :eql)
             (wasm-get (third inputs)) (wasm-get (fourth inputs)) (wasm-call "native-eql-test"))
            ((eq kind :consp) (emit-native-tag-test (third inputs) 7 nil nil (null (hir-instruction-attributes call))))
            ((eq kind :symbolp) (emit-native-tag-test (third inputs) 6 nil t))
            ((member kind '(:evenp :oddp))
             (emit-native-unboxed-integer (third inputs) (native-integer-width (third inputs)))
             (wasm-byte #x42) (wasm-s32 1) (wasm-byte #x83)
             (wasm-byte (if (eq kind :evenp) #x50 #xa7))))
          (unless truep (wasm-byte #x45)))))
        (progn
          (wasm-get (if test (first test) (first (hir-instruction-inputs end))))
          (wasm-byte #xd1)
          (when truep (wasm-byte #x45))))))

(defun emit-native-branch (end block next)
  (declare (type simple-vector end block) (type (or null simple-vector) next))
  (let* ((successors (hir-instruction-attributes end))
         (yes (first successors)) (no (second successors))
         (depth (svref *wasm-label-depths* (the (unsigned-byte 32) (hir-block-id block)))))
    (when (eq yes no)
      (emit-native-jump yes nil block) (return-from emit-native-branch nil))
    (when (or (<= (hir-block-id yes) (hir-block-id block))
              (<= (hir-block-id no) (hir-block-id block)))
      (wasm-call "native-poll"))
    (cond
      ((or (eq yes next) (eq no next))
       (let ((target (if (eq yes next) no yes)))
         (emit-native-condition end (eq target yes))
         (if (> (hir-block-id target) (hir-block-id block))
             (progn
               (wasm-byte #x0d)
               (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id target)) depth 1)))
             (progn
               ;; A backward target needs the dispatcher, while NEXT falls
               ;; through. Account for this IF's extra control scope.
               (wasm-byte #x04) (wasm-byte #x40)
               (let ((*wasm-dispatch-depth* (1+ *wasm-dispatch-depth*)))
                 (emit-native-dispatch-target target block))
               (wasm-byte #x0b)))))
      (t
       (let ((target (cond ((> (hir-block-id no) (hir-block-id block)) no)
                           ((> (hir-block-id yes) (hir-block-id block)) yes))))
         (if target
             (progn
               (emit-native-condition end (eq target yes))
               (wasm-byte #x0d)
               (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id target)) depth 1))
               (emit-native-dispatch-target (if (eq target yes) no yes) block))
             (progn
               (emit-native-condition end nil)
               (wasm-byte #x04) (wasm-byte #x7f)
               (wasm-integer (svref *wasm-dispatch-indices* (hir-block-id no))) (wasm-byte #x05)
               (wasm-integer (svref *wasm-dispatch-indices* (hir-block-id yes))) (wasm-byte #x0b)
               (wasm-byte #x21) (wasm-u32 2)
               (wasm-byte #x0c) (wasm-u32 *wasm-dispatch-depth*))))))))

(defun emit-native-return (value)
  (when *wasm-private-scalar-return*
    (when *wasm-reusable-scalar-identity*
      (emit-native-scalar-argument *wasm-reusable-scalar-identity*)
      (emit-native-scalar-argument-status *wasm-reusable-scalar-identity* 0)
      (unless (= *wasm-reusable-scalar-result* 1)
        (wasm-integer *wasm-reusable-scalar-result*) (wasm-byte #x6c))
      (wasm-get *wasm-private-scalar-return*)
      (return-from emit-native-return nil))
    (let ((raw (native-value-slot *wasm-unboxed-values* *wasm-private-scalar-return*)))
      (unless (and raw (eq *wasm-private-scalar-return* (svref raw 1))
                   (eq (svref raw 4) (case *wasm-reusable-scalar-result*
                                        ((3 4) :float-snapshot)
                                        (otherwise (if (eq *wasm-private-scalar-type* 'character)
                                                       :character-result :result))))
                   (or *wasm-private-scalar-forward* (eq value *wasm-private-scalar-return*)))
        (throw 'native-multiple-result-fallback
          (if *wasm-private-scalar-argument* :retry-sealed-argument :retry-sealed-scalar)))
      (unless *wasm-private-scalar-framed*
        (wasm-null) (wasm-call "native-return1") (wasm-byte #x1a))
      (wasm-local (svref raw 0))
      (cond ((member *wasm-reusable-scalar-result* '(3 4))
             (wasm-local (svref raw 6))
             (wasm-integer *wasm-reusable-scalar-result*) (wasm-byte #x6c)
             (wasm-get *wasm-private-scalar-return*))
            (*wasm-reusable-scalar-result*
             (wasm-integer *wasm-reusable-scalar-result*) (wasm-null))
            ((eq (svref raw 4) :character-result) (wasm-byte #xa7)))
      (return-from emit-native-return nil)))
  (when *wasm-dynamic-bindings*
    (emit-native-restore *wasm-entry-environment*))
  (let ((locals (and *wasm-multiple-result-locals*
                     (native-value-slot *wasm-multiple-result-locals* value))))
    (cond (*wasm-multiple-result-count*
           (emit-native-results value *wasm-multiple-result-count*))
          ((and locals (<= 2 (length locals) 3))
           (dolist (local locals) (wasm-local local))
           (wasm-call (if (= (length locals) 2) "native-return2" "native-return3")))
          ((and locals (native-counted-result-return-p value))
           (dolist (local locals) (wasm-local local))
           (dotimes (padding (- 8 (length locals))) (wasm-null))
           (wasm-integer (length locals))
           (wasm-call "native-return-counted8"))
          (t (wasm-get value)
             (wasm-call (if (native-value-slot *wasm-scalar-groups* value) "native-return1" "native-return"))))))

(defun emit-native-throw (inputs)
  (dolist (input inputs) (wasm-get input))
  (when (native-value-slot *wasm-scalar-groups* (third inputs)) (wasm-null) (wasm-call "native-cons"))
  (wasm-call "native-throw") (wasm-byte #x00))

(defun native-block-targets (function)
  (let ((targets nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :block-enter)
                   (hir-target-captured-p (first (hir-instruction-attributes instruction))))
          (push (first (hir-instruction-attributes instruction)) targets))))
    (nreverse targets)))

(defun emit-native-block-return (inputs)
  (wasm-get (second inputs)) (wasm-get (third inputs))
  (when (native-value-slot *wasm-scalar-groups* (third inputs)) (wasm-null) (wasm-call "native-cons"))
  (wasm-call "native-block-return") (wasm-byte #x00))

(defun emit-native-forward (blocks)
  (declare (type list blocks))
  (let ((position 0) (depth 0))
    (declare (type (unsigned-byte 32) position depth))
    ;; The innermost label names the next block, the next label the one after
    ;; it, and so on. Close a label just before emitting its block.
    (dotimes (i (svref *wasm-label-depths* (hir-block-id (car (last blocks)))))
      (wasm-byte #x02) (wasm-byte #x40))
    (loop for tail on blocks for block = (car tail) for next = (cadr tail) do
      (let ((current (svref *wasm-label-depths* (hir-block-id block))))
        (unless (= current depth) (wasm-byte #x0b) (setq depth current)))
      (when (and *wasm-dynamic-bindings* (not (zerop position)))
        (emit-native-restore (hir-block-environment block)))
      (dolist (instruction (hir-block-instructions block)) (emit-native-instruction instruction (hir-block-terminator block)))
      (let* ((terminator (hir-block-terminator block)) (inputs (hir-instruction-inputs terminator))
             (successors (hir-instruction-attributes terminator)) (target (first successors)))
        (ecase (hir-instruction-opcode terminator)
          (:unreachable (wasm-byte #x00))
          (:throw (emit-native-throw inputs))
          (:return
           (emit-native-return (first inputs))
           (when next (wasm-byte #x0f)))
          ((:jump :block-return)
           (if (and (eq (hir-instruction-opcode terminator) :block-return) (not target))
               (emit-native-block-return inputs)
               (progn
                 (unless (eq terminator *wasm-private-scalar-forward*)
                   (emit-native-jump-value target (if (eq (hir-instruction-opcode terminator) :jump)
                                                    (first inputs) (third inputs))))
                 (unless (eq target next)
                   (wasm-byte #x0c)
                   (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id target)) depth 1))))))
          (:branch
           (let ((no (second successors)))
             (if (eq target no)
                 (unless (eq target next)
                   (wasm-byte #x0c)
                   (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id target)) depth 1)))
                 (progn
                   (emit-native-condition terminator (eq no next))
                   (wasm-byte #x0d)
                   (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id (if (eq no next) target no))) depth 1))
                   (unless (or (eq target next) (eq no next))
                     (wasm-byte #x0c)
                     (wasm-u32 (- (svref *wasm-label-depths* (hir-block-id target)) depth 1)))))))))
      (incf position))
    (wasm-byte #x0b)))

(defun emit-native-structured-target (target)
  (declare (type simple-vector target *wasm-structured-labels*)
           (type (unsigned-byte 32) *wasm-control-depth*))
  (wasm-u32 (- *wasm-control-depth*
               (the (unsigned-byte 32) (svref *wasm-structured-labels*
                                         (the (unsigned-byte 32) (hir-block-id target)))) 1)))

(defun native-single-return-target-p (block)
  ;; Bypass joins which only project and rewrap the one returned value.
  ;; A full value group, assertion, dynamic restore or other effect keeps the
  ;; ordinary edge. Bound work even for hostile or cyclic public HIR.
  (loop repeat 32 do
    (let ((value (first (hir-block-parameters block))))
      (unless (and value (native-value-slot *wasm-scalar-groups* value)) (return nil))
      (dolist (instruction (hir-block-instructions block))
        (unless (and (member (hir-instruction-opcode instruction) '(:copy :primary :values))
                     (eq (hir-instruction-effect instruction) :pure)
                     (null (cdr (hir-instruction-inputs instruction)))
                     (eq value (first (hir-instruction-inputs instruction)))
                     (hir-instruction-output instruction)
                     (eq (hir-value-type (hir-instruction-output instruction)) t))
          (return-from native-single-return-target-p nil))
        (setq value (hir-instruction-output instruction)))
      (let* ((end (hir-block-terminator block)) (opcode (hir-instruction-opcode end))
             (returned (if (eq opcode :block-return)
                           (third (hir-instruction-inputs end)) (first (hir-instruction-inputs end)))))
        (unless (eq value returned) (return nil))
        (case opcode
          (:return (return (and (native-value-slot *wasm-scalar-groups* value) t)))
          ((:jump :block-return)
           (unless (hir-instruction-attributes end) (return nil))
           (setq block (first (hir-instruction-attributes end))))
          (otherwise (return nil)))))))

(defun emit-native-structured-jump (target value from next)
  (declare (type simple-vector target from))
  (when (and value (not *wasm-debug*) (native-value-slot *wasm-scalar-groups* value)
             (native-single-return-target-p target))
    (emit-native-return value) (wasm-byte #x0f)
    (return-from emit-native-structured-jump nil))
  (emit-native-jump-value target value)
  ;; The structured plan proves which targets are natural-loop headers.
  ;; Low numeric IDs also name ordinary lexical exits, which cannot cycle.
  (when (and (<= (hir-block-id target) (hir-block-id from))
             (svref *wasm-loop-regions* (hir-block-id target)))
    (when *wasm-dynamic-bindings*
      (emit-native-restore (hir-block-environment target)))
    (wasm-call "native-poll"))
  (unless (eq target next) (wasm-byte #x0c) (emit-native-structured-target target)))

(defun emit-native-structured-region (blocks header)
  (declare (type list blocks) (type (or null simple-vector) header)
           (type simple-vector *wasm-structured-labels* *wasm-loop-regions*)
           (type (unsigned-byte 32) *wasm-control-depth*))
  ;; Outer labels stay open around the loop, so an exit branches directly to
  ;; its ordinary successor. Inside, HEADER names LOOP rather than BLOCK.
  (dolist (block (reverse blocks))
    (unless (eq block header)
      (setf (svref *wasm-structured-labels* (the (unsigned-byte 32) (hir-block-id block))) *wasm-control-depth*)
      (wasm-byte #x02) (wasm-byte #x40) (incf *wasm-control-depth*)))
  (loop for tail on blocks for block = (car tail) for next = (cadr tail) do
    (unless (eq block header) (wasm-byte #x0b) (decf *wasm-control-depth*))
    (let ((region (svref *wasm-loop-regions* (the (unsigned-byte 32) (hir-block-id block)))))
     (if (and region (not (eq block header)))
        (progn
          (when *wasm-loop-hoists*
            (dolist (record (svref *wasm-loop-hoists* (the (unsigned-byte 32) (hir-block-id block))))
              (emit-native-hoisted-primitive record)))
          (when *wasm-vector-snapshot-prefixes*
            (let ((record (gethash block *wasm-vector-snapshot-prefixes*)))
              (when record (emit-native-vector-snapshot-prefix record))))
          (setf (svref *wasm-structured-labels* (the (unsigned-byte 32) (hir-block-id block))) *wasm-control-depth*)
          (wasm-byte #x03) (wasm-byte #x40) (incf *wasm-control-depth*)
          (emit-native-structured-region (svref region 3) block)
          (wasm-byte #x0b) (decf *wasm-control-depth*)
          ;; Every reachable path returned, exited, trapped or took a
          ;; backedge; there is no ordinary fallthrough from this region.
          (wasm-byte #x00))
        (progn
          (when *wasm-dynamic-bindings*
            (emit-native-restore (hir-block-environment block)))
          (dolist (instruction (hir-block-instructions block)) (emit-native-instruction instruction (hir-block-terminator block)))
          (let* ((end (hir-block-terminator block)) (inputs (hir-instruction-inputs end))
                 (successors (hir-instruction-attributes end)) (target (first successors)))
            (ecase (hir-instruction-opcode end)
              (:unreachable (wasm-byte #x00))
              (:return (emit-native-return (first inputs)) (when (or next header) (wasm-byte #x0f)))
              (:throw (emit-native-throw inputs))
              ((:jump :block-return)
               (if (and (eq (hir-instruction-opcode end) :block-return) (null target))
                   (emit-native-block-return inputs)
                   (emit-native-structured-jump target
                     (if (eq (hir-instruction-opcode end) :jump) (first inputs) (third inputs)) block next)))
              (:branch
               (let ((no (second successors)))
                 (if (eq target no)
                     (emit-native-structured-jump target nil block next)
                     (progn
                       (when (or (and (<= (hir-block-id target) (hir-block-id block))
                                      (svref *wasm-loop-regions* (hir-block-id target)))
                                 (and (<= (hir-block-id no) (hir-block-id block))
                                      (svref *wasm-loop-regions* (hir-block-id no))))
                         (wasm-call "native-poll"))
                       (emit-native-condition end (not (eq target next)))
                       (wasm-byte #x0d) (emit-native-structured-target (if (eq target next) no target))
                       (unless (or (eq target next) (eq no next))
                         (wasm-byte #x0c) (emit-native-structured-target no)))))))))))))

(defun emit-native-function (function)
  (let ((*wasm-current-function* function) (*wasm-captured-result-p* nil)
        (*wasm-call-frame-locals* nil))
    (let* ((body (emit-native-function-body function))
           (plan (and *wasm-function-plans* (gethash function *wasm-function-plans*))))
      (when *wasm-direct-raw-bodies* (setf (gethash function *wasm-direct-raw-bodies*) body))
      ;; Acyclic evaluation entries execute their instructions at most once.
      ;; Their local/stack sizing work cannot pay back during execution. Keep
      ;; optimizing every persistent child and any entry containing a loop.
      (cond ((or *wasm-captured-result-p* *wasm-call-frame-locals*
                 (and plan (or (plusp (svref plan 26)) (svref plan 27))))
             (native-optimize-typed-body body 2))
            ((not (and *wasm-single-use-entry* (eq function (first *native-functions*))
                         plan (svref plan 1))) (native-optimize-body body))
            (t body)))))

(defun native-loop-literals (blocks forward &optional (local-start 0) checked-entry)
  ;; Literal identity is stable for the lifetime of this function object.
  ;; Cache repeated operands and global *names*, never their live bindings.
  ;; Acyclic bodies must recover the prologue and local costs in static bytes;
  ;; loops also benefit from avoiding a lookup on every iteration.
  ;; All caches occupy frame slots from ENTRY, even before their first use.
  ;; Do not grow frames retained across potentially reentrant calls: caching
  ;; names used only after recursion still reduces the available stack depth.
  (unless (and (integerp local-start) (<= 0 local-start #xffffffff))
    (return-from native-loop-literals nil))
  (let ((seen (make-hash-table :test 'eq)) (literals nil))
    (dolist (block blocks)
      (let ((end (hir-block-terminator block)))
        (unless (or (member (hir-instruction-opcode end) '(:branch :jump :return :unreachable))
                    (and (eq (hir-instruction-opcode end) :block-return)
                         (hir-instruction-attributes end)))
          (return-from native-loop-literals nil)))
      (dolist (instruction (hir-block-instructions block))
        ;; Signaling helpers can reenter through Lisp condition handlers too.
        ;; Use the existing non-signaling proof and private packet operations;
        ;; a live global function/variable lookup supplies no such proof.
        (unless (or (and (native-removable-p instruction)
                         (or (not (eq (hir-instruction-opcode instruction) :function-reference))
                             (not (native-value-slot *wasm-live-values*
                                    (hir-instruction-output instruction)))))
                    (member (hir-instruction-opcode instruction)
                            '(:rest-argument :dynamic-rest-argument :values-list :cell-write :environment-restore))
                    (member instruction *wasm-tail-calls* :test #'eq)
                    ;; Direct workers strip this check; ordinary entries must
                    ;; retain their small checked wrapper outside the worker.
                    (and checked-entry (eq (hir-instruction-opcode instruction) :check-arguments)))
          (return-from native-loop-literals nil))
        (when (and (member (hir-instruction-opcode instruction)
                          '(:constant :function-reference :global-read :global-write :dynamic-bind))
                   (not (and (native-removable-p instruction)
                             (not (native-value-slot *wasm-live-values* (hir-instruction-output instruction))))))
          (let ((value (first (hir-instruction-attributes instruction))))
            ;; Immediate literals bypass the cache in WASM-LITERAL. Giving
            ;; them cache slots adds unused prologue work and locals.
            (when (and value (not (and (integerp value) (<= -1073741824 value 1073741823))))
              (unless (gethash value seen) (push value literals))
              (incf (gethash value seen 0)))))))
    ;; Leaf/tail-only functions cannot accumulate these roots recursively.
    ;; Still bound the extra frame pressure to eight references, sharing a
    ;; 32-slot working allowance with the already planned locals. Rematerialize
    ;; cheap operands when that allowance is exhausted, including inside loops.
    (let* ((budget (min 8 (max 0 (- 32 local-start))))
           (limit (+ local-start (min budget (length literals)))))
      ;; A projected local index need not fit just because both addends do.
      ;; Unknown/out-of-format costs keep the ordinary uncached lowering.
      (when (> limit #xffffffff)
        (return-from native-loop-literals nil))
      (let* ((local-cost (1+ (wasm-u32-width limit)))
             (profitable (remove-if
               (lambda (literal)
                 (let* ((uses (gethash literal seen))
                        ;; Lower bounds include one-byte import/literal indices.
                        ;; T uses NATIVE-BOOLEAN rather than the literal bank.
                        (load-cost (if (eq literal t) 4 6))
                        (keep (if forward
                                  (> (* uses (- load-cost local-cost))
                                     (+ load-cost local-cost 2))
                                  (>= uses 2))))
                   ;; Price indices at the maximum possible cache/local slot,
                   ;; since coloring may reorder slots. Reserve two bytes.
                   (not keep)))
               (nreverse literals))))
        ;; Use the limited roots where they avoid the most rematerialization;
        ;; stable ordering gives equal-benefit literals their source order.
        (setq profitable (stable-sort profitable #'> :key
          (lambda (literal)
            (let ((load-cost (if (eq literal t) 4 6)))
              (- (* (gethash literal seen) (- load-cost local-cost))
                 load-cost local-cost 2)))))
        (subseq profitable 0 (min budget (length profitable)))))))

(defun native-loop-dominates-p (definition header query)
  (declare (type simple-vector definition header query))
  (when (eq definition header) (return-from native-loop-dominates-p nil))
  (when (eq definition (svref query 0)) (return-from native-loop-dominates-p t))
  (let* ((key (cons (hir-block-id definition) (hir-block-id header)))
         (cache (svref query 2)) (seen (svref query 3)))
    (declare (type hash-table cache) (type simple-vector seen))
    (multiple-value-bind (proof found) (gethash key cache)
      (when found (return-from native-loop-dominates-p proof)))
    (let ((epoch (incf (svref query 4))) (work (list (svref query 0))) (proof t))
      (declare (type (unsigned-byte 32) epoch) (type list work))
      (loop while work do
        (when (zerop (the (integer 0 65536) (svref query 5))) (setq proof nil) (return))
        (decf (svref query 5))
        (let* ((block (pop work)) (id (hir-block-id block)))
          (declare (type simple-vector block) (type (unsigned-byte 32) id))
          (unless (or (eq block definition) (= (the (unsigned-byte 32) (svref seen id)) epoch))
            (when (eq block header) (setq proof nil) (return))
            (setf (svref seen id) epoch)
            (dolist (target (hir-instruction-attributes (hir-block-terminator block))) (push target work)))))
      ;; Exhaustion supplies no proof; it does not establish non-dominance.
      (setf (gethash key cache) proof))))

(defun native-hoist-source (value header query)
  (declare (type simple-vector value header query))
  ;; A dominating definition has executed before this loop's prefix. Stop
  ;; there, preserving its established assertion; a body-only assertion
  ;; supplies no evidence for speculation before its own execution.
  (loop repeat 32 do
    (when (zerop (the (integer 0 65536) (svref query 5))) (return-from native-hoist-source nil))
    (decf (svref query 5))
    (let ((alias (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value))))
      (when alias (setq value alias)))
    (let* ((producer (hir-value-producer value))
           (definition (native-value-slot (the simple-vector (svref query 1)) value)))
      (when (or (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
                (and definition (native-loop-dominates-p definition header query)))
        (return-from native-hoist-source value))
      (unless (and (%hir-instruction-p producer)
                   (or (eq (hir-instruction-opcode producer) :copy)
                       (and (eq (hir-instruction-opcode producer) :primary)
                            (native-value-slot *wasm-scalar-groups* (first (hir-instruction-inputs producer))))))
        (return-from native-hoist-source nil))
      (setq value (first (hir-instruction-inputs producer)))))
  nil)

(defun native-hoist-width (value)
  (declare (type simple-vector value))
  (let ((producer (hir-value-producer value)))
    (if (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
        (let ((literal (first (hir-instruction-attributes producer))))
          ;; A literal's actual target value supplies the proof, rather than
          ;; a potentially narrower assertion scoped to its original use.
          (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807)
               (1+ (integer-length literal))))
        (let ((width (native-type-integer-width (gethash value *wasm-inferred-types* t))))
          (and width (<= 1 width 64) width)))))

(defun native-hoist-type (value)
  (declare (type simple-vector value))
  (let ((producer (hir-value-producer value)))
    (if (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
        (hir-constant-type (first (hir-instruction-attributes producer)))
        (let ((type (gethash value *wasm-inferred-types* t)))
          (if (and (consp type) (eq (car type) 'values))
              (if (member (second type) lambda-list-keywords) t (second type)) type)))))

(defun native-hoist-immediate-result-p (instruction arguments)
  (declare (type simple-vector instruction) (type list arguments))
  (let* ((reference (hir-value-producer (second (hir-instruction-inputs instruction))))
         (name (and (%hir-instruction-p reference) (eq (hir-instruction-opcode reference) :function-reference)
                    (first (hir-instruction-attributes reference))))
         (type (and name (hir-inline-result-type name (mapcar #'native-hoist-type arguments))))
         (width (and type (native-type-integer-width type))))
    ;; This target representation is immediate and allocates no number object.
    ;; The result proof also excludes intermediate i64 overflow for these ops.
    (and width (<= 1 width 31))))

(defun native-plan-loop-hoists (function blocks structured)
  (declare (type simple-vector function) (type list blocks))
  (unless structured (return-from native-plan-loop-hoists nil))
  (let* ((regions (cdr structured)) (entry (hir-function-entry function))
         (owners (make-array (hir-function-block-count function) :initial-element nil))
         (parents (make-array (hir-function-block-count function) :initial-element nil))
         (definitions (native-value-table function))
         (query (vector entry definitions (make-hash-table :test 'equal)
                        (make-array (hir-function-block-count function) :initial-element 0) 0 65536))
         (fused (make-hash-table :test 'eq)) (hoists nil) (fuel 4096))
    (declare (type simple-vector regions owners parents definitions entry)
             (type (or null simple-vector) hoists) (type (integer 0 4096) fuel))
    (dotimes (id (length regions))
      (let ((region (svref regions id)))
        (when region
          (dolist (block (svref region 3))
            (let* ((next (hir-block-id block)) (child (svref regions next)))
              (declare (type (unsigned-byte 32) next))
              (if (and child (not (eq child region)))
                  (setf (svref parents next) (svref region 0))
                  (setf (svref owners next) (svref region 0))))))))
    (dolist (block blocks)
      (dolist (value (hir-block-parameters block)) (setf (native-value-slot definitions value) block))
      (dolist (instruction (hir-block-instructions block))
        (when (hir-instruction-output instruction)
          (setf (native-value-slot definitions (hir-instruction-output instruction)) block))))
    (when *wasm-branch-tests*
      (maphash (lambda (end test)
        (when (and (second test)
                   (not (eq (first (hir-instruction-attributes end)) (second (hir-instruction-attributes end)))))
          (setf (gethash (second test) fused) t))) *wasm-branch-tests*))
    (dolist (block blocks)
      (let ((header (svref owners (the (unsigned-byte 32) (hir-block-id block)))))
        (when header
          (dolist (instruction (hir-block-instructions block))
            (let* ((output (hir-instruction-output instruction))
                   (capture (and (eq (hir-instruction-opcode instruction) :capture)
                                 (< (hir-function-debug-level function) 2)
                                 (eq (hir-value-type output) t)
                                 (not (eq (hir-value-kind output) :multiple))))
                   (kind (if capture :capture
                             (and (eq (hir-instruction-opcode instruction) :call)
                                  (gethash instruction *wasm-inline-simple-calls*)))))
              (when (and kind
                         (not (and *wasm-proven-integer-arithmetic*
                                   (gethash instruction *wasm-proven-integer-arithmetic*)))
                         (or (and (integerp kind) (or (<= #x50 kind #x59) (member kind '(#x7c #x7d #x7e #x83 #x84 #x85))))
                                  (member kind '(:eq :null :consp :simple-vector-p :vectorp :stringp :symbolp :integerp :characterp :evenp :oddp :capture)))
                         (or capture (native-value-slot *wasm-scalar-groups* output))
                         (or (native-value-slot *wasm-live-values* output) (gethash instruction fused))
                         (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output))))
                ;; Try each containing loop from inside outward. A failed outer
                ;; proof keeps the already established inner placement.
                (loop repeat 16 for target = header then (svref parents (the (unsigned-byte 32) (hir-block-id target)))
                      while (and target (not (eq target entry))) do
                  (when (zerop fuel) (return-from native-plan-loop-hoists hoists))
                  (decf fuel)
                  (let ((arguments nil) (widths nil) (valid t)
                        (numeric (or (integerp kind) (member kind '(:evenp :oddp)))))
                    (dolist (input (cddr (hir-instruction-inputs instruction)))
                      (let ((operand (native-hoist-source input target query)))
                        (unless operand (setq valid nil) (return))
                        (push operand arguments)
                        (when numeric
                          (let ((width (native-hoist-width operand)))
                            (unless width (setq valid nil) (return)) (push width widths)))))
                    (when (and valid (integerp kind) (>= kind #x60))
                      (unless (native-hoist-immediate-result-p instruction (reverse arguments)) (setq valid nil)))
                    (unless valid (return))
                    (unless hoists (setq hoists (native-value-table function)))
                    ;; Original instruction, canonical plan, available operands,
                    ;; independently proven widths, and destination loop header.
                    (setf (native-value-slot hoists output)
                      (let ((operands (nreverse arguments)))
                        (vector instruction kind operands (nreverse widths) target
                          (remove-if (lambda (value)
                            (let ((producer (hir-value-producer value)))
                              (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant)))) operands)
                          ;; Fused tests have a proven single-use suffix. Keep
                          ;; their cached condition raw when no Lisp value is live.
                          (and (gethash instruction fused)
                               (not (native-value-slot *wasm-live-values* output))))))))))))))
    hoists))

(defun native-finalize-loop-hoists (function blocks hoists)
  (declare (type simple-vector function hoists) (type list blocks))
  (let ((prefixes (make-array (hir-function-block-count function) :initial-element nil)))
    (declare (type simple-vector prefixes))
    ;; Preserve source instruction order within each prefix. All selected
    ;; operations are independent, total and nonallocating; no body type
    ;; assertion or runtime function lookup is evaluated speculatively.
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (let* ((output (hir-instruction-output instruction)) (record (native-value-slot hoists output)))
          (when record
            (if (or (svref record 6) (native-value-slot *wasm-live-values* output))
                (push record (svref prefixes (the (unsigned-byte 32) (hir-block-id (svref record 4)))))
                (setf (native-value-slot hoists output) nil))))))
    (dotimes (id (length prefixes)) (setf (svref prefixes id) (nreverse (svref prefixes id))))
    prefixes))

(defun emit-native-invariant-value (value)
  (declare (type simple-vector value))
  (let ((producer (hir-value-producer value)))
    (if (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
        (wasm-literal (first (hir-instruction-attributes producer))) (wasm-get value))))

(defun wasm-s64 (number)
  (declare (type (signed-byte 64) number))
  (loop
    (let ((byte (logand number 127)))
      (declare (type (unsigned-byte 7) byte))
      (setq number (ash number -7))
      (let ((done (or (and (= number 0) (< byte 64)) (and (= number -1) (>= byte 64)))))
        (wasm-byte (logior byte (if done 0 128))) (when done (return))))))

(defun emit-native-hoisted-primitive (record)
  (declare (type simple-vector record))
  (compiler-statistic :optimization/loop-hoist (svref record 1))
  (let ((kind (svref record 1)) (arguments (svref record 2)) (widths (svref record 3)))
    (cond
      ((and (member kind '(#x53 #x55 #x57 #x59)) (> (length arguments) 2))
       (emit-native-integer-chain arguments widths kind t)
       (unless (svref record 6) (wasm-call "native-boolean")))
      ((eq kind :capture)
       ;; Closure capture slots are immutable. Cache the binding cell's address,
       ;; not its mutable contents; subsequent CELL-READs stay in the loop.
       (wasm-local 0)
       (wasm-integer (first (hir-instruction-attributes (svref record 0))))
       (wasm-call "native-capture"))
      ((or (integerp kind) (member kind '(:evenp :oddp)))
       (loop for input in arguments do
         (let* ((producer (hir-value-producer input))
                (literal (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant)
                              (first (hir-instruction-attributes producer)))))
           (if (integerp literal)
               (progn (wasm-byte #x42) (wasm-s64 literal))
               (progn (wasm-get input)
                 (wasm-call "native-unbox-int64")))))
       (cond ((member kind '(:evenp :oddp))
              (wasm-byte #x42) (wasm-s32 1) (wasm-byte #x83)
              (wasm-byte (if (eq kind :evenp) #x50 #xa7)))
             (t (when (and (null (cdr arguments)) (/= kind #x50))
                  (wasm-byte #x42) (wasm-s32 (cond ((member kind '(#x7c #x7d)) 1) ((= kind #x85) -1) (t 0))))
                (wasm-byte kind)))
       (unless (svref record 6)
         (if (and (integerp kind) (>= kind #x60))
             (emit-native-integer-box (hir-instruction-output (svref record 0)))
             (wasm-call "native-boolean"))))
      ((and (svref record 6) (eq kind :eq))
       (if (null (hir-instruction-attributes (svref record 0)))
           (progn
             (dolist (input arguments) (emit-native-invariant-value input))
             (wasm-call "native-eq-test"))
           (progn
             (dolist (input arguments) (emit-native-invariant-value input) (wasm-gc #x1a) (wasm-gc #x17 #x6d))
             (wasm-byte #xd3))))
      ((eq kind :consp)
       (emit-native-tag-test (first arguments) 7 t nil (null (hir-instruction-attributes (svref record 0)))
         (not (svref record 6))))
      ((eq kind :symbolp)
       (emit-native-tag-test (first arguments) 6 t t)
       (unless (svref record 6) (wasm-call "native-boolean")))
      (t
       (dolist (input arguments) (emit-native-invariant-value input))
       (case kind (:eq (wasm-call "native-eq-inline"))
                  (:simple-vector-p (wasm-call "native-simple-vector-p-inline"))
                  (:vectorp (wasm-call "native-vector-p-inline"))
                  (:stringp (wasm-call "native-string-p-inline"))
                  (:integerp (wasm-call "native-integer-p-inline"))
                  (:characterp (wasm-call "native-character-p-inline"))
                  (:null (wasm-byte #xd1) (unless (svref record 6) (wasm-call "native-boolean"))))))
    (if (svref record 6)
        (progn (wasm-byte #x21) (wasm-u32 (svref record 6)))
        (wasm-set (hir-instruction-output (svref record 0))))))

;; Emitter-owned physical storage. This is never a logical type proof.
(defvar *wasm-tiered-values* nil)
(defvar *wasm-tiered-comparisons* nil)

(defun native-tiered-unary (instruction)
  (when (and (eq (hir-instruction-opcode instruction) :call)
             (member (length (hir-instruction-inputs instruction)) '(3 4)))
    (let* ((reference (second (hir-instruction-inputs instruction)))
           (producer (hir-value-producer reference)))
      (when (and (%hir-instruction-p producer)
                 (eq (hir-instruction-opcode producer) :function-reference)
                 (not (eq (second (hir-instruction-attributes producer)) 'notinline)))
        (let* ((name (first (hir-instruction-attributes producer)))
               (inputs (hir-instruction-inputs instruction))
               (unary (= (length inputs) 3))
               (literal (and (not unary) (hir-value-producer (fourth inputs)))))
          (and (if unary (member name '(1+ 1-))
                   (and (member name '(+ -)) (%hir-instruction-p literal)
                        (eq (hir-instruction-opcode literal) :constant)
                        (eql (first (hir-instruction-attributes literal)) 1)))
               ;; These checked binary calls poll just like unary stepping.
               ;; Existing raw integer plans keep their own checkpoint rules.
               (or (eq (gethash instruction *wasm-inline-simple-calls*) :standard-call)
                   (and unary *wasm-proven-integer-arithmetic*
                        (gethash instruction *wasm-proven-integer-arithmetic*)))
               (if (member name '(1+ +)) '1+ '1-)))))))

(defun native-tiered-source (value)
  ;; Do not follow arbitrary PHIs or client capture metadata.
  (loop repeat 32 do
    (let ((producer (hir-value-producer value)))
      (unless (and (%hir-instruction-p producer)
                   (member (hir-instruction-opcode producer) '(:copy :primary)))
        (return-from native-tiered-source value))
      (setq value (first (hir-instruction-inputs producer)))))
  nil)

(defun native-plan-tiered-counters (function blocks raw)
  "Admit private canonical successor/predecessor cells after existing raw facts.
All physical entry checks are nonsignaling. Exhaustion discards the whole plan."
  ;; Three physical tiers duplicate classification, update and materialization
  ;; arms. Keep that growth local to speed-oriented callers; proven raw storage
  ;; and shared checked arithmetic remain available under compact policy.
  (when (or (let ((policy (hir-function-optimization-policy function)))
              (<= (logand policy 3) (logand 3 (ash policy -2))))
            (>= (hir-function-debug-level function) 2)
            (hir-block-predecessors (hir-function-entry function))
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*)))
    (return-from native-plan-tiered-counters nil))
  (native-compute-tiered-counters function blocks raw))

(defun native-compute-tiered-counters (function blocks raw)
  (let ((cells (native-value-table function)) (table (native-value-table function))
        (chosen nil) (fuel 4096) (emitted (make-hash-table :test 'eq)))
    (dolist (block blocks) (setf (gethash block emitted) t))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
        (let ((cell (hir-instruction-output instruction)))
          (when (and (eq (hir-instruction-opcode instruction) :cell-new)
                     (native-value-slot *wasm-local-cells* cell)
                     (native-value-slot *wasm-live-values* cell)
                     *wasm-optimization-barriers*
                     (not (gethash cell *wasm-optimization-barriers*))
                     (not (and raw (native-value-slot raw cell)))
                     (eq (hir-value-kind (first (hir-instruction-inputs instruction))) :single))
            ;; Read count, write count, canonical update instructions, valid.
            (setf (native-value-slot cells cell) (vector 0 0 nil t))))))
    ;; Audit all actual operands, including unreachable/captured paths.
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
        (loop for input in (hir-instruction-inputs instruction) for index from 0 do
          (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
          (let ((record (native-value-slot cells input)))
            (when record
              (let ((opcode (hir-instruction-opcode instruction)))
                (cond
                  ((and (gethash block emitted) (zerop index) (eq opcode :cell-read)
                        (eq (hir-value-kind (hir-instruction-output instruction)) :single)
                        (not (and raw (native-value-slot raw (hir-instruction-output instruction)))))
                   (incf (svref record 0)))
                  ((and (gethash block emitted) (zerop index) (eq opcode :cell-write)
                        (eq (hir-value-kind (hir-instruction-output instruction)) :single)
                        (eq (hir-value-kind (second (hir-instruction-inputs instruction))) :single))
                   (let* ((source (native-tiered-source (second (hir-instruction-inputs instruction))))
                          (update (and source (hir-value-producer source)))
                          (name (and (%hir-instruction-p update) (native-tiered-unary update)))
                          (argument (and name (native-tiered-source (third (hir-instruction-inputs update)))))
                          (read (and argument (hir-value-producer argument))))
                     (if (and name (%hir-instruction-p read)
                              (eq (hir-instruction-opcode read) :cell-read)
                              (eq (first (hir-instruction-inputs read)) input)
                              (native-value-slot *wasm-scalar-groups* source)
                              (not (and raw (native-value-slot raw source)))
                              (not (member update *wasm-tail-calls* :test #'eq)))
                         (progn (incf (svref record 1)) (pushnew update (svref record 2)))
                         (setf (svref record 3) nil))))
                  (t (setf (svref record 3) nil)))))))))
    (dolist (value (hir-function-values function))
      (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
      (let ((record (native-value-slot cells value)))
        (when (and record (svref record 3) (plusp (svref record 0)) (plusp (svref record 1)))
          ;; Four indices are assigned only after ordinary body-local layout.
          (setf (native-value-slot table value) (make-array 4))
          (push value chosen)
          (dolist (update (svref record 2))
            (setf (native-value-slot table (hir-instruction-output update)) (make-array 4))))))
    (when (or (null chosen) (> (length chosen) 4))
      (return-from native-compute-tiered-counters nil))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
        (let* ((opcode (hir-instruction-opcode instruction))
               (output (hir-instruction-output instruction))
               (cell (and (member opcode '(:cell-read :cell-write))
                          (member (first (hir-instruction-inputs instruction)) chosen :test #'eq))))
          (when (and cell (native-value-slot *wasm-live-values* output))
            (setf (native-value-slot table output) (make-array 4))))))
    ;; Flatten accepted single-value aliases, independently of HIR ID order.
    (dolist (value (hir-function-values function))
      (when (minusp (decf fuel)) (return-from native-compute-tiered-counters nil))
      (let* ((source (native-tiered-source value))
             (record (and source (native-value-slot table source))))
        (when (and record (not (member source chosen :test #'eq))
                   (or (eq (hir-value-kind value) :single)
                       (native-value-slot *wasm-scalar-groups* value)))
          (setf (native-value-slot table value) record))))
    (let ((records nil))
      (loop for record across table when record do (pushnew record records :test #'eq))
      (when (> (length records) 32) (return-from native-compute-tiered-counters nil)))
    table))

(defun native-plan-tiered-comparisons (blocks table)
  (when table
    (native-compute-tiered-comparisons blocks table)))

(defun native-compute-tiered-comparisons (blocks table)
  (let ((comparisons (make-hash-table :test 'eq)) (count 0))
      (dolist (block blocks)
        (dolist (instruction (hir-block-instructions block))
          (let* ((inputs (hir-instruction-inputs instruction))
                 (reference (and (eq (hir-instruction-opcode instruction) :call) (second inputs)))
                 (producer (and reference (hir-value-producer reference)))
                 (name (and (%hir-instruction-p producer)
                            (eq (hir-instruction-opcode producer) :function-reference)
                            (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                            (first (hir-instruction-attributes producer)))))
            (when (and (= (length inputs) 4) (member name '(< > <= >= = /=))
                       (or (native-value-slot table (third inputs))
                           (native-value-slot table (fourth inputs))))
              (when (> (incf count) 16) (return-from native-compute-tiered-comparisons :exhausted))
              (setf (gethash instruction comparisons)
                (vector (or (native-value-slot table (third inputs)) (make-array 4))
                        (or (native-value-slot table (fourth inputs)) (make-array 4))
                        (not (native-value-slot table (third inputs)))
                        (not (native-value-slot table (fourth inputs))) name))))))
    comparisons))

(defun native-assign-tiered-locals (table comparisons offset)
  (if (or table comparisons)
      (native-compute-tiered-locals table comparisons offset)
      0))

(defun native-compute-tiered-locals (table comparisons offset)
  (let ((records nil))
    (when table
      (loop for record across table when record do (pushnew record records :test #'eq)))
    (when comparisons
      (maphash (lambda (instruction plan)
                 (declare (ignore instruction))
                 (pushnew (svref plan 0) records :test #'eq)
                 (pushnew (svref plan 1) records :test #'eq)) comparisons))
    (let ((count (length records)))
      (loop for record in records for index from 0 do
        (dotimes (component 4) (setf (svref record component) (+ offset index (* component count)))))
      count)))

(defun emit-native-tiered-copy (source destination)
  (unless (eq source destination)
    (dotimes (i 4)
      (wasm-local (svref source i)) (wasm-byte #x21) (wasm-u32 (svref destination i)))))

(defun emit-native-tiered-small (record)
  (wasm-local (svref record 1)) (wasm-gc #x16 #x6c) (wasm-gc #x1d))

(defun emit-native-tiered-i64 (record)
  (wasm-local (svref record 0)) (wasm-byte #x45)
  (wasm-byte #x04) (wasm-type :i64)
  (emit-native-tiered-small record) (wasm-byte #xac)
  (wasm-byte #x05) (wasm-local (svref record 2)) (wasm-byte #x0b))

(defun emit-native-tiered-materialize (record)
  ;; Keep an existing original scalar object until arithmetic changes it.
  ;; This also preserves zero-trip object identity without another local.
  (wasm-local (svref record 3)) (wasm-byte #xd1)
  (wasm-local (svref record 0)) (wasm-integer 2) (wasm-byte #x46) (wasm-byte #x45)
  (wasm-byte #x71) (wasm-byte #x45)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (svref record 3))
  (wasm-byte #x05)
  (wasm-local (svref record 0)) (wasm-byte #x45)
  (wasm-byte #x04) (wasm-type :object)
  (wasm-local (svref record 1)) (wasm-gc #x1b)
  (wasm-byte #x05) (wasm-local (svref record 2)) (wasm-call "native-int64")
  (wasm-byte #x0b) (wasm-byte #x0b))

(defun emit-native-tiered-classify (record)
  ;; The incoming externref is consumed once. Test actual GC representation
  ;; before OBJ fields or the scalar unbox operation. Never signal here.
  (setq *wasm-object-types* t)
  (wasm-byte #x21) (wasm-u32 (svref record 3))
  (wasm-byte #x02) (wasm-byte #x40)
  (wasm-local (svref record 3)) (wasm-gc #x1a) (wasm-gc #x14 #x6c)
  (wasm-byte #x04) (wasm-byte #x40)
  (wasm-local (svref record 3)) (wasm-gc #x1a) (wasm-gc #x16 #x6c)
  (wasm-byte #x21) (wasm-u32 (svref record 1))
  (wasm-integer 0) (wasm-byte #x21) (wasm-u32 (svref record 0))
  (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b)
  (wasm-local (svref record 3)) (wasm-gc #x1a) (wasm-gc #x14 1)
  (wasm-byte #x04) (wasm-byte #x40)
  (wasm-local (svref record 3)) (wasm-gc #x1a) (wasm-gc #x16 1) (wasm-gc 2 1 0)
  (wasm-integer 63) (wasm-byte #x71) (wasm-integer 1) (wasm-byte #x46)
  (wasm-local (svref record 3)) (wasm-gc #x1a) (wasm-gc #x16 1) (wasm-gc 2 1 3)
  (wasm-byte #xd1) (wasm-byte #x71)
  (wasm-byte #x04) (wasm-byte #x40)
  (wasm-local (svref record 3)) (wasm-call "native-unbox-int64")
  (wasm-byte #x21) (wasm-u32 (svref record 2))
  (wasm-local (svref record 2)) (wasm-byte #x42) (wasm-s32 -1073741824) (wasm-byte #x59)
  (wasm-local (svref record 2)) (wasm-byte #x42) (wasm-s32 1073741823) (wasm-byte #x57)
  (wasm-byte #x71) (wasm-byte #x04) (wasm-byte #x40)
  (wasm-local (svref record 2)) (wasm-byte #xa7) (wasm-gc #x1c)
  (wasm-byte #x21) (wasm-u32 (svref record 1))
  (wasm-integer 0) (wasm-byte #x21) (wasm-u32 (svref record 0))
  (wasm-byte #x05) (wasm-integer 1) (wasm-byte #x21) (wasm-u32 (svref record 0))
  (wasm-byte #x0b) (wasm-byte #x0c) (wasm-u32 2)
  (wasm-byte #x0b) (wasm-byte #x0b)
  (wasm-integer 2) (wasm-byte #x21) (wasm-u32 (svref record 0))
  (wasm-byte #x0b))

(defun emit-native-tiered-update (source destination name)
  (let ((step (if (eq name '1+) 1 -1)))
    (wasm-byte #x02) (wasm-byte #x40)
    (wasm-local (svref source 0)) (wasm-byte #x45)
    (wasm-byte #x04) (wasm-byte #x40)
    ;; Standard unary arithmetic polls once after evaluation. Generic fallback
    ;; below performs its own original poll, and is not speculatively invoked.
    (wasm-call "native-poll")
    (emit-native-tiered-small source)
    (wasm-integer (if (= step 1) 1073741823 -1073741824)) (wasm-byte #x46)
    (wasm-byte #x04) (wasm-byte #x40)
    (emit-native-tiered-small source) (wasm-byte #xac)
    (wasm-byte #x42) (wasm-s32 step) (wasm-byte #x7c)
    (wasm-byte #x21) (wasm-u32 (svref destination 2))
    (wasm-integer 1) (wasm-byte #x21) (wasm-u32 (svref destination 0))
    (wasm-byte #x05)
    (emit-native-tiered-small source) (wasm-integer step) (wasm-byte #x6a) (wasm-gc #x1c)
    (wasm-byte #x21) (wasm-u32 (svref destination 1))
    (wasm-integer 0) (wasm-byte #x21) (wasm-u32 (svref destination 0))
    (wasm-byte #x0b)
    (wasm-null) (wasm-byte #x21) (wasm-u32 (svref destination 3))
    (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b)
    (wasm-local (svref source 0)) (wasm-integer 1) (wasm-byte #x46)
    (wasm-byte #x04) (wasm-byte #x40)
    (wasm-call "native-poll")
    (wasm-local (svref source 2))
    (wasm-byte #x42) (wasm-s64 (if (= step 1) 9223372036854775807 -9223372036854775808))
    (wasm-byte #x51) (wasm-byte #x04) (wasm-byte #x40)
    (wasm-local (svref source 2)) (wasm-byte #x42) (wasm-s32 1)
    (wasm-call (if (= step 1) "native-add-int64-checked" "native-sub-int64-checked"))
    (emit-native-tiered-classify destination)
    (wasm-byte #x05)
    (wasm-local (svref source 2)) (wasm-byte #x42) (wasm-s32 step) (wasm-byte #x7c)
    (wasm-byte #x21) (wasm-u32 (svref destination 2))
    (wasm-integer 1) (wasm-byte #x21) (wasm-u32 (svref destination 0))
    (wasm-null) (wasm-byte #x21) (wasm-u32 (svref destination 3))
    (wasm-byte #x0b) (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b)
    (wasm-integer (native-canonical-builtin-code name))
    (wasm-local (svref source 3)) (wasm-call "native-standard-call1")
    (emit-native-tiered-classify destination)
    (wasm-byte #x0b)))

(defun emit-native-tiered-instruction (instruction)
  (when *wasm-tiered-values*
    (emit-native-tiered-instruction-worker instruction)))

(defun emit-native-tiered-instruction-worker (instruction)
  (when *wasm-tiered-values*
    (let* ((opcode (hir-instruction-opcode instruction))
           (inputs (hir-instruction-inputs instruction))
           (output (hir-instruction-output instruction))
           (record (native-value-slot *wasm-tiered-values* output)))
      (cond
        ((and record (member opcode '(:copy :primary))) t)
        ((and record (eq opcode :cell-new))
         (wasm-get (first inputs)) (emit-native-tiered-classify record) t)
        ((and record (eq opcode :cell-read))
         (emit-native-tiered-copy (native-value-slot *wasm-tiered-values* (first inputs)) record) t)
        ((and (eq opcode :cell-write) (native-value-slot *wasm-tiered-values* (first inputs)))
         (let ((source (native-value-slot *wasm-tiered-values* (second inputs))))
           (emit-native-tiered-copy source (native-value-slot *wasm-tiered-values* (first inputs)))
           (when record (emit-native-tiered-copy source record))) t)
        ((and record (eq opcode :call))
         (emit-native-tiered-update (native-value-slot *wasm-tiered-values* (third inputs))
                                   record (native-tiered-unary instruction)) t)))))

(defun emit-native-tiered-comparison (instruction &optional raw)
  (when *wasm-tiered-comparisons*
    (emit-native-tiered-comparison-worker instruction raw)))

(defun emit-native-tiered-comparison-worker (instruction raw)
  (let ((plan (and *wasm-tiered-comparisons* (gethash instruction *wasm-tiered-comparisons*))))
    (when plan
      (let ((left (svref plan 0)) (right (svref plan 1)) (name (svref plan 4))
            (inputs (hir-instruction-inputs instruction)))
        ;; These operands already ran at their original producer positions.
        ;; Classification itself is total and has no scheduling checkpoint.
        (when (svref plan 2) (wasm-get (third inputs)) (emit-native-tiered-classify left))
        (when (svref plan 3) (wasm-get (fourth inputs)) (emit-native-tiered-classify right))
        (wasm-local (svref left 0)) (wasm-integer 2) (wasm-byte #x46) (wasm-byte #x45)
        (wasm-local (svref right 0)) (wasm-integer 2) (wasm-byte #x46) (wasm-byte #x45) (wasm-byte #x71)
        (wasm-byte #x04) (wasm-type :i32)
        ;; The ordinary ordered inline worker has no poll. Other canonical
        ;; comparison workers retain the checked standard call's checkpoint.
        (when (eq (gethash instruction *wasm-inline-simple-calls*) :standard-call)
          (wasm-call "native-poll"))
        (wasm-local (svref left 0)) (wasm-byte #x45)
        (wasm-local (svref right 0)) (wasm-byte #x45) (wasm-byte #x71)
        (wasm-byte #x04) (wasm-type :i32)
        (emit-native-tiered-small left) (emit-native-tiered-small right)
        (wasm-byte (ecase name (< #x48) (> #x4a) (<= #x4c) (>= #x4e) ((= /=) #x46)))
        (when (eq name '/=) (wasm-byte #x45))
        (wasm-byte #x05)
        (emit-native-tiered-i64 left) (emit-native-tiered-i64 right)
        (wasm-byte (ecase name (< #x53) (> #x55) (<= #x57) (>= #x59) (= #x51) (/= #x52)))
        (wasm-byte #x0b)
        (wasm-byte #x05)
        (cond
          ((member (gethash instruction *wasm-inline-simple-calls*)
                   '(:less-real :greater-real :not-greater-real :not-less-real))
           (emit-native-tiered-materialize left) (emit-native-tiered-materialize right)
           (wasm-integer (position name '(< > <= >=))) (wasm-call "native-ordered-compare-inline"))
          (t
           (wasm-integer (native-canonical-builtin-code name))
           (emit-native-tiered-materialize left) (emit-native-tiered-materialize right)
           (wasm-call "native-standard-call2")))
        (wasm-byte #xd1) (wasm-byte #x45) (wasm-byte #x0b)
        (unless raw (wasm-call "native-boolean")))
      t)))

(defun native-plan-unboxed-arguments (function blocks offset)
  "Keep repeated scalar reads of immutable argument values in i64 locals.
A real first read must occur in the entry block. Initialize at that original
read, preserving earlier validation and effects. Captured landing entries and
entry backedges keep the ordinary representation. HIR and snapshots stay boxed."
  (declare (type simple-vector function) (type list blocks) (type (unsigned-byte 32) offset))
  (when (or (>= (hir-function-debug-level function) 2)
            (hir-block-predecessors (hir-function-entry function))
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*)))
    (return-from native-plan-unboxed-arguments (values nil 0)))
  ;; Without a scalar operation in the entry there can be no dominating
  ;; first read. Skip the whole-function scan for that common case.
  (unless (some (lambda (instruction)
                  (let ((kind (gethash instruction *wasm-inline-simple-calls*)))
                    (or (integerp kind) (member kind '(:evenp :oddp))
                        (gethash instruction *wasm-inline-vector-calls*))))
                (hir-block-instructions (hir-function-entry function)))
    (return-from native-plan-unboxed-arguments (values nil 0)))
  (let ((counts nil) (chosen nil) (table nil) (count 0) (fuel 4096))
    (declare (type (or null hash-table) counts chosen) (type (or null simple-vector) table)
             (type (unsigned-byte 32) count) (type (integer -1 4096) fuel))
    (labels ((observe (value entryp)
               (let* ((source (native-predicate-source value *wasm-reused-values*))
                      (producer (hir-value-producer source)))
                 (when (and (%hir-instruction-p producer)
                            (eq (hir-instruction-opcode producer) :argument)
                            (zerop (hir-instruction-safety-level producer)))
                   (let ((width (native-compute-integer-width value)))
                     (when width
                       (unless counts (setq counts (make-hash-table :test 'eq)))
                       (let ((record (or (gethash source counts)
                                         (setf (gethash source counts) (vector 0 nil width)))))
                         (incf (svref record 0))
                         (setf (svref record 2) (max width (svref record 2)))
                         (when entryp (setf (svref record 1) t)))))))))
      (dolist (block blocks)
        (let* ((entryp (eq block (hir-function-entry function)))
               (test (and *wasm-branch-tests* (gethash (hir-block-terminator block) *wasm-branch-tests*)))
               (fused (second test)))
          (dolist (instruction (hir-block-instructions block))
            (when (minusp (decf fuel)) (return-from native-plan-unboxed-arguments (values nil 0)))
            (let* ((output (hir-instruction-output instruction))
                   (inputs (hir-instruction-inputs instruction))
                   (kind (and (eq (hir-instruction-opcode instruction) :call)
                              (gethash instruction *wasm-inline-simple-calls*))))
              (when (and (or (eq instruction fused) (native-value-slot *wasm-live-values* output))
                         (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output)))
                         (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output))))
                (cond ((or (integerp kind) (member kind '(:evenp :oddp)))
                       (dolist (input (cddr inputs)) (observe input entryp)))
                      ((gethash instruction *wasm-inline-vector-calls*)
                       (observe (if (= (length inputs) 5) (fifth inputs) (fourth inputs)) entryp)))))))))
    (when counts
      (maphash (lambda (source record)
                 (when (and (>= (svref record 0) 2) (svref record 1))
                   (unless chosen (setq chosen (make-hash-table :test 'eq)
                                        table (native-value-table function)))
                   (compiler-statistic :planning/unboxed-argument (svref record 2))
                   (setf (gethash source chosen) (vector (+ offset count) source (svref record 2) nil :argument))
                   (incf count))) counts))
    (when chosen
      (dolist (value (hir-function-values function))
        (let ((record (gethash (native-predicate-source value *wasm-reused-values*) chosen)))
          (when record (setf (native-value-slot table value) record)))))
    (values table count)))

(defun native-plan-unboxed-cell-values (function blocks table count offset)
  "Keep structurally private integer cells scalar across loop backedges.
All writes must fit i64; read results get separate SSA snapshots so a later
write cannot change an earlier observation. SAFETY 1 requires executable
initializer and store facts, independently of declarations. Unknown uses keep boxes."
  (declare (type simple-vector function) (type list blocks)
           (type (or null simple-vector) table) (type (unsigned-byte 32) count offset))
  (let ((cells nil) (fuel 4096) (chosen nil) (emitted (make-hash-table :test 'eq)))
    (declare (type (or null simple-vector) cells) (type (integer -1 4096) fuel))
    (dolist (block blocks)
      (when (minusp (decf fuel))
        (return-from native-plan-unboxed-cell-values (values table count)))
      (setf (gethash block emitted) t)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel))
          (return-from native-plan-unboxed-cell-values (values table count)))
        (let ((cell (hir-instruction-output instruction)))
          (when (and (eq (hir-instruction-opcode instruction) :cell-new)
                     (native-value-slot *wasm-local-cells* cell)
                     (native-value-slot *wasm-live-values* cell)
                     (or (zerop (hir-instruction-safety-level instruction))
                         (and (= (hir-instruction-safety-level instruction) 1)
                              *wasm-proven-cell-types* *wasm-optimization-barriers*
                              (not (gethash cell *wasm-optimization-barriers*))
                              (eq (hir-value-kind (first (hir-instruction-inputs instruction))) :single))))
            (let* ((provenp (= (hir-instruction-safety-level instruction) 1))
                   (width (if provenp
                              (native-type-integer-width
                                (gethash (first (hir-instruction-inputs instruction)) *wasm-proven-cell-types* t))
                              (native-compute-integer-width (first (hir-instruction-inputs instruction))))))
              (when (and width (<= width 64))
                (unless cells (setq cells (native-value-table function)))
                (setf (native-value-slot cells cell) (vector width 0 0 nil provenp))))))))
    (unless cells (return-from native-plan-unboxed-cell-values (values table count)))
    ;; Inspect operands rather than trusting client-visible capture flags.
    (dolist (block (hir-function-blocks function))
      (when (minusp (decf fuel))
        (return-from native-plan-unboxed-cell-values (values table count)))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (when (minusp (decf fuel))
          (return-from native-plan-unboxed-cell-values (values table count)))
        (let* ((opcode (hir-instruction-opcode instruction)) (inputs (hir-instruction-inputs instruction))
               (kind (and (eq opcode :call) (gethash instruction *wasm-inline-simple-calls*))))
          (when (or (integerp kind) (member kind '(:evenp :oddp)))
            (dolist (input (cddr inputs))
              (when (minusp (decf fuel))
                (return-from native-plan-unboxed-cell-values (values table count)))
              (let* ((source (native-predicate-source input *wasm-reused-values*))
                     (producer (hir-value-producer source))
                     (cell (and (%hir-instruction-p producer)
                                (eq (hir-instruction-opcode producer) :cell-read)
                                (first (hir-instruction-inputs producer))))
                     (record (and cell (native-value-slot cells cell))))
                (when record (setf (svref record 3) t)))))
          (loop for input in inputs for index from 0 do
            (when (minusp (decf fuel))
              (return-from native-plan-unboxed-cell-values (values table count)))
            (let ((record (native-value-slot cells input)))
              (when record
                (cond
                  ((and (svref record 4)
                        (or (not (gethash block emitted))
                            (not (hir-instruction-output instruction))
                            (not (eq (hir-value-kind (hir-instruction-output instruction)) :single))))
                   (setf (native-value-slot cells input) nil))
                  ((and (eq opcode :cell-read) (zerop index)) (incf (svref record 1)))
                  ((and (eq opcode :cell-write) (zerop index)
                        (or (svref record 4) (zerop (hir-instruction-safety-level instruction))))
                   (let ((width (if (svref record 4)
                                    (and (eq (hir-value-kind (second inputs)) :single)
                                         (native-type-integer-width (gethash (second inputs) *wasm-proven-cell-types* t)))
                                    (native-compute-integer-width (second inputs)))))
                     (if (and width (<= width 64))
                         (progn (setf (svref record 0) (max width (svref record 0)))
                                (incf (svref record 2)))
                         (setf (native-value-slot cells input) nil))))
                  (t (setf (native-value-slot cells input) nil)))))))))
    (dolist (value (hir-function-values function))
      (let ((record (native-value-slot cells value)))
        (when (and record (plusp (svref record 1)) (plusp (svref record 2)) (svref record 3))
          (unless table (setq table (native-value-table function)))
          (compiler-statistic :planning/unboxed-cell (svref record 0))
          (setq chosen t)
          (setf (native-value-slot table value)
            (vector (+ offset count) value (svref record 0) nil :cell 0 (svref record 4)))
          (incf count))))
    (unless chosen (return-from native-plan-unboxed-cell-values (values table count)))
    (when table
      (dolist (block blocks)
        (dolist (instruction (hir-block-instructions block))
          (let* ((opcode (hir-instruction-opcode instruction))
                 (output (hir-instruction-output instruction))
                 (cell (and (member opcode '(:cell-read :cell-write))
                            (native-value-slot table (first (hir-instruction-inputs instruction))))))
            (when (and cell (eq (svref cell 4) :cell)
                       (native-value-slot *wasm-live-values* output)
                       (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output)))
                       (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output))))
              (setf (native-value-slot table output) (vector (+ offset count) output (svref cell 2) nil :result 0))
              (incf count))))))
    (values table count)))

(defun native-snapshot-source (instruction function &optional call-only)
  (let* ((inputs (hir-instruction-inputs instruction))
         (kind (gethash instruction *wasm-inline-simple-calls*)))
    (when (and (eq (hir-instruction-opcode instruction) :call)
               (null (cddddr (cddr inputs))))
      (cond ((and (not call-only) (fourth inputs) (null (cddddr inputs)) (eq kind :standard-call)
                  (eq (native-scalar-call-name inputs) 'aref)
                  (not (native-vector-call-width inputs (hir-instruction-safety-level instruction))))
             :load)
            ((and (null kind) (not *compiler-bootstrapping*)
                  (< (logand 3 (ash (hir-function-optimization-policy function) -2)) 3)
                  (not (gethash instruction *wasm-inline-vector-calls*))
                  (not (native-known-callee (second inputs)))
                  (not (native-standard-operation (second inputs)))
                  (let ((reference (hir-value-producer (second inputs))))
                    (or (not (%hir-instruction-p reference))
                        (not (eq (hir-instruction-opcode reference) :function-reference))
                        (native-scalar-call-name inputs))))
             ;; Preserve the value produced by the original callable
             ;; observation, including a computed FUNCALL designator.
             :call)))))

(defun native-snapshot-masks (function blocks &optional forward)
  ;; Admit only a single same-block path to the checked mask.
  ;; The ordinary reference locals carry the miss arm through SSA copies.
  (let ((masks nil) (uses nil) (fuel 4096))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (minusp (decf fuel)) (return-from native-snapshot-masks nil))
        (when (eq (gethash instruction *wasm-inline-simple-calls*) :logand-general)
          (push (cons block instruction) masks))))
    (unless masks (return-from native-snapshot-masks nil))
    (setq uses (native-value-table function))
    (let ((plans (make-hash-table :test 'eq)) (owners (make-hash-table :test 'eq)))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel)) (return-from native-snapshot-masks nil))
          (setf (gethash instruction owners) block)
          (dolist (input (hir-instruction-inputs instruction))
            (when (minusp (decf fuel)) (return-from native-snapshot-masks nil))
            (setf (native-value-slot uses input)
                  (if (native-value-slot uses input) :shared instruction)))))
      (dolist (entry masks)
        (let* ((block (car entry)) (mask-call (cdr entry))
               (output (hir-instruction-output mask-call))
               (operands (cddr (hir-instruction-inputs mask-call))))
          (when (and (native-value-slot *wasm-live-values* output)
                     (native-value-slot *wasm-scalar-groups* output)
                     (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output)))
                     (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output))))
            (loop for (value mask-input) in (list operands (reverse operands))
                  for mask = (hir-primary-constant mask-input)
                  when (and (integerp mask) (<= 0 mask 9223372036854775807)) do
              (let ((consumer mask-call))
                (dotimes (depth 16)
                  (when (minusp (decf fuel)) (return-from native-snapshot-masks nil))
                  (let ((use (native-value-slot uses value))
                        (producer (hir-value-producer value)))
                    (unless (and (eq consumer use)
                                 (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* value)))
                                 (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value)))
                                 (not (and *wasm-checked-integer-copies*
                                           (gethash value *wasm-checked-integer-copies*))))
                      (return))
                    (when (and forward (eq producer block))
                      (setf (gethash block plans) :join
                            (gethash mask-call plans) (cons value mask))
                      (return))
                    (unless (and (eq block (gethash producer owners)) (%hir-instruction-p producer))
                      (return))
                    (cond
                      ((or (eq (hir-instruction-opcode producer) :copy)
                           (and (eq (hir-instruction-opcode producer) :primary)
                                (native-value-slot *wasm-scalar-groups*
                                  (first (hir-instruction-inputs producer)))))
                       (setq consumer producer value (first (hir-instruction-inputs producer))))
                      (t
                       (let ((source (and (native-value-slot *wasm-scalar-groups* value)
                                          (native-snapshot-source producer function))))
                         (when source
                           (setf (gethash producer plans) source
                                 (gethash mask-call plans) (cons value mask))))
                       (return))))))))))
      plans)))

(defun native-snapshot-operand (value table)
  (let* ((source (native-predicate-source value *wasm-reused-values*))
         (producer (hir-value-producer source))
         (source (if (eq (gethash producer *wasm-inline-simple-calls*) :left)
                     (native-predicate-source (third (hir-instruction-inputs producer)) *wasm-reused-values*)
                     source))
         (record (and table (native-value-slot table source))))
    (if (and record (eq (svref record 4) :result))
        (values record (if (> (length record) 6) (svref record 6)
                         (list 'signed-byte (svref record 2))))
        (let ((literal (hir-primary-constant value)))
          (when (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807))
            (values literal (hir-constant-type literal)))))))

(defun native-snapshot-bit-operation (inputs kind table)
  (when (and table (not *wasm-fixed-call-emitter*) (not *wasm-closed-worker-functions*)
             (= (length inputs) 4) (member kind '(:logand-general :ash-general :standard-call)))
    (let ((name (native-scalar-call-name inputs)))
      (when (member name '(logand logior logxor ash))
        (multiple-value-bind (left left-type) (native-snapshot-operand (third inputs) table)
          (multiple-value-bind (right right-type) (native-snapshot-operand (fourth inputs) table)
            (when (and left right (or (and (vectorp left) (> (length left) 6))
                                      (and (vectorp right) (> (length right) 6))))
              (let* ((type (hir-inline-result-type name (list left-type right-type)))
                     (width (native-type-integer-width type))
                     (opcode (case name (logand #x83) (logior #x84) (logxor #x85)
                               (ash (and (integerp right) (<= right 63)
                                         (if (minusp right) #x87 #x86))))))
                (when (and opcode width (<= width 64))
                  (values (list opcode
                                (and (eq kind :standard-call)
                                     (not (every #'native-integer-width (cddr inputs))))
                                (when (eq name 'ash) (if (minusp right) (min 63 (- right)) right))
                                (if (eq name 'ash) (list left) (list left right)))
                          type width))))))))))

(defun native-unboxed-join-input-p (record)
  (and record (eq (svref record 4) :result)
       ;; Character-code producers have a distinct materialization recipe.
       (not (native-immediate-integer-result-p record))))

(defun native-plan-unboxed-join (block table offset status reachable)
  (let ((parameter (first (hir-block-parameters block)))
        (incoming (hir-block-predecessors block))
        (type nil) (width 0) (boxed nil))
    (when (and parameter (cdr incoming)
               (native-value-slot *wasm-live-values* parameter)
               (native-value-slot *wasm-scalar-groups* parameter))
      ;; The completed acyclic order makes the inputs available before the
      ;; join. Check every predecessor, including any statically dead edge.
      (loop for predecessor in incoming repeat 4
            for end = (hir-block-terminator predecessor)
            for value = (case (hir-instruction-opcode end)
                          (:jump (first (hir-instruction-inputs end)))
                          (:block-return (third (hir-instruction-inputs end))))
            for record = (and value (native-value-slot table (native-predicate-source value *wasm-reused-values*)))
            for rest on incoming do
        (unless (and value (native-value-slot *wasm-scalar-groups* value)
                     (gethash predecessor reachable)
                     (eq block (first (hir-instruction-attributes end))))
          (return-from native-plan-unboxed-join nil))
        (cond ((native-unboxed-join-input-p record)
               (setq width (max width (svref record 2))
                     type (hir-type-join type (if (> (length record) 6) (svref record 6)
                                                (list 'signed-byte (svref record 2))))))
              ((and status (not (and record (eq (svref record 4) :array-snapshot))))
               (setq boxed t))
              (t (return-from native-plan-unboxed-join nil)))
        (unless (cdr rest)
          (return (when (plusp width)
                    (if boxed (vector offset parameter width nil :array-snapshot 0 status)
                        (vector offset parameter width nil :result 0 type)))))))))

(defun native-float-call-op (instruction)
  (when (and (%hir-instruction-p instruction)
             (eq (gethash instruction *wasm-inline-simple-calls*) :standard-call))
    (let ((inputs (hir-instruction-inputs instruction)))
      (when (and (fourth inputs) (null (cddddr inputs)))
        (case (native-scalar-call-name inputs) (+ 0) (- 1) (* 2))))))

(defun native-float-precision (value)
  (let ((type (and *wasm-inferred-types* (gethash value *wasm-inferred-types*))))
    (case type
      (single-float 3)
      (double-float 4)
      (otherwise
       (typecase (hir-primary-constant value)
         (single-float 3)
         (double-float 4))))))

(defun native-scalar-pair-plans (function candidates retained)
  ;; A precision hint buys a guarded operation, never an unchecked extraction.
  ;; A hit may omit its box only when the complete use scan finds a single
  ;; same-block consumer through transparent one-value forwarding.
  (let ((fuel 4096) (uses (native-value-table function)) (owners (make-hash-table :test 'eq))
          (claimed (make-hash-table :test 'eq)) (plans nil) (calls nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (when (minusp (decf fuel)) (return-from native-scalar-pair-plans nil))
        (setf (gethash instruction owners) block)
        (when (and retained (eq (hir-instruction-opcode instruction) :call)
                   (eq (native-snapshot-source instruction function t) :call))
          (push (list block instruction nil) calls))
        (dolist (value (hir-instruction-inputs instruction))
          (when (minusp (decf fuel)) (return-from native-scalar-pair-plans nil))
          ;; Predicate reuse can create additional users of a producer.
          (let ((value (or (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value)) value)))
            (setf (native-value-slot uses value)
                  (if (native-value-slot uses value) :shared instruction))))))
    (dolist (candidate (append candidates (nreverse calls)))
      (destructuring-bind (block producer kind) candidate
        (unless (gethash producer claimed)
          (let ((value (hir-instruction-output producer)))
            (when (native-value-slot *wasm-scalar-groups* value)
              (dotimes (depth 16)
                (let ((use (native-value-slot uses value)))
                  (unless (and use (eq (gethash use owners) block)
                               (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* value)))
                               (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* value))))
                    (return))
                  (case (hir-instruction-opcode use)
                    ((:primary :copy) (setq value (hir-instruction-output use)))
                    (:call
                     (let* ((inputs (hir-instruction-inputs use))
                            (transport
                              (cond ((native-float-call-op use)
                                     (or kind (native-float-precision (third inputs))
                                              (native-float-precision (fourth inputs))))
                                    ((and (null kind) (eq (gethash use *wasm-inline-simple-calls*) :character-code)) 2))))
                       (when (and transport (not (gethash use claimed))
                                  (eq (third inputs) value)
                                  (native-value-slot *wasm-scalar-groups* (hir-instruction-output use)))
                         (setf (gethash use claimed) t (gethash producer claimed) t)
                         (push (list producer transport use (null kind)) plans)))
                     (return))
                    (otherwise (return))))))))))
    plans))

(defun native-plan-unboxed-values (function blocks offset forward)
  "Choose transient i64 storage for proven bounded results and their aliases.
Arithmetic uses overflow-safe canonical plans. Fused character reads use the
shared target CHAR-CODE result rule while retaining the checked read. Generic
Lisp uses materialize the number; arithmetic consumers read it directly."
  (declare (type simple-vector function) (type list blocks) (type (unsigned-byte 32) offset))
  (when (or (>= (hir-function-debug-level function) 2)
            (hir-block-predecessors (hir-function-entry function))
            (and *wasm-optimization-barriers* (gethash function *wasm-optimization-barriers*)))
    (return-from native-plan-unboxed-values (values nil 0 0)))
  (multiple-value-bind (table count) (native-plan-unboxed-arguments function blocks offset)
    (declare (type (or null simple-vector) table) (type (unsigned-byte 32) count))
    (let ((fuel 4096) (statuses 0) (typed-result-p nil) (reachable nil) (float-pairs nil) (float-candidates nil)
          (retained-consumer nil)
          (float-enabled (and (null *wasm-fixed-call-emitter*) (null *wasm-closed-worker-functions*)
                              (/= 3 (logand 3 (ash (hir-function-optimization-policy function) -2)))))
          (forward (and forward (not *wasm-dynamic-bindings*)))
          (masks (when (and *wasm-snapshot-mask-functions*
                            (gethash (hir-function-entry function) *wasm-snapshot-mask-functions*))
                   (native-snapshot-masks function blocks forward))))
      (declare (type (integer -1 4096) fuel))
      (dolist (block blocks)
        (when (and forward typed-result-p (cdr (hir-block-predecessors block)))
          (when (null reachable)
            (setq reachable (make-hash-table :test 'eq))
            (dolist (candidate blocks)
              (when (minusp (decf fuel))
                (return-from native-plan-unboxed-values (values nil 0 0)))
              (setf (gethash candidate reachable) t)))
          (when (minusp (decf fuel 5))
            (return-from native-plan-unboxed-values (values nil 0 0)))
          (let ((join (native-plan-unboxed-join block table (+ offset count)
                        (when (and masks (gethash block masks)) (+ offset statuses)) reachable)))
            (when join
              (incf count)
              (when (eq (svref join 4) :array-snapshot) (incf statuses))
              (setf (native-value-slot table (svref join 1)) join))))
        (dolist (instruction (hir-block-instructions block))
          ;; A partial plan has not classified aliases or reference uses.
          ;; Discard it rather than emit repeated boxes from incomplete facts.
          (when (minusp (decf fuel)) (return-from native-plan-unboxed-values (values nil 0 0)))
          (let* ((output (hir-instruction-output instruction))
                 (kind (and (eq (hir-instruction-opcode instruction) :call)
                            (gethash instruction *wasm-inline-simple-calls*)))
                 (snapshot (and masks (gethash instruction masks)))
                 (private (and *wasm-private-scalar-sites*
                               (member instruction *wasm-private-scalar-sites* :test #'eq)))
                 (integer-op (and (integerp kind) (>= kind #x60)))
                 (character (or (and private (eq *wasm-private-scalar-type* 'character))
                                (and (eq kind :character-ref) (eq output *wasm-private-scalar-return*))))
                 (character-input
                   (and table (eq kind :character-code)
                        (native-value-slot table
                          (native-predicate-source (third (hir-instruction-inputs instruction)) *wasm-reused-values*))))
                 (character-code (or (eq kind :character-code-at)
                                     (and character-input (eq (svref character-input 4) :character-result))))
                 (bit-op (and typed-result-p (member kind '(:logand-general :ash-general :standard-call))))
                 (ingress (eq output *wasm-private-scalar-ingress*)))
            (when (and float-enabled (eq kind :character-code)) (setq retained-consumer t))
            (when (and float-enabled (eq kind :standard-call) (native-float-call-op instruction))
              (let* ((inputs (hir-instruction-inputs instruction))
                     (precision (or (native-float-precision (third inputs))
                                    (native-float-precision (fourth inputs)))))
                (when precision
                  (setq retained-consumer t)
                  (push (list block instruction precision) float-candidates))))
            (when (and (consp snapshot) (%hir-block-p (hir-value-producer (car snapshot)))
                       (not (and table (native-value-slot table (car snapshot)))))
              (setq snapshot nil))
            (when (and output (or snapshot private integer-op character character-code bit-op ingress)
                       (native-value-slot *wasm-live-values* output)
                       (or ingress (native-value-slot *wasm-scalar-groups* output))
                       (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output)))
                       (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output))))
              (multiple-value-bind (plan type bits)
                  (cond ((consp snapshot)
                         (let* ((source (native-value-slot table (car snapshot)))
                                (mask (cdr snapshot))
                                (mask-type (hir-inline-result-type 'logand (list t (hir-constant-type mask)))))
                           (values (if (and source (eq (svref source 4) :result))
                                       (list #x83 nil nil (list source mask))
                                       (list :mask source mask))
                                   mask-type (native-type-integer-width mask-type))))
                        (bit-op (native-snapshot-bit-operation (hir-instruction-inputs instruction) kind table)))
                (let* ((character-type (when character-code
                                         (hir-inline-result-type 'char-code '(character))))
                       (proof (and integer-op *wasm-proven-integer-arithmetic*
                                   (gethash instruction *wasm-proven-integer-arithmetic*)))
                       (width (cond (private *wasm-private-scalar-width*)
                                    (proof (svref proof 1))
                                    (character-type (native-type-integer-width character-type))
                                    (t 64)))
                       (record
                        (cond (character (vector (+ offset count) output 32 nil :character-result 0))
                              ((member snapshot '(:load :call))
                               (prog1 (vector (+ offset count) output 64 nil
                                              (if (eq snapshot :load) :array-snapshot :call-snapshot)
                                              0 (+ offset statuses))
                                 (incf statuses)))
                              (plan (vector (+ offset count) output bits nil :result 0 type plan))
                              ((and width (<= width 64)
                                    (or private integer-op character-type ingress))
                               (if (or character-type (and private *wasm-private-scalar-type*))
                                   (vector (+ offset count) output width nil :result 0
                                           (or character-type *wasm-private-scalar-type*))
                                   (vector (+ offset count) output width nil :result 0))))))
                  (when record
                    ;; Snapshot arithmetic needs a previously admitted exact
                    ;; typed result. Initial argument records cannot seed it.
                    (when (or plan character-type (and private *wasm-private-scalar-type*))
                      (setq typed-result-p t))
                    (unless table (setq table (native-value-table function)))
                    (compiler-statistic :planning/unboxed-result kind)
                    (setf (native-value-slot table output) record)
                    (incf count))))))))
    (dolist (plan (append (when (and (null *wasm-reusable-scalar-identity*)
                                    (member *wasm-reusable-scalar-result* '(3 4)))
                           (list (list (hir-value-producer *wasm-private-scalar-return*)
                                       *wasm-reusable-scalar-result* nil)))
                         (when (or float-candidates retained-consumer)
                           (native-scalar-pair-plans function float-candidates retained-consumer))))
      (destructuring-bind (producer kind consumer &optional retained) plan
        (let ((output (hir-instruction-output producer)))
          (unless (and table (or (native-value-slot table output)
                                (and consumer (native-value-slot table (hir-instruction-output consumer)))))
            (unless table (setq table (native-value-table function)))
            (unless float-pairs (setq float-pairs (make-hash-table :test 'eq)))
            (let ((record (vector (+ offset count) output kind nil :float-snapshot 0 (+ offset statuses) retained)))
              (setf (native-value-slot table output) record
                    (gethash producer float-pairs) record)
              (when consumer (setf (gethash consumer float-pairs) record)))
            (incf count)
            (incf statuses)))))
    (multiple-value-setq (table count)
      (native-plan-unboxed-cell-values function blocks table count offset))
    (when table
      ;; The fusion planner leaves CHAR-CODE as a transparent :LEFT call.
      ;; Its CHAR producer still owns the checked read and its raw snapshot.
      ;; Only that proved one-value forwarding edge may share this storage.
      (dolist (block blocks)
        (dolist (instruction (hir-block-instructions block))
          (when (and (eq (hir-instruction-opcode instruction) :call)
                     (eq (gethash instruction *wasm-inline-simple-calls*) :left))
            (let ((record (native-value-slot table
                            (native-predicate-source (third (hir-instruction-inputs instruction))
                              *wasm-reused-values*))))
              (when (and record (member (svref record 4) '(:result :character-result)))
                (setf (native-value-slot table (hir-instruction-output instruction)) record))))))
      (dolist (value (hir-function-values function))
        (let ((record (native-value-slot table (native-predicate-source value *wasm-reused-values*))))
          (when (and record (member (svref record 4) '(:result :character-result)))
            (setf (native-value-slot table value) record))))
      ;; Count reference uses after flattening aliases. Keep one original
      ;; box when several consumers would otherwise allocate repeated copies.
      (dolist (block blocks)
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (let* ((opcode (hir-instruction-opcode instruction))
                 (inputs (hir-instruction-inputs instruction))
                 (kind (and (eq opcode :call) (gethash instruction *wasm-inline-simple-calls*)))
                 (output (hir-instruction-output instruction))
                 (alias (and output (native-value-slot table output)))
                 (edge (and (member opcode '(:jump :block-return))
                            (first (hir-instruction-attributes instruction))))
                 (parameter (and edge (first (hir-block-parameters edge))))
                 (join (and parameter (native-value-slot table parameter)))
                 (vectorp (gethash instruction *wasm-inline-vector-calls*)))
            (unless (or (and (native-removable-p instruction)
                             (not (native-value-slot *wasm-live-values* output)))
                        (and alias (member (svref alias 4) '(:result :character-result))
                             (or (member opcode '(:copy :primary)) (eq kind :left))))
              (loop for input in inputs for index from 0 do
                (let ((record (native-value-slot table input)))
                  (when (and record (eq (svref record 4) :character-result)
                             (not (and (= index 2) (eq kind :character-set)
                                       (not (native-value-slot *wasm-live-values* output))))
                             (not (and (eq kind :character-code) (= index 2) alias
                                       (eq (svref alias 1) output))))
                    (incf (svref record 5)))
                  (when (and record (eq (svref record 4) :result)
                             (not (and join (eq (svref join 1) parameter)
                                       (member (svref join 4) '(:result :array-snapshot))
                                       (= index (if (eq opcode :jump) 0 2))
                                       (native-unboxed-join-input-p record)))
                             (not (and *wasm-private-scalar-argument*
                                       (= index (+ 2 *wasm-private-scalar-argument*))
                                       (member instruction *wasm-private-scalar-sites* :test #'eq)))
                             (not (and (>= index 2)
                                       (or (integerp kind)
                                           (member kind '(:evenp :oddp :proven-less-int64 :proven-greater-int64
                                                          :proven-not-greater-int64 :proven-not-less-int64)))))
                             (not (and vectorp (= index (1- (length inputs)))))
                             (not (and (= index 3) (native-raw-aref-index instruction inputs kind table
                                                     (not (and alias (eq (svref alias 4) :array-snapshot))))))
                             (not (and (= index 2) (native-raw-array-store instruction table)))
                             (not (and (>= index 2) alias (> (length alias) 7)
                                       (not (eq (first (svref alias 7)) :mask))))
                             (not (and (eq kind :character-code-at) (= index 3)))
                             (not (and (eq opcode :cell-write) (= index 1)
                               (let ((cell (native-value-slot table (first inputs))))
                                 (and cell (eq (svref cell 4) :cell))))))
                    (incf (svref record 5) (if (and (eq opcode :cell-write) (= index 1)) 2 1))))))))))
    (values table count statuses float-pairs))))

(defun native-checked-integer-bound-root (value)
  ;; Only actual checked SSA copies confer authority. Predicate reuse
  ;; can erase the identity of the check and is not a provenance query.
  (dotimes (depth 16)
    (when (gethash value *wasm-checked-integer-copies*)
      (return value))
    (let ((producer (hir-value-producer value)))
      (unless (and (%hir-instruction-p producer)
                   (eq (hir-instruction-opcode producer) :copy))
        (return nil))
      (setq value (first (hir-instruction-inputs producer))))))

(defun native-plan-unboxed-checked-copies (function structured table count offset tiered)
  (unless (and structured *wasm-checked-integer-copies*
               (not (hir-block-predecessors (hir-function-entry function))))
    (return-from native-plan-unboxed-checked-copies (values table count)))
  (let ((fuel 8192) (candidates nil))
    (labels ((charge (n)
               (when (minusp (decf fuel n))
                 (return-from native-plan-unboxed-checked-copies (values table count)))))
      (loop for region across (cdr structured) do
        (charge 1)
        (when region
          (dolist (block (svref region 3))
            (charge 1)
            (let* ((end (hir-block-terminator block)) (branch (native-active-branch-test end)))
              (dolist (instruction (hir-block-instructions block))
                (charge 1)
                (let ((output (hir-instruction-output instruction)))
                  (when (and (member (gethash instruction *wasm-inline-simple-calls*)
                                     '(:proven-less-int64 :proven-greater-int64
                                       :proven-not-greater-int64 :proven-not-less-int64))
                             (or (eq instruction (second branch))
                                 (native-value-slot *wasm-live-values* output))
                             (not (and tiered (gethash instruction tiered)))
                             (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* output)))
                             (not (and *wasm-hoisted-values* (native-value-slot *wasm-hoisted-values* output))))
                    (dolist (operand (cddr (hir-instruction-inputs instruction)))
                      (unless (and table (native-value-slot table operand))
                        (charge 16)
                        (let ((root (native-checked-integer-bound-root operand)))
                          (when (and root (native-value-slot *wasm-live-values* root)
                                     (not (and table (native-value-slot table root)))
                                     (not (and *wasm-reused-values* (native-value-slot *wasm-reused-values* root))))
                            (push (cons operand root) candidates)))))))))))))
    ;; The checked-copy planner only admits single-value argument copies
    ;; in entry. Verified SSA dominance covers each selected use. Commit
    ;; after scanning so fuel exhaustion never leaves a half-built plan.
    (dolist (candidate candidates)
      (unless table (setq table (native-value-table function)))
      (let* ((root (cdr candidate))
             (record (or (native-value-slot table root)
                         (prog1 (setf (native-value-slot table root)
                                      (vector (+ offset count) root 64 nil :checked-copy))
                           (incf count)))))
        (setf (native-value-slot table (car candidate)) record))))
  (values table count))

(defun native-ordinary-snapshot-p (value raw tiered &optional cellp)
  ;; These observations keep their ordinary reference representation.
  (when value
    (let ((slot (1+ (hir-value-id value))))
      (and (not (or (and raw (svref raw slot))
                    (and tiered (svref tiered slot))
                    (and *wasm-optimization-barriers* (gethash value *wasm-optimization-barriers*))))
           (if cellp
               (and (svref *wasm-local-cells* slot) (svref *wasm-live-values* slot))
               (and (or (eq (hir-value-kind value) :single) (svref *wasm-scalar-groups* slot))
                    (not (or (and *wasm-reused-values* (svref *wasm-reused-values* slot))
                             (and *wasm-hoisted-values* (svref *wasm-hoisted-values* slot))
                             (and *wasm-checked-integer-copies*
                                  (gethash value *wasm-checked-integer-copies*))))))))))

(defun native-vector-snapshot-source (value raw tiered)
  ;; Reference copies and scalar PRIMARY preserve identity. A checked copy,
  ;; independent read, or unknown path does not prove the same snapshot.
  (loop repeat 16 do
    (unless (native-ordinary-snapshot-p value raw tiered) (return nil))
    (let ((producer (hir-value-producer value)))
      (unless (and (%hir-instruction-p producer)
                   (case (hir-instruction-opcode producer)
                     (:copy t)
                     (:primary (native-value-slot *wasm-scalar-groups*
                                  (first (hir-instruction-inputs producer))))))
        (return value))
      (setq value (first (hir-instruction-inputs producer))))))

(defun native-vector-snapshot-call-p (instruction name arity kind)
  (when (and (%hir-instruction-p instruction)
             (eq (hir-instruction-opcode instruction) :call)
             (eql (hir-instruction-safety-level instruction) 1)
             (eq (gethash instruction *wasm-inline-simple-calls*) kind))
    (let* ((inputs (hir-instruction-inputs instruction))
           (reference (hir-value-producer (second inputs))))
      (and (null (nthcdr (+ 2 arity) inputs)) (nth (1+ arity) inputs)
           (%hir-instruction-p reference)
           (eq (hir-instruction-opcode reference) :function-reference)
           (eq (first (hir-instruction-attributes reference)) name)
           (not (eq (second (hir-instruction-attributes reference)) 'notinline))
           (not (member instruction *wasm-tail-calls*))
           (not (gethash instruction *wasm-inline-vector-calls*))
           (eql (standard-function-result-count name arity) 1)))))

(defun native-vector-snapshot-region (function header body test raw tiered budget)
  ;; The only loop entry initializes this private boxed cell to immediate zero.
  ;; A strict continuation under the physical i31 bound then admits an i31
  ;; snapshot in [0, 1073741822]. Its sole successor and write preserve i31
  ;; through the next header, even if that header exits the loop.
  (flet ((charge (amount)
           (when (minusp (decf (car budget) amount))
             (return-from native-vector-snapshot-region)))
         (first-input (instruction opcode)
           (when (and (%hir-instruction-p instruction)
                      (eq (hir-instruction-opcode instruction) opcode))
             (first (hir-instruction-inputs instruction))))
         (slot (table value) (native-value-slot table value))
         (jump-p (block target)
           (let ((end (hir-block-terminator block)))
             (and (eq (hir-instruction-opcode end) :jump)
                  (eq (first (hir-instruction-attributes end)) target)))))
   (let* ((end (hir-block-terminator header))
         (successors (hir-instruction-attributes end))
         (kind (third test)) (compare (second test))
         (name (if (eq kind :less-real) '< '>=))
         (previous (hir-block-predecessors header))
         (setup (if (eq (first previous) body) (second previous) (first previous)))
         (entry (hir-function-entry function)))
    (unless (and setup (not (eq setup body)) (not (eq setup header))
                 (eq (if (eq setup (first previous)) (second previous) (first previous)) body)
                 (null (cddr previous))
                 (eq (first (hir-block-predecessors body)) header)
                 (null (rest (hir-block-predecessors body)))
                 (null (hir-block-parameters header)) (null (hir-block-parameters body))
                 (jump-p setup header) (jump-p body header)
                 (member body successors)
                 (eq (eq kind :less-real)
                     (eq (if (fourth test) (second successors) (first successors)) body))
                 (native-vector-snapshot-call-p compare name 2 kind)
                 (not (and *wasm-hoisted-values*
                           (slot *wasm-hoisted-values* (hir-instruction-output compare)))))
      (return-from native-vector-snapshot-region))
    ;; Check every operand path that can supply the same snapshot.
    ;; Each path is limited to 16 ordinary reference aliases.
    (charge 48)
    (let* ((inputs (hir-instruction-inputs compare))
           (current (native-vector-snapshot-source (third inputs) raw tiered))
           (read (and current (hir-value-producer current)))
           (cell (first-input read :cell-read))
           (new (and cell (hir-value-producer cell)))
           (zero (first-input new :cell-new))
           (literal (and zero (hir-value-producer zero)))
           (bound (fourth inputs)) (bound-producer (hir-value-producer bound))
           ;; A value produced in entry or setup can be observed directly.
           ;; For a header copy, observe its already evaluated source.
           (root (first-input bound-producer :copy))
           (root-producer (and root (hir-value-producer root)))
           (write nil) (accesses nil) (binding nil) (read-seen nil) (new-seen nil)
           (bound-block nil) (root-block nil))
      (unless (and (native-ordinary-snapshot-p cell raw tiered t)
                   (native-ordinary-snapshot-p zero raw tiered)
                   (%hir-instruction-p literal) (eq (hir-instruction-opcode literal) :constant)
                   (eql (first (hir-instruction-attributes literal)) 0)
                   (native-ordinary-snapshot-p bound raw tiered)
                   (slot *wasm-live-values* bound)
                   (%hir-instruction-p bound-producer))
        (return-from native-vector-snapshot-region))
      (dolist (candidate (hir-function-bindings function))
        (charge 1)
        (when (eq (hir-binding-cell candidate) cell)
          (when (or binding (hir-binding-captured-p candidate))
            (return-from native-vector-snapshot-region))
          (setq binding candidate)))
      ;; Include all validated blocks and operands, even when outside the
      ;; emitted region. The only actual uses of the cell are read and write.
      (dolist (block (hir-function-blocks function))
        (charge 1)
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (charge 1)
          (cond ((eq instruction new) (setq new-seen (eq block setup)))
                ((eq instruction bound-producer) (setq bound-block block))
                ((eq instruction root-producer) (setq root-block block))
                ((eq instruction read) (setq read-seen (eq block header))))
          (let ((arguments (hir-instruction-inputs instruction)))
            (loop for operand in arguments for index from 0 do
              (charge 1)
              (when (eq operand cell)
                (cond ((and (zerop index) (eq instruction read) (eq block header)))
                      ((and (zerop index) (eq block body) (null write)
                            (eq (hir-instruction-opcode instruction) :cell-write))
                       (setq write instruction))
                      (t (return-from native-vector-snapshot-region)))))
            (when (and (eq block body)
                       (native-vector-snapshot-call-p instruction 'aref 2 :standard-call)
                       (progn (charge 16)
                              (eq (native-vector-snapshot-source (fourth arguments) raw tiered) current))
                       (native-ordinary-snapshot-p (hir-instruction-output instruction) raw tiered)
                       (slot *wasm-scalar-groups* (hir-instruction-output instruction)))
              (push instruction accesses)))))
      (setq root (cond ((or (eq bound-block setup) (eq bound-block entry)) bound)
                       ((and (eq bound-block header)
                             (or (eq root-block setup) (eq root-block entry)))
                        root)))
      (unless (and binding read-seen new-seen write accesses
                   (native-ordinary-snapshot-p root raw tiered)
                   (eq (hir-value-kind root) :single)
                   (slot *wasm-live-values* root)
                   (not (slot *wasm-inline-constants* root)))
        (return-from native-vector-snapshot-region))
      (let* ((next (native-vector-snapshot-source (second (hir-instruction-inputs write)) raw tiered))
             (step (and next (hir-value-producer next))))
        (unless (and (native-vector-snapshot-call-p step '1+ 1 :standard-call)
                     (slot *wasm-scalar-groups* next)
                     (native-ordinary-snapshot-p (hir-instruction-output write) raw tiered)
                     (eq (native-vector-snapshot-source (third (hir-instruction-inputs step)) raw tiered) current)
                     (member step (hir-block-instructions body)))
          (return-from native-vector-snapshot-region))
        ;; The total physical bound check and strict body edge establish the
        ;; comparison's integer domain and successor's operand interval.
        (let ((facts (make-hash-table :test 'eq)))
          (setf (gethash (third inputs) facts) 'integer (gethash bound facts) 'integer)
          (unless (and (hir-standard-property-negative-p name (cddr inputs) :may-signal facts)
                       (hir-type-proves-p (hir-inline-result-type '1+ '((integer 0 1073741822)))
                                          '(integer 1 1073741823)))
            (return-from native-vector-snapshot-region)))
        (vector header root compare step accesses nil nil))))))

(defun native-plan-vector-snapshots (function structured raw tiered tiered-comparisons offset)
  (declare (type simple-vector function) (type (unsigned-byte 32) offset))
  ;; Keep the common path native and enter the full audit only for an
  ;; active ordered comparison in a two-block loop. Bound the tail-call
  ;; membership checks and sparse region traversal before visiting operands.
  (unless (and structured *wasm-branch-tests* *wasm-optimization-barriers* (not *wasm-fixed-call-emitter*)
               (null (hir-block-predecessors (hir-function-entry function)))
               (null (nthcdr 16 *wasm-tail-calls*))
               (<= (length (cdr structured)) 4096))
    (return-from native-plan-vector-snapshots (values nil nil 0)))
  (let ((regions (cdr structured)) (budget nil) (candidates nil))
    (declare (type simple-vector regions))
    (loop for region across regions when region do
      (let* ((header (svref region 0)) (nodes (svref region 3)) (body (second nodes))
             (test (and (eq (first nodes) header) body (null (cddr nodes))
                        (not (svref regions (hir-block-id body)))
                        (let ((end (hir-block-terminator header)))
                          (when (member (third (gethash end *wasm-branch-tests*))
                                        '(:less-real :not-less-real))
                            (native-active-branch-test end))))))
        (when (and test (not (and tiered-comparisons (gethash (second test) tiered-comparisons))))
          (unless budget (setq budget (list (- 4096 (length regions)))))
          (let ((record (native-vector-snapshot-region function header body test raw tiered budget)))
            (when (minusp (car budget)) (return-from native-plan-vector-snapshots (values nil nil 0)))
            (when record (push record candidates))))))
    (when candidates
      (let ((prefixes (make-hash-table :test 'eq)) (operations (make-hash-table :test 'eq))
            (count 0))
        (dolist (record (nreverse candidates))
          (setf (svref record 5) (+ offset count) (svref record 6) (+ offset count 1)
                (gethash (svref record 0) prefixes) record
                (gethash (svref record 2) operations) record
                (gethash (svref record 3) operations) record)
          (dolist (access (svref record 4))
            (setf (gethash access operations) record))
          (incf count 2)
          (compiler-statistic :planning/vector-snapshot :raw-aref))
        (return-from native-plan-vector-snapshots (values prefixes operations count))))
    (values nil nil 0)))

(defun emit-native-vector-snapshot-prefix (record)
  (let ((bound (svref record 1)))
    (emit-native-i31-operand bound t)
    (wasm-byte #x04) (wasm-type :i32)
    (emit-native-i31-operand bound)
    (wasm-byte #x05) (wasm-integer -2147483648)
    (wasm-byte #x0b)
    (wasm-byte #x21) (wasm-u32 (svref record 5))))

(defun emit-native-vector-snapshot-mode (record type)
  (wasm-local (svref record 5)) (wasm-integer -2147483648) (wasm-byte #x47)
  (wasm-byte #x04) (wasm-type type))

(defun emit-native-vector-snapshot-comparison (call inputs kind)
  (let ((record (and *wasm-vector-snapshot-operations* (gethash call *wasm-vector-snapshot-operations*))))
    (when record
      (compiler-statistic :optimization/vector-snapshot :comparison)
      (emit-native-vector-snapshot-mode record :i32)
      (emit-native-i31-operand (third inputs))
      (wasm-byte #x22) (wasm-u32 (svref record 6))
      (wasm-local (svref record 5)) (wasm-byte (if (eq kind :less-real) #x48 #x4e))
      (wasm-byte #x05)
      (emit-native-i31-comparison inputs kind t)
      (wasm-byte #x0b)
      t)))

(defun native-plan-low-field-add (function ldb block raw tiered)
  (let ((fuel (list 1024)) (private (make-hash-table :test 'eq))
        (aliases nil) (references nil) (arguments nil))
    (labels ((decline () (return-from native-plan-low-field-add nil))
             (charge () (when (minusp (decf (car fuel))) (decline)))
             (snapshot (value)
               (unless (native-ordinary-snapshot-p value raw tiered) (decline))
               (hir-value-producer value))
             (call-reference (call name)
               (let* ((inputs (hir-instruction-inputs call))
                      (reference (second inputs))
                      (producer (and reference (snapshot reference))))
                 (unless (and (eq (gethash call *wasm-inline-simple-calls*) :standard-call)
                              (cdddr inputs) (null (cddddr inputs))
                              (eq (hir-standard-operation-leaf reference) name)
                              (eq (native-standard-operation reference) name)
                              (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                              (eql (standard-function-result-count name 2) 1)
                              (native-value-slot *wasm-single-calls* (hir-instruction-output call))
                              (snapshot (hir-instruction-output call))
                              (gethash reference *wasm-inline-references*)
                              (not (or (gethash call *wasm-inline-vector-calls*)
                                       (gethash call *wasm-proven-integer-arithmetic*)
                                       (member call *wasm-private-scalar-sites* :test #'eq)
                                       (native-named-call-p inputs function)
                                       (member call *wasm-tail-calls* :test #'eq))))
                   (decline))
                 (setf (gethash reference private) 0)
                 (push producer references)))
             (forward-input (producer)
               (let ((input (first (hir-instruction-inputs producer))))
                 (unless (or (eq (hir-instruction-opcode producer) :copy)
                             (and (eq (hir-instruction-opcode producer) :primary)
                                  (native-value-slot *wasm-scalar-groups* input)))
                   (decline))
                 (push producer aliases)
                 input))
             (argument-root (operand)
               (loop repeat 32 for value = operand then (forward-input producer)
                     for producer = (snapshot value) do
                 (charge)
                 (unless (and (%hir-instruction-p producer) (eq (hir-value-kind value) :single))
                   (decline))
                 (case (hir-instruction-opcode producer)
                   (:argument (return-from argument-root producer))
                   (:copy)
                   (otherwise (decline))))
               (decline)))
      (call-reference ldb 'ldb)
      (multiple-value-bind (uses width)
          (hir-literal-byte-field-type ldb function *wasm-optimization-barriers* fuel t)
        (unless uses (decline))
        (let* ((byte (hir-value-producer (caar uses)))
               (add (loop repeat 32
                          for value = (fourth (hir-instruction-inputs ldb)) then (forward-input producer)
                          for producer = (snapshot value) do
                      (charge)
                      (unless (%hir-instruction-p producer) (decline))
                      (setf (gethash value private) 0)
                      (when (eq (hir-instruction-opcode producer) :call) (return producer)))))
          (unless add (decline))
          (call-reference byte 'byte)
          (call-reference add '+)
          (loop for (value) in uses for producer = (snapshot value) do
            (setf (gethash value private) 0)
            (unless (eq producer byte) (forward-input producer)))
          (dolist (value (cddr (hir-instruction-inputs byte)))
            (let ((producer (snapshot value)))
              (unless (and (%hir-instruction-p producer)
                           (eq (hir-instruction-opcode producer) :constant))
                (decline))))
          (let ((operands (cddr (hir-instruction-inputs add))) (phase nil))
            ;; Both values use ordinary boxed snapshots. Starting at arguments
            ;; also rules out the literal +1 immediate step at any policy.
            (setq arguments (mapcar #'argument-root operands))
            (unless (every #'native-integer-width operands) (decline))
            (setf (gethash (hir-instruction-output ldb) private) 0)
            (dolist (instruction (hir-block-instructions block))
              (charge)
              (case (hir-instruction-opcode instruction)
                ((:debug-step :debug-resume) (decline)))
              (cond ((eq phase ldb) (decline))
                    ((eq instruction byte)
                     (when arguments (decline))
                     (setq phase byte))
                    ((eq instruction add)
                     (unless (eq phase byte) (decline))
                     (setq phase add))
                    ((eq instruction ldb)
                     (unless (eq phase add) (decline))
                     (setq phase ldb))
                    (phase
                     (unless (or (member instruction aliases :test #'eq)
                                 (member instruction references :test #'eq))
                       (decline)))
                    (t (setq arguments (remove instruction arguments :test #'eq))))
              (dolist (input (hir-instruction-inputs instruction))
                (charge)
                (let ((count (gethash input private)))
                  (when count
                    (unless (zerop count) (decline))
                    (setf (gethash input private) 1)))))
            (charge) (charge) ; the sole RETURN and its audited operand
            (unless (and (eq phase ldb)
                         (zerop (gethash (hir-instruction-output ldb) private)))
              (decline))
            (list* ldb byte add width :add 0 operands)))))))

(defun native-plan-low-field-chain (function ldb block raw tiered fuel shift-uses)
  (let ((shift (and shift-uses (hir-value-producer (caar shift-uses)))))
    (multiple-value-bind (byte-uses width position)
        (hir-literal-byte-field-chain ldb function *wasm-optimization-barriers* fuel (null shift))
      (unless (and byte-uses (or (null shift) (eql position 0)))
        (return-from native-plan-low-field-chain nil))
      (let ((byte (hir-value-producer (caar byte-uses)))
            (uses (make-hash-table :test 'eq))
            (transparent nil) (roots nil) (remaining 0) (phase nil))
        (labels ((decline () (return-from native-plan-low-field-chain nil))
                 (charge () (when (minusp (decf (car fuel))) (decline)))
                 (snapshot (value)
                   (unless (native-ordinary-snapshot-p value raw tiered) (decline))
                   (hir-value-producer value))
                 (expect-use (value consumer position)
                   (when (gethash value uses) (decline))
                   (setf (gethash value uses) (cons consumer position))
                   (incf remaining))
                 (call-reference (call name mode)
                   (let* ((inputs (hir-instruction-inputs call))
                          (reference (second inputs))
                          (producer (snapshot reference))
                          (output (hir-instruction-output call)))
                     (unless (and (eq (gethash call *wasm-inline-simple-calls*) mode)
                                  (cdddr inputs) (null (cddddr inputs))
                                  (eq (hir-standard-operation-leaf reference) name)
                                  (eq (native-standard-operation reference) name)
                                  (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                                  (eql (standard-function-result-count name 2) 1)
                                  (snapshot output)
                                  (native-value-slot *wasm-single-calls* output)
                                  (gethash reference *wasm-inline-references*)
                                  (not (or (gethash call *wasm-inline-vector-calls*)
                                           (gethash call *wasm-proper-list-checks*)
                                           (gethash call *wasm-proven-integer-arithmetic*)
                                           (member call *wasm-private-scalar-sites* :test #'eq)
                                           (native-named-call-p inputs function)
                                           (member call *wasm-tail-calls* :test #'eq))))
                       (decline))
                     (expect-use reference call 1)
                     (push producer transparent)))
                 (forward-input (producer)
                   (let ((input (first (hir-instruction-inputs producer))))
                     (unless (case (hir-instruction-opcode producer)
                               (:copy t)
                               (:primary (native-value-slot *wasm-scalar-groups* input)))
                       (decline))
                     (push producer transparent)
                     input))
                 (chain-uses (chain origin position)
                   (loop for use in chain
                         for value = (car use) for consumer = (cdr use)
                         for producer = (snapshot value) do
                     (unless (eq producer origin) (forward-input producer))
                     (expect-use value consumer (if (eq consumer ldb) position 0))))
                 (argument-root (operand)
                   (loop repeat 32 for value = operand then (forward-input producer)
                         for producer = (snapshot value) do
                     (charge)
                     (unless (and (%hir-instruction-p producer) (eq (hir-value-kind value) :single))
                       (decline))
                     (case (hir-instruction-opcode producer)
                       (:argument (return-from argument-root producer))
                       (:copy)
                       (otherwise (decline))))
                   (decline))
                 (audit-inputs (instruction)
                   (loop for input in (hir-instruction-inputs instruction) for position from 0 do
                     (charge)
                     (let ((use (gethash input uses)))
                       (when use
                         (unless (and (eq instruction (car use)) (eql position (cdr use)))
                           (decline))
                         (setf (cdr use) nil)
                         (decf remaining))))))
          (call-reference ldb 'ldb :standard-call)
          (call-reference byte 'byte :standard-call)
          (chain-uses byte-uses byte 2)
          (when shift
            (call-reference shift 'ash :ash-general)
            (chain-uses shift-uses shift 3))
          (dolist (value (cddr (hir-instruction-inputs byte)))
            (let ((producer (snapshot value)))
              (unless (and (%hir-instruction-p producer)
                           (eq (hir-instruction-opcode producer) :constant))
                (decline))))
          (let ((operands (if shift (cddr (hir-instruction-inputs shift))
                              (list (fourth (hir-instruction-inputs ldb))))))
            (setq roots (mapcar #'argument-root operands))
            (dolist (instruction (hir-block-instructions block))
              (charge)
              (let ((opcode (hir-instruction-opcode instruction)))
                (when (member opcode '(:debug-step :debug-resume)) (decline))
                (cond ((eq instruction byte)
                       (when (or phase roots) (decline))
                       (setq phase byte))
                      ((eq instruction shift)
                       (unless (eq phase byte) (decline))
                       (setq phase shift))
                      ((eq instruction ldb)
                       (unless (eq phase (or shift byte)) (decline))
                       (setq phase ldb))
                      ((null phase) (setq roots (remove instruction roots :test #'eq)))
                      ((eq phase ldb))
                      ((member instruction transparent :test #'eq))
                      ((and (eq opcode :constant) (snapshot (hir-instruction-output instruction))))
                      (t (decline))))
              (audit-inputs instruction))
            (charge)
            (audit-inputs (hir-block-terminator block))
            ;; Keep the LDB definition and all its suffix consumers. Use the
            ;; exact replacement inputs, including any copies still live there.
            (when (and (eq phase ldb) (zerop remaining))
              (list* ldb byte shift width (if shift :ash :extract) position operands))))))))

(defun native-plan-low-field-ash (function ldb block raw tiered)
  (let* ((fuel (list 1024)) (shift-uses (native-low-field-ash-chain ldb fuel)))
    (when shift-uses
      (native-plan-low-field-chain function ldb block raw tiered fuel shift-uses))))

(defun native-plan-literal-byte-extract (function block raw tiered)
  ;; Keep discovery and exact use audits in one bounded budget. This runs
  ;; after the arithmetic plans and cannot use their candidate discovery fuel.
  (let ((fuel (list 1024)))
    (dolist (instruction (hir-block-instructions block))
      (when (minusp (decf (car fuel)))
        (return-from native-plan-literal-byte-extract nil))
      (dolist (input (hir-instruction-inputs instruction))
        (declare (ignore input))
        (when (minusp (decf (car fuel)))
          (return-from native-plan-literal-byte-extract nil)))
      (when (and (eq (gethash instruction *wasm-inline-simple-calls*) :standard-call)
                 (eq (hir-standard-operation-leaf (second (hir-instruction-inputs instruction))) 'ldb))
        (let ((plan (native-plan-low-field-chain function instruction block raw tiered fuel nil)))
          (when plan (return-from native-plan-literal-byte-extract plan)))))))

(defun native-function-plan (function)
  (declare (type simple-vector function))
  ;; Cache the lowering facts separately from the emitted literals, closure
  ;; objects and relocation offsets.
  (let* ((targets (native-block-targets function))
        (forward (unless targets (native-forward-blocks function)))
        (blocks (or forward (native-reachable-blocks function targets)))
        (structured (unless (or forward targets) (native-loop-plan function blocks)))
        (direct (or forward structured))
        (*wasm-inline-constants* (native-inline-constants function blocks))
        (*wasm-local-cells* (native-direct-cells function))
        (*wasm-dynamic-bindings* (or targets (some (lambda (block)
          (some (lambda (instruction) (eq (hir-instruction-opcode instruction) :dynamic-bind))
            (hir-block-instructions block))) blocks)))
        (*wasm-entry-environment* (hir-function-environment function))
        (*wasm-full-groups* (native-full-groups function))
        (*wasm-scalar-groups* (native-scalar-groups function *wasm-full-groups*))
        ;; Planning a callee can run while its caller is being emitted.
        ;; Width caches use function-local IDs and cannot cross that boundary.
        (*wasm-integer-widths* nil)
        (*wasm-single-calls* (native-single-calls function *wasm-scalar-groups*))
        (*wasm-tail-calls* (native-tail-calls function))
        (*wasm-branch-tests* (native-plan-branch-tests function blocks))
        (*wasm-reused-values* nil)
        (*wasm-hoisted-values* nil)
        (*wasm-low-field* nil)
        (*wasm-live-values* (native-live-values function *wasm-dynamic-bindings* blocks))
        (reused (native-plan-predicate-reuse function blocks))
        (locals nil) (count 0) (literals nil) (prefixes nil) (raw-count 0) (unboxed nil) (wide-count 0)
        (tiered nil) (tiered-comparisons nil) (float-pairs nil)
        (snapshot-prefixes nil) (snapshot-operations nil)
        (literal-locals (make-hash-table :test 'eq))
        (dispatch (unless direct (native-dispatch-targets function targets blocks)))
        (next-blocks (make-array (hir-function-block-count function) :initial-element nil)))
    (declare (type (unsigned-byte 32) count) (type simple-vector next-blocks) (type list targets blocks literals)
             (type hash-table literal-locals) (type (or null simple-vector) locals))
    (when reused
      (native-predicate-aliases function blocks reused)
      (setq *wasm-reused-values* reused
            *wasm-live-values* (native-live-values function *wasm-dynamic-bindings* blocks)))
    (setq *wasm-hoisted-values* (native-plan-loop-hoists function blocks structured))
    (when *wasm-hoisted-values*
      ;; A formerly fused condition now reads its cached scalar result.
      (when *wasm-branch-tests*
        (maphash (lambda (end test)
          (let ((call (second test)))
            (when (and call (native-value-slot *wasm-hoisted-values* (hir-instruction-output call))
                       (not (svref (native-value-slot *wasm-hoisted-values* (hir-instruction-output call)) 6)))
              (setf (gethash end *wasm-branch-tests*) (list (first test) nil nil (fourth test)
                    (remove call (fifth test) :test #'eq)))))) *wasm-branch-tests*))
      (setq *wasm-live-values* (native-live-values function *wasm-dynamic-bindings* blocks)
            prefixes (native-finalize-loop-hoists function blocks *wasm-hoisted-values*)))
    ;; Keep i32 condition locals before object locals, like the existing
    ;; dispatcher local. Native bytecode decoding treats this prefix as pinned.
    (when prefixes
      (dotimes (id (length prefixes))
        (dolist (record (svref prefixes id))
          (when (svref record 6)
            (setf (svref record 6) (+ (if direct 2 3) raw-count)) (incf raw-count)))))
    (let ((statuses 0))
      (multiple-value-setq (unboxed wide-count statuses float-pairs)
        (native-plan-unboxed-values function blocks (+ (if direct 2 3) raw-count) forward))
      (when (plusp statuses)
        (let ((shifted (make-hash-table :test 'eq)))
          (loop for record across unboxed when (and record (not (gethash record shifted))) do
            (setf (gethash record shifted) t)
            (incf (svref record 0) statuses)))
        (incf raw-count statuses)))
    (setq tiered (native-plan-tiered-counters function blocks unboxed)
          tiered-comparisons (native-plan-tiered-comparisons blocks tiered))
    (when (eq tiered-comparisons :exhausted) (setq tiered nil tiered-comparisons nil))
    (multiple-value-setq (unboxed wide-count)
      (native-plan-unboxed-checked-copies function structured unboxed wide-count
        (+ (if direct 2 3) raw-count) tiered-comparisons))
    (when structured
      (let ((added 0))
        (multiple-value-setq (snapshot-prefixes snapshot-operations added)
          (native-plan-vector-snapshots function structured unboxed tiered tiered-comparisons
            (+ 2 raw-count)))
        (when snapshot-prefixes
          ;; Raw records can have multiple SSA aliases. Relocate each once.
          (when unboxed
            (let ((shifted (make-hash-table :test 'eq)))
              (loop for record across unboxed when (and record (not (gethash record shifted))) do
                (setf (gethash record shifted) t) (incf (svref record 0) added))))
          (incf raw-count added))))
    (when (and (null (cdr (hir-function-blocks function)))
               (null *wasm-dynamic-bindings*) (null *wasm-fixed-call-emitter*)
               (null *wasm-closed-worker-functions*)
               (< (hir-function-debug-level function) 2)
               *wasm-optimization-barriers* (not (gethash function *wasm-optimization-barriers*)))
      (let* ((block (hir-function-entry function)) (end (hir-block-terminator block))
             (inputs (hir-instruction-inputs end))
             (producer (and (eq (hir-instruction-opcode end) :return) (null (cdr inputs))
                            (hir-value-producer (first inputs)))))
        (when (and (%hir-instruction-p producer)
                   (eq (gethash producer *wasm-inline-simple-calls*) :standard-call)
                   (eq (hir-standard-operation-leaf (second (hir-instruction-inputs producer))) 'ldb))
          (setq *wasm-low-field* (native-plan-low-field-add function producer block unboxed tiered)))
        (unless *wasm-low-field*
          (let ((owner (or (and *wasm-worker-origins* (gethash function *wasm-worker-origins*))
                           function)))
            (unless (gethash owner *wasm-optimization-barriers*)
              (let ((ldb (and *wasm-low-field-ash-candidates*
                              (gethash owner *wasm-low-field-ash-candidates*))))
                (when ldb
                  (setq *wasm-low-field* (native-plan-low-field-ash function ldb block unboxed tiered))))
              (when (and (null *wasm-low-field*) (eq function owner)
                         *wasm-literal-byte-extract-owners*
                         (gethash owner *wasm-literal-byte-extract-owners*))
                (setq *wasm-low-field* (native-plan-literal-byte-extract function block unboxed tiered))))))
        (when *wasm-low-field*
          (setq *wasm-live-values* (native-live-values function nil blocks)))))
    ;; Preserve the shared active branch plan. Consumed comparisons emit a
    ;; raw predicate from tiered snapshots at EMIT-NATIVE-CONDITION; checked
    ;; comparisons on backward edges retain their original instruction position.
    (multiple-value-setq (locals count)
      (native-local-layout function blocks (+ (if direct 2 3) raw-count wide-count)))
    ;; Reuse the earlier CFG query; captured landing targets can force dispatch
    ;; even when the entry's graph is acyclic. Price caches after local layout
    ;; so their actual index widths enter the static size estimate.
    (setq literals (native-loop-literals blocks
                     (or forward (and targets (native-forward-blocks function)))
                     (+ (if direct 2 3) raw-count wide-count count)
                     (and *wasm-direct-workers* (gethash function *wasm-direct-workers*))))
    (loop for tail on blocks do
      (setf (svref next-blocks (the (unsigned-byte 32) (hir-block-id (car tail)))) (cadr tail)))
    (dolist (literal literals)
      (setf (gethash literal literal-locals) (+ (if direct 2 3) raw-count wide-count count)) (incf count))
    (vector targets forward *wasm-inline-constants* *wasm-local-cells*
            *wasm-dynamic-bindings* *wasm-entry-environment* *wasm-live-values*
            *wasm-scalar-groups* *wasm-single-calls* locals count literal-locals literals
            *wasm-tail-calls* (native-value-table function :unknown)
            dispatch (native-block-label-depths function blocks (cdr dispatch)) blocks next-blocks *wasm-branch-tests* structured reused *wasm-hoisted-values* prefixes raw-count unboxed wide-count tiered tiered-comparisons
            snapshot-prefixes snapshot-operations *wasm-low-field* *wasm-full-groups* float-pairs)))

(defun emit-native-function-body (function)
  (declare (type simple-vector function))
  ;; Boxed SSA values are externref locals. Acyclic and proven loop regions
  ;; use direct branches; arbitrary control-flow graphs retain the dispatcher.
  (let* ((*wasm-current-function* function)
         (*wasm-last-local* nil)
         (*wasm-last-restore* nil)
         ;; Unit initializers run once. Other stable closures may share
         ;; their function object, as permitted by CLHS FUNCTION.
         (*wasm-static-closures* (unless (and *wasm-literal-groups* (gethash function *wasm-literal-groups*))
                                  (make-hash-table :test 'eq)))
         (root (loop for root = function then (hir-function-parent root)
                    while (hir-function-parent root) finally (return root)))
        (group (and *wasm-literal-groups* (gethash root *wasm-literal-groups*)))
        (*wasm-literals* (if group (first group) *wasm-literals*))
        (*wasm-literal-index* (if group (second group) *wasm-literal-index*))
        (plan (or (and *wasm-function-plans* (gethash function *wasm-function-plans*))
                  (let ((plan (native-function-plan function)))
                    (when *wasm-function-plans* (setf (gethash function *wasm-function-plans*) plan))
                    plan)))
        (*wasm-named-call-plan* (native-plan-named-call-sites function plan))
        (targets (svref plan 0)) (forward (svref plan 1))
        (structured (svref plan 20)) (direct (or forward structured))
        (blocks (svref plan 17))
        (last-id (hir-block-id (car (last blocks))))
        (dispatch (svref plan 15))
        (*wasm-label-depths* (svref plan 16))
        (*wasm-next-blocks* (svref plan 18))
        (*wasm-branch-tests* (svref plan 19))
        (*wasm-reused-values* (svref plan 21))
        (*wasm-hoisted-values* (svref plan 22))
        (*wasm-loop-hoists* (svref plan 23))
        (raw-count (svref plan 24))
        (*wasm-unboxed-values* (svref plan 25))
        (wide-count (svref plan 26))
        (*wasm-tiered-values* (svref plan 27))
        (*wasm-tiered-comparisons* (svref plan 28))
        (*wasm-vector-snapshot-prefixes* (svref plan 29))
        (*wasm-vector-snapshot-operations* (svref plan 30))
        (*wasm-low-field* (svref plan 31))
        (*wasm-full-groups* (svref plan 32))
        (*wasm-float-pairs* (svref plan 33))
        (*wasm-dispatch-indices* (car dispatch))
        (*wasm-inline-constants* (svref plan 2)) (*wasm-local-cells* (svref plan 3))
        (*wasm-dynamic-bindings* (svref plan 4)) (*wasm-entry-environment* (svref plan 5))
        (*wasm-live-values* (svref plan 6)) (*wasm-scalar-groups* (svref plan 7))
        (*wasm-single-calls* (svref plan 8)) (*wasm-locals* (svref plan 9)) (count (svref plan 10))
        (*wasm-literal-locals* (svref plan 11))
        (*wasm-tail-calls* (svref plan 13)) (*wasm-integer-widths* (svref plan 14)))
   (declare (type simple-vector plan *wasm-integer-widths* *wasm-inline-constants*
                  *wasm-local-cells* *wasm-live-values* *wasm-scalar-groups* *wasm-full-groups*
                  *wasm-single-calls* *wasm-locals* *wasm-label-depths*)
            (type (unsigned-byte 32) count)
            (type hash-table *wasm-literal-locals*))
   (let ((*wasm-multiple-result-count* (and *wasm-multiple-result-functions*
                                          (gethash function *wasm-multiple-result-functions*)))
         (*wasm-multiple-result-locals* (native-value-table function))
         (*wasm-captured-result-locals* (native-value-table function))
         (captured-groups (native-captured-result-groups function blocks))
         (captured-count 0) (tiered-count 0)
         (scoped-calls
           (or (and *wasm-private-scalar-framed*
                    (some (lambda (site) (eq function (hir-value-owner (hir-instruction-output site))))
                          *wasm-private-scalar-sites*))
               (native-scoped-call-sites-p function plan))))
    ;; The primary and secondary results have ordinary reference locals, but
    ;; never an allocated Lisp packet. Dead component stores disappear during
    ;; the existing liveness/stack pass. Primary-only demand keeps its scalar
    ;; representation and does not allocate these locals at all.
    (let ((local-groups (native-local-result-groups function blocks))
          (worker-groups (or *wasm-multiple-result-count*
              (some (lambda (block)
                      (some (lambda (instruction)
                              (and (eq (hir-instruction-opcode instruction) :call)
                                   (native-multiple-call-count (hir-instruction-inputs instruction) instruction)))
                            (hir-block-instructions block))) blocks))))
     (when (or local-groups worker-groups)
      (dolist (value (hir-function-values function))
        (let* ((arity (gethash value *wasm-result-arities*))
               (origin (hir-value-producer value)))
          (when (and (eq (hir-value-kind value) :multiple)
                     (not (native-value-slot *wasm-scalar-groups* value))
                     arity
                     (or (and local-groups (native-value-slot local-groups value))
                         (and worker-groups (<= 2 arity 4)))
                     (or (not (%hir-instruction-p origin))
                         (member (hir-instruction-opcode origin) '(:values :copy))
                         (and (eq (hir-instruction-opcode origin) :call)
                              (native-multiple-call-count (hir-instruction-inputs origin) origin))))
            (let ((locals nil))
              (dotimes (i arity)
                (push (+ (if direct 2 3) raw-count wide-count count) locals) (incf count))
              (setf (native-value-slot *wasm-multiple-result-locals* value) (nreverse locals))))))))
   (when scoped-calls
     (setq *wasm-call-frame-locals* (list nil (+ (if direct 2 3) raw-count wide-count count)))
     (incf count))
   ;; Keep all existing raw and reference indices stable. Captured counts have
   ;; their own trailing I32 local group and force typed register allocation.
   (when captured-groups
     (dolist (value (hir-function-values function))
       (when (native-value-slot captured-groups value)
         (let ((locals nil))
           (dotimes (i 4)
             (push (+ (if direct 2 3) raw-count wide-count count) locals) (incf count))
           (setf (native-value-slot *wasm-captured-result-locals* value)
                 (append (nreverse locals) (list captured-count)))
           (incf captured-count))))
     (dolist (value (hir-function-values function))
       (let ((locals (native-value-slot *wasm-captured-result-locals* value)))
         (when locals (incf (fifth locals) (+ (if direct 2 3) raw-count wide-count count)))))
     (setq *wasm-captured-result-p* (plusp captured-count)))
   (setq tiered-count (native-assign-tiered-locals *wasm-tiered-values* *wasm-tiered-comparisons*
                        (+ (if direct 2 3) raw-count wide-count count captured-count)))
   (when scoped-calls
     (setf (first *wasm-call-frame-locals*)
       (+ (if direct 2 3) raw-count wide-count count captured-count (* 4 tiered-count))))
   (when *wasm-function-relocations*
     (setf (gethash function *wasm-function-relocations*) nil))
   (compiler-statistic :lowering/function-control
     (cond (structured :structured) (forward :forward) (t :dispatcher)))
   (when *wasm-unboxed-values*
     (loop for record across *wasm-unboxed-values* when record do (setf (svref record 3) nil)))
   (with-wasm-bytes
    (wasm-u32 (+ (if (or (not direct) (plusp raw-count)) 2 1)
                 (if (plusp wide-count) 1 0) (if (plusp captured-count) 1 0)
                 (if scoped-calls 1 0)
                 (if (plusp tiered-count) 4 0)))
    (when (or (not direct) (plusp raw-count))
      (wasm-u32 (+ (if direct 0 1) raw-count)) (wasm-type :i32))
    (when (plusp wide-count) (wasm-u32 wide-count) (wasm-type :i64))
    (wasm-u32 count) (wasm-type :object)
    (when (plusp captured-count) (wasm-u32 captured-count) (wasm-type :i32))
    (when (plusp tiered-count)
      (dolist (type '(:i32 :i31 :i64 :object))
        (wasm-u32 tiered-count) (wasm-type type)))
    (when scoped-calls (wasm-u32 1) (wasm-type :i32))
    ;; This entry is callable directly through its module export. Snapshot raw
    ;; foreign slot storage before initializers, literal resolution or callbacks.
    ;; Internal fixed workers remove this prologue when replacing packet reads
    ;; with their already evaluated scalar Wasm parameters.
    (wasm-local 1) (wasm-call "native-ingress-arguments") (wasm-byte #x21) (wasm-u32 1)
    (dolist (literal (svref plan 12))
      (let ((*wasm-literal-locals* nil)) (wasm-literal literal))
      (wasm-byte #x21) (wasm-u32 (gethash literal *wasm-literal-locals*)))
    (when (native-value-slot *wasm-live-values* (hir-function-environment function))
      (wasm-call "native-environment") (wasm-set (hir-function-environment function)))
    (when forward
      (emit-native-forward blocks)
      (return-from emit-native-function-body *wasm-bytes*))
    (when structured
      (let ((*wasm-structured-labels* (make-array (hir-function-block-count function) :initial-element nil))
            (*wasm-loop-regions* (cdr structured))
            (*wasm-control-depth* 0))
        (emit-native-structured-region (car structured) nil))
      (wasm-byte #x0b)
      (return-from emit-native-function-body *wasm-bytes*))
    ;; Fresh i32 locals already contain zero. No prologue work writes the
    ;; dispatcher local, so only a nonzero initial dispatch index needs a store.
    (let ((initial (svref *wasm-dispatch-indices* (hir-block-id (hir-function-entry function)))))
      (unless (zerop initial) (wasm-integer initial) (wasm-byte #x21) (wasm-u32 2)))
    (when targets
      (setq *wasm-exceptions* t)
      (wasm-byte #x03) (wasm-byte #x40) ; resume after a caught lexical exit
      (wasm-byte #x02) (wasm-byte #x40) ; exception landing pad
      ;; TRY_TABLE catches the existing payload-free runtime transfer tag.
      (dolist (byte '(#x1f #x40 1 0 0 0)) (wasm-byte byte)))
    (wasm-byte #x03) (wasm-byte #x40) ; dispatch loop
    (wasm-byte #x02) (wasm-byte #x40) ; invalid program counter
    (dotimes (i (svref *wasm-label-depths* last-id)) (wasm-byte #x02) (wasm-byte #x40))
    (wasm-local 2) (wasm-byte #x0e) (wasm-u32 (length (cdr dispatch)))
    (dolist (id (cdr dispatch)) (wasm-u32 (1- (svref *wasm-label-depths* id))))
    (wasm-u32 (svref *wasm-label-depths* last-id))
    ;; BR_TABLE selects a block in constant time. The earlier chain of IFs
    ;; rescanned every preceding block on each edge of a compiler's inner loop.
    (let ((*wasm-dispatch-depth* (1+ (svref *wasm-label-depths* last-id))) (depth 0))
     (loop for tail on blocks for block = (car tail) for next = (cadr tail) do
      (let ((current (svref *wasm-label-depths* (hir-block-id block))))
        (unless (= current depth)
          (wasm-byte #x0b) (decf *wasm-dispatch-depth*) (setq depth current)))
      (when *wasm-dynamic-bindings*
        (emit-native-restore (hir-block-environment block)))
      (dolist (instruction (hir-block-instructions block)) (emit-native-instruction instruction (hir-block-terminator block)))
      (let* ((terminator (hir-block-terminator block)) (inputs (hir-instruction-inputs terminator))
             (successors (hir-instruction-attributes terminator)))
        (ecase (hir-instruction-opcode terminator)
          (:unreachable (wasm-byte #x00))
          (:throw (emit-native-throw inputs))
          (:return (emit-native-return (first inputs)) (wasm-byte #x0f))
          (:jump (emit-native-jump (first successors) (first inputs) block))
          (:block-return
           (if successors (emit-native-jump (first successors) (third inputs) block)
               (emit-native-block-return inputs)))
          (:branch (emit-native-branch terminator block next))))
      ))
    (wasm-byte #x0b) (wasm-byte #x00) ; invalid program counter traps
    (wasm-byte #x0b) (wasm-byte #x00)
    (when targets
      (wasm-byte #x0b) (wasm-byte #x0b) ; try and landing pad
      (dolist (target targets)
        (wasm-get (hir-target-token target)) (wasm-call "native-block-match")
        (wasm-byte #x04) (wasm-byte #x40)
        (wasm-call "native-block-values")
        (let ((parameter (first (hir-block-parameters (hir-target-block target)))))
          (if parameter
              (progn (when (native-value-slot *wasm-scalar-groups* parameter) (wasm-call "native-primary"))
                     (wasm-set parameter))
              (wasm-byte #x1a)))
        (wasm-integer (svref *wasm-dispatch-indices* (hir-block-id (hir-target-block target))))
        (wasm-byte #x21) (wasm-u32 2)
        (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b))
      (wasm-byte #x08) (wasm-u32 0) ; propagate foreign transfers unchanged
      (wasm-byte #x0b) (wasm-byte #x00))
    (wasm-byte #x0b)))))

(defun native-literal-groups (entries)
  (let ((groups (make-hash-table :test 'eq)))
    (dolist (entry entries)
      ;; Nested bare entries use the same bank as their lexical root and
      ;; siblings, exactly as EMIT-NATIVE-FUNCTION-BODY selects it.
      (let* ((root (loop for root = entry then (hir-function-parent root)
                         while (hir-function-parent root) finally (return root)))
             (group (or (gethash root groups)
                        (setf (gethash root groups)
                          (list (make-array 16 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq))))))
        (setf (gethash entry groups) group)))
    groups))

(defun native-function-indices (functions)
  (declare (type list functions))
  (let ((indices (make-hash-table :test 'eq)) (index 0))
    (declare (type hash-table indices) (type (unsigned-byte 32) index))
    (dolist (function functions) (setf (gethash function indices) index) (incf index))
    indices))

(defun native-argument-usage (function)
  ;; Prove which operands a verified private worker reads. Captures by escaping
  ;; closures remain live; declarations alone never establish non-use.
  (when *wasm-argument-usages*
    (multiple-value-bind (usage found) (gethash function *wasm-argument-usages*)
      (when found (return-from native-argument-usage usage))))
  (let ((arity (native-fixed-arity function)))
    (when (and arity (< (hir-function-debug-level function) 2))
      (let* ((plan (or (gethash function *wasm-function-plans*)
                       (setf (gethash function *wasm-function-plans*) (native-function-plan function))))
             (live (svref plan 6)) (mask 0))
        (dolist (value (hir-function-values function))
          (let ((producer (hir-value-producer value)))
            (when (and (%hir-instruction-p producer)
                       (eq (hir-instruction-opcode producer) :argument)
                       (native-value-slot live value))
              (setq mask (logior mask (ash 1 (first (hir-instruction-attributes producer))))))))
        ;; An all-used mask supplies no useful optimization information.
        ;; This also avoids metadata for zero-argument initializers.
        (unless (= mask (1- (ash 1 arity))) mask)))))

(defun native-primary-functions (unit entries)
  ;; A code-origin fact is not an escape certificate. Audit every reference,
  ;; including dead instructions, before changing a private body's results.
  ;; Only direct calls, copies and completely unused lexical cells are admitted.
  (when (or *wasm-debug* *wasm-fixed-call-emitter* *wasm-closed-worker-functions*)
    (return-from native-primary-functions nil))
  (let ((candidates (make-hash-table :test 'eq)) (bad (make-hash-table :test 'eq))
        (used (make-hash-table :test 'eq)) (called (make-hash-table :test 'eq))
        (full (make-hash-table :test 'eq)) (fuel 65536))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel (+ 1 (hir-function-value-count function))))
        (return-from native-primary-functions nil))
      (dolist (block (hir-function-blocks function))
        (when (minusp (decf fuel)) (return-from native-primary-functions nil))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel)) (return-from native-primary-functions nil))
          (dolist (input (hir-instruction-inputs instruction))
            (when (minusp (decf fuel)) (return-from native-primary-functions nil))
            (setf (gethash input used) t)))))
    (dolist (function (hir-unit-functions unit))
      (when (and (gethash function *wasm-direct-workers*)
                 (null (hir-function-captures function))
                 (not (gethash function *wasm-optimization-barriers*))
                 (not (member function entries :test #'eq))
                 (some (lambda (block)
                         (some (lambda (instruction)
                                 (and (eq (hir-instruction-opcode instruction) :values)
                                      (cdr (hir-instruction-inputs instruction))))
                               (hir-block-instructions block)))
                       (hir-function-blocks function))
                 (zerop (hir-function-debug-level function))
                 (native-simple-callee-p function))
        (setf (gethash function candidates) t)))
    (when (zerop (hash-table-count candidates)) (return-from native-primary-functions nil))
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (let ((opcode (hir-instruction-opcode instruction))
                (output (hir-instruction-output instruction)))
            (loop for input in (hir-instruction-inputs instruction) for index from 0 do
              (let ((target (native-known-callee input)))
                (when (gethash target candidates)
                  (cond
                    ((and (eq opcode :call) (= index 1)
                          (eql (length (cddr (hir-instruction-inputs instruction))) (native-fixed-arity target)))
                     (setf (gethash target called) t)
                     (let ((demand (or (gethash function full)
                                       (setf (gethash function full) (native-full-groups function)))))
                       (when (native-value-slot demand output) (setf (gethash target bad) t))))
                    ((and (eq opcode :copy) (eq target (native-known-callee output))))
                    ((and (eq opcode :cell-new) (not (gethash output used))
                          (not (gethash output *wasm-optimization-barriers*))
                          (not (hir-binding-captured-p (first (hir-instruction-attributes instruction))))))
                    (t (setf (gethash target bad) t))))))))))
    (maphash (lambda (function present)
               (declare (ignore present))
               (unless (and (gethash function called) (not (gethash function bad)))
                 (remhash function candidates))) candidates)
    candidates))

(defun native-plan-argument-usages (unit)
  ;; Only exact HIR calls can shed operand roots. Plan their targets once with
  ;; no recursive mask queries, then invalidate callers whose dependencies can
  ;; shrink. All other cached function plans remain reusable.
  (when (or *wasm-fixed-call-emitter* *wasm-closed-worker-functions*)
    (return-from native-plan-argument-usages nil))
  (let ((targets (make-hash-table :test 'eq)) (callers (make-hash-table :test 'eq))
        (pending (make-hash-table :test 'eq)) (fuel 65536))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel (+ 1 (hir-function-value-count function))))
        (return-from native-plan-argument-usages nil))
      (dolist (block (hir-function-blocks function))
        (when (minusp (decf fuel)) (return-from native-plan-argument-usages nil))
        (dolist (instruction (hir-block-instructions block))
          (when (minusp (decf fuel)) (return-from native-plan-argument-usages nil))
          (dolist (input (hir-instruction-inputs instruction))
            (declare (ignore input))
            (when (minusp (decf fuel)) (return-from native-plan-argument-usages nil)))
          (when (eq (hir-instruction-opcode instruction) :call)
            (let* ((inputs (hir-instruction-inputs instruction))
                   (target (native-known-callee (second inputs))))
              (when (and target (gethash target *wasm-direct-workers*)
                         (eql (length (cddr inputs)) (native-fixed-arity target)))
                (setf (gethash target targets) t)
                (push function (gethash target callers))))))))
    (let ((*wasm-argument-usages* nil))
      (maphash (lambda (target present)
                 (declare (ignore present))
                 (setf (gethash target pending) (native-argument-usage target))) targets))
    (setq *wasm-argument-usages* pending)
    (maphash (lambda (target usage)
               (when usage
                 (dolist (caller (gethash target callers)) (remhash caller *wasm-function-plans*)))) pending)))

(defun native-direct-workers (functions)
  ;; These entries are private to this module. Public entries and exported
  ;; function objects retain the checked two-argument convention.
  (let ((forward (make-hash-table :test 'eq)) (reverse (make-hash-table :test 'eq)) (workers nil))
    (when (or (not *compiler-bootstrapping*) *wasm-bootstrap-direct-workers*)
      (dolist (function functions)
        (when (and
                   ;; The linked image loader creates the root factory through
                   ;; its ordinary packet entry, without direct-entry metadata.
                   ;; Its children attach their workers in the factory itself.
                   (or (not *compiler-bootstrapping*) (hir-function-parent function))
                   (< (hir-function-debug-level function) 2)
                   (native-fixed-arity function))
          (let ((worker (copy-seq function)))
            (setf (gethash function forward) worker (gethash worker reverse) function)
            (push worker workers)))))
    (values forward reverse (nreverse workers))))

(defun native-direct-worker-body (function body &optional raw-argument reusable)
  ;; Rewrite the compiler's own checked parameter reads into Wasm parameters.
  ;; Unknown shapes keep a general entry rather than guessing about stack use.
  (let* ((ops (native-read-ops body)) (arity (native-fixed-arity function))
         (argument (position "native-argument" *native-imports* :key #'first :test #'equal))
         (check (position "native-check-arguments" *native-imports* :key #'first :test #'equal))
         (ingress (position "native-ingress-arguments" *native-imports* :key #'first :test #'equal))
         (size (length ops)) (raw-reads 0))
    (labels ((op (index code &optional value (valuep nil))
               (and (< index size) (= (native-op-code (aref ops index)) code)
                    (or (not valuep) (eql (native-op-argument (aref ops index)) value)))))
      (with-wasm-bytes
        (wasm-append body 0 (native-op-start (aref ops 0)))
        (loop for i from 0 below size for record = (aref ops i) for code = (native-op-code record) do
          (cond
            ((op i #x20 1 t)
             (cond
               ((and (zerop i) (op (1+ i) #x10 ingress t) (op (+ i 2) #x21 1 t))
                (incf i 2))
               ((and (op (1+ i) #x41) (op (+ i 2) #x10 argument t)
                     (<= 0 (native-op-argument (aref ops (1+ i))))
                     (< (native-op-argument (aref ops (1+ i))) arity))
                (let ((index (native-op-argument (aref ops (1+ i)))))
                  (when (eql index raw-argument) (incf raw-reads))
                  (wasm-local (1+ index)))
                (incf i 2))
               ((and (op (1+ i) #x41 arity t) (op (+ i 2) #x41 arity t)
                     (op (+ i 3) #x10 check t)) (incf i 3))
               (t (return-from native-direct-worker-body nil))))
            ((member code '(#x20 #x21 #x22))
             (let ((index (native-op-argument record)))
               (when (= index 1) (return-from native-direct-worker-body nil))
               (wasm-byte code)
               (wasm-u32 (cond ((and reusable (<= #x10000000 index #x10000004))
                                (+ 5 (- index #x10000000)))
                               ((>= index 2) (+ index (if reusable 8 3)))
                               (t index)))))
            (t (wasm-append body (native-op-start record) (native-op-end record)))))
        (when (and raw-argument (/= raw-reads 1))
          (return-from native-direct-worker-body nil))))))

(defun native-optimize-typed-body (body parameters &optional raw-argument reusable)
  ;; Mixed scalar/reference locals need exact types during coloring. This
  ;; also supplies the direct worker's actual five-parameter signature.
  (let ((position 0) (groups nil) (locals nil) (count 0))
    (multiple-value-setq (groups position) (native-read-uleb body position))
    (dotimes (i groups)
      (let ((size nil))
        (multiple-value-setq (size position) (native-read-uleb body position))
        (push (cons size (list (aref body position))) locals)
        (incf count size) (incf position)))
    (let ((types (make-array (+ parameters count) :initial-element '(#x6f))) (index parameters))
      (when raw-argument (setf (aref types (1+ raw-argument)) '(#x7e)))
      (when reusable
        (fill types '(#x7e) :start 5 :end 9)
        (setf (aref types 9) '(#x7f)))
      (dolist (group (nreverse locals))
        (dotimes (i (car group)) (setf (aref types index) (cdr group)) (incf index)))
      (native-optimize-body body (native-read-ops body) parameters count types))))

(defun native-direct-wrapper (function worker)
  (when (and *wasm-shared-entries*
             (not (and *wasm-reusable-scalars* (gethash function *wasm-reusable-scalars*))))
    (return-from native-direct-wrapper
      (with-wasm-bytes
        (wasm-u32 0) (wasm-local 0) (wasm-local 1)
        (let ((start (length *wasm-bytes*)))
          (wasm-call "native-shared-entry") (setf (aref *wasm-bytes* start) #x12))
        (wasm-byte #x0b))))
  (let ((arity (native-fixed-arity function)))
    (with-wasm-bytes
      (wasm-u32 0)
      (wasm-local 1) (wasm-integer arity) (wasm-integer arity) (wasm-call "native-check-arguments")
      (wasm-local 0)
      (dotimes (i 4)
        (if (< i arity) (progn (wasm-local 1) (wasm-integer i) (wasm-call "native-argument")) (wasm-null)))
      ;; The adapter must itself be a tail call. Otherwise a recursive worker
      ;; accumulates an adapter frame even when its own call is RETURN_CALL.
      (wasm-byte #x12)
      (push (list (length *wasm-bytes*) worker) (gethash function *wasm-function-relocations*))
      (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
      (wasm-byte #x0b))))

(defun native-boxed-result-worker (function)
  "Preserve the ordinary direct ABI around one native multi-result body."
  (let ((worker (gethash function *wasm-direct-workers*))
        (count (gethash function *wasm-multiple-result-functions*)))
    (with-wasm-bytes
      (wasm-u32 0)
      (dotimes (i 5) (wasm-local i))
      (wasm-byte #x10)
      (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
      (case count
        (2 (wasm-call "native-return2"))
        (3 (wasm-call "native-return3"))
        (otherwise (wasm-null) (dotimes (i count) (wasm-call "native-cons"))
                   (wasm-call "native-return")))
      (wasm-byte #x0b))))

(defun native-boxed-scalar-worker (worker arguments result)
  ;; Reusable proof derives from this adapter around the actual raw body.
  ;; Both callers pass the same closure and checked boxed arguments.
  (with-wasm-bytes
    (wasm-u32 0)
    (dotimes (i 5) (wasm-local i))
    (loop for index from 1 to 4 do (wasm-local index))
    (wasm-integer arguments)
    (wasm-call "native-scalar-unpack")
    (wasm-byte #x10)
    (wasm-u32 (+ (length *native-imports*) (gethash worker *native-function-indices*)))
    (wasm-integer result)
    (wasm-call "native-scalar-box")
    (wasm-call "native-return1")
    (wasm-byte #x0b)))

(defun native-direct-fallback (function)
  ;; Some stack/local rewrites can obscure the parameter prologue. The direct
  ;; entry is still valid, but uses the ordinary implementation for that body.
  (with-wasm-bytes
    (wasm-u32 0) (wasm-local 0)
    (dotimes (i (native-fixed-arity function)) (wasm-local (1+ i)))
    (wasm-null)
    (dotimes (i (native-fixed-arity function)) (wasm-call "native-cons"))
    (wasm-byte #x12)
    (wasm-u32 (+ (length *native-imports*) (gethash function *native-function-indices*)))
    (wasm-byte #x0b)))

(defun native-direct-relocations (body functions)
  (declare (type vector body) (type simple-vector functions))
  ;; Parameter rewriting changes byte offsets. Rebuild references against the
  ;; original function numbering before sharing/pruning relocates the module.
  (let ((result nil) (imports (length *native-imports*)))
    (loop for op across (native-read-ops body t)
          when (and (member (native-op-code op) '(#x10 #x12 #xd2))
                    (>= (native-op-argument op) imports))
          do (push (list (1+ (native-op-start op)) (aref functions (- (native-op-argument op) imports))) result))
    result))

(defun native-coalesce-bodies (bodies)
  (declare (type list bodies) (inline code-char))
  ;; Equal bodies may still have different literal values and captures: both
  ;; live in the function object passed through the boxed entry convention.
  (let ((seen (make-hash-table :test 'equal)) (indices (make-hash-table :test 'eq))
        (unique nil) (count 0) (changed nil))
    (declare (type hash-table seen indices) (type list unique) (type (unsigned-byte 32) count))
    (loop for function in *native-functions* for body in bodies do
      (let ((key (or (gethash body *wasm-body-keys*)
                     (setf (gethash body *wasm-body-keys*) (concatenate 'string
                       (let ((source (and *wasm-worker-origins* (gethash function *wasm-worker-origins*))))
                         (cond ((or (eq function *wasm-private-scalar-worker*)
                                    (eq function (first (and *wasm-reusable-scalars*
                                                             (gethash source *wasm-reusable-scalars*)))))
                                "I")
                               (source (format nil "D~D" (if (eq function (gethash source *wasm-direct-workers*))
                                 (or (gethash source *wasm-multiple-result-functions*) 0) 0)))
                               (t "G")))
                       (map 'string (lambda (code) (code-char code)) body))))))
       (multiple-value-bind (index found) (gethash key seen)
        (when found (compiler-statistic :optimization/shared-body))
        (unless found
          (setq index count) (incf count) (push body unique)
          (setf (gethash key seen) index))
        (unless (= index (gethash function *native-function-indices*)) (setq changed t))
        (setf (gethash function indices) index))))
    (values (nreverse unique) indices changed)))

(defun native-relocate-body (function body original-indices)
  (declare (type simple-vector function) (type vector body) (type hash-table original-indices))
  (let ((relocations (gethash function *wasm-function-relocations*)) (cursor 0))
    (declare (type list relocations) (type (unsigned-byte 32) cursor))
    (unless (some (lambda (entry)
              (/= (gethash (second entry) original-indices)
                  (gethash (second entry) *native-function-indices*))) relocations)
      (return-from native-relocate-body body))
    (with-wasm-bytes
      (dolist (entry (reverse relocations))
        (wasm-append body cursor (first entry))
        (setq cursor (first entry))
        ;; Skip the original unsigned LEB; its replacement may be shorter.
        (loop for byte = (aref body cursor) do (incf cursor) until (< byte 128))
        (wasm-u32 (+ (length *native-imports*) (gethash (second entry) *native-function-indices*))))
      (wasm-append body cursor))))

(defun emit-native-bodies (entries)
  (let* ((*wasm-direct-raw-bodies* (make-hash-table :test 'eq))
         ;; A single adapter costs more as an import. Sharing becomes useful
         ;; in groups, while debug entries retain their individual listings.
         (*wasm-shared-entries* (and (not *wasm-debug*) (> (hash-table-count *wasm-direct-workers*) 1)))
         (full (make-hash-table :test 'eq)) (workers (make-hash-table :test 'eq))
         ;; Initial indices follow this list, including multiple-result adapters.
         ;; Share their inverse while rewriting bodies, before any renumbering.
         (relocation-functions (and *wasm-worker-origins*
                                   (plusp (hash-table-count *wasm-worker-origins*))
                                   (coerce *native-functions* 'simple-vector)))
         (original
           (progn
             (dolist (function *native-functions*)
               (unless (and *wasm-worker-origins* (gethash function *wasm-worker-origins*))
                 (setf (gethash function full) (emit-native-function function))))
             (dolist (function *native-functions*)
               (let ((source (and *wasm-worker-origins* (gethash function *wasm-worker-origins*))))
                 (when source
                   (let* ((adapter (eq function (gethash source *wasm-boxed-result-workers*)))
                          (reusable (and *wasm-reusable-scalars* (gethash source *wasm-reusable-scalars*)))
                          (reusable-worker (first reusable))
                          (private-scalar (or (eq function *wasm-private-scalar-worker*)
                                              (eq function reusable-worker)))
                          (*wasm-private-scalar-width* (if reusable (second reusable) *wasm-private-scalar-width*))
                          (*wasm-private-scalar-type* (if reusable (third reusable) *wasm-private-scalar-type*))
                          (*wasm-private-scalar-argument* (unless reusable *wasm-private-scalar-argument*))
                          (*wasm-private-scalar-framed* (or reusable *wasm-private-scalar-framed*))
                          (body (cond ((and reusable-worker
                                            (eq function (gethash source *wasm-direct-workers*)))
                                       (native-boxed-scalar-worker reusable-worker (fifth reusable) (fourth reusable)))
                                      (adapter (native-boxed-result-worker source))
                                      (private-scalar
                                       (multiple-value-bind (value edge) (native-private-scalar-return-source source)
                                         (let ((*wasm-private-scalar-return* value)
                                               (*wasm-reusable-scalar-result* (fourth reusable))
                                               (*wasm-reusable-scalar-arguments* (fifth reusable))
                                               (*wasm-reusable-scalar-identity* (sixth reusable))
                                               (*wasm-private-scalar-forward* edge)
                                               (*wasm-private-scalar-ingress*
                                                 (and *wasm-private-scalar-argument*
                                                      (native-private-scalar-argument-value
                                                        source *wasm-private-scalar-argument*)))
                                               (*wasm-call-frame-locals* nil)
                                               (*wasm-captured-result-p* nil)
                                               (*wasm-function-plans* (if (or *wasm-private-scalar-argument*
                                                                             (and (not (sixth reusable))
                                                                                  (member (fourth reusable) '(3 4)))
                                                                             (eq *wasm-private-scalar-type* 'character))
                                                                          (make-hash-table :test 'eq)
                                                                          *wasm-function-plans*))
                                               ;; A clone re-emits the body's instructions but does not own
                                               ;; the ordinary source wrapper or its relocated direct call.
                                               (*wasm-function-relocations* (make-hash-table :test 'eq)))
                                           (native-direct-worker-body source (emit-native-function-body source)
                                             *wasm-private-scalar-argument* reusable))))
                                      (t (native-direct-worker-body source
                                           (gethash source *wasm-direct-raw-bodies*)))))
                          (*wasm-current-function* function))
                     (when (and private-scalar
                                (or (null body)
                                    (> (length body) (if *wasm-private-scalar-type* 200 96))))
                       (throw 'native-multiple-result-fallback
                         (if *wasm-private-scalar-argument* :retry-sealed-argument :retry-sealed-scalar)))
                     (when (and (null body) (gethash source *wasm-multiple-result-functions*))
                       ;; Unsupported checked-entry shapes retain the ordinary
                       ;; ABI for this emission, including already emitted callers.
                       (throw 'native-multiple-result-fallback :retry))
                     (when (and (null body) *wasm-named-recursion* *wasm-scoped-call-functions*
                                (eq source (car *wasm-named-recursion*))
                                (gethash source *wasm-scoped-call-functions*))
                       ;; Retrying through a generic worker would invalidate
                       ;; the scoped body proof, even with the same arity.
                       (throw 'native-multiple-result-fallback :retry-named-recursion))
                     (unless body
                       ;; A failed rewrite retains the checked generic entry.
                       ;; Its argument error can invoke Lisp handlers, so it
                       ;; cannot retain speculative worker-only cache roots.
                       (let ((plan (gethash source *wasm-function-plans*)))
                         (when (svref plan 12)
                           (decf (svref plan 10) (length (svref plan 12)))
                           (clrhash (svref plan 11)) (setf (svref plan 12) nil)
                           (setf (gethash source full) (emit-native-function source)))))
                     ;; Remap arguments before register allocation can reuse
                     ;; the old argument-packet parameter as an ordinary local.
                     (when body
                       (setf (gethash function *wasm-function-relocations*)
                         (native-direct-relocations body relocation-functions))
                       (unless (or adapter (and reusable-worker (not private-scalar)))
                         (setq body (native-optimize-typed-body body (if (and reusable private-scalar) 10 5)
                                      (and private-scalar *wasm-private-scalar-argument*)
                                      (and reusable private-scalar)))))
                     (setf (gethash function workers) (or body (native-direct-fallback source)))
                     (when (and body (not adapter) (not private-scalar))
                       (setf (gethash source *wasm-function-relocations*) nil
                             (gethash source full) (native-direct-wrapper source
                               (or (gethash source *wasm-boxed-result-workers*) function))))
                     (setf (gethash function *wasm-function-relocations*)
                       (native-direct-relocations (gethash function workers) relocation-functions))))))
             (mapcar (lambda (function) (or (gethash function workers) (gethash function full))) *native-functions*)))
        (original-indices *native-function-indices*)
        (*wasm-body-keys* (make-hash-table :test 'eq)))
    (loop
      (let ((bodies (mapcar (lambda (function body)
                             (native-relocate-body function body original-indices)) *native-functions* original)))
        (multiple-value-bind (unique indices changed) (native-coalesce-bodies bodies)
          (setq *native-function-indices* indices)
          ;; Patch REF.FUNC operands when their targets merge. Literal layouts
          ;; stay unchanged; HIR and full bodies need to be emitted only once.
          (unless changed
            ;; A single-entry module keeps its factory at index zero. Shared
            ;; bodies still use each closure's own literals and captures.
            (return (if entries (native-order-bodies entries original original-indices unique) unique))))))))

(defun native-order-bodies (entries original original-indices unique)
  ;; Compact frequently referenced function indices after body sharing reaches
  ;; a fixed point. Relocate from the original bodies, whose offsets are known.
  (when (<= (+ (length *native-imports*) (length unique)) 128)
    (return-from native-order-bodies unique))
  (let* ((size (length unique)) (representatives (make-array size :initial-element nil))
         (counts (make-array size :initial-element 0))
         (order (loop for i below size collect i)) (mapping (make-array size)))
    (loop for function in *native-functions* for body in original do
      (let ((index (gethash function *native-function-indices*)))
        (unless (aref representatives index)
          (setf (aref representatives index) (cons function body)))))
    (loop for entry across representatives do
      (dolist (relocation (gethash (car entry) *wasm-function-relocations*))
        (incf (aref counts (gethash (second relocation) *native-function-indices*)))))
    (dolist (entry entries) (incf (aref counts (gethash entry *native-function-indices*))))
    (setq order (stable-sort order #'> :key (lambda (i) (aref counts i))))
    (loop for old in order for new from 0 do (setf (aref mapping old) new))
    (dolist (function *native-functions*)
      (setf (gethash function *native-function-indices*)
        (aref mapping (gethash function *native-function-indices*))))
    (mapcar (lambda (index)
              (let ((entry (aref representatives index)))
                (native-relocate-body (car entry) (cdr entry) original-indices))) order)))

(defun native-fuse-literal-functions (bodies decoded live functions used entries)
  ;; Work on actual adjacent calls, after local optimization and liveness.
  ;; Literal locals already avoid the first call and keep their existing path.
  (let* ((imports (coerce *native-imports* 'vector)) (size (length imports))
         (literal (position "native-literal" imports :key #'first :test #'equal))
         (reference (position "native-function-reference" imports :key #'first :test #'equal))
         (fused (position "native-literal-function" imports :key #'first :test #'equal))
         (counts (make-array size :initial-element 0))
         (actual (make-array size :initial-element 0))
         (replacements (make-hash-table :test 'eq)) (removed (make-hash-table :test 'eq)) (pairs 0))
    ;; Relocated function offsets are assigned below LENGTH(FUNCTIONS) by the
    ;; module planner. Guard their sum with imports before unchecked u32 pricing.
    (when (> (+ size (length functions)) #x100000000)
      (return-from native-fuse-literal-functions nil))
    (unless (and literal reference fused (aref used literal) (aref used reference))
      (return-from native-fuse-literal-functions nil))
    ;; The helper reuses LITERAL's signature. This ABI has fewer than 128
    ;; distinct signatures, so reordered type indices retain their byte width.
    (when (>= (length (remove-duplicates (mapcar #'cdr *native-imports*) :test #'equal)) 120)
      (return-from native-fuse-literal-functions nil))
    (dotimes (i size) (setf (aref counts i) (gethash (first (aref imports i)) *wasm-import-counts* 0)))
    (dotimes (i (length decoded))
      (when (aref live i)
        (let ((previous nil))
          (loop for op across (aref decoded i) for index = (native-op-argument op) do
            (when (< index size) (incf (aref actual index)))
            (when (and (eql (native-op-code op) #x10) (= index reference)
                       previous (eql (native-op-code previous) #x10)
                       (= (native-op-argument previous) literal)
                       (= (native-op-end previous) (native-op-start op)))
              (setf (gethash previous replacements) t (gethash op removed) t)
              (incf pairs))
            (setq previous op)))))
    (when (zerop pairs) (return-from native-fuse-literal-functions nil))
    (let ((next-used (copy-seq used)) (next-counts (copy-seq counts)))
      (setf (aref next-used fused) t
            (aref next-used literal) (> (aref actual literal) pairs)
            (aref next-used reference) (> (aref actual reference) pairs))
      (decf (aref next-counts literal) pairs) (decf (aref next-counts reference) pairs)
      (incf (aref next-counts fused) pairs)
      (labels ((layout (retained frequencies)
                 (let* ((order (stable-sort (loop for i below size when (aref retained i) collect i)
                                 #'> :key (lambda (i) (aref frequencies i))))
                        (mapping (make-array size)))
                   (loop for i in order for index from 0 do (setf (aref mapping i) index))
                   (values mapping (length order))))
               (declarations (retained)
                 (loop for i below size when (aref retained i)
                   ;; One-byte namespace/name/type lengths, namespace M, and
                   ;; the ABI's ASCII NATIVE- prefix omitted from each field.
                   sum (- (length (first (aref imports i))) 2))))
        (multiple-value-bind (before before-count) (layout used counts)
          (multiple-value-bind (after after-count) (layout next-used next-counts)
            ;; Include every relocated call/REF.FUNC operand, body length and
            ;; import declaration. Reserve eight bytes for section headers and
            ;; import-count LEB boundaries. Function/data roots stay unchanged.
            (let ((saving (- (declarations used) (declarations next-used) 8)))
              (loop for body in bodies for i from 0 when (aref live i) do
                (let ((old-size (length body)) (new-size (length body)))
                  (loop for op across (aref decoded i) for index = (native-op-argument op)
                        for original-size = (- (native-op-end op) (native-op-start op)) do
                    (incf old-size (- (1+ (wasm-u32-width (if (< index size) (aref before index)
                                                (+ before-count (aref functions (- index size)))))) original-size))
                    (incf new-size
                      (- (if (gethash op removed) 0
                             (1+ (wasm-u32-width (if (gethash op replacements) (aref after fused)
                                            (if (< index size) (aref after index)
                                                (+ after-count (aref functions (- index size))))))))
                         original-size)))
                  ;; Relocation changes lengths mathematically. An oversized
                  ;; projected body supplies no size proof and is left alone.
                  (unless (and (<= 0 old-size #xffffffff) (<= 0 new-size #xffffffff))
                    (return-from native-fuse-literal-functions nil))
                  (incf saving (+ (- old-size new-size)
                                  (- (wasm-u32-width old-size) (wasm-u32-width new-size))))))
              ;; Export aliases may all name one shared adapter. Crossing an
              ;; import-index tier then changes every alias, not just its body.
              (let ((exported (if entries
                                  (mapcar (lambda (entry) (gethash entry *native-function-indices*)) entries)
                                  '(0))))
                (dolist (entry (or entries (list (first *native-functions*))))
                  (let* ((worker (gethash entry *wasm-direct-workers*))
                         (index (and worker (gethash worker *native-function-indices*))))
                    (when (and index (aref live index)) (push index exported))))
                (dolist (index exported)
                  (let ((offset (aref functions index)))
                    (incf saving (- (wasm-u32-width (+ before-count offset))
                                    (wasm-u32-width (+ after-count offset)))))))
              ;; Declarative element entries follow the same relocation, but
              ;; unlike exports the emitted segment deduplicates shared bodies.
              (dolist (index (remove-duplicates
                              (loop for function in *wasm-referenced-functions*
                                    for old = (gethash function *native-function-indices*)
                                    when (and old (aref live old)) collect (aref functions old))))
                (incf saving (- (wasm-u32-width (+ before-count index))
                                (wasm-u32-width (+ after-count index)))))
              (when (plusp saving)
                (maphash (lambda (op ignored) (declare (ignore ignored))
                           (setf (native-op-argument op) fused)) replacements)
                (maphash (lambda (op ignored) (declare (ignore ignored))
                           (setf (native-op-code op) nil)) removed)
                (replace used next-used)
                (dotimes (i size)
                  (setf (gethash (first (aref imports i)) *wasm-import-counts*) (aref next-counts i)))
                (compiler-statistic :optimization/literal-function-fusion nil pairs)
                t))))))))

(defun native-fold-cells (bodies decoded live used)
  (declare (type list bodies) (type simple-vector decoded live used))
  ;; CELL is precisely CONS(value,NIL). Reuse an already retained helper;
  ;; with one-byte final import/type indices this can only shrink the module,
  ;; including relocations, section lengths and either entry layout.
  (let* ((cell (position "native-cell" *native-imports* :key #'first :test #'equal))
         (cons (position "native-cons" *native-imports* :key #'first :test #'equal))
         (remaining 0) (pairs 0))
    (declare (type (integer 0 *) remaining pairs))
    (unless (and cell cons (aref used cell) (aref used cons)
                 (< (loop for retained across used count retained) 128)
                 (< (length (remove-duplicates (mapcar #'cdr *native-imports*) :test #'equal)) 120))
      (return-from native-fold-cells nil))
    (loop for body in bodies for i from 0 when (aref live i) do
      (let ((references (aref decoded i)) (candidate nil))
        (declare (type simple-vector references))
        (loop for op across references when (and (native-op-code op) (= (native-op-argument op) cons)) do
          (incf remaining)
          (when (eql (native-op-code op) #x10) (setq candidate t)))
        (when candidate
          ;; The reference-only decoder omits NULL. Decode actual instruction
          ;; boundaries: matching bytes before a call could be an immediate.
          (let ((ops (native-read-ops body)) (cursor 0))
            (declare (type simple-vector ops) (type (unsigned-byte 32) cursor))
            (loop for op across references when (and (eql (native-op-code op) #x10)
                                                     (= (native-op-argument op) cons)) do
              (loop while (< (native-op-start (aref ops cursor)) (native-op-start op)) do (incf cursor))
              (let ((previous (and (plusp cursor) (aref ops (1- cursor)))))
                (when (and previous (eql (native-op-code previous) #xd0)
                           (eql (native-op-argument previous) #x6f)
                           (= (native-op-end previous) (native-op-start op)))
                  ;; The relocation now covers NULL plus CALL. Every other
                  ;; record remains a function reference, including fusion's
                  ;; removed records; their consumers need no new op kind.
                  (setf (native-op-start op) (native-op-start previous)
                        (native-op-argument op) cell)
                  (incf pairs) (decf remaining))))))))
    (when (plusp pairs)
      (setf (aref used cons) (plusp remaining))
      (incf (gethash "native-cell" *wasm-import-counts* 0) pairs)
      (decf (gethash "native-cons" *wasm-import-counts* 0) pairs)
      (compiler-statistic :optimization/cell-folding nil pairs)
      t)))

(defun native-scalar-work-loop (body ops poll)
  ;; Match the final executable loop, after wrapper construction, arity
  ;; copies and local optimization. Candidate instructions cannot trap or
  ;; change anything outside this activation except at the recorded polls.
  (when (find #x1f ops :key (lambda (op) (native-op-code op)))
    (return-from native-scalar-work-loop nil))
  (let ((scopes nil) (size (length ops)))
    (loop for op across ops for start from 0 do
      (when (and (eql (native-op-code op) #x03) (eql (native-op-argument op) #x40)
                 (= (- (native-op-end op) (native-op-start op)) 2))
        (let ((sites nil) (nesting 0) (prefix 0))
          (loop for i from (1+ start) below (min size (+ start 48))
                for instruction = (aref ops i)
                for code = (native-op-code instruction) do
            (unless (eql code (aref body (native-op-start instruction))) (return))
            (case code
              ((#x1a #x20 #x21 #x22 #x41 #x42
                #x45 #x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f
                #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a
                #x6a #x6b #x6c #x71 #x7c #x7d #x7e #x83 #x84 #x85 #x86 #x87 #xa7 #xac #xad
                #xd1 #xd3))
              ((#x02 #x04) (incf nesting))
              (#x05 (when (zerop nesting) (return)))
              (#x0b (when (zerop nesting) (return)) (decf nesting))
              ((#x0c #x0d)
               ;; Inner labels are forward destinations within a poll-free
               ;; scope. An exit must target a known enclosing void label:
               ;; a rewritten BR_IF cannot fetch operands below its IF.
               (let ((depth (- (native-op-argument instruction) nesting)))
                 (unless (minusp depth)
                   (let ((target (and (plusp depth) (nth (1- depth) scopes))))
                     (unless (and target (member (native-op-code target) '(#x02 #x04))
                                  (eql (native-op-argument target) #x40)
                                  (= (- (native-op-end target) (native-op-start target)) 2))
                       (return)))
                   (push (cons instruction prefix) sites))))
              (#x10
               ;; All paths that reach this observation passed the same
               ;; earlier polls. No nested path may skip or add one.
               (unless (and (zerop nesting) (eql (native-op-argument instruction) poll)) (return))
               (when (and (< (+ i 2) size)
                          (eql (native-op-code (aref ops (1+ i))) #x0c)
                          (eql (native-op-argument (aref ops (1+ i))) 0)
                          (eql (native-op-code (aref ops (+ i 2))) #x0b))
                 (return-from native-scalar-work-loop
                   (list (nreverse sites) instruction (1+ prefix))))
               (push (cons instruction nil) sites)
               (incf prefix))
              (otherwise (return))))))
      (case (native-op-code op)
        ((#x02 #x03 #x04) (push op scopes))
        (#x0b (pop scopes))))))

(defun native-batch-scalar-work (body parameters poll ops)
  (destructuring-bind (sites latch weight)
      (or (native-scalar-work-loop body ops poll)
          (return-from native-batch-scalar-work body))
    (let ((remaining parameters) (cursor 0)
          (*wasm-last-local* nil))
      (multiple-value-bind (groups first) (native-read-uleb body 0)
        (setq cursor first)
        (dotimes (i groups)
          (multiple-value-bind (count type) (native-read-uleb body cursor)
            (incf remaining count)
            (setq cursor (native-local-type-end body type))))
        (unless (< remaining #xfffffffe) (return-from native-batch-scalar-work body))
        (compiler-statistic :optimization/scalar-work-batch weight)
        (decf (gethash "native-poll" *wasm-import-counts*) weight)
        (let ((granted (1+ remaining)))
         (with-wasm-bytes
          (wasm-u32 (1+ groups))
          (wasm-append body first cursor)
          (wasm-u32 2) (wasm-type :i32)
          (dolist (entry sites)
            (let ((site (car entry)) (prefix (cdr entry)))
              (wasm-append body cursor (native-op-start site))
              (if prefix
                  (let ((conditional (eql (native-op-code site) #x0d)))
                    (when conditional (wasm-byte #x04) (wasm-byte #x40))
                    ;; Charge completed trips and the prefix of this trip.
                    ;; Without a grant, the ordinary prefix already polled.
                    (when (zerop prefix)
                      (wasm-local granted) (wasm-local remaining) (wasm-byte #x6b)
                      (wasm-byte #x22) (wasm-u32 remaining))
                    (when (plusp prefix) (wasm-local remaining))
                    (wasm-byte #x04) (wasm-byte #x40)
                    (if (plusp prefix)
                        (progn
                          (wasm-local granted) (wasm-local remaining) (wasm-byte #x6b)
                          (wasm-integer prefix) (wasm-byte #x6a))
                        (wasm-local remaining))
                    (wasm-call "native-work-complete") (wasm-byte #x0b)
                    ;; An enclosing loop can run this same region again.
                    (wasm-integer 0) (wasm-byte #x22) (wasm-u32 remaining)
                    (wasm-byte #x21) (wasm-u32 granted)
                    (if conditional
                        (progn
                          (wasm-byte #x0c) (wasm-u32 (1+ (native-op-argument site)))
                          (wasm-byte #x0b))
                        (wasm-append body (native-op-start site) (native-op-end site))))
                  (progn
                    (wasm-local remaining) (wasm-byte #x45) (wasm-byte #x04) (wasm-byte #x40)
                    (wasm-call "native-poll") (wasm-byte #x0b)))
              (setq cursor (native-op-end site))))
          (wasm-append body cursor (native-op-start latch))
          ;; Zero on entry means the whole trip took its ordinary poll path.
          ;; A positive countdown is an integral number of trips, capped by
          ;; the checked grant. The last admitted latch settles at the same
          ;; logical observation as its ordinary poll, before readmission.
          (wasm-local remaining) (wasm-integer weight) (wasm-byte #x6b)
          (wasm-byte #x22) (wasm-u32 remaining)
          (wasm-integer 0) (wasm-byte #x4c)
          (wasm-byte #x04) (wasm-byte #x40)
          (wasm-local remaining) (wasm-byte #x04) (wasm-byte #x40)
          (wasm-call "native-poll")
          (wasm-byte #x05)
          (wasm-local granted) (wasm-call "native-work-complete")
          (wasm-byte #x0b)
          (wasm-integer 2048) (wasm-call "native-work-limit")
          ;; Keep only complete trips. The divisor is the nonzero compile-time
          ;; count of observations in one trip. The grant is a bounded i32.
          (when (> weight 1)
            (if (zerop (logand weight (1- weight)))
                (progn (wasm-integer (- weight)) (wasm-byte #x71))
                (progn
                  (wasm-byte #x22) (wasm-u32 remaining) (wasm-local remaining)
                  ;; The native decoders support unsigned remainder at i64 width.
                  (wasm-byte #xad) (wasm-byte #x42) (wasm-s64 weight)
                  (wasm-byte #x82) (wasm-byte #xa7) (wasm-byte #x6b))))
          (wasm-byte #x22) (wasm-u32 remaining)
          (wasm-byte #x21) (wasm-u32 granted)
          (wasm-byte #x0b)
          (wasm-append body (native-op-end latch))))))))

(defun native-epoch-poll-body (body ops poll local)
  ;; Replace only calls in the structured loop paths of the final body.
  ;; Each replacement consumes and produces no operands and leaves the
  ;; enclosing label stack unchanged.
  (when (>= local #xffffffff) (return-from native-epoch-poll-body body))
  (let ((scopes nil) (loops 0) (sites nil))
    (loop for op across ops for code = (native-op-code op) do
      (case code
        ((#x02 #x04) (push nil scopes))
        (#x03
         (unless (and (eql (native-op-argument op) #x40)
                      (= (- (native-op-end op) (native-op-start op)) 2))
           (return-from native-epoch-poll-body body))
         (push t scopes) (incf loops))
        (#x0b (when (pop scopes) (decf loops)))
        (#x1f (return-from native-epoch-poll-body body))
        (#x10
         (when (and (plusp loops) (eql (native-op-argument op) poll))
           (push op sites)))))
    (unless (and sites (null scopes)) (return-from native-epoch-poll-body body))
    (let ((cursor (native-op-start (aref ops 0))) (count (length sites)))
      (compiler-statistic :optimization/epoch-poll nil count)
      (decf (gethash "native-poll" *wasm-import-counts*) count)
      (with-wasm-bytes
        (multiple-value-bind (groups first) (native-read-uleb body 0)
          (wasm-u32 (1+ groups)) (wasm-append body first cursor))
        (wasm-u32 1) (wasm-type :i64)
        (dolist (site (nreverse sites))
          (wasm-append body cursor (native-op-start site))
          ;; If active, debit reserved credit only while it exceeds the
          ;; live epoch base. The slow call performs the original poll and
          ;; returns a token after any callback or suspension.
          (dolist (byte '(#x23 1 #x04 #x40 #x20)) (wasm-byte byte)) (wasm-u32 local)
          (dolist (byte '(#x23 0 #x55 #x04 #x7e #x20)) (wasm-byte byte)) (wasm-u32 local)
          (dolist (byte '(#x42 1 #x7d #x05)) (wasm-byte byte))
          (wasm-call "native-poll-token")
          (wasm-byte #x0b) (wasm-byte #x21) (wasm-u32 local) (wasm-byte #x0b)
          (setq cursor (native-op-end site)))
        (wasm-append body cursor)))))

(defun native-batch-work-bodies (bodies epoch-polling)
  ;; Leave sealed host kernels on their separately owned checkpoint ABI,
  ;; and keep maximum-space or zero-speed workers compact. Coalesced
  ;; entries must all agree.
  (when (or *wasm-debug* *compiler-bootstrapping*
            *wasm-fixed-call-emitter* *wasm-closed-worker-functions*
            (not (gethash "native-poll" *wasm-import-counts*)))
    (return-from native-batch-work-bodies bodies))
  (let ((candidates (make-array (length bodies) :initial-element nil))
        (poll (position "native-poll" *native-imports* :key #'first :test #'equal))
        (fuel 8192))
    (dolist (function *native-functions*)
      (let* ((source (or (gethash function *wasm-worker-origins*) function))
             (plan (gethash source *wasm-function-plans*))
             (policy (hir-function-optimization-policy source))
             (speed (logand policy 3))
             (space (logand 3 (ash policy -2)))
             (index (gethash function *native-function-indices*))
             (parameters (and plan (svref plan 20) (not (svref plan 4))
                              (plusp speed) (< space 3)
                              (length (first (native-result-signature function))))))
        (setf (aref candidates index)
              (if (and parameters (or (null (aref candidates index))
                                      (eql parameters (aref candidates index))))
                  parameters :ordinary))))
    (loop for body in bodies for parameters across candidates
          collect (if (and (integerp parameters) (<= (length body) 4096 fuel))
                      (progn
                        (decf fuel (length body))
                        (multiple-value-bind (ops offset locals) (native-read-ops body)
                          (let ((batched (native-batch-scalar-work body parameters poll ops)))
                            (if (and epoch-polling (eq body batched))
                                (native-epoch-poll-body body ops poll (+ parameters (- offset 2) locals))
                                batched))))
                      body))))

(defun native-prune-module (bodies entries &optional (prune-functions t))
  ;; RemoveUnusedModuleElements after branch/DCE cleanup. No calls or closure
  ;; entries are renumbered until body sharing and function layout are final.
  (let* ((old-count (length *native-imports*)) (used (make-array old-count :initial-element nil))
         (mapping (make-array old-count))
         (decoded (coerce (mapcar (lambda (body) (native-read-ops body t)) bodies) 'vector))
         (live (make-array (length bodies) :initial-element nil))
         (functions (make-array (length bodies))) (function-count 0)
         (work (if prune-functions
                   (append (if entries (mapcar (lambda (entry) (gethash entry *native-function-indices*)) entries) '(0))
                     (loop for entry in (or entries (list (first *native-functions*)))
                           for worker = (and *wasm-direct-workers*
                                             (or (gethash entry *wasm-boxed-result-workers*)
                                                 (gethash entry *wasm-direct-workers*)))
                           when worker collect (gethash worker *native-function-indices*)))
                   (loop for i below (length bodies) collect i)))
         (imports nil) (next 0) (fused-p nil))
    ;; Only exports and function references in reachable code are roots. The
    ;; declarative element segment grants REF.FUNC permission, not reachability.
    (loop while work for index = (pop work) do
      (unless (aref live index)
        (setf (aref live index) t)
        (loop for op across (aref decoded index) do
          (when (member (native-op-code op) '(#x10 #x12 #xd2))
            (let ((target (native-op-argument op)))
              (if (< target old-count) (setf (aref used target) t)
                  (push (- target old-count) work)))))))
    (dotimes (i (length bodies))
      (when (aref live i) (setf (aref functions i) function-count) (incf function-count)))
    (setq fused-p (native-fuse-literal-functions bodies decoded live functions used entries))
    (when (native-fold-cells bodies decoded live used) (setq fused-p t))
    ;; Emit once against the complete ABI, then choose compact import indices
    ;; from the actual surviving instructions. Frequency order keeps common
    ;; calls within the one-byte LEB tier, including in large application units.
    (let ((order (loop for i below old-count when (aref used i) collect i))
          (all-imports (coerce *native-imports* 'vector)))
      (setq order (stable-sort order #'>
                    :key (lambda (i) (gethash (first (aref all-imports i)) *wasm-import-counts* 0))))
      (dolist (i order)
        (setf (aref mapping i) next) (incf next) (push (aref all-imports i) imports)))
    (when (< next old-count)
      (compiler-statistic :optimization/removed-imports nil (- old-count next)))
    (when (< function-count (length bodies))
      (compiler-statistic :optimization/removed-functions nil (- (length bodies) function-count)))
    (when (and (not fused-p) (= next old-count) (= function-count (length bodies))
               (loop for i below old-count always (= i (aref mapping i))))
      (return-from native-prune-module bodies))
    (setq *native-imports* (nreverse imports))
    (dolist (function *native-functions*)
      (let ((old (gethash function *native-function-indices*)))
        (setf (gethash function *native-function-indices*) (and (aref live old) (aref functions old)))))
    (setq *wasm-referenced-functions*
      (remove-duplicates
        (remove-if-not (lambda (function) (gethash function *native-function-indices*)) *wasm-referenced-functions*)
        :key (lambda (function) (gethash function *native-function-indices*))))
    (loop for body in bodies for old from 0 when (aref live old) collect
      (let ((ops (aref decoded old)))
              (with-wasm-bytes
                (let ((cursor 0))
                 (loop for op across ops for code = (native-op-code op) do
                      (wasm-append body cursor (native-op-start op))
                      (when code
                        (let ((index (native-op-argument op)))
                          (wasm-byte code)
                          (wasm-u32 (if (< index old-count) (aref mapping index)
                                        (+ next (aref functions (- index old-count)))))))
                      (setq cursor (native-op-end op)))
                 (wasm-append body cursor)))))))

(defun native-type-roots (unit)
  (declare (type hir-unit unit))
  ;; Request only facts consumed by current lowering rules. Type propagation
  ;; itself remains independent of the backend and can infer an entire unit.
  (let ((roots nil))
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (let ((end (hir-block-terminator block)))
          (when (eq (hir-instruction-opcode end) :branch)
            (push (first (hir-instruction-inputs end)) roots)))
        (dolist (instruction (hir-block-instructions block))
          (when (eq (hir-instruction-opcode instruction) :call)
            (let* ((inputs (hir-instruction-inputs instruction))
                   (producer (hir-value-producer (second inputs))))
              (when (and (%hir-instruction-p producer)
                         (eq (hir-instruction-opcode producer) :function-reference)
                         (not (eq (second (hir-instruction-attributes producer)) 'notinline))
                         (or (member (first (hir-instruction-attributes producer))
                           '(1+ 1- + - * logand logior logxor lognot ash < > <= >= = /= eq eql zerop minusp plusp evenp oddp car first cdr rest endp rplaca rplacd length svref aref row-major-aref make-array make-string
                             (setf svref) (setf aref) (setf row-major-aref) (setf char)) :test #'equal)
                             (and (zerop (hir-instruction-safety-level instruction))
                                  (member (first (hir-instruction-attributes producer))
                                    '(consp atom listp functionp simple-vector-p vectorp stringp symbolp integerp characterp null not)))))
                (dolist (input (cddr inputs)) (push input roots))
                ;; Return-only arithmetic results also need range facts for
                ;; choosing their boxed representation, even without a later
                ;; numeric consumer to request those facts.
                (when (member (first (hir-instruction-attributes producer))
                              '(consp atom listp functionp simple-vector-p vectorp stringp symbolp integerp characterp null not eq eql = /= < <= > >=
                                1+ 1- + - * logand logior logxor lognot ash))
                  (push (hir-instruction-output instruction) roots))))))))
    roots))

(defun emit-native-module (unit &key entries (entry-tables t))
  "Return Wasm bytes and literals, without running application code.
ENTRY-TABLES permits compact entry tables for explicit ENTRIES; NIL retains
the named-entry format for hosts that translate individual Wasm workers."
  (declare (optimize (safety 3)))
  (compiler-list-length entries "native entry points")
  (ensure-compiler)
  (verify-hir unit)
  (verify-hir-code-operands unit)
  (verify-hir-code-dominance unit)
  (dolist (entry (or entries (list (first (hir-unit-functions unit)))))
    (unless (member entry (hir-unit-functions unit) :test #'eq)
      (compiler-input-fail "native entry point" entry "entry does not belong to this HIR unit"))
    ;; Bare entry factories receive no closure environment. Captured functions
    ;; must be constructed by a checked :CLOSURE with its complete operands.
    (when (hir-function-captures entry)
      (compiler-input-fail "native entry point" entry "entry requires a closure environment")))
  (emit-native-module-checked unit entries entry-tables))

(defun native-entry-sections (entries direct-exports argument-usages entry-tables imported-globals)
  ;; Physical entry layout never changes the definitions or their order.
  ;; Named factories and small bundles use the same current entry convention.
  (let* ((offset (length *native-imports*))
         (references (remove-duplicates
                       (mapcar (lambda (function) (+ offset (gethash function *native-function-indices*)))
                               *wasm-referenced-functions*)))
         (named
           (with-wasm-bytes
             (when argument-usages
               (wasm-section 6 (with-wasm-bytes
                 (wasm-u32 (length argument-usages))
                 (dolist (usage argument-usages)
                   (wasm-type :i32) (wasm-byte 0) (wasm-integer (second usage)) (wasm-byte #x0b)))))
             (wasm-section 7 (with-wasm-bytes
               (wasm-u32 (+ (if entries (length entries) 1) (length direct-exports) (length argument-usages)))
               (if entries
                   (loop for entry in entries for index from 0 do
                     (wasm-name (format nil "entry~D" index)) (wasm-byte 0)
                     (wasm-u32 (+ offset (gethash entry *native-function-indices*))))
                   (progn (wasm-name "entry") (wasm-byte 0) (wasm-u32 offset)))
               (dolist (entry direct-exports)
                 (wasm-name (first entry)) (wasm-byte 0)
                 (wasm-u32 (+ offset (gethash (second entry) *native-function-indices*))))
               (loop for usage in argument-usages for i from 0 do
                 (wasm-name (first usage)) (wasm-byte 3)
                 (wasm-u32 (+ imported-globals i)))))
             (when references
               (wasm-section 9 (with-wasm-bytes
                 (wasm-u32 1) (wasm-u32 3) (wasm-byte 0) (wasm-u32 (length references))
                 (dolist (index references) (wasm-u32 index))))))))
    (unless (and entries entry-tables) (return-from native-entry-sections (values nil named)))
    (let* ((count (length entries)) (size (+ count (length direct-exports)))
           (table (with-wasm-bytes
                    (wasm-section 4 (with-wasm-bytes
                      (wasm-u32 1) (wasm-type :function) (wasm-byte 1)
                      (wasm-u32 size) (wasm-u32 size)))))
           (compact
             (with-wasm-bytes
               (wasm-section 7 (with-wasm-bytes
                 (wasm-u32 1) (wasm-name "entries") (wasm-byte 1) (wasm-u32 0)))
               (wasm-section 9 (with-wasm-bytes
                 (wasm-u32 (if references 2 1))
                 (wasm-u32 0) (wasm-byte #x41) (wasm-s32 0) (wasm-byte #x0b) (wasm-u32 size)
                 (dolist (entry entries) (wasm-u32 (+ offset (gethash entry *native-function-indices*))))
                 (dolist (entry direct-exports)
                   (wasm-u32 (+ offset (gethash (second entry) *native-function-indices*))))
                 (when references
                   (wasm-u32 3) (wasm-byte 0) (wasm-u32 (length references))
                   (dolist (index references) (wasm-u32 index)))))
               ;; Version 1: count, mode, then one packed direct descriptor
               ;; per entry in mode 1. Zero means no direct worker; otherwise
               ;; low three bits are arity+1 and high bits are usage-mask+1.
               (wasm-section 0 (with-wasm-bytes
                 (wasm-name "miricl.entries") (wasm-u32 1) (wasm-u32 count)
                 (wasm-byte (if direct-exports 1 0))
                 (when direct-exports
                   (let ((descriptors (make-array count :initial-element 0)))
                     (dolist (entry direct-exports)
                       (setf (aref descriptors (fifth entry))
                             (+ (1+ (third entry)) (if (fourth entry) (ash (1+ (fourth entry)) 3) 0))))
                     (loop for descriptor across descriptors do (wasm-byte descriptor)))))))))
      (if (< (+ (length table) (length compact)) (length named))
          (values table compact) (values nil named)))))

(defun emit-native-module-checked (unit entries &optional (entry-tables t) named-recursion sealed epoch-polling)
  (let ((*wasm-native-multiple-results* *wasm-native-multiple-results*)
        (*wasm-private-scalar-enabled* *wasm-private-scalar-enabled*)
        (*wasm-private-scalar-arguments-enabled* *wasm-private-scalar-arguments-enabled*))
    (loop
      (let ((result (catch 'native-multiple-result-fallback
                      (multiple-value-list
                        (emit-native-module-with-results unit entries entry-tables named-recursion sealed epoch-polling)))))
        (case result
          (:retry (setq *wasm-native-multiple-results* nil))
          (:retry-named-recursion
           (compiler-statistic :planning/named-recursion-retry)
           (setq named-recursion nil))
          (:retry-sealed-scalar (setq *wasm-private-scalar-enabled* nil))
          (:retry-sealed-argument (setq *wasm-private-scalar-arguments-enabled* nil))
          (otherwise (return (values-list result))))))))

(defun native-outline-bodies (bodies)
  ;; Final function/import numbers are stable here. Helpers stay private and
  ;; cannot alter a closure's literal bank, checked entry or tail-call ABI.
  ;; The runtime kernel adapter translates one worker at a time; its complete
  ;; linked image uses the same pass through OUTLINE-WASM-MODULE instead.
  (let* ((count (length bodies)) (body-signatures (make-array count :initial-element '((:object :object) :object)))
         (eligible (make-array count :initial-element nil)) (records nil))
    (dolist (function *native-functions*)
      (let ((index (gethash function *native-function-indices*)))
        (when index
          (setf (aref body-signatures index) (native-result-signature function))
          ;; Coalesced bodies satisfy every caller's most restrictive policy.
          (let ((previous (aref eligible index))
                (policy (if (or (>= (hir-function-debug-level function) 2)
                                (and *wasm-reusable-scalars*
                                     (or (gethash function *wasm-reusable-scalars*)
                                         (let ((source (gethash function *wasm-worker-origins*)))
                                           (and (gethash source *wasm-reusable-scalars*)
                                                (eq function (gethash source *wasm-direct-workers*))))))
                                (and *wasm-named-recursion* *wasm-scoped-call-functions*
                                     (let ((source (car *wasm-named-recursion*)))
                                       (and (gethash source *wasm-scoped-call-functions*)
                                            (or (eq source function)
                                                (eq function (gethash source *wasm-direct-workers*)))))))
                            0
                            (hir-function-optimization-policy function))))
            (setf (aref eligible index)
              (if previous
                  (+ (max (logand previous 3) (logand policy 3))
                     (* 4 (min (logand 3 (ash previous -2)) (logand 3 (ash policy -2))))
                     (* 16 (max (ash previous -4) (ash policy -4))))
                  policy))))))
    (when (or *wasm-debug* *wasm-closed-worker-functions*
              (not (plusp *native-outlining-budget*)) (< count 2))
      (return-from native-outline-bodies (values bodies nil eligible)))
    (labels ((raw (type) (list (ecase type (:object #x6f) (:i32 #x7f) (:i64 #x7e) (:i31 #x6c) (:function #x70))))
             (signature (signature)
               (list (mapcar #'raw (first signature))
                     (mapcar #'raw (if (listp (second signature)) (second signature) (list (second signature))))))
             (native-type (bytes) (ecase (first bytes) (#x6f :object) (#x7f :i32) (#x7e :i64) (#x6c :i31) (#x70 :function))))
      (loop for body in bodies for i from 0 do
        (let* ((parameters (first (aref body-signatures i))) (types (mapcar #'raw parameters))
               (position 0) (groups nil))
          (multiple-value-setq (groups position) (native-read-uleb body position))
          (dotimes (group groups)
            (multiple-value-bind (size next) (native-read-uleb body position)
              (setq position (1+ next) types (append types (make-list size :initial-element (list (aref body next)))))))
          (push (list body (native-read-ops body) (coerce types 'vector) nil) records)))
      (multiple-value-bind (replacements helpers)
          (native-outline-code (coerce (nreverse records) 'vector)
            (map 'vector #'signature (append (mapcar #'cdr *native-imports*) (coerce body-signatures 'list))) eligible)
        (when helpers
          (compiler-statistic :optimization/outlined-region nil (length helpers)))
        (values (append replacements (mapcar #'first helpers))
                (mapcar (lambda (helper) (list (mapcar #'native-type (second helper)) (mapcar #'native-type (third helper)))) helpers)
                (concatenate 'vector eligible (make-array (length helpers) :initial-element 0)))))))

(defun native-private-scalar-return-source (function)
  ;; The implicit function block may forward the last result through one
  ;; local edge. The clone returns the dominating raw producer directly.
  (let* ((blocks (hir-function-blocks function))
         (entry (hir-function-entry function))
         (end (hir-block-terminator entry))
         (opcode (hir-instruction-opcode end))
         (target (first (hir-instruction-attributes end))))
    (cond ((and (not (cdr blocks)) (eq opcode :return))
           (first (hir-instruction-inputs end)))
          ((and (member opcode '(:jump :block-return)) target
                (cdr blocks) (null (cddr blocks)) (not (eq entry target))
                (null (hir-block-instructions target))
                (let ((return (hir-block-terminator target)))
                  (and (eq (hir-instruction-opcode return) :return)
                       (eq (first (hir-instruction-inputs return)) (first (hir-block-parameters target))))))
           (values (if (eq opcode :jump) (first (hir-instruction-inputs end))
                       (third (hir-instruction-inputs end))) end)))))

(defun native-private-scalar-contract (target plan &optional float-kind identity-kind)
  ;; Successful CHAR independently establishes a character. The bounded mask
  ;; uses the executable read and both arms of its existing integer plan.
  (let* ((value (native-private-scalar-return-source target))
         (producer (and value (hir-value-producer value)))
         (raw (and value (svref plan 25) (native-value-slot (svref plan 25) value)))
         (kind (and producer (gethash producer *wasm-inline-simple-calls*)))
         (mask (and raw (> (length raw) 7) (eq (first (svref raw 7)) :mask)
                    (second (svref raw 7))))
         (read (and mask (eq (svref mask 4) :array-snapshot)
                    (hir-value-producer (svref mask 1))))
         (contract
           (cond ((or float-kind identity-kind) (cons 64 nil))
                 ((eq kind :character-ref) (cons 32 'character))
                 ((and raw (eq (svref raw 1) value) (eq (svref raw 4) :result)
                       (or read (gethash producer *wasm-proven-integer-arithmetic*)))
                  (cons (svref raw 2) (and read (svref raw 6)))))))
    (when (and contract
               (every (lambda (instruction)
                        (or (eq instruction producer) (eq instruction read)
                            (and identity-kind (eq (hir-instruction-opcode instruction) :values)
                                 (null (cdr (hir-instruction-inputs instruction))))
                            (and (eq (hir-instruction-opcode instruction) :cell-new)
                                 (native-value-slot (svref plan 3) (hir-instruction-output instruction)))
                            (member (hir-instruction-opcode instruction)
                              '(:check-arguments :argument :capture :constant :function-reference
                                :copy :primary :block-enter :block-environment))))
                      (hir-block-instructions (hir-function-entry target))))
      (list (car contract) (cdr contract)
            (when (and read *wasm-private-scalar-arguments-enabled*)
              (let* ((*wasm-scalar-groups* (svref plan 7))
                     (index (fourth (hir-instruction-inputs read)))
                     (origin (native-predicate-source index (svref plan 21)))
                     (argument (hir-value-producer origin)))
                (when (and (%hir-instruction-p argument)
                           (eq (hir-instruction-opcode argument) :argument))
                  (first (hir-instruction-attributes argument)))))))))

(defun native-scalar-type-kind (type)
  (cond ((hir-type-proves-p type 'bit) 6)
        ((hir-type-proves-p type '(signed-byte 31)) 5)
        ((hir-type-proves-p type 'integer) 1)
        ((hir-type-proves-p type 'character) 2)
        ((hir-type-proves-p type 'single-float) 3)
        ((hir-type-proves-p type 'double-float) 4)))

(defun native-plan-reusable-scalar (function)
  (when (and (native-fixed-arity function)
             (< (hir-function-value-count function) 48)
             (< (logand 3 (ash (hir-function-optimization-policy function) -2)) 3)
             (gethash function *wasm-direct-workers*)
             (eql (gethash function *wasm-result-arities*) 1)
             (not (gethash function *wasm-optimization-barriers*)))
    (let* ((value (native-private-scalar-return-source function))
           (producer (and value (hir-value-producer value)))
           (kind (and producer (gethash producer *wasm-inline-simple-calls*)))
           (identity-value
             (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :values))
               (let ((inputs (hir-instruction-inputs producer)))
                 (unless (cdr inputs) (first inputs)))))
           (argument (and identity-value (hir-value-producer (hir-copy-source identity-value))))
           (identity (when (and (%hir-instruction-p argument) (eq (hir-instruction-opcode argument) :argument))
                       (first (hir-instruction-attributes argument))))
           (identity-kind (and identity (native-scalar-type-kind (hir-value-type identity-value))))
           (float-kind (when (native-float-call-op producer)
                         (let ((inputs (hir-instruction-inputs producer)))
                           (let ((kind (max (or (native-float-precision (third inputs)) 0)
                                            (or (native-float-precision (fourth inputs)) 0))))
                             (unless (zerop kind) kind)))))
           (plan (when (or float-kind identity-kind (member kind '(:character-ref :logand-general))
                           (gethash producer *wasm-proven-integer-arithmetic*))
                   (or (gethash function *wasm-function-plans*)
                       (setf (gethash function *wasm-function-plans*) (native-function-plan function)))))
           (contract (and plan (svref plan 1) (not (svref plan 4))
                          (native-private-scalar-contract function plan float-kind identity-kind))))
      (when contract
        (let ((worker (copy-seq function)) (arguments 0)
              (identity (and identity-kind identity))
              (*wasm-reused-values* (svref plan 21)) (*wasm-scalar-groups* (svref plan 7)))
          (when identity
            (setf arguments (ash identity-kind (* 3 identity))))
          (when (third contract)
            (setf (ldb (byte 3 (* 3 (third contract))) arguments) 1))
          (when float-kind
            (dolist (input (cddr (hir-instruction-inputs producer)))
              (let ((index (native-scalar-argument-index input)))
                (when index
                  (let ((kind (native-scalar-type-kind (hir-value-type input))))
                    (setf (ldb (byte 3 (* 3 index)) arguments)
                          (if (member kind '(1 3 4 5 6)) kind float-kind)))))))
          (unless *wasm-reusable-scalars*
            (setq *wasm-reusable-scalars* (make-hash-table :test 'eq)))
          (setf (gethash function *wasm-reusable-scalars*)
                (list worker (first contract) (second contract)
                      (or float-kind (and identity (if (> identity-kind 4) 1 identity-kind))
                          (if (eq (second contract) 'character) 2 1))
                      arguments identity))
          (setf (gethash worker *wasm-worker-origins*) function
                (gethash worker *native-function-indices*) (length *native-functions*))
          (setq *native-functions* (append *native-functions* (list worker))
                *native-call-stack-effects*
                (concatenate 'simple-vector *native-call-stack-effects* (vector (cons 10 3)))))))))

(defun native-reusable-scalar-section (bodies)
  ;; These indices describe the final physical entries in either module
  ;; layout. A loader must establish the same relationship independently.
  (let ((bodies (coerce bodies 'simple-vector)) (claims nil)
        (offset (length *native-imports*))
        (*wasm-import-counts* nil) (*wasm-shared-entries* nil)
        (*wasm-function-relocations* (make-hash-table :test 'eq)))
    (maphash
      (lambda (root contract)
        (let* ((worker (gethash root *wasm-direct-workers*))
               (raw-worker (first contract))
               (packet (gethash root *native-function-indices*))
               (boxed (gethash worker *native-function-indices*))
               (raw (gethash raw-worker *native-function-indices*)))
          (when (and packet boxed raw
                     (equalp (svref bodies boxed)
                             (native-boxed-scalar-worker raw-worker (fifth contract) (fourth contract)))
                     (equalp (svref bodies packet) (native-direct-wrapper root worker)))
            (pushnew (list (+ offset packet) (+ offset boxed) (+ offset raw)
                           (native-fixed-arity root) (fifth contract) (fourth contract))
                     claims :test #'equal))))
      *wasm-reusable-scalars*)
    (when claims
      (wasm-section 0 (with-wasm-bytes
        (wasm-name "miricl.scalars")
        (wasm-u32 (length claims))
        (dolist (claim (nreverse claims))
          (dolist (index claim) (wasm-u32 index))))))))

(defun native-private-scalar-argument-value (function index)
  (dolist (instruction (hir-block-instructions (hir-function-entry function)))
    (when (and (eq (hir-instruction-opcode instruction) :argument)
               (eql index (first (hir-instruction-attributes instruction))))
      (return (hir-instruction-output instruction)))))

(defun native-plan-private-scalar-site (unit certificates escaped)
  ;; Charge the whole unit before examining any certificate or forcing plans.
  (unless (loop for function in (hir-unit-functions unit) repeat 4096
                for policy = (hir-function-optimization-policy function)
                thereis (and (< (hir-function-debug-level function) 2)
                             (< (logand 3 (ash policy -2)) 3)))
    (return-from native-plan-private-scalar-site nil))
  (let ((fuel 4096) (targets (make-hash-table :test 'eq)) (contracts (make-hash-table :test 'eq))
        (ordinary nil) (raw-sites nil) (raw-target nil) (raw-contract nil))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel (+ 1 (hir-function-value-count function)
                                   (hir-function-block-count function))))
        (return-from native-plan-private-scalar-site nil))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel (1+ (length (hir-instruction-inputs instruction)))))
            (return-from native-plan-private-scalar-site nil)))))
    (maphash
      (lambda (initializer certificate)
        (declare (ignore initializer))
        (let ((target (car certificate)))
          (when (and (gethash target *wasm-direct-workers*)
                     (not (gethash target escaped))
                     (eql (gethash target *wasm-result-arities*) 1)
                     (< (hir-function-debug-level target) 2)
                     (not (and *wasm-optimization-barriers* (gethash target *wasm-optimization-barriers*))))
            (setf (gethash target targets) t))))
      certificates)
    (unless (plusp (hash-table-count targets))
      (return-from native-plan-private-scalar-site nil))
    (labels ((plan-for (function)
               (or (gethash function *wasm-function-plans*)
                   (setf (gethash function *wasm-function-plans*) (native-function-plan function))))
             (contract-for (target)
               (multiple-value-bind (contract present) (gethash target contracts)
                 (if present contract
                     (let ((plan (plan-for target)))
                       (setf (gethash target contracts)
                             (and (svref plan 1) (not (svref plan 4))
                                  (native-private-scalar-contract target plan)))))))
             (consider (function value plan character)
               (let* ((site (hir-value-producer (native-predicate-source value (svref plan 21))))
                      (output (and (%hir-instruction-p site) (hir-instruction-output site))))
                 (when (and output (eq (hir-instruction-opcode site) :call)
                            (native-value-slot (svref plan 8) output)
                            (not (member site (svref plan 13) :test #'eq))
                            (not (gethash site *wasm-inline-simple-calls*))
                            (not (gethash site *wasm-inline-vector-calls*)))
                   (let* ((arguments (hir-instruction-inputs site))
                          (target (native-known-callee (second arguments)))
                          (contract (and (gethash target targets) (not (eq target function))
                                         (eql (- (length arguments) 2) (native-fixed-arity target))
                                         (contract-for target))))
                     (when (and contract (eq character (eq (second contract) 'character)))
                       (unless ordinary (setq ordinary (list site target contract)))
                       (unless *wasm-private-scalar-arguments-enabled*
                         (return-from native-plan-private-scalar-site
                           (values (list site) target (first contract) (second contract))))
                       (let* ((index (third contract))
                              (record (and index (svref plan 25)
                                           (native-value-slot (svref plan 25) (nth (+ 2 index) arguments)))))
                         (when (and record (eq (svref record 4) :result)
                                    (or (null raw-target) (eq raw-target target)))
                           (setq raw-target target raw-contract contract)
                           (pushnew site raw-sites :test #'eq)))))))))
      (dolist (function (hir-unit-functions unit))
        (let ((policy (hir-function-optimization-policy function)) (plan nil))
          (when (and (< (hir-function-debug-level function) 2)
                     (< (logand 3 (ash policy -2)) 3))
            (dolist (block (hir-function-blocks function))
              (dolist (instruction (hir-block-instructions block))
                (let* ((kind (gethash instruction *wasm-inline-simple-calls*))
                       (character (eq kind :character-code))
                       (character-store (eq kind :character-set))
                       (inputs (hir-instruction-inputs instruction))
                       (bit-op (and (member kind '(:logand-general :ash-general :standard-call))
                                    (= (length inputs) 4)
                                    (member (native-scalar-call-name inputs) '(logand logior logxor ash)))))
                  (when (or character character-store bit-op (gethash instruction *wasm-proven-integer-arithmetic*))
                    (unless plan (setq plan (plan-for function)))
                    (when (and (member block (svref plan 17) :test #'eq)
                               (or (not character-store)
                                   (not (native-value-slot (svref plan 6) (hir-instruction-output instruction)))))
                      (let ((*wasm-scalar-groups* (svref plan 7)))
                        (if character-store
                            (consider function (third inputs) plan t)
                            (dolist (operand (cddr inputs))
                              (consider function operand plan character)))))))))))))
    (cond (raw-sites (values (nreverse raw-sites) raw-target (first raw-contract) (second raw-contract)
                       (third raw-contract)))
          (ordinary (values (list (first ordinary)) (second ordinary) (first (third ordinary))
                      (second (third ordinary)))))))

(defun native-private-scalar-edges-valid-p (bodies direct-exports entries)
  ;; Audit final indices after relocation, pruning and outlining. The clone
  ;; can be reached by a typed CALL; no runtime object can acquire its entry.
  (let* ((worker *wasm-private-scalar-worker*)
         (index (gethash worker *native-function-indices*))
         (read (and *wasm-private-scalar-argument*
                    (position "native-aref-snapshot64" *native-imports* :key #'first :test #'equal)))
         (callers (make-hash-table :test 'eq)) (allowed (make-hash-table :test 'eql))
         (seen (make-hash-table :test 'eql)) (fuel 65536))
    (unless (and index (or read (null *wasm-private-scalar-argument*))
                 (not (member worker *wasm-referenced-functions* :test #'eq))
                 (not (member worker entries :test #'eq))
                 (not (member worker direct-exports :key #'second :test #'eq)))
      (return-from native-private-scalar-edges-valid-p nil))
    (dolist (site *wasm-private-scalar-sites*)
      (incf (gethash (hir-value-owner (hir-instruction-output site)) callers 0)))
    (maphash (lambda (caller count)
               (dolist (entry (list caller (gethash caller *wasm-direct-workers*)))
                 (let ((body-index (gethash entry *native-function-indices*)))
                   (when body-index
                     (setf (gethash body-index allowed) (max count (gethash body-index allowed 0)))))))
             callers)
    (loop with target = (+ index (length *native-imports*))
          for body in bodies for body-index from 0 for called = 0 for checked-reads = 0 do
      (when (minusp (decf fuel (length body)))
        (return-from native-private-scalar-edges-valid-p nil))
      (loop for op across (native-read-ops body t) do
        (when (and read (eql body-index index) (eql read (native-op-argument op))
                   (= #x10 (native-op-code op)))
          (incf checked-reads))
        (when (eql target (native-op-argument op))
          (unless (= #x10 (native-op-code op))
            (return-from native-private-scalar-edges-valid-p nil))
          (incf called)))
      (when (and read (eql body-index index) (/= checked-reads 1))
        (return-from native-private-scalar-edges-valid-p nil))
      (when (plusp called)
        (unless (and (= called (gethash body-index allowed 0))
                     (or (not *wasm-private-scalar-framed*)
                         (native-named-worker-protocol-p body target nil called)))
          (return-from native-private-scalar-edges-valid-p nil))
        (setf (gethash body-index seen) called)))
    (and (plusp (hash-table-count seen))
         (block admitted
           (maphash (lambda (caller count)
                      (unless (or (eql count (gethash (gethash caller *native-function-indices*) seen))
                                  (eql count (gethash
                                               (gethash (gethash caller *wasm-direct-workers*) *native-function-indices*)
                                               seen)))
                        (return-from admitted nil)))
                    callers)
           t))))

(defun native-named-worker-protocol-p (body target sites &optional (scoped 0))
  ;; The existing optimizer preserves structured scopes and effects. Also
  ;; audit the resolved call and transfer protocol for every admitted edge.
  (let ((tail 0) (state nil) (previous nil)
        (enter (position "native-frame-enter" *native-imports* :key #'first :test #'equal))
        (leave (position "native-frame-leave" *native-imports* :key #'first :test #'equal))
        (replace-frame (position "native-tail-enter" *native-imports* :key #'first :test #'equal)))
    (when sites
      (maphash (lambda (reference site)
                 (declare (ignore reference))
                 (if (eq (cdr site) :tail) (incf tail) (incf scoped))) sites))
    (loop for op across (native-read-ops body)
          for code = (native-op-code op) for argument = (native-op-argument op)
          for next =
          (cond
            ((and (= code #x10) (eql argument target))
             (decf scoped) (and (eq state :try) :normal-leave))
            ((and (= code #x12) (eql argument target))
             (decf tail) (if (eq state :tail) :done nil))
            ((and (= code #x10) (eql argument enter))
             (and (null state) :enter))
            ((= code #x1f)
             (and (eq state :enter)
                  (let ((start (native-op-start op))
                        (end (native-op-end op)))
                    (and (= (- end start) 6)
                         (loop for byte in '(#x1f #x40 1 0 0 0)
                               for offset from start
                               always (eql byte (aref body offset)))))
                  :try))
            ((and (= code #x10) (eql argument leave))
             (when (and previous (= (native-op-code previous) #x41))
               (case state
                 (:normal-leave (and (eql (native-op-argument previous) 1) :exceptional-leave))
                 (:exceptional-leave (and (eql (native-op-argument previous) 0) :rethrow)))))
            ((= code #x08)
             (and (eq state :rethrow) (eql argument 0) :done))
            ((and (= code #x10) (eql argument replace-frame))
             (and (null state) :tail))
            (t :unchanged))
          do (case next
               ((nil) (return-from native-named-worker-protocol-p nil))
               (:unchanged)
               (:done (setq state nil))
               (otherwise (setq state next)))
             (setq previous op))
    (and (null state) (zerop scoped) (zerop tail))))

(defun native-named-worker-edges-valid-p (bodies function direct-exports entries)
  ;; The named source path publishes one root with one literal bank. Check the
  ;; final generic adapter, direct descriptor, and all incoming references.
  (let ((sites (native-plan-named-call-sites function (gethash function *wasm-function-plans*))))
    (unless sites (return-from native-named-worker-edges-valid-p t))
    (let* ((worker (gethash function *wasm-direct-workers*))
           (index (gethash worker *native-function-indices*))
           (arity (gethash function *wasm-scoped-call-functions*))
           (descriptor (first direct-exports))
           (imports (length *native-imports*))
           (*wasm-shared-entries* (> (hash-table-count *wasm-direct-workers*) 1))
           (fuel 65536))
      (unless (and (null entries) (eq function (first *native-functions*))
                   (eql (gethash function *native-function-indices*) 0)
                   index (plusp index)
                   (null (cdr direct-exports)) (eq (second descriptor) worker)
                   (eql (third descriptor) arity) (eql (fifth descriptor) 0)
                   (equal (native-result-signature worker)
                          '((:object :object :object :object :object) :object))
                   (assoc (if *wasm-shared-entries* "native-shared-entry" "native-check-arguments")
                          *native-imports* :test #'equal)
                   (or *wasm-shared-entries* (zerop arity)
                       (assoc "native-argument" *native-imports* :test #'equal))
                   (let ((*wasm-import-counts* nil) (*wasm-function-relocations* (make-hash-table :test 'eq)))
                     (equalp (first bodies) (native-direct-wrapper function worker))))
        (return-from native-named-worker-edges-valid-p nil))
      (dolist (other *native-functions*)
        (unless (or (eq other function) (eq other worker))
          (when (member (gethash other *native-function-indices*) (list 0 index))
            (return-from native-named-worker-edges-valid-p nil))))
      (loop for body in (rest bodies) for i from 1 do
        (when (minusp (decf fuel (length body)))
          (return-from native-named-worker-edges-valid-p nil))
        (loop for op across (native-read-ops body t) for target = (native-op-argument op) do
          ;; A declarative element entry is harmless. An actual REF.FUNC
          ;; would let another code path choose the closure context.
          (when (or (= target imports)
                    (and (= target (+ imports index))
                         (or (/= i index) (= (native-op-code op) #xd2))))
            (return-from native-named-worker-edges-valid-p nil))))
      (native-named-worker-protocol-p (nth index bodies) (+ imports index) sites))))

(defun emit-native-module-with-results (unit entries entry-tables named-recursion sealed-unit epoch-polling)
  "Emit a verified unit. Internal callers must retain ownership until emission."
  (declare (type hir-unit unit) (type list entries))
  (let* ((*wasm-named-recursion* named-recursion)
         (*wasm-named-single-result* nil)
         (*wasm-named-call-plan* nil)
         (*wasm-literals* (make-array 16 :adjustable t :fill-pointer 0))
         (*wasm-literal-index* (make-hash-table :test 'eq))
         (*wasm-literal-groups* (and entries (native-literal-groups entries)))
         (*native-functions* (hir-unit-functions unit))
         (*wasm-optimization-barriers* (hir-optimization-barriers unit))
         (factory (first *native-functions*))
         (*wasm-debug* (some (lambda (function) (>= (hir-function-debug-level function) 2)) *native-functions*))
         (sealed (and (eq sealed-unit unit) (not *compiler-bootstrapping*) (not *wasm-debug*)
                      (not *wasm-closed-worker-functions*) (not *wasm-fixed-call-emitter*)))
         (primary-flow (multiple-value-list
                         (hir-primary-result-sources unit *wasm-optimization-barriers* (and sealed factory))))
         (certificates (second primary-flow))
         (certificates (and certificates (plusp (hash-table-count certificates)) certificates))
         (analysis-entries (or entries (list factory)))
         (*wasm-result-callees* (hir-primary-result-facts unit (first primary-flow) #'hir-result-callee))
         (*hir-standard-operations* (hir-primary-result-facts unit (first primary-flow) #'hir-standard-operation-leaf))
         (*wasm-private-scalar-worker* nil) (*wasm-private-scalar-sites* nil)
         (*wasm-reusable-scalars* nil)
         (*wasm-private-scalar-argument* nil) (*wasm-private-scalar-ingress* nil)
         (*wasm-private-scalar-framed* nil)
         (*wasm-private-scalar-width* nil) (*wasm-private-scalar-return* nil) (*wasm-private-scalar-forward* nil)
         (*wasm-private-scalar-type* nil)
         ;; These roots come directly from validated instruction operands.
         (type-analysis (multiple-value-list
                         (infer-hir-types-checked unit (native-type-roots unit) t analysis-entries nil
                           *wasm-result-callees* *hir-standard-operations* nil certificates
                           *wasm-optimization-barriers*)))
         (*wasm-inferred-types* (first type-analysis))
         (*wasm-type-guards* (second type-analysis))
         (*wasm-function-plans* (make-hash-table :test 'eq))
         (*wasm-argument-usages* (make-hash-table :test 'eq))
         (*wasm-primary-functions* nil)
         ;; The host runtime linker currently accepts only single-result
         ;; workers. Its explicit closed-worker contract retains that lowering.
         (result-analysis (and *wasm-native-multiple-results* (not *wasm-closed-worker-functions*)
                               (not *compiler-bootstrapping*)
                               (not *wasm-debug*) (multiple-value-list (hir-result-arities-checked unit *wasm-result-callees* *hir-standard-operations*))))
         (*wasm-result-arities* (first result-analysis))
         (*wasm-multiple-result-functions* (make-hash-table :test 'eq))
         (*wasm-frameless-multiple-functions* (make-hash-table :test 'eq))
         (*wasm-boxed-result-workers* (make-hash-table :test 'eq))
         (*wasm-local-call-functions* nil)
         (*wasm-scoped-call-functions* nil)
         (direct (multiple-value-list (native-direct-workers *native-functions*)))
         (*wasm-direct-workers* (first direct)) (*wasm-worker-origins* (second direct))
         (*native-functions* (append *native-functions* (third direct)))
         (*native-function-indices* (native-function-indices *native-functions*))
         (*native-call-stack-effects*
           (concatenate 'simple-vector
             (mapcar (lambda (import) (native-signature-stack-effect (cdr import))) *native-imports*)
             (mapcar (lambda (function) (if (gethash function *wasm-worker-origins*) 11 5)) *native-functions*)))
         (*wasm-function-relocations* (make-hash-table :test 'eq))
         (*wasm-import-counts* (make-hash-table :test 'equal))
         (*native-import-indices* nil)
         (*native-import-stack-effects* nil)
         (*native-scalar-stack-effects* nil)
         (*wasm-referenced-functions* nil)
         (*wasm-object-types* nil) (*wasm-exceptions* nil))
    (let ((*wasm-inline-vector-calls* (make-hash-table :test 'eq))
          (*wasm-low-field-ash-candidates* nil)
          (*wasm-literal-byte-extract-owners* nil)
          (*wasm-checked-integer-copies* nil)
          (*wasm-inline-simple-calls* (make-hash-table :test 'eq))
          (*wasm-snapshot-mask-functions*
            (unless (or *wasm-fixed-call-emitter* *wasm-closed-worker-functions*)
              (make-hash-table :test 'eq)))
          (*wasm-dynamic-rests* nil)
          (*wasm-proven-integer-arithmetic* (make-hash-table :test 'eq))
          (*wasm-proven-cell-types* nil)
          (*wasm-cons-forward-blocks* (make-hash-table :test 'eq))
          (*wasm-proper-list-checks* nil)
          (*wasm-inline-references* (make-hash-table :test 'eq)))
    (native-plan-inline-vectors unit)
    (native-plan-inline-simple unit (and certificates analysis-entries) certificates)
    (setq *wasm-dynamic-rests* (native-plan-dynamic-rests unit))
    (native-keep-shared-references unit)
    (setq *wasm-proper-list-checks*
      (hir-proper-list-checks unit *wasm-inline-simple-calls*
                            *wasm-inline-references* *wasm-optimization-barriers*))
    (setq *wasm-primary-functions* (native-primary-functions unit analysis-entries))
    (when *wasm-primary-functions*
      (maphash (lambda (function present)
                 (declare (ignore present))
                 (remhash function *wasm-function-plans*))
               *wasm-primary-functions*))
    (native-plan-argument-usages unit)
    (when *wasm-result-arities*
     (let ((used (make-hash-table :test 'eq)))
      (dolist (function (hir-unit-functions unit))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (hir-block-instructions block))
            (when (eq (hir-instruction-opcode instruction) :call)
              (let* ((inputs (hir-instruction-inputs instruction))
                     (target (hir-result-callee (second inputs) *wasm-result-callees*)))
                (when (and target (eql (- (length inputs) 2) (native-fixed-arity target)))
                  (setf (gethash target used) t)))))))
      (setq *wasm-frameless-multiple-functions* (native-multiple-result-functions unit used)
            *wasm-multiple-result-functions*
            (native-multiple-result-bodies unit *wasm-frameless-multiple-functions*))
      (dolist (function (hir-unit-functions unit))
        (let ((count (gethash function *wasm-multiple-result-functions*)))
          (when count
            (setf (svref *native-call-stack-effects*
                    (+ (length *native-imports*)
                       (gethash (gethash function *wasm-direct-workers*) *native-function-indices*)))
                  (cons 5 count))
            (let ((adapter (copy-seq function)))
              (setf (gethash function *wasm-boxed-result-workers*) adapter
                    (gethash adapter *wasm-worker-origins*) function)
              (setq *native-functions* (append *native-functions* (list adapter))
                    *native-call-stack-effects* (concatenate 'simple-vector *native-call-stack-effects* (vector 11))))))))
      (setq *native-function-indices* (native-function-indices *native-functions*)))
    (setq *wasm-local-call-functions* (native-local-call-functions unit))
    (when (and *wasm-private-scalar-enabled* (not sealed) (not *wasm-debug*)
               (not *compiler-bootstrapping*)
               *wasm-result-arities* (null *wasm-fixed-call-emitter*) (null *wasm-closed-worker-functions*))
      (if entries
          (loop for function in (hir-unit-functions unit) repeat 4096
                do (native-plan-reusable-scalar function))
          (unless (cdr (hir-unit-functions unit)) (native-plan-reusable-scalar factory))))
    (when (and *wasm-private-scalar-enabled* certificates
               *wasm-result-arities* (third type-analysis))
      (multiple-value-bind (sites target width representation argument)
          (native-plan-private-scalar-site unit certificates (third type-analysis))
        (when sites
          (let ((pending nil))
            (when
                (let ((*wasm-private-scalar-sites* sites) (*wasm-private-scalar-width* width)
                      (*wasm-private-scalar-type* representation) (*wasm-private-scalar-argument* argument))
                  (dolist (site sites t)
                    (let* ((output (hir-instruction-output site)) (caller (hir-value-owner output))
                           (plan (or (cdr (assoc caller pending))
                                     (let ((plan (native-function-plan caller)))
                                       (push (cons caller plan) pending)
                                       plan)))
                           (table (svref plan 25))
                           (raw (and table (native-value-slot table output)))
                           (input (and table argument
                                       (native-value-slot table (nth (+ 2 argument) (hir-instruction-inputs site))))))
                      (unless (and raw (eq (svref raw 1) output) (zerop (svref raw 5))
                                   (or (null argument) (and input (eq (svref input 4) :result))))
                        (when argument (throw 'native-multiple-result-fallback :retry-sealed-argument))
                        (return nil)))))
              ;; Publish the plans and the raw-only ABI together after preflight.
              (let ((worker (copy-seq target)))
                (setq *wasm-private-scalar-sites* sites *wasm-private-scalar-worker* worker
                      *wasm-private-scalar-argument* argument
                      *wasm-private-scalar-framed* (eql (hir-function-debug-level target) 1)
                      *wasm-private-scalar-width* width *wasm-private-scalar-type* representation)
                (dolist (entry pending) (setf (gethash (car entry) *wasm-function-plans*) (cdr entry)))
                (setf (gethash worker *wasm-worker-origins*) target
                      (gethash worker *native-function-indices*) (length *native-functions*))
                (setq *native-functions* (append *native-functions* (list worker))
                      *native-call-stack-effects* (concatenate 'simple-vector *native-call-stack-effects* (vector 11)))))))))
    (setq *wasm-scoped-call-functions* (native-scoped-call-functions unit))
    (when (and named-recursion *wasm-result-arities* *wasm-scoped-call-functions*)
      (let* ((function (car named-recursion))
             (plan (gethash function *wasm-function-plans*))
             (sites (and plan (native-plan-named-call-sites function plan))))
        (when (and sites (native-named-single-result-p function sites))
          (compiler-statistic :proof/named-single-result)
          ;; Replan cardinality and its dependent stores as one unit. The
          ;; worker-rewrite retry drops both the proof and call substitutions.
          (setq *wasm-named-single-result* sites)
          (remhash function *wasm-function-plans*))))
    ;; Local sizing, body sharing and import relocation operate on the first
    ;; emitted bodies. Discovering imports no longer requires a throwaway
    ;; emission of every instruction and literal in the application.
    (let* ((*native-imports* *native-imports*)
         (bodies (emit-native-bodies entries))
         (bodies (native-prune-module (native-batch-work-bodies bodies epoch-polling) entries))
         (imported-globals (if (assoc "native-poll-token" *native-imports* :test #'equal) 2 0))
         (outlining (multiple-value-list (native-outline-bodies bodies)))
         (bodies (first outlining)) (outline-signatures (second outlining))
         (outline-policies (third outlining))
         (direct-exports
           (loop for entry in (or entries (list (first *native-functions*))) for i from 0
                 for worker = (or (gethash entry *wasm-boxed-result-workers*) (gethash entry *wasm-direct-workers*))
                 when (and worker (gethash worker *native-function-indices*))
                 collect (list (format nil "direct-~A-~D" (if entries (format nil "entry~D" i) "entry")
                                      (native-fixed-arity entry)) worker (native-fixed-arity entry)
                               (native-argument-usage entry) i)))
         (argument-usages
           (loop for entry in direct-exports when (fourth entry)
                 collect (list (format nil "argument-usage-~A"
                                       (if entries (format nil "entry~D" (fifth entry)) "entry")) (fourth entry))))
         (entry-sections
           (progn
             (when (and named-recursion *wasm-scoped-call-functions*
                        (gethash (car named-recursion) *wasm-scoped-call-functions*)
                        (not (native-named-worker-edges-valid-p bodies (car named-recursion) direct-exports entries)))
               (throw 'native-multiple-result-fallback :retry-named-recursion))
             (when (and *wasm-private-scalar-worker*
                        (not (native-private-scalar-edges-valid-p bodies direct-exports entries)))
               (throw 'native-multiple-result-fallback
                 (if *wasm-private-scalar-argument* :retry-sealed-argument :retry-sealed-scalar)))
             (multiple-value-list
               (native-entry-sections entries direct-exports argument-usages entry-tables imported-globals))))
         (body-types (make-array (length bodies) :initial-element nil))
         (type-prefix (if *wasm-object-types* 2 0))
         (signatures (remove-duplicates
                       (append (mapcar #'cdr *native-imports*) (list '((:object :object) :object))
                         (mapcar #'native-result-signature *native-functions*) outline-signatures
                         (when *wasm-exceptions* (list '(nil nil)))) :test #'equal))
         (bytes (progn
           (loop for signature in outline-signatures
                 for i from (- (length bodies) (length outline-signatures))
                 do (setf (aref body-types i) signature))
           (maphash (lambda (worker source) (declare (ignore source))
             (let ((index (gethash worker *native-function-indices*)))
               (when index (setf (aref body-types index) (native-result-signature worker))))) *wasm-worker-origins*)
           (with-wasm-bytes
           (dolist (byte '(0 97 115 109 1 0 0 0)) (wasm-byte byte))
           (wasm-section 1 (with-wasm-bytes
             (wasm-u32 (+ (length signatures) type-prefix))
             (when *wasm-object-types* (emit-native-object-types))
             (dolist (signature signatures)
               (wasm-byte #x60) (wasm-u32 (length (first signature)))
               (dolist (type (first signature)) (wasm-type type))
               (let ((results (second signature)))
                 (if (listp results)
                     (progn (wasm-u32 (length results)) (dolist (type results) (wasm-type type)))
                     (progn (wasm-u32 1) (wasm-type results)))))))
           (wasm-section 2 (with-wasm-bytes
             (wasm-u32 (+ (length *native-imports*) (if *wasm-exceptions* 1 0) imported-globals))
             (dolist (import *native-imports*)
               (wasm-name "m")
               ;; Runtime helper names have the ASCII prefix NATIVE-. Skip
               ;; its encoded bytes without allocating another Lisp string.
               (let ((name (utf8-bytes (first import))))
                 (wasm-u32 (- (length name) 7)) (wasm-append name 7))
               (wasm-byte 0)
               (wasm-u32 (+ type-prefix (position (cdr import) signatures :test #'equal))))
             (when (plusp imported-globals)
               ;; Canonical modules grant read-only access. The installer
               ;; checks these exact bytes before binding the live globals.
               (wasm-name "p") (wasm-name "epoch")
               (wasm-byte 3) (wasm-type :i64) (wasm-byte 0)
               (wasm-name "p") (wasm-name "active")
               (wasm-byte 3) (wasm-type :i32) (wasm-byte 0))
             (when *wasm-exceptions*
               (wasm-name "m") (wasm-name "transfer") (wasm-byte 4) (wasm-byte 0)
               (wasm-u32 (+ type-prefix (position '(nil nil) signatures :test #'equal))))))
           (wasm-section 3 (with-wasm-bytes
             (wasm-u32 (length bodies))
             (dotimes (i (length bodies))
               (wasm-u32 (+ type-prefix (position (or (aref body-types i)
                 '((:object :object) :object)) signatures :test #'equal))))))
           (when (first entry-sections) (wasm-append (first entry-sections)))
           (wasm-append (second entry-sections))
           (when *wasm-reusable-scalars* (native-reusable-scalar-section bodies))
           ;; Current-version compilation policy survives FASLs, native bundle
           ;; linking and the final runtime-image pass without public exports.
           (wasm-section 0 (with-wasm-bytes
             (wasm-name "miricl.policy") (wasm-u32 (length outline-policies))
             (loop for policy across outline-policies do (wasm-byte (or policy 0)))))
           (wasm-section 10 (with-wasm-bytes
             (wasm-u32 (length bodies))
             (dolist (body bodies) (wasm-u32 (length body)) (wasm-append body))))))))
    (values bytes (if entries
                     (mapcar (lambda (entry) (coerce (first (gethash entry *wasm-literal-groups*)) 'simple-vector)) entries)
                     (coerce *wasm-literals* 'simple-vector))
      (when *wasm-debug*
        (vector bytes
          (coerce (loop for function in *native-functions*
                        for index = (gethash function *native-function-indices*)
                        collect (and index (>= (hir-function-debug-level function) 2)
                                     (+ (length *native-imports*) index))) 'simple-vector)
          (coerce (mapcar (lambda (entry) (position entry *native-functions* :test #'eq)) entries) 'simple-vector))))))))

(defun compile-native-hir (payload &key name)
  "Compile stored HIR directly, without reconstructing or retaining source."
  (ensure-compiler)
  (let* ((unit (hir-code-to-unit payload)) (root (first (hir-unit-functions unit))))
    (when name (setf (hir-function-name root) name))
    (multiple-value-bind (bytes literals debug) (emit-native-module-checked unit nil t nil nil t)
      (multiple-value-bind (function native) (compiler-primitive 17 bytes literals name)
        (unless native (error "This host cannot instantiate compiled Wasm modules."))
        (let ((arity (native-fixed-arity root)))
          (when arity (setq function (compiler-primitive 30 function arity))))
        (if debug (compiler-primitive 34 function debug 0) function)))))

(defun compile-native (lambda-expression &key name)
  "Compile a lambda to native Wasm using the current host's module loader."
  (compile-native-source lambda-expression name nil))

(defun compile-native-source (lambda-expression name named-definition)
  ;; Restrict the caller's one-use promise to this entry. Macroexpansion and
  ;; bootstrap may explicitly compile other, persistent functions.
  (let ((single-use *wasm-single-use-entry*) (*wasm-single-use-entry* nil))
   (ensure-compiler)
   (let* ((definition (and named-definition name (symbolp name) (list name)))
          (unit (compile-source-hir lambda-expression name nil definition))
          (references (cdr definition)))
    (multiple-value-bind (bytes literals debug)
        ;; Lowering and emission retain exclusive ownership of this unit.
        (let ((*wasm-single-use-entry* single-use))
          (emit-native-module-checked unit nil t
            (when references (cons (first (hir-unit-functions unit)) references)) nil t))
      (multiple-value-bind (function native) (compiler-primitive 17 bytes literals name)
        (unless native (error "This host cannot instantiate compiled Wasm modules."))
        (let ((arity (native-fixed-arity (first (hir-unit-functions unit)))))
          (when arity (setq function (compiler-primitive 30 function arity)))
          (if debug (values (compiler-primitive 34 function debug 0)) function)))))))

(defun compile-native-module (definitions &key (entry-points nil entriesp) (inline t))
  "Compile a sealed group of DEFUN forms to one Wasm module.
Return an alist of ENTRY-POINTS and their native functions, module bytes, and HIR.
Global function bindings are not changed. INLINE enables bounded small-function
inlining; explicit local INLINE and NOTINLINE declarations take precedence."
  (declare (optimize (safety 3)))
  (compiler-list-length definitions "module definitions")
  (unless entriesp
    (dolist (definition definitions)
      (unless (>= (compiler-list-length definition "module definition") 3)
        (compiler-input-fail "module definition" definition "expected (DEFUN name lambda-list body...)")))
    (setq entry-points (mapcar #'second definitions)))
  (let ((unit (compile-module-to-hir definitions :entry-points entry-points :inline inline)))
    (multiple-value-bind (bytes literals debug) (emit-native-module-checked unit nil t nil unit t)
      (multiple-value-bind (factory native) (compiler-primitive 17 bytes literals nil)
        (unless native (error "This host cannot instantiate compiled Wasm modules."))
        (when debug (setq factory (compiler-primitive 34 factory debug 0)))
        (values (funcall factory) bytes unit)))))

(defparameter *native-runtime-image-functions*
  ;; These shared runtime paths are also reached by native applications.
  ;; Keep their original declarations and conservative singleton organization.
  ;; Calls to private MIRICL runtime functions retain live binding behavior.
  '(miricl::%qualified miricl::%primary miricl::%auxiliary
    miricl::%execute-standard miricl::%around
    miricl::%dispatch-cache-permitted-p miricl::%quote miricl::%coerce
    mapc nreverse member (setf slot-value) (setf cdr)))

(defparameter *native-compiler-cold-functions*
  ;; Keep opt-in expansion machinery and rarely used source/report builders
  ;; in the ordinary compiler library. Native admission wrappers avoid an
  ;; interpreted call on the compact compiler's normal path. The image builder
  ;; may compile these functions in its disposable bootstrap generation.
  '(emit-native-i31-operand emit-native-i31-step-worker emit-native-i31-comparison-worker
    native-tiered-unary native-tiered-source native-compute-tiered-counters
    native-compute-tiered-comparisons native-compute-tiered-locals
    emit-native-tiered-copy emit-native-tiered-small emit-native-tiered-i64
    emit-native-tiered-materialize emit-native-tiered-classify emit-native-tiered-update
    emit-native-tiered-instruction-worker emit-native-tiered-comparison-worker
    native-ordinary-snapshot-p native-vector-snapshot-source native-plan-low-field-add
    native-plan-low-field-ash-candidate native-plan-low-field-ash native-low-field-ash-chain
    native-plan-low-field-chain native-plan-literal-byte-extract
    native-vector-snapshot-call-p native-vector-snapshot-region
    native-private-scalar-return-source native-private-scalar-contract native-plan-private-scalar-site native-private-scalar-edges-valid-p
    native-plan-reusable-scalar native-reusable-scalar-section native-boxed-scalar-worker
    native-scalar-work-loop native-batch-scalar-work native-epoch-poll-body
    compilation-statistics-report compiler-input-summary
    hir-assert-form hir-correctable-place-form hir-restart-options
    hir-condition-initializer hir-define-condition hir-restart-expression
    hir-literal-counter-cells hir-bounded-integer-cells hir-literal-clock-cells
    structure-info class-info method-info))

(defparameter *native-compiler-seeds*
  (remove-if (lambda (name) (member name *native-compiler-cold-functions*))
  '(compiler-primitive record-compilation-statistic compilation-statistics-report
                   standard-function-contract standard-function-name-p standard-function-property
                   standard-function-result-type standard-function-result-count
                   standard-function-predicate-type standard-function-list-path
                   native-standard-function-name-p native-standard-operation native-search-list-code native-list-path-code native-standard-effect-free-p
                   wasm-byte wasm-append wasm-u32 wasm-u32-width wasm-s32 wasm-s64 wasm-name wasm-type wasm-section wasm-call
                   native-signature-stack-effect wasm-remember-local wasm-get wasm-set wasm-integer wasm-local wasm-null wasm-gc wasm-as-object
                   emit-native-tag-test
                   ;; These small constructors and declaration readers are
                   ;; exercised while compiling nearly every remaining kernel.
                   compiler-input-summary compiler-input-fail compiler-type-specifier compiler-variable-name compiler-function-name
                   compiler-lambda-list compiler-body compiler-source-form validate-hir-shapes
                   hir-new-value hir-new-block hir-emit hir-terminate hir-single hir-multiple hir-nth-value hir-constant
                   hir-clear-source
                   ;; FASL loading and lazy application modules traverse these
                   ;; checked boundaries too. Keep the complete debug-copy
                   ;; path native, including the per-binding type traversal.
                   validate-fasl-graph
                   hir-code-error hir-debug-check-write pack-hir-blocks unpack-hir-blocks copy-hir-debug-info copy-hir-debug-sites copy-hir-debug-type copy-hir-debug-type-checked copy-hir-parameters
                   read-hir-code-instruction validate-hir-code copy-hir-code verify-hir-code-operands
                   verify-hir-code-dominance hir-code-to-unit hir-code-to-unit-checked
                   hir-special-p hir-declarations hir-intersect-types hir-type-environment hir-bound-type-environment hir-declared-argument-types
                   native-optimization-level native-size-policy native-debug-level native-safety-level hir-note-optimization-policy hir-inline-limit
                   native-canonical-builtin-code emit-native-object-types wasm-literal wasm-list wasm-arguments
                   native-active-branch-test native-removable-p native-live-inputs native-direct-cells native-forward-blocks native-reachable-blocks native-loop-region-order native-loop-plan native-live-values native-local-layout native-single-calls native-full-groups native-proven-truth native-proof-reachable-blocks native-scalar-groups native-plan-branch-tests native-stable-predicate-p native-predicate-source native-predicate-key native-plan-cons-forwarding native-plan-predicate-reuse native-predicate-aliases native-block-targets native-single-return-target-p
                   native-argument-usage native-primary-functions native-plan-argument-usages native-call-usage emit-native-call-operands
                   native-boxed-local-p emit-native-primary-call5 native-direct-workers native-direct-worker-body native-optimize-typed-body native-direct-wrapper native-boxed-result-worker native-direct-fallback native-direct-relocations
                   native-simple-body-p native-simple-callee-p native-multiple-result-functions native-multiple-result-bodies native-local-call-functions emit-native-local-call
                   native-named-call-p native-named-call-mode native-named-single-call-p native-plan-named-call-sites native-named-single-result-p native-named-worker-protocol-p native-named-worker-edges-valid-p native-scoped-call-functions native-scoped-call-target native-scoped-call-sites-p emit-native-call-frame-enter emit-native-call-frame-leave emit-native-scoped-call
                   native-no-prefix-apply-p native-captured-call-p native-captured-result-groups emit-native-captured-test emit-native-captured-list
                   native-counted-result-return-p
                   native-result-signature emit-native-result emit-native-results native-local-result-groups native-call-frame-elidable-p native-multiple-call-count emit-native-multiple-call
                   emit-native-fixed-value-call emit-native-fixed-call native-type-integer-width native-compute-integer-width native-integer-width emit-native-integer-box
                   emit-native-unboxed-integer native-integer-result-fits-p native-inline-shift-opcode native-inline-nonnegative-division-opcode native-inline-integer-opcode emit-native-integer-chain emit-native-inline-integer emit-native-i31-operand emit-native-i31-step emit-native-i31-comparison emit-native-integer-call
                   native-simple-vector-value-p native-list-value-p native-vector-call-width emit-native-vector-call emit-native-list-call emit-native-character-call
                   native-plan-inline-simple native-plan-dynamic-rests native-cell-storage-roots native-plan-checked-integer-copies native-plan-proven-integers native-plan-proven-integer-arithmetic emit-native-proven-integer-arithmetic native-plan-proven-integer-comparisons emit-native-proven-integer-comparison
                   native-plan-character-code-fusion native-keep-shared-references native-plan-inline-vectors emit-native-inline-vector
                   native-tail-forward-p native-tail-environment-p native-tail-calls native-known-callee emit-native-tail-builtin emit-native-tail-call
                   native-fixed-arity native-immediate-integer-result-p native-raw-aref-index native-scalar-call-name native-raw-array-store
                   emit-native-snapshot-arithmetic emit-native-snapshot-mask emit-native-scalar-snapshot
                   native-float-call-op native-float-precision native-scalar-pair-plans emit-native-scalar-pair
                   emit-native-character-code-at emit-native-raw-result-box emit-native-raw-result-store emit-native-unused-context emit-native-restore emit-native-instruction emit-native-jump-value native-dispatch-targets native-block-label-depths emit-native-dispatch-target emit-native-jump emit-native-condition emit-native-branch emit-native-return emit-native-throw emit-native-block-return emit-native-forward
                   emit-native-structured-target emit-native-structured-jump emit-native-structured-region
                   hir-capture hir-cell-for-binding
                   hir-value-type-at hir-type-equal-p hir-integer-interval hir-integer-bounds hir-nominal-type-mask hir-disjoint-type-mask hir-type-proves-p hir-type-join hir-widen-integer-interval hir-type-restrict hir-constant-type hir-type-cell
                   hir-type-without-null hir-copy-source hir-type-truth-premise hir-type-branch-guards
                   hir-standard-property-negative-p hir-inline-primitive hir-primary-result-sources hir-primary-result-facts hir-standard-operation-leaf hir-standard-operations hir-primary-constant hir-literal-list-cells hir-literal-counter-cells hir-bounded-integer-cells hir-literal-clock-cells hir-symbolic-target-type-p hir-type-query-result-type hir-literal-byte-field-chain hir-literal-byte-field-type hir-integer-identity hir-integer-branch-type hir-integer-comparison-branch-type hir-integer-predicate-type hir-integer-comparison-type hir-inline-result-type infer-hir-types infer-hir-types-checked hir-primary-flow hir-local-callee-flow hir-result-callee hir-result-callees hir-result-arities-checked
                   hir-dispatch-candidate
                   hir-forward-blocks hir-optimization-barriers hir-private-cells hir-copy-instruction hir-forward-private-cells
                   hir-constant-fact hir-constant-join hir-update-constant hir-constant-edge hir-propagate-constants hir-simplify-cfg hir-tail-pair-p hir-share-branch-tails hir-refresh-predecessors hir-proper-list-checks optimize-hir-checked optimize-hir
                   hir-lexical-function-entry hir-inline-environment hir-function-result-type hir-inline-size hir-inline-cost hir-inline-candidate hir-inline-body hir-shadow-specials
                   compiler-source-policy native-entry-sections
                   hir-bind-value hir-read-variable hir-write-variable hir-function-reference hir-direct-function-reference
                   hir-small-membership hir-membership-branches hir-call-arguments hir-literal-apply-form hir-case-form hir-macro-environment hir-assert-form
                   hir-expand-parallel-places hir-expand-arithmetic-step hir-restart-options hir-correctable-place-form
                   hir-condition-initializer hir-define-condition hir-restart-expression hir-control-form verify-hir verify-hir-contents
                   native-inline-constants native-loop-literals native-loop-dominates-p native-hoist-source native-hoist-width native-hoist-type native-hoist-immediate-result-p native-plan-loop-hoists native-finalize-loop-hoists emit-native-invariant-value emit-native-hoisted-primitive native-tiered-unary native-tiered-source native-plan-tiered-counters native-plan-tiered-comparisons
                   native-assign-tiered-locals emit-native-tiered-copy emit-native-tiered-small emit-native-tiered-i64
                   emit-native-tiered-materialize emit-native-tiered-classify emit-native-tiered-update
                   emit-native-tiered-instruction emit-native-tiered-comparison
                   native-plan-vector-snapshots emit-native-vector-snapshot-prefix emit-native-vector-snapshot-mode
                   emit-native-vector-snapshot-comparison emit-native-vector-snapshot-operation
                   native-snapshot-masks native-snapshot-operand native-snapshot-bit-operation
                   native-plan-unboxed-arguments native-plan-unboxed-cell-values native-snapshot-source native-unboxed-join-input-p native-plan-unboxed-join native-plan-unboxed-values native-checked-integer-bound-root native-plan-unboxed-checked-copies native-function-plan emit-native-function-body emit-native-function native-literal-groups native-function-indices native-coalesce-bodies native-relocate-body native-order-bodies emit-native-bodies native-type-roots emit-native-module-with-results emit-native-module-checked emit-native-module
                   hir-body hir-close hir-local-macros-form hir-lambda
                   hir-tagbody-form hir-setq-form hir-local-functions-form
                   hir-multiple-value-bind-form hir-block-form hir-simple-restart-form
                   hir-restart-signal-form hir-symbol-iterate-form hir-while-form
                   hir-multiple-value-call-form hir-return-from-form
                   hir-body-declaration-base hir-body-macro-context hir-literal-membership hir-application-form hir-recursive-form hir-form lower-source-to-hir compile-source-hir compile-to-hir compile-module-to-hir compile-native-module
                   make-native-op make-native-flow native-read-uleb native-read-sleb native-read-ops native-read-ops-portable native-op-target native-control-matches native-op-flow native-liveness-blocks native-op-liveness native-op-stack native-scalar-call-effects native-stackify-ops native-stackify-ops-lisp native-coalesce-ops native-forward-locals native-result-blocks native-fold-locals native-optimize-body
                   ;; Compile larger size passes after the frontend and emitter.
                   native-clean-scopes native-simplify-ops native-fuse-literal-functions native-fold-cells native-prune-module native-batch-work-bodies native-outline-result native-outline-match-region native-outline-region native-outline-site native-outline-code native-outline-bodies
                   symbol-key function-key literal-initializer-p static-type-form-p structure-info class-info method-info
                   definition-info namespace reachability-case-clauses form-written-variables form-references
                   miricl::%normalize-lambda-1 miricl::%normalize-lambda
                   miricl::%compile-place-form miricl::%compile-parallel-places miricl::%compile-loop-collector
                   miricl::%body-declaration-environment miricl::%declaration-environment
                   miricl::%compile-forms miricl::%compile-body miricl::%compile-lambda-parts
                   miricl::%compile-lambda-form miricl::%compile-closure miricl::%compile-bindings
                   miricl::%compile-local-functions miricl::%expand-backquote miricl::%compile-backquote
                   miricl::%compile-option-function miricl::%compile-condition miricl::%compile-recursive-form miricl::%compile-form
                   miricl::%file-lexical-name miricl::%file-lexical-specials miricl::%file-lexical-bind
                   miricl::%file-lexical-reference miricl::%file-code-cons miricl::%file-lexical-forms
                   miricl::%file-lexical-function miricl::%file-lexical-call miricl::%file-lexical-form
                   miricl::%fasl-reference miricl::%fasl-dump
                   miricl::%fasl-initialize miricl::%fasl-object-slow miricl::%fasl-finish-initializations
                   miricl::%execute-fasl-initializer
                   ;; Cached generic dispatch still executes method plans and
                   ;; CALL-NEXT-METHOD continuations on every invocation.
                   ;; Keep those shared paths native alongside the compiler.
                   miricl::%effective-call miricl::%around-plan miricl::%call-next miricl::%make-effective-function miricl::%invoke-effective
                   ;; Allocation and protocol introspection select methods too,
                   ;; independently of the executable generic-call cache.
                   miricl::%compute-applicable miricl::%applicable miricl::%specializer-matches
                   miricl::%more-specific miricl::%insert-method
                   miricl::%lambda-profile miricl::%dispatch-key miricl::%key-equal
                   miricl::%required miricl::%instancep miricl::%subclassp
                   miricl::%keyword
                   find-class
                   ;; Externalizing large constant graphs repeatedly uses
                   ;; these shared slot and load-form operations.
                   miricl::%slot-cell miricl::%cpl make-load-form-saving-slots
                   compile-native compile-native-source record-edges pool-record-less-p layout-records compact-unit compile-linker ensure-compiler compile-compiler
                   ;; Traverse large initializer and literal graphs natively.
                   prepare-native-unit native-unit-records compile-native-bundle
                   hir-code-parameters hir-code-name hir-code-debug-info hir-code-cell-aliases hir-code-register hir-code-single-calls
                   hir-code-scope-aliases
                   emit-hir-code emit-hir-code-checked hir-unit-function compile-native-hir)))

(defvar *native-compiler-functions* (make-hash-table :test 'eq))
(defparameter *native-compiler-blocks*
  ;; Each block starts at its first seed. Only its own definitions are sealed;
  ;; calls into later bootstrap stages retain their global bindings. The
  ;; interpreter still installs and executes ordinary global definitions.
  '((wasm-byte wasm-append wasm-u32 wasm-u32-width wasm-s32 wasm-s64 wasm-name wasm-type wasm-section wasm-call)
    (hir-new-value hir-new-block hir-emit hir-terminate hir-single hir-multiple hir-nth-value hir-constant hir-clear-source)
    (hir-value-type-at hir-type-equal-p hir-integer-interval hir-integer-bounds hir-nominal-type-mask hir-disjoint-type-mask hir-type-proves-p hir-type-join hir-widen-integer-interval hir-type-restrict hir-constant-type hir-type-cell hir-literal-list-cells)
    (hir-forward-blocks hir-optimization-barriers hir-private-cells hir-copy-instruction hir-forward-private-cells)
    (miricl::%file-lexical-name miricl::%file-lexical-specials miricl::%file-lexical-bind
     miricl::%file-lexical-reference miricl::%file-code-cons miricl::%file-lexical-forms
     miricl::%file-lexical-function miricl::%file-lexical-call miricl::%file-lexical-form)))
(defvar *compiler-state* :cold)
(defvar *compiler-origin* :source)

(defun compiler-source-policy (source)
  "Compile shipped compiler kernels at SAFETY 0 and DEBUG 0, preserving user defaults."
  (declare (type list source))
  (let ((body (cddr source)) (documentation nil))
    (when (and (stringp (car body)) (cdr body)) (push (pop body) documentation))
    (list* (car source) (second source)
      (append documentation (list '(declare (optimize (safety 0) (debug 0))
        (inline svref aref row-major-aref (setf svref) (setf aref) (setf row-major-aref)
                length integer-length logand logior logxor lognot ash car first cdr rest eq eql null not cons list list*
                gethash (setf gethash) consp simple-vector-p zerop minusp plusp evenp oddp
                1+ 1- + - * < > <= >= = /=))) body))))

(defun ensure-compiler ()
  "Compile the compiler once on first use; startup only loads its packed source."
  (cond (*compiler-bootstrapping* t)
        ((eq *compiler-state* :ready) t)
        ((eq *compiler-state* :interpreted) nil)
        ((eq *compiler-state* :failed) (error "Compiler bootstrap previously failed."))
        ((eq *compiler-state* :compiling)
         (loop while (eq *compiler-state* :compiling) do (miricl-threads:thread-yield))
         (ensure-compiler))
        (t
         (if (compiler-primitive 21 nil)
             (let ((complete nil))
               (setq *compiler-state* :compiling)
               (unwind-protect
                   (multiple-value-bind (installed unsupported) (compile-compiler)
                     (declare (ignore installed))
                     (when unsupported (error "Compiler bootstrap failed: ~S" unsupported))
                     (setq complete t *compiler-state* :ready) t)
                 (unless complete (setq *compiler-state* :failed))))
             (progn (setq *compiler-state* :interpreted) nil)))))

(defun compiler-function-source (name)
  (function-lambda-expression (fdefinition name)))

(defun compile-compiler ()
  "Bootstrap supported compiler kernels; return installed names and unsupported sources.
This does not change the policy for compiling application bodies."
  ;; Bootstrap uses the ordinary optimization policy. Only recursion into
  ;; ENSURE-COMPILER is suppressed while its own kernels are being compiled.
  (let ((*compiler-bootstrapping* t) (installed nil) (unsupported nil))
    (dolist (name *native-compiler-seeds*)
      (unless (gethash name *native-compiler-functions*)
        (handler-case
            (let* ((block (assoc name *native-compiler-blocks*))
                   ;; Packed HIR kernels need no retained source. Keep their
                   ;; existing individual bootstrap path when a source block
                   ;; cannot be reconstructed in its entirety.
                   (definitions (and block
                     (loop for member in block
                       for source = (and (not (gethash member *native-compiler-functions*))
                                     (compiler-function-source member))
                       unless source do (return nil)
                       collect (list* 'defun member (cdr (compiler-source-policy source)))))))
              (if definitions
                  (let ((functions (compile-native-module definitions :inline nil)))
                    ;; Publish the complete block only after compilation and
                    ;; factory invocation succeed.
                    (dolist (entry functions)
                      (setf (symbol-function (car entry)) (cdr entry)
                            (gethash (car entry) *native-compiler-functions*) t)))
                  (let* ((original (compiler-function-source name))
                         (source (and original (compiler-source-policy original)))
                         (code (unless source (compiler-primitive 43 (symbol-function name))))
                         (function (cond (code (compile-native-hir code :name name))
                                         (source (compile-native source :name name))
                                         (t (error "Missing compiler bootstrap code for ~S." name)))))
                    (setf (symbol-function name) function (gethash name *native-compiler-functions*) t))))
          (error (condition) (push (list name (princ-to-string condition)) unsupported))))
      (when (gethash name *native-compiler-functions*) (push name installed)))
    (unless unsupported
      (unless (eq *compiler-state* :ready)
        ;; Boot-time effective methods were built before native compilation
        ;; became available. Rebuild those plans through the ready compiler.
        (miricl::%invalidate-all-dispatch))
      (setq *compiler-state* :ready)
      (unless (eq *compiler-origin* :image) (setq *compiler-origin* :first-use)))
    (values (nreverse installed) (nreverse unsupported))))

(defvar *native-linker* nil)
(defun compile-linker ()
  "Bootstrap the compiler's graph traversal kernels using its native backend."
  (ensure-compiler)
  (unless *native-linker*
    (let ((functions nil))
      (dolist (name '(record-edges compact-unit))
        (unless (gethash name *native-compiler-functions*)
          (let ((source (compiler-function-source name)))
            (unless source (error "Missing bootstrap source for ~S." name))
            (push (cons name (compile-native (compiler-source-policy source) :name name)) functions))))
      ;; Install only when all kernels compiled successfully.
      (dolist (entry functions) (setf (symbol-function (car entry)) (cdr entry)))
      (setq *native-linker* t)))
  t)
