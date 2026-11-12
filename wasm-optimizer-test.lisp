(in-package :miricl-test)
(in-suite :miricl)

(test compiler-record-inline-metadata-on-load
  ;; Loading compiled definitions does not repeat macroexpansion side effects.
  ;; Clear those effects before evaluating the already expanded definition.
  (let ((definition (macroexpand-1
                     '(miricl-compiler::define-hir-record inline-host-record-fixture value)))
        (macros '(make-inline-host-record-fixture-record
                  inline-host-record-fixture-value %inline-host-record-fixture-p)))
    (dolist (name macros) (remprop name 'm::runtime-inline-macro))
    (eval definition)
    (dolist (name macros) (is (get name 'm::runtime-inline-macro)))))

(defun check-module-optimization (wat expected &optional arguments)
  (let* ((original (wt:wat-to-wasm wat))
         (optimized (miricl-compiler::optimize-wasm-module original)))
    (is (<= (length optimized) (length original)))
    (wt:with-engine (engine)
      (wt:with-store ((store context) engine)
        (dolist (bytes (list original optimized))
          (let ((module (wt:compile-module engine bytes)))
            (unwind-protect
                (wt:with-instance (instance context module)
                  (is (equalp expected (multiple-value-list
                    (apply #'wt:instance-call context instance "answer" arguments)))))
              (wt::wasmtime-module-delete module))))))
    optimized))

(test module-optimizer-mixed-types-and-calls
  (dolist (value '(-9223372036854775808 9223372036854775807))
    (check-module-optimization
      (format nil "(module (func (export ~S) (result i64) i64.const ~D))" "answer" value)
      (list value)))
  (check-module-optimization
    "(module (memory 1)
       (func $pair (param i32) (result i32 i64) local.get 0 i64.const 40)
       (func (export \"answer\") (param $n i32) (result i64)
         (local $a i64) (local $b i32) (local $c f64) (local $d i32)
         local.get $n call $pair local.set $a local.set $b
         f64.const 2 local.set $c
         i32.const 0 local.get $b i32.store
         i32.const 0 i32.load local.set $d
         local.get $a local.get $d i64.extend_i32_s i64.add
         local.get $c i64.trunc_f64_s i64.add))" '(49) '(7))
  (check-module-optimization
    "(module (type $sig (func (param i32) (result i32)))
       (func $inc (type $sig) local.get 0 i32.const 1 i32.add)
       (table 1 funcref) (elem (i32.const 0) func $inc)
       (func (export \"answer\") (result i32) (local $n i32)
         i32.const 40 i32.const 0 call_indirect (type $sig) local.set $n
         local.get $n ref.func $inc call_ref $sig))" '(42)))

(test module-optimizer-integral-f64-constants
  (dolist (value '(-2147483648 -65536 -64 -1 0 1 63 64 65536 2147483647))
    (let* ((wat (format nil "(module (func (export ~S) (result i32)
                   f64.const ~D i32.const ~D f64.convert_i32_s f64.eq))" "answer" value value))
           (original (wt:wat-to-wasm wat))
           (optimized (check-module-optimization wat '(1))))
      (is (< (length optimized) (length original)))))
  ;; Reinterpretation observes every bit, including signed zero, subnormals,
  ;; infinities and NaN payloads. None can be rounded or canonicalized here.
  (dolist (entry '(("-0" #x8000000000000000)
                   ("0.5" #x3fe0000000000000) ("-0.5" #xbfe0000000000000)
                   ("inf" #x7ff0000000000000) ("-inf" #xfff0000000000000)
                   ("nan:0x1234" #x7ff0000000001234) ("-nan:0x1234" #xfff0000000001234)
                   ("0x0.0000000000001p-1022" 1)
                   ("2147483648" #x41e0000000000000)))
    (let ((bits (second entry)))
      (when (logbitp 63 bits) (decf bits (ash 1 64)))
      (check-module-optimization
        (format nil "(module (func (export ~S) (result i64) f64.const ~A i64.reinterpret_f64))"
          "answer" (first entry)) (list bits))))
  ;; Constant-expression rules differ from instructions inside functions.
  (check-module-optimization
    "(module (global $zero f64 (f64.const 0))
       (func (export \"answer\") (result i32) global.get $zero f64.const 0 f64.eq))" '(1)))

(test module-optimizer-gc-definite-initialization
  ;; Inline reference block types contain a second LEB. Heap type 32 would be
  ;; mistaken for LOCAL.GET if the decoder consumed only the first byte.
  (check-module-optimization
    (format nil "(module ~{~A~} (type $box (struct (field i32)))
       (func (export ~S) (result i32) (local $saved (ref $box))
         block (result (ref $box)) i32.const 42 struct.new $box end
         local.set $saved local.get $saved struct.get $box 0))"
      (loop repeat 32 collect "(type (struct))") "answer") '(42))
  ;; Runtime liveness does not replace Wasm's structural initialization rule.
  (check-module-optimization
    "(module (type $box (struct (field i32)))
       (func (export \"answer\") (param $flag i32) (result i32)
         (local $saved (ref $box)) (local $other (ref $box))
         i32.const 1 struct.new $box local.set $saved
         local.get $flag if
           i32.const 42 struct.new $box local.set $saved
         else i32.const 41 struct.new $box local.set $saved end
         i32.const 3 struct.new $box local.set $other
         local.get $saved struct.get $box 0))" '(42) '(1))
  (check-module-optimization
    "(module (type $a (array (mut i32)))
       (func (export \"answer\") (result i32)
         (local $a (ref null $a)) (local $n i32)
         i32.const 20 i32.const 22 array.new_fixed $a 2 local.set $a
         local.get $a i32.const 0 array.get $a local.set $n
         local.get $n local.get $a i32.const 1 array.get $a i32.add))" '(42)))

(test module-optimizer-loops-defaults-and-handlers
  (check-module-optimization
    "(module (func (export \"answer\") (param $n i32) (result i32)
       (local $sum i32) (local $old i32)
       block $done loop $again
         local.get $n i32.eqz br_if $done
         local.get $sum local.set $old
         local.get $sum local.get $n i32.add local.set $sum
         local.get $n i32.const 1 i32.sub local.set $n br $again
       end end local.get $sum))" '(55) '(10))
  (dolist (raising '("call $raise" "throw $e"))
    (check-module-optimization
      (format nil "(module (tag $e) (func $raise throw $e)
         (func (export ~S) (result i32) (local $saved i32)
           i32.const 42 local.set $saved
           block $caught
             try_table (catch_all $caught) ~A end
             i32.const 99 return
           end local.get $saved))" "answer" raising) '(42))))

(test module-optimizer-unsupported-body-and-names
  (let* ((original (wt:wat-to-wasm
           "(module $named
              (func $simd (result i32) v128.const i32x4 7 8 9 10 i32x4.extract_lane 0)
              (func $answer (export \"answer\") (result i32)
                (local $unused i32) (local $a i32) (local $b i32)
                call $simd local.set $a local.get $a i32.const 35 i32.add
                local.set $b local.get $b))"))
         (metadata #(0 7 3 102 111 111 1 2 3))
         (bytes (concatenate '(simple-array (unsigned-byte 8) (*))
                  (subseq original 0 8) metadata (subseq original 8))))
    (multiple-value-bind (optimized changed skipped) (miricl-compiler::optimize-wasm-module bytes)
      (is (= changed 1)) (is (= skipped 1))
      (is (< (length optimized) (length bytes)))
      (is (equalp metadata (subseq optimized 8 (+ 8 (length metadata)))))
      (wt:with-engine (engine)
        (wt:with-store ((store context) engine)
          (let ((module (wt:compile-module engine optimized)))
            (unwind-protect
                (wt:with-instance (instance context module)
                  (is (= 42 (wt:instance-call context instance "answer"))))
              (wt::wasmtime-module-delete module)))))
      (let ((input (miricl-compiler::wasm-input optimized 8)) (names nil))
        (loop while (< (miricl-compiler::wasm-input-position input) (length optimized)) do
          (let* ((id (miricl-compiler::module-byte input))
                 (payload (miricl-compiler::module-span input (miricl-compiler::module-uleb input))))
            (when (and (zerop id) (equalp (subseq payload 0 (min 5 (length payload))) #(4 110 97 109 101)))
              (setq names t))))
        (is (eq names t))))))

(test module-optimizer-integer-precompute-and-vacuum
  (check-module-optimization
    "(module (func (export \"answer\") (result i32)
       i32.const -2147483648 i32.const 1 i32.sub
       i32.const 2147483647 i32.eq
       i32.const -1 i32.const 33 i32.shr_u i32.const 2147483647 i32.eq
       i32.and i32.const 41 i32.add))" '(42))
  (check-module-optimization
    "(module (func (export \"answer\") (param i32) (result i32)
       local.get 0 i32.eqz i32.eqz drop
       i32.const 123 drop
       i32.const 42 i32.const 0 i32.add))" '(42) '(17))
  (dolist (expression '("i32.const 1 i32.const 0 i32.div_s drop"
                        "ref.null any ref.as_non_null drop"
                        "i32.const 65536 i32.load drop"))
    (let* ((bytes (wt:wat-to-wasm (format nil
             "(module (memory 1) (func (export ~S) ~A))" "answer" expression)))
           (optimized (miricl-compiler::optimize-wasm-module bytes)))
      (wt:with-engine (engine)
        (wt:with-store ((store context) engine)
          (dolist (image (list bytes optimized))
            (let ((module (wt:compile-module engine image)))
              (unwind-protect
                  (wt:with-instance (instance context module)
                    (signals wt:wasm-trap (wt:instance-call context instance "answer")))
                (wt::wasmtime-module-delete module)))))))))

(test module-optimizer-constant-control-and-depths
  (check-module-optimization
    "(module (func (export \"answer\") (result i32)
       i32.const 0 if (result i32) unreachable else i32.const 42 end))" '(42))
  (check-module-optimization
    "(module (func (export \"answer\") (result i32)
       ref.null extern ref.is_null
       if (result i32) i32.const 42 else unreachable end))" '(42))
  (check-module-optimization
    "(module (func (export \"answer\") (result i32)
       block $done (result i32)
         block loop i32.const 42 br $done
           i32.const 1 i32.const 0 i32.div_s drop
         end end unreachable
       end))" '(42))
  (dolist (n '(0 1 -1))
    (check-module-optimization
      "(module (func (export \"answer\") (param i32) (result i32)
         block $outer (result i32) block $inner (result i32)
           block i32.const 42 local.get 0 br_table $inner $outer end unreachable
         end end))" '(42) (list n)))
  (dolist (n '(0 1))
    (check-module-optimization
      (format nil "(module (func (export ~S) (result i32)
         block $done (result i32) i32.const 42 i32.const ~D br_if $done
           drop i32.const 42 end))" "answer" n) '(42))))

(test module-optimizer-dead-parameter-reuse
  (check-module-optimization
    "(module (func (export \"answer\") (param $n i32) (result i32) (local $sum i32)
       local.get $n i32.const 1 i32.add local.set $sum
       local.get $sum local.get $sum i32.add))" '(12) '(5))
  ;; A local read before any write must retain its implicit zero, rather than
  ;; inheriting the arbitrary value in an otherwise unused parameter slot.
  (check-module-optimization
    "(module (func (export \"answer\") (param $n i32) (result i32) (local $zero i32)
       local.get $n drop local.get $zero))" '(0) '(97)))

(test module-optimizer-deep-branch-depths
  ;; Both an explicit result block and the implicit function label remain
  ;; valid BR_TABLE targets after hundreds of unused inner blocks disappear.
  (let* ((depth 384)
         (wat (format nil "(module (func (export ~S) (param i32) (result i32)
                  block $done (result i32) ~{~A~}
                    i32.const 42 local.get 0 br_table $done ~D
                  ~{~A~} unreachable end))"
                "answer" (make-list depth :initial-element "block ") (1+ depth)
                (make-list depth :initial-element "end "))))
    (dolist (index '(0 1 127))
      (check-module-optimization wat '(42) (list index)))))
