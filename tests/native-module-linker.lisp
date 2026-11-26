;;;; Exercise the production Common Lisp linker without a compiler bootstrap.
;; A public HIR entry need not begin with its arity check. A long valid entry
;; must consume the scope proof budget before callers can repeatedly rescan it.
(let* ((unit (miricl-compiler:compile-to-hir '(lambda () nil)))
       (function (first (miricl-compiler::hir-unit-functions unit)))
       (entry (miricl-compiler::hir-function-entry function))
       (instructions (miricl-compiler::hir-block-instructions entry))
       (check (find :check-arguments instructions
                    :key (lambda (instruction) (miricl-compiler::hir-instruction-opcode instruction)))))
  (assert check)
  (setf (miricl-compiler::hir-block-instructions entry)
    (append (remove check instructions)
      (loop repeat 70000 collect
        (miricl-compiler::make-hir-instruction-record :constant nil nil '(nil) nil :pure 1))
      (list check)))
  (miricl-compiler:verify-hir unit)
  (let ((miricl-compiler::*wasm-debug* nil)
        (miricl-compiler::*wasm-fixed-call-emitter* nil)
        (miricl-compiler::*wasm-closed-worker-functions* nil)
        (miricl-compiler::*wasm-direct-workers*
          (miricl-compiler::native-direct-workers (miricl-compiler::hir-unit-functions unit)))
        (miricl-compiler::*wasm-multiple-result-functions* (make-hash-table :test 'eq))
        (miricl-compiler::*wasm-optimization-barriers* (make-hash-table :test 'eq)))
    (assert (null (miricl-compiler::native-scoped-call-functions unit)))))

(let* ((root (asdf:system-source-directory :miricl))
       (directory (merge-pathnames
                   (format nil "miricl-native-linker-~D-~D/" (get-universal-time)
                           (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (input (merge-pathnames "input.wasm" directory))
       (wasm (merge-pathnames "linked.wasm" directory))
       (script (merge-pathnames "tests/native-module-linker.mjs" root)))
  (labels ((bytes (file)
             (with-open-file (in file :element-type '(unsigned-byte 8))
               (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
                 (read-sequence bytes in) bytes)))
           (write-wat (file wat)
             (with-open-file (out file :direction :output :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
               (write-sequence (wasmtime:wat-to-wasm wat) out)))
           (rejected (code reason)
             (handler-case
                 (progn (miricl::link-native-modules (list code))
                        (error "Native linker accepted malformed fixture ~A." reason))
               (miricl-compiler::compiler-input-error (condition)
                 (assert (search reason (miricl-compiler::compiler-error-reason condition)
                                 :test #'char-equal))))))
    (ensure-directories-exist input)
    (uiop:run-program (list "node" (namestring script) "prepare" (namestring input))
                      :output *standard-output* :error-output *error-output*)
    (loop for suffix in '("foreign" "payload" "catch")
          for reason in '("exception tag" "no payload" "invalid compiler module") do
      (rejected (bytes (pathname (concatenate 'string (namestring input) "." suffix))) reason))
    (let ((code (bytes input)))
      (rejected (subseq code 0 (1- (length code))) "invalid compiler module")
      (rejected #(0 97 115 109 1 0 0) "bounds")
      (rejected '(0 97 115 109 1 0 0 0 256) "byte elements")
      (let ((cycle (list 0)))
        (setf (cdr cycle) cycle)
        (rejected cycle "circular"))
      ;; All malformed requests above leave the linker usable for the original.
      (multiple-value-bind (wat entries declarations) (miricl::link-native-modules (list code))
        (write-wat wasm
          (format nil "(module
            (type $native-entry (func (param externref externref) (result externref)))
            (type $native-direct-entry (func (param externref externref externref externref externref) (result externref)))
            (import \"m\" \"before\" (func $native-before (param externref) (result externref)))
            (import \"m\" \"transfer\" (tag $transfer))
            (import \"m\" \"after\" (func $native-after (param externref) (result externref)))
            ~A (export \"entry\" (func ~A)) ~{~A~})"
            wat (first entries)
            (loop for name in (cddr declarations) for i from 0 collect
              (format nil "(export ~S (func ~A))" (format nil "numeric-~D" i) name)))))
      (uiop:run-program (list "node" (namestring script) "verify" (namestring wasm))
                        :output *standard-output* :error-output *error-output*))
    (let* ((source (merge-pathnames "counters.wat" directory))
           (linked (merge-pathnames "counters-linked.wasm" directory)))
      (uiop:run-program (list "node" (namestring script) "prepare-counters" (namestring source))
                        :output *standard-output* :error-output *error-output*)
      (multiple-value-bind (wat entries)
          (miricl::link-native-modules (list (wasmtime:wat-to-wasm (uiop:read-file-string source))))
        (write-wat linked
          (format nil "(module
            (import \"m\" \"wide\" (func $native-wide (param i64) (result externref)))
            (import \"m\" \"step\" (func $native-step (param externref i32) (result externref)))
            ~A (export \"entry\" (func ~A)))" wat (first entries))))
      (uiop:run-program (list "node" (namestring script) "verify-counters" (namestring linked))
                        :output *standard-output* :error-output *error-output*)))
  (let ((source (merge-pathnames "optimizer.wat" directory))
        (original (merge-pathnames "original.wasm" directory))
        (optimized (merge-pathnames "optimized.wasm" directory)))
    (uiop:run-program (list "node" (namestring script)
                            "prepare-optimizer" (namestring source))
                      :output *standard-output* :error-output *error-output*)
    (let* ((bytes (wasmtime:wat-to-wasm (uiop:read-file-string source)))
           (result (miricl-compiler::optimize-wasm-module bytes)))
      (with-open-file (output original :direction :output :element-type '(unsigned-byte 8))
        (write-sequence bytes output))
      (with-open-file (output optimized :direction :output :element-type '(unsigned-byte 8))
        (write-sequence result output)))
    (uiop:run-program (list "node" (namestring script)
                            "verify-optimizer" (namestring original) (namestring optimized))
                      :output *standard-output* :error-output *error-output*))
  (dolist (fixture '("outlining" "outlining-loops"))
    (dolist (operands '(2 18 22))
      (let ((source (merge-pathnames (format nil "~A-~D.wat" fixture operands) directory))
            (original (merge-pathnames (format nil "~A-~D-original.wasm" fixture operands) directory))
            (plain (merge-pathnames (format nil "~A-~D-plain.wasm" fixture operands) directory))
            (shared (merge-pathnames (format nil "~A-~D-shared.wasm" fixture operands) directory)))
        (uiop:run-program (list "node" (namestring script)
                                (format nil "prepare-~A" fixture) (namestring source) (princ-to-string operands))
                          :output *standard-output* :error-output *error-output*)
        (let* ((bytes (wasmtime:wat-to-wasm (uiop:read-file-string source)))
               (simplified (let ((miricl-compiler::*native-outlining-budget* 0))
                             (miricl-compiler::optimize-wasm-module bytes)))
               (outlined (miricl-compiler::outline-wasm-module simplified)))
          (loop for pathname in (list original plain shared)
                for contents in (list bytes simplified outlined) do
            (with-open-file (output pathname :direction :output
                                            :element-type '(unsigned-byte 8))
              (write-sequence contents output))))
        (uiop:run-program (list "node" (namestring script)
                                (format nil "verify-~A" fixture) (namestring original) (namestring plain)
                                (namestring shared) (princ-to-string operands))
                          :output *standard-output* :error-output *error-output*))))
  (let ((source (merge-pathnames "call-pairs.wat" directory))
        (plain (merge-pathnames "call-pairs-plain.wasm" directory))
        (shared (merge-pathnames "call-pairs-shared.wasm" directory)))
    (uiop:run-program (list "node" (namestring script) "prepare-call-pairs" (namestring source))
                      :output *standard-output* :error-output *error-output*)
    (let ((wat (uiop:read-file-string source))
          (miricl-compiler::*native-outlining-minimum-saving* 128))
      (loop for pathname in (list plain shared) for pairs in '(nil t) do
        (with-open-file (output pathname :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (miricl::optimize-runtime-wat wat :runtime-call-pairs pairs) output))))
    (uiop:run-program (list "node" (namestring script)
                            "verify-call-pairs" (namestring plain) (namestring shared))
                      :output *standard-output* :error-output *error-output*))
  (dolist (fixture '("linked-control" "rejected-control"))
    (let ((source (merge-pathnames (format nil "~A.wat" fixture) directory))
          (original (merge-pathnames (format nil "~A-original.wasm" fixture) directory))
          (optimized (merge-pathnames (format nil "~A-optimized.wasm" fixture) directory)))
      (uiop:run-program (list "node" (namestring script) (format nil "prepare-~A" fixture)
                              (namestring source))
                        :output *standard-output* :error-output *error-output*)
      (let* ((bytes (miricl::wasm-without-debug-names (wasmtime:wat-to-wasm (uiop:read-file-string source))))
             (result (wasmtime:with-engine (engine)
                       (miricl::native-link-validate engine bytes)
                       (miricl::compact-runtime-code engine bytes nil))))
        (loop for file in (list original optimized) for contents in (list bytes result) do
          (with-open-file (output file :direction :output :element-type '(unsigned-byte 8))
            (write-sequence contents output))))
      (uiop:run-program (list "node" (namestring script) (format nil "verify-~A" fixture)
                              (namestring original) (namestring optimized))
                        :output *standard-output* :error-output *error-output*)))
  (let* ((original (merge-pathnames "helper-layout-original.wasm" directory))
         (compact (merge-pathnames "helper-layout-compact.wasm" directory))
         (first-helper 128)
         (padding (loop for i below (- first-helper 5) collect (format nil "$pad~D" i)))
         (wat (with-output-to-string (out)
                (format out "(module
                  (table 0 funcref)
                  (table $table (export \"table\") ~D funcref)
                  (elem (table $table) (i32.const 0) func $table-entry ~{~A~^ ~})
                  (global (export \"held\") funcref (ref.func $held))
                  (global (export \"minimum\") i64 (i64.const -9223372036854775808))
                  (func $table-entry (result i32) i32.const 77)
                  (func $held (result i32) i32.const 88)
                  (func $cold (result i32) i32.const 17)
                  (func (export \"call\") (param i32) (result i32)"
                  (1+ (length padding)) padding)
                (dotimes (i 8) (write-string " local.get 0 call $helper drop" out))
                (write-string " local.get 0 call $helper call $cold i32.add)
                   (func (export \"tail\") (param i32) (result i32) local.get 0 return_call $helper)" out)
                (dolist (name padding) (format out " (func ~A)" name))
                (write-string "
                  (func $helper (param i32) (result i32) local.get 0 i32.const 3 i32.add)
                  (func $keep (param externref) (result externref) local.get 0)
                  (func (export \"nullGet\") (param $ref externref) (result i32 externref)
                    (local $kept externref)
                    block $exit (result i32)
                      block
                        i32.const 37
                        local.get $ref
                        ref.is_null
                        br_if $exit
                        local.get $ref
                        call $keep
                        local.set $kept
                        drop
                      end
                      i32.const 41
                    end
                    local.get $kept)
                  (func (export \"nullTee\") (param $ref externref) (param $old externref)
                    (result i32 externref externref)
                    (local $kept externref)
                    block $exit (result i32)
                      block
                        i32.const 43
                        local.get $ref
                        local.tee $old
                        ref.is_null
                        br_if $exit
                        local.get $old
                        call $keep
                        local.set $kept
                        drop
                      end
                      i32.const 47
                    end
                    call $helper
                    local.get $kept
                    local.get $old)
                  (func (export \"nullDifferent\") (param $tested externref) (param $selected externref)
                    (result i32 externref)
                    (local $kept externref)
                    block $exit (result i32)
                      i32.const 51
                      local.get $tested
                      ref.is_null
                      br_if $exit
                      local.get $selected
                      call $keep
                      local.set $kept
                      drop
                      i32.const 53
                    end
                    local.get $kept))" out)))
         (bytes (miricl::wasm-without-debug-names (wasmtime:wat-to-wasm wat))))
    (wasmtime:with-engine (engine)
      (miricl::native-link-validate engine bytes)
      (let* ((result (miricl::compact-runtime-code engine bytes first-helper))
             (branches (miricl-compiler::compact-linked-wasm-code bytes nil))
             (custom (coerce (miricl::wasm-with-custom-section bytes "unknown.fixture" #(0))
                             '(simple-array (unsigned-byte 8) (*)))))
        (miricl::native-link-validate engine result)
        (miricl::native-link-validate engine branches)
        (assert (< (length result) (length bytes)))
        (assert (< (length branches) (length bytes)))
        (miricl::native-link-validate engine custom)
        (assert (equalp custom (miricl::compact-runtime-code engine custom first-helper)))
        (loop for file in (list original compact) for contents in (list bytes result) do
          (with-open-file (output file :direction :output :element-type '(unsigned-byte 8))
            (write-sequence contents output)))))
    (uiop:run-program (list "node" (namestring script)
                            "verify-helper-layout" (namestring original) (namestring compact))
                      :output *standard-output* :error-output *error-output*)))
