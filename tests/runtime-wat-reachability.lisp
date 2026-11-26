;;;; Host composition contracts beyond ANSI: retain every observable WAT root.
;;;; Loaded after MIRICL by scripts/check-host-compilation.lisp.
(labels ((check (text kept removed)
           (multiple-value-bind (output names supported) (miricl::runtime-prune-wat text)
             (assert supported)
             (dolist (name kept)
               (assert (search (format nil "(func ~A" name) output)))
             (dolist (name removed)
               (assert (member name names :test #'string=)))
             ;; Comments and data may still spell a removed function's name.
             (when removed (assert (< (length output) (length text))))
             ;; Name resolution must still find all references after pruning.
             (wasmtime:wat-to-wasm text)
             (wasmtime:wat-to-wasm output))))
  (check "(module (func $dead) (func $entry (export \"entry\") call $callee) (func $callee))"
         '("$entry" "$callee") '("$dead"))
  (check "(module (func $a call $b) (func $b call $a))" nil '("$a" "$b"))
  (check "(module (func $init) (func $dead) (start $init))" '("$init") '("$dead"))
  (check "(module (func $entry) (func $dead) (export \"entry\" (func $entry)))"
         '("$entry") '("$dead"))
  (dolist (root '("(elem (i32.const 0) func $entry)" "(elem func $entry)"
                  "(elem declare func $entry)" "(elem declare funcref (ref.func $entry))"
                  "(global $g (ref func) (ref.func $entry))"))
    (check (format nil "(module (table 1 funcref) (func $entry) (func $dead) ~A)" root)
           '("$entry") '("$dead")))
  (check "(module (table $table funcref (elem $entry)) (func $entry) (func $dead))"
         '("$entry") '("$dead"))
  (check "(module (func $import (import \"env\" \"f\")) (func $dead))"
         '("$import") '("$dead"))
  (check "(module (import \"env\" \"f\" (func $import)) (func $dead))"
         '("$import") '("$dead"))
  (check "(module (func (export \"entry\") call $target) (func $target) (func $dead))"
         '("$target") '("$dead"))
  ;; Identifier namespaces can collide; retaining extra functions is safe.
  (check "(module (func $entry (export \"entry\") (local $unused i32)) (func $unused))"
         '("$entry" "$unused") nil)
  (check "(module ;; (export \"fake\" (func $dead))
    (; ignored (; nested (func $bad) ;) $dead ;)
    (data \"(func $dead) call $dead \\\" export\") (func $dead) (func $entry (export \"entry\")))"
         '("$entry") '("$dead")))

;; Numeric references and uncertain syntax preserve the original text, so the
;; assembler retains validation and no function-index relocation is guessed.
(dolist (text '("(module (func $entry (export \"entry\") call 0))"
                "(module (func $entry) (export \"entry\" (func 0)))"
                "(module (table 1 funcref) (func $entry) (elem (i32.const 0) func 0))"
                "(module (func $x) (func $x))" "(module (data \"unfinished))"
                "(module (; unfinished)" "(module (func $x)" "(module))"
                "(module) trailing"))
  (multiple-value-bind (output removed supported) (miricl::runtime-prune-wat text)
    (assert (eq output text)) (assert (not removed)) (assert (not supported))))

;; A live caller can use a numeric type index even when all function
;; references are symbolic. Keep the pruned source order in that case.
(let* ((template (with-output-to-string (out)
              (write-line "(module (type $signature (func (result i32)))" out)
              (write-line "(table 1 funcref)" out)
              ;; Fill the one-byte index range with otherwise infrequent roots.
              (loop for i below (ash 1 7)
                    do (format out "(func $f~D (result i32) i32.const ~D)~%" i i))
              (write-string "(elem declare func" out)
              (loop for i below (ash 1 7) do (format out " $f~D" i))
              (write-line ")" out)
              (write-line "(func $target (result i32) i32.const 42)" out)
              (write-line "(elem (i32.const 0) func $target)" out)
              (write-line "(func $runner (export \"run\") (result i32)
                             call $target drop i32.const 0 call_indirect (type ~A)))" out)))
       (numeric (format nil template 0))
       (symbolic (format nil template "$signature"))
       (candidate (miricl::runtime-prune-wat symbolic :layout-functions t)))
  (assert (string= (miricl::runtime-prune-wat numeric)
                   (miricl::runtime-prune-wat numeric :layout-functions t)))
  ;; The layout is active on the equivalent symbolic version of this module.
  (assert (string/= candidate (miricl::runtime-prune-wat symbolic)))
  (wasmtime:wat-to-wasm numeric)
  (wasmtime:wat-to-wasm candidate))
(format t "~&Runtime WAT reachability contracts passed.~%")
