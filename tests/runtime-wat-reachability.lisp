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
(format t "~&Runtime WAT reachability contracts passed.~%")
