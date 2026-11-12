;;;; Minimal compilation uses the shared HIR frontend and portable FASLs,
;;;; without the optimizing compiler or Wasm generation.
(defvar miricl-compiler:*compile-evaluations* nil)

(defun miricl-compiler::compile-minimal-function (function name)
  (when (compiled-function-p function) (return-from miricl-compiler::compile-minimal-function function))
  (multiple-value-bind (source closure source-name) (function-lambda-expression function)
    ;; Existing interpreter closures may own writable lexical cells or local
    ;; macros. Preserve that environment with the compact source compiler.
    ;; Closures created by HIR compilation itself use HIR capture instructions.
    (if (and source (not closure))
        (let* ((unit (miricl-compiler:compile-to-hir source :name (or name source-name)))
               (debug-source (some (lambda (f) (= (miricl-compiler::hir-function-debug-level f) 3))
                              (miricl-compiler:hir-unit-functions unit))))
          (if debug-source (miricl-compiler::compiler-primitive 0 function)
              (miricl-compiler:load-hir-code (miricl-compiler:emit-hir-code unit))))
        (miricl-compiler::compiler-primitive 0 function))))
