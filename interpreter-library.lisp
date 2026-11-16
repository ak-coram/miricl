;;;; Minimal compilation uses the shared HIR frontend and portable FASLs,
;;;; without requiring the optimizing compiler or Wasm generation.
(defvar miricl-compiler:*compile-evaluations* nil)

(defun miricl-compiler::compile-minimal-function (function name)
  (when (compiled-function-p function)
    (return-from miricl-compiler::compile-minimal-function function))
  (multiple-value-bind (source closure source-name) (function-lambda-expression function)
    ;; Existing source closures can own writable lexical cells and local macros.
    ;; Preserve their environments; newly created HIR closures capture HIR cells.
    (let ((miricl-compiler::*compiling-evaluation* t)
          (miricl-compiler::*hir-debug-info-enabled* t)
          (miricl-compiler::*hir-debug-write-types* t))
      (if (and source (not closure))
          (let ((name (or name source-name)))
            (miricl-compiler::compiler-source-form source)
            (miricl-compiler::compiler-function-name name "function name")
            (let ((unit (miricl-compiler::lower-source-to-hir source name)))
              (miricl-compiler:load-hir-code (miricl-compiler:emit-hir-code unit))))
          (miricl-compiler::compiler-primitive 0 function)))))
