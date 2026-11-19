;;;; Regenerate pages from explicit matching distribution files, without building
;;;; or loading the MIRICL host compiler. Used on both CI image cache routes.
(require :asdf)
(load (merge-pathnames "html-builder.lisp" *load-truename*))
(let ((output (uiop:ensure-directory-pathname
                (or (first (uiop:command-line-arguments)) (uiop:getcwd)))))
  (build-browser-page "scripts/repl.html" "miricl.html" output
                      :image (merge-pathnames "miricl.wasm" output))
  (build-browser-page "scripts/todo.html" "todo.html" output :compiler nil
                      :image (merge-pathnames "miricl-interpreter.wasm" output)))
