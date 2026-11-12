;;;; sbcl --load ~/quicklisp/setup.lisp --script scripts/build-todo.lisp [output-directory]
(require :asdf)
(load (merge-pathnames "html-builder.lisp" *load-truename*))
(build-browser-page "scripts/todo.html" "todo.html"
  (or (first (uiop:command-line-arguments)) (uiop:getcwd)) :compiler nil)
