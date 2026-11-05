;;;; sbcl --load ~/quicklisp/setup.lisp --script scripts/build-html.lisp [output-directory]
(require :asdf)
(load (merge-pathnames "html-builder.lisp" *load-truename*))
(build-browser-page "scripts/repl.html" "miricl.html"
  (or (first (uiop:command-line-arguments)) (uiop:getcwd)))
