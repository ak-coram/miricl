;;;; Compile and load the bootstrap system; do not build guest images or run regressions.
(require :asdf)
(handler-case
    (let* ((root (uiop:pathname-parent-directory-pathname
                  (uiop:pathname-directory-pathname *load-truename*)))
           (output (merge-pathnames
                    (format nil "miricl-host-compilation-~D-~D/"
                            (get-universal-time) (random most-positive-fixnum))
                    (uiop:temporary-directory))))
      (asdf:load-asd (merge-pathnames "miricl.asd" root))
      ;; Compile the project's sources afresh while reusing dependency caches.
      (asdf:initialize-output-translations
       `(:output-translations (,(uiop:wilden root) ,(uiop:wilden output))
         :inherit-configuration))
      (format t "~&Compiling MIRICL with ~A ~A; project output: ~A~%"
              (lisp-implementation-type) (lisp-implementation-version) output)
      (if (find-package :ql)
          (uiop:symbol-call :ql :quickload :miricl :verbose t)
          (asdf:load-system :miricl))
      (format t "~&MIRICL host compilation and loading passed on ~A ~A.~%"
              (lisp-implementation-type) (lisp-implementation-version))
      (uiop:quit 0))
  (error (condition)
    (format *error-output* "~&MIRICL host compilation failed: ~A~%" condition)
    (uiop:quit 1)))
