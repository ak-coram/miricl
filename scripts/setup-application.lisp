;;;; Link an application's Quicklisp dependencies without copying their sources.
(require :asdf)
(let* ((root (uiop:pathname-parent-directory-pathname
               (uiop:pathname-directory-pathname *load-truename*)))
       (arguments (uiop:command-line-arguments))
       (directory (uiop:ensure-directory-pathname (or (first arguments) (error "Expected application directory."))))
       (system (or (second arguments) (error "Expected ASDF system name.")))
       (output (uiop:ensure-directory-pathname (or (third arguments) root)))
       (node (or (fourth arguments) (uiop:getenv "NODE") "node")))
  (asdf:load-asd (merge-pathnames (concatenate 'string system ".asd") directory))
  ;; Load dependencies on the host, never the browser application's code.
  (dolist (dependency (asdf:system-depends-on (asdf:find-system system)))
    (ql:quickload (if (consp dependency) (second dependency) dependency)))
  (let* ((app (asdf:find-system system))
         (systems (remove app (remove-duplicates
                    (asdf:required-components app :other-systems t
                      :keep-component 'asdf:system :keep-operation 'asdf:load-op))))
         (sources (loop for dependency in systems
                    unless (member (asdf:component-name dependency) '("asdf" "uiop") :test #'equal)
                    append (list (asdf:component-name dependency)
                                 (namestring (asdf:system-source-directory dependency))))))
    (uiop:run-program
      (append (list node (namestring (merge-pathnames "scripts/setup-application.mjs" root))
                    (namestring (truename directory)) system (namestring root)
                    (namestring (truename output))) sources)
      :output *standard-output* :error-output *error-output*)))
