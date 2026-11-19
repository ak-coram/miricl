;;; Separate primary system: the retained core and frontend remain independent.
(asdf:defsystem "lem-browser-lisp-mode"
  :description "LEM's Lisp editing and listener UI with a local MIRICL provider"
  :depends-on ("lem-browser-frontend" "lem-browser/lisp-syntax" "micros" "miricl-ide-services")
  :serial t
  :components
  ((:module "foundation" :pathname "../lem/extensions/lisp-mode/"
    :serial t :components ((:file "errors") (:file "ui-mode") (:file "connections")))
   (:file "connection")
   (:file "implementation")
   (:module "ui" :pathname "../lem/extensions/lisp-mode/"
    :serial t :components ((:file "message-dispatcher") (:file "grammar")
                          (:file "internal-package") (:file "message")
                          (:file "file-conversion") (:file "ext/detective")
                          (:file "ext/completion") (:file "lisp-mode")
                          (:file "message-definitions") (:file "repl")
                          (:file "ext/inspector") (:file "ext/eval")
                          (:file "ext/sldb") (:file "ext/paren-coloring")
                          (:file "package")))
   (:file "local-ui")))
