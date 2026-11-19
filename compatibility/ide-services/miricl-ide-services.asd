(asdf:defsystem "miricl-ide-services"
  :description "Local asynchronous Lisp evaluation services"
  :depends-on ("miricl-gray")
  :serial t :components ((:file "services")))
