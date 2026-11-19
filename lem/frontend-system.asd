;;; Independent primary system: loading it does not redefine the retained core.
(in-package #:asdf-user)
(defsystem "lem-browser-frontend"
  :description "Local Common Lisp Canvas/DOM frontend for LEM"
  :version "2.3.0"
  :depends-on ("lem-browser/core")
  :serial t
  :components ((:file "backend/package")
               (:file "backend/display")
               (:file "backend/drawing")
               (:file "backend/input")
               (:file "startup")))
