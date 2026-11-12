(in-package #:asdf-user)
(defsystem "mcclim-browser"
  :depends-on ("clim-core")
  :serial t
  :components ((:file "package") (:file "port") (:file "input") (:file "windows")
               (:file "medium") (:file "fonts") (:file "bitmaps")))
