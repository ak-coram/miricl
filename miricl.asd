;;;; miricl.asd

(asdf:defsystem #:miricl
  :author "Ákos Kiss <ak@coram.pub>"
  :license "LGPL-3.0-or-later"
  :serial t
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:cffi
               ;; Don't load libffi on ECL without DFFI
               (:feature (:or (:not :ecl) :dffi)
                         #:cffi-libffi))
  :components ((:file "package")
               (:file "wasmtime")
               (:file "miricl")))
