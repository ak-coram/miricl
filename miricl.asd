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
               (:file "miricl"))
  :in-order-to ((test-op (test-op "miricl/test"))))

(asdf:defsystem #:miricl/test
  :depends-on (#:miricl
               #:fiveam)
  :components ((:file "miricl-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :miricl)))

(asdf:defsystem #:miricl/*
  :depends-on (#:miricl
               #:miricl/test))
