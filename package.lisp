;;;; package.lisp

(defpackage #:miricl
  (:use #:cl))

(defpackage #:wasmtime
  (:use #:cl #:cffi)
  (:export #:with-engine
           #:with-store
           #:with-byte-vec
           #:with-module
           #:wat-to-wasm

           #:callback-func
           #:func-instance
           #:instance-call))
