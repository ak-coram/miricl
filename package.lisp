;;;; package.lisp

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

(defpackage #:miricl
  (:use #:cl)
  (:local-nicknames (#:wt #:wasmtime)))
