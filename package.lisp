;;;; package.lisp
(defpackage #:wasmtime
  (:use #:cl #:cffi)
  (:export #:with-engine #:with-store #:with-byte-vec #:with-module
           #:wat-to-wasm #:compile-module #:with-instance #:instantiate
           #:callback-func #:with-callback-func #:func-instance #:instance-call
           #:with-export #:extern-payload #:read-value #:write-value
           #:wasmtime-error #:wasm-trap #:wasmtime-error-message-text
           #:check-error #:wasmtime-context-set-fuel
           #:wasmtime-memory-data #:wasmtime-memory-data-size))

(defpackage #:miricl
  (:use #:cl)
  (:local-nicknames (#:wt #:wasmtime))
  (:export #:wat-string #:wat-fragment
           #:make-interpreter #:close-interpreter #:with-interpreter
           #:evaluate #:evaluate-string #:load-source #:read-form #:repl
           #:interpreter-wasm #:write-interpreter-wasm
           #:interpreter-error #:interpreter-package-error #:interpreter-unbound-variable
           #:interpreter-warning
           #:guest-function-p #:guest-instance-p
           #:guest-opaque-object-p
           #:guest-condition-p #:guest-restart-p
           #:guest-symbol-p #:guest-symbol-name #:guest-symbol-package
           #:guest-package-p #:guest-package-name))
