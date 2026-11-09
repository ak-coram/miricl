;;;; package.lisp
(defpackage #:miricl-js
  (:use #:cl)
  (:shadow #:array)
  (:nicknames #:js)
  (:export #:available-p #:value #:valuep #:global #:property #:call #:call-with-this
           #:invoke #:new #:to-js #:from-js #:typeof #:strict-equal #:callback #:array
           #:+null+ #:+undefined+ #:javascript-error #:javascript-error-value
           #:javascript-unavailable #:define-import))

(defpackage #:miricl-threads
  (:use #:cl)
  (:export #:thread #:threadp #:thread-name #:current-thread #:all-threads
           #:make-thread #:join-thread #:thread-alive-p #:thread-yield
           #:interrupt-thread #:destroy-thread #:start-multiprocessing
           #:+supports-threads-p+ #:*standard-io-bindings* #:*default-special-bindings*
           #:thread-error #:abnormal-exit #:abnormal-exit-condition #:abnormal-exit-thread
           #:lock #:lockp #:lock-name #:make-lock #:acquire-lock #:release-lock #:with-lock-held
           #:recursive-lock #:recursive-lock-p #:make-recursive-lock
           #:acquire-recursive-lock #:release-recursive-lock #:with-recursive-lock-held
           #:condition-variable #:condition-variable-p #:make-condition-variable
           #:condition-wait #:condition-notify #:condition-broadcast
           #:semaphore #:semaphorep #:semaphore-name #:semaphore-count
           #:make-semaphore #:signal-semaphore #:wait-on-semaphore
           #:atomic-integer #:atomic-integer-p #:make-atomic-integer
           #:atomic-integer-value #:atomic-integer-compare-and-swap
           #:atomic-integer-incf #:atomic-integer-decf #:timeout #:with-timeout))

(defpackage #:miricl-bundle
  (:use #:cl)
  (:export #:initialize #:dump-system))

(defpackage #:miricl-compiler
  (:use #:cl)
  (:export #:analyze-file #:shake-file #:optimize-system
           #:*compile-evaluations*
           #:compile-to-hir #:verify-hir #:hir-unit #:hir-unit-functions
           #:emit-native-module #:compile-native #:compile-native-bundle
           #:ir-program #:ir-program-nodes #:ir-program-units
           #:ir-node #:ir-node-kind #:ir-node-name #:ir-node-source
           #:ir-node-references #:ir-node-reason #:ir-node-retained-p))

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
           #:interpreter-error-backtrace
           #:interpreter-warning
           #:guest-function-p #:guest-instance-p
           #:guest-opaque-object-p
           #:guest-condition-p #:guest-restart-p
           #:guest-symbol-p #:guest-symbol-name #:guest-symbol-package
           #:guest-package-p #:guest-package-name))
