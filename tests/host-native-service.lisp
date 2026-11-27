;;;; Native module loading through the Common Lisp Wasmtime embedding.
;;;; Load in a matching host system; this does not bootstrap the compiler.
(in-package #:miricl)

(with-interpreter (vm :fuel 5000000000 :native-service t)
  (evaluate-string vm "(setq miricl-compiler:*compile-evaluations* nil miricl-compiler::*compiler-state* :interpreted)")
  (assert (equal '(42 43)
    (evaluate-string vm
      "(let ((function (miricl-compiler:compile-native '(lambda (x) (values (+ x 2) (+ x 3))))))
         (multiple-value-list (funcall function 40)))")))
  (assert (= 42
    (evaluate-string vm
      "(defparameter *host-native-test-closure*
         (funcall (miricl-compiler:compile-native '(lambda (x) (lambda (y) (+ x y)))) 40))
       (funcall *host-native-test-closure* 2)")))
  (assert (= 16
    (evaluate-string vm
      "(let ((outer (miricl-compiler:compile-native
                      '(lambda () (lambda (a b c d e f g h i j k l m n o p) p)))))
         (funcall (funcall outer) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))")))
  (wt:check-error (wt::wasmtime-context-gc (interpreter-context vm)))
  (assert (= 43 (evaluate-string vm "(funcall *host-native-test-closure* 3)")))
  (assert (= 19
    (evaluate-string vm
      "(let ((function (miricl-compiler:compile-native
                        '(lambda () (handler-case (error \"native test\") (error () 19))))))
         (funcall function))")))
  ;; Reject invalid code and current-format metadata without executing a start
  ;; function or losing the surrounding Lisp condition handler.
  (let* ((named (wt:wat-to-wasm
                   "(module (func (export \"entry\") (param externref externref) (result externref) ref.null extern))"))
         (started (wt:wat-to-wasm
                   "(module (func $init) (start $init) (func (export \"entry\") (param externref externref) (result externref) ref.null extern))"))
         (table (wt:wat-to-wasm
                   "(module (table (export \"entries\") 1 1 funcref))"))
         (malformed (list #(0 97 115 109 1 0 0 0 255) started table
                          (concatenate '(vector (unsigned-byte 8)) named
                            (miricl-compiler::with-wasm-bytes
                              (miricl-compiler::wasm-byte 0)
                              (let ((payload (miricl-compiler::with-wasm-bytes
                                               (miricl-compiler::wasm-name "miricl.entries")
                                               (dolist (byte '(2 1 1 255)) (miricl-compiler::wasm-byte byte)))))
                                (miricl-compiler::wasm-u32 (length payload))
                                (miricl-compiler::wasm-append payload)))))))
    (dolist (bytes malformed)
      (assert (evaluate-string vm
        (format nil "(handler-case (progn (miricl-compiler::compiler-primitive 17 ~S #() nil) nil)
                      (error (condition) (> (length (princ-to-string condition)) 0)))" bytes)))
      (assert (= 42 (evaluate-string vm
        "(funcall (miricl-compiler:compile-native '(lambda (x) (+ x 2))) 40)"))))
    ;; A malformed entry index is checked on the Wasm side before entering C.
    (assert (evaluate-string vm
      (format nil "(handler-case (progn (miricl-compiler::compiler-primitive 17 (cons ~S \"bad index\") #() nil) nil)
                    (type-error () t))" named)))
    (assert (= 44 (evaluate-string vm "(funcall *host-native-test-closure* 4)"))))
  ;; Wasmtime installs the same canonical bytes using ordinary, immutable
  ;; globals and a token function that performs the poll and returns zero.
  (let ((ordinary (wt:wat-to-wasm
                    "(module
                       (import \"p\" \"epoch\" (global $epoch i64))
                       (import \"p\" \"active\" (global $active i32))
                       (import \"m\" \"poll-token\" (func $poll (result i64)))
                       (func (export \"entry\") (param externref externref) (result externref)
                         i32.const 42 i32.const -1
                         global.get $epoch i64.eqz
                         global.get $active i32.const 1 i32.eq i32.and
                         call $poll i64.eqz i32.and
                         select ref.i31 extern.convert_any))")))
    (flet ((run-ordinary ()
             (evaluate-string vm
               (format nil "(funcall (miricl-compiler::compiler-primitive 17 ~S #() nil))"
                       ordinary))))
      (assert (= 42 (run-ordinary)))
      (dolist (import '(("epoch" "(mut i64)") ("active" "(mut i32)")
                        ("epoch" "i32") ("active" "i64") ("unknown" "i64")))
        (destructuring-bind (name type) import
          (let ((bytes (wt:wat-to-wasm
                         (format nil "(module (import \"p\" ~S (global ~A))
                                        (func (export \"entry\") (param externref externref)
                                          (result externref) ref.null extern))" name type))))
            (assert (evaluate-string vm
              (format nil "(handler-case
                               (progn (miricl-compiler::compiler-primitive 17 ~S #() nil) nil)
                             (error (condition) (search \"poll global\" (princ-to-string condition))))"
                      bytes))))))
      (assert (= 42 (run-ordinary)))))
  (close-interpreter vm)
  (close-interpreter vm))
(format t "~&Common Lisp native-module service checks passed.~%")
