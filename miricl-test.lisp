;;;; miricl-test.lisp

(defpackage #:miricl-test
  (:use #:cl #:fiveam #:cffi)
  (:local-nicknames (#:wt #:wasmtime)))
(in-package #:miricl-test)

(def-suite :miricl)
(in-suite :miricl)

(test trivial-compile
  (is (equalp (wt:wat-to-wasm "(module)") #(0 97 115 109 1 0 0 0))))

(test trivial-compile-error
  (signals error
    (wt:wat-to-wasm "(module")))

;;; No public API for these yet:

(defvar *input*)
(defvar *output*)

(defcallback copy-callback wt::p-wasm-trap
    ((env wt::p-void)
     (caller wt::p-wasmtime-caller)
     (args wt::p-wasmtime-val)
     (nargs wt::size)
     (results wt::p-wasmtime-val)
     (nresults wt::size))
  (declare (ignore env caller args nargs results nresults))
  (setf *output* *input*)
  (null-pointer))

(defun callback-func (context callback)
  (with-foreign-objects ((params '(:struct wt::wasm-valtype-vec))
                         (results '(:struct wt::wasm-valtype-vec)))
    (let ((func (foreign-alloc '(:struct wt::wasmtime-func))))
      (wt::wasm-valtype-vec-new-empty params)
      (wt::wasm-valtype-vec-new-empty results)
      (wt::wasmtime-func-new context
                             (wt::wasm-functype-new params results)
                             callback
                             (null-pointer)
                             (null-pointer)
                             func)
      func)))

(defmacro with-callback-func ((func-var context callback) &body body)
  `(let* ((,func-var (callback-func ,context ,callback)))
     (unwind-protect (progn ,@body)
       (foreign-free ,func-var))))

(defun func-instance (context module func)
  (with-foreign-objects ((trap '(:pointer wt::p-wasm-trap))
                         (import '(:struct wt::wasmtime-extern)))
    (with-foreign-slots ((wt::kind wt::of) import (:struct wt::wasmtime-extern))
      (setf wt::kind 0)
      (wt::copy-struct wt::of func '(:struct wt::wasmtime-func)))
    (let* ((instance (foreign-alloc '(:struct wt::wasmtime-instance)))
           (error (wt::wasmtime-instance-new context
                                             module
                                             import
                                             1
                                             instance
                                             trap)))
      (if (null-pointer-p error)
          instance
          (unwind-protect (error "Error instantiating module: ~A"
                                 (wt::get-error-message error))
            (wt::wasmtime-error-delete error))))))

(defmacro with-func-instance ((instance-var context module func) &body body)
  `(let* ((,instance-var (func-instance ,context ,module ,func)))
     (unwind-protect (progn ,@body)
       (foreign-free ,instance-var))))

(defun instance-call (context instance func-name)
  (with-foreign-objects ((trap '(:pointer wt::p-wasm-trap))
                         (run '(:struct wt::wasmtime-extern)))
    (with-foreign-string ((name name-length) func-name)
      (unless (wt::wasmtime-instance-export-get context
                                                instance
                                                name
                                                (1- name-length)
                                                run)
        (error "Failed to get instance export."))
      (with-foreign-slots ((wt::of) run (:struct wt::wasmtime-extern))
        (wt::wasmtime-func-call context
                                wt::of
                                (null-pointer)
                                0
                                (null-pointer)
                                0
                                trap)))))

(test trivial-callback
  (setf *input* (gensym))
  (let ((callback-wat "(module
                         (func $callback (import \"\" \"callback\"))
                         (func (export \"run\") (call $callback)))"))
    (wt:with-engine (engine)
      (wt:with-store ((store context) engine)
        (wt:with-module (module engine callback-wat)
          (with-callback-func (func context (callback copy-callback))
            (with-func-instance (instance context module func)
              (instance-call context instance "run")))))))
  (is (eql *input* *output*)))
