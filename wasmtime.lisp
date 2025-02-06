;;;; wasmtime.lisp

(in-package #:wasmtime)

(define-foreign-library libwasmtime
  (:darwin "libwasmtime.dylib")
  (:unix "libwasmtime.so")
  (:windows "libwasmtime.dll")
  (t (:default "libwasmtime")))

(use-foreign-library libwasmtime)
(eval-when (:compile-toplevel)
  (defun make-sym (&rest args)
    (intern (format nil "~:@(~{~A~^-~}~)" args))))

(defmacro wasm-declare-own (name)
  (let ((type-sym (make-sym 'wasm name))
        (pointer-sym (make-sym 'p-wasm name))
        (delete-sym (make-sym 'wasm name 'delete)))
    `(progn
       (defcstruct ,type-sym)
       (defctype ,pointer-sym (:pointer (:struct ,type-sym)))
       (defcfun ,delete-sym :void
         (own (:pointer (:struct ,type-sym)))))))

(defctype size :unsigned-int)
(defctype p-void (:pointer :void))

(wasm-declare-own config)
(defcfun wasm-config-new p-wasm-config)

(wasm-declare-own engine)
(defcfun wasm-engine-new p-wasm-engine)
(defcfun wasm-engine-new-with-config p-wasm-engine
  (own p-wasm-config))

(defcstruct wasmtime-store)
(defctype p-wasmtime-store (:pointer (:struct wasmtime-store)))
(defcfun wasmtime-store-new p-wasmtime-store
  (engine p-wasm-engine)
  (data p-void)
  (finalizer :pointer))

(defcstruct wasmtime-store-context)
(defctype p-wasmtime-store-context (:pointer (:struct wasmtime-store-context)))
(defcfun wasmtime-store-context p-wasmtime-store-context
  (store p-wasmtime-store))
(defcfun wasmtime-store-delete :void
  (store p-wasmtime-store))

(defmacro wasm-declare-vec (name &key is-struct)
  (let ((type-sym (make-sym 'wasm name 'vec))
        (pointer-sym (make-sym 'p-wasm name 'vec))
        (value-type (if is-struct
                        `(:struct ,(make-sym 'wasm name))
                        (make-sym 'wasm name))))
    `(progn
       (defcstruct ,type-sym
         (size size)
         (data (:pointer ,value-type)))
       (defctype ,pointer-sym (:pointer (:struct ,type-sym)))
       (defcfun ,(make-sym 'wasm name 'vec-new-empty) :void
         (out (:pointer (:struct ,type-sym))))
       (defcfun ,(make-sym 'wasm name 'vec-new-uninitialized) :void
         (out ,pointer-sym)
         (size size))
       (defcfun ,(make-sym 'wasm name 'vec-new) :void
         (out ,pointer-sym)
         (size size)
         (own ,value-type))
       (defcfun ,(make-sym 'wasm name 'vec-copy) :void
         (out ,pointer-sym)
         (own ,pointer-sym))
       (defcfun ,(make-sym 'wasm name 'vec-delete) :void
         (own ,pointer-sym)))))

(defctype wasm-byte :char)
(wasm-declare-vec byte)

(defcstruct wasmtime-error)
(defctype p-wasmtime-error (:pointer (:struct wasmtime-error)))
(defcfun wasmtime-error-new p-wasmtime-error
  (message :string))
(defcfun wasmtime-error-delete :void
  (error p-wasmtime-error))
(defcfun wasmtime-error-message :void
  (error p-wasmtime-error)
  (message p-wasm-byte-vec))

(defcfun wasmtime-wat2wasm p-wasmtime-error
  (wat :string)
  (wat-length size)
  (ret p-wasm-byte-vec))

(defcstruct wasmtime-module)
(defctype p-wasmtime-module (:pointer (:struct wasmtime-module)))
(defcfun wasmtime-module-new p-wasmtime-error
  (engine p-wasm-engine)
  (wasm (:pointer :uint8))
  (size size)
  (ret (:pointer p-wasmtime-module))) ; why a pointer to a pointer?
(defcfun wasmtime-module-delete :void
  (module p-wasmtime-module))

(wasm-declare-own functype) (wasm-declare-vec functype :is-struct t)
(wasm-declare-own valtype) (wasm-declare-vec valtype :is-struct t)

(defcfun wasm-functype-new p-wasm-functype
  (params p-wasm-valtype-vec)
  (results p-wasm-valtype-vec))

(defun byte-vec-to-string (byte-vec)
  (with-foreign-slots ((data size) byte-vec (:struct wasm-byte-vec))
    (foreign-string-to-lisp data :count size)))

(defun get-error-message (error)
  (with-foreign-object (error-message 'p-wasm-byte-vec)
    (wasmtime-error-message error error-message)
    (byte-vec-to-string error-message)))

(defun byte-vec-to-octets (byte-vec)
  (with-foreign-slots ((data size) byte-vec (:struct wasm-byte-vec))
    (let ((out (make-array (list size) :element-type '(unsigned-byte 8))))
      (loop :for i :below size
            :do (setf (aref out i) (mem-ref data 'wasm-byte i))
            :finally (return out)))))

(defun wat-to-wasm (input)
  (with-foreign-string ((wat wat-size) input)
    (with-foreign-object (wasm 'p-wasm-byte-vec)
      ;; wat2wasm doesn't expect null-terminator, reduce size by 1:
      (let ((error (wasmtime-wat2wasm wat (1- wat-size) wasm)))
        (if (null-pointer-p error)
            (unwind-protect (byte-vec-to-octets wasm)
              (wasm-byte-vec-delete wasm))
            (unwind-protect (error "Error parsing module: ~A"
                                   (get-error-message error))
              (wasmtime-error-delete error)))))))

(defun compile-module (engine wasm)
  (let ((size (length wasm)))
    (with-foreign-objects ((module 'p-wasmtime-module))
      (with-foreign-array (raw-wasm wasm (list :array :uint8 size))
        (let ((error (wasmtime-module-new engine raw-wasm size module)))
          (if (null-pointer-p error)
              (mem-ref module 'p-wasmtime-module)
              (unwind-protect (error "Error loading module: ~A"
                                     (get-error-message error))
                (wasmtime-error-delete error))))))))

(defmacro with-byte-vec ((byte-vec-var octets) &body body)
  (alexandria:with-gensyms (octets-sym octet-sym i-sym)
    `(let ((,octets-sym ,octets))
       (with-foreign-object (,byte-vec-var 'p-wasm-byte-vec)
         (wasm-byte-vec-new-uninitialized ,byte-vec-var (length ,octets-sym))
         (unwind-protect
              (progn
                (with-foreign-slots ((data) ,byte-vec-var (:struct wasm-byte-vec))
                  (loop :for ,octet-sym :across ,octets-sym
                        :for ,i-sym :from 0
                        :do (setf (mem-ref data 'wasm-byte ,i-sym)
                                  ,octet-sym)))
                ,@body)
           (wasm-byte-vec-delete ,byte-vec-var))))))

(defmacro with-module ((module-var engine wat) &body body)
  (alexandria:with-gensyms (wasm-sym)
    `(let* ((,wasm-sym (wat-to-wasm ,wat))
            (,module-var (compile-module ,engine ,wasm-sym)))
       (unwind-protect (progn ,@body)
         (wasmtime-module-delete module)))))

#+nil
(let* ((hello-wat "(module
                      (func $hello (import \"\" \"hello\"))
                      (func (export \"run\") (call $hello)))")
       (engine (wasm-engine-new))
       (store (wasmtime-store-new engine (null-pointer) (null-pointer))))
  (unwind-protect (with-module (module engine hello-wat)
                    (format t "~A~%" module))
    (wasmtime-store-delete store)
    (wasm-engine-delete engine)))
