;;;; wasmtime.lisp

(in-package #:wasmtime)

(define-foreign-library libwasmtime
  (:darwin "libwasmtime.dylib")
  (:unix "libwasmtime.so")
  (:windows "wasmtime.dll")
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

(defmacro with-engine ((engine-var) &body body)
  `(let ((,engine-var (wasm-engine-new)))
     (unwind-protect (progn ,@body)
       (wasm-engine-delete ,engine-var))))

(defcstruct wasmtime-store)
(defctype p-wasmtime-store (:pointer (:struct wasmtime-store)))
(defcfun wasmtime-store-new p-wasmtime-store
  (engine p-wasm-engine)
  (data p-void)
  (finalizer :pointer))

(defcstruct wasmtime-context)
(defctype p-wasmtime-context (:pointer (:struct wasmtime-context)))
(defcfun wasmtime-store-context p-wasmtime-context
  (store p-wasmtime-store))

(defcfun wasmtime-store-delete :void
  (store p-wasmtime-store))

(defmacro with-store (((store-var context-var) engine) &body body)
  `(let* ((,store-var (wasmtime-store-new ,engine
                                          (null-pointer)
                                          (null-pointer)))
          (,context-var (wasmtime-store-context ,store-var)))
     (unwind-protect (progn ,@body)
       (wasmtime-store-delete ,store-var))))

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
         (wasmtime-module-delete ,module-var)))))

(wasm-declare-own functype) (wasm-declare-vec functype :is-struct t)
(wasm-declare-own valtype) (wasm-declare-vec valtype :is-struct t)

(defcfun wasm-functype-new p-wasm-functype
  (params p-wasm-valtype-vec)
  (results p-wasm-valtype-vec))

(defcstruct wasmtime-func
  (store-id :uint64)
  (size size))
(defctype p-wasmtime-func (:pointer (:struct wasmtime-func)))

(defcfun wasmtime-func-new :void
  (store p-wasmtime-context)
  (type p-wasm-functype)
  (callback :pointer)
  (env p-void)
  (finalizer :pointer)
  (ret p-wasmtime-func))

(wasm-declare-own trap)

(defcstruct wasmtime-caller)
(defctype p-wasmtime-caller (:pointer (:struct wasmtime-caller)))

(defctype wasmtime-valkind :uint8)
(defctype wasmtime-valunion :uint64) ; TODO: add proper definition

(defcstruct wasmtime-val
  (kind wasmtime-valkind)
  (of wasmtime-valunion))
(defctype p-wasmtime-val (:pointer (:struct wasmtime-val)))

(defcstruct wasmtime-instance
  (store-id :uint64)
  (size size))
(defctype p-wasmtime-instance (:pointer (:struct wasmtime-instance)))

(defctype wasmtime-extern-kind :uint8)
(defctype wasmtime-extern-union (:struct wasmtime-func)) ; TODO: add proper definition

(defcstruct wasmtime-extern
  (kind wasmtime-extern-kind)
  (of wasmtime-extern-union))
(defctype p-wasmtime-extern (:pointer (:struct wasmtime-extern)))

(defcfun wasmtime-instance-new p-wasmtime-error
  (context p-wasmtime-context)
  (module p-wasmtime-module)
  (imports p-wasmtime-extern)
  (nimports size)
  (instance p-wasmtime-instance)
  (trap (:pointer p-wasm-trap)))

(defcfun wasmtime-instance-export-get :boolean
  (context p-wasmtime-context)
  (instance p-wasmtime-instance)
  (name :string)
  (name-len size)
  (item p-wasmtime-extern))

(defcfun wasmtime-func-call p-wasmtime-error
  (context p-wasmtime-context)
  (func p-wasmtime-func)
  (args p-wasmtime-val)
  (nargs size)
  (results p-wasmtime-val)
  (nresults size)
  (trap (:pointer p-wasm-trap)))

(defun copy-struct (to from type)
  (loop :for i :below (cffi:foreign-type-size type)
        :do (setf (mem-ref to :uint8 i) (mem-ref from :uint8 i))))
