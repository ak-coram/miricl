;;;; McCLIM image extensions. No changes to the base miricl runtime.
(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :clim-mp *features*)
  (unless (find-package "C2MOP")
    (let ((mop (find-package "MIRICL-MOP")))
      (rename-package mop (package-name mop) (cons "C2MOP" (package-nicknames mop)))))
  (unless (find-package "CLOSER-COMMON-LISP")
    (let ((package (make-package "CLOSER-COMMON-LISP" :use '("COMMON-LISP"))))
      (do-external-symbols (symbol "COMMON-LISP") (export (list symbol) package)))))

(defpackage #:bordeaux-threads
  (:nicknames #:bt)
  (:use #:cl)
  (:export #:*supports-threads-p* #:*default-special-bindings*
           #:make-thread #:destroy-thread #:join-thread #:current-thread
           #:all-threads #:threadp #:thread-name #:thread-alive-p
           #:thread-yield #:interrupt-thread #:make-lock #:make-recursive-lock
           #:with-lock-held #:with-recursive-lock-held #:make-condition-variable
           #:condition-wait #:condition-notify))
(in-package #:bordeaux-threads)
(defparameter *supports-threads-p* t)
(defparameter *default-special-bindings* nil)
(defun make-thread (function &key name (initial-bindings *default-special-bindings*))
  (miricl-threads:make-thread function :name name :initial-bindings initial-bindings))
(macrolet ((forward (&rest names)
             `(progn ,@(loop for name in names
                         for native = (find-symbol (symbol-name name) "MIRICL-THREADS")
                         collect `(setf (fdefinition ',name) (fdefinition ',native))))))
  (forward destroy-thread join-thread current-thread all-threads threadp
           thread-name thread-alive-p thread-yield interrupt-thread
           condition-wait condition-notify))
(defun make-lock (&optional name) (miricl-threads:make-lock :name name))
(defun make-recursive-lock (&optional name) (miricl-threads:make-recursive-lock :name name))
(defmacro with-lock-held ((lock) &body body)
  `(miricl-threads:with-lock-held (,lock) ,@body))
(defmacro with-recursive-lock-held ((lock) &body body)
  `(miricl-threads:with-recursive-lock-held (,lock) ,@body))
(defun make-condition-variable (&key name)
  (miricl-threads:make-condition-variable :name name))

(defpackage #:trivial-features (:use #:cl))
(in-package #:cl-user)
