;; Appended to FSet's portable port.lisp in the guest overlay. The Quicklisp
;; checkout stays untouched; these definitions are compiled into its FASL.
#+miricl
(progn
  (defun make-lock (&optional name)
    (miricl-threads:make-lock :name (string name)))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (let ((value (gensym "LOCK")))
      `(let ((,value ,lock))
         (when (miricl-threads:acquire-lock ,value :wait ,wait?)
           (unwind-protect (progn ,@body)
             (miricl-threads:release-lock ,value))))))
  (defmacro read-memory-barrier () nil)
  (defmacro write-memory-barrier () nil))
