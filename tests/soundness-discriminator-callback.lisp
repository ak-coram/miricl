;;;; Live discriminator selection across a lazy condition-class callback.
(in-package :miricl-compiler)

(setq *compile-evaluations* nil)
(ensure-compiler)

(defgeneric soundness-discriminator-current (object))
(defmethod soundness-discriminator-current ((object condition))
  (declare (ignore object))
  (values :old :default))
(defvar *soundness-discriminator-current-armed* nil)
(defvar *soundness-discriminator-current-calls* 0)
(defvar *soundness-discriminator-current-name* nil)

(assert
 (let* ((driver (compile-native '(lambda (object)
                  (declare (optimize (space 3) (speed 1) (safety 1) (debug 1)))
                  (multiple-value-list (soundness-discriminator-current object)))))
        (ordinary (make-condition 'simple-condition))
        (named (symbol-function (compiler-primitive 10 1 "%%NAMED-CLASS")))
        (hook nil))
   (unwind-protect
        (progn
          (setq hook
                (eval '(defmethod miricl-mop:finalize-inheritance :after
                           ((class standard-class))
                         (when (and *soundness-discriminator-current-armed*
                                    (eq (class-name class) *soundness-discriminator-current-name*))
                           (setq *soundness-discriminator-current-armed* nil)
                           (incf *soundness-discriminator-current-calls*)
                           (miricl-mop:set-funcallable-instance-function
                            #'soundness-discriminator-current
                            (lambda (object)
                              (declare (ignore object))
                              (values :new nil :discriminator)))))))
          (dotimes (i 3)
            (assert (equal (funcall driver ordinary) '(:old :default))))
          ;; Unlike FIND-CLASS, this lookup preserves the lazy precondition.
          (setq *soundness-discriminator-current-name*
                (find-if (lambda (name) (null (funcall named name)))
                         '(floating-point-inexact floating-point-underflow floating-point-overflow
                           floating-point-invalid-operation storage-condition print-not-readable)))
          (unless *soundness-discriminator-current-name*
            (error "Dispatch callback fixture needs an unmaterialized builtin condition class"))
          (let ((lazy (make-condition *soundness-discriminator-current-name*)))
            (when (funcall named *soundness-discriminator-current-name*)
              (error "Dispatch callback fixture materialized its class before dispatch"))
            (setq *soundness-discriminator-current-armed* t
                  *soundness-discriminator-current-calls* 0)
            ;; Observe the in-flight call before any class-inspecting helper.
            (let ((result (funcall driver lazy)))
              (and (equal result '(:new nil :discriminator))
                   (not *soundness-discriminator-current-armed*)
                   (plusp *soundness-discriminator-current-calls*)
                   (equal (funcall driver ordinary) '(:new nil :discriminator))))))
     (setq *soundness-discriminator-current-armed* nil)
     (when hook (remove-method #'miricl-mop:finalize-inheritance hook)))))
(values)
