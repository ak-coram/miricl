;;;; Closer MOP's convenience extension used by McCLIM. Class and slot
;;;; protocols come from Miricl's MOP under its C2MOP package nickname.
(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mcclim-ensure-finalized (class)
    (unless (c2mop:class-finalized-p class) (c2mop:finalize-inheritance class))
    class)
  (let ((symbol (intern "ENSURE-FINALIZED" "C2MOP")))
    (setf (fdefinition symbol) #'mcclim-ensure-finalized)
    (export (list symbol 'class) "C2MOP")))
