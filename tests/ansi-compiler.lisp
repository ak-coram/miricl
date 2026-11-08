;;;; Adapt RT's existing compilation hook; never change the upstream sources.
(defpackage :miricl-ansi-compiler (:use :cl))
(in-package :miricl-ansi-compiler)
(defvar *attempted* 0)
(defvar *compiled* 0)
(defvar *failed* nil)
(defun reset-counters () (setq *attempted* 0 *compiled* 0 *failed* nil))
(setf (symbol-function 'rt::compile-test-function)
  (lambda (entry)
    (incf *attempted*)
    (handler-case
      (let ((function (miricl-compiler:compile-native
                       (list 'lambda nil (rt::form entry)) :name (rt::name entry))))
        (incf *compiled*) function)
      (error (condition) (setq *failed* t) (error condition)))))
(values)
