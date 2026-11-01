;;;; Load with (miricl:load-source vm "examples/packages.lisp").
(defpackage :miricl-demo
  (:nicknames :demo)
  (:use :cl)
  (:export :factorial :reciprocal-square))

(in-package :miricl-demo)

(defun factorial (n)
  (declare (type (integer 0 *) n))
  (if (zerop n) 1 (* n (factorial (1- n)))))

(defun reciprocal-square (x)
  (declare (number x))
  (/ 1 (* x x)))
