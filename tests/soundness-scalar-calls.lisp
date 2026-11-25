;;;; Ignored operands still cross the checked scalar native call boundary.
(in-package :miricl-compiler)

(defparameter *soundness-scalar-workers*
  (map 'vector #'compile-native
       '((lambda ()
           (declare (optimize (debug 1) (safety 1))) 42)
         (lambda (a)
           (declare (ignore a) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b)
           (declare (ignore a b) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b c)
           (declare (ignore a b c) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b c d)
           (declare (ignore a b c d) (optimize (debug 1) (safety 1))) 42))))
(values)
