;;;; Load after Slynk backend interfaces and before slynk-gray.lisp.
(in-package #:slynk-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (miricl-ide-mop:import-native-mop "SLYNK-MOP"))

(defimplementation gray-package-name () :miricl-gray)

(defimplementation package-local-nicknames (package)
  (miricl-packages:package-local-nicknames package))

(defimplementation make-lock (&key name)
  (miricl-threads:make-recursive-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (miricl-threads:with-recursive-lock-held (lock)
    (funcall function)))
