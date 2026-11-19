;;;; Load after Micros backend interfaces and before backend/gray.lisp.
(in-package #:micros/backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (miricl-ide-mop:import-native-mop "MICROS/MOP"))

(defimplementation gray-package-name () :miricl-gray)

(defimplementation package-local-nicknames (package)
  (miricl-packages:package-local-nicknames package))

(defimplementation make-lock (&key name)
  (miricl-threads:make-recursive-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (miricl-threads:with-recursive-lock-held (lock)
    (funcall function)))
