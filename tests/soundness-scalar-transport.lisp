(in-package :cl-user)

(declaim (optimize (debug 1) (space 2) (speed 1)))

(defun soundness-transport-mask (array index)
  (logand (aref array index) 36028797018963967))

(defun soundness-transport-bit (value)
  (declare (type bit value))
  value)

(defun soundness-transport-immediate (value)
  (declare (type (signed-byte 31) value))
  value)

(defun soundness-transport-integer (value)
  (declare (type integer value))
  value)

(defun soundness-transport-character (text index)
  (char text index))

(defun soundness-transport-character-argument (ignored value)
  (declare (ignore ignored) (type character value))
  value)

(defun soundness-transport-single (value factor)
  (* value (the single-float factor)))

(defun soundness-transport-double (value offset)
  (declare (type single-float value) (type double-float offset))
  (+ value offset))

(defun soundness-transport-mixed (value factor)
  (declare (type integer value) (type single-float factor))
  (* value factor))
