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

(defgeneric soundness-transport-generic (left right))

(defmethod soundness-transport-generic ((left single-float) right)
  (declare (single-float left right))
  (* left right))

(defmethod soundness-transport-generic ((left integer) right)
  (values (+ left right) :integer))

(defun soundness-transport-generic-consumer (left right)
  (declare (notinline soundness-transport-generic))
  (* (soundness-transport-generic left right) 2.0f0))

(defun soundness-transport-generic-pipeline (left right)
  (declare (single-float right) (notinline soundness-transport-generic))
  (* (soundness-transport-generic left (+ right 1.0f0)) 2.0f0))

(defgeneric soundness-transport-generic-forward (selector value))

(defmethod soundness-transport-generic-forward ((selector (eql :integer)) value)
  (declare (ignore selector) (integer value))
  value)

(defmethod soundness-transport-generic-forward ((selector (eql :character)) value)
  (declare (ignore selector) (character value))
  value)

(defmethod soundness-transport-generic-forward ((selector (eql :double)) value)
  (declare (ignore selector) (double-float value))
  value)

(defmethod soundness-transport-generic-forward ((selector (eql :immediate)) value)
  (declare (ignore selector) (type (signed-byte 31) value))
  value)

(defmethod soundness-transport-generic-forward ((selector (eql :bit)) value)
  (declare (ignore selector) (bit value))
  value)

(defun soundness-transport-generic-character (text index)
  (declare (notinline soundness-transport-generic-forward))
  (char-code (soundness-transport-generic-forward :character (char text index))))

(defun soundness-transport-generic-integer (array index)
  (declare (notinline soundness-transport-generic-forward))
  (logand (soundness-transport-generic-forward
            :integer (logand (aref array index) 36028797018963967))
          127))
