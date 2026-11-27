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

(defgeneric soundness-transport-pair (a b))

(defmethod soundness-transport-pair (a b)
  (declare (single-float b))
  (* a b))

(defun soundness-transport-pair-caller (a b)
  (declare (single-float b) (notinline soundness-transport-pair))
  (* (soundness-transport-pair (+ a 1.0f0) (+ b 2.0f0)) 2.0f0))

(defgeneric soundness-transport-triple (a b c))

(defmethod soundness-transport-triple (a b c)
  (declare (single-float a) (double-float b) (integer c))
  (+ (* a b) c))

(defun soundness-transport-triple-caller (a b array)
  (declare (single-float a) (double-float b) (notinline soundness-transport-triple))
  (* (soundness-transport-triple (+ a 1.0f0) (+ b 2.0d0)
       (logand (aref array 0) 36028797018963967))
     2.0d0))

(defgeneric soundness-transport-triple-forward (a b c))

(defmethod soundness-transport-triple-forward (a b c)
  (declare (ignore a b) (character c))
  c)

(defun soundness-transport-triple-forward-caller (bits text)
  (declare (notinline soundness-transport-triple-forward))
  (char-code (soundness-transport-triple-forward
    (char-code (char text 0)) (sbit bits 0) (char text 1))))

(defgeneric soundness-transport-triple-small (a b c))

(defmethod soundness-transport-triple-small (a b c)
  (declare (single-float a) (bit b) (type (signed-byte 31) c))
  (+ (+ a b) c))

(defun soundness-transport-triple-small-caller (a bits text)
  (declare (single-float a) (notinline soundness-transport-triple-small))
  (* (soundness-transport-triple-small (+ a 1.0f0) (sbit bits 0)
       (char-code (char text 0)))
     2.0f0))
