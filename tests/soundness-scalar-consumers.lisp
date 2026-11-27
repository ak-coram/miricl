(in-package :cl-user)

(defun soundness-scalar-consumers ()
  (let ((cases
          '((soundness-transport-mask
             (lambda (array index) (logand (soundness-transport-mask array index) 127))
             (lambda (array index) (logand (aref array index) 31))
             (#(753) 0) 113 17 243 115)
            (soundness-transport-character
             (lambda (text index) (char-code (soundness-transport-character text index)))
             (lambda (text index) (declare (ignore index)) (char text 1))
             ("AB" 0) 65 66 #\C 67)
            (soundness-transport-single
             (lambda (value factor) (+ (soundness-transport-single value factor) 1.0f0))
             (lambda (value factor) (+ value (the single-float factor)))
             (5.0f0 2.0f0) 11.0f0 8.0f0 3.0d0 4.0d0)
            (soundness-transport-double
             (lambda (value offset) (- (soundness-transport-double value offset) 1.0d0))
             (lambda (value offset) (- (the single-float value) (the double-float offset)))
             (5.0f0 2.0d0) 6.0d0 2.0d0 3.0d0 2.0d0))))
    (dolist (case cases)
      (destructuring-bind (name caller-form replacement-form arguments first second primary last) case
        (let ((caller (miricl-compiler:compile-native caller-form))
              (original (fdefinition name))
              (replacement (miricl-compiler:compile-native replacement-form)))
          (unwind-protect
               (progn
                 (assert (equal (multiple-value-list (apply caller arguments)) (list first)))
                 (setf (fdefinition name) replacement)
                 (assert (equal (multiple-value-list (apply caller arguments)) (list second)))
                 (setf (fdefinition name) (lambda (&rest values)
                                            (declare (ignore values))
                                            (values primary :secondary)))
                 (assert (equal (multiple-value-list (apply caller arguments)) (list last)))
                 (assert (equal (multiple-value-list (apply name arguments)) (list primary :secondary)))
                 (setf (fdefinition name) (lambda (&rest values)
                                            (declare (ignore values))
                                            (throw 'soundness-scalar :escaped)))
                 (assert (eq (catch 'soundness-scalar (apply caller arguments)) :escaped))
                 (fmakunbound name)
                 (assert (handler-case (progn (apply caller arguments) nil)
                           (undefined-function () t))))
            (setf (fdefinition name) original)))))
    t))
