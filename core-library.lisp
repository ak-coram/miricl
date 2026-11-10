(in-package #:miricl-core)

(defvar *image-toplevel* (list nil))

(miricl-js:define-import %write-image ("miriclCore" "saveImage") (:string) :string)

(defun save-image (filename &key toplevel)
  "Save the current image to FILENAME. Run the optional function TOPLEVEL after restore."
  (unless (miricl-js:available-p)
    (error "Saving a Miricl image requires the JavaScript host."))
  (let ((path (merge-pathnames filename))
        (function (cond ((not toplevel) nil)
                        ((symbolp toplevel) (fdefinition toplevel))
                        ((functionp toplevel) toplevel)
                        (t (error 'type-error :datum toplevel :expected-type '(or symbol function)))))
        (previous (car *image-toplevel*)))
    (unwind-protect
        (progn
          (setf (car *image-toplevel*) function)
          (let ((message (%write-image (namestring path))))
            (unless (zerop (length message)) (error "~A" message)))
          path)
      (setf (car *image-toplevel*) previous))))

(defun %restart-image ()
  (let ((function (car *image-toplevel*)))
    (setf (car *image-toplevel*) nil)
    (when function (funcall function)))
  (values))
