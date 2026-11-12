;;;; Canvas implements the render mixins used by upstream RasterImage.
;;;; The portable image-array operations remain upstream Common Lisp code.
(in-package #:mcclim-render)

(defclass render-port-mixin (mcclim-browser:browser-font-port basic-port) ())
(defclass render-medium-mixin (mcclim-browser:browser-medium) ())
(defclass image-mirror-mixin (mcclim-browser::browser-mirror)
  ((image :initform nil :accessor image-mirror-image)
   (dirty-region :initform +nowhere+ :accessor image-dirty-region)))

(defmethod initialize-instance :after ((mirror image-mirror-mixin) &key)
  (let ((canvas (mcclim-browser::element "canvas")))
    (setf (slot-value mirror 'mcclim-browser::canvas) canvas
          (slot-value mirror 'mcclim-browser::element) canvas)))

(defmethod (setf image-mirror-image) :after (image (mirror image-mirror-mixin))
  (when image
    (let ((canvas (mcclim-browser::mirror-canvas mirror)))
      (setf (js:property canvas "width") (pattern-width image)
            (js:property canvas "height") (pattern-height image))
      (js:invoke (js:invoke canvas "getContext" "2d") "drawImage"
        (mcclim-browser::design-canvas image (pattern-width image) (pattern-height image)) 0 0))))

(defmethod image-mirror-image :around ((mirror image-mirror-mixin))
  (let* ((canvas (mcclim-browser::mirror-canvas mirror))
         (array (mcclim-browser::canvas-image-array canvas))
         (image (call-next-method)))
    (if (and image (equal (array-dimensions (climi::pattern-array image)) (array-dimensions array)))
        (progn
          (loop for i below (array-total-size array) do
            (setf (row-major-aref (climi::pattern-array image) i) (row-major-aref array i)))
          image)
        (setf (slot-value mirror 'image) (make-instance 'image-pattern :array array)))))

(defmacro with-image-locked ((image) &body body)
  `(climi::with-port-locked ((port ,image)) ,@body))

(defun resize-canvas-mirror (mirror region)
  (let ((width (max 1 (ceiling (bounding-rectangle-width region))))
        (height (max 1 (ceiling (bounding-rectangle-height region))))
        (canvas (mcclim-browser::mirror-canvas mirror)))
    (unless (and (= width (mcclim-browser::mirror-width mirror))
                 (= height (mcclim-browser::mirror-height mirror)))
      (setf (mcclim-browser::mirror-width mirror) width
            (mcclim-browser::mirror-height mirror) height
            (js:property canvas "width") width
            (js:property canvas "height") height))))

(defmethod put-mirror-geometry :after ((port render-port-mixin) (sheet mirrored-sheet-mixin) region)
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror (resize-canvas-mirror mirror region))))
(defmethod set-mirror-geometry :after ((port render-port-mixin) (sheet mirrored-sheet-mixin) region)
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror (resize-canvas-mirror mirror region))))
(defmethod realize-mirror ((port render-port-mixin) (sheet mirrored-sheet-mixin))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (setf (sheet-direct-mirror sheet) mirror)
    (climi::update-mirror-geometry sheet)
    mirror))
(defmethod destroy-mirror ((port render-port-mixin) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (setf (js:property (mcclim-browser::mirror-canvas mirror) "width") 0
            (js:property (mcclim-browser::mirror-canvas mirror) "height") 0))))
