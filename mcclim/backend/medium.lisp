(in-package #:mcclim-browser)

(defclass browser-medium (browser-font-medium basic-medium) ())
(defclass browser-pixmap (browser-mirror)
  ((depth :initform 32 :reader pixmap-depth)))
(defmethod pixmap-width ((pixmap browser-pixmap)) (mirror-width pixmap))
(defmethod pixmap-height ((pixmap browser-pixmap)) (mirror-height pixmap))
(defmethod allocate-pixmap ((medium browser-medium) width height)
  (let ((canvas (element "canvas")))
    (setf (js:property canvas "width") (ceiling width)
          (js:property canvas "height") (ceiling height))
    (let ((pixmap (make-instance 'browser-pixmap :element canvas :canvas canvas)))
      (setf (mirror-width pixmap) width (mirror-height pixmap) height)
      pixmap)))
(defmethod deallocate-pixmap ((pixmap browser-pixmap))
  (setf (js:property (mirror-canvas pixmap) "width") 0
        (js:property (mirror-canvas pixmap) "height") 0))

(defun canvas-transform (context transformation &optional (method "setTransform"))
  (multiple-value-bind (xx xy yx yy dx dy) (climi::get-transformation transformation)
    (js:invoke context method xx yx xy yy dx dy)))
(defun ellipse-path (context cx cy dx1 dy1 dx2 dy2 start end filled)
  (let* ((start (or start 0)) (end (or end (* 2 pi)))
         (end (if (< end start) (+ end (* 2 pi)) end)))
    (js:invoke context "save")
    (unwind-protect
      (progn
        (js:invoke context "transform" dx1 dy1 dx2 dy2 cx cy)
        (when (and filled (< (- end start) (* 2 pi))) (js:invoke context "moveTo" 0 0))
        (js:invoke context "arc" 0 0 1 start end)
        (when filled (js:invoke context "closePath")))
      (js:invoke context "restore"))))
(defun simple-region-path (context region)
  (typecase region
    (rectangle
     (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
       (js:invoke context "rect" x1 y1 (- x2 x1) (- y2 y1))) t)
    (polygon
     (let ((first t))
       (map-over-polygon-coordinates
         (lambda (x y) (js:invoke context (if first "moveTo" "lineTo") x y) (setq first nil)) region)
       (js:invoke context "closePath")) t)
    (ellipse
     (multiple-value-bind (cx cy) (ellipse-center-point* region)
       (multiple-value-bind (dx1 dy1 dx2 dy2) (ellipse-radii region)
         (ellipse-path context cx cy dx1 dy1 dx2 dy2 0 (* 2 pi) t))) t)
    (climi::standard-rectangle-set
     (map-over-region-set-regions (lambda (part) (simple-region-path context part)) region) t)
    (climi::standard-region-union
     (every (lambda (part) (simple-region-path context part)) (region-set-regions region)))
    (t nil)))
(defun clip-canvas-region (context region width height)
  (cond ((eq region +everywhere+) nil)
        ((typep region 'climi::standard-region-intersection)
         (dolist (part (region-set-regions region)) (clip-canvas-region context part width height)))
        (t
         (js:invoke context "beginPath")
         (unless (or (eq region +nowhere+) (simple-region-path context region))
           ;; General Boolean regions retain the upstream region predicates.
           ;; Build horizontal pixel runs only when no analytic path applies.
           (js:invoke context "beginPath")
           (loop for y below height do
             (let ((run nil))
               (loop for x from 0 to width
                 for inside = (and (< x width) (region-contains-position-p region (+ x 0.5) (+ y 0.5))) do
                   (cond ((and inside (null run)) (setq run x))
                         ((and (not inside) run)
                          (js:invoke context "rect" run y (- x run) 1) (setq run nil)))))))
         (js:invoke context "clip"))))

(defun css-color (ink)
  (multiple-value-bind (r g b alpha) (climi::color-rgba ink)
    (format nil "rgba(~D,~D,~D,~,6F)" (round (* r 255)) (round (* g 255)) (round (* b 255)) alpha)))
(defun design-canvas (ink width height)
  (let* ((width (max 1 (ceiling width))) (height (max 1 (ceiling height)))
         (canvas (element "canvas")))
    (setf (js:property canvas "width") width (js:property canvas "height") height)
    (let* ((target (js:invoke canvas "getContext" "2d"))
           (image (js:invoke target "createImageData" width height))
           (data (js:property image "data")))
      (loop for y below height do
        ;; Bound interop argument lists even for very wide patterns.
        (loop for start from 0 below width by 1024 do
          (let ((bytes nil))
            (loop for x from start below (min width (+ start 1024))
                  for rgba = (climi::%pattern-rgba-value ink x y) do
              (dolist (shift '(16 8 0 24)) (push (ldb (byte 8 shift) rgba) bytes)))
            (js:invoke data "set" (apply #'js:array (nreverse bytes)) (* (+ (* y width) start) 4)))))
      (js:invoke target "putImageData" image 0 0))
    canvas))
(defun canvas-design (context medium ink)
  (cond ((typep ink '(or color climi::standard-opacity climi::uniform-compositum climi::indirect-ink))
         (css-color ink))
        ((climi::flipping-ink-p ink)
         (let ((first (climi::argb32-from-ink (climi::flipping-ink-design1 ink)))
               (second (climi::argb32-from-ink (climi::flipping-ink-design2 ink))))
           (setf (js:property context "globalCompositeOperation") "difference")
           (css-color (climi::argb32-to-ink (logior #xff000000 (logxor first second))))))
        (t
         (let* ((mirror (medium-drawable medium))
                (canvas (design-canvas ink
                          (if (typep ink 'clime:pattern) (pattern-width ink) (mirror-width mirror))
                          (if (typep ink 'clime:pattern) (pattern-height ink) (mirror-height mirror)))))
           (js:invoke context "createPattern" canvas "repeat")))))

(defun present-mirror (mirror &optional force)
  (when (and (mirror-front-canvas mirror)
             (or force (zerop (mirror-buffer-depth mirror))))
    (let ((context (js:invoke (mirror-front-canvas mirror) "getContext" "2d")))
      (js:invoke context "save")
      (unwind-protect
          (progn
            (js:invoke context "setTransform" 1 0 0 1 0 0)
            (setf (js:property context "globalCompositeOperation") "copy"
                  (js:property context "globalAlpha") 1)
            (js:invoke context "drawImage" (mirror-canvas mirror) 0 0))
        (js:invoke context "restore"))))
  (values))

(defmethod invoke-with-output-buffered ((medium browser-medium) continuation &optional (buffered-p t))
  (let ((mirror (medium-drawable medium)) (previous (medium-buffering-output-p medium)))
    (if (or (null mirror) (null (mirror-front-canvas mirror)))
        (unwind-protect
            (progn (setf (medium-buffering-output-p medium) buffered-p) (funcall continuation))
          (setf (medium-buffering-output-p medium) previous))
        (climi::with-mirror-locked ((medium-sheet medium) :repaint)
          (let ((depth (mirror-buffer-depth mirror)) (completed nil))
            (when (not buffered-p) (present-mirror mirror t))
            (setf (medium-buffering-output-p medium) buffered-p
                  (mirror-buffer-depth mirror) (if buffered-p (1+ depth) 0))
            (unwind-protect (multiple-value-prog1 (funcall continuation) (setq completed t))
              (setf (mirror-buffer-depth mirror) depth
                    (medium-buffering-output-p medium) previous)
              (when (and completed (not previous))
                (climi::with-port-locked ((port medium)) (present-mirror mirror)))))))))

(defmacro with-canvas ((context medium) &body body)
  (let ((object (gensym "MEDIUM")) (mirror (gensym "MIRROR")))
    `(let* ((,object ,medium) (,mirror (medium-drawable ,object)))
       (when ,mirror
         (climi::with-port-locked ((port ,object))
           (let* ((,context (js:invoke (mirror-canvas ,mirror) "getContext" "2d"))
                  (climi::*foreground-ink* (medium-foreground ,object))
                  (climi::*background-ink* (medium-background ,object)))
             (js:invoke ,context "save")
             (unwind-protect
               (progn
                 (js:invoke ,context "setTransform" 1 0 0 1 0 0)
                 (clip-canvas-region ,context (medium-native-region ,object) (mirror-width ,mirror) (mirror-height ,mirror))
                 (clip-canvas-region ,context
                   (transform-region (medium-native-transformation ,object) (climi::medium-region ,object))
                   (mirror-width ,mirror) (mirror-height ,mirror))
                 (canvas-transform ,context (medium-native-transformation ,object))
                 (let* ((style (medium-line-style ,object))
                        (dashes (climi::line-style-effective-dashes style ,object))
                        (paint (canvas-design ,context ,object (medium-ink ,object))))
                   (setf (js:property ,context "fillStyle") paint
                         (js:property ,context "strokeStyle") paint
                         (js:property ,context "lineWidth") (max 1 (climi::line-style-effective-thickness style ,object))
                         (js:property ,context "lineCap") (case (line-style-cap-shape style) (:square "square") (:round "round") (t "butt"))
                         (js:property ,context "lineJoin") (case (line-style-joint-shape style) (:bevel "bevel") (:round "round") (t "miter")))
                   (js:invoke ,context "setLineDash" (apply #'js:array
                     (typecase dashes (null nil) (number (list dashes dashes)) (sequence (coerce dashes 'list)))))
                 ,@body)
               (js:invoke ,context "restore")
               (unless (medium-buffering-output-p ,object) (present-mirror ,mirror))))))))))
(defun paint-path (context filled)
  (if filled (js:invoke context "fill")
      (progn (js:invoke context "setTransform" 1 0 0 1 0 0) (js:invoke context "stroke"))))
(defmethod climi::medium-draw-pattern* ((medium browser-medium) pattern x y)
  (with-canvas (context medium)
    (js:invoke context "drawImage"
      (design-canvas pattern (pattern-width pattern) (pattern-height pattern)) x y)))
(defmethod medium-draw-point* ((medium browser-medium) x y)
  (with-canvas (context medium)
    (js:invoke context "beginPath")
    (js:invoke context "arc" x y (/ (js-field context "lineWidth") 2) 0 (* 2 pi))
    (paint-path context t)))
(defmethod medium-draw-points* ((medium browser-medium) coordinates)
  (loop for i from 0 below (length coordinates) by 2 do
    (medium-draw-point* medium (elt coordinates i) (elt coordinates (1+ i)))))
(defmethod medium-draw-line* ((medium browser-medium) x1 y1 x2 y2)
  (with-canvas (context medium)
    (js:invoke context "beginPath") (js:invoke context "moveTo" x1 y1)
    (js:invoke context "lineTo" x2 y2) (paint-path context nil)))
(defmethod medium-draw-lines* ((medium browser-medium) coordinates)
  (loop for i from 0 below (length coordinates) by 4 do
    (medium-draw-line* medium (elt coordinates i) (elt coordinates (+ i 1))
      (elt coordinates (+ i 2)) (elt coordinates (+ i 3)))))
(defmethod medium-draw-polygon* ((medium browser-medium) coordinates closed filled)
  (with-canvas (context medium)
    (js:invoke context "beginPath")
    (loop for i from 0 below (length coordinates) by 2 do
      (js:invoke context (if (zerop i) "moveTo" "lineTo") (elt coordinates i) (elt coordinates (1+ i))))
    (when closed (js:invoke context "closePath")) (paint-path context filled)))
(defmethod medium-draw-rectangle* ((medium browser-medium) x1 y1 x2 y2 filled)
  (with-canvas (context medium)
    (js:invoke context "beginPath") (js:invoke context "rect" x1 y1 (- x2 x1) (- y2 y1))
    (paint-path context filled)))
(defmethod medium-draw-rectangles* ((medium browser-medium) coordinates filled)
  (loop for i from 0 below (length coordinates) by 4 do
    (medium-draw-rectangle* medium (elt coordinates i) (elt coordinates (+ i 1))
      (elt coordinates (+ i 2)) (elt coordinates (+ i 3)) filled)))
(defmethod medium-draw-ellipse* ((medium browser-medium) cx cy dx1 dy1 dx2 dy2 start end filled)
  (with-canvas (context medium)
    (js:invoke context "beginPath") (ellipse-path context cx cy dx1 dy1 dx2 dy2 start end filled)
    (paint-path context filled)))
(defmethod medium-clear-area ((medium browser-medium) left top right bottom)
  (with-canvas (context medium)
    (setf (js:property context "globalCompositeOperation") "source-over"
          (js:property context "fillStyle") (css-color (medium-background medium)))
    (js:invoke context "fillRect" left top (- right left) (- bottom top))))
(defmethod medium-finish-output ((medium browser-medium))
  (climi::with-port-locked ((port medium))
    (let ((mirror (medium-drawable medium))) (when mirror (present-mirror mirror t)))))
(defmethod medium-force-output ((medium browser-medium)) (medium-finish-output medium))
(defmethod medium-beep ((medium browser-medium)) (values))

(defun copy-canvas-area (source x y width height target tx ty &optional buffered-p)
  (js:invoke (js:invoke (mirror-canvas target) "getContext" "2d") "drawImage"
    (mirror-canvas source) x y width height tx ty width height)
  (unless buffered-p (present-mirror target)))
(defmethod medium-copy-area ((source browser-medium) x y width height (target browser-medium) tx ty)
  (climi::with-port-locked ((port source))
    (copy-canvas-area (medium-drawable source) x y width height (medium-drawable target) tx ty
      (medium-buffering-output-p target))))
(defmethod medium-copy-area ((source browser-medium) x y width height (target browser-pixmap) tx ty)
  (climi::with-port-locked ((port source))
    (copy-canvas-area (medium-drawable source) x y width height target tx ty)))
(defmethod medium-copy-area ((source browser-pixmap) x y width height (target browser-medium) tx ty)
  (climi::with-port-locked ((port target))
    (copy-canvas-area source x y width height (medium-drawable target) tx ty
      (medium-buffering-output-p target))))
(defmethod medium-copy-area ((source browser-pixmap) x y width height (target browser-pixmap) tx ty)
  (copy-canvas-area source x y width height target tx ty))
