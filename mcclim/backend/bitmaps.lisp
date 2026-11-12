(in-package #:mcclim-browser)

(defun await-browser-promise (promise)
  ;; Promise callbacks only publish a result. SLEEP yields the Lisp process
  ;; until the browser settles it, allowing painting and input to continue.
  (let ((done nil) (result nil) (failed nil))
    (js:invoke promise "then"
      (js:callback (lambda (value) (setq result value done t) (values)))
      (js:callback (lambda (value) (setq result value failed t done t) (values))))
    (loop until done do (sleep 0.01))
    (when failed (error 'js:javascript-error :value result))
    result))

(defun browser-octets (octets)
  (let ((data (js:new (js:global "Uint8Array") (length octets))))
    (loop for start from 0 below (length octets) by 4096 do
      (js:invoke data "set"
        (apply #'js:array (coerce (subseq octets start (min (length octets) (+ start 4096))) 'list)) start))
    data))

(defun canvas-image-array (canvas)
  (let* ((width (truncate (js-field canvas "width")))
         (height (truncate (js-field canvas "height")))
         (context (js:invoke canvas "getContext" "2d"))
         (pixels (js:property (js:invoke context "getImageData" 0 0 width height) "data"))
         (convert (js:property (js:global "String") "fromCharCode"))
         (array (make-array (list height width) :element-type '(unsigned-byte 32))))
    ;; Transfer bytes in bounded chunks rather than one JS call per channel.
    (loop for start from 0 below (* width height 4) by 4096 do
      (let ((bytes (js:from-js (js:invoke convert "apply" js:+undefined+
                     (js:invoke pixels "subarray" start (min (* width height 4) (+ start 4096)))))))
        (loop for i from 0 below (length bytes) by 4 do
          (setf (row-major-aref array (floor (+ start i) 4))
            (logior (ash (char-code (char bytes (+ i 3))) 24)
                    (ash (char-code (char bytes i)) 16)
                    (ash (char-code (char bytes (+ i 1))) 8)
                    (char-code (char bytes (+ i 2))))))))
    array))

(defun bitmap-file-octets (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream) bytes)))

(defun decode-browser-bitmap (octets)
  (let* ((blob (js:new (js:global "Blob") (js:array (browser-octets octets))))
         (bitmap (handler-case
                     (await-browser-promise (js:call (js:global "createImageBitmap") blob))
                   (js:javascript-error () (error 'climi::unsupported-bitmap-format)))))
    (unwind-protect
        (let ((canvas (element "canvas")))
          (setf (js:property canvas "width") (js-field bitmap "width")
                (js:property canvas "height") (js-field bitmap "height"))
          (js:invoke (js:invoke canvas "getContext" "2d") "drawImage" bitmap 0 0)
          (values (canvas-image-array canvas) nil))
      (js:invoke bitmap "close"))))

(defun browser-read-bitmap (pathname)
  (decode-browser-bitmap (bitmap-file-octets pathname)))

;; Build-time icons retain their encoded bytes in the image. No DOM object or
;; host decoder is needed until a browser first uses their pixels or dimensions.
(defclass browser-file-pattern (climi::image-pattern)
  ((octets :initarg :octets :accessor encoded-bitmap)
   (format :initarg :format :reader bitmap-format)
   (designs :initarg :designs :reader bitmap-designs)))
(defmethod climi::pattern-array :around ((pattern browser-file-pattern))
  (unless (slot-boundp pattern 'climi::array)
    (multiple-value-bind (array inks)
        (if (member (bitmap-format pattern) '(:xpm :pixmap :pixmap-3))
            (climi::xpm-parse* (encoded-bitmap pattern))
            (decode-browser-bitmap (encoded-bitmap pattern)))
      (setf (slot-value pattern 'climi::array)
        (if inks
            (climi::pattern-array
              (climi::%collapse-pattern (make-pattern array (or (bitmap-designs pattern) inks))
                0 0 (array-dimension array 1) (array-dimension array 0)))
            array)
        (encoded-bitmap pattern) nil)))
  (call-next-method))

(defun make-pattern-from-bitmap-file (pathname &key designs (format :bitmap))
  (if (or (not (js:available-p)) (string= (js:typeof (js:global "document")) "undefined"))
      (make-instance 'browser-file-pattern :octets (bitmap-file-octets pathname)
        :format format :designs designs)
      (multiple-value-bind (array inks) (climi::read-bitmap-file pathname :format format)
        (if inks (make-pattern array (or designs inks))
            (make-instance 'climi::image-pattern :array array)))))

(defun pattern-data-url (pattern &optional (format :png))
  (let* ((mime (case format ((:png :bitmap) "image/png") ((:jpeg :jpg) "image/jpeg")
                (:webp "image/webp") (otherwise (error 'climi::unsupported-bitmap-format))))
         (canvas (design-canvas pattern (pattern-width pattern) (pattern-height pattern)))
         (url (js:from-js (js:invoke canvas "toDataURL" mime))))
    ;; Browsers silently fall back to PNG for an unsupported encoder.
    ;; Report that explicitly instead of writing PNG under another format.
    (unless (and (> (length url) (+ 6 (length mime)))
                 (string= url (concatenate 'string "data:" mime ";") :end1 (+ 6 (length mime))))
      (error 'climi::unsupported-bitmap-format))
    url))

(defun browser-write-bitmap (pattern destination &optional (format :png))
  (let* ((url (pattern-data-url pattern format))
         (bytes (js:from-js (js:call (js:global "atob") (subseq url (1+ (position #\, url))))))
         (octets (map '(vector (unsigned-byte 8)) #'char-code bytes)))
    (if (streamp destination) (write-sequence octets destination)
        (with-open-file (stream destination :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
          (write-sequence octets stream)))
    destination))

(defun preview-file (pathname)
  (let* ((options (js:new (js:global "Object")))
         (ignored (setf (js:property options "type") "image/svg+xml"))
         (blob (js:new (js:global "Blob") (js:array (browser-octets (bitmap-file-octets pathname))) options))
         (url (js:invoke (js:global "URL") "createObjectURL" blob))
         (dialog (element "dialog" (js:property (js:global "document") "body")))
         (button (element "button" dialog)) (image (element "img" dialog)))
    (declare (ignore ignored))
    (setf (js:property button "textContent") "Close preview"
          (js:property image "src") url
          (js:property image "alt") (namestring pathname))
    (css dialog "max-width" "90vw") (css dialog "max-height" "90vh")
    (css dialog "overflow" "auto") (css image "display" "block")
    (js:invoke button "addEventListener" "click"
      (js:callback (lambda (event) (declare (ignore event)) (js:invoke dialog "close") (values))))
    (js:invoke dialog "addEventListener" "close"
      (js:callback (lambda (event) (declare (ignore event))
                     (js:invoke (js:global "URL") "revokeObjectURL" url)
                     (js:invoke dialog "remove") (values))))
    (js:invoke dialog "showModal")
    (values)))

(dolist (format '(:bitmap :png :jpg :jpeg :gif :webp :avif :bmp :fallback))
  (setf (gethash format climi::*bitmap-file-readers*) #'browser-read-bitmap))
(dolist (format '(:bitmap :png :jpg :jpeg :webp :fallback))
  (let ((format (if (member format '(:bitmap :fallback)) :png format)))
    (setf (gethash format climi::*bitmap-file-writers*)
      (lambda (image destination) (browser-write-bitmap image destination format)))))
