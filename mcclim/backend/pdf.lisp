;;;; PDF generation is deferred for the browser port.
(defpackage #:clim-pdf
  (:use #:clim #:clim-extensions #:clim-lisp #:clim-backend)
  (:export #:with-output-to-pdf-stream))
(in-package #:clim-pdf)

(defmacro with-output-to-pdf-stream ((stream destination &rest options) &body body)
  `(with-output-to-drawing-stream (,stream :pdf ,destination ,@options) ,@body))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (backend (eql :pdf)) destination &rest options)
  (declare (ignore continuation backend destination options))
  (js:invoke (js:global "console") "error"
    "McCLIM PDF export is not implemented in the browser port yet.")
  (values))
