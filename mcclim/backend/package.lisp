(defpackage #:mcclim-browser
  (:use #:clim #:clim-lisp #:clim-backend)
  (:export #:browser-port #:browser-medium #:browser-graft #:browser-frame-manager
           #:browser-font-port #:browser-font-medium #:pattern-data-url #:preview-file))
(in-package #:mcclim-browser)

(defclass browser-pointer (standard-pointer)
  ((x :initform 0 :accessor browser-pointer-x)
   (y :initform 0 :accessor browser-pointer-y)
   (buttons :initform 0 :accessor browser-pointer-buttons)))
(defclass browser-font-port ()
  ((measure-context :initform nil :accessor measure-context)))
(defclass browser-font-medium () ())
(defclass browser-port (browser-font-port basic-port)
  ((root :reader browser-root)
   (incoming :initform nil :accessor incoming-events)
   (outgoing :initform nil :accessor outgoing-events)
   (modifiers :initform 0 :accessor browser-modifiers)
   (stack :initform 0 :accessor browser-stack))
  (:default-initargs :pointer (make-instance 'browser-pointer)))
(defclass browser-graft (graft) ())
(defclass browser-frame-manager (standard-frame-manager) ())
(defclass browser-mirror ()
  ((element :initarg :element :reader mirror-element)
   (canvas :initarg :canvas :reader mirror-canvas)
   (front-canvas :initarg :front-canvas :initform nil :reader mirror-front-canvas)
   (buffer-depth :initform 0 :accessor mirror-buffer-depth)
   (title :initarg :title :initform nil :accessor mirror-title)
   (x :initform 0 :accessor mirror-x)
   (y :initform 0 :accessor mirror-y)
   (decoration-height :initform 0 :accessor mirror-decoration-height)
   (width :initform 1 :accessor mirror-width)
   (height :initform 1 :accessor mirror-height)))

(defun js-field (object name) (js:from-js (js:property object name)))
(defun css (element name value)
  (js:invoke (js:property element "style") "setProperty" name value))
(defun pixels (number) (format nil "~Dpx" (round number)))
(defun element (tag &optional parent)
  (let ((element (js:invoke (js:global "document") "createElement" tag)))
    (when parent (js:invoke parent "appendChild" element))
    element))
(defun enqueue-browser-event (port event)
  ;; JS callbacks only copy input into this inbox. Event construction, locks
  ;; and CLIM handlers run later on the port's cooperative Lisp process.
  (miricl-threads:without-interrupts
    (push event (incoming-events port)))
  ;; This is also the last form of DOM callbacks. Do not return the Lisp
  ;; queue: JS callbacks can return only values supported by the interop.
  (values))
(defun next-browser-event (port)
  ;; A scheduler checkpoint between swapping and clearing the inbox could
  ;; otherwise discard input appended by a DOM callback during that yield.
  (miricl-threads:without-interrupts
    (unless (outgoing-events port)
      (setf (outgoing-events port) (nreverse (incoming-events port))
            (incoming-events port) nil))
    (pop (outgoing-events port))))
