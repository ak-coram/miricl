(defpackage #:lem-browser
  (:use #:cl)
  (:export #:browser #:start-editor #:stop-editor #:editor-status #:*browser*))
(in-package #:lem-browser)

;; Definitions are safe to load/save without a browser. Host objects are created
;; only by INVOKE and are released when the actual editor thread finishes.
(defclass browser (lem:implementation)
  ((root :initform nil :accessor browser-root)
   (container :initform nil :accessor browser-container)
   (views :initform nil :accessor browser-views)
   (listeners :initform nil :accessor browser-listeners)
   (observer :initform nil :accessor browser-observer)
   (editor :initform nil :accessor browser-editor)
   (bridge :initform nil :accessor browser-bridge)
   (state :initform :new :accessor browser-state)
   (failure :initform nil :accessor browser-failure)
   (incoming :initform nil :accessor browser-incoming)
   (outgoing :initform nil :accessor browser-outgoing)
   (pending :initform 0 :accessor browser-pending)
   (overflow :initform 0 :accessor browser-overflow)
   (columns :initform 80 :accessor browser-columns)
   (rows :initform 24 :accessor browser-rows)
   (dpr :initform 1 :accessor browser-dpr)
   (char-width :initform 10 :accessor browser-char-width)
   (char-height :initform 22 :accessor browser-char-height)
   (ascent :initform 16 :accessor browser-ascent)
   (foreground :initform "#dedede" :accessor browser-foreground)
   (background :initform "#202124" :accessor browser-background)
   (title :initform "LEM" :accessor browser-title)
   (cursor-shape :initform :box :accessor browser-cursor-shape)
   (frames :initform 0 :accessor browser-frames)
   (frame :initform nil :accessor browser-frame)
   (messenger :initform nil :accessor browser-messenger)
   (services :initform nil :accessor browser-services)
   (generation :initform (gensym "BROWSER-") :accessor browser-generation)
   (popups :initform nil :accessor browser-popups))
  (:default-initargs :name :browser :support-floating-window t
                    :support-pixel-positioning nil :html-support nil
                    :no-force-needed t :window-left-margin 0 :window-bottom-margin 0))

(defclass browser-view ()
  ((window :initarg :window :reader view-window)
   (group :initarg :group :accessor view-group)
   (edges :initarg :edges :accessor view-edges)
   (canvas :initarg :canvas :accessor view-canvas)
   (backing :initarg :backing :accessor view-backing)
   (context :initarg :context :accessor view-context)
   (border :initarg :border :reader view-border)
   (border-shape :initarg :border-shape :reader view-border-shape)
   (background-color :initform nil :accessor view-background-color)
   (x :initarg :x :accessor view-x)
   (y :initarg :y :accessor view-y)
   (width :initarg :width :accessor view-columns)
   (height :initarg :height :accessor view-body-rows)
   (modeline :initarg :modeline :reader view-modeline-p)
   (cursor-x :initform nil :accessor view-cursor-x)
   (cursor-y :initform nil :accessor view-cursor-y)))

(defvar *browser* nil)
(defvar *last-failure* nil)
(defconstant +inbox-limit+ 1024)

;; Optional local IDE providers retain their own worker authority through STOP.
(defgeneric close-browser-services (browser))
(defgeneric browser-services-live-p (browser))
(defgeneric release-browser-services (browser))
(defmethod close-browser-services ((browser t)) (declare (ignore browser)) (values))
(defmethod browser-services-live-p ((browser t)) (declare (ignore browser)) nil)
(defmethod release-browser-services ((browser t)) (declare (ignore browser)) (values))

(defmacro with-browser-messenger ((browser) &body body)
  `(call-with-browser-messenger ,browser (lambda () ,@body)))

(defun js-field (object name) (js:from-js (js:property object name)))
(defun css (element name value)
  (js:invoke (js:property element "style") "setProperty" name value))
(defun pixels (number) (format nil "~,3Fpx" number))
(defun element (tag &optional parent)
  (let ((node (js:invoke (js:global "document") "createElement" tag)))
    (when parent (js:invoke parent "appendChild" node))
    node))
(defun unsupported (operation)
  (lem:editor-error "Browser frontend: ~A is not supported yet." operation))
(defun diagnostic-description (object &optional (limit 512))
  "Bound retained diagnostic text. User-defined printers may perform arbitrary
work; print depth/length and truncation do not bound their execution time."
  (handler-case
      (let* ((*print-readably* nil) (*print-circle* t) (*print-level* 4) (*print-length* 8)
             (text (if (stringp object) object (princ-to-string object))))
        (if (> (length text) limit)
            (concatenate 'string (subseq text 0 limit) "…")
            (copy-seq text)))
    (error () "<diagnostic printing failed>")))
(defun set-state (browser state &optional failure)
  (setf (browser-state browser) state)
  (when (and failure (null (browser-failure browser)))
    (setf (browser-failure browser) (diagnostic-description failure 2048)
          *last-failure* (browser-failure browser)))
  (when (browser-container browser)
    (setf (js:property (js:property (browser-container browser) "dataset") "lemState")
          (string-downcase (symbol-name state))))
  (values))
(defun record-failure (browser stage condition)
  ;; A diagnostic must never replace the condition being observed. Retain only
  ;; copied bounded scalar text, not a condition/datum or its reachable graph.
  (ignore-errors
    (unless (browser-failure browser)
      (let ((description
              (if (typep condition 'type-error)
                  (format nil "~A: ~A [~A; datum ~A; expected ~A]"
                          stage (diagnostic-description condition)
                          (diagnostic-description (type-of condition))
                          (diagnostic-description (type-error-datum condition))
                          (diagnostic-description (type-error-expected-type condition)))
                  (format nil "~A: ~A [~A]" stage
                          (diagnostic-description condition)
                          (diagnostic-description (type-of condition))))))
        (set-state browser :failed description))))
  (values))

(defun enqueue-input (browser record)
  ;; No lock, wait, evaluator entry or editor command is allowed in DOM callbacks.
  (miricl-threads:without-interrupts
    ;; Input after an explicit close request belongs to the closed lifecycle.
    (unless (member (browser-state browser) '(:close-requested :closing :stopping :stopped :failed))
      (if (< (browser-pending browser) +inbox-limit+)
          (progn (push record (browser-incoming browser))
                 (incf (browser-pending browser)))
          (incf (browser-overflow browser)))))
  (values))
(defun request-stop (browser)
  (miricl-threads:without-interrupts
    (unless (member (browser-state browser) '(:close-requested :closing :stopping :stopped))
      ;; One reserved control slot makes STOP non-droppable at the data limit.
      ;; The state transition closes admission before the marker is appended.
      (invalidate-popups browser)
      (close-browser-services browser)
      (setf (browser-generation browser) nil
            (browser-state browser) :close-requested)
      (push (list :stop) (browser-incoming browser))
      (incf (browser-pending browser))))
  (values))
(defun next-input (browser)
  (miricl-threads:without-interrupts
    (unless (browser-outgoing browser)
      (setf (browser-outgoing browser) (nreverse (browser-incoming browser))
            (browser-incoming browser) nil))
    (when (browser-outgoing browser)
      (decf (browser-pending browser))
      (pop (browser-outgoing browser)))))
(defun input-overflow (browser)
  (miricl-threads:without-interrupts
    (prog1 (browser-overflow browser) (setf (browser-overflow browser) 0))))
(defun bounded-number-p (value minimum maximum)
  ;; Ordered comparisons reject NaN/infinity before integer conversion.
  (and (realp value) (<= minimum value maximum)))
(defun editor-live-p (browser)
  (and (browser-editor browser) (bt2:thread-alive-p (browser-editor browser))))
