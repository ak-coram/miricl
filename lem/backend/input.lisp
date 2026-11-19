(in-package #:lem-browser)

(defparameter *named-browser-keys*
  '(("Enter" . "Return") ("ArrowLeft" . "Left") ("ArrowRight" . "Right")
    ("ArrowUp" . "Up") ("ArrowDown" . "Down") (" " . "Space")
    ("Backspace" . "Backspace") ("Delete" . "Delete") ("Home" . "Home")
    ("End" . "End") ("PageUp" . "PageUp") ("PageDown" . "PageDown")
    ("Tab" . "Tab") ("Escape" . "Escape")))
(defun normalized-key (text ctrl alt meta shift)
  (unless (and (stringp text) (<= 1 (length text) 32)
               (member ctrl '(nil t)) (member alt '(nil t))
               (member meta '(nil t)) (member shift '(nil t)))
    (error "Malformed browser key record."))
  (let ((named (cdr (assoc text *named-browser-keys* :test #'string=))))
    (when (or named (= 1 (length text)))
      (lem:make-key :sym (or named text) :ctrl ctrl :meta alt :super meta
                    :shift (if (and (= 1 (length text)) (not (or ctrl alt meta))) nil shift)))))
(defun listen-to (browser target type function)
  (let ((callback (js:callback function)))
    (js:invoke target "addEventListener" type callback)
    (push (list target type callback) (browser-listeners browser))))
(defun enqueue-size (browser)
  (handler-case
      (multiple-value-bind (width height dpr) (browser-size browser)
        (unless (and (bounded-number-p width 0 32768) (bounded-number-p height 0 32768)
                     (bounded-number-p dpr 0.25 8))
          (error "Malformed display dimensions."))
        (enqueue-input browser (list :resize width height dpr)))
    (error () (enqueue-input browser (list :unsupported "malformed display dimensions"))))
  (values))
(defun install-input (browser)
  (listen-to browser (browser-root browser) "keydown"
          (lambda (event)
            (handler-case
                (let* ((text (js-field event "key"))
                       (ctrl (js-field event "ctrlKey")) (alt (js-field event "altKey"))
                       (meta (js-field event "metaKey")) (shift (js-field event "shiftKey")))
                  (unless (and (stringp text) (<= 1 (length text) 32)
                               (every (lambda (value) (member value '(nil t))) (list ctrl alt meta shift)))
                    (error "Malformed keyboard fields."))
                  (cond
                    ((js-field event "isComposing")
                     (js:invoke event "preventDefault")
                     (enqueue-input browser (list :unsupported "keyboard composition input")))
                    ((js:from-js (js:invoke event "getModifierState" "AltGraph"))
                     (js:invoke event "preventDefault")
                     (enqueue-input browser (list :unsupported "AltGraph modifier input")))
                    ((member text '("Dead" "Process" "Unidentified") :test #'string=)
                     (js:invoke event "preventDefault")
                     (enqueue-input browser
                                    (list :unsupported
                                          (cond ((string= text "Dead") "dead-key input")
                                                ((string= text "Process") "keyboard process input")
                                                (t "unidentified keyboard input")))))
                    ;; Keep standard browser reload/address/tab/window shortcuts.
                    ((and (or ctrl meta) (stringp text)
                          (member (string-downcase text) '("r" "l" "t" "w" "n") :test #'string=))
                     (values))
                    ((and (stringp text)
                          (or (= 1 (length text)) (assoc text *named-browser-keys* :test #'string=)))
                     (js:invoke event "preventDefault")
                     (enqueue-input browser (list :key (copy-seq text) ctrl alt meta shift)))))
              (error () (enqueue-input browser (list :unsupported "malformed keyboard input"))))
            (values)))
  (listen-to browser (browser-root browser) "compositionstart"
          (lambda (event)
            (js:invoke event "preventDefault")
            (enqueue-input browser (list :unsupported "composition input"))
            (values)))
  (listen-to browser (browser-root browser) "paste"
          (lambda (event)
            (js:invoke event "preventDefault")
            (enqueue-input browser (list :unsupported "browser clipboard paste"))
            (values)))
  (listen-to browser (js:global "window") "resize"
          (lambda (event) (declare (ignore event)) (enqueue-size browser) (values)))
  ;; ResizeObserver detects container resizing independently of window events.
  (let ((callback (js:callback (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (enqueue-size browser) (values)))))
    (setf (browser-observer browser) (js:new (js:global "ResizeObserver") callback))
    (js:invoke (browser-observer browser) "observe" (browser-container browser)))
  (values))

(defun record-size (record)
  (loop for tail = record then (cdr tail)
        for count from 0 to 8
        when (null tail) return count
        unless (consp tail) return nil
        finally (return nil)))

(defun deliver-input (browser record)
  ;; Records originate at the external DOM boundary. Validate again before
  ;; constructing keys, changing geometry, or calling the real command queue.
  (let ((size (record-size record)))
    (unless size (error "Malformed browser input: expected a bounded proper record.")))
  (case (first record)
    (:key
     (unless (= (record-size record) 6) (error "Malformed browser key record."))
     (let ((key (apply #'normalized-key (rest record))))
       (unless key (error "Unsupported browser key record."))
       (lem:send-event key)))
    (:resize
     (unless (and (= (record-size record) 4)
                  (bounded-number-p (second record) 0 32768)
                  (bounded-number-p (third record) 0 32768)
                  (bounded-number-p (fourth record) 0.25 8))
       (error "Malformed browser display dimensions."))
     (let ((width (second record)) (height (third record)) (dpr (fourth record)))
       ;; RECEIVE-EVENT may discard :RESIZE when more keys are pending. A
       ;; function event runs the same real resize algorithm in strict order.
       (lem:send-event (lambda ()
                         (resize-browser browser width height dpr)
                         (lem:update-on-display-resized)))))
    (:unsupported
     (unless (and (= (record-size record) 2) (stringp (second record))
                  (<= (length (second record)) 256))
       (error "Malformed unsupported-input record."))
     (let ((operation (second record)))
       (lem:send-event (lambda () (unsupported operation)))))
    (:stop
     (unless (= (record-size record) 1) (error "Malformed browser stop record."))
     (set-state browser :closing)
     (lem:send-event #'lem:exit-editor))
    (otherwise (error "Unknown browser input record.")))
  (values))
(defclass browser-popup-messenger (lem/popup-message::popup-messenger)
  ((browser :initarg :browser :reader messenger-browser)))

(defstruct popup-entry owner messenger frame window generation deadline (state :pending))
(defconstant +popup-limit+ 128)
(defconstant +popup-timeout-limit+ 86400)

(defun call-with-browser-messenger (browser function)
  (let* ((messenger (or (browser-messenger browser)
                        (setf (browser-messenger browser)
                              (make-instance 'browser-popup-messenger :browser browser))))
         (lem-core/popup-message-interface:*popup-messenger* messenger)
         ;; BT2 evaluates these forms in the creator. Propagate the complete
         ;; scoped alist to descendants without changing the global defaults.
         (bt2:*default-special-bindings*
           (acons 'bt2:*default-special-bindings* 'bt2:*default-special-bindings*
                  (acons 'lem-core/popup-message-interface:*popup-messenger*
                         (list 'quote messenger) bt2:*default-special-bindings*))))
    (funcall function)))

(defun validate-popup-timeout (timeout)
  (unless (or (null timeout) (bounded-number-p timeout 0 +popup-timeout-limit+))
    (lem:editor-error "Browser popup timeout must be NIL or a finite real from 0 through ~D seconds; received ~A."
                      +popup-timeout-limit+ (diagnostic-description timeout)))
  timeout)
(defun invalidate-popup-entry (entry)
  ;; Queued events capture only ENTRY. Revoke authority and release the entire
  ;; old browser/frame/window graph even if the event has not been consumed.
  (setf (popup-entry-state entry) :invalid
        (popup-entry-owner entry) nil (popup-entry-frame entry) nil
        (popup-entry-messenger entry) nil
        (popup-entry-window entry) nil (popup-entry-generation entry) nil
        (popup-entry-deadline entry) nil))
(defun invalidate-popups (browser)
  (dolist (entry (browser-popups browser)) (invalidate-popup-entry entry))
  (setf (browser-popups browser) nil)
  (values))
(defun invalidate-popup-window (browser window)
  (setf (browser-popups browser)
        (delete-if (lambda (entry)
                     (when (eq window (popup-entry-window entry))
                       (invalidate-popup-entry entry) t))
                   (browser-popups browser)))
  (values))
(defun popup-entry-live-p (browser entry)
  (let ((window (popup-entry-window entry)) (frame (popup-entry-frame entry)))
    (and (eq browser (popup-entry-owner entry)) frame window
         (eq (popup-entry-messenger entry) (browser-messenger browser))
         (eq frame (lem:get-frame browser))
         (not (lem-core::window-deleted-p window))
         (member window (lem:frame-floating-windows frame))
         (member entry (browser-popups browser)))))
(defun popup-entry-authorized-p (browser entry)
  (and (member (browser-state browser) '(:starting :ready))
       (eq browser (lem:implementation))
       (eq (popup-entry-frame entry) (lem:current-frame))
       (browser-generation browser)
       (eq (popup-entry-generation entry) (browser-generation browser))
       (eq browser (messenger-browser (browser-messenger browser)))
       (popup-entry-live-p browser entry)))
(defun prune-popups (browser)
  (setf (browser-popups browser)
        (delete-if (lambda (entry)
                     (unless (popup-entry-live-p browser entry)
                       (invalidate-popup-entry entry) t))
                   (browser-popups browser))))

(defmethod lem-core/popup-message-interface:display-popup-message
    ((messenger browser-popup-messenger) value &key timeout destination-window source-window style)
  (validate-popup-timeout timeout)
  (let* ((browser (messenger-browser messenger)) (frame (lem:get-frame browser)))
    (unless (and (eq messenger (browser-messenger browser))
                 (eq browser (lem:implementation)) frame (eq frame (lem:current-frame))
                 (member (browser-state browser) '(:starting :ready :close-requested)))
      (lem:editor-error "Browser popup messenger belongs to an inactive editor."))
    (prune-popups browser)
    (let ((replacement (find destination-window (browser-popups browser) :key #'popup-entry-window)))
      (when (>= (- (length (browser-popups browser)) (if replacement 1 0)) +popup-limit+)
        (lem:editor-error "Browser popup capacity (~D) is exhausted." +popup-limit+)))
    ;; Keep the real buffer/window construction, gravity and deletion protocol.
    ;; The bridge owns expiry, so upstream starts no unretained timer thread.
    (let ((window (call-next-method messenger value :timeout nil
                                    :destination-window destination-window
                                    :source-window source-window :style style)))
      (push (make-popup-entry
             :owner browser :messenger messenger :frame frame :window window :generation (browser-generation browser)
             ;; Accepted STOP may still drain earlier useful messages, but
             ;; those messages acquire no new deadline or lifecycle authority.
             :deadline (when (and timeout (member (browser-state browser) '(:starting :ready)))
                         (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
             :state (if (browser-generation browser) :pending :inactive))
            (browser-popups browser))
      window)))
(defmethod lem-core/popup-message-interface:delete-popup-message
    ((messenger browser-popup-messenger) window)
  (invalidate-popup-window (messenger-browser messenger) window)
  (call-next-method))

(defun expire-popup-entry (entry)
  (let ((browser (popup-entry-owner entry)))
    (when (and browser (eq :queued (popup-entry-state entry))
               (eq browser (lem:implementation))
               (eq (popup-entry-frame entry) (lem:current-frame))
               (eq (browser-messenger browser) lem-core/popup-message-interface:*popup-messenger*)
               (popup-entry-authorized-p browser entry))
      (let ((window (popup-entry-window entry)) (frame (popup-entry-frame entry)))
        (invalidate-popup-window browser window)
        (lem:delete-window window)
        (when (eq window (lem:frame-message-window frame))
          (setf (lem:frame-message-window frame) nil))
        (lem:redraw-display)))))
(defun enqueue-popup-expirations (browser)
  (prune-popups browser)
  (dolist (entry (browser-popups browser))
    (when (and (< (lem:event-queue-length) 64)
               (eq :pending (popup-entry-state entry)) (popup-entry-deadline entry)
               (<= (popup-entry-deadline entry) (get-internal-real-time))
               (popup-entry-authorized-p browser entry))
      (setf (popup-entry-state entry) :queued)
      (let ((entry entry)) (lem:send-event (lambda () (expire-popup-entry entry))))))
  (values))

(defun cleanup-browser (browser)
  (when (editor-live-p browser)
    (error "Cannot release a browser while its editor is running."))
  (close-browser-services browser)
  (when (browser-services-live-p browser)
    (error "Cannot release a browser while its local Lisp worker is running."))
  (release-browser-services browser)
  (invalidate-popups browser)
  (setf (browser-generation browser) nil)
  ;; Release every owned resource even if one DOM operation fails, retaining
  ;; and resignaling the first real error after all handles have been cleared.
  (let ((failure nil))
    (flet ((release (function)
             (handler-case (funcall function)
               (error (condition)
                 (unless failure (setf failure condition))
                 (record-failure browser :cleanup condition)))))
      (dolist (view (copy-list (browser-views browser)))
        (release (lambda () (lem-if:delete-view browser view))))
      (dolist (listener (browser-listeners browser))
        (destructuring-bind (target type callback) listener
          (release (lambda () (js:invoke target "removeEventListener" type callback)))))
      (when (browser-observer browser)
        (release (lambda () (js:invoke (browser-observer browser) "disconnect"))))
      (when (browser-root browser)
        (release (lambda () (js:invoke (browser-root browser) "remove"))))
      ;; Core TEARDOWN has already freed window points before editor death.
      ;; Never double-free them or remove another lifecycle's frame mapping.
      (when (and (browser-frame browser) (eq (browser-frame browser) (lem:get-frame browser)))
        (release (lambda () (lem:unmap-frame browser)))))
    (setf (browser-listeners browser) nil (browser-observer browser) nil
          (browser-root browser) nil (browser-views browser) nil
          (browser-container browser) nil (browser-editor browser) nil
          (browser-bridge browser) nil (browser-frame browser) nil (browser-messenger browser) nil
          (browser-incoming browser) nil (browser-outgoing browser) nil
          (browser-pending browser) 0)
    (when failure (error failure)))
  (values))
(defun bridge-input (browser)
  (unwind-protect
       (loop while (editor-live-p browser)
             do (enqueue-popup-expirations browser)
                (when (and (member (browser-state browser) '(:starting :ready :close-requested))
                           (< (lem:event-queue-length) 64))
                  (let ((overflow (input-overflow browser)))
                  (when (plusp overflow)
                    (lem:send-event (lambda ()
                                      (lem:editor-error "Browser input queue overflow: ~D events rejected." overflow))))))
                ;; Bound both inbox and forwarded events while a command is
                ;; busy. Removing an inbox record must not create an unlimited
                ;; second backlog in LEM's concurrent queue.
                (loop repeat 64
                      while (member (browser-state browser) '(:starting :ready :close-requested))
                      while (< (lem:event-queue-length) 64)
                      do (let ((record (next-input browser)))
                           (unless record (return))
                           (handler-case (deliver-input browser record)
                           (error (condition)
                             (let ((description (princ-to-string condition)))
                               (lem:send-event (lambda ()
                                                 (lem:editor-error "Browser input rejected: ~A" description))))))))
                (sleep 0.01))
    (cond ((editor-live-p browser)
           ;; An input bridge failure must not erase authority over a live
           ;; editor. Retain its handles; STOP-EDITOR can still request exit.
           (set-state browser :failed "The input bridge stopped while the editor is still running."))
          (t
           ;; A cancelled worker may still be unwinding user cleanup. Keep the
           ;; old lifecycle and its handles until it actually exits.
           (close-browser-services browser)
           (loop while (browser-services-live-p browser) do (sleep 0.01))
           (if (and (eq :stopping (browser-state browser)) (null (browser-failure browser)))
               (set-state browser :stopped)
               (ignore-errors
                 (set-state browser :failed "The editor thread stopped; inspect the reported Lisp condition.")))
           (cleanup-browser browser)))))
