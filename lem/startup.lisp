(in-package #:lem-browser)

(defun start-editor ()
  "Start the genuine LEM command loop, or focus its existing browser instance."
  (when (and *browser* (editor-live-p *browser*))
    (unless (member (browser-state *browser*) '(:starting :ready))
      (error "The previous LEM editor is still running in state ~S; stop it before restarting."
             (browser-state *browser*)))
    (when (browser-root *browser*) (js:invoke (browser-root *browser*) "focus"))
    (return-from start-editor (values)))
  (when (and *browser* (browser-bridge *browser*)
             (bt2:thread-alive-p (browser-bridge *browser*)))
    (error "The previous LEM frontend is still stopping."))
  (when (and *browser* (browser-services-live-p *browser*))
    (error "The previous local Lisp service is still stopping."))
  (when *browser* (cleanup-browser *browser*))
  (setf *last-failure* nil)
  (setf *browser* (make-instance 'browser))
  (handler-case
      (lem-core::invoke-frontend
       (lambda (&optional initialize finalize)
         (lem-core::run-editor-thread initialize (lem:parse-args '("-q")) finalize))
       :implementation *browser*)
    (error (condition)
      (record-failure *browser* :startup condition)
      ;; INVOKE may have started the real editor before bridge creation failed.
      ;; Request normal exit, wait only a bounded time, and retain live handles
      ;; on failure so another START cannot create an orphaned duplicate.
      (when (editor-live-p *browser*)
        (lem:send-event #'lem:exit-editor)
        (loop repeat 100 while (editor-live-p *browser*) do (sleep 0.01)))
      (unless (or (editor-live-p *browser*)
                  (and (browser-bridge *browser*)
                       (bt2:thread-alive-p (browser-bridge *browser*))))
        (cleanup-browser *browser*))
      (error condition)))
  (values))
(defun stop-editor ()
  "Request ordinary LEM exit; cleanup follows the real editor thread's exit."
  (when (and *browser* (editor-live-p *browser*))
    (cond ((member (browser-state *browser*) '(:close-requested :closing :stopping))
           (values))
          ((and (browser-bridge *browser*)
                (bt2:thread-alive-p (browser-bridge *browser*)))
           (request-stop *browser*))
          (t
           ;; An actually dead bridge forwards nothing. This ordinary Lisp API
           ;; can still queue EXIT directly after its previously delivered keys.
           (invalidate-popups *browser*)
           (close-browser-services *browser*)
           (setf (browser-generation *browser*) nil)
           (lem:send-event #'lem:exit-editor))))
  (values))
(defun editor-status ()
  "Scalar diagnostic state; no host objects or callbacks escape this interface."
  (if *browser*
    (list :state (browser-state *browser*) :failure (browser-failure *browser*)
          :last-failure *last-failure*
          :editor-alive (not (null (editor-live-p *browser*)))
          :bridge-alive (and (browser-bridge *browser*)
                             (not (null (bt2:thread-alive-p (browser-bridge *browser*)))))
          :columns (browser-columns *browser*) :rows (browser-rows *browser*)
          :frames (browser-frames *browser*) :pending (browser-pending *browser*)
          :commands (lem:command-loop-counter))
    (when *last-failure*
      (list :state :failed :failure *last-failure* :last-failure *last-failure*
            :editor-alive nil :bridge-alive nil))))
