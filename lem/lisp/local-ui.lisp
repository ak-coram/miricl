(in-package #:lem-lisp-mode/connection)

(defun live-buffer-p (buffer) (member buffer (lem:buffer-list) :test #'eq))
(defun destination-live-p (request)
  (and (connection-live-p (local-request-connection request))
       (live-buffer-p (local-request-destination request))
       ;; The upstream transcript helpers resolve this exact named buffer.
       (eq (local-request-destination request) (lem-lisp-mode/internal:repl-buffer))))
(defun finalize-request (request)
  (let ((finalizer (local-request-finalizer request)))
    (setf (local-request-finalizer request) nil)
    (when finalizer (funcall finalizer))))
(defun release-ui-request (request)
  (unwind-protect (finalize-request request)
    (setf (local-request-active request) nil (local-request-callback request) nil
          (local-request-source request) nil (local-request-destination request) nil)
    (let ((connection (local-request-connection request)))
      (setf (connection-requests connection) (delete request (connection-requests connection))))))
(defun finalize-local-requests (connection)
  ;; Called on the editor thread before teardown; never from a worker/DOM hook.
  (require-editor-thread (connection-owner connection))
  (dolist (request (copy-list (connection-requests connection)))
    (handler-case (release-ui-request request)
      (error (condition)
        (lem-browser::record-failure (connection-owner connection) :local-finalize condition))))
  (setf (connection-requests connection) nil)
  (values))
(defun local-result-value (request result)
  (case (local-request-kind request)
    (:pretty (list :value (miricl-ide-services:request-printed-values result)
                   :id (miricl-ide-services:request-id result)))
    (:compile (miricl-ide-services:request-compilation result))
    (:package (let ((name (package-name (miricl-ide-services:request-package result))))
                (list name name)))
    (:packages (first (miricl-ide-services:request-values result)))
    (otherwise (miricl-ide-services:request-printed-values result))))
(defun deliver-result (request result)
  (let ((connection (local-request-connection request)))
    (unwind-protect
         (when (and (local-request-active request) (destination-live-p request))
           (let* ((package (miricl-ide-services:request-package result))
                  (name (and (packagep package) (package-name package)))
                  (failure (miricl-ide-services:request-condition result))
                  (output (miricl-ide-services:request-output result))
                  (listener (eq :listener (local-request-kind request))))
             (when name (setf (connection-package connection) name
                              (connection-prompt-string connection) name))
             (when (or listener (plusp (length output))
                       (not (member (local-request-kind request) '(:packages :package))))
               (lem-lisp-mode/internal:write-string-to-repl
                (format nil "~&; Request ~D (~A)~%~A~A~%"
                        (local-request-id request) (local-request-kind request) output
                        (or failure (miricl-ide-services:request-printed-values result)))))
             (let ((callback (local-request-callback request)))
               (when callback
                 (funcall callback
                          (if (eq :ok (miricl-ide-services:request-state result))
                              (list :ok (local-result-value request result))
                              (list :abort (or failure "The Lisp request did not complete."))))))))
      (release-ui-request request)))
  (values))
(defun enqueue-result (request result)
  ;; Recheck the actual owner before enqueue AND in DELIVER-RESULT. No protocol
  ;; dispatcher/event log, serialization, global current-buffer lookup or UI
  ;; mutation runs on the service worker.
  (when (and (local-request-active request)
             (connection-live-p (local-request-connection request)))
    (if (destination-live-p request)
        (lem:send-event (lambda () (deliver-result request result)))
        ;; A deleted target receives no output/callback. The editor still owns
        ;; spinner cleanup; the worker may only enqueue this distinct route.
        (lem:send-event
         (lambda ()
           (when (and (local-request-active request)
                      (connection-live-p (local-request-connection request)))
             (release-ui-request request)))))))
(defun submit-local-source (connection operation source
                            &key continuation package request-id (kind :eval) finalizer)
  (check-type connection connection)
  (require-editor-thread (connection-owner connection))
  (unless (connection-live-p connection) (lem:editor-error "The local Lisp connection is closed."))
  (let* ((origin (lem:current-buffer))
         (destination (lem-lisp-mode/internal::ensure-repl-buffer-exist))
         (request (make-local-request :connection connection :id (or request-id (new-request-id))
                   :source origin :destination destination :tick (lem:buffer-modified-tick origin)
                   :callback continuation :kind kind :finalizer finalizer)))
    (handler-case
        (progn
          ;; Include completed requests whose terminal editor event is pending.
          ;; The generic worker queue alone cannot bound this UI backlog.
          (when (>= (length (connection-requests connection)) 32)
            (error "The local Lisp UI has too many outstanding requests."))
          (push request (connection-requests connection))
          (setf (local-request-service request)
                (miricl-ide-services:submit
                 (connection-session connection) operation source
                 :package (or package (connection-package connection))
                 :history-p (eq kind :listener)
                 :callback (lambda (result) (enqueue-result request result))))
          (local-request-id request))
      (error (condition)
        (setf (connection-requests connection) (delete request (connection-requests connection)))
        ;; Boundary rejection happens on the editor thread. It still finishes
        ;; the UI request/prompt and preserves an observable useful diagnostic.
        (unwind-protect
             (when (destination-live-p request)
               (let ((description (lem-browser::diagnostic-description condition 2048)))
                 (lem-lisp-mode/internal:write-string-to-repl
                  (format nil "~&; Request ~D aborted: ~A~%" (local-request-id request) description))
                 (when continuation (funcall continuation (list :abort description)))))
          (release-ui-request request))
        (local-request-id request)))))
(defun bounded-form-size (form)
  (loop for tail = form then (cdr tail) for size from 0 to 16
        when (null tail) return size unless (consp tail) return nil finally (return nil)))
(defun submit-local-form (connection form &key continuation package request-id)
  (unless (and (bounded-form-size form) (symbolp (first form)))
    (lem:editor-error "Malformed local Lisp service form."))
  (let ((operator (first form)) (arguments (rest form)))
    (cond
      ((member operator '(micros:interactive-eval micros:interactive-eval-region))
       (unless (and (= (length arguments) 1) (stringp (first arguments)))
         (lem:editor-error "Evaluation requires source text."))
       (submit-local-source connection :eval (first arguments) :continuation continuation
                           :package package :request-id request-id))
      ((eq operator 'micros:compile-string-for-emacs)
       (unless (and (= (length arguments) 5) (stringp (first arguments)))
         (lem:editor-error "Compilation requires source text and its source context."))
       (when (fifth arguments)
         (unsupported-operation "compile-string policy overrides"))
       (submit-local-source connection :compile-and-load (first arguments) :kind :compile
                           :continuation continuation :package package :request-id request-id))
      ((eq operator 'micros:compile-file-for-emacs)
       (unsupported-operation "file compilation; use compile-region or compile-defun"))
      ((eq operator 'micros:list-all-package-names)
       (submit-local-source connection :eval "(mapcar #'package-name (list-all-packages))"
                           :kind :packages :continuation continuation :package package :request-id request-id))
      ((eq operator 'micros:set-package)
       (unless (and (= (length arguments) 1) (stringp (first arguments)))
         (lem:editor-error "Package selection requires a package name."))
       (submit-local-source connection :eval (format nil "(cl:in-package ~S)" (first arguments))
                           :kind :package :continuation continuation :package (first arguments) :request-id request-id))
      ((eq operator 'micros/pretty-eval:remove-evaluated-values)
       (unless (and (= (length arguments) 1) (typep (first arguments) '(integer 1 *)))
         (lem:editor-error "Malformed local result identifier."))
       (miricl-ide-services:forget-result (connection-session connection) (first arguments))
       (when continuation (funcall continuation '(:ok nil))))
      (t (unsupported-operation (format nil "~S" operator))))))

(in-package #:lem-lisp-mode/internal)
(defun local-lisp-eval-async (form cont package)
  (let ((buffer (current-buffer)))
    (with-remote-eval (form :package package)
      (lambda (outcome)
        (cond ((eq :abort (first outcome)) (display-message "Evaluation aborted: ~A" (second outcome)))
              ((and cont (lem-lisp-mode/connection::live-buffer-p buffer))
               (with-current-buffer buffer (funcall cont (second outcome)))))))))
(defun local-listener-eval (string)
  (let* ((connection (ensure-local-connection)) (buffer (ensure-repl-buffer-exist)))
    (when (buffer-value buffer 'local-listener-owner)
      (editor-error "The local REPL is evaluating; cancel its request or wait for the next prompt."))
    (let ((spinner (start-loading-spinner :modeline :buffer buffer :loading-message "Evaluating..."))
          (owner (gensym "LISTENER-")) (read-only (buffer-read-only-p buffer))
          (handed-off nil) (finalized nil))
      (labels ((finish ()
                 (unless finalized
                   (setf finalized t)
                   (unwind-protect (stop-loading-spinner spinner)
                     (when (eq owner (buffer-value buffer 'local-listener-owner))
                       (setf *repl-evaluating* nil
                             (buffer-value buffer 'local-listener-owner) nil
                             (buffer-read-only-p buffer) read-only))))))
        (unwind-protect
             (progn
               (setf *repl-evaluating* t (buffer-value buffer 'local-listener-owner) owner
                     (buffer-read-only-p buffer) t)
               (lem-lisp-mode/connection:submit-local-source
                connection :eval string :kind :listener
                :continuation
                (lambda (outcome)
                  (declare (ignore outcome))
                  (when (and (not finalized)
                             (lem-lisp-mode/connection::live-buffer-p buffer)
                             (eq buffer (repl-buffer))
                             (eq owner (buffer-value buffer 'local-listener-owner)))
                    (with-buffer-read-only buffer nil
                      (lem/listener-mode:refresh-prompt buffer))))
                :finalizer #'finish)
               (setf handed-off t))
          (unless handed-off (finish)))))))
(defmethod execute :before
    ((mode lisp-repl-mode) (command lem/listener-mode:listener-return) argument)
  (declare (ignore mode command argument))
  (when (buffer-value (current-buffer) 'local-listener-owner)
    (editor-error "The local REPL is evaluating; cancel its request or wait for the next prompt.")))
(defun local-interrupt ()
  (let* ((connection (current-connection))
         (requests (and connection (lem-lisp-mode/connection:connection-requests connection))))
    (dolist (request requests)
      (when (lem-lisp-mode/connection:local-request-service request)
        (miricl-ide-services:cancel
         (lem-lisp-mode/connection:connection-session connection)
         (lem-lisp-mode/connection:local-request-service request))))))
(defun local-compilation-finished (compilation)
  (display-message "Compilation ~A~{~%~A~}"
                   (if (and (getf compilation :loaded) (not (getf compilation :failure)))
                       "finished and loaded" "failed")
                   (getf compilation :diagnostics)))
(defun close-editor-local-service ()
  (when (and lem-browser:*browser* (lem-browser::browser-services lem-browser:*browser*))
    (let ((connection (lem-browser::browser-services lem-browser:*browser*)))
      (lem-lisp-mode/connection:close-local-connection connection)
      (lem-lisp-mode/connection:finalize-local-requests connection))))
(add-hook *exit-editor-hook* 'close-editor-local-service)
(defun initialize-browser-lisp-mode ()
  (when lem-browser:*browser* (change-buffer-mode (current-buffer) 'lisp-mode)))
(add-hook *after-init-hook* 'initialize-browser-lisp-mode)

(in-package #:lem-lisp-mode/eval)
(defun local-eval-region (start end)
  ;; Capture fallible source context before allocating spinner resources.
  (let* ((connection (lem-lisp-mode/connection:ensure-local-connection))
         (buffer (point-buffer start)) (tick (buffer-modified-tick buffer))
         (source (points-to-string start end)) (package (lem-lisp-mode/internal:current-package))
         (spinner (lem/loading-spinner:start-loading-spinner :region :start start :end end))
         (handed-off nil) (finalized nil))
    (labels ((finish ()
               (unless finalized
                 (setf finalized t)
                 (lem/loading-spinner:stop-loading-spinner spinner))))
      (unwind-protect
           (progn
             (setf (spinner-eval-request-id spinner)
                   (lem-lisp-mode/connection:submit-local-source
                    connection :eval source :kind :pretty :package package
                    :continuation
                    (lambda (outcome)
                      (when (and (not finalized)
                                 (lem-lisp-mode/connection::live-buffer-p buffer)
                                 (= tick (buffer-modified-tick buffer)))
                        (lem/loading-spinner:with-line-spinner-points (a b spinner)
                          (if (eq :ok (first outcome))
                              (display-evaluated-message a b (getf (second outcome) :value)
                                                        :id (getf (second outcome) :id))
                              (display-evaluated-message a b (second outcome) :is-error t)))))
                    :finalizer #'finish))
             (setf handed-off t))
        (unless handed-off (finish))))))

(in-package #:lem-browser)
(defmethod close-browser-services ((browser browser))
  (when (browser-services browser)
    (lem-lisp-mode/connection:close-local-connection (browser-services browser)))
  (values))
(defmethod browser-services-live-p ((browser browser))
  (and (browser-services browser)
       (miricl-ide-services:session-worker-alive-p
        (lem-lisp-mode/connection:connection-session (browser-services browser)))))
(defmethod release-browser-services ((browser browser))
  (when (browser-services-live-p browser)
    (error "The local Lisp service is still stopping."))
  (when (browser-services browser)
    (lem-lisp-mode/connections:remove-connection (browser-services browser))
    (when (eq (lem-lisp-mode/connection:current-connection) (browser-services browser))
      (setf (lem-lisp-mode/connection:current-connection) nil))
    (setf (browser-services browser) nil))
  (values))
