(in-package #:lem-lisp-mode/connection)

(defun live-buffer-p (buffer) (member buffer (lem:buffer-list) :test #'eq))
(defun macroexpand-request-p (request) (eq :macroexpand-1 (local-request-kind request)))
(defun destination-live-p (request)
  (and (connection-live-p (local-request-connection request))
       (live-buffer-p (local-request-destination request))
       (if (macroexpand-request-p request)
           (eq request (lem:buffer-value (local-request-destination request) 'macroexpand-owner))
           ;; The upstream transcript helpers resolve this exact named buffer.
           (eq (local-request-destination request) (lem-lisp-mode/internal:repl-buffer)))))
(defun finalize-request (request)
  (let ((finalizer (local-request-finalizer request)))
    (setf (local-request-finalizer request) nil)
    (when finalizer (funcall finalizer))))
(defun release-ui-request (request)
  (unwind-protect (finalize-request request)
    (when (and (macroexpand-request-p request) (local-request-destination request)
               (eq request (lem:buffer-value (local-request-destination request) 'macroexpand-owner)))
      (setf (lem:buffer-value (local-request-destination request) 'macroexpand-owner) nil))
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
(defun query-text-prefix (text &optional (limit 2048))
  (if (> (length text) limit)
      (concatenate 'string (subseq text 0 limit) " [source label truncated]")
      (copy-seq text)))
(defun render-macroexpand-result (request result &optional rejection)
  ;; Called only by the owning editor. All arbitrary printers have already run
  ;; on the service worker; this display consumes strings and a boolean only.
  (require-editor-thread (connection-owner (local-request-connection request)))
  (when (destination-live-p request)
    (let* ((buffer (local-request-destination request))
           (context (lem:buffer-value buffer 'macroexpand-context))
           (state (if result (miricl-ide-services:request-state result) :error))
           (failure (or rejection (and result (miricl-ide-services:request-condition result))))
           (text
             (format nil "MACROEXPAND-1 — global environment (NIL)~%Source: ~A (tick ~D)~%Package at start: ~A~%Reader: session readtable at serialized execution~%Request ~D: ~A~%expanded-p: ~A~%Display is bounded (circle/depth/length/characters); it is not replacement source.~%~%Input snapshot:~%~A~%~%~A~A~A"
                     (getf context :name) (local-request-tick request)
                     (query-text-prefix
                      (or (and result (miricl-ide-services:request-start-package result))
                          (getf context :package)) 256)
                     (local-request-id request) state
                     (if (and result (miricl-ide-services:request-values result))
                         (if (second (miricl-ide-services:request-values result)) "true" "false")
                         "unavailable")
                     (getf context :source)
                     (if (and result (plusp (length (miricl-ide-services:request-output result))))
                         (format nil "Captured output:~%~A~%~%" (miricl-ide-services:request-output result)) "")
                     (if failure (format nil "Failure: ~A~%" failure) "Expansion:~%")
                     (if (and result (eq state :ok))
                         (miricl-ide-services:request-printed-values result) ""))))
      (lem:with-buffer-read-only buffer nil
        (lem:erase-buffer buffer)
        (lem:insert-string (lem:buffer-point buffer) text)
        (lem:buffer-start (lem:buffer-point buffer)))
      (setf (lem:buffer-read-only-p buffer) t))))
(defun cancel-macroexpand-buffer-request (buffer)
  ;; Deletion revokes execution and delivery before the buffer is freed. The
  ;; worker sees the existing scalar cancellation flag, never editor structures.
  (let ((request (lem:buffer-value buffer 'macroexpand-owner)))
    (when request
      (require-editor-thread (connection-owner (local-request-connection request)))
      (unwind-protect
           (when (local-request-service request)
             (miricl-ide-services:cancel
              (connection-session (local-request-connection request)) (local-request-service request)))
        (release-ui-request request)))))
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
             (if (macroexpand-request-p request)
                 (render-macroexpand-result request result)
                 (when (or listener (plusp (length output))
                       (not (member (local-request-kind request) '(:packages :package))))
               (lem-lisp-mode/internal:write-string-to-repl
                (format nil "~&; Request ~D (~A)~%~A~A~%"
                        (local-request-id request) (local-request-kind request) output
                        (or failure (miricl-ide-services:request-printed-values result))))))
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
    (if (macroexpand-request-p request)
        ;; Query buffer ownership is inspected only in the editor event. Its
        ;; deletion hook has already revoked the scalar active/cancel authority.
        (lem:send-event (lambda () (deliver-result request result)))
        (if (destination-live-p request)
            (lem:send-event (lambda () (deliver-result request result)))
        ;; A deleted target receives no output/callback. The editor still owns
        ;; spinner cleanup; the worker may only enqueue this distinct route.
        (lem:send-event
         (lambda ()
           (when (and (local-request-active request)
                      (connection-live-p (local-request-connection request)))
             (release-ui-request request))))))))
(defun submit-local-source (connection operation source
                            &key continuation package request-id (kind :eval) finalizer destination
                              (source-buffer (lem:current-buffer))
                              (source-tick (lem:buffer-modified-tick source-buffer)))
  (check-type connection connection)
  (require-editor-thread (connection-owner connection))
  (unless (connection-live-p connection) (lem:editor-error "The local Lisp connection is closed."))
  (let* ((origin source-buffer)
         (destination (or destination (lem-lisp-mode/internal::ensure-repl-buffer-exist)))
         (request (make-local-request :connection connection :id (or request-id (new-request-id))
                   :source origin :destination destination :tick source-tick
                   :callback continuation :kind kind :finalizer finalizer)))
    (handler-case
        (progn
          (when (macroexpand-request-p request)
            (unless (and (live-buffer-p destination)
                         (null (lem:buffer-value destination 'macroexpand-owner)))
              (error "The macroexpansion result destination is unavailable."))
            (setf (lem:buffer-value destination 'macroexpand-owner) request)
            (lem:add-hook (lem:variable-value 'lem:kill-buffer-hook :buffer destination)
                          'cancel-macroexpand-buffer-request))
          (unless (and (lem:bufferp origin) (not (lem:deleted-buffer-p origin)))
            (error "The Lisp request source buffer was deleted."))
          (check-type source-tick (integer 0 *))
          ;; Include completed requests whose terminal editor event is pending.
          ;; Establish query ownership first so rejection is displayed there.
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
                 (if (macroexpand-request-p request)
                     (render-macroexpand-result request nil description)
                     (lem-lisp-mode/internal:write-string-to-repl
                      (format nil "~&; Request ~D aborted: ~A~%" (local-request-id request) description)))
                 (when continuation (funcall continuation (list :abort description)))))
          (release-ui-request request))
        (local-request-id request)))))
(defun bounded-form-size (form)
  (loop for tail = form then (cdr tail) for size from 0 to 16
        when (null tail) return size unless (consp tail) return nil finally (return nil)))
(defun submit-local-form (connection form &key continuation package request-id destination finalizer
                         source-buffer source-tick)
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
      ((eq operator 'micros:swank-macroexpand-1)
       (unless (and (= (length arguments) 1) (stringp (first arguments)))
         (lem:editor-error "One-step macroexpansion requires source text."))
       (unless destination
         (unsupported-operation "one-step expansion without an owned result destination"))
       (submit-local-source connection :macroexpand-1 (first arguments) :kind :macroexpand-1
                           :continuation continuation :package package :request-id request-id
                           :destination destination :finalizer finalizer
                           :source-buffer source-buffer :source-tick source-tick))
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
(defun local-macroexpand-1 (expander)
  (unless (eq expander 'micros:swank-macroexpand-1)
    (unsupported-operation "full, recursive or lexical macroexpansion"))
  ;; Capture source and its actual package before creating/switching result UI.
  (let* ((connection (ensure-local-connection))
         (origin (current-buffer)) (tick (buffer-modified-tick origin))
         (name (lem-lisp-mode/connection::query-text-prefix (buffer-name origin) 256))
         (source (form-string-at-point)) (package (copy-seq (current-package)))
         (buffer (make-buffer (unique-buffer-name "*lisp-macroexpand*")))
         (spinner nil) (handed-off nil) (finalized nil))
    (labels ((finish ()
               (unless finalized
                 (setf finalized t)
                 (when spinner (stop-loading-spinner spinner)))))
      (unwind-protect
           (progn
             (change-buffer-mode buffer 'lisp-mode)
             (setf (buffer-package buffer) package
                   (buffer-value buffer 'lem-lisp-mode/connection::macroexpand-context)
                   (list :name name :package (lem-lisp-mode/connection::query-text-prefix package 256)
                         :source (lem-lisp-mode/connection::query-text-prefix source)))
             (with-buffer-read-only buffer nil
               (insert-string (buffer-point buffer)
                              (format nil "MACROEXPAND-1 — global environment (NIL)~%Source: ~A (tick ~D)~%Package: ~A~%Reader: session readtable at execution~%~%Waiting for one-step expansion...~%~%Captured output and bounded expansion will appear here.~%"
                                      name tick (lem-lisp-mode/connection::query-text-prefix package 256))))
             (setf (buffer-read-only-p buffer) t
                   spinner (start-loading-spinner :modeline :buffer buffer
                                                  :loading-message "Expanding once..."))
             (lem-lisp-mode/connection:submit-local-form
              connection (list expander source) :package package :destination buffer :finalizer #'finish
              :source-buffer origin :source-tick tick)
             ;; Use LEM's actual typeout window once, on the command thread.
             ;; Completion only updates its owned buffer, never steals later focus
             ;; or recreates a dismissed/deleted window or buffer.
             (with-pop-up-typeout-window (out buffer) (declare (ignore out)))
             (setf handed-off t))
        (unless handed-off
          (unwind-protect
               (let ((request (buffer-value buffer 'lem-lisp-mode/connection::macroexpand-owner)))
                 (when request
                   (lem-lisp-mode/connection::cancel-macroexpand-buffer-request buffer)))
            (finish)))))))
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
