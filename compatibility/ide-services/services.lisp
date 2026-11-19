;;;; Optional same-heap services shared by editor and IDE protocol adapters.
(defpackage #:miricl-ide-services
  (:use #:cl)
  (:export #:make-session #:session-package #:session-readtable #:session-live-p
           #:session-worker-alive-p #:close-session #:submit #:cancel #:result
           #:forget-result #:request-id #:request-state #:request-values
           #:request-output #:request-printed-values #:request-condition
           #:request-package #:request-compilation #:request-operation
           #:request-cancelled #:input-unavailable #:debugger-unavailable))
(in-package #:miricl-ide-services)

(defconstant +source-limit+ 1048576)
(defconstant +output-limit+ 65536)
(defconstant +queue-limit+ 32)
(defconstant +result-limit+ 128)

(define-condition request-cancelled (error) () (:report "The Lisp request was cancelled."))
(define-condition input-unavailable (stream-error) ()
  (:report "Interactive input is unavailable in this local Lisp session."))
(define-condition debugger-unavailable (error)
  ((description :initarg :description :reader debugger-description))
  (:report (lambda (condition stream)
             (format stream "The local debugger UI is unavailable: ~A"
                     (debugger-description condition)))))

(defclass capture-stream (miricl-gray:fundamental-character-output-stream)
  ((text :initform (make-array 128 :element-type 'character :adjustable t :fill-pointer 0)
         :reader capture-text)
   (limit :initarg :limit :initform +output-limit+ :reader capture-limit)
   (truncated :initform nil :accessor capture-truncated)
   (column :initform 0 :accessor capture-column)))
(defmethod miricl-gray:stream-write-char ((stream capture-stream) character)
  (if (< (length (capture-text stream)) (capture-limit stream))
      (vector-push-extend character (capture-text stream))
      (setf (capture-truncated stream) t))
  (setf (capture-column stream) (if (char= character #\Newline) 0
                                  (1+ (capture-column stream))))
  character)
(defmethod miricl-gray:stream-line-column ((stream capture-stream)) (capture-column stream))
(defclass unavailable-input (miricl-gray:fundamental-character-input-stream) ())
(defmethod miricl-gray:stream-read-char ((stream unavailable-input))
  (error 'input-unavailable :stream stream))

(defun captured-string (stream)
  (if (capture-truncated stream)
      (concatenate 'string (capture-text stream) (format nil "~%; [output truncated]~%"))
      (copy-seq (capture-text stream))))
(defun describe-safely (object &optional (limit 2048))
  ;; Length/depth and retained characters are bounded. Custom printers may do
  ;; arbitrary work; these controls do not bound their execution time.
  (handler-case
      (let ((*print-readably* nil) (*print-circle* t) (*print-level* 4) (*print-length* 12)
            (out (make-instance 'capture-stream :limit limit)))
        (princ object out) (captured-string out))
    (request-cancelled (condition) (error condition))
    (serious-condition () "<diagnostic printing failed>")))
(defun checked-package (designator)
  (unless (typep designator '(or string symbol package))
    (error 'type-error :datum designator :expected-type '(or string symbol package)))
  (or (find-package designator) (error "Unknown Lisp package ~A." (describe-safely designator))))

(defstruct (session (:constructor %make-session))
  package readtable (live-p t) worker current (queue nil) (next-id 0)
  (results nil) (history (make-list 9)) compile-evaluations notification-error)
(defstruct (request (:constructor %make-request))
  session id operation source package callback history-p
  (state :queued) (values nil) (output "") (printed-values "") condition
  compilation (cancel-p nil))
(defun make-session (&key (package "CL-USER") (readtable *readtable*))
  (check-type readtable readtable)
  (%make-session :package (checked-package package) :readtable (copy-readtable readtable)
                 :compile-evaluations miricl-compiler:*compile-evaluations*))
(defun session-worker-alive-p (session)
  (check-type session session)
  (let ((worker (session-worker session)))
    (cond ((null worker) nil)
          ((miricl-threads:thread-alive-p worker) t)
          (t (setf (session-worker session) nil) nil))))
(defun result (session id)
  (check-type session session) (check-type id (integer 1 *))
  (cdr (assoc id (session-results session))))
(defun forget-result (session id)
  (check-type session session) (check-type id (integer 1 *))
  (setf (session-results session) (delete id (session-results session) :key #'car))
  (values))
(defun retain-result (session request)
  (push (cons (request-id request) request) (session-results session))
  (when (> (length (session-results session)) +result-limit+)
    (setf (cdr (nthcdr (1- +result-limit+) (session-results session))) nil)))
(defun print-values (values)
  (let ((*print-readably* nil) (*print-circle* t) (*print-level* 6) (*print-length* 32)
        (out (make-instance 'capture-stream)))
    (handler-case
        (if values
            (loop for tail on values do (prin1 (car tail) out)
                  (when (cdr tail) (terpri out)))
            (write-string "; No value" out))
      (request-cancelled (condition) (error condition))
      (error () (write-string "<value printing failed>" out)))
    (captured-string out)))

(defun evaluate-source (request)
  (with-input-from-string (stream (request-source request))
    (let ((eof (list nil)) (last-form nil) (values nil))
      (loop for form = (read stream nil eof) until (eq form eof)
            do (setf last-form form values (multiple-value-list (eval form)))
               (finish-output))
      ;; Match Micros' request-level history: a failing later form leaves the
      ;; previous completed request's history intact.
      (when (request-history-p request)
        (setf *** ** ** * * (first values)
              /// // // / / values +++ ++ ++ + + last-form))
      values)))
(defun compile-source (request)
  (let* ((stem (format nil "/tmp/miricl-ide-~A-~D" (symbol-name (gensym)) (request-id request)))
         (source (concatenate 'string stem ".lisp"))
         (output (concatenate 'string stem ".fasl"))
         (diagnostics nil) (paths nil))
    (unwind-protect
         (progn
           (ensure-directories-exist source)
           (when (or (probe-file source) (probe-file output))
             (error "Local compilation scratch path already exists."))
           (with-open-file (stream source :direction :output :if-exists :error)
             (push source paths) (write-string (request-source request) stream))
           ;; Both paths are reserved while this single cooperative request runs.
           (push output paths)
           (handler-bind ((warning (lambda (condition)
                                     (push (describe-safely condition) diagnostics))))
             (multiple-value-bind (pathname warnings failure)
                 (compile-file source :output-file output :verbose nil :print nil)
               (setf (request-compilation request)
                     (list :pathname (and pathname (namestring pathname))
                           :warnings warnings :failure failure :loaded nil
                           :diagnostics (nreverse diagnostics)))
               (when (or failure (null pathname))
                 (error "Compilation failed~{~%~A~}." (getf (request-compilation request) :diagnostics)))
               (let ((loaded (load pathname :verbose nil :print nil)))
                 (setf (getf (request-compilation request) :loaded) loaded)
                 (unless loaded (error "The compiled file was not loaded."))
                 (list loaded)))))
      (dolist (path paths) (when (probe-file path) (delete-file path))))))

(defun execute-request (session request)
  ;; Scalar reader/printer controls reset for each request; the explicitly
  ;; owned readtable and package remain session state.
  (with-standard-io-syntax
   (let* ((sink (make-instance 'capture-stream))
         (*package* (request-package request)) (*readtable* (session-readtable session))
         (miricl-compiler:*compile-evaluations* (session-compile-evaluations session))
         (*standard-output* sink) (*error-output* sink) (*trace-output* sink)
         (*print-readably* nil)
         (*standard-input* (make-instance 'unavailable-input))
         (*terminal-io* (make-two-way-stream *standard-input* sink))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*)
         (*debugger-hook* (lambda (condition hook)
                           (declare (ignore hook))
                           (error 'debugger-unavailable :description (describe-safely condition))))
         (* (first (session-history session))) (** (second (session-history session)))
         (*** (third (session-history session))) (+ (fourth (session-history session)))
         (++ (fifth (session-history session))) (+++ (sixth (session-history session)))
         (/ (seventh (session-history session))) (// (eighth (session-history session)))
         (/// (ninth (session-history session))) (state :ok) (failure nil))
    (unwind-protect
         (handler-case
             (progn
               (when (request-cancel-p request) (error 'request-cancelled))
               (setf (request-values request)
                     (ecase (request-operation request)
                       (:eval (evaluate-source request))
                       (:compile-and-load (compile-source request))))
               ;; Keep :RUNNING through all user printers and diagnostics.
               (setf (request-printed-values request) (print-values (request-values request))))
           (request-cancelled ()
             (setf state :cancelled failure "The Lisp request was cancelled."))
           (serious-condition (condition)
             (handler-case
                 (setf state :error failure (describe-safely condition))
               (request-cancelled ()
                 (setf state :cancelled failure "The Lisp request was cancelled.")))))
      (miricl-threads:without-interrupts
        (setf (request-output request) (captured-string sink)
              (request-package request) *package*
              (session-package session) *package*
              (session-readtable session) *readtable*
              (session-compile-evaluations session) miricl-compiler:*compile-evaluations*
              (session-history session) (list * ** *** + ++ +++ / // ///))))
    (values state failure))))

(defun notify-request (session request callback)
  (when callback
    (handler-case (funcall callback request)
      (serious-condition (condition)
        (setf (session-notification-error session) (describe-safely condition))))))
(defun settle-request (session request state &optional condition)
  ;; Return the notification separately: callbacks never run with interrupts
  ;; deferred or with authority to cancel the next request.
  (miricl-threads:without-interrupts
    (let ((callback (request-callback request)))
      (setf (request-state request) state (request-condition request) condition
            (request-callback request) nil (request-source request) nil
            (session-current session) nil)
      (retain-result session request)
      callback)))
(defun run-queue (session)
  (let ((normal-exit nil))
    (unwind-protect
         (progn
           (loop
             (let ((request nil) (callback nil))
               (miricl-threads:without-interrupts
                 (setf request (pop (session-queue session)) (session-current session) request)
                 (unless request (return))
                 (setf (request-state request) :running)
                 (let ((state :ok) (failure nil))
                   (handler-case
                       (multiple-value-setq (state failure)
                         (miricl-threads:with-interrupts (execute-request session request)))
                     (request-cancelled ()
                       (setf state :cancelled failure "The Lisp request was cancelled."))
                     (serious-condition ()
                       ;; An infrastructure failure during cleanup must still
                       ;; settle this job; do not invoke another arbitrary printer.
                       (setf state :error failure "The Lisp request could not be finalized.")))
                   (when (request-cancel-p request)
                     (setf state :cancelled failure "The Lisp request was cancelled."))
                   (setf callback (settle-request session request state failure))))
               (notify-request session request callback)))
           (setf normal-exit t))
      ;; Preserve the worker handle until it really dies. Unexpected nonlocal
      ;; exit still settles every accepted request and closes this session.
      (unless normal-exit
        (setf (session-live-p session) nil)
        (let ((pending (append (when (session-current session) (list (session-current session)))
                               (session-queue session))))
          (setf (session-queue session) nil)
          (dolist (request pending)
            (notify-request session request
              (settle-request session request :cancelled "The Lisp service worker exited."))))))))
(defun submit (session operation source &key package callback history-p)
  (check-type session session)
  (unless (member operation '(:eval :compile-and-load)) (error "Unknown Lisp service operation ~S." operation))
  (check-type source string)
  (unless (<= (length source) +source-limit+) (error "Lisp source exceeds the 1,048,576 character request limit."))
  (when callback (check-type callback function))
  (let ((package (checked-package (or package (session-package session)))))
    (miricl-threads:without-interrupts
      (unless (session-live-p session) (error "The local Lisp session is closed."))
      (when (>= (+ (length (session-queue session)) (if (session-current session) 1 0)) +queue-limit+)
        (error "The local Lisp request queue is full."))
      (let ((request (%make-request :session session :id (incf (session-next-id session))
                       :operation operation :source (copy-seq source) :package package
                       :callback callback :history-p history-p)))
        (setf (session-queue session) (nconc (session-queue session) (list request)))
        (unless (session-worker-alive-p session)
          (handler-case
              (setf (session-worker session)
                    (miricl-threads:make-thread (lambda () (run-queue session)) :name "Local Lisp service"))
            (serious-condition (condition)
              ;; A rejected submission cannot execute on some later submit.
              (setf (session-queue session) (delete request (session-queue session))
                    (request-callback request) nil (request-source request) nil
                    (request-state request) :rejected)
              (error condition))))
        request))))
(defun cancel (session request)
  (check-type session session) (check-type request request)
  (unless (eq session (request-session request)) (error "The request belongs to another Lisp session."))
  (miricl-threads:without-interrupts
    (when (member (request-state request) '(:queued :running))
      (setf (request-cancel-p request) t)
      (when (and (eq request (session-current session)) (session-worker-alive-p session))
        (miricl-threads:interrupt-thread
         (session-worker session)
         (lambda ()
           ;; A delayed interrupt must never abort the next queued request.
           (when (and (eq request (session-current session))
                      (eq :running (request-state request)))
             (error 'request-cancelled)))))))
  (values))
(defun close-session (session)
  (check-type session session)
  (setf (session-live-p session) nil)
  (when (session-current session) (cancel session (session-current session)))
  (dolist (request (session-queue session)) (cancel session request))
  (values))
