;;;; Live interpreter frames; source forms are the interpreter's executable code.
(in-package #:miricl-debugger)

(defconstant %policy-name 'miricl::%debug-level)
(defvar *step-hook* nil)
(defvar %breakpoints nil)
(defvar %session-counter 0)
(defvar %stop-counter 0)
(defvar %active-session nil)
(defvar %debugger-level 0)
(defvar %terminal-lock nil)
(js:define-import %host-stop ("miriclDebugger" "stop") (:value :value) :void)
(js:define-import %host-close ("miriclDebugger" "close") (:number) :void)

(defun frames () (miricl::%%debugger 0))
(defun %check-frame (frame)
  (unless (member frame (frames) :test #'eq) (error 'control-error)) frame)
(defun frame-name (frame) (svref (%check-frame frame) 0))
(defun frame-form (frame) (svref (%check-frame frame) 1))
(defun frame-locals (frame) (miricl::%%debugger 2 (%check-frame frame)))
(defun eval-in-frame (form frame) (miricl::%%debugger 1 (%check-frame frame) form))
(defun set-breakpoint (name) (pushnew name %breakpoints :test #'equal) name)
(defun remove-breakpoint (name) (setq %breakpoints (remove name %breakpoints :test #'equal)) name)
(defun clear-breakpoints () (setq %breakpoints nil))

(defun %unhandled-thread-condition (condition)
  (when (and (frames) (or (miricl::%%debugger 11) *debugger-hook*))
    (invoke-debugger condition)))

(define-condition step-condition (condition) ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (write-string "Interpreter stopped before evaluating the current form." stream))))

(defun %before-form (frame entryp)
  (when *step-hook*
    (let ((hook *step-hook*) (*step-hook* nil)) (funcall hook frame)))
  (when (or (and entryp (member (frame-name frame) %breakpoints :test #'equal))
            (miricl::%%debugger 5))
    (restart-case (invoke-debugger (make-condition 'step-condition))
      (continue () :report "Resume evaluating this form." nil))))

(defun %text (object &optional display)
  (handler-case
      (let ((*print-circle* t) (*print-level* 4) (*print-length* 12)
            (*print-pretty* t) (*print-right-margin* 76) (*print-readably* nil))
        (if display (princ-to-string object) (prin1-to-string object)))
    (error () "#<unprintable>")))

(defun %restart-arguments (arguments)
  (unless (handler-case (list-length arguments) (type-error () nil))
    (error "Restart arguments must evaluate to a proper list, got ~A. For one argument, use (list value)."
           (%text arguments)))
  arguments)

(defun %frame-description (frame depth)
  (js:array (- depth (svref frame 5))
    (%text (frame-name frame)) (%text (frame-form frame))
    (apply #'js:array (mapcar (lambda (binding)
      (js:array (%text (car binding)) (%text (cdr binding)))) (frame-locals frame)))))

(defun %terminal (condition hook)
  (declare (ignore hook))
  (let* ((%debugger-level (1+ %debugger-level))
         (thread (miricl::%%debugger 9))
         (stack (miricl::%%debugger 3)) (depth (miricl::%%debugger 8))
         (restarts (compute-restarts condition)) (selected (first (frames)))
         (message (format nil "~%Debugger: thread #~D, level ~D~%~A: ~A~%~A"
                    thread %debugger-level (%text (type-of condition)) (%text condition t) stack)))
    (miricl-threads::%critical
      (unless %terminal-lock (setq %terminal-lock (miricl-threads:make-recursive-lock :name "Debugger terminal"))))
    ;; Like a terminal's foreground session, only one stopped thread reads
    ;; input at a time. The lock is recursive for nested debugger entries.
    (miricl-threads:with-recursive-lock-held (%terminal-lock)
      (let ((previous (miricl::%%debugger 13 thread)))
        (unwind-protect
          (loop
            (let* ((line (miricl::%%debugger 12
                     (concatenate 'string message
                       (format nil "debug[~D:~D]> " thread %debugger-level))))
                   (command nil))
              (setq message "")
              (miricl::%%debugger 10 nil)
              (handler-case
                (progn
                  (setq command (read-from-string (concatenate 'string "(" line ")")))
                  (when command
                    (let* ((head (car command))
                           (name (and (symbolp head) (string-upcase (symbol-name head))))
                           (index (if (integerp head) head (second command))))
                      (cond
                        ((member name '("HELP" "?") :test #'equal)
                         (setq message "bt | frame N | locals | eval FORM | restarts | restart N [LIST-FORM] | into | next | out | resume | abort
Other threads may wait for terminal input; resolve this session to give them the foreground.
"))
                        ((equal name "BT") (setq message stack))
                        ((equal name "FRAME")
                         (setq selected (find index (frames) :key (lambda (frame) (- depth (svref frame 5)))))
                         (unless selected (error 'control-error))
                         (setq message (format nil "  ~2D: ~A~%      ~A~%" index (%text (frame-name selected)) (%text (frame-form selected)))))
                        ((equal name "LOCALS")
                         (unless selected (error 'control-error))
                         (setq message (with-output-to-string (out)
                           (dolist (binding (frame-locals selected))
                             (format out "      ~A = ~A~%" (%text (car binding)) (%text (cdr binding)))))))
                        ((equal name "RESTARTS")
                         (setq message (with-output-to-string (out)
                           (loop for restart in restarts for number from 0 do
                             (format out "  ~2D: ~A — ~A~%" number (%text (restart-name restart)) (%text restart t))))))
                        ((or (integerp head) (equal name "RESTART"))
                         (unless (and (integerp index) (<= 0 index) (< index (length restarts))) (error 'control-error))
                         (apply #'invoke-restart (nth index restarts)
                           (%restart-arguments
                             (when (third command) (if selected (eval-in-frame (third command) selected) (eval (third command)))))))
                        ((equal name "ABORT") (miricl::%%debugger 4 nil) (return nil))
                        ((member name '("INTO" "STEP" "NEXT" "OVER" "OUT" "RESUME") :test #'equal)
                         (unless (typep condition 'step-condition) (error 'control-error))
                         (miricl::%%debugger 4
                           (cond ((member name '("INTO" "STEP") :test #'equal) 0)
                                 ((member name '("NEXT" "OVER") :test #'equal) (+ 2 (miricl::%%debugger 6)))
                                 ((equal name "OUT") (and selected (1+ (svref selected 3))))))
                         (invoke-restart 'continue))
                        (t
                         (unless selected (error 'control-error))
                         (setq message (with-output-to-string (out)
                           (let ((*standard-output* out))
                             (dolist (value (multiple-value-list
                                 (eval-in-frame (if (equal name "EVAL") (second command) (read-from-string line)) selected)))
                               (format out "~A~%" (%text value)))))))))))
                (error (error) (setq message (format nil "Error: ~A~%" (%text error t)))))
              (miricl::%%debugger 10 t)))
          (miricl::%%debugger 13 previous))))))

(defun %interactive (condition hook)
  (declare (ignore hook))
  (let* ((%debugger-level (1+ %debugger-level))
         (session (miricl-threads::%critical (incf %session-counter)))
         (parent %active-session) (%active-session session)
         (thread (miricl-threads::%thread 0))
         (public-thread (miricl-threads::%thread 3 thread))
         (thread-name (if public-thread (miricl-threads:thread-name public-thread) "Main thread"))
         (pending (list nil))
         (stack (miricl::%%debugger 3)) (stack-depth (miricl::%%debugger 8))
         (restarts (compute-restarts condition))
         (has-result nil) (result-values nil) (result-output "") (result-error nil))
    ;; Commands execute on the application's stopped thread. Nested sessions
    ;; have distinct IDs and retain their parent's pending command.
    (unwind-protect
      (loop
        (%host-stop
          (js:array session (miricl-threads::%critical (incf %stop-counter))
            (miricl::%%debugger 9) thread-name %debugger-level parent (%text (type-of condition)) (%text condition t) stack
            (apply #'js:array (mapcar (lambda (frame) (%frame-description frame stack-depth)) (frames)))
            (apply #'js:array (mapcar (lambda (restart)
              (js:array (%text (restart-name restart)) (%text restart t))) restarts))
            (not (null (typep condition 'step-condition)))
            has-result (apply #'js:array result-values) result-output (or result-error ""))
          (js:callback (lambda (text)
            (setf (car pending) (js:from-js text))
            (miricl-threads::%thread 6 thread) js:+undefined+)))
        (loop until (car pending) do (miricl-threads::%thread 5))
        (let ((text (car pending)))
          (setf (car pending) nil)
          (miricl::%%debugger 10 nil)
          (setq has-result t result-values nil result-output "" result-error nil)
          (handler-case
              (let* ((command (read-from-string text)) (op (first command))
                     (index (second command)) (source (third command))
                     (frame (and (integerp index) (<= 0 index) (nth index (frames)))))
                (case op
                  (:abort (miricl::%%debugger 4 nil) (return nil))
                  (:eval
                   (unless frame (error 'control-error))
                   (setq result-output
                     (with-output-to-string (*standard-output*)
                       (setq result-values (mapcar #'%text
                         (multiple-value-list (eval-in-frame (read-from-string source) frame)))))))
                  (:restart
                   (unless (and (integerp index) (<= 0 index) (< index (length restarts))) (error 'control-error))
                   (let ((arguments (if (and source (plusp (length source)))
                       (if (frames) (eval-in-frame (read-from-string source) (first (frames)))
                           (eval (read-from-string source))) nil)))
                     (apply #'invoke-restart (nth index restarts) (%restart-arguments arguments))))
                  ((:into :over :out :resume)
                   (unless (typep condition 'step-condition) (error 'control-error))
                   (miricl::%%debugger 4
                     (case op (:into 0) (:over (+ 2 (miricl::%%debugger 6)))
                       (:out (and (frames) (1+ (svref (first (frames)) 3))))))
                   (invoke-restart 'continue))
                  (otherwise (error 'program-error))))
            (error (error) (setq result-error (%text error t))))
          (miricl::%%debugger 10 t)))
      (%host-close session))))
