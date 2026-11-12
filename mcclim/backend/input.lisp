(in-package #:mcclim-browser)

(defun event-modifiers (event)
  (logior (if (js-field event "shiftKey") +shift-key+ 0)
          (if (js-field event "ctrlKey") +control-key+ 0)
          (if (js-field event "altKey") +meta-key+ 0)
          (if (js-field event "metaKey") +super-key+ 0)))
(defun pointer-button (number)
  (case (truncate number) (0 +pointer-left-button+) (1 +pointer-middle-button+)
    (2 +pointer-right-button+) (otherwise climi::+pointer-no-button+)))
(defun pointer-buttons (buttons)
  ;; JS numbers arrive as doubles, including DOM integer-valued fields.
  (let ((bits (truncate buttons)))
    (logior (if (logbitp 0 bits) +pointer-left-button+ 0)
            (if (logbitp 1 bits) +pointer-right-button+ 0)
            (if (logbitp 2 bits) +pointer-middle-button+ 0))))

(defun install-input-handlers (port sheet mirror)
  (let ((canvas (or (mirror-front-canvas mirror) (mirror-canvas mirror))))
    (flet ((listen-to (type handler)
             (js:invoke canvas "addEventListener" type (js:callback handler))))
      (dolist (binding '(("pointerdown" :press) ("pointerup" :release) ("pointermove" :motion)
                         ("pointerenter" :enter) ("pointerleave" :exit) ("wheel" :wheel)))
        (let ((kind (second binding)))
          (listen-to (first binding)
            (lambda (event)
              (let ((bounds (js:invoke canvas "getBoundingClientRect")))
                (when (eq kind :press)
                  (js:invoke canvas "focus")
                  (enqueue-browser-event port (list :focus sheet))
                  (js:invoke canvas "setPointerCapture" (js-field event "pointerId")))
                (when (eq kind :wheel) (js:invoke event "preventDefault"))
                (enqueue-browser-event port
                  (list :pointer sheet kind
                    (- (js-field event "clientX") (js-field bounds "left"))
                    (- (js-field event "clientY") (js-field bounds "top"))
                    (pointer-button (js-field event "button")) (event-modifiers event)
                    (floor (js-field event "timeStamp"))
                    (if (eq kind :wheel) (js-field event "deltaX") 0)
                    (if (eq kind :wheel) (js-field event "deltaY") 0)
                    (pointer-buttons (js-field event "buttons")))))))))
      (dolist (binding '(("keydown" :down) ("keyup" :up)))
        (let ((kind (second binding)))
          (listen-to (first binding)
            (lambda (event)
              (js:invoke event "preventDefault")
              (enqueue-browser-event port (list :key sheet kind (js-field event "key")
                (event-modifiers event) (floor (js-field event "timeStamp")))))))))))

(defun browser-key (text)
  (cond ((= (length text) 1) (values (char text 0) (char text 0)))
        (t (let ((entry (assoc text '(("Enter" :return 13) ("Tab" :tab 9)
                     ("Backspace" :backspace 8) ("Delete" :delete 127) ("Escape" :escape 27)
                     ("ArrowLeft" :left) ("ArrowRight" :right) ("ArrowUp" :up) ("ArrowDown" :down)
                     ("PageUp" :page-up) ("PageDown" :page-down) ("Home" :home) ("End" :end)) :test #'string=)))
             (values (if entry (second entry) (intern (string-upcase text) :keyword))
                     (when (third entry) (code-char (third entry))))))))
(defun translate-browser-event (port input)
  (destructuring-bind (kind sheet &rest data) input
    (unless (sheet-grafted-p sheet) (return-from translate-browser-event nil))
    (case kind
      (:focus
       (raise-sheet sheet)
       (make-instance 'clime:window-manager-focus-event :sheet sheet))
      (:close (make-instance 'window-manager-delete-event :sheet sheet))
      (:configure
       (destructuring-bind (x y width height) data
         (make-instance 'window-configuration-event :sheet sheet
           :region (make-rectangle* x y (+ x width) (+ y height)))))
      (:repaint
       (make-instance 'window-repaint-event :sheet sheet
         :region (make-rectangle* 0 0 (first data) (second data))))
      (:key
       (destructuring-bind (direction text modifiers timestamp) data
         (setf (browser-modifiers port) modifiers)
         (multiple-value-bind (name character) (browser-key text)
           (make-instance (if (eq direction :down) 'key-press-event 'key-release-event)
             :sheet sheet :key-code (if character (char-code character) 0)
             :key-name name :key-character character :modifier-state modifiers :timestamp timestamp))))
      (:pointer
       (destructuring-bind (action x y button modifiers timestamp delta-x delta-y buttons) data
         (setf (browser-modifiers port) modifiers
               (browser-pointer-buttons (port-pointer port)) buttons)
         (multiple-value-bind (screen-x screen-y) (climi::screen-position-from-sheet-position sheet x y)
           (setf (browser-pointer-x (port-pointer port)) screen-x
                 (browser-pointer-y (port-pointer port)) screen-y))
         (apply #'make-instance
           (ecase action (:press 'pointer-button-press-event) (:release 'pointer-button-release-event)
             (:motion 'pointer-motion-event) (:enter 'pointer-enter-event) (:exit 'pointer-exit-event)
             (:wheel 'pointer-scroll-event))
           :sheet sheet :pointer (port-pointer port) :x x :y y
           :modifier-state modifiers :timestamp timestamp
           (case action
             ((:press :release) (list :button button))
             (:wheel (list :button (if (minusp delta-y) +pointer-wheel-up+ +pointer-wheel-down+)
                           :delta-x (/ delta-x 40) :delta-y (/ delta-y 40))))))))))

(defmethod process-next-event ((port browser-port) &key wait-function timeout)
  (let ((deadline (when timeout (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))))
    (loop
      (when (and wait-function (funcall wait-function))
        (return (values nil :wait-function)))
      (let ((input (next-browser-event port)))
        (when input
          (let ((event (translate-browser-event port input)))
            (when event (distribute-event port event) (return t)))))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return (values nil :timeout)))
      (sleep 0.01))))
