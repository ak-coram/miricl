// Examples embedded in the standalone browser REPL.
export const config = {name: 'Common Lisp', storageKey: 'miricl.lisp.sessions.v1',
  draftKey: 'miricl.lisp.draft.v1', allowSelection: true, inputHistory: true, backgroundOutput: true,
  busyStatus: 'Evaluating…', rejectedMessage: 'Evaluation failed.', cancelledMessage: 'Evaluation cancelled.',
  manualBase: 'https://www.lispworks.com/documentation/HyperSpec/'};

const exampleSources = {
  debugger: `;; Debug 3 keeps these definitions in the interpreter.
;; Press Evaluate. Each pause opens the debugger dialog.
;; 1. Pause a worker thread with BREAK. The main thread keeps running.
;;    Select its tab, inspect MESSAGE, or evaluate (string-upcase message).
;;    Click CONTINUE to let the worker finish.
(defun debug-worker ()
  (declare (optimize (debug 3)))
  (let ((message "Hello from the worker"))
    (break "The worker is paused. Inspect MESSAGE, then continue.")
    (format t "~A~%" message)))

(miricl-threads:make-thread #'debug-worker :name "Greeting worker")

(defun debug-price (unit-price quantity discount)
  (declare (optimize (debug 3)))
  (let ((subtotal (* unit-price quantity)))
    (restart-case
        (unless (<= 0 discount 100)
          (error "Discount ~A must be between 0 and 100." discount))
      (use-discount (replacement)
        :report "Supply a replacement discount, then finish the calculation."
        (setf discount replacement)))
    (* subtotal (- 1 (/ discount 100)))))

(defun debug-order (quantity)
  (declare (optimize (debug 3)))
  (let ((unit-price 12))
    (debug-price unit-price quantity 150)))

;; 2. This error pauses the main thread too: the dialog now has two tabs.
;;    Switch between the main thread and Greeting worker.
;;    Inspect DEBUG-PRICE's locals and its current expression.
;;    Evaluate (list subtotal discount), or (incf subtotal 5).
;;    Select DEBUG-ORDER to inspect its separate lexical environment.
;;    Enter (list 10) as restart arguments, then click USE-DISCOUNT.
(format t "Recovered total: ~A~%" (debug-order 3))`,
  arithmetic: '(+ 1/3 2/3)\n(expt 2 100)\n(values "hello λ 🙂" #c(1 2))',
  function: '(defun factorial (n)\n  (if (zerop n)\n      1\n      (* n (factorial (1- n)))))\n\n(factorial 30)',
  compilation: `;; Press Restart first to measure a cold compiler. Reruns reuse it.
;; One form keeps progress output streaming while compilation is running.
(progn
;; DEBUG 2 keeps optimized Wasm bytes for DISASSEMBLE.
(defun square (x)
  (declare (optimize (debug 2)))
  (* x x))
;; DEBUG 1 is the default; it retains no disassembly data.
(defun sum-of-squares (x y)
  (declare (optimize (debug 1)))
  (+ (* x x) (* y y)))

;; Inspect the Lisp code and its packed size before compilation.
(disassemble #'square)
(disassemble #'sum-of-squares)

;; The first native compilation also bootstraps the compiler with itself.
(format t "First compilation (includes compiler bootstrap):~%")
(time
  (setf (symbol-function 'square)
    (miricl-compiler:compile-native
      (function-lambda-expression #'square) :name 'square)))

(format t "Second compilation (compiler already bootstrapped):~%")
(time
  (setf (symbol-function 'sum-of-squares)
    (miricl-compiler:compile-native
      (function-lambda-expression #'sum-of-squares) :name 'sum-of-squares)))

;; Native sizes measure each Wasm module, including its imports.
;; SQUARE also shows its Wasm instructions, enabled by DEBUG 2.
(disassemble #'square)
(disassemble #'sum-of-squares)
(values (square 12) (sum-of-squares 3 4)))`,
  loop: '(loop for n from 1 to 5\n      do (format t "~D squared is ~D~%" n (* n n))\n      collect (* n n))',
  environment: `;; Open your browser's developer console to inspect the logged object.
(let* ((navigator (js:global "navigator"))
       (formatter (js:new (js:property (js:global "Intl") "DateTimeFormat")))
       (options (js:invoke formatter "resolvedOptions"))
       (info (js:new (js:global "Object"))))
  (dolist (key '("timeZone" "locale" "calendar" "numberingSystem"))
    (setf (js:property info key) (js:property options key)))
  (setf (js:property info "preferredLocales") (js:property navigator "languages"))
  (dolist (key '("language" "userAgent" "hardwareConcurrency" "onLine"))
    (setf (js:property info key) (js:property navigator key)))
  (setf (js:property info "origin") (js:property (js:global "location") "origin"))
  (setf (js:property info "isSecureContext") (js:global "isSecureContext"))
  (setf (js:property info "crossOriginIsolated") (js:global "crossOriginIsolated"))
  (js:invoke (js:global "console") "log" "Browser environment" info)
  (format t "Browser environment (also logged to the browser console):~%~A~%"
    (js:from-js (js:invoke (js:global "JSON") "stringify" info js:+null+ 2)))
  (values))`,
  timeout: `;; The timer returns immediately; the Lisp closure runs one second later.
(let ((message "Hello from a Lisp callback!"))
  (js:call (js:global "setTimeout")
    (js:callback (lambda ()
      (format t "~A~%" message)
      (js:invoke (js:global "console") "log" message)
      js:+undefined+))
    1000)
  (format t "Timer scheduled. You can keep using the REPL while it waits.~%")
  (values))`
};

export const examples = [
  [
    "arithmetic",
    "NUMBERS",
    "Exact arithmetic and multiple values"
  ],
  [
    "function",
    "FUNCTIONS",
    "Factorial, defined here"
  ],
  [
    "compilation",
    "COMPILER",
    "Compile and disassemble"
  ],
  [
    "debugger",
    "DEBUGGING",
    "A price, a restart and two threads"
  ],
  [
    "loop",
    "ITERATION",
    "A loop that prints and collects"
  ],
  [
    "environment",
    "BROWSER",
    "Inspect the browser environment"
  ],
  [
    "timeout",
    "CALLBACKS",
    "A Lisp closure on a timer"
  ]
].map(([id, category, title]) => ({
  id, category, title, code: exampleSources[id], note: 'Edit the example and evaluate it in your session.',
  manual: 'Front/index.htm', source: 'Common Lisp HyperSpec',
}));
