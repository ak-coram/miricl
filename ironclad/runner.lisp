(in-package :cl-user)

(js:define-import ironclad-benchmark-entropy ("benchmark" "entropy") (:i32) :string)

(defclass ironclad-browser-prng () ())

(defmethod ironclad::prng-random-data (size (prng ironclad-browser-prng))
  (declare (ignore prng))
  (ironclad:hex-string-to-byte-array (ironclad-benchmark-entropy size)))

(defvar *ironclad-benchmark-cases* nil)
(defvar *ironclad-benchmark-selection* nil)
(defvar *data-size*)
(defvar *buffer-size*)
(defvar *iterations*)
(defvar *result*)

(defmacro ironclad-benchmark-time (&body body)
  (let ((start (gensym "START")) (result (gensym "RESULT")))
    `(let* ((,start (get-internal-real-time))
            (,result (progn ,@body)))
       (values (- (get-internal-real-time) ,start) ,result))))

;; Keep the upstream timing boundaries and return ticks so the host can retain
;; fractional operation rates and export the unrounded measurements.
(defmacro get-speed-data (&body body)
  `(ironclad-benchmark-time ,@body))

(defmacro get-speed-ops (&body body)
  `(ironclad-benchmark-time ,@body))

(defun ironclad-benchmark-select (names)
  (when (member *ironclad-benchmark-selection* names)
    (list *ironclad-benchmark-selection*)))

(defun ironclad-benchmark-install (form)
  ;; The upstream benchmark functions each enumerate cases in a single DOLIST.
  ;; Keep their setup and timed body and restrict that enumeration to one case.
  (destructuring-bind (operator name parameters (let-operator bindings enumeration &rest rest)) form
    (destructuring-bind (dolist-operator (variable names) &rest body) enumeration
      (assert (and (eq operator 'defun) (eq let-operator 'let) (eq dolist-operator 'dolist)))
      (push (cons name (eval names)) *ironclad-benchmark-cases*)
      (eval `(defun ,name ,parameters
               (let ,bindings
                 (dolist (,variable (ironclad-benchmark-select ,names)) ,@body)
                 ,@rest))))))

(defun ironclad-benchmark-load ()
  (setf *ironclad-benchmark-cases* nil)
  (with-open-file (input "/upstream-benchmarks.lisp")
    ;; The browser bundle already contains the library. The host script loads it with ASDF.
    (assert (string= (read-line input) "(asdf:load-system \"ironclad\")"))
    (loop for form = (read input nil nil)
          until (equal form '(benchmark-ciphers))
          do (case (first form)
               (defparameter (eval form))
               (defmacro
                (assert (member (second form) '(get-speed-data get-speed-ops))))
               (defun (ironclad-benchmark-install form))
               (otherwise (error "Unexpected upstream benchmark form: ~S" form)))))
  (setf *ironclad-benchmark-cases* (nreverse *ironclad-benchmark-cases*))
  (dolist (group *ironclad-benchmark-cases*)
    (dolist (name (cdr group))
      (format t "IB_CASE ~A ~A~%" (car group) name)))
  (values))

(defun ironclad-benchmark-run (group name data-size iterations)
  (let* ((entry (assoc group *ironclad-benchmark-cases*))
         (ironclad::*prng* (make-instance 'ironclad-browser-prng))
         (*ironclad-benchmark-selection* name)
         (*data-size* data-size)
         (*iterations* iterations)
         (*result* nil))
    (unless (and entry (member name (cdr entry)))
      (error "Unknown Ironclad benchmark: ~S ~S" group name))
    (assert (and (typep data-size '(integer 32768 1048576))
                 (zerop (mod data-size *buffer-size*)) (typep iterations '(integer 1 100))))
    (funcall (symbol-function group))
    (let ((ticks (cdr (assoc name (cdr (first *result*))))))
      (unless (typep ticks '(integer 1 *))
        (error "The benchmark interval was below the timer resolution. Try a larger workload."))
      (format t "IB_RESULT ~D ~D~%" ticks internal-time-units-per-second)))
  (values))

(defun ironclad-benchmark-check ()
  (let* ((message (ironclad:hex-string-to-byte-array "616263"))
         (hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 message)))
         (key (ironclad:hex-string-to-byte-array "000102030405060708090a0b0c0d0e0f"))
         (block (ironclad:hex-string-to-byte-array "00112233445566778899aabbccddeeff")))
    (assert (string-equal hash "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (ironclad:encrypt-in-place (ironclad:make-cipher :aes :key key :mode :ecb) block)
    (assert (string-equal (ironclad:byte-array-to-hex-string block) "69c4e0d86a7b0430d8cdb78070b4c55a")))
  (values))
