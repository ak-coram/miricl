(in-package :miricl-compiler)

(defvar *soundness-named-scope* :outside)

(let ((*compile-evaluations* t))
  (eval '(defun soundness-named-frame (n input)
           (declare (optimize (debug 1) (safety 1)))
           (if (< n 1) (+ input 0)
               (+ 1 (soundness-named-frame (- n 1) input)))))
  (eval '(defun soundness-named-values (n mode)
           (declare (optimize (debug 1) (safety 1)))
           (cond ((< n 1) (if (eq mode :empty) (values) (values 7 :second)))
                 ((eq mode :primary) (+ 10 (soundness-named-values (- n 1) :two)))
                 (t (multiple-value-call #'values (soundness-named-values (- n 1) mode))))))
  (eval '(defun soundness-named-shadow (n)
           (declare (optimize (debug 1) (safety 1)))
           (cond ((< n 1)
                  (flet ((soundness-named-shadow (x) (+ x 20)))
                    (+ 1 (soundness-named-shadow n))))
                 ((< n 2)
                  (macrolet ((soundness-named-shadow (x) (list '+ x 30)))
                    (+ 1 (soundness-named-shadow n))))
                 (t (+ 1 (soundness-named-shadow (- n 2)))))))
  (eval '(defun soundness-named-notinline (n)
           (declare (notinline soundness-named-notinline)
                    (optimize (debug 1) (safety 1)))
           (if (< n 1) 1 (+ 1 (soundness-named-notinline (- n 1))))))
  (eval '(defun soundness-named-indirect (n direct)
           (declare (optimize (debug 1) (safety 1)))
           (cond ((< n 0) #'soundness-named-indirect)
                 ((< n 1) 1)
                 (direct (+ 1 (soundness-named-indirect (- n 1) direct)))
                 (t (+ 1 (funcall #'soundness-named-indirect (- n 1) direct))))))
  ;; Use a fresh name because PROCLAIM affects subsequent compilations.
  (let ((name (gensym "SOUNDNESS-NAMED-PROCLAIMED-")))
    (proclaim (list 'notinline name))
    (eval `(defun ,name (n)
             (declare (optimize (debug 1) (safety 1)))
             (if (< n 1) 1 (+ 1 (,name (- n 1))))))
    (unwind-protect
        (let ((retained (fdefinition name)))
          (assert (compiled-function-p retained))
          ;; The old object is inactive when the global binding changes.
          (setf (fdefinition name) (lambda (n) (+ n 40)))
          (assert (= (funcall retained 1) 41)))
      (fmakunbound name))))

(let ((retained #'soundness-named-notinline))
  (unwind-protect
      (progn
        (assert (compiled-function-p retained))
        (setf (fdefinition 'soundness-named-notinline) (lambda (n) (+ n 40)))
        (assert (= (funcall retained 1) 41)))
    (fmakunbound 'soundness-named-notinline)))

;; Exercise both call forms under the original binding. An escaping FUNCTION
;; value observes the current definition after the inactive entry is replaced.
(let ((retained #'soundness-named-indirect))
  (unwind-protect
      (progn
        (assert (compiled-function-p retained))
        (assert (= (funcall retained 1 t) 2))
        (assert (= (funcall retained 1 nil) 2))
        (let ((replacement (lambda (n direct) (declare (ignore direct)) (+ n 40))))
          (setf (fdefinition 'soundness-named-indirect) replacement)
          (assert (eq (funcall retained -1 nil) replacement))))
    (fmakunbound 'soundness-named-indirect)))

(let ((entry (compile-native
               '(lambda (n)
                  (declare (optimize (debug 1) (safety 1)))
                  (if (< n 1) 1 (+ 1 (soundness-named-diagnostic (- n 1)))))
               :name 'soundness-named-diagnostic)))
  (unwind-protect
      (progn
        (setf (fdefinition 'soundness-named-diagnostic) (lambda (n) (+ n 40)))
        (assert (= (funcall entry 1) 41)))
    (fmakunbound 'soundness-named-diagnostic)))

(assert (every #'compiled-function-p
               (list #'soundness-named-frame #'soundness-named-values #'soundness-named-shadow)))
(assert (= (soundness-named-shadow 2) 22))
(assert (= (soundness-named-shadow 3) 33))
(assert (equal (multiple-value-list (soundness-named-values 5 :two)) '(7 :second)))
(assert (null (multiple-value-list (soundness-named-values 5 :empty))))
(assert (equal (multiple-value-list (soundness-named-values 5 :primary)) '(17)))

(let ((entry #'soundness-named-frame) (cleanup nil))
  (assert (handler-case (progn (funcall entry 5) nil) (program-error () t)))
  (assert (equal (catch 'soundness-named-exit
                   (let ((*soundness-named-scope* :inside))
                     (unwind-protect
                         (handler-bind ((type-error
                                          (lambda (condition)
                                            (throw 'soundness-named-exit
                                              (list (type-error-datum condition)
                                                *soundness-named-scope*
                                                (funcall entry 3 8))))))
                           (funcall entry 5 :invalid))
                       (push *soundness-named-scope* cleanup))))
                 '(:invalid :inside 11)))
  (assert (equal cleanup '(:inside)))
  (assert (eq *soundness-named-scope* :outside))
  (assert (= (funcall entry 5 0) 5)))
