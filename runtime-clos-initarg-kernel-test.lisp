;;;; Explicit target service model; no host MOP layout assumptions.
(in-package #:miricl-test)
(in-suite :miricl)

(defvar *clos-initarg-test-methods* nil)
(defvar *clos-initarg-test-calls* nil)
(defvar *clos-initarg-test-hook* nil)
(defun clos-initarg-test-metadata (selector object extra)
  (case selector
    (0 (svref object 0))
    (1 (setf (svref object 0) t))
    (3 (svref object 1))
    (5 (when *clos-initarg-test-hook* (funcall *clos-initarg-test-hook* selector object extra))
       (svref object 0))
    (9 (or (miricl-runtime:list-length-or-cycle object)
           (error 'type-error :datum object :expected-type 'list)))
    (10 (let ((size (clos-initarg-test-metadata 9 object nil)))
          (if (oddp size) (error 'program-error) size)))
    (11 (svref object 0))
    (12 (error 'type-error :datum object :expected-type 'list))
    (15 object)
    (16 (push (cons object extra) *clos-initarg-test-calls*)
        (when *clos-initarg-test-hook* (funcall *clos-initarg-test-hook* selector object extra))
        (cdr (assoc object *clos-initarg-test-methods*)))
    (17 (when *clos-initarg-test-hook* (funcall *clos-initarg-test-hook* selector object extra))
        (svref object 0))
    (18 (intern (symbol-name object) :keyword))
    (19 (unless (or (cdr extra) (getf object :allow-other-keys))
          (loop for scan on object by #'cddr for key = (car scan) do
            (unless (or (eq key :allow-other-keys) (member key (car extra)))
              (error 'program-error)))) nil)
    (20 (unless (typep object '(simple-vector 3))
          (error 'type-error :datum object :expected-type '(simple-vector 3))))
    (21 (symbolp object))
    (22 (unless (typep object '(simple-vector 4))
          (error 'type-error :datum object :expected-type '(simple-vector 4))) object)
    (23 (when (svref object 2) (error 'error)) :metaclass)
    (24 (vector object :unbound))
    (25 (svref object 3))
    (26 (list :funcallable object))
    (27 (and (symbolp object) object (not (eq object t)) (not (keywordp object))))
    (otherwise (error 'program-error))))

(test clos-initarg-slot-and-method-keys
  (let* ((class (vector t (list (vector '(:a :alias)) (vector '(non-keyword)))))
         (object (vector class))
         (method (vector '(object &key method-key ((:explicit local) nil supplied))))
         (*clos-initarg-test-methods* (list (cons 'initialize-instance (list method))))
         (*clos-initarg-test-calls* nil)
         (state (vector '(initialize-instance) t nil))
         (arguments (list :a nil :alias 2 'non-keyword 3 :method-key 4 :explicit 5)))
    (is (equal '(nil) (multiple-value-list
          (miricl-runtime:clos-check-initargs-value object arguments state #'clos-initarg-test-metadata))))
    (is (equal '(:a nil :alias 2 non-keyword 3 :method-key 4 :explicit 5) arguments))
    (signals program-error
      (miricl-runtime:clos-check-initargs-value object '(:unknown t) state #'clos-initarg-test-metadata))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:unknown t :allow-other-keys t)
                    state #'clos-initarg-test-metadata)))
    ;; A changed result with the same method and same lambda-list spine must
    ;; change validity immediately; there is no identity-only profile cache.
    (setf (third (svref method 0)) 'changed)
    (is (null (miricl-runtime:clos-check-initargs-value object '(:changed 7 :explicit 8)
                    state #'clos-initarg-test-metadata)))
    (signals program-error
      (miricl-runtime:clos-check-initargs-value object '(:method-key 7) state #'clos-initarg-test-metadata))
    (setf (svref method 0) '(object &key &allow-other-keys))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:unknown t) state #'clos-initarg-test-metadata)))))

(test clos-initarg-applicable-arguments-and-effects
  (let* ((class (vector t nil)) (object (vector class)) (previous (vector :previous))
         (selection (list 'slot)) (arguments (list :allow-other-keys t))
         (*clos-initarg-test-calls* nil)
         (names '(allocate-instance initialize-instance shared-initialize update-instance-for-different-class)))
    (is (null (miricl-runtime:clos-check-initargs-value object arguments
          (vector names selection previous) #'clos-initarg-test-metadata)))
    (let ((calls (reverse *clos-initarg-test-calls*)))
      (is (equal names (mapcar #'car calls)))
      (is (eq class (second (first calls))))
      (is (eq object (second (second calls))))
      (is (eq selection (third (third calls))))
      (is (eq previous (second (fourth calls))))
      (is (eq object (third (fourth calls))))
      (is (eq arguments (cddr (first calls))))
      (is (eq arguments (cdddr (third calls))))))
  (let* ((class (vector t nil)) (object (vector class))
         (names (list 'initialize-instance 'shared-initialize))
         (methods (list (vector '(object &key a)) (vector '(object &key b))))
         (*clos-initarg-test-methods* (list (cons 'initialize-instance methods)))
         (*clos-initarg-test-calls* nil)
         (*clos-initarg-test-hook*
           (lambda (selector object extra)
             (declare (ignore object extra))
             (if (= selector 17) (setf (cdr methods) methods) (setf (cdr names) names)))))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:a 1 :b 2)
          (vector names nil nil) #'clos-initarg-test-metadata))))
  (let* ((class (vector t nil)) (object (vector class)) (arguments (list :a 1))
         (*clos-initarg-test-hook* (lambda (&rest ignored) (declare (ignore ignored))
                                    (setf (cddr arguments) arguments))))
    (signals type-error
      (miricl-runtime:clos-check-initargs-value object arguments
        (vector '(initialize-instance shared-initialize) nil nil) #'clos-initarg-test-metadata))))

(test clos-initarg-class-change-during-metadata
  (let* ((after (vector t nil)) (before (vector t (list (vector '(:a)))))
         (object (vector before)) (*clos-initarg-test-calls* nil)
         (*clos-initarg-test-hook* (lambda (selector slot extra)
           (declare (ignore slot extra))
           (when (= selector 5) (setf (svref object 0) after)))))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:a 1)
          (vector '(allocate-instance) nil nil) #'clos-initarg-test-metadata)))
    (is (eq after (second (first *clos-initarg-test-calls*))))))

(test clos-initarg-malformed-input-and-recovery
  (let* ((class (vector t (list (vector '(:a))))) (object (vector class))
         (state (vector nil nil nil)))
    (dolist (bad-state '(nil #(nil) #(nil nil nil nil) (nil nil nil)))
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil bad-state #'clos-initarg-test-metadata)))
    (dolist (bad '(7 (:a . 1) (:a)))
      (signals error
        (miricl-runtime:clos-check-initargs-value object bad state #'clos-initarg-test-metadata)))
    (let ((cycle (list :a 1)))
      (setf (cddr cycle) cycle)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object cycle state #'clos-initarg-test-metadata))
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil (vector cycle nil nil) #'clos-initarg-test-metadata)))
    (dolist (lambda-list '((object &key ()) (object &key (7)) (object &key (((:x var)) nil))
                          (object &key ((:x . var) nil)) (object &key (:x nil supplied extra)) (object &key (x nil 7))))
      (let ((*clos-initarg-test-methods*
              (list (cons 'initialize-instance (list (vector lambda-list))))))
        (signals error
          (miricl-runtime:clos-check-initargs-value object nil
            (vector '(initialize-instance) nil nil) #'clos-initarg-test-metadata))))
    (let* ((cycle (list 'object '&key 'x))
           (*clos-initarg-test-methods* (list (cons 'initialize-instance (list (vector cycle))))))
      (setf (cdddr cycle) cycle)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil
          (vector '(initialize-instance) nil nil) #'clos-initarg-test-metadata)))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:a 1) state #'clos-initarg-test-metadata)))))

(test clos-standard-allocation-policy
  (dolist (funcallable '(nil t))
    (let* ((class (vector nil nil nil funcallable))
           (result (multiple-value-list
             (miricl-runtime:clos-allocate-instance-value class #'clos-initarg-test-metadata)))
           (object (if funcallable (second (first result)) (first result))))
      (is (svref class 0))
      (is (= 1 (length result)))
      (is (eq class (svref object 0)))
      (is (eq :unbound (svref object 1)))))
  (signals error
    (miricl-runtime:clos-allocate-instance-value (vector nil nil t nil) #'clos-initarg-test-metadata))
  (signals type-error
    (miricl-runtime:clos-allocate-instance-value 7 #'clos-initarg-test-metadata)))

(test clos-initarg-malformed-metadata-spines
  (let* ((slot (vector (list :a))) (class (vector t (list slot))) (object (vector class))
         (state (vector '(initialize-instance) nil nil)))
    (let ((saved (svref class 1)) (cycle (list slot)))
      (setf (cdr cycle) cycle (svref class 1) cycle)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil state #'clos-initarg-test-metadata))
      (setf (svref class 1) saved))
    (dolist (keys '(7 (:a . bad)))
      (setf (svref slot 0) keys)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil state #'clos-initarg-test-metadata)))
    (let ((cycle (list :a)))
      (setf (cdr cycle) cycle (svref slot 0) cycle)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil state #'clos-initarg-test-metadata)))
    (setf (svref slot 0) '(:a))
    (let* ((cycle (list (vector '(object &key a))))
           (*clos-initarg-test-methods* (list (cons 'initialize-instance cycle))))
      (setf (cdr cycle) cycle)
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil state #'clos-initarg-test-metadata)))
    (let ((*clos-initarg-test-methods* (list (cons 'initialize-instance (list (vector 7))))))
      (signals type-error
        (miricl-runtime:clos-check-initargs-value object nil state #'clos-initarg-test-metadata)))
    (is (null (miricl-runtime:clos-check-initargs-value object '(:a 1) state #'clos-initarg-test-metadata)))))

(test clos-allocation-protocol-order
  (let* ((events nil) (class (vector nil nil nil t))
         (metadata (lambda (selector object extra)
           (push selector events)
           (when (= selector 25) (is (eq :metaclass extra)))
           (clos-initarg-test-metadata selector object extra))))
    (miricl-runtime:clos-allocate-instance-value class metadata)
    (is (equal '(22 0 1 23 24 25 26) (reverse events)))))
