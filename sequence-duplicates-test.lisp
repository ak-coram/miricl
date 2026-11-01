;;;; Duplicate selection, direction, bounded traversal, and key identity.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-sequence-duplicates
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (sequence '(nil (1) (1 2 1 3 2) (1 1.0 2 2.0 1d0) "aAλΛa" "abcab"))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence) do
          (dolist (from-end '(nil t))
            (dolist (operator '(remove-duplicates delete-duplicates))
              (dolist (test '(eql equal equalp))
                (dolist (keyword '(:test :test-not))
                  (let ((form `(,operator (copy-seq ',sequence) :start ,start :end ,end
                                  :from-end ,from-end ,keyword (function ,test))))
                    (is (equal (eval (cons 'reference-remove-duplicates (cdr form)))
                               (m:evaluate vm form)) "Mismatch for ~S" form)))))))))
    (dolist (fixture '((car ((1 a) (2 b) (1 c) (3 d)))
                       (cdr ((a . 1) (b . 2) (c . 1) (d . 3)))
                       (1+ (1 2 1 3 2)) (identity (1 2 1 3 2))))
      (destructuring-bind (key sequence) fixture
        (dolist (from-end '(nil t))
          (dolist (operator '(remove-duplicates delete-duplicates))
            (dolist (test '(= < > <= >=))
              (dolist (keyword '(:test :test-not))
                (let ((form `(,operator (copy-seq ',sequence) :key (function ,key)
                                :from-end ,from-end ,keyword (function ,test))))
                  (is (equal (eval (cons 'reference-remove-duplicates (cdr form)))
                             (m:evaluate vm form)) "Mismatch for ~S" form))))))))
    (dolist (operator '(remove-duplicates delete-duplicates))
      (dolist (from-end '(nil t))
        (dolist (test '(char= char-equal))
          (let ((form `(,operator (copy-seq "aAλΛ🙂a") :test (function ,test) :from-end ,from-end)))
            (is (equal (eval form) (m:evaluate vm form)))))))
    (dolist (form '((let ((x (list 1 2 1 3 2)))
                     (list (remove-duplicates x) x))
                    (let* ((a (list 1)) (b (list 1)) (x (list a b a)))
                      (let ((out (remove-duplicates x :test 'eq)))
                        (list (eq (car out) b) (eq (cadr out) a) (length out) (length x))))
                    (let* ((s (copy-seq "aAλΛa")) (out (remove-duplicates s :test 'char-equal)))
                      (setf (char out 0) #\z) (list s out))
                    (multiple-value-list (remove-duplicates (values '(1 2 1) 99)))
                    (remove-duplicates '(1 2 1) :key nil :key 'car :from-end nil :from-end t)
                    (remove-duplicates '(1 2 1) :start 0 :start 2 :end nil :end 0)
                    (remove-duplicates '(1 2 1) :unknown t :allow-other-keys t :allow-other-keys nil)
                    (remove-duplicates '(1 2 1) :count 1 :allow-other-keys t)
                    (let ((seen nil))
                      (list (remove-duplicates (progn (push 1 seen) '(1 2 1))
                              :from-end (progn (push 2 seen) t)
                              :key (progn (push 3 seen) 'identity)) seen))))
      (is (equal (eval form) (m:evaluate vm form))))
    (is (equal '((1 2) :changed) (m:evaluate vm
      '(let ((saved #'remove-duplicates))
         (unwind-protect
           (progn (setf (symbol-function 'remove-duplicates) (lambda (&rest x) (declare (ignore x)) :changed))
                  (list (funcall saved '(1 1 2)) (remove-duplicates '(1 1 2))))
           (setf (symbol-function 'remove-duplicates) saved))))))))

(test native-duplicate-errors
  (m:with-interpreter (vm)
    (dolist (source '("(remove-duplicates)" "(delete-duplicates)" "(remove-duplicates nil :start)"
                      "(remove-duplicates nil :count 1)" "(remove-duplicates nil :bad t)"
                      "(remove-duplicates nil :bad t :allow-other-keys nil :allow-other-keys t)"
                      "(remove-duplicates '(1 2) :test 'identity)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(remove-duplicates 1)" "(remove-duplicates '(1 . 2))"
                      "(let ((x (list 1))) (setf (cdr x) x) (delete-duplicates x))"
                      "(remove-duplicates '(1 2) :start nil)" "(remove-duplicates '(1 2) :start -1)"
                      "(remove-duplicates '(1 2) :start 2 :end 1)" "(remove-duplicates '(1 2) :end 3)"
                      "(remove-duplicates '(1 2) :key 'car)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
