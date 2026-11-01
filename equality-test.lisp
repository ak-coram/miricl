;;;; Structural equality, including long flat lists and aggregate objects.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-object-equality
  (m:with-interpreter (vm)
    (let ((objects '(nil t a b 0 1 1.0 1.0d0 1/2 #c(1 2) #c(1.0 2.0)
                     #\a #\A #\λ #\Λ "" "abc" "ABC" "aBc" "λ" "Λ"
                     (1 . 2) (1.0 . 2.0) ("a" #\λ) ("A" #\Λ) (1 2 3) (1 2))))
      (dolist (operator '(equal equalp))
        (dolist (a objects)
          (dolist (b objects)
            (is (eql (not (null (funcall operator a b)))
                     (m:evaluate vm `(not (null (,operator ',a ',b))))))))))
    (dolist (operator '(equal equalp))
      (dolist (source '("(let ((a (make-list 20000 :initial-element 1)))
                          (list (TEST a (copy-list a)) (TEST a (cons 2 (cdr a)))
                            (TEST (cons 2 (cdr a)) a)))"
                        "(let ((a (make-list 12000 :initial-element 1))
                               (b (make-list 12000 :initial-element 1)))
                           (setf (cdr (last a)) 2 (cdr (last b)) 3)
                           (TEST a b))"))
        (let* ((*package* (find-package :miricl-test)) (form (read-from-string source)))
          (setf form (sublis (list (cons 'test operator)) form))
          (is (equal (eval form) (m:evaluate vm form))))))
    (is (equal '(t nil t 1) (m:evaluate vm
      '(list (equalp 0.0 -0.0) (equal 0.0 -0.0) (equalp 1 1.0)
             (length (multiple-value-list (equalp (values 1 2) 1.0)))))))
    (is (eq t (m:evaluate vm
      '(let ((saved #'equalp))
         (unwind-protect
           (progn (setf (symbol-function 'equalp) (lambda (a b) (declare (ignore a b)) nil))
                  (funcall saved '(1 "a") '(1.0 "A")))
           (setf (symbol-function 'equalp) saved))))))
    (dolist (source '("(equalp)" "(equalp 1)" "(equalp 1 2 3)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))))

(test equalp-structures-and-instances
  (m:with-interpreter (vm)
    (clos-eval vm "(defstruct equality-record value next)
      (defstruct equality-empty)
      (defstruct (equality-child (:include equality-record)) extra)
      (defstruct equality-other value next)
      (defclass equality-instance () ((value :initarg :value)))")
    (dolist (source '("(equalp (make-equality-record :value '(1 \"a\"))
                             (make-equality-record :value '(1.0 \"A\")))"
                      "(equalp (make-equality-empty) (make-equality-empty))"
                      "(let ((a (make-equality-record :value 3))) (equalp a (copy-structure a)))"
                      "(equalp (make-equality-child :value 3 :extra \"a\")
                               (make-equality-child :value 3.0 :extra \"A\"))"))
      (is (eq t (clos-eval vm source))))
    (dolist (source '("(equalp (make-equality-record :value 1) (make-equality-record :value 2))"
                      "(equalp (make-equality-child) (make-equality-record))"
                      "(equalp (make-equality-record) (make-equality-other))"
                      "(equal (make-equality-empty) (make-equality-empty))"
                      "(equalp (make-instance 'equality-instance :value 1)
                               (make-instance 'equality-instance :value 1))"))
      (is (null (clos-eval vm source))))
    (is (eq t (clos-eval vm
      "(let ((a (make-instance 'equality-instance :value 1))) (equalp a a))")))))

(test equalp-hash-tables
  (m:with-interpreter (vm)
    (dolist (test '(eq eql equal equalp))
      (dolist (other-test '(eq eql equal equalp))
        (dolist (reverse '(nil t))
          (dolist (value '(nil (1 "a") (2 "a")))
            (let ((form `(let ((a (make-hash-table :test ',test))
                              (b (make-hash-table :test ',other-test)))
                          (setf (gethash 'first a) '(1 "A") (gethash 'second a) nil)
                          (if ,reverse
                            (setf (gethash 'second b) nil (gethash 'first b) ',value)
                            (setf (gethash 'first b) ',value (gethash 'second b) nil))
                          (list (equalp a b) (equalp b a) (equal a b)
                            (hash-table-count a) (hash-table-count b)))))
              (is (equal (eval form) (m:evaluate vm form))))))))
    (dolist (form '((let ((a (make-hash-table)) (b (make-hash-table)))
                     (setf (gethash 1 a) nil (gethash 2 b) nil) (equalp a b))
                    (let ((a (make-hash-table)) (b (make-hash-table)))
                     (setf (gethash 1 a) nil) (equalp a b))
                    (let ((a (make-hash-table :test 'equalp)) (b (make-hash-table :test 'equalp)))
                     (setf (gethash '(1 "a") a) '(#\λ)
                           (gethash '(1.0 "A") b) '(#\Λ)) (equalp a b))
                    (let ((a (make-hash-table :test 'equalp)))
                     (setf (gethash '(1 "a") a) 7) (gethash '(1.0 "A") a))
                    (let ((a (make-hash-table)) (b (make-hash-table)))
                     (setf (gethash 1 a) (make-hash-table :test 'equalp)
                           (gethash 1 b) (make-hash-table :test 'equalp))
                     (equalp a b))))
      (is (equal (eval form) (m:evaluate vm form))))))
