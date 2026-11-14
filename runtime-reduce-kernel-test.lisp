(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-reduce-regions
  (dolist (sequence (list '(1 2 3 4) #(1 2 3 4) #*0110 "abλ" nil))
    (loop for first from 0 to (length sequence) do
      (loop for stop from first to (length sequence) do
        (dolist (reverse '(nil t))
          (dolist (initialp '(nil t))
            (let* ((initial :initial)
                   (flags (+ (if reverse 1 0) (if initialp 2 0)))
                   (expected (apply #'reduce #'list sequence :start first :end stop :from-end reverse
                                    (if initialp (list :initial-value initial) nil))))
              (is (equal expected
                         (miricl-runtime:reduce-sequence-value #'list sequence nil
                           (vector first stop initial flags))))))))))
  (let ((object (list :shared)))
    (is (eq object (miricl-runtime:reduce-sequence-value #'list (list object) nil (vector 0 1 nil 0))))
    (is (eq object (miricl-runtime:reduce-sequence-value #'list nil nil (vector 0 0 object 2)))))
  (let ((calls 0) (key-calls 0))
    (is (null (miricl-runtime:reduce-sequence-value
                (lambda (&rest values) (declare (ignore values)) (incf calls)) nil
                (lambda (x) (incf key-calls) x) (vector 0 0 nil 2))))
    (is (= calls 0)) (is (= key-calls 0)))
  (is (equal '(17) (multiple-value-list
                    (miricl-runtime:reduce-sequence-value (lambda () (values 17 :extra)) nil nil (vector 0 0 nil 0)))))
  (let ((seen nil))
    (is (= 16 (miricl-runtime:reduce-sequence-value #'+ '(1 2 3) (lambda (x) (push x seen) (values x :extra))
                (vector 0 3 10 3))))
    (is (equal '(3 2 1) (reverse seen)))))

(test runtime-kernel-reduce-callback-boundaries
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (is (equal '(104 ((2 3) (99 5)))
                 (m:evaluate-string vm
                   "(let ((items (list 1 2 3)) (seen nil))
                      (let ((result (reduce (lambda (a b) (push (list a b) seen)
                                                       (setf (car items) 99) (+ a b))
                                            items :from-end t)))
                        (list result (reverse seen))))")))
      (is (equal '(6 (1 2 3))
                 (m:evaluate-string vm
                   "(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))) (seen nil))
                      (let ((result (reduce #'+ items :key (lambda (x) (push x seen)
                                                           (setf (fill-pointer items) 0) x))))
                        (list result (reverse seen))))")))
      (dolist (source '("(reduce #'+ '(1 . :bad))"
                        "(let ((items (list 1 2))) (setf (cddr items) items) (reduce #'+ items))"
                        "(let ((args (list #'+ '(1 2) :from-end t)))
                           (setf (cddddr args) args) (apply #'reduce args))"
                        "(let ((items (list 1 2 3)))
                           (reduce #'+ items :key (lambda (x) (setf (cdr items) :bad) x)))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (= 3 (m:evaluate-string vm
                 "(let ((items (list 1 2 3)))
                    (reduce #'+ items :key (lambda (x) (setf (cdr items) items) x)))")))
      (is (equal '(17) (m:evaluate-string vm
                        "(multiple-value-list (reduce (lambda () (values 17 :extra)) nil))")))
      (is (equal '(6) (m:evaluate-string vm
                       "(multiple-value-list (reduce (lambda (a b) (values (+ a b) :extra)) '(1 2 3)))"))))))
