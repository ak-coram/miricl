(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-mismatch-regions
  (dolist (a (list nil '(0 1 2) #(0 1 2) #*011))
    (dolist (b (list nil '(1 1 2 3) #(1 1 2 3) #*1101))
      (loop for first1 from 0 to (length a) do
        (loop for stop1 from first1 to (length a) do
          (loop for first2 from 0 to (length b) do
            (loop for stop2 from first2 to (length b) do
              (dolist (reverse '(nil t))
                (dolist (negate '(nil t))
                  (is (eql (apply #'mismatch a b :start1 first1 :end1 stop1
                                  :start2 first2 :end2 stop2 :from-end reverse
                                  (list (if negate :test-not :test) #'eql))
                           (miricl-runtime:mismatch-sequence-value a b #'eql
                             (vector nil first1 stop1 first2 stop2
                                     (+ (if negate 1 0) (if reverse 2 0))))))))))))))
  (dolist (reverse '(nil t))
    (is (eql (mismatch '(1 2) '(2 3 1 2 3) :test #'< :from-end reverse)
             (miricl-runtime:mismatch-sequence-value '(1 2) '(2 3 1 2 3) #'<
               (vector nil 0 2 0 5 (if reverse 2 0))))))
  (let ((seen nil))
    (is (eql 1 (miricl-runtime:mismatch-sequence-value '((1) (9)) #((1) (2)) #'eql
                 (vector (lambda (x) (push x seen) (values (car x) :extra)) 0 2 0 2 0))))
    (is (equal '((1) (1) (9) (2)) (reverse seen))))
  (let ((source (list 2)))
    (is (null (miricl-runtime:mismatch-sequence-value '(1) source
                (lambda (left right) (declare (ignore left)) (= right 2))
                (vector (lambda (x) (setf (car source) 99) x) 0 1 0 1 0)))))
  (is (equal '(nil) (multiple-value-list
                     (miricl-runtime:mismatch-sequence-value '(1) '(2)
                       (lambda (&rest args) (declare (ignore args)) (values t :extra))
                       (vector nil 0 1 0 1 0)))))
  (is (eql 1 (miricl-runtime:mismatch-sequence-value "λ🙂" "λx" #'char=
               (vector nil 0 2 0 2 0)))))

(test runtime-kernel-mismatch-mutation-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (dolist (source '("(mismatch nil '(1 . :bad))"
                        "(let ((items (list 1 2))) (setf (cddr items) items) (mismatch '(42) items))"
                        "(let ((items (list 1 2 3))) (mismatch '(1 2 3) items :test (lambda (a b) (setf (cdr items) :bad) (= a b))))"
                        "(let ((args (list '(1) '(1 2) :from-end t))) (setf (cddddr args) args) (apply #'mismatch args))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (eq t (m:evaluate-string vm
                  "(let ((items (list 1 2 3)))
                     (null (mismatch items '(1 2 3) :test (lambda (a b) (declare (ignore a b)) (setf (cdr items) items) t))))")))
      (is (= 2 (m:evaluate-string vm
                 "(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))))
                    (mismatch items '(1 2 99) :key (lambda (x) (setf (fill-pointer items) 0) x)))")))
      (is (equal '(1 (3 3 2 2 99 0)) (m:evaluate-string vm
                  "(let ((items (list 1 2 3)) (seen nil))
                     (let ((result (mismatch items '(0 2 3) :from-end t
                                     :key (lambda (x) (push x seen) (setf (car items) 99) x))))
                       (list result (reverse seen))))")))
      (is (equal '(nil) (m:evaluate-string vm
                         "(multiple-value-list (mismatch '(1) '(2) :test (lambda (a b) (declare (ignore a b)) (values t :extra))))")))
      (is (eq t (m:evaluate-string vm
                  "(let ((items (list 2)))
                     (null (mismatch '(1) items :key (lambda (x) (setf (car items) 99) x)
                                     :test (lambda (left right) (declare (ignore left)) (= right 2)))))")))
      (is (equal '(nil ((1 2) (3 4)) t) (m:evaluate-string vm
                  "(let ((saved nil))
                     (let ((result (mismatch '(1 3) '(2 4)
                                     :test (lambda (&rest args) (push args saved) t))))
                       (list result (reverse saved) (not (eq (first saved) (second saved))))))")))
      (is (= 1 (m:evaluate-string vm "(mismatch '(1 9) '(1 2))"))))))
