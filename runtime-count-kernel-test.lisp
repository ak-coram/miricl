(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-count-regions
  (dolist (sequence (list '(0 1 2 1 3) #(0 1 2 1 3) #*01101 nil))
    (loop for first from 0 to (length sequence) do
      (loop for stop from first to (length sequence) do
        (dolist (reverse '(nil t))
          (dolist (mode '(0 1 2))
            (dolist (negate (if (= mode 0) '(nil t) '(nil)))
              (let* ((function (if (= mode 0) #'eql #'oddp))
                     (flags (+ mode (if negate 4 0) (if reverse 8 0)))
                     (operator (nth mode '(count count-if count-if-not)))
                     (expected (if (= mode 0)
                                   (apply operator 1 sequence :start first :end stop :from-end reverse
                                          (list (if negate :test-not :test) function))
                                   (funcall operator function sequence :start first :end stop :from-end reverse))))
                (is (= expected (miricl-runtime:count-sequence-value
                                  1 sequence function (vector nil first stop flags)))))))))))
  (let ((seen nil))
    (is (= 2 (miricl-runtime:count-sequence-value
              2 '((1) (2) (3) (4)) #'<
              (vector (lambda (value) (push value seen) (values (car value) :extra)) 0 4 8))))
    (is (equal '((4) (3) (2) (1)) (reverse seen))))
  (is (equal '(0) (multiple-value-list
                  (miricl-runtime:count-sequence-value nil '(1 2 3)
                    (lambda (x) (declare (ignore x)) (values nil :extra)) (vector nil 0 3 1))))))

(test runtime-kernel-count-mutation-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (is (equal '(1 (3 2 99))
                 (m:evaluate-string vm
                  "(let ((items (list 1 2 3)) (seen nil))
                     (let ((count (count-if (lambda (value) (push value seen)
                                                      (setf (car items) 99) (= value 99))
                                             items :from-end t)))
                       (list count (reverse seen))))")))
      (is (equal '(2 (1 2 3))
                 (m:evaluate-string vm
                  "(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))) (seen nil))
                     (let ((count (count-if (lambda (value) (push value seen)
                                                      (setf (fill-pointer items) 0) (oddp value)) items)))
                       (list count (reverse seen))))")))
      (is (equal '(3 ((1) (2) (3)) T)
                 (m:evaluate-string vm
                  "(let ((saved nil))
                     (let ((count (count-if (lambda (&rest arguments) (push arguments saved) t) '(1 2 3))))
                       (list count (reverse saved) (not (eq (first saved) (second saved))))))")))
      (dolist (source '("(count 1 '(1 . :bad))"
                        "(let ((items (list 1 2))) (setf (cddr items) items) (count 42 items))"
                        "(let ((items (list 1 2 3)))
                           (count-if (lambda (x) (declare (ignore x)) (setf (cdr items) :bad) nil) items))"
                        "(let ((args (list 1 '(1 2 3) :from-end t)))
                           (setf (cddddr args) args) (apply #'count args))"))
        (is (eq t (m:evaluate-string vm
                   (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (= 3 (m:evaluate-string vm
                  "(let ((items (list 1 2 3)))
                     (count-if (lambda (x) (declare (ignore x)) (setf (cdr items) items) t) items))")))
      (is (equal '(2) (m:evaluate-string vm
                      "(multiple-value-list (count-if (lambda (x) (values (oddp x) :extra)) '(1 2 3)))")))
      (is (= 2 (m:evaluate-string vm "(count 1 '(1 2 1))"))))))
