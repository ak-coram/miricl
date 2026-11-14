(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-find-regions
  (dolist (sequence (list '(0 1 2 1 3) #(0 1 2 1 3) #*01101))
    (loop for first from 0 to (length sequence) do
      (loop for stop from first to (length sequence) do
        (dolist (reverse '(nil t))
          (dolist (position '(nil t))
            (dolist (mode '(0 1 2))
              (dolist (negate (if (= mode 0) '(nil t) '(nil)))
                (let* ((function (if (= mode 0) #'eql #'oddp))
                       (flags (+ mode (if negate 4 0) (if reverse 8 0) (if position 16 0)))
                       (operator (if position
                                     (nth mode '(position position-if position-if-not))
                                     (nth mode '(find find-if find-if-not))))
                       (expected (if (= mode 0)
                                     (apply operator 1 sequence :start first :end stop :from-end reverse
                                            (list (if negate :test-not :test) function))
                                     (funcall operator function sequence :start first :end stop :from-end reverse))))
                  (is (eql expected (miricl-runtime:find-sequence-value
                                     1 sequence function (vector nil first stop flags))))))))))))
  (let* ((items (list (list 1) (list 2) (list 3)))
         (found (miricl-runtime:find-sequence-value 2 items #'eql (vector #'car 0 3 0))))
    (is (eq found (second items)))))

(test runtime-kernel-find-callback-mutation-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (is (equal '(0 (3 2 99))
                 (m:evaluate-string vm
                   "(let ((items (list 1 2 3)) (seen nil))
                      (let ((position (position-if
                        (lambda (value) (push value seen) (setf (car items) 99) (= value 99))
                        items :from-end t)))
                        (list position (reverse seen))))")))
      (is (equal '(2 (1 2 3))
                 (m:evaluate-string vm
                   "(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))) (seen nil))
                      (let ((position (position-if
                        (lambda (value) (push value seen) (setf (fill-pointer items) 0) (= value 3)) items)))
                        (list position (reverse seen))))")))
      (dolist (source '("(find 1 '(1 . :bad))"
                        "(let ((items (list 1 2))) (setf (cddr items) items) (find 42 items))"
                        "(let ((items (list 1 2 3)))
                           (find-if (lambda (x) (declare (ignore x)) (setf (cdr items) :bad) nil) items))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (eq t (m:evaluate-string vm
                  "(let ((items (list 1 2 3)))
                     (null (find-if (lambda (x) (declare (ignore x)) (setf (cdr items) items) nil) items)))")))
      (is (equal '(2) (m:evaluate-string vm
                       "(multiple-value-list (position-if (lambda (x) (values (= x 3) :extra)) '(1 2 3)))"))))))
