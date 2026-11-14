(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-search-regions
  (dolist (pattern (list nil '(0 1) #(0 1) #*01))
    (dolist (source (list nil '(1 0 1 0 1) #(1 0 1 0 1) #*10101))
      (loop for first1 from 0 to (length pattern) do
        (loop for stop1 from first1 to (length pattern) do
          (loop for first2 from 0 to (length source) do
            (loop for stop2 from first2 to (length source) do
              (dolist (reverse '(nil t))
                (dolist (negate '(nil t))
                  (let ((expected (apply #'search pattern source
                                    :start1 first1 :end1 stop1 :start2 first2 :end2 stop2
                                    :from-end reverse (list (if negate :test-not :test) #'eql))))
                    (is (eql expected
                             (miricl-runtime:search-sequence-value pattern source #'eql
                               (vector nil first1 stop1 first2 stop2
                                       (+ (if negate 1 0) (if reverse 2 0)))))))))))))))
  (dolist (reverse '(nil t))
    (is (eql (search '(1 2) '(2 3 1 2 3) :test #'< :from-end reverse)
             (miricl-runtime:search-sequence-value '(1 2) '(2 3 1 2 3) #'<
               (vector nil 0 2 0 5 (if reverse 2 0))))))
  (let ((seen nil))
    (is (eql 1 (miricl-runtime:search-sequence-value '((1) (2)) #((0) (1) (2)) #'eql
                 (vector (lambda (x) (push x seen) (values (car x) :extra)) 0 2 0 3 0))))
    (is (equal '((1) (0) (1) (1) (2) (2)) (reverse seen))))
  (let ((source (list 2)))
    (is (eql 0 (miricl-runtime:search-sequence-value '(1) source
                 (lambda (left right) (declare (ignore left)) (= right 2))
                 (vector (lambda (x) (setf (car source) 99) x) 0 1 0 1 0)))))
  (is (equal '(0) (multiple-value-list
                  (miricl-runtime:search-sequence-value '(1) '(2)
                    (lambda (&rest args) (declare (ignore args)) (values t :extra))
                    (vector nil 0 1 0 1 0)))))
  (is (eql 1 (miricl-runtime:search-sequence-value "λ🙂" "xλ🙂y" #'char=
               (vector nil 0 2 0 4 0)))))

(test runtime-kernel-search-mutation-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (dolist (source '("(search '(1) '(1 . :bad))"
                        "(let ((items (list 1 2))) (setf (cddr items) items) (search '(42) items))"
                        "(let ((items (list 1 2 3))) (search '(1 3) items :test (lambda (a b) (setf (cdr items) :bad) (= a b))))"
                        "(let ((args (list '(1) '(1 2) :from-end t))) (setf (cddddr args) args) (apply #'search args))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (eq t (m:evaluate-string vm
                  "(let ((items (list 1 2 3)))
                     (null (search '(99) items :test (lambda (a b) (setf (cdr items) items) (= a b)))))")))
      (is (= 1 (m:evaluate-string vm
                 "(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))))
                    (search '(2 3) items :key (lambda (x) (setf (fill-pointer items) 0) x)))")))
      (is (equal '(0) (m:evaluate-string vm
                       "(multiple-value-list (search '(1) '(2) :test (lambda (a b) (declare (ignore a b)) (values t :extra))))")))
      (is (= 0 (m:evaluate-string vm
                 "(let ((items (list 2)))
                    (search '(1) items :key (lambda (x) (setf (car items) 99) x)
                            :test (lambda (left right) (declare (ignore left)) (= right 2))))")))
      (is (equal '(1 ((1 2) (1 3)) t) (m:evaluate-string vm
                  "(let ((saved nil))
                     (let ((result (search '(1) '(2 3) :from-end t
                                     :test (lambda (&rest args) (push args saved) t))))
                       (list result (reverse saved) (not (eq (first saved) (second saved))))))")))
      (is (= 1 (m:evaluate-string vm "(search '(2 3) '(1 2 3 4))"))))))
