(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'find-sequence-value)) miricl-port-bench:*cases*)
       '(("find/list-hit" find-sequence-value (list 7 '(1 2 3 4 5 6 7 8) #'eql (vector nil 0 8 0)) :calls 100000)
         ("find/list-miss" find-sequence-value (list 99 '(1 2 3 4 5 6 7 8) #'eql (vector nil 0 8 0)) :calls 100000)
         ("find/list-reverse" find-sequence-value (list 2 '(1 2 3 4 5 6 7 8) #'eql (vector nil 0 8 8)) :calls 20000)
         ("find/vector-hit" find-sequence-value (list 7 #(1 2 3 4 5 6 7 8) #'eql (vector nil 0 8 0)) :calls 100000)
         ("find/position-if" find-sequence-value (list nil '(0 2 4 5 6) #'oddp (vector nil 0 5 17)) :calls 100000)
         ("find/string-key" find-sequence-value (list 955 "abλxyz" #'eql (vector #'char-code 0 6 0)) :calls 20000)
         ("find/empty" find-sequence-value (list nil nil #'eql (vector nil 0 0 0))))))
