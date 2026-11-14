(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'reduce-sequence-value)) miricl-port-bench:*cases*)
       '(("reduce/list" reduce-sequence-value (list #'+ '(1 2 3 4 5 6 7 8) nil (vector 0 8 nil 0)) :calls 100000)
         ("reduce/list-reverse" reduce-sequence-value (list #'- '(1 2 3 4 5 6 7 8) nil (vector 0 8 nil 1)) :calls 20000)
         ("reduce/vector" reduce-sequence-value (list #'+ #(1 2 3 4 5 6 7 8) nil (vector 0 8 nil 0)) :calls 100000)
         ("reduce/key" reduce-sequence-value (list #'+ '(1 2 3 4 5 6 7 8) #'1+ (vector 0 8 10 2)) :calls 100000)
         ("reduce/single" reduce-sequence-value (list #'+ '(7) nil (vector 0 1 nil 0)))
         ("reduce/empty-call" reduce-sequence-value (list #'+ nil nil (vector 0 0 nil 0)))
         ("reduce/empty-initial" reduce-sequence-value (list #'+ nil nil (vector 0 0 17 2))))))
