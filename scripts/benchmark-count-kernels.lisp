(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'count-sequence-value)) miricl-port-bench:*cases*)
       '(("count/list" count-sequence-value (list 2 '(1 2 3 2 4 2 5 6) #'eql (vector nil 0 8 0)) :calls 100000)
         ("count/list-reverse" count-sequence-value (list 2 '(1 2 3 2 4 2 5 6) #'eql (vector nil 0 8 8)) :calls 20000)
         ("count/vector" count-sequence-value (list 2 #(1 2 3 2 4 2 5 6) #'eql (vector nil 0 8 0)) :calls 100000)
         ("count/if" count-sequence-value (list nil '(1 2 3 4 5 6 7 8) #'oddp (vector nil 0 8 1)) :calls 100000)
         ("count/if-not" count-sequence-value (list nil '(1 2 3 4 5 6 7 8) #'oddp (vector nil 0 8 2)) :calls 100000)
         ("count/key" count-sequence-value (list 2 '((1) (2) (3) (2)) #'eql (vector #'car 0 4 0)) :calls 100000)
         ("count/string-key" count-sequence-value (list 955 "aλbλ🙂λ" #'eql (vector #'char-code 0 6 0)) :calls 20000)
         ("count/empty" count-sequence-value (list nil nil #'eql (vector nil 0 0 0))))))
