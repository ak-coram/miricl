(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'search-sequence-value)) miricl-port-bench:*cases*)
       '(("search/list-hit" search-sequence-value (list '(1 2) '(0 1 2 1 2 3) #'eql (vector nil 0 2 0 6 0)) :calls 100000)
         ("search/list-miss" search-sequence-value (list '(1 9) '(0 1 2 1 2 3) #'eql (vector nil 0 2 0 6 0)) :calls 100000)
         ("search/list-rightmost" search-sequence-value (list '(1 2) '(0 1 2 1 2 3) #'eql (vector nil 0 2 0 6 2)) :calls 100000)
         ("search/vector" search-sequence-value (list #(1 2) #(0 1 2 1 2 3) #'eql (vector nil 0 2 0 6 0)) :calls 100000)
         ("search/mixed" search-sequence-value (list '(1 2) #(0 1 2 1 2 3) #'eql (vector nil 0 2 0 6 0)) :calls 100000)
         ("search/key" search-sequence-value (list '((1) (2)) #((0) (1) (2) (1) (2) (3)) #'eql (vector #'car 0 2 0 6 0)) :calls 100000)
         ("search/string-key" search-sequence-value (list "λ🙂" "abλ🙂xy" #'eql (vector #'char-code 0 2 0 6 0)) :calls 20000)
         ("search/negated" search-sequence-value (list '(1 2) '(1 2 3 4 5 6) #'eql (vector nil 0 2 0 6 1)) :calls 100000)
         ("search/empty" search-sequence-value (list nil '(1 2 3) #'eql (vector nil 0 0 0 3 0)))
         ("search/too-short" search-sequence-value (list '(1 2 3) '(1 2) #'eql (vector nil 0 3 0 2 0))))))
