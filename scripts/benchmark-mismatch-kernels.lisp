(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'mismatch-sequence-value)) miricl-port-bench:*cases*)
       '(("mismatch/list-equal" mismatch-sequence-value (list '(1 2 3 4) '(1 2 3 4) #'eql (vector nil 0 4 0 4 0)) :calls 100000)
         ("mismatch/list-miss" mismatch-sequence-value (list '(1 2 9 4) '(1 2 3 4) #'eql (vector nil 0 4 0 4 0)) :calls 100000)
         ("mismatch/list-reverse" mismatch-sequence-value (list '(1 2 3 4) '(9 2 3 4) #'eql (vector nil 0 4 0 4 2)) :calls 20000)
         ("mismatch/vector" mismatch-sequence-value (list #(1 2 3 4) #(1 2 3 4) #'eql (vector nil 0 4 0 4 0)) :calls 100000)
         ("mismatch/mixed" mismatch-sequence-value (list '(1 2 3 4) #(1 2 3 4) #'eql (vector nil 0 4 0 4 0)) :calls 100000)
         ("mismatch/key" mismatch-sequence-value (list '((1) (2) (3)) #((1) (2) (3)) #'eql (vector #'car 0 3 0 3 0)) :calls 100000)
         ("mismatch/string-key" mismatch-sequence-value (list "λ🙂xy" "λ🙂xz" #'eql (vector #'char-code 0 4 0 4 0)) :calls 20000)
         ("mismatch/negated" mismatch-sequence-value (list '(1 2 3) '(2 3 4) #'eql (vector nil 0 3 0 3 1)) :calls 100000)
         ("mismatch/empty" mismatch-sequence-value (list nil nil #'eql (vector nil 0 0 0 0 0)))
         ("mismatch/shorter-reverse" mismatch-sequence-value (list '(1 2 3 4) '(3 4) #'eql (vector nil 0 4 0 2 2)) :calls 20000))))
