;;;; Load after benchmark-runtime-kernels.lisp in a persistent host REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "list-exclusive/" (first case))) miricl-port-bench:*cases*)
       '(("list-exclusive/disjoint" list-exclusive-values
           (list '(1 2 3 4) '(10 11 12 13) nil (cons #'= nil)) :calls 20000)
         ("list-exclusive/asymmetric" list-exclusive-values
           (list '(1 4 8) '(3 5 7) nil (cons #'< nil)) :calls 20000)
         ("list-exclusive/negated" list-exclusive-values
           (list '(1 4 8) '(3 5 7) nil (cons #'eql #'<)) :calls 20000)
         ("list-exclusive/keyed" list-exclusive-values
           (list '((1 . :a) (2 . :b) (3 . :c)) '((2 . :d) (4 . :e)) #'car (cons #'= nil)) :calls 20000)
         ("list-exclusive/marked-range" list-exclusive-values
           (list '(2 8 33) (loop for i below 65 collect i) nil (cons #'= nil)) :calls 2000)
         ("list-exclusive/empty" list-exclusive-values
           (list nil '(1 2 3 4) nil (cons #'= nil)) :calls 200000))))
