;;;; Load after benchmark-runtime-kernels.lisp in the persistent SBCL REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
  (append
    (remove-if (lambda (case) (search "list-search/" (first case))) miricl-port-bench:*cases*)
    '(("list-search/member-hit" search-list-values
        (list '(1 2 3 4 5 6 7 8) 7 nil (list 0 0 #'eql nil)) :calls 200000)
      ("list-search/member-miss" search-list-values
        (list '(1 2 3 4 5 6 7 8) 9 nil (list 0 0 #'eql nil)) :calls 200000)
      ("list-search/assoc" search-list-values
        (list '(nil (a . 1) (b . 2) (c . 3)) 'c nil (list 1 0 #'eql nil)) :calls 200000)
      ("list-search/rassoc" search-list-values
        (list '(nil (a . 1) (b . 2) (c . 3)) 3 nil (list 2 0 #'eql nil)) :calls 200000)
      ("list-search/if" search-list-values
        (list '(0 0 0 1 2) nil nil (list 0 1 #'plusp nil)) :calls 200000)
      ("list-search/if-not" search-list-values
        (list '(1 3 5 6 7) nil nil (list 0 2 #'oddp nil)) :calls 200000)
      ("list-search/key" search-list-values
        (list '(1 2 3 4 5 6 7 8) 7 #'1+ (list 0 0 #'eql nil)) :calls 200000))))
