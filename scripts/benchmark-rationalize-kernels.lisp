(in-package #:cl-user)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (eq (second case) 'rational-in-interval-value))
                     miricl-port-bench:*cases*)
    '(("rational-interval/integer" rational-in-interval-value
       (list 2 4 t (vector #'+ #'- #'* #'/ #'truncate)) :calls 20000)
      ("rational-interval/tenth" rational-in-interval-value
       (list 13421772/134217728 13421774/134217728 t (vector #'+ #'- #'* #'/ #'truncate)) :calls 10000)
      ("rational-interval/continued" rational-in-interval-value
       (list 317810/514229 514229/832040 nil (vector #'+ #'- #'* #'/ #'truncate)) :calls 2000)
      ("rational-interval/bignum" rational-in-interval-value
       (list (/ 1 (ash 1 120)) (/ 1 (1- (ash 1 120))) nil (vector #'+ #'- #'* #'/ #'truncate)) :calls 1000))))
