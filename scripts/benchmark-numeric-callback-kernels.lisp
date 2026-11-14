;;;; Actual arithmetic callback transport through existing generic workers.
;;;; Load after benchmark-runtime-kernels and benchmark-rationalize-kernels.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
  (append
   (remove-if-not (lambda (case)
                    (eq (second case) 'rational-in-interval-value))
                  miricl-port-bench:*cases*)
   '(("numeric-callback/divide-integer" ordering-predicate (list 13 4 nil #'/) :calls 200000)
     ("numeric-callback/divide-rational" ordering-predicate (list 13/7 4/11 nil #'/) :calls 100000)
     ("numeric-callback/divide-complex" ordering-predicate (list #c(1 2) #c(3 -4) nil #'/) :calls 50000)
     ("numeric-callback/divide-float" ordering-predicate (list 1.25d0 0.5d0 nil #'/) :calls 200000)
     ("numeric-callback/divide-bignum" ordering-predicate (list (ash 1 120) 7 nil #'/) :calls 20000)
     ("numeric-callback/truncate-integer" ordering-predicate (list 13 4 nil #'truncate) :calls 200000)
     ("numeric-callback/truncate-rational" ordering-predicate (list 13/7 4/11 nil #'truncate) :calls 100000)
     ("numeric-callback/truncate-float" ordering-predicate (list 1.25d0 0.5d0 nil #'truncate) :calls 200000)
     ("numeric-callback/truncate-bignum" ordering-predicate (list (ash 1 120) 7 nil #'truncate) :calls 20000)
     ("numeric-callback/add-control" ordering-predicate (list 13 4 nil #'+) :calls 200000)
     ("numeric-callback/replacement-control" ordering-predicate
      (list 13 4 nil (lambda (a b) (declare (ignore a b)) (values :replacement :extra))) :calls 100000))))
