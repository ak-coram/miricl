;;;; Load after benchmark-runtime-kernels.lisp in the persistent SBCL REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
  (append
    (remove-if (lambda (case) (search "mapping/" (first case))) miricl-port-bench:*cases*)
    '(("mapping/car" mapping-argument-value (list '(1 2 3) 0 0))
      ("mapping/sublist" mapping-argument-value (list '(1 2 3) 0 2))
      ("mapping/vector" mapping-argument-value (list #(1 2 3 4) 3 4))
      ("mapping/mapcar" map-list-values (list #'1+ (list '(1 2 3 4 5 6 7 8)) 0 nil) :calls 20000)
      ("mapping/mapcar-two" map-list-values (list #'+ (list '(1 2 3 4) '(10 20 30 40)) 0 nil) :calls 20000)
      ("mapping/maplist" map-list-values (list #'length (list '(1 2 3 4 5 6 7 8)) 2 nil) :calls 20000)
      ("mapping/every" map-list-values (list #'plusp (list #(1 2 3 4 5 6 7 8)) 4 nil) :calls 20000)
      ("mapping/some" map-list-values (list #'identity (list #(nil nil 3 nil)) 5 nil) :calls 20000)
      ("mapping/map-into" map-list-values
        (list #'1+ (list '(1 2 3 4)) 8 (make-array 4 :fill-pointer 0 :initial-element 99)) :calls 20000)
      ("mapping/result-vector" map-sequence-result-value (list 'vector (list '(1 2 3 4) #(1 2 3))) :calls 20000))))
