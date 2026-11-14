;;;; Load after scripts/benchmark-runtime-kernels.lisp in the persistent host.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (eq (second case) 'extract-integer-field)) miricl-port-bench:*cases*)
       '(("integer-field/scalar" extract-integer-field (list 123456789 8 5 nil) :calls 200000)
         ("integer-field/scalar-test" extract-integer-field (list 123456789 8 5 t) :calls 200000)
         ("integer-field/scalar64" extract-integer-field (list -9223372036854775808 64 0 nil) :calls 100000)
         ("integer-field/bignum" extract-integer-field (list (1- (ash 1 200)) 100 64 nil) :calls 20000)
         ("integer-field/bignum-test" extract-integer-field (list (- (ash 1 201)) 100 64 t) :calls 20000)
         ("integer-field/positive-outside" extract-integer-field (list 17 (ash 1 100) (ash 1 100) nil) :calls 200000)
         ("integer-field/sign-extension" extract-integer-field (list -1 1 (ash 1 100) nil) :calls 200000)
         ("integer-field/predicate-huge" extract-integer-field (list -1 (ash 1 100) 0 t) :calls 200000)
         ("integer-field/zero-width" extract-integer-field (list -1 0 (ash 1 100) nil) :calls 200000))))
