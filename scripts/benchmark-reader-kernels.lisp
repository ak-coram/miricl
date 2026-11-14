;;;; Load after scripts/benchmark-runtime-kernels.lisp in the persistent host.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       '(("reader/integer" small-reader-integer-value (list "123456789" 9 10) :calls 200000)
         ("reader/negative-limit" small-reader-integer-value (list "-2147483648" 11 10) :calls 200000)
         ("reader/overflow" small-reader-integer-value (list "2147483648" 10 10) :calls 200000)
         ("reader/radix36" small-reader-integer-value (list "z12ab" 5 36) :calls 200000)
         ("reader/not-integer" small-reader-integer-value (list "abcdef" 6 10))
         ("reader/empty" small-reader-integer-value (list "" 0 10))
         ("reader/float-format" reader-float-format-code (list 'double-float)))
       '(("parse-integer/short" parse-integer-region-value (list "  -12345  " 0 10 (cons 10 nil)) :calls 20000)
         ("parse-integer/bignum" parse-integer-region-value
           (list "1234567890123456789012345678901234567890" 0 40 (cons 10 nil)) :calls 10000)
         ("parse-integer/junk" parse-integer-region-value (list "123abc" 0 6 (cons 10 t)) :calls 20000)
         ("parse-integer/empty" parse-integer-region-value (list "" 0 0 (cons 10 t)) :calls 20000))
       (remove-if (lambda (case) (member (second case) '(small-reader-integer-value reader-float-format-code parse-integer-region-value)))
                  miricl-port-bench:*cases*)))
