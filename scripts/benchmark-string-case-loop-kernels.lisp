;;;; Load after benchmark-runtime-kernels.lisp in the persistent SBCL REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "case-loop/" (first case))) miricl-port-bench:*cases*)
       (loop for (name text flags) in
               '(("ascii-copy-upper" "aBcDeFgHiJkLmNoP" 0)
                 ("ascii-unchanged" "ABCDEFGHIJKLMNOP" 0)
                 ("ascii-copy-lower" "AbCdEfGhIjKlMnOp" 2)
                 ("capitalize" "hELLO 13A, wORLD!" 4)
                 ("unicode-upper" "λΛαΑσΣωΩéÉжЖ" 0)
                 ("empty" "" 0)
                 ("mutate-unchanged" "ABCDEFGHIJKLMNOP" 1))
             collect
             (list (concatenate 'string "case-loop/" name) 'case-string-region-value
                   `(list (copy-seq ,text) 0 ,(length text)
                          (vector ,flags #'char-upcase #'char-downcase #'alphanumericp #'subseq))
                   :calls 100000))))
