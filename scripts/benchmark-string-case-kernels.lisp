;;;; Load after scripts/benchmark-runtime-kernels.lisp in a persistent host REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "case-kernel/" (first case))) miricl-port-bench:*cases*)
       (loop for (name code down ansi) in
               '(("ascii-upper" 97 nil t) ("ascii-lower" 65 t t)
                 ("unicode-upper" 955 nil nil) ("unicode-lower" 923 t nil)
                 ("ansi-upper" 955 nil t) ("ansi-lower" 923 t t)
                 ("alternating" 257 nil t) ("uncased" 128578 nil t))
             for raw = (if down miricl::*string-downcase-ranges* miricl::*string-case-ranges*)
             collect (list (concatenate 'string "case-kernel/" name) 'character-case-code-value
                           `(list ,code ,down (map 'string #'code-char ',(miricl::string-case-table-values raw)) ,ansi)))))
