;;;; Load after scripts/benchmark-runtime-kernels.lisp in a persistent SBCL REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "character-kernel/" (first case))) miricl-port-bench:*cases*)
       `(("character-kernel/digit-ascii" character-digit-code-value
          (list 55 ,(map 'string #'code-char miricl::*character-digit-ranges*)))
         ("character-kernel/digit-nonascii" character-digit-code-value
          (list 1637 ,(map 'string #'code-char miricl::*character-digit-ranges*)))
         ("character-kernel/digit-miss" character-digit-code-value
          (list 128578 ,(map 'string #'code-char miricl::*character-digit-ranges*)))
         ("character-kernel/alpha-ascii" character-alpha-code-p
          (list 97 ,(map 'string #'code-char miricl::*character-alpha-ranges*)))
         ("character-kernel/alpha-nonascii" character-alpha-code-p
          (list 955 ,(map 'string #'code-char miricl::*character-alpha-ranges*)))
         ("character-kernel/alpha-miss" character-alpha-code-p
          (list 128578 ,(map 'string #'code-char miricl::*character-alpha-ranges*)))
         ("character-kernel/name-unicode" unicode-character-name-code (list "U+1F642"))
         ("character-kernel/name-miss" unicode-character-name-code (list "U+123G")))))
