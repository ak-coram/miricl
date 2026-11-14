(in-package :cl-user)
(setf miricl-port-bench:*cases*
      '(("copy-tree/atom" copy-tree-value (list :atom) :calls 1000000)
        ("copy-tree/flat" copy-tree-value (list '(1 2 3 4 5 6 7 8)) :calls 100000)
        ("copy-tree/nested" copy-tree-value (list '(1 (2 3) (4 . 5) . 6)) :calls 20000)
        ("copy-tree/shared" copy-tree-value (let ((shared (list 1 2))) (list (list shared shared))) :calls 20000)
        ("reverse/string-copy" reverse-vector-value (list (copy-seq "aλ🙂bcdef") nil) :calls 100000)
        ("reverse/string-write" reverse-vector-value (list (copy-seq "aλ🙂bcdef") t) :calls 100000)))
