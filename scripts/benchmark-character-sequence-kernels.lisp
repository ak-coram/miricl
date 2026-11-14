(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (member (second case) '(scan-character-region mismatch-character-regions search-character-regions))) miricl-port-bench:*cases*)
       '(("character-scan/absent" scan-character-region (list "abcdefghijklmnop" (vector 0 16 122 0 nil)))
         ("character-scan/reverse" scan-character-region (list "abcdefghijklmnop" (vector 0 16 110 2 nil)))
         ("character-scan/count" scan-character-region (list "abacadaeafagahaia" (vector 0 16 97 4 nil)))
         ("character-scan/fold" scan-character-region (list "AbAcAdAeAfAgAhAiA" (vector 0 16 65 4 #'char-upcase)) :calls 100000)
         ("character-mismatch/equal" mismatch-character-regions (list "abcdefghijklmnop" "abcdefghijklmnop" (vector 0 16 0 16 0 nil)))
         ("character-mismatch/reverse" mismatch-character-regions (list "xbcdefghijklmnop" "abcdefghijklmnop" (vector 0 16 0 16 2 nil)))
         ("character-mismatch/fold" mismatch-character-regions (list "AbCdEfGhIjKlMnOp" "abcdefghijklmnop" (vector 0 16 0 16 0 #'char-upcase)) :calls 100000)
         ("character-search/repeated" search-character-regions (list "ababac" "ababababababababababababac" (vector 0 6 0 26 nil nil)) :calls 20000)
         ("character-search/reverse" search-character-regions (list "ababac" "ababacabababababababababac" (vector 0 6 0 26 t nil)) :calls 20000)
         ("character-search/fold" search-character-regions (list "AbAbAc" "ababababababababababababac" (vector 0 6 0 26 nil #'char-upcase)) :calls 20000)
         ("character-search/single" search-character-regions (list "z" "abcdefghijklmnopqrstuvwxyz" (vector 0 1 0 26 nil nil)) :calls 20000))))
(setf miricl-port-bench:*cases*
      (append miricl-port-bench:*cases*
       '(("character-relation/less" compare-character-regions (list "abcdefghijklmnoa" "abcdefghijklmnop" (vector 0 16 0 16 2 nil)))
         ("character-relation/equal" compare-character-regions (list "abcdefghijklmnop" "abcdefghijklmnop" (vector 0 16 0 16 0 nil)))
         ("character-relation/fold" compare-character-regions (list "AbCdEfGhIjKlMnOp" "abcdefghijklmnop" (vector 0 16 0 16 4 #'char-upcase)) :calls 100000)
         ("bootstrap-text/equal" bootstrap-text-equal-p (list (copy-seq "abcdefghijklmnop") (copy-seq "abcdefghijklmnop")))
         ("bootstrap-text/miss" bootstrap-text-equal-p (list "abcdefghijklmnop" "abcdefghijklmnoq")))))
