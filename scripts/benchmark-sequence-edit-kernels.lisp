(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (member (second case) '(remove-marked-values substitute-marked-values)))
                  miricl-port-bench:*cases*)
       '(("edit/remove-list" remove-marked-values
           (list '(1 2 3 4 5 6 7 8) #*01010101 #(8 0 8 4 nil)) :calls 20000)
         ("edit/remove-vector" remove-marked-values
           (list #(1 2 3 4 5 6 7 8) #*01010101 #(8 0 8 4 nil)) :calls 20000)
         ("edit/remove-string" remove-marked-values
           (list "abcdefgh" #*01010101 #(8 0 8 4 nil)) :calls 20000)
         ("edit/remove-bits" remove-marked-values
           (list #*10101010 #*01010101 #(8 0 8 4 nil)) :calls 20000)
         ("edit/remove-unchanged" remove-marked-values
           (list '(1 2 3 4 5 6 7 8) #*00000000 #(8 0 8 0 nil)))
         ("edit/substitute-list" substitute-marked-values
           (list (list 1 2 3 4 5 6 7 8) #*01010101 :new #(0 8)) :calls 100000)
         ("edit/substitute-vector" substitute-marked-values
           (list (vector 1 2 3 4 5 6 7 8) #*01010101 :new #(0 8)) :calls 100000)
         ("edit/substitute-string" substitute-marked-values
           (list (copy-seq "abcdefgh") #*01010101 #\x #(0 8)) :calls 100000))))
