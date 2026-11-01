;;;; Native READ-FROM-STRING label state is separate from surrounding reads.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-reader-labels
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((let ((x (read-from-string "(#1=(1 2) #1#)"))) (eq (car x) (cadr x)))
        (let ((x (read-from-string "#1=(1 2 . #1#)"))) (eq x (cddr x)))
        (let ((x (read-from-string "#1=(#1#)"))) (eq x (car x)))
        (let ((x (read-from-string "(#1=(#2=#1#) #2#)"))) (eq (car x) (cadr x)))
        (let ((x (read-from-string "(#1=nil #1#)"))) (equal x '(nil nil)))
        (let ((x (read-from-string "(#0=#:foo #0#)"))) (eq (car x) (cadr x)))
        (let ((x (read-from-string "(#1000000000000000000000000000000=(1) #1000000000000000000000000000000#)")))
          (eq (car x) (cadr x)))
        (let ((x (read-from-string "(#1=(1) #.(read-from-string \"#1=3\") #1#)")))
          (and (eq (car x) (caddr x)) (= (cadr x) 3)))
        (let ((x (read-from-string "(#1=(1) #.(handler-case (read-from-string \"#1#\") (reader-error () nil)) #1#)")))
          (and (eq (car x) (caddr x)) (null (cadr x))))
        (progn (read-from-string "#1=17")
          (handler-case (read-from-string "#1#") (reader-error () t)))
        (= (read-from-string "#16rff") 255)
        (= (read-from-string "#2r101") 5)))
      (is (eq t (m:evaluate vm form))))
    (dolist (source '("#1#" "(#1=1 #1=2)" "#1=#1#" "#1=#2=#1#" "#=1" "##"))
      (is (eq t (m:evaluate vm
        `(handler-case (read-from-string ,source) (reader-error () t)))))
      (is (= 3 (m:evaluate vm '(+ 1 2)))))))
