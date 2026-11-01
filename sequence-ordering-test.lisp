;;;; Compare ordering, stability, and storage effects with the host.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-sequence-ordering
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (operator '(sort stable-sort))
      (dolist (sequence '(nil (3) (3 1 2 1) (0 -2 7 1/2 1.0d0) #(3 1 2 1) #*101001 "bAλaΛ"))
        (dolist (predicate (if (stringp sequence) '(char< char> char-lessp) '(< >)))
          (let ((form `(,operator (copy-seq ',sequence) (function ,predicate))))
            (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))
      (dolist (form
        `((,operator (list (cons 2 'a) (cons 1 'b) (cons 2 'c) (cons 1 'd)) '< :key 'car)
          (let* ((v (make-array 6 :initial-contents '(3 1 2 9 8 7) :fill-pointer 3))
                 (out (,operator v '<)))
            (list (eq out v) (fill-pointer v) (coerce v 'list) (aref v 3) (aref v 4) (aref v 5)))
          (let* ((base (vector 9 3 1 2 8))
                 (v (make-array 3 :displaced-to base :displaced-index-offset 1)))
            (,operator v '<) (coerce base 'list))
          (let* ((base (copy-seq "xbAay"))
                 (v (make-array 3 :element-type 'character :displaced-to base :displaced-index-offset 1)))
            (,operator v 'char-lessp) base)
          (multiple-value-list (,operator (list 3 1 2) (lambda (a b) (values (< a b) :extra))))
          (,operator (list 3 1 2) '< :key nil :key 'car)
          (,operator (list 3 1 2) '< :unused t :allow-other-keys t :allow-other-keys nil)
          (block exit (,operator (list 3 1 2) (lambda (a b) (declare (ignore a b)) (return-from exit :escaped))))))
        (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))
    ;; Sorting lists permutes the original conses, preserving each cons's CAR.
    (is (eq t (m:evaluate vm
      '(let* ((a (cons 3 nil)) (b (cons 1 nil)) (c (cons 2 nil)))
         (setf (cdr a) b (cdr b) c)
         (let ((out (stable-sort a '<)))
           (and (eq out b) (eq (cdr b) c) (eq (cdr c) a) (null (cdr a))
             (= (car a) 3) (= (car b) 1) (= (car c) 2)))))))
    ;; Even inconsistent predicates terminate and retain the complete multiset.
    (is (equal '(0 1 2 3 4 5 6 7 8 9)
      (m:evaluate vm '(sort (sort (list 9 8 7 6 5 4 3 2 1 0)
        (let ((n 0)) (lambda (a b) (declare (ignore a b)) (oddp (incf n))))) '<))))))

(test native-sequence-merge
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (type '(list cons vector simple-vector (vector * 6) (simple-array t (6))))
      (dolist (a '((1 3 5) #(1 3 5)))
        (dolist (b '((2 4 6) #(2 4 6)))
          (let ((form `(merge ',type (copy-seq ',a) (copy-seq ',b) '<)))
            (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (dolist (form
      '((merge 'null nil nil '<)
        (merge '(bit-vector 6) (list 0 1 1) (copy-seq #*001) '<)
        (merge 'string (list #\a #\c) (copy-seq "bd") 'char<)
        (merge 'list (list '(1 a) '(2 b)) (list '(1 c) '(2 d)) '< :key 'car)
        (merge 'vector (list '(1 a) '(2 b)) (list '(1 c) '(2 d)) '< :key 'car)
        (merge 'list nil (list 1 2 3) '<)
        (merge 'vector (list 1 2 3) nil '<)
        (merge 'list (list 1 3) (list 2 4) '< :key nil :key 'car)
        (merge 'list (list 1 3) (list 2 4) '< :unused t :allow-other-keys t)
        (multiple-value-list (merge 'list (list 1 3) (list 2 4)
                              (lambda (a b) (values (< a b) :extra))))
        (let ((v (make-array 5 :initial-contents '(1 3 5 99 98) :fill-pointer 3)))
          (merge 'list v (list 2 4 6) '<))))
      (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))

(test native-list-merge-storage
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate vm
      '(let* ((a (list 1 3 5)) (b (list 2 4 6))
              (a2 (cdr a)) (a3 (cddr a)) (b2 (cdr b)) (b3 (cddr b))
              (out (merge 'list a b '<)))
         (and (eq out a) (eq (cdr out) b) (eq (cddr out) a2)
           (eq (cdddr out) b2) (eq (cdr (cdddr out)) a3)
           (eq (cdr a3) b3) (null (cdr b3)))))))
    (is (equal '(1 2 3 3 5 5) (m:evaluate vm
      '(let* ((tail (list 3 5)) (a (cons 1 tail)) (b (cons 2 tail)))
         (merge 'list a b '<)))))
    (is (equal '(1 1 2 2 3 3) (m:evaluate vm
      '(let ((a (list 1 2 3))) (merge 'list a a '<)))))
    (is (eq t (m:evaluate vm '(let ((a (list 1 2))) (eq a (merge 'list nil a '<))))))))

(test native-sequence-ordering-errors
  (m:with-interpreter (vm)
    (dolist (source '("(sort)" "(stable-sort nil)" "(merge 'list nil nil)"
                      "(sort nil '< :key)" "(sort nil '< :extra t)"
                      "(stable-sort nil '< :extra t :allow-other-keys nil :allow-other-keys t)"
                      "(merge 'list nil nil '< :key)" "(merge 'list nil nil '< :extra t)"
                      "(sort (list 2 1) 'car)" "(merge 'list (list 1) (list 2) 'car)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(sort 42 '<)" "(sort (cons 1 2) '<)"
                      "(sort (make-array '(2 2)) '<)"
                      "(sort (let ((s (list 1 2))) (setf (cddr s) s) s) '<)"
                      "(merge '(vector * 3) (list 1 2) (list 3 4) '<)"
                      "(merge 'null (list 1) nil '<)"
                      "(sort (list 'a 'b) '<)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
