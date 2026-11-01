;;;; Substitution shares selection with removal, preserving sequence storage.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-sequence-substitution
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (sequence '(nil (1 2 1 3) "aAλa" #(1 2 1 3) #*1010))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence) do
          (dolist (count '(nil -1 0 1 2 1000000000000000000000))
            (dolist (from-end '(nil t))
              (dolist (operator '(substitute substitute-if substitute-if-not
                                 nsubstitute nsubstitute-if nsubstitute-if-not))
                (let* ((string (stringp sequence))
                       (item-p (member operator '(substitute nsubstitute)))
                       (item (if string #\a 1)) (new (if string #\z 0))
                       (predicate (if string 'upper-case-p 'oddp))
                       (form `(let* ((s (copy-seq ',sequence))
                                     (out (,operator ,new ,(if item-p `',item `(function ,predicate)) s
                                            :start ,start :end ,end :count ,count :from-end ,from-end)))
                                (list (coerce out 'list) (coerce s 'list)))))
                  (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))))))
    (dolist (form
      '((substitute 9 2 '(1 2 3 2 4) :test #'<)
        (substitute 9 2 '(1 2 3 2 4) :test-not #'<)
        (substitute 9 2 '((1) (2) (3) (2)) :key #'car)
        (substitute '(9) '(2) '((1) (2) (3)) :key #'car
          :test (lambda (item value) (= (car item) value)))
        (substitute-if 9 #'evenp '((1) (2) (3) (4)) :key 'car :from-end t :count 1)
        (substitute #\z #\a "aAλΛ🙂a" :test #'char-equal :from-end t :count 2)
        (substitute #\z #\a "aAλΛ🙂a" :test-not #'char-equal)
        (substitute #\z #\a "aAλΛ🙂a" :key #'identity :test #'char-equal)
        (substitute nil nil "abc")
        (substitute nil #\a "abc" :count 0)
        (substitute-if nil #'oddp '(1 2 3))
        (multiple-value-list (substitute-if 9 (lambda (x) (values (oddp x) :extra)) '(1 2 3)))
        (let ((saved nil))
          (list (substitute-if 9 (lambda (&rest x) (push x saved) (oddp (car x))) '(1 2 3)) saved))
        (let* ((x (list (list 1) (list 2) (list 1))) (out (substitute :new '(1) x :test 'equal)))
          (list x out (eq (second x) (second out))))
        (let* ((s (list 1 2 1)) (tail (cdr s)) (last (cddr s))
               (out (nsubstitute 9 1 s)))
          (list s (eq out s) (eq tail (cdr s)) (eq last (cddr s))))
        (let* ((s (make-array 6 :initial-contents '(1 2 1 3 1 4) :fill-pointer 4))
               (out (nsubstitute 9 1 s :from-end t :count 1)))
          (list (eq s out) (coerce s 'list) (fill-pointer s) (aref s 4)))
        (let* ((s (make-array 6 :initial-contents '(1 2 1 3 1 4) :fill-pointer 4))
               (out (substitute 9 1 s)))
          (list (coerce s 'list) (coerce out 'list) (array-element-type out) (length out)))
        (let* ((base (vector 1 2 1 3 1 4))
               (s (make-array 4 :displaced-to base :displaced-index-offset 1))
               (out (nsubstitute 9 1 s)))
          (list (eq s out) (coerce s 'list) (coerce base 'list) (eq (array-displacement s) base)))
        (let* ((base (copy-seq "aAλΛ🙂a"))
               (s (make-array 4 :element-type 'character :displaced-to base :displaced-index-offset 1))
               (out (substitute #\z #\λ s :test 'char-equal)))
          (list base (coerce out 'list) (array-element-type out)))
        (let* ((base (copy-seq #*101101))
               (s (make-array 4 :element-type 'bit :displaced-to base :displaced-index-offset 1))
               (out (nsubstitute 0 1 s :count 2 :from-end t)))
          (list (eq s out) (coerce base 'list) (array-element-type out)))
        (let ((calls 0))
          (list (substitute-if 9 (lambda (x) (declare (ignore x)) (incf calls) t) '(1 2 3) :count -1) calls))
        (substitute 9 1 '(1 2 1) :from-end nil :from-end t :count 1 :count 0)
        (substitute 9 1 '(1 2 1) :start 0 :start 2 :end nil :end 0)
        (substitute 9 1 '(1 2 1) :unknown t :allow-other-keys t :allow-other-keys nil)
        (substitute-if 9 'oddp '(1 2 1) :test 'car :allow-other-keys t)
        (let ((seen nil))
          (list (substitute (progn (push 1 seen) 9) (progn (push 2 seen) 1)
                  (progn (push 3 seen) '(1 2 1)) :count (progn (push 4 seen) 1)) seen))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    ;; NIL is the identity key, and the leftmost repeated keyword wins.
    ;; CCL tries to call NIL when this combination uses its list fast path.
    (is (equal '(9 2 1) (m:evaluate vm
      '(substitute 9 1 '(1 2 1) :key nil :key 'car :from-end nil :from-end t :count 1 :count 0))))
    (dolist (count '(nil 1 31 32 33 64))
      (dolist (from-end '(nil t))
        (let ((form `(substitute-if 99 #'oddp (loop for i below 70 collect i)
                      :start 2 :end 68 :count ,count :from-end ,from-end)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (is (equal '((9 2) :changed) (m:evaluate vm
      '(let ((saved #'substitute))
         (unwind-protect
           (progn (setf (symbol-function 'substitute) (lambda (&rest args) (declare (ignore args)) :changed))
             (list (funcall saved 9 1 '(1 2)) (substitute 9 1 '(1 2))))
           (setf (symbol-function 'substitute) saved))))))))

(test native-substitution-errors
  (m:with-interpreter (vm)
    (dolist (source '("(substitute)" "(substitute 1)" "(substitute 1 2)"
                      "(nsubstitute-if 1 #'oddp)" "(substitute 1 2 nil :start)"
                      "(substitute 1 2 nil :bad t)"
                      "(substitute 1 2 nil :bad t :allow-other-keys nil :allow-other-keys t)"
                      "(substitute-if 9 #'oddp nil :test #'eql)"
                      "(substitute 1 2 nil :test #'eql :test-not #'eql)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(substitute 1 2 3)" "(substitute 1 2 '(1 . 2))"
                      "(let ((s (list 1))) (setf (cdr s) s) (nsubstitute 1 2 s))"
                      "(substitute 1 2 nil :count 1/2)" "(substitute 1 2 nil :start nil)"
                      "(substitute 1 2 '(1 2) :start -1)" "(substitute 1 2 nil :end 1)"
                      "(substitute 1 2 '(1 2) :start 2 :end 1)"
                      "(substitute nil #\\a \"abc\")" "(nsubstitute 2 1 (copy-seq #*101))"
                      "(substitute #\\z nil \"abc\" :test #'char=)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
