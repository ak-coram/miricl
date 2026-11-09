;;;; Subsequence alignment, absolute indices, and callback argument semantics.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-sequence-mismatch
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (pair '((nil nil) (nil (1 2)) ((1 2) nil)
                    ((1 2 1) (0 2 1 3)) ((1 2 1) (1 2 1)) ((1 2) (0 1 2))))
      (dolist (type1 '(list vector))
        (dolist (type2 '(list vector))
          (let ((a (coerce (first pair) type1)) (b (coerce (second pair) type2)))
            (dolist (bounds1 (sequence-test-bounds a))
              (destructuring-bind (start1 end1) bounds1
                (dolist (bounds2 (sequence-test-bounds b))
                  (destructuring-bind (start2 end2) bounds2
                    (dolist (from-end '(nil t))
                      (let ((form `(mismatch ',a ',b :start1 ,start1 :end1 ,end1
                                     :start2 ,start2 :end2 ,end2 :from-end ,from-end)))
                        (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))))))))
    (dolist (test '(eql char= char-equal))
      (dolist (keyword '(:test :test-not))
        (dolist (bounds1 (sequence-test-bounds "aλ🙂"))
          (destructuring-bind (start1 end1) bounds1
            (dolist (bounds2 (sequence-test-bounds "AΛ🙂x"))
              (destructuring-bind (start2 end2) bounds2
                (dolist (from-end '(nil t))
                  (let ((form `(mismatch "aλ🙂" "AΛ🙂x" :start1 ,start1 :end1 ,end1
                                 :start2 ,start2 :end2 ,end2 :from-end ,from-end ,keyword (function ,test))))
                    (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))))))
    (dolist (form
      '((mismatch '(1 2 3) '(2 3 4) :key #'oddp :test-not #'eq)
        (mismatch '((1) (2) (3)) '((1) (2) (4)) :key 'car)
        (mismatch '(1 2 3) '(2 3 4) :test #'<)
        (mismatch '(1 2 3) '(2 3 4) :test-not #'>)
        (mismatch '(1 2) #(1 2 3))
        (mismatch '(#\a #\λ) "aΛ" :test 'char-equal)
        (mismatch #*101 #*00101 :from-end t)
        (mismatch "abc" "ABC" :key 'char-upcase)
        (mismatch '(1 2 3) '(1 2 3) :key nil :key 'car :from-end nil :from-end t)
        (mismatch '(1 2 3) '(1 2 3) :start1 0 :start1 2 :end1 nil :end1 0)
        (mismatch '(1 2) '(1 2) :extra t :allow-other-keys t :allow-other-keys nil)
        (multiple-value-list (mismatch '(1 2) '(1 3) :test (lambda (a b) (values (= a b) 99))))
        (let ((saved nil))
          (list (mismatch '(1 2 3) '(1 3 4) :test (lambda (&rest args) (push args saved) (= (car args) (cadr args)))) saved))
        (let ((keys 0) (tests 0))
          (list (mismatch nil '(1 2) :key (lambda (x) (incf keys) x)
                  :test (lambda (a b) (incf tests) (= a b))) keys tests))
        (let* ((a (make-array 5 :initial-contents '(0 1 2 3 4) :fill-pointer 3))
               (b (make-array 2 :displaced-to a :displaced-index-offset 1)))
          (mismatch a b :start1 1))
        (let* ((a (copy-seq "xλ🙂y"))
               (b (make-array 2 :element-type 'character :displaced-to a :displaced-index-offset 1)))
          (mismatch "Λ🙂" b :test 'char-equal))
        (let ((seen nil))
          (list (mismatch (progn (push 1 seen) '(1 2)) (progn (push 2 seen) '(1 2))
                  :from-end (progn (push 3 seen) t)) seen))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    (is (equal '(1 :changed) (m:evaluate vm
      '(let ((saved #'mismatch))
         (unwind-protect
           (progn (setf (symbol-function 'mismatch) (lambda (&rest args) (declare (ignore args)) :changed))
             (list (funcall saved '(1 2) '(1 3)) (mismatch '(1 2) '(1 3))))
           (setf (symbol-function 'mismatch) saved))))))))

(test native-mismatch-errors
  (m:with-interpreter (vm)
    (dolist (source '("(mismatch)" "(mismatch nil)" "(mismatch nil nil :start1)"
                      "(mismatch nil nil :extra t)"
                      "(mismatch nil nil :extra t :allow-other-keys nil :allow-other-keys t)"
                      "(mismatch nil nil :test 'eql :test-not 'eql)"
                      "(mismatch '(1) '(1) :test 'identity)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(mismatch 1 nil)" "(mismatch nil 1)" "(mismatch '(1 . 2) nil)"
                      "(let ((s (list 1))) (setf (cdr s) s) (mismatch nil s))"
                      "(mismatch nil nil :start1 nil)" "(mismatch nil nil :start2 nil)"
                      "(mismatch nil nil :end1 1)" "(mismatch nil nil :end2 1)"
                      "(mismatch '(1) '(1) :start1 1 :end1 0)"
                      "(mismatch '(1) '(1) :start2 -1)" "(mismatch '(1) '(1) :key 'car)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))

(defun reference-sequence-search (a b start1 end1 start2 end2 from-end test)
  ;; Enumerate candidate starts, including END2 for an empty pattern. ECL's
  ;; backwards SEARCH does not consistently honor that empty-pattern rule.
  (let ((matches (loop for start from start2 to (- end2 (- end1 start1))
                      when (loop for i from start1 below end1 for j from start
                                 always (funcall test (elt a i) (elt b j)))
                        collect start)))
    (if from-end (car (last matches)) (first matches))))

(test native-sequence-search
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (pair '((nil nil) (nil (1 2)) ((1 2) nil) ((1 2) (0 1 2 1 2))
                    ((1 2 1) (1 2 1 2)) ((1 3) (1 2 1 2))
                    ("aa" "aaaaa") ("aAb" "aaABa") ("λ🙂" "xΛ🙂λ🙂")))
      (dolist (type1 (if (stringp (first pair)) '(string list vector) '(list vector)))
        (dolist (type2 (if (stringp (second pair)) '(string list vector) '(list vector)))
          (let ((a (coerce (first pair) type1)) (b (coerce (second pair) type2)))
            (dolist (bounds1 (sequence-test-bounds a))
              (destructuring-bind (start1 end1) bounds1
                (dolist (bounds2 (sequence-test-bounds b))
                  (destructuring-bind (start2 end2) bounds2
                    (dolist (from-end '(nil t))
                      (dolist (test (if (stringp (first pair)) '(eql char-equal) '(eql)))
                        (let ((form `(search ',a ',b :start1 ,start1 :end1 ,end1
                                       :start2 ,start2 :end2 ,end2 :from-end ,from-end :test (function ,test))))
                          (is (eql (reference-sequence-search a b start1 end1 start2 end2 from-end test)
                                   (m:evaluate vm form)) "Mismatch for ~S" form))))))))))))
    (dolist (form
      '((search '(0 1) '(2 4 6 1 3 5) :key #'oddp)
        (search '((1) (2)) '((0) (1) (2) (1) (2)) :key 'car :from-end t)
        (search '(1 2) '(0 1 2 1 2) :test #'<)
        (search '(1 2) '(0 1 2 1 2) :test-not #'>)
        (search "ab" "abab" :test-not 'char=)
        (search "aB" "abab" :test-not 'char-equal :from-end t)
        (search "aB" "abab" :key 'char-upcase :from-end t)
        (search #*101 #*010101 :from-end t)
        (search "aaaab" "aaaaaaaaaaaaaaaaaaab")
        (search "aaaab" "aaaaaaaaaaaaaaaaaaaa" :from-end t)
        (search "abab" "abababab" :from-end t)
        (search "a" "aaaa" :from-end t)
        (search "A" "aaaa" :test 'char-equal :start2 1 :end2 3 :from-end t)
        (search "ab" "abab" :key nil :key 'car :start2 0 :start2 4 :end2 nil :end2 0)
        (search "ab" "abab" :extra t :allow-other-keys t :allow-other-keys nil)
        (multiple-value-list (search '(1 2) '(1 2) :test (lambda (a b) (values (= a b) 99))))
        (let ((args nil))
          (let ((index (search '(1 2) '(1 2) :test (lambda (&rest x) (push x args) (eql (car x) (cadr x))))))
            (list index args)))
        (let ((calls 0)) (list (search nil '(1 2) :test (lambda (a b) (incf calls) (= a b))) calls))
        (let* ((base (vector 0 1 2 1 2 0))
               (s (make-array 4 :displaced-to base :displaced-index-offset 1)))
          (search '(1 2) s :from-end t))
        (let* ((base (copy-seq "xλ🙂Λ🙂x"))
               (s (make-array 4 :element-type 'character :displaced-to base :displaced-index-offset 1)))
          (search "Λ🙂" s :test 'char-equal :from-end t))
        (let ((s (make-array 6 :initial-contents '(0 1 2 1 2 0) :fill-pointer 3)))
          (search '(1 2) s :from-end t))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    (is (equal '(2 :changed) (m:evaluate vm
      '(let ((saved #'search))
         (unwind-protect
           (progn (setf (symbol-function 'search) (lambda (&rest args) (declare (ignore args)) :changed))
             (list (funcall saved "ab" "abab" :from-end t) (search "ab" "abab")))
           (setf (symbol-function 'search) saved))))))))

(test native-search-errors
  (m:with-interpreter (vm)
    (dolist (source '("(search)" "(search nil)" "(search nil nil :end1)"
                      "(search nil nil :bad t)"
                      "(search nil nil :bad t :allow-other-keys nil :allow-other-keys t)"
                      "(search nil nil :test 'eql :test-not 'eql)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(search 1 nil)" "(search nil 1)" "(search nil '(1 . 2))"
                      "(search nil nil :start1 nil)" "(search nil nil :start2 nil)"
                      "(search nil nil :end1 1)" "(search nil nil :end2 1)"
                      "(search '(1) '(1) :start1 1 :end1 0)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
