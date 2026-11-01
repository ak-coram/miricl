;;;; Tree copying/sharing, replacement boundaries, and ordered tests.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-tree-substitution
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (tree '(nil a 1 (1 2 1) (1 (2 1) . 1) ((1 . 2) (3 1) nil)))
      (dolist (new '(nil 9 (new . tail)))
        (dolist (old '(nil 1 (1 . 2)))
          (dolist (operator '(subst nsubst))
            (dolist (test '(eql equal equalp))
              (dolist (keyword '(:test :test-not))
                (let ((form `(let* ((tree (copy-tree ',tree)) (result (,operator ',new ',old tree
                                ,keyword (function ,test)))) (list tree result))))
                  (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))))))
    (dolist (operator '(subst-if subst-if-not nsubst-if nsubst-if-not))
      (dolist (predicate '(null consp atom numberp))
        (let ((form `(,operator :new (function ,predicate) (copy-tree '(1 (2 . 3) nil a)))))
          (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))
    (dolist (operator '(sublis nsublis))
      (dolist (alist '(nil ((1 . 9) (a . b)) (nil (nil . x) (1 . nil))
                       (((1 . 2) . :pair) (1 . :atom)) ((1 . first) (1 . second))))
        (dolist (test '(eql equal equalp))
          (let ((form `(let* ((tree (copy-tree '(1 a (1 . 2) nil)))
                             (result (,operator ',alist tree :test (function ,test)))) (list tree result))))
            (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (dolist (form
      '((sublis '((1 . small) (5 . medium) (10 . large)) '(0 3 8 20)
          :test (lambda (a b) (and (numberp a) (numberp b) (< a b))))
        (sublis '((t . :number)) '(1 a (2 . b)) :key 'numberp)
        (sublis '((nil . :cons)) '(1 a (2 . b)) :key 'atom)
        (sublis '((1 . 9)) '(1 2 1) :key nil :key 'car)
        (subst :changed 2 '(1 2 3) :key nil :key 'car)
        (subst :changed 2 '(1 2 3) :extra t :allow-other-keys t :allow-other-keys nil)
        (subst-if 9 'null '(1 nil 2) :test 'car :allow-other-keys t)
        (let* ((tree (list 1 2 3)) (tail (cdr tree)) (out (nsubst 9 2 tree)))
          (list (eq out tree) (eq tail (cdr tree)) tree))
        (let* ((new (list :replacement)) (tree (list 1 2)))
          (list (eq new (subst new tree tree :test 'eq)) (eq new (nsubst new tree tree :test 'eq))))
        (let* ((new (list :replacement)) (result (subst new 1 '(1 . 1))))
          (list (eq new (car result)) (eq new (cdr result))))
        (multiple-value-list (subst-if 9 (lambda (x) (values (eql x 1) :ignored)) '(1 2)))
        (let ((saved nil))
          (let ((result (subst-if 9 (lambda (&rest x) (push x saved) (eql (car x) 1)) '(1 2))))
            (list result (length saved) (car (car (last saved))))))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    ;; Sharing is permitted, but hosts need not choose the same policy.
    ;; Miricl retains unaffected subtrees and returns an unchanged input.
    (is (equal '(((1 2) 3) ((1 2) 9) t) (m:evaluate vm
      '(let* ((tree (list (list 1 2) 3)) (unchanged (car tree)) (result (subst 9 3 tree)))
         (list tree result (eq unchanged (car result)))))))
    (is (eq t (m:evaluate vm
      '(let ((tree (list 1 2 3))) (eq tree (sublis '((4 . 5)) tree))))))
    (let ((tree (loop for i below 20000 collect i)))
      (is (eq t (m:evaluate vm
      `(let ((tree ',tree))
         (and (eq tree (sublis '((20001 . :absent)) tree))
           (let ((out (subst :last 19999 tree)))
             (and (= (length out) 20000) (eq (car (last out)) :last) (= (car (last tree)) 19999)))))))))))

(defun reference-tree-equal (a b test negate)
  ;; Compare every leaf, including NIL, without the host's identity shortcuts.
  (cond ((consp a) (and (consp b) (reference-tree-equal (car a) (car b) test negate)
                       (reference-tree-equal (cdr a) (cdr b) test negate)))
        ((consp b) nil)
        (t (not (null (if negate (not (funcall test a b)) (funcall test a b)))))))

(test native-tree-equality
  (m:with-interpreter (vm)
    (dolist (a '(nil 1 a (1 2) (1 . 2) ((1 2) . 3) (nil . a)))
      (dolist (b '(nil 1 a (1 2) (1 . 2) ((1 2) . 3) (nil . a)))
        (dolist (test '(eql equal equalp))
          (dolist (keyword '(:test :test-not))
            (let ((form `(not (null (tree-equal ',a ',b ,keyword (function ,test))))))
              (is (eq (reference-tree-equal a b test (eq keyword :test-not))
                      (m:evaluate vm form)) "Mismatch for ~S" form))))))
    (dolist (form
      '((not (null (tree-equal '(1 2) '(2 3) :test (lambda (a b) (if (and (numberp a) (numberp b)) (< a b) (eql a b))))))
        (let ((n 0)) (list (tree-equal '(1 2) '(1 3) :test (lambda (a b) (incf n) (eql a b))) n))
        (let ((n 0)) (list (tree-equal '(1) 1 :test (lambda (a b) (declare (ignore a b)) (incf n))) n))
        (multiple-value-list (tree-equal '(1 2) '(1 2) :test (lambda (a b) (values (eql a b) :ignored))))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-tree-errors
  (m:with-interpreter (vm)
    (dolist (source '("(subst)" "(subst 1 2)" "(sublis nil)" "(tree-equal nil)"
                      "(subst 1 2 nil :key)" "(sublis nil nil :bad t)"
                      "(subst-if 9 'null nil :test 'eql)" "(tree-equal nil nil :key nil)"
                      "(subst 1 2 nil :bad t :allow-other-keys nil :allow-other-keys t)"
                      "(sublis nil nil :test 'eql :test-not 'eql)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(sublis 1 nil)" "(sublis '(1) nil)" "(sublis '((1 . 2) . 3) nil)"
                      "(subst 1 2 3 :key 'car)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
