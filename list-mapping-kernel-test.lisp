;;;; Mapping migration regressions: effects, sharing, primary values and bounds.
(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-exclusive-pairs-host
  (labels ((expected (left right predicate negate key)
             (flet ((match (x y)
                      (let ((truth (funcall predicate (if key (funcall key x) x)
                                             (if key (funcall key y) y))))
                        (if negate (not truth) truth))))
               (append (remove-if (lambda (x) (some (lambda (y) (match x y)) right)) left)
                       (remove-if (lambda (y) (some (lambda (x) (match x y)) left)) right)))))
    (dolist (left '(nil (1) (1 1 2 3) (1 4 8)))
      (dolist (right '(nil (1) (2 2 3 5) (3 5 7)))
        (dolist (predicate (list #'= #'< #'> #'/=))
          (dolist (negate '(nil t))
            (let* ((want (expected left right predicate negate nil))
                   (got (miricl-runtime:list-exclusive-values
                          left right nil (if negate (cons nil predicate) (cons predicate nil)))))
              (is (equal (sort (copy-list want) #'<) (sort (copy-list got) #'<))))))))
    (dolist (size '(1 5 17 32 65 100))
      (let* ((left (loop for i below size collect (cons (mod i 7) :left)))
             (right (loop for i from 4 below (+ 4 size) collect (cons (mod i 13) :right)))
             (want (expected left right #'= nil #'car))
             (got (miricl-runtime:list-exclusive-values left right #'car (cons #'= nil))))
        (is (equal (sort (mapcar #'car want) #'<) (sort (mapcar #'car got) #'<)))
        (is (every (lambda (value) (member value (append left right) :test #'eq)) got)))))
  (let ((right (list :a :b)))
    (is (eq right (miricl-runtime:list-exclusive-values nil right nil (cons #'eql nil))))
    (is (eq right (miricl-runtime:list-exclusive-values right nil nil (cons #'eql nil)))))
  (let ((events nil))
    (is (null (miricl-runtime:list-exclusive-values
                '(1 2) '(3 4)
                (lambda (x) (push (list :key x) events) (values x :ignored))
                (cons (lambda (x y) (push (list :test x y) events) (values t nil)) nil))))
    (is (equal '((:key 1) (:key 3) (:test 1 3) (:key 4) (:test 1 4)
                 (:key 2) (:key 3) (:test 2 3) (:key 4) (:test 2 4))
               (reverse events))))
  (is (= 1 (length (multiple-value-list
                    (miricl-runtime:list-exclusive-values '(1) '(2) nil (cons #'= nil))))))
  (dolist (arguments (list (list '(1 . :bad) '(2) nil (cons #'= nil))
                           (list '(1) '(2 . :bad) nil (cons #'= nil))
                           (list nil nil nil nil)
                           (list '(1) '(2) 42 (cons #'= nil))
                           (list '(1) '(2) nil (cons 42 nil))))
    (signals type-error (apply #'miricl-runtime:list-exclusive-values arguments)))
  (let ((cycle (list 1 2)) (calls 0))
    (setf (cddr cycle) cycle)
    (signals type-error (miricl-runtime:list-exclusive-values
                         cycle '(3) nil (cons (lambda (&rest ignored) (declare (ignore ignored)) (incf calls)) nil)))
    (signals type-error (miricl-runtime:list-exclusive-values '(3) cycle nil (cons #'= nil)))
    (is (= 0 calls)))
  (let ((left (list 1 2)))
    (signals type-error (miricl-runtime:list-exclusive-values
                         left '(3) nil (cons (lambda (a b) (setf (cdr left) left) (= a b)) nil))))
  (let ((right (list 3 4)))
    (signals type-error (miricl-runtime:list-exclusive-values
                         '(1 2) right nil (cons (lambda (a b) (setf (cdr right) :bad) (= a b)) nil))))
  (is (equal '(2 1) (miricl-runtime:list-exclusive-values '(1) '(2) nil (cons #'= nil)))))

(test runtime-kernel-exclusive-pairs-guest
  (m:with-interpreter (vm)
    (dolist (operator '(set-exclusive-or nset-exclusive-or))
      (dolist (source
                '("(let ((s (list 1 2))) (setf (cddr s) s) (OP s '(3) :test #'<))"
                  "(let ((s (list 1 2))) (setf (cddr s) s) (OP '(3) s :test-not #'<))"
                  "(OP '(1 . :bad) '(3) :test #'<)"
                  "(let ((s (list 1 2))) (OP s '(3) :test (lambda (a b) (setf (cdr s) s) (< a b))))"))
        (let* ((text (substitute #\Space #\Newline source))
               (text (concatenate 'string (subseq text 0 (search "OP" text))
                                  (string-downcase (symbol-name operator))
                                  (subseq text (+ 2 (search "OP" text))))))
          (is (eq t (m:evaluate-string vm
                      (concatenate 'string "(handler-case " text " (type-error () t))")))))))
    (is (eq t (m:evaluate-string vm
      "(let ((events nil))
         (and (= 1 (length (multiple-value-list
                    (set-exclusive-or '(1 2) '(3 4)
                      :key (lambda (x) (push (list :key x) events) (values x :extra))
                      :test (lambda (x y) (push (list :test x y) events) (values t :extra))))))
              (equal (reverse events)
                '((:key 1) (:key 3) (:test 1 3) (:key 4) (:test 1 4)
                  (:key 2) (:key 3) (:test 2 3) (:key 4) (:test 2 4)))))")))
    (is (equal '(1 3) (m:evaluate-string vm "(sort (set-exclusive-or '(1 2) '(2 3) :test #'=) #'<)")))))

(test runtime-kernel-mapping-host
  (is (equal '(2 3 4) (miricl-runtime:map-list-values #'1+ (list '(1 2 3)) 0 nil)))
  (is (equal '(11 22) (miricl-runtime:map-list-values #'+ (list '(1 2 3) '(10 20)) 0 nil)))
  (let* ((items (list 1 2 3)) (seen nil))
    (is (eq items (miricl-runtime:map-list-values
                    (lambda (tail) (push tail seen)) (list items) 3 nil)))
    (is (every #'eq (reverse seen) (list items (cdr items) (cddr items)))))
  (is (equal '(3 2 1) (miricl-runtime:map-list-values #'length (list '(1 2 3)) 2 nil)))
  (is (eq t (miricl-runtime:map-list-values #'plusp (list #(1 2 3)) 4 nil)))
  (is (eq nil (miricl-runtime:map-list-values #'plusp (list #(1 0 3)) 4 nil)))
  (is (eql 3 (miricl-runtime:map-list-values #'identity (list #(nil nil 3 4)) 5 nil)))
  (let ((saved nil) (out (make-array 4 :initial-element 99 :fill-pointer 0)))
    (is (eq out (miricl-runtime:map-list-values
                  (lambda (&rest arguments)
                    (push arguments saved) (values (apply #'+ arguments) :extra))
                  (list '(1 2 3) #(10 20 30 40)) 8 out)))
    (is (= 3 (fill-pointer out)))
    (is (equal '(11 22 33 99) (loop for i below 4 collect (aref out i))))
    (is (equal '((1 10) (2 20) (3 30)) (reverse saved)))
    (is (not (eq (first saved) (second saved)))))
  (let ((out (make-array 3 :fill-pointer 0)) (calls 0))
    (is (eq out (miricl-runtime:map-list-values (lambda () (incf calls)) nil 8 out)))
    (is (= 3 calls))
    (is (equalp #(1 2 3) out)))
  (is (equal '(nil) (multiple-value-list
                     (miricl-runtime:map-list-values (lambda (x) (values x :extra)) (list '(1 2)) 7 nil))))
  (signals program-error (miricl-runtime:map-list-values #'identity nil 0 nil))
  (signals type-error (miricl-runtime:map-list-values #'identity '(nil . :bad) 0 nil))
  (signals type-error (miricl-runtime:map-list-values #'identity (list nil :bad) 0 nil))
  (signals type-error (miricl-runtime:map-list-values #'identity (list '(1 . :bad)) 0 nil))
  (let ((cycle (list 1 2)))
    (setf (cddr cycle) cycle)
    (signals type-error (miricl-runtime:map-list-values #'identity (list cycle) 0 nil))
    (signals type-error (miricl-runtime:map-list-values #'identity cycle 0 nil)))
  (let ((items (list 1 2 3)))
    (signals type-error
      (miricl-runtime:map-list-values
        (lambda (x) (setf (cdr items) :bad) x) (list items) 0 nil)))
  (let ((items (list 1 2 3)))
    ;; Undefined destructive traversal must still terminate if a callback
    ;; introduces a cycle after the initial proper-list boundary check.
    (is (= 3 (length (miricl-runtime:map-list-values
                      (lambda (x) (setf (cdr items) items) x) (list items) 0 nil)))))
  (is (equal '(2 3) (miricl-runtime:map-list-values #'1+ (list '(1 2)) 0 nil)))
  (is (= 2 (length (miricl-runtime:map-sequence-result-value 'vector (list '(1 2 3) #(1 2)))))))

(test runtime-kernel-mapping-guest
  (m:with-interpreter (vm)
    (dolist (form
              '((mapcar #'1+ '(1 2 3))
                (mapcar #'+ '(1 2 3) '(10 20))
                (maplist #'length '(1 2 3))
                (let* ((items (list 1 2 3)) (tails (maplist #'identity items)))
                  (every #'eq tails (list items (cdr items) (cddr items))))
                (let ((saved nil))
                  (list (mapcar (lambda (&rest args) (push args saved) (apply #'+ args))
                                '(1 2 3) '(10 20 30)) (reverse saved)))
                (multiple-value-list (some (lambda (x) (values x :extra)) '(nil 3 4)))
                (multiple-value-list (every #'identity '(t nil t)))
                (map 'vector #'1+ '(1 2 3))
                (let ((out (make-array 4 :fill-pointer 0 :initial-element 99)))
                  (map-into out #'1+ '(1 2 3))
                  (list (fill-pointer out) (loop for i below 4 collect (aref out i))))))
      (is (equalp (eval form) (m:evaluate vm form)) "Mapping mismatch for ~S" form))
    (dolist (source
              '("(mapcar #'identity nil :bad)"
                "(mapcar #'identity '(1 . :bad))"
                "(let ((s (list 1 2))) (setf (cddr s) s) (mapcar #'identity s))"
                "(let ((s (list 1 2 3))) (mapcar (lambda (x) (setf (cdr s) :bad) x) s))"))
      (is (eq t (m:evaluate-string vm
                  (concatenate 'string "(handler-case " source " (type-error () t))")))))
    (is (equal '(2 3) (m:evaluate-string vm "(mapcar #'1+ '(1 2))")))))

(test runtime-kernel-list-search-host
  (dolist (mode '(0 1 2))
    (dolist (kind '(0 1 2))
      (let* ((items (if (= kind 0) (list 1 2 3 2)
                        (list nil (cons 1 3) (cons 2 2) (cons 3 1))))
             (function (if (= mode 0) #'eql #'oddp))
             (operator (nth mode (nth kind '((member member-if member-if-not)
                                             (assoc assoc-if assoc-if-not)
                                             (rassoc rassoc-if rassoc-if-not)))))
             (expected (funcall operator (if (= mode 0) 2 function) items)))
        (is (eq expected (miricl-runtime:search-list-values items 2 nil (list kind mode function nil)))))))
  (let ((items (list 1 2 3)))
    (is (eq (cddr items) (miricl-runtime:search-list-values items 2 nil (list 0 0 #'< nil))))
    (is (eq (cdr items) (miricl-runtime:search-list-values items 3 #'1+ (list 0 0 #'eql nil))))
    (is (eq items (miricl-runtime:search-list-values items 2 nil (list 0 0 #'eql t)))))
  (let* ((entry (cons nil :value)) (items (list nil entry)))
    (is (eq entry (miricl-runtime:search-list-values items nil nil (list 1 0 #'eql nil)))))
  (let ((items (cons 1 :tail)))
    (is (eq items (miricl-runtime:search-list-values items 1 nil (list 0 0 #'eql nil))))
    (signals type-error (miricl-runtime:search-list-values items 2 nil (list 0 0 #'eql nil))))
  (signals type-error (miricl-runtime:search-list-values '(nil :bad) 1 nil (list 1 0 #'eql nil)))
  (signals type-error (miricl-runtime:search-list-values nil 1 nil (list 3 0 #'eql nil)))
  (signals type-error (miricl-runtime:search-list-values nil 1 nil (list 0 -1 #'eql nil)))
  (signals type-error (miricl-runtime:search-list-values nil 1 nil (list 0 0 #'eql nil :excess)))
  (let ((cycle (list 1 2 3)))
    (setf (cdddr cycle) cycle)
    (is (eq cycle (miricl-runtime:search-list-values cycle 1 nil (list 0 0 #'eql nil))))
    (signals type-error (miricl-runtime:search-list-values cycle :missing nil (list 0 0 #'eql nil))))
  (let ((items (list 1 2 3)))
    (signals type-error
      (miricl-runtime:search-list-values items nil nil
        (list 0 1 (lambda (value) (declare (ignore value)) (setf (cdr items) items) nil) nil))))
  (is (equal '((2 3)) (multiple-value-list
                       (miricl-runtime:search-list-values '(1 2 3) nil nil
                         (list 0 1 (lambda (value) (values (= value 2) :extra)) nil)))))
  (is (equal '(2 3) (miricl-runtime:search-list-values '(1 2 3) 2 nil (list 0 0 #'eql nil)))))

(test runtime-kernel-list-search-boundaries
  (m:with-interpreter (vm)
    (dolist (source
              '("(let ((items (cons 1 :tail))) (eq items (member 1 items)))"
                "(let ((items (list 1 2))) (setf (cddr items) items) (eq items (member 1 items)))"
                "(handler-case (let ((items (list 1 2))) (setf (cddr items) items) (member :missing items)) (type-error (c) (not (typep (type-error-datum c) (type-error-expected-type c)))))"
                "(handler-case (let ((items (list 1 2 3))) (member-if (lambda (x) (declare (ignore x)) (setf (cdr items) items) nil) items)) (type-error () t))"
                "(let* ((entry (cons nil :found)) (items (list nil entry))) (eq entry (assoc nil items)))"
                "(let ((seen nil)) (assoc-if (lambda (x) (push x seen) nil) '(nil (1 . a) nil (2 . b))) (equal '(1 2) (reverse seen)))"
                "(let ((items (list 1 2 3))) (eq (cddr items) (member 2 items :test #'<)))"
                "(let ((items (list 1 2 3))) (eq (cdr items) (member 3 items :key #'1+)))"
                "(equal '((2 3)) (multiple-value-list (member-if (lambda (x) (values (= x 2) :extra)) '(1 2 3))))"
                "(equal '(2 3) (member 2 '(1 2 3)))"))
      (is (eq t (m:evaluate-string vm source)) "Search boundary mismatch for ~A" source))))
