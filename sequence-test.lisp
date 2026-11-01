;;;; Native sequence loops and their observable Common Lisp semantics.
(in-package #:miricl-test)
(in-suite :miricl)

(test dotted-nconc-and-cons-errors
  (m:with-interpreter (vm)
    (dolist (form '((nconc) (nconc 'tail) (nconc nil nil 'tail)
                    (nconc (list* 1 2 'a) (list* 3 'b) 'end)
                    (nconc (cons 1 'a) nil (cons 2 'b) nil)
                    (nconc nil (cons 1 'a) nil)
                    (let* ((a (list* 1 2 'a)) (b (cons 3 'b))
                           (result (nconc a b 'end)))
                      (list (eq result a) (eq (cddr result) b) result b))
                    (let ((x (list 1)))
                      (nconc x x) (list (car x) (eq x (cdr x))))))
      (is (equalp (eval form) (m:evaluate vm form)) "NCONC mismatch for ~S" form))
    (dolist (operator '(rplaca rplacd))
      (dolist (datum '(nil t 17 #\a "bad" #(1 2)))
        (is (eq t (m:evaluate vm
          `(let ((datum ',datum))
             (handler-case (,operator datum nil)
               (type-error (condition)
                 (and (eq datum (type-error-datum condition))
                      (eq 'cons (type-error-expected-type condition)))))))))))
    (is (eq t (m:evaluate-string vm
      "(handler-case (append (cons 1 'tail) nil) (type-error () t))")))))

(test native-map-into
  (m:with-interpreter (vm)
    (dolist (result-type '(list vector))
      (dolist (input-type '(list vector))
        (loop for result-length from 0 to 5 do
          (loop for input-length from 0 to 5 do
            (let ((form `(let* ((out (make-sequence ',result-type ,result-length :initial-element 0))
                                (input (make-sequence ',input-type ,input-length :initial-element 1))
                                (tail (if (listp out) (cdr out) nil))
                                (result (map-into out #'1+ input)))
                           (list (eq result out) (coerce out 'list)
                             (if (listp out) (eq tail (cdr out)) t) (coerce input 'list)))))
              (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))))
    (dolist (fill '(0 1 5))
      (dolist (input-length '(0 3 8))
        (let ((form `(let* ((out (make-array 5 :initial-element 9 :fill-pointer ,fill))
                            (input (make-list ,input-length :initial-element 1))
                            (result (map-into out #'1+ input)))
                       (list (eq result out) (fill-pointer out)
                         (loop for i below (array-total-size out) collect (aref out i))))))
          (is (equal (eval form) (m:evaluate vm form))))))
    (dolist (form
      '((let ((out (list 0 0 0)) (n 0))
          (list (map-into out (lambda () (incf n))) out n))
        (let ((out (make-array 4 :initial-element 0 :fill-pointer 0)) (n 0))
          (map-into out (lambda () (incf n))) (list (coerce out 'list) (fill-pointer out) n))
        (let ((out (list 1 2 3 4))) (map-into out #'+ out '(10 20)) out)
        (let ((out (make-array 4 :initial-contents '(1 2 3 4) :fill-pointer 2)))
          (map-into out #'1+ out) (list (coerce out 'list) (fill-pointer out) (aref out 2)))
        (let* ((base (vector 1 2 3 4 5))
               (input (make-array 4 :displaced-to base))
               (out (make-array 4 :displaced-to base :displaced-index-offset 1)))
          (map-into out #'1+ input) (coerce base 'list))
        (let* ((base (copy-seq "xabcx"))
               (out (make-array 3 :element-type 'character :displaced-to base :displaced-index-offset 1)))
          (map-into out #'char-upcase '(#\a #\λ #\b)) base)
        (let ((out (copy-seq #*1010))) (map-into out (lambda (x) (if (zerop x) 1 0)) out) (coerce out 'list))
        (let ((out (list 0 0 0)) (saved nil))
          (map-into out (lambda (&rest x) (push x saved) (+ (car x) (cadr x))) '(1 2 3) '(10 20 30))
          (list out saved))
        (multiple-value-list (map-into (list 0 0) (lambda (x) (values (1+ x) :extra)) '(1 2)))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    (dolist (source '("(map-into)" "(map-into nil)" "(map-into '(1) #'cons '(1))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(map-into 1 #'identity nil)" "(map-into (list 0) #'identity 1)"
                      "(map-into '(1 . 2) #'identity '(1 2))"
                      "(let ((s (list 1))) (setf (cdr s) s) (map-into (list 0) #'identity s))"
                      "(map-into (copy-seq #*101) #'1+ '(1 1 1))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (type-error () t))")))))))

(test native-sequence-construction-functions
  (m:with-interpreter (vm)
    (dolist (case '((string (#\a) "a") (copy-seq ("abc") "abc")
                    (subseq ("abc" 1) "bc") (concatenate ('string "a" "b") "ab")))
      (destructuring-bind (name args expected) case
        (is (equal (list expected :changed) (m:evaluate vm
          `(let ((saved (function ,name)))
             (unwind-protect
               (progn (setf (symbol-function ',name) (lambda (&rest x) (declare (ignore x)) :changed))
                      (list (funcall saved ,@args) (,name ,@args)))
               (setf (symbol-function ',name) saved))))))))
    (dolist (source '("(string)" "(string nil nil)" "(copy-seq)" "(copy-seq nil nil)"
                      "(subseq)" "(subseq nil)" "(subseq nil 0 nil nil)" "(concatenate)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (form '((multiple-value-list (string (values #\a 99)))
                    (multiple-value-list (copy-seq (values "abc" 99)))
                    (multiple-value-list (subseq (values "abc" 99) (values 1 99)))
                    (multiple-value-list (concatenate (values 'string 99) (values "abc" 99)))
                    (let ((x (list (list 1))))
                      (let ((y (copy-seq x))) (list (eq x y) (eq (car x) (car y)))))
                    (let ((x (copy-seq "abc")))
                      (let ((y (concatenate 'string x))) (setf (char y 0) #\z) (list x y)))
                    (concatenate '(string 3) "a" "bc")
                    (concatenate 'cons '(1) '(2)) (concatenate 'null)))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-simple-predicates
  (m:with-interpreter (vm)
    (dolist (name '(null consp atom listp stringp identity))
      (dolist (object '(nil t a 0 1 1/2 1.0 #c(1 2) #\a "" "abc" (1) (1 . 2) ((1))))
        (let ((form `(,name ',object)))
          (is (equal (eval form) (m:evaluate vm form)) "Predicate mismatch for ~S" form)))
      (dolist (form `((,name) (,name 1 2)))
        (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
      (let ((form `(multiple-value-list (,name (values '(1 . 2) 99)))))
        ;; CCL can optimize IDENTITY to propagate its argument's extra values.
        (is (equal (list (funcall name '(1 . 2))) (m:evaluate vm form))))
      (let ((form `(let ((x (list 1 2)) (f (function ,name)))
                     (unwind-protect
                       (progn (setf (symbol-function ',name) (lambda (x) (declare (ignore x)) :changed))
                              (list (funcall f x) (,name x)))
                       (setf (symbol-function ',name) f)))))
        ;; Rebinding host CL functions is forbidden; the original result is
        ;; computed independently before rebinding only the guest's cell.
        (is (equal (list (funcall name '(1 2)) :changed) (m:evaluate vm form)))))
    (dolist (name '(null consp atom listp stringp))
      (dolist (operator '(count-if count-if-not remove-if remove-if-not))
        (let ((form `(,operator (function ,name) (list nil 1 'a "abc" '(1 . 2)) :from-end t)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (is (equal '(t t nil nil) (m:evaluate vm
      '(let ((x (list 1))) (setf (cdr x) x) (list (listp x) (consp x) (atom x) (null x))))))
    (is (eq t (m:evaluate vm '(let ((x (list 1))) (eq x (identity x))))))))

(test native-sequence-key-functions
  (m:with-interpreter (vm)
    (dolist (fixture '((1+ (1 2 3 4) 3) (1- (1 2 3 4) 3)
                       (car ((1 . a) (2 . b) (3 . c)) 2)
                       (cdr ((a . 1) (b . 2) (c . 3)) 2)
                       (char-code "abcd" 98) (char-int "aλ🙂b" 98)))
      (destructuring-bind (key sequence item) fixture
        (dolist (operator '(count find position remove delete count-if count-if-not
                            find-if find-if-not position-if position-if-not
                            remove-if remove-if-not delete-if delete-if-not))
          (dolist (from-end '(nil t))
            (let ((form `(,operator ,(if (member operator '(count find position remove delete)) item '(function oddp))
                            (copy-seq ',sequence) :key (function ,key) :from-end ,from-end)))
              (is (equal (eval form) (m:evaluate vm form))))))))
    (is (equal '(1 294 3 297) (m:evaluate vm
      '(let ((key #'char-code))
         (unwind-protect
           (progn (setf (symbol-function 'char-code) (lambda (x) (declare (ignore x)) 99))
             (list (count 97 "abc" :key key) (reduce '+ "abc" :key key)
                   (count 99 "abc" :key 'char-code) (reduce '+ "abc" :key 'char-code)))
           (setf (symbol-function 'char-code) key))))))
    (dolist (operator '(count count-if count-if-not))
      (is (eq t (m:evaluate vm
        `(handler-case
           (let ((x (list 1))) (setf (cdr x) x)
             (,operator ,(if (eq operator 'count) 1 '(function oddp)) x))
           (type-error () t))))))))

(test native-list-accessors
  (m:with-interpreter (vm)
    (let ((names '(first second third fourth fifth rest
                   caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
                   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                   sixth seventh eighth ninth tenth)))
      (labels ((tree (depth index)
                 (if (zerop depth) index
                   (cons (tree (1- depth) (* 2 index)) (tree (1- depth) (1+ (* 2 index))))))
               (sample (name)
                 (if (member name '(sixth seventh eighth ninth tenth))
                   (loop for i from 1 to 20 collect i) (tree 5 1))))
        (dolist (name names)
          (dolist (object (list nil (sample name)))
            (let ((form `(,name ',object)))
              (is (equal (eval form) (m:evaluate vm form)))))
          (dolist (form `((let ((x (copy-tree ',(sample name))))
                           (list (setf (,name x) :changed) x))
                          (let ((seen nil) (x (copy-tree ',(sample name))))
                            (list (setf (,name (progn (push 1 seen) x))
                                    (progn (push 2 seen) :changed)) x seen))))
            (is (equal (eval form) (m:evaluate vm form))))
          (dolist (form `((,name 1) (setf (,name nil) 1)))
            (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t))))))
          (dolist (form `((,name) (,name nil nil)))
            (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))
      (dolist (form '((let ((x (list 1))) (setf (cdr x) x) (eq x (cddddr x)))
                      (let ((x (list 1))) (setf (cdr x) x) (setf (cadddr x) 2) (car x))
                      (cadr '(1 2 . 3)) (cddr '(1 2 . 3))
                      (multiple-value-list (caar (values '((1)) 99)))
                      (let ((x (list 1 2 3)))
                        (multiple-value-list (setf (cadr x) (values 4 5))))))
        (is (equal (eval form) (m:evaluate vm form))))
      (is (equal '(2 :rebound 9) (m:evaluate vm
        '(let ((getter #'cadr) (setter (fdefinition '(setf cadr))) (x (list 1 2 3)))
           (unwind-protect
             (progn (setf (symbol-function 'cadr) (lambda (x) (declare (ignore x)) :rebound))
                    (list (funcall getter x) (cadr x)
                      (progn (funcall setter 9 x) (funcall getter x))))
             (setf (symbol-function 'cadr) getter)))))))))

(test native-reduce
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (sequence '(nil (1) (1 2 3 4) "" "a" "aλ🙂b"))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence) do
          (dolist (from-end '(nil t))
            (dolist (initial '(nil (:initial-value nil) (:initial-value (quote z))))
              (dolist (key '(nil identity))
                (let ((form `(reduce 'list ',sequence :start ,start :end ,end
                                :from-end ,from-end :key ',key ,@initial)))
                  (is (equal (eval form) (m:evaluate vm form))))))))))
    (dolist (form '((reduce '+ nil) (reduce '* nil)
                    (reduce '- '(1 2 3 4)) (reduce '- '(1 2 3 4) :from-end t)
                    (reduce '+ '(1 2 3) :key '1+ :initial-value 7)
                    (let ((seen nil) (calls nil))
                      (list (reduce (lambda (a b) (push (list a b) calls) (list a b))
                              '(1 2 3) :initial-value 'z
                              :key (lambda (x) (push x seen) (values x 'ignored))) seen calls))
                    (let ((seen nil) (calls nil))
                      (list (reduce (lambda (a b) (push (list a b) calls) (list a b))
                              '(1 2 3) :initial-value 'z :from-end t
                              :key (lambda (x) (push x seen) (values x 'ignored))) seen calls))
                    (let ((args nil) (keys nil))
                      (list (reduce (lambda (&rest x) (push x args) (car x)) '(1 2 3 4)
                              :key (lambda (&rest x) (push x keys) (car x))) args keys))
                    (let ((x (list 1 2 3)))
                      (list (reduce 'cons x) x (reduce 'cons x :from-end t) x))
                    (let ((x (list 1))) (eq x (reduce 'cons (list x))))
                    (let ((x (list 1))) (eq x (reduce 'cons nil :initial-value x)))
                    (multiple-value-list (reduce 'values '(1 2 3)))
                    (multiple-value-list (reduce 'values '(1) :key (lambda (x) (values x 2))))
                    (multiple-value-list (reduce (lambda (a b) (declare (ignore a b)) (values)) '(1 2)))
                    (reduce 'cons '(1 2 3) :from-end t :from-end nil
                      :initial-value nil :initial-value 'z)
                    (reduce '+ '(1 2 3) :start 0 :start 2 :end nil :end 0 :key nil :key '1+)
                    (reduce '+ '(1 2 3) :bad 1 :allow-other-keys t :allow-other-keys nil)
                    (let ((seen nil))
                      (list (reduce (progn (push 1 seen) 'cons)
                              (progn (push 2 seen) '(a b c))
                              :from-end (progn (push 3 seen) t)
                              :key (progn (push 4 seen) 'identity)
                              :initial-value (progn (push 5 seen) nil)) seen))
                    (reduce '+ (make-list 20000 :initial-element 1))
                    (reduce '+ (make-list 20000 :initial-element 1) :from-end t)))
      (is (equal (eval form) (m:evaluate vm form))))
    ;; REDUCE returns one result even if its reducing function returns none.
    ;; ECL propagates zero values from the empty-sequence callback instead.
    (dolist (form '((multiple-value-list (reduce 'values nil))
                    (multiple-value-list (reduce (lambda () (values)) nil))))
      (is (equal '(nil) (m:evaluate vm form))))
    (is (equal '(6 (1 2 . 3) :rebound) (m:evaluate vm
      '(let ((add #'+) (pair #'cons))
         (unwind-protect
           (progn
             (setf (symbol-function '+) (lambda (&rest x) (declare (ignore x)) :rebound)
                   (symbol-function 'cons) (lambda (&rest x) (declare (ignore x)) :rebound))
             (list (reduce add '(1 2 3)) (reduce pair '(1 2 3) :from-end t)
                   (reduce '+ '(1 2 3))))
           (setf (symbol-function '+) add (symbol-function 'cons) pair))))))
    (dolist (source '("(reduce)" "(reduce '+)" "(reduce '+ nil :start)"
                      "(reduce '+ nil :unknown t)"
                      "(reduce '+ nil :unknown t :allow-other-keys nil :allow-other-keys t)"
                      "(reduce 'identity '(1 2))" "(reduce 'cons '(1 2) :key 'cons)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(reduce '+ 1)" "(reduce '+ '(1 . 2))"
                      "(let ((x (list 1))) (setf (cdr x) x) (reduce '+ x))"
                      "(reduce '+ '(1 2) :start nil)" "(reduce '+ '(1 2) :start -1)"
                      "(reduce '+ '(1 2) :end 3)" "(reduce '+ '(1 2) :start 2 :end 1)"
                      "(reduce '+ '(1 2) :key 'car)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))

(test parameterized-cons-types
  (m:with-interpreter (vm)
    (dolist (object '(nil 1 "a" #\a (nil . nil) (1 . 2) (1 . "a") ("a" . 1)
                      (1 2) (1 2 3) ((1 . 2) . (3 . 4))))
      (dolist (type '(atom cons (cons) (cons *) (cons * *) (cons integer)
                      (cons integer integer) (cons integer list) (cons integer null)
                      (cons * nil) (cons nil *) (cons nil nil)
                      (cons integer (cons integer null))
                      (cons (cons integer integer) (cons integer integer))
                      (cons (or null integer) (or null string))
                      (or null (cons integer list)) (and cons (not (cons integer integer)))))
        (is (eql (typep object type) (m:evaluate vm `(typep ',object ',type))))))
    (dolist (form '((coerce '(1 2) '(cons integer (cons integer null)))
                    (let ((value (list 1 2)))
                      (eq value (coerce value '(cons integer (cons integer null)))))
                    (let ((value '(1))) (declare (type (cons integer null) value)) value)))
      (is (equal (eval form) (m:evaluate vm form))))
    ;; COERCE's result must satisfy the requested type. SBCL currently leaves
    ;; specialized CONS constraints unchecked, so assert the required error.
    (is (eq t (m:evaluate vm
      '(handler-case (coerce '(a) '(cons integer null)) (type-error () t)))))))

(test logical-and-singleton-types
  (m:with-interpreter (vm)
    (dolist (object '(nil t * 0 1 1.0 1.0d0 1/2 #\a #\A (1 . 2)))
      (dolist (type '((not cons) (not atom) (not nil) (not t)
                      (not (or integer character)) (and atom (not integer))
                      (eql nil) (eql *) (eql 1) (eql 1.0) (eql 1.0d0) (eql 1/2) (eql #\a)
                      (member) (member nil) (member nil * 1 1.0 #\a) (member 1 1 1)
                      (not (member 1 #\a)) (cons (member 1 2) (eql 2))))
        (is (eql (typep object type) (m:evaluate vm `(typep ',object ',type))))))
    (is (equal '(t nil t nil) (m:evaluate vm
      '(let ((first (list 1)) (second (list 1)))
         (list (typep first (list 'eql first)) (typep second (list 'eql first))
               (typep first (list 'member first)) (typep second (list 'member first)))))))
    (dolist (source '("(typep nil 'eql)" "(typep nil 'member)" "(typep nil 'not)"
                      "(typep nil '(eql))" "(typep nil '(not))" "(typep nil '(not t nil))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (error () t))")))))))

(test native-character-sequence-scans
  (m:with-interpreter (vm)
    (dolist (text '("" "abcaa" "aAλΛ🙂"))
      (loop for start from 0 to (length text) do
        (loop for end from start to (length text) do
          (dolist (from-end '(nil t))
            (dolist (operator '(count find position))
              (dolist (character '(#\a #\λ #\🙂))
                (dolist (test '(eql char= char-equal))
                  (dolist (keyword '(:test :test-not))
                    (let ((form `(,operator ,character ,text :start ,start :end ,end
                                    :from-end ,from-end ,keyword (function ,test))))
                      (is (equal (eval form) (m:evaluate vm form))))))))))))
    (dolist (operator '(count find position))
      (dolist (keyword '(:test :test-not))
        (dolist (item '(nil 17))
          (let ((form `(,operator ,item "abc" :start 1 :from-end t ,keyword #'eql)))
            (is (equal (eval form) (m:evaluate vm form)))))))
    (dolist (form '((let ((seen nil))
                     (list (count-if (lambda (x) (push x seen) t) '(1 2 3) :from-end t) seen))
                    (let ((seen nil))
                      (list (count 1 '(1 2 3) :key (lambda (x) (push x seen) x) :from-end t) seen))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-sequence-keywords
  (m:with-interpreter (vm)
    (dolist (operator '(count count-if count-if-not find find-if find-if-not position position-if position-if-not
                        remove remove-if remove-if-not delete delete-if delete-if-not))
      (let ((argument (if (member operator '(count find position remove delete)) 1 '(function oddp))))
        (dolist (keywords '((:start 0 :start 2 :end nil :end 0)
                            (:start 2 :start 0)
                            (:key nil :key (function 1+))
                            (:from-end nil :from-end t)
                            (:from-end t :from-end nil)
                            (:unknown 17 :allow-other-keys t)
                            (:allow-other-keys t :allow-other-keys nil :unknown 17)))
          (let ((form `(,operator ,argument (list 1 2 1 3 1) ,@keywords)))
            (is (equal (eval form) (m:evaluate vm form)))))
        (let ((form `(let ((seen nil))
                       (list (,operator (progn (push 1 seen) ,argument)
                               (progn (push 2 seen) (list 1 2 1))
                               :start (progn (push 3 seen) 0)
                               :from-end (progn (push 4 seen) t)) seen))))
          (is (equal (eval form) (m:evaluate vm form))))
        (dolist (keywords '((:unknown t) (:start) (:start 0 :end)
                            (:unknown t :allow-other-keys nil :allow-other-keys t)))
          (is (eq t (m:evaluate vm
            `(handler-case (,operator ,argument (list 1 2 3) ,@keywords) (program-error () t))))))
        (is (eq t (m:evaluate vm
          `(handler-case (,operator ,argument (list 1 2) :start nil :start 0)
             (type-error () t)))))))
    (dolist (operator '(count find position remove delete))
      (dolist (keywords '((:test (function eql) :test (function <))
                          (:test-not (function eql) :test-not (function <))))
        (let ((form `(,operator 1 (list 1 2 1) ,@keywords)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (dolist (operator '(count-if count-if-not find-if find-if-not position-if position-if-not
                        remove-if remove-if-not delete-if delete-if-not))
      (let ((form `(,operator #'oddp (list 1 2 3) :test 42 :test-not 43 :allow-other-keys t)))
        (is (equal (eval form) (m:evaluate vm form)))))))

(test native-sequence-search
  (m:with-interpreter (vm)
    (dolist (sequence '((1 2 1 3 1) "aBabA"))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence) do
          (dolist (from-end '(nil t))
            (dolist (operator '(find position find-if position-if find-if-not position-if-not))
              (let* ((item-p (member operator '(find position)))
                     (argument (if item-p (if (stringp sequence) #\a 1)
                       (list 'function (if (stringp sequence) 'upper-case-p 'oddp))))
                     (form `(,operator ,argument ',sequence :start ,start :end ,end :from-end ,from-end)))
                (is (equal (eval form) (m:evaluate vm form)))))))))
    (dolist (form
      '((position nil '(nil 1 2))
        (position nil "abc")
        (position 2 '(1 2 3 2 4) :test #'<)
        (position 2 '(1 2 3 2 4) :test-not #'< :from-end t)
        (find #\a "Abλa🙂" :test #'char-equal)
        (find #\a "Abλa🙂" :test #'char-equal :from-end t)
        (find #\a "Abλa🙂" :test #'char=)
        (find #\a "Abλa🙂" :key #'identity :test #'char-equal)
        (find nil "" :test #'char=)
        (find '(2) '((1) (2) (3)) :key #'car :test (lambda (item value) (= (car item) value)))
        (let* ((sequence (list (list 1) (list 2) (list 3)))
               (found (find 2 sequence :key #'car)))
          (eq found (second sequence)))
        (position-if #'evenp '((1) (2) (3) (4)) :key 'car :from-end t :start 1)
        (multiple-value-list (find-if (lambda (x) (values (oddp x) 99)) '(1 2 3)))
        (multiple-value-list (position-if (lambda (x) (values (oddp x) 99)) '(1 2 3)))
        (let ((seen nil))
          (let ((position (position-if (lambda (x) (push x seen) (= x 2)) '(0 1 2 3)
                            :start 1 :end 3 :from-end t)))
            (list position (not (null (member 2 seen))) (every (lambda (x) (<= 1 x 2)) seen))))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (source '("(find)" "(position 1)" "(find-if #'oddp nil :unknown t)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(find 1 42)" "(position 1 '(1 . 2))" "(find 1 nil :end 1)"
                      "(find nil \"abc\" :test #'char=)"
                      "(position 1 '(1 2) :start 2 :end 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-sequence-removal
  (m:with-interpreter (vm)
    (dolist (sequence '((1 2 1 3 1) "aBabA"))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence) do
          (dolist (count '(nil -1 0 1 2 1000000000000000000000))
            (dolist (from-end '(nil t))
              (dolist (operator '(remove delete remove-if delete-if remove-if-not delete-if-not))
                (let* ((item-p (member operator '(remove delete)))
                       (argument (if item-p (if (stringp sequence) #\a 1)
                         (list 'function (if (stringp sequence) 'upper-case-p 'oddp))))
                       (form `(let ((s (copy-seq ',sequence)))
                                (,operator ,argument s :start ,start :end ,end
                                  :count ,count :from-end ,from-end))))
                  (is (equal (eval form) (m:evaluate vm form))))))))))
    (dolist (form
      '((remove 2 '(1 2 3 2 4) :test #'<)
        (remove 2 '(1 2 3 2 4) :test-not #'<)
        (remove 2 '((1) (2) (3) (2)) :key #'car)
        (remove-if #'evenp '((1) (2) (3) (4)) :key 'car :from-end t :count 1)
        (remove '(2) '((1) (2) (3)) :key #'car :test (lambda (item value) (= (car item) value)))
        (remove #\a "aλAa🙂" :test #'char-equal :from-end t :count 2)
        (remove #\a "aAaλ" :test #'char=)
        (remove #\a "aAaλ" :test-not #'char=)
        (remove #\a "aAaλ" :test-not #'char-equal)
        (remove #\a "aAaλ" :key #'identity :test #'char-equal)
        (remove nil "abc" :test #'char-equal :count 0)
        (remove nil "" :test #'char=)
        (remove nil "abc")
        (remove nil "abc" :test-not #'eql)
        (let ((x (list (list 1) (list 2) (list 1))))
          (list (remove-if (lambda (x) (= (car x) 1)) x) x))
        (multiple-value-list (remove-if (lambda (x) (values (oddp x) 99)) '(1 2 3)))
        (let ((s (copy-seq "abc"))) (list (string= s (remove #\z s)) s))
        (let ((s (list 1 2 3))) (list (eq s (delete 2 s)) s))
        (let ((calls 0))
          (list (remove-if (lambda (x) (incf calls) t) '(1 2 3) :count -1) calls))
        (let ((saved nil))
          (list (remove-if (lambda (&rest args) (push args saved) (oddp (car args))) '(1 2 3)) saved))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (from-end '(nil t))
      (dolist (count '(nil 0 1 31 32 33 64))
        (dolist (operator '(remove-if delete-if))
          (let ((form `(,operator #'oddp (loop for i below 70 collect i)
                          :start 2 :end 68 :count ,count :from-end ,from-end)))
            (is (equal (eval form) (m:evaluate vm form)))))))
    (is (eq t (m:evaluate vm
      '(let ((cycle (list 1 2 3)))
         (setf (cdr (last cycle)) cycle)
         (handler-case (remove 1 cycle) (type-error (c)
           (not (typep (type-error-datum c) (type-error-expected-type c)))))))))
    (dolist (source '("(remove)" "(remove 1)" "(delete-if #'oddp nil :unknown t)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(remove 1 42)" "(remove 1 '(1 . 2))" "(remove 1 nil :count 1/2)"
                      "(remove nil \"abc\" :test #'char=)" "(remove nil \"abc\" :test #'char-equal)"
                      "(remove 1 '(1 2) :start 2 :end 1)" "(remove 1 nil :end 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-real-coercion
  (m:with-interpreter (vm)
    (dolist (number (list 0 -1 1/3 (1+ (expt 2 24)) (1+ (expt 2 53))
                         2147483647 2147483648 -2147483648 -2147483649
                         2147483646.5d0 -2147483647.5d0
                         0.0 -0.0 1.5 0.0d0 -0.0d0 1.5d0
                         least-positive-single-float))
      (dolist (type '(float short-float single-float double-float long-float))
        (let ((form `(let ((result (coerce ,number ',type)))
                       (list (rational result) (typep result ',type)
                             (multiple-value-list (eql result (coerce ,number ',type)))))))
          ;; The guest uses binary64 for LONG-FLOAT on every host.
          (is (equal (eval `(let ((result (coerce ,number ',(if (eq type 'long-float) 'double-float type))))
                             (list (rational result) t (list t))))
                     (m:evaluate vm form)) "COERCE mismatch for ~S" form))))
    ;; Just above a binary32 midpoint: converting via binary64 first loses
    ;; the low bit and rounds to 1.0 on some hosts.
    (let ((number (/ (+ (expt 2 80) (expt 2 56) 1) (expt 2 80))))
      (is (= 8388609/8388608 (m:evaluate vm `(rational (coerce ,number 'single-float)))))
      (is (= 16777217/16777216 (m:evaluate vm `(rational (coerce ,number 'double-float))))))
    (is (eq t (m:evaluate vm '(let ((x 1.0d0))
                               (and (eq x (coerce x 'float)) (eq x (coerce x 'double-float)))))))
    (dolist (source '("(coerce)" "(coerce 1)" "(coerce 1 'float nil)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(coerce 'bad 'float)" "(coerce #c(1 2) 'single-float)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test sequence-result-class-and-alias-types
  (m:with-interpreter (vm)
    (dolist (name '(list cons null vector string bit-vector))
      (dolist (size (if (eq name 'null) '(0) (if (eq name 'cons) '(1 3) '(0 1 3))))
        (let* ((element (case name (string #\a) (bit-vector 1) (otherwise nil)))
               (form `(let* ((type (find-class ',name))
                             (result (make-sequence type ,size :initial-element ',element)))
                        (list (length result) (typep result type)
                              (length (multiple-value-list result))))))
          (is (equal (list size t 1) (m:evaluate vm form)) "Sequence class mismatch ~S" form))))
    (dolist (source
      '("(deftype result-bits () '(simple-array bit (3)))
         (equalp #*111 (make-sequence 'result-bits 3 :initial-element 1))"
        "(deftype result-text () '(vector character 3))
         (string= \"abc\" (concatenate 'result-text \"a\" \"bc\"))"
        "(string= \"abc\" (concatenate (find-class 'string) \"a\" \"bc\"))"
        "(equalp #*101 (map (find-class 'bit-vector) #'identity '(1 0 1)))"
        "(equal '(1 2 3) (map (find-class 'list) #'1+ #(0 1 2)))"
        "(string= \"abc\" (merge (find-class 'string) \"ac\" \"b\" #'char<))"
        "(equal '(1 2 3) (merge (find-class 'cons) '(1 3) #(2) #'<))"
        "(string= \"abc\" (coerce '(#\\a #\\b #\\c) (find-class 'string)))"
        "(equal '(#\\a #\\b) (coerce \"ab\" (find-class 'list)))"
        "(typep (coerce 17 (find-class 'double-float)) 'double-float)"
        "(handler-case (make-sequence (find-class 'null) 1) (type-error () t))"
        "(handler-case (make-sequence (find-class 'cons) 0) (type-error () t))"
        "(handler-case (make-sequence 'result-bits 2) (type-error () t))"
        "(handler-case (make-sequence (find-class 'integer) 0) (type-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Sequence result type mismatch ~A" source))))

(test native-sequence-construction
  (m:with-interpreter (vm)
    (dolist (sequence '(nil (a b c d) "" "a" "aλ🙂b"))
      (loop for start from 0 to (length sequence) do
        (loop for end from start to (length sequence)
              for form = `(subseq ',sequence ,start ,end) do
          (is (equal (eval form) (m:evaluate vm form)) "SUBSEQ mismatch for ~S" form))))
    (dolist (form
      '((concatenate 'string)
        (concatenate 'list)
        (concatenate 'string "ab" '(#\λ #\🙂) "cd")
        (concatenate 'list '(a b) "λ🙂" '(c d))
        (concatenate '(string 4) "ab" '(#\c #\d))
        (concatenate '(simple-string 4) "ab" "cd")
        (coerce '(#\a #\λ #\🙂) 'string)
        (coerce "λ🙂" 'list)
        (coerce '(#\a #\b) '(string 2))
        (coerce '(#\a #\b) '(vector character 2))
        (coerce '(#\a #\b) '(simple-array character (2)))
        (concatenate '(simple-array character (3)) "ab" '(#\c))
        (let ((s (copy-seq "abc"))) (eq s (coerce s 'simple-string)))
        (let ((s (list 'a 'b))) (eq s (coerce s 'list)))
        (let ((s (copy-seq "abc"))) (eq s (concatenate 'string s)))
        (let ((s (list 'a 'b))) (eq s (concatenate 'list s)))
        (let ((s (copy-seq "abc"))) (eq s (subseq s 0)))
        (let ((s (list 'a 'b))) (eq s (subseq s 0)))
        (make-string 4 :initial-element #\λ)
        (make-string 4 :initial-element #\a :element-type 'standard-char)
        (make-string 0 :element-type nil)
        (make-string 2 :initial-element #\a :initial-element #\b)
        (make-string 2 :initial-element #\a :unused t :allow-other-keys t)
        (length (make-string 4))
        (coerce 7 'single-float)
        (coerce 7 'double-float)
        (coerce 1.5d0 'single-float)
        (coerce 7/2 'complex)
        (coerce 1.5d0 'complex)
        (coerce nil t)
        (funcall (coerce 'car 'function) '(a b))
        (funcall (coerce '(lambda (x) (+ x 1)) 'function) 41)
        (mapcar #'character '(#\a "a" a))
        (list (string nil) (string #\λ) (string 'abc))
        (multiple-value-list (coerce "ab" 'list))
        (multiple-value-list (subseq "ab" 1))
        (multiple-value-list (make-string 1 :initial-element #\a))))
      (is (equal (eval form) (m:evaluate vm form)) "Construction mismatch for ~S" form))
    (dolist (source '("(subseq 1 0)" "(subseq '(a . b) 0)" "(subseq \"abc\" -1)"
                      "(subseq \"abc\" 2 1)" "(subseq \"abc\" 0 4)"
                      "(subseq \"abc\" 1000000000000000000000)"
                      "(make-string -1)" "(make-string array-dimension-limit)"
                      "(make-string 1 :initial-element 1)"
                      "(make-string 1 :element-type 'integer)"
                      "(coerce \"abc\" '(string 2))" "(coerce '(#\\a) '(string 2))"
                      "(coerce \"abc\" '(vector * 2))"
                      "(coerce '(1 2) 'string)" "(coerce 1 'string)"
                      "(coerce \"a\" nil)" "(coerce 1.5 'integer)"
                      "(character 'apple)" "(character 65)"
                      "(concatenate '(string 1) \"ab\")" "(concatenate 'string '(1))"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-subsequence-assignment
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((s (copy-seq "abcdef"))) (list (setf (subseq s 1 4) "XY") s))
        (let ((s (copy-seq "abcdef"))) (list (setf (subseq s 4) "XYZ") s))
        (let ((s (copy-seq "abcdef"))) (setf (subseq s 1 5) s) s)
        (let ((s (list 'a 'b 'c 'd 'e))) (setf (subseq s 1 4) s) s)
        (let ((s (list 'a 'b 'c 'd 'e))) (setf (subseq s 0 4) (cdr s)) s)
        (let ((s (list 'a 'b 'c))) (list (setf (subseq s 1) "λ🙂") s))
        (let ((s (copy-seq "abc"))) (list (setf (subseq s 1) '(#\λ #\🙂)) s))
        (let ((s (copy-seq "abc")) (new "XYZ"))
          (eq new (setf (subseq s 2 2) new)))
        (let ((s (copy-seq "abc")) (i 0))
          (setf (subseq (progn (incf i) s) (progn (incf i) 1) (progn (incf i) 2))
                (progn (incf i) "X")) (list s i))))
      (is (equal (eval form) (m:evaluate vm form)) "Assignment mismatch for ~S" form))))

(test predicating-and-string-types
  (m:with-interpreter (vm)
    (dolist (form
      '((typep "abc" '(string 3)) (typep "abc" '(string 2))
        (typep "abc" '(simple-string *))
        (typep "abc" '(simple-string 3))
        (typep #\a 'standard-char) (typep #\Newline 'standard-char)
        (typep #\Tab 'standard-char)
        (typep 4 '(satisfies evenp)) (typep 3 '(satisfies evenp))
        (typep 3 '(and integer (satisfies oddp)))))
      (is (equal (eval form) (m:evaluate vm form))))
    (is (eq t (m:evaluate vm '(typep #\λ 'base-char))))
    (is (eq t (m:evaluate vm '(typep "λ🙂" 'simple-base-string))))
    (dolist (type '((vector character 3) (vector character 2) (vector * 3) (vector t 3)
                    (array character 1) (array character 2) (array * (3))
                    (array * (2)) (array * nil) (array * (* *))
                    (simple-array character (3))))
      (let ((form `(typep (make-string 3 :initial-element #\a :element-type 'character) ',type)))
        (is (equal (eval form) (m:evaluate vm form)) "Array type mismatch for ~S" type)))
    (is (eq t (m:evaluate vm
      '(typep (copy-seq (make-string 0 :element-type nil)) '(simple-array nil (0))))))
    (is (eq t (m:evaluate-string vm "
      (defun predicate-values (x) (values x 99))
      (equal '(t) (multiple-value-list (typep 1 '(satisfies predicate-values))))")))))

(test native-string-comparisons
  (m:with-interpreter (vm)
    (dolist (operator '(string= string/= string< string> string<= string>=
                        string-equal string-not-equal string-lessp string-greaterp
                        string-not-greaterp string-not-lessp))
      ;; Compare all valid substring boundaries with SBCL. In particular, an
      ;; inequality returns an absolute index in the first string, including
      ;; its END1 for equal substrings and proper prefixes.
      (dolist (pair '(("Abc" "abc") ("abc" "abd") ("abcd" "abc")
                      ("" "a") ("a" "") ("Éλ" "éΛ") ("𐐀a" "𐐨b")))
        (destructuring-bind (a b) pair
          (loop for start1 from 0 to (length a) do
            (loop for end1 from start1 to (length a) do
              (loop for start2 from 0 to (length b) do
                (loop for end2 from start2 to (length b)
                      for form = `(,operator ,a ,b :start1 ,start1 :end1 ,end1
                                      :start2 ,start2 :end2 ,end2) do
                  (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))))
      (dolist (arguments '(("abc" "ABC" :start1 0 :start1 2)
                           ("abc" "abc" :end1 nil :end1 0)
                           ("abc" "abc" :start2 0 :start2 nil)
                           ("abc" "abc" :end2 nil :end2 0)
                           ("abc" "abc" :unknown t :allow-other-keys t)
                           ("abc" "abc" :allow-other-keys t :allow-other-keys nil :unknown t)
                           ('abc #\a) (nil "nil") (#\a "a")))
        (let ((form `(multiple-value-list (,operator ,@arguments))))
          (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))
      (dolist (arguments '(nil (:start1 1 :start2 1) (:start1 2 :start2 2 :end1 2 :end2 2)))
        (let ((form `(let ((s (copy-seq "abc")))
                       (multiple-value-list (,operator s s ,@arguments)))))
          (is (equal (eval form) (m:evaluate vm form)) "Identity mismatch for ~S" form)))
      (is (equal '(t t 6) (m:evaluate vm
        `(let ((a (copy-seq "abc")) (b (copy-seq "abc")) (i 0))
           (,operator (progn (incf i) a) (progn (incf i) b)
              :start1 (progn (incf i) 1) :end1 (progn (incf i) 3)
              :start2 (progn (incf i) 1) :end2 (progn (incf i) 3))
           (list (equal a "abc") (equal b "abc") i)))))
      ;; Validation must run even if a different argument already determines
      ;; the comparison result. Huge bounds must not wrap into the valid range.
      (dolist (arguments '("1 \"a\"" "\"a\" '(a)"
                           "\"abc\" \"x\" :start1 nil"
                           "\"abc\" \"x\" :start2 -1"
                           "\"abc\" \"x\" :end1 4"
                           "\"abc\" \"x\" :start1 2 :end1 1"
                           "\"abc\" \"x\" :end2 1.0"
                           "\"\" \"\" :end1 1000000000000000000000"))
        (is (eq t (m:evaluate-string vm (format nil
          "(handler-case (~A ~A) (type-error (c)
             (not (typep (type-error-datum c) (type-error-expected-type c)))))"
          operator arguments)))))
      (dolist (arguments '("" "\"a\"" "\"a\" \"b\" :start1"
                           "\"a\" \"b\" :unknown t"
                           "\"a\" \"b\" 1 nil"
                           "\"a\" \"b\" :allow-other-keys nil :allow-other-keys t :unknown t"))
        (is (eq t (m:evaluate-string vm (format nil
          "(handler-case (~A ~A) (program-error () t))" operator arguments))))))))

(test native-string-equal
  (m:with-interpreter (vm)
    (dolist (form
      '((string-equal "" "")
        (string-equal "abc" "AbC")
        (string-equal "abc" "abcd")
        (string-equal "AbC" "abD")
        (string-equal "a" "")
        (string-equal 'abc "aBc")
        (string-equal #\a 'a)
        (string-equal nil "nil")
        (string-equal "ÉΛЖ🙂" "éλж🙂")
        (string-equal "𐐀" "𐐨")
        (string-equal "prefixABCtail" "abc" :start1 6 :end1 9)
        (string-equal "abc" "prefixABCtail" :start2 6 :end2 9)
        (string-equal "abc" "abC" :end1 nil :end2 nil)
        (string-equal "abc" "x" :start1 2 :end1 2 :start2 1 :end2 1)
        (string-equal "abc" "ABC" :start1 3 :start2 3)
        (string-equal "abc" "ABC" :start1 0 :start1 2)
        (string-equal "abc" "ABC" :unused t :allow-other-keys t)
        (length (multiple-value-list (string-equal "abc" "ABC")))
        (length (multiple-value-list (string-equal "a" "bc")))
        (let ((a (copy-seq "AbC")) (b (copy-seq "aBc")))
          (string-equal a b) (list a b))
        (let ((i 0))
          (list (string-equal (progn (incf i) "abc") (progn (incf i) "ABC")
                  :start1 (progn (incf i) 0) :end1 (progn (incf i) 3)
                  :start2 (progn (incf i) 0) :end2 (progn (incf i) nil)) i))))
      (is (equal (eval form) (m:evaluate vm form)) "String equality mismatch for ~S" form))
    ;; Every CHAR-UPCASE pair must compare equally in both directions,
    ;; including titlecase, sparse ranges, and supplementary characters.
    (let ((original (coerce (loop for code below (min char-code-limit #x110000)
                                 for char = (code-char code)
                                 when (and char (char/= char (char-upcase char))) collect char) 'string)))
      (dolist (pair (list (list original (string-upcase original))
                         (list (string-upcase original) original)))
        (is (eq t (m:evaluate vm `(string-equal ,@pair))))))
    (dolist (source '("(string-equal 1 \"a\")" "(string-equal \"a\" '(a))"
                      "(string-equal \"abc\" \"x\" :start1 -1)"
                      "(string-equal \"abc\" \"x\" :start2 nil)"
                      "(string-equal \"abc\" \"x\" :end1 4)"
                      "(string-equal \"abc\" \"x\" :end2 2)"
                      "(string-equal \"abc\" \"x\" :start1 2 :end1 1)"
                      "(string-equal \"abc\" \"x\" :start2 2)"
                      "(string-equal \"\" \"\" :end1 1000000000000000000000)"
                      "(string-equal \"abc\" \"x\" :end1 1.0)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))
    (dolist (source '("(string-equal)" "(string-equal \"a\")"
                      "(string-equal \"a\" \"b\" :start1)"
                      "(string-equal \"a\" \"b\" :unknown t)"
                      "(string-equal \"a\" \"b\" :allow-other-keys nil :allow-other-keys t :unknown t)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))))

(test native-list-set-semantics
  (m:with-interpreter (vm)
    (dolist (form
      '((adjoin 2 '(1 2 3) :key nil)
        (adjoin 4 '(1 2 3))
        (adjoin '(2 . new) '((1 . a) (2 . b)) :key 'car)
        (adjoin 1000000000000 (list 1 (1- 1000000000001)))
        (adjoin 10 '(1 2 3) :test '<)
        (adjoin 10 '(1 2 3) :test-not '<)
        (subsetp '((1 . a) (2 . b)) '((2 . z) (1 . y)) :key 'car)
        (subsetp '(1 2) '(3 4) :test '<)
        (subsetp '(3 4) '(1 2) :test '<)
        (subsetp '(1 2) '(3 4) :test-not '>=)
        (subsetp nil nil)
        (subsetp '(1) nil)
        (let* ((x (list 1 2 3)) (y (adjoin 2 x))) (eq x y))
        (let* ((x (list 1 2 3)) (y (adjoin 4 x))) (eq x (cdr y)))
        (multiple-value-list (subsetp '(1) '(2) :test (lambda (a b) (values (< a b) 99))))))
      (is (equal (eval form) (m:evaluate vm form)) "List set mismatch for ~S" form))
    (dolist (form
      '((intersection '(1 2 3) '(2 3 4))
        (intersection '(1 2) '(3 4) :test '<)
        (intersection '(3 4) '(1 2) :test '<)
        (intersection '(1 2 3) '(2 3) :test-not '/=)
        (set-difference '(1 2 3) '(2 3 4))
        (set-difference '(1 2) '(3 4) :test '<)
        (set-difference '(3 4) '(1 2) :test '<)
        (set-difference '((1 . a) (2 . b)) '((2 . z)) :key 'car)
        (union '(1 2 3) '(2 3 4))
        (union '(1 2 3) '(2 3 4) :key nil)
        (union nil nil)))
      (is (null (set-exclusive-or (eval form) (m:evaluate vm form) :test #'equal))
          "Set membership mismatch for ~S" form))
    (is (equal '(2) (mapcar #'car (m:evaluate vm
      '(intersection '((1 . a) (2 . b)) '((2 . z)) :key 'car)))))
    (is (eq t (m:evaluate-string vm "
      (block fail
        (union '(1 2) '(3 4) :test
          (lambda (a b) (unless (and (member a '(1 2)) (member b '(3 4)))
                          (return-from fail nil)) (< a b))) t)")))
    (dolist (operator '(intersection set-difference union))
      (is (equal '(t t) (m:evaluate-string vm (format nil "
        (let ((a (list 1 2 3)) (b (list 2 3 4)))
          (~A a b)
          (list (equal a '(1 2 3)) (equal b '(2 3 4))))" operator)))))
    (dolist (source '("(adjoin 1 2)" "(subsetp 1 nil)" "(subsetp nil 2)"
                      "(intersection '(1 . 2) '(3))" "(set-difference '(1) 2)"
                      "(union 1 '(2))"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-exclusive-sets
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (operator '(set-exclusive-or nset-exclusive-or))
      (dolist (left '(nil (1) (1 2) (2 3) (1 1 2 3)))
        (dolist (right '(nil (1) (2 3) (3 4) (1 2 2 4)))
          (dolist (test '(eql equalp < > <= /=))
            (dolist (negate '(nil t))
              (dolist (key '(nil car))
                (let* ((a (if key (mapcar (lambda (x) (cons x :left)) left) left))
                       (b (if key (mapcar (lambda (x) (cons x :right)) right) right))
                       (extract (or key #'identity))
                       (matches (lambda (x y)
                         (let ((result (funcall test (funcall extract x) (funcall extract y))))
                           (if negate (not result) result))))
                       (expected (append
                         (remove-if (lambda (x) (some (lambda (y) (funcall matches x y)) b)) a)
                         (remove-if (lambda (y) (some (lambda (x) (funcall matches x y)) a)) b)))
                       (form `(,operator (copy-tree ',a) (copy-tree ',b) :key ',key
                                 ,(if negate :test-not :test) (function ,test)))
                       (actual (m:evaluate vm form)))
                  (is (= (length expected) (length actual)) "Set cardinality mismatch for ~S" form)
                  (is (null (set-exclusive-or expected actual :test #'equal))
                      "Set membership mismatch for ~S" form))))))))
    (dolist (operator '(nintersection nset-difference nunion set-exclusive-or nset-exclusive-or))
      (let ((form `(,operator (loop for i below 64 collect i) (loop for i from 32 below 96 collect i))))
        (is (null (set-exclusive-or (eval form) (m:evaluate vm form))))))
    (is (eq t (m:evaluate vm
      '(block done
         (set-exclusive-or '(1 2) '(3 4) :test
           (lambda (a b) (unless (and (member a '(1 2)) (member b '(3 4))) (return-from done nil)) (< a b))) t))))
    (is (eq t (m:evaluate vm
      '(let ((saved #'set-exclusive-or))
         (unwind-protect
           (progn (setf (symbol-function 'set-exclusive-or) (lambda (&rest args) (declare (ignore args)) nil))
                  (let ((out (funcall saved '(1 2) '(2 3))))
                    (and (= (length out) 2) (member 1 out) (not (null (member 3 out))))))
           (setf (symbol-function 'set-exclusive-or) saved))))))
    (dolist (operator '(nintersection nset-difference nunion set-exclusive-or nset-exclusive-or))
      (dolist (arguments '(nil (nil) (nil nil :bad t) (nil nil :test)))
        (is (eq t (m:evaluate vm `(handler-case (,operator ,@arguments) (program-error () t)))))))))

(test indexed-list-sets
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (size '(0 1 31 32 33 64))
      (dolist (test '(eql equal equalp))
        (dolist (key '(nil identity car))
          (let* ((a (loop for i below size collect
                      (if (eq key 'car) (cons (mod i 17) i) (mod i 17))))
                 (b (loop for i from 8 below (+ 8 size) collect
                      (if (eq key 'car) (cons (mod i 19) (- i)) (mod i 19))))
                 (predicate (if key
                    (lambda (x y) (funcall test (funcall key x) (funcall key y)))
                    (symbol-function test))))
            (dolist (operator '(subsetp intersection set-difference union))
              (let* ((form `(,operator ',a ',b :key ',key :test ',test))
                     (expected (eval form)) (actual (m:evaluate vm form)))
                (if (eq operator 'subsetp)
                  (is (eq (not (null expected)) (not (null actual))))
                  (is (null (set-exclusive-or expected actual :test predicate))
                      "Indexed set mismatch for ~S" form))))))))
    (dolist (test '(eq eql equal equalp))
      (is (eq t (m:evaluate vm
        `(let* ((keys (loop for i below 64 collect (list i)))
                (a (copy-list keys)) (b (reverse keys)))
           (and (subsetp a b :test ',test)
                (= (length (intersection a b :test ',test)) 64)
                (null (set-difference a b :test ',test))
                (= (length (union a b :test ',test)) 64)
                (equal a keys) (equal b (reverse keys))))))))
    (dolist (operator '(subsetp intersection set-difference union))
      (let* ((operation `(,operator (make-list 64 :initial-element 1) (make-list 64 :initial-element 2) :test '<))
             (form (if (eq operator 'subsetp) operation `(length ,operation))))
        (is (equal (eval form) (m:evaluate vm form)))))
    (is (eq t (m:evaluate vm
      '(let* ((a (loop for i below 64 collect (cons i :a)))
              (b (loop for i below 64 collect (cons i :b)))
              (out (intersection a b :key 'car)))
         (every (lambda (item) (eq (cdr item) :a)) out)))))))

(test native-list-set-function-designators
  (m:with-interpreter (vm)
    (dolist (operator '(subsetp intersection set-difference union))
      (is (equal '(1 2 3) (m:evaluate-string vm (format nil "
        (defparameter *set-seen* nil)
        (defun set-changing (a b)
          (setf (symbol-function 'set-changing)
                (lambda (a b) (push 99 *set-seen*) nil))
          (push a *set-seen*) ~A)
        (~A '(1 2 3) '(4) :test 'set-changing)
        (reverse *set-seen*)" (if (eq operator 'subsetp) "t" "nil") operator)))))
    (is (eq t (m:evaluate-string vm "
      (defun set-changing-key (x)
        (setf (symbol-function 'set-changing-key) (lambda (x) 99)) x)
      (let ((x (list 1 2 3))) (eq x (adjoin 2 x :key 'set-changing-key)))")))))

(test string-array-access
  (m:with-interpreter (vm)
    (dolist (form
      '((list (arrayp "") (vectorp "a") (arrayp nil) (vectorp '(1)))
        (list (array-rank "") (array-dimensions "") (array-total-size "")
              (array-dimension "abc" 0))
        (let ((x (array-dimensions "abc")) (y (array-dimensions "abc")))
          (list x y (eq x y)))
        (list (aref "λ🙂" 1) (schar "λ🙂" 0) (row-major-aref "λ🙂" 1))
        (let ((s (copy-seq "abcd")))
          (list (setf (char s 0) #\λ) (setf (schar s 1) #\🙂)
                (setf (aref s 2) #\z) (setf (row-major-aref s 3) #\w) s))
        (let ((i 0) (s (copy-seq "abc")))
          (values-list (list (setf (aref (progn (incf i) s) (progn (incf i) 1))
                                  (progn (incf i) #\z)) i s)))
        (multiple-value-list (schar "ab" 1))
        (multiple-value-list (setf (aref (copy-seq "ab") 1) #\z))))
      (is (equal (eval form) (m:evaluate vm form)) "String array mismatch for ~S" form))
    (dolist (source '("(array-rank nil)" "(array-dimensions 1)" "(array-total-size 'a)"
                      "(array-dimension \"abc\" 1)" "(array-dimension \"abc\" 0.0)"
                      "(aref '(a) 0)" "(schar 1 0)" "(schar \"abc\" -1)"
                      "(schar \"abc\" 3)" "(schar \"\" 0)"
                      "(schar \"abc\" 100000000000000000000000)"
                      "(setf (aref (copy-seq \"abc\") 0) 1)"
                      "(setf (schar (copy-seq \"abc\") 3) #\\a)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))
    (dolist (source '("(aref \"abc\")" "(aref \"abc\" 0 1)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))))

(test integer-parity
  (m:with-interpreter (vm)
    (dolist (integer '(0 1 -1 2 -2 536870911 -536870912
                      10000000000000000000000 -10000000000000000000001))
      (dolist (predicate '(oddp evenp))
        (is (equal (list (funcall predicate integer))
                   (m:evaluate vm `(multiple-value-list (,predicate ,integer)))))))
    (dolist (predicate '(oddp evenp))
      (dolist (value '(a nil 1.0 1/2 #\a))
        (is (eq t (m:evaluate vm
          `(handler-case (,predicate ',value)
             (type-error (c)
               (and (eql (type-error-datum c) ',value)
                    (not (typep (type-error-datum c) (type-error-expected-type c)))))))))))))

(test native-list-search-semantics
  (m:with-interpreter (vm)
    (dolist (form
      '((member 2 '(1 2 3) :key nil)
        (member 2 '(1 2 3) :test-not #'<)
        (member 18000000000000 (list 1 (1- 18000000000001)))
        (member-if #'oddp '(2 4 5 7) :key nil)
        (member-if-not #'oddp '(1 3 4 5))
        (assoc nil '(nil (nil . 2) (a . b)) :key nil)
        (rassoc nil '(nil (a . nil) (b . c)) :key nil)
        (assoc-if #'oddp '(nil (2 . a) (3 . b)))
        (assoc-if-not #'oddp '(nil (1 . a) (2 . b)))
        (rassoc-if #'oddp '(nil (a . 2) (b . 3)))
        (rassoc-if-not #'oddp '(nil (a . 1) (b . 2)))
        (assoc 3 '(((1) . a) ((3) . b)) :key 'car :test '=)
        (rassoc 3 '((a 1) (b 3)) :key 'car :test '=)
        (let* ((x (list 1 2 3)) (tail (member-if #'evenp x)))
          (list (eq tail (cdr x)) x))
        (let* ((x (list nil (cons 1 2) (cons 3 4)))
               (entry (rassoc-if #'evenp x)))
          (list (eq entry (cadr x)) x))
        (let ((seen nil))
          (member 3 '(1 2 3 4) :key (lambda (x) (push x seen) x))
          (reverse seen))
        (let ((seen nil))
          (assoc-if (lambda (x) (push x seen) (= x 2))
                    '(nil (1 . a) nil (2 . b) (3 . c)))
          (reverse seen))
        (multiple-value-list
          (member-if (lambda (x) (values x 99)) '(nil 1 2)))
        (multiple-value-list
          (rassoc-if-not (lambda (x) (values x 99)) '((a . 1) (b . nil))))
        (multiple-value-list
          (assoc-if (lambda (x) (values nil 99)) '((a . 1))))))
      ;; CCL's RASSOC :KEY NIL attempts to FUNCALL NIL. Normalize only
      ;; the host oracle's NIL key; keep exercising NIL in the guest.
      (let ((host-form (copy-list form)))
        (loop for tail on (cdr host-form)
              when (and (eq (car tail) :key) (cdr tail) (null (cadr tail)))
                do (setf (cadr tail) '(function identity)))
        (is (equal (eval host-form) (m:evaluate vm form)) "List search mismatch for ~S" form)))
    (dolist (source '("(member 1 2)" "(member-if #'identity '(nil . 2))"
                      "(member-if-not #'identity '(1 . 2))"
                      "(assoc 1 '(2))" "(rassoc 1 '(2))"
                      "(assoc-if #'identity '(2))" "(rassoc-if-not #'identity '(2))"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-list-search-function-designators
  (m:with-interpreter (vm)
    (dolist (operator '(member-if member-if-not assoc-if assoc-if-not rassoc-if rassoc-if-not))
      (is (equal '(1 2 3) (m:evaluate-string vm
        (format nil "
          (defparameter *search-seen* nil)
          (defun search-changing (arg)
            (setf (symbol-function 'search-changing)
                  (lambda (arg) (push 99 *search-seen*) ~A))
            (push arg *search-seen*) ~A)
          (~A 'search-changing ~A)
          (reverse *search-seen*)"
          (if (search "NOT" (symbol-name operator)) "nil" "t")
          (if (search "NOT" (symbol-name operator)) "t" "nil")
          operator
          (case operator
            ((member-if member-if-not) "'(1 2 3)")
            ((assoc-if assoc-if-not) "'((1 . a) (2 . b) (3 . c))")
            (t "'((a . 1) (b . 2) (c . 3))")))))))))

(test native-sequence-semantics
  (m:with-interpreter (vm)
    (dolist (form
      '((count 2 '(1 2 2) :key nil)
        (count 18000000000000 (list (1- 18000000000001) -18000000000000 18000000000001))
        (count 0 '(0 1 -1 0.0))
        (count 2 '(1 2 3 2 4) :start 1 :end 4 :test-not #'eql)
        (count-if (lambda (x) (> x 1)) '(1 2 3) :start 1 :end 2 :key nil)
        (count-if-not #'null '(nil 1 nil 2) :from-end t)
        (count #\a "abaca" :start 1 :end 4 :from-end t :key nil)
        (count 3 '((1) (2) (3) (4)) :key #'car :test #'<)
        (let ((seen nil))
          (count-if (lambda (x) (push x seen) t) '(1 2 3) :from-end t)
          seen)
        (let ((seen nil))
          (count 2 '(1 2 3 4 5) :start 1 :end 4 :from-end t
            :key (lambda (x) (push x seen) x))
          seen)
        (let ((seen nil))
          (count-if-not (lambda (x) (push x seen) nil) "abcd" :from-end t :start 1 :end 3)
          seen)
        (multiple-value-list (count-if (lambda (x) (values x 99)) '(1 nil 2)))
        (reverse "λ🙂") (reverse "") (reverse "a")
        (let* ((s (copy-seq "abcd")) (result (nreverse s)))
          (list (eq result s) s result))
        (let* ((s (copy-seq "abc")) (result (reverse s)))
          (list (eq result s) s result))
        (format nil (lambda (s)
          (let ((a (fresh-line s)))
            (write-string "x" s)
            (let ((b (fresh-line s)) (c (fresh-line s)))
              (format s "~S" (list a b c))))))
        (format nil (lambda (s)
          (let ((*standard-output* s))
            (let ((a (fresh-line nil)))
              (write-string "x" s)
              (let ((b (fresh-line nil)) (c (fresh-line nil)))
                (format s "~S" (list a b c)))))))))
      (is (equal (eval form) (m:evaluate vm form)) "Sequence mismatch for ~S" form))
    (dolist (source '("(car 1)" "(cdr 1)" "(cadr '(1 . 2))"
                      "(count 1 '(1 2) :start -1)" "(count 1 '(1 2) :end 3)"
                      "(count 1 '(1 2) :start 2 :end 1)"
                      "(count 1 '(1 2) :start 1000000000000000000000000000)"
                      "(count-if #'identity '(1 . 2))" "(reverse 1)" "(nreverse 1)"
                      "(fresh-line 1)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-mapping-function-designators
  (m:with-interpreter (vm)
    (dolist (operator '(mapcar mapc mapcan maplist mapcon mapl every some))
      (is (equal '(1 2 3) (m:evaluate-string vm
        (format nil "
          (defparameter *mapping-seen* nil)
          (defun mapping-changing (arg)
            (setf (symbol-function 'mapping-changing)
                  (lambda (arg) (declare (ignore arg)) (push 99 *mapping-seen*) t))
            (push (if (consp arg) (car arg) arg) *mapping-seen*)
            ~A)
          (~A 'mapping-changing '(1 2 3))
          (reverse *mapping-seen*)"
          (cond ((eq operator 'some) "nil") ((member operator '(mapcan mapcon)) "(list arg)") (t "t"))
          operator)))))))

(test native-list-traversal-budget
  ;; This budget includes construction and three linear traversals; indexed
  ;; list counting or per-cons interpreter evaluation cannot complete it.
  (m:with-interpreter (vm :fuel 20000000)
    (is (equal '(10000 10000 10000) (m:evaluate vm
      '(let ((input (make-list 10000 :initial-element 1)))
        (list (count 1 input) (length (copy-list input)) (length (revappend input nil)))))))))

(test list-set-keyword-binding
  (m:with-interpreter (vm)
    (dolist (operator '(adjoin subsetp intersection nintersection set-difference
                       nset-difference union nunion set-exclusive-or nset-exclusive-or))
      (let ((left (if (eq operator 'adjoin) 2 '(1 2))) (right '(2 3)))
        (dolist (keys '((:key nil :key #'1+) (:test #'eql :test #'<)
                        (:test-not #'/= :test-not #'<)
                        (:unknown 17 :allow-other-keys t)
                        (:allow-other-keys t :allow-other-keys nil :unknown 17)))
          (let* ((call `(,operator ,(if (eq operator 'adjoin) left `(copy-list ',left)) (copy-list ',right) ,@keys))
                 (form (if (eq operator 'subsetp) `(not (null ,call)) `(sort ,call #'<))))
            (is (equal (eval form) (m:evaluate vm form)) "Keyword mismatch for ~S" form)))
        (dolist (keys '((:key) (:unknown 1)
                        (:allow-other-keys nil :allow-other-keys t :unknown 1)))
          (is (eq t (m:evaluate vm
            `(handler-case (,operator ',left ',right ,@keys) (program-error () t))))))))))
