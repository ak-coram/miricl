;;;; Simple general vectors use native storage and preserve ordinary object layout.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-general-vectors
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((vector) (vector 1 nil "λ🙂" #\a '(2 3))
        (let ((v (vector 1 2 3)))
          (list (length v) (svref v 1) (elt v 2) (aref v 0) (row-major-aref v 1)))
        (let ((v (vector 1 2 3)))
          (list (setf (svref v 1) :new) (setf (aref v 2) :last) v))
        (let ((v (vector nil nil))) (list (setf (row-major-aref v 0) 'a) v))
        (let ((value (list 1))) (let ((v (vector value value))) (eq (svref v 0) (svref v 1))))
        (let ((a (vector)) (b (vector))) (list (eq a b) (equal a b) (simple-vector-p a)))
        (list (arrayp (vector)) (vectorp (vector)) (simple-vector-p (vector))
              (simple-vector-p "a") (arrayp '(1)) (vectorp nil))
        (let ((v (vector 1 2)))
          (list (array-rank v) (array-dimension v 0) (array-dimensions v) (array-total-size v)))
        (let ((v (vector 1 2)))
          (list (typep v 'sequence) (typep v 'array) (typep v 'vector) (typep v 'simple-vector)
                (typep v 'simple-array) (typep v '(vector t 2)) (typep v '(simple-vector 2))
                (typep v '(array t (2))) (typep v '(array t 1))
                (typep v '(vector character)) (typep v '(simple-vector 3))))
        (let ((seen nil))
          (list (vector (progn (push 1 seen) :a) (progn (push 2 seen) :b)) (reverse seen)))
        (let ((v (vector 0)) (seen nil))
          (list (setf (svref (progn (push 1 seen) v) (progn (push 2 seen) 0))
                      (progn (push 3 seen) :done)) v (reverse seen)))
        (multiple-value-list (svref (vector 1 2) 0))
        (multiple-value-list (setf (svref (vector 1 2) 0) (values :new 99)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Vector mismatch for ~S" form))
    (dolist (literal (list #() #(1 nil 2) #("λ🙂" (1 2) #(#\a 17))))
      (is (equalp literal (m:evaluate vm literal))))
    (is (equal '(t :changed) (m:evaluate vm
      '(let ((saved #'vector))
         (unwind-protect
           (progn (setf (symbol-function 'vector) (lambda (&rest x) (declare (ignore x)) :changed))
                  (list (simple-vector-p (funcall saved 1 2)) (vector 1 2)))
           (setf (symbol-function 'vector) saved))))))))

(test native-vector-sequences
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((copy-seq #(1 2 3)) (subseq #(1 2 3) 1) (subseq #() 0)
        (let ((a (vector 1 2))) (let ((b (copy-seq a))) (list (eq a b) (progn (setf (svref b 0) :new) (list a b)))))
        (let ((x (list 1))) (eq x (svref (copy-seq (vector x)) 0)))
        (let ((x (vector 1 2 3))) (list (setf (subseq x 1) x) x))
        (let ((x (vector 1 2 3))) (list (setf (subseq x 1) '(4 5)) x))
        (let ((x (list 1 2 3))) (list (setf (subseq x 1) #(4 5)) x))
        (let ((x (copy-seq "abc"))) (list (setf (subseq x 1) #(#\λ #\🙂)) x))
        (let ((x (vector nil nil nil))) (list (setf (subseq x 1) "λ🙂") x))
        (let ((x (vector 1 2 3))) (list (setf (subseq x 1) #(4)) x))
        (let ((x (vector 1 2 3))) (list (setf (subseq x 1 1) #(4 5)) x))
        (let ((x (vector 1 2 3))) (list x (reverse x)))
        (let ((x (vector 1 2 3))) (let ((y (nreverse x))) (list (eq x y) x y)))
        (concatenate 'vector '(1 2) #(3 4) "ab")
        (concatenate 'simple-vector) (concatenate 'list #(1 2) #(3 4))
        (concatenate 'string #(#\a #\λ) "🙂")
        (concatenate '(vector t 3) #(1) '(2 3))
        (concatenate '(simple-vector 3) #(1) '(2 3))
        (coerce #(1 2 3) 'list) (coerce '(1 2 3) 'simple-vector)
        (coerce "aλ🙂" 'simple-vector) (coerce #(#\a #\λ) 'string)
        (let ((v (vector 1))) (eq v (coerce v 'simple-vector)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Vector sequence mismatch for ~S" form))))

(test native-vector-equality-and-hashing
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (a '(#() #(1 2) #(1.0 2.0) #(#\a #\λ #\🙂) "AΛ🙂" #((1 2) #(3 4))))
      (dolist (b '(#() #(1 2) #(1.0 2.0) #(#\a #\λ #\🙂) "AΛ🙂" #((1 2) #(3 4))))
        (is (eq (equalp a b) (m:evaluate vm `(equalp ',a ',b))))))
    (dolist (form
      '((let ((h (make-hash-table :test 'equalp)))
          (setf (gethash (vector #\a #\λ #\🙂) h) 7) (list (gethash "AΛ🙂" h) (hash-table-count h)))
        (let ((h (make-hash-table :test 'equalp)))
          (setf (gethash #(1 2.0 #(3)) h) nil)
          (list (multiple-value-list (gethash #(1.0 2 #(3.0)) h)) (hash-table-count h)))
        (let ((v (vector 1 2)) (h (make-hash-table :test 'equal)))
          (setf (gethash v h) 7) (setf (svref v 0) 99) (list (gethash v h) (hash-table-count h)))
        (let* ((v (vector 1 2)) (key (list v)) (old (sxhash key)))
          (setf (svref v 0) 99) (= old (sxhash key)))))
      (is (equal (eval form) (m:evaluate vm form))))
    (is (equal '(t 42) (m:evaluate vm
      '(let ((v (vector nil)) (h (make-hash-table :test 'equalp)))
         (setf (svref v 0) v (gethash v h) 42)
         (list (typep (sxhash v) 'fixnum) (gethash v h))))))))

(test native-vector-errors
  (m:with-interpreter (vm)
    (dolist (source '("(svref)" "(svref #())" "(svref #() 0 1)" "(simple-vector-p)"
                      "(arrayp nil nil)" "(vectorp)" "(funcall (fdefinition '(setf svref)) 1 #())"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(svref nil 0)" "(svref \"ab\" 0)" "(svref #() 0)" "(svref #(1) -1)"
                      "(svref #(1) 1)" "(svref #(1) 1.0)" "(svref #(1) 9999999999999999999999)"
                      "(setf (svref #(1) 1) 2)" "(schar #(1) 0)" "(char #(1) 0)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))

(test native-vector-traversal
  (m:with-interpreter (vm :fuel 1000000000)
    (dolist (input '(#() #(1) #(1 2 1 3 2)))
      (loop for start from 0 to (length input) do
        (loop for end from start to (length input) do
          (dolist (from-end '(nil t))
            (dolist (key '(nil 1+))
              (dolist (operation '(find position count remove delete))
                (let ((form `(,operation 2 (copy-seq ',input) :start ,start :end ,end
                                        :from-end ,from-end :key ',key)))
                  (is (equalp (eval form) (m:evaluate vm form)) "Vector traversal mismatch for ~S" form))))))))
    (dolist (input '(#() #(1) #(1 2 1 3 2)))
      (dolist (from-end '(nil t))
        (dolist (count '(nil -1 0 1 2 8))
          (dolist (operation '(remove delete))
            (let ((form `(,operation 2 (copy-seq ',input) :count ,count :from-end ,from-end)))
              (is (equalp (eval form) (m:evaluate vm form)))))))
      (dolist (from-end '(nil t))
        (dolist (operation '(remove-duplicates delete-duplicates))
          (dolist (test '(eql equalp <))
            (let ((form `(,operation (copy-seq ',input) :from-end ,from-end :test ',test)))
              (is (equalp (eval form) (m:evaluate vm form))))))))
    (dolist (form
      '((reduce #'+ #()) (reduce #'+ #(1 2 3))
        (reduce #'list #(1 2 3) :from-end t :initial-value nil)
        (reduce #'list #(1 2 3) :start 1 :end 2 :key #'1+)
        (let ((seen nil))
          (list (reduce #'+ #(1 2 3) :key (lambda (x) (push x seen) (1+ x))
                        :from-end t :initial-value 10) (reverse seen)))
        (let ((seen nil))
          (list (count-if (lambda (x) (push x seen) (oddp x)) #(1 2 3) :from-end t) (reverse seen)))
        (find-if #'oddp #(2 3 4 5) :from-end t)
        (position-if-not #'oddp #(2 3 4 5) :start 1 :from-end t)
        (count-if-not #'oddp #(2 3 4 5))
        (remove-if #'oddp #(1 2 3 4 5) :count 1 :from-end t)
        (delete-if-not #'oddp (vector 1 2 3 4 5) :start 1 :end 4)
        (remove-duplicates #((1) (2) (1)) :key #'car)
        (remove-duplicates #(1 2 1 3 2) :start 1 :end 4 :from-end t)
        (let ((v (vector 1 2 1))) (let ((r (remove 1 v))) (list v r (eq v r))))))
      (is (equalp (eval form) (m:evaluate vm form)) "Vector callback mismatch for ~S" form))
    (is (equalp #(1 1) (m:evaluate vm
      '(remove-duplicates #(1 2 1 3 2) :test-not #'eql :from-end t))))))

(test native-vector-quantifiers
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((every #'characterp "aλ🙂") (some #'oddp #(2 4 5 6))
        (notany #'oddp #(2 4 6)) (notevery #'oddp #(1 3 4))
        (every #'= #(1 2 3) '(1 2 3 4))
        (every #'char-equal "aλ🙂" #(#\A #\Λ #\🙂))
        (some #'list #() '(1 2)) (every #'list #(1 2) nil)
        (let ((seen nil))
          (list (some (lambda (&rest args) (push args seen) (if (= (car args) 2) :found nil))
                      #(1 2 3) "abc" '(4 5)) (reverse seen)))
        (let ((seen nil))
          (list (every (lambda (&rest args) (push args seen) (values :true nil))
                       #(1 2 3) "ab" '(4 5 6)) (reverse seen)))
        (multiple-value-list (some (lambda (x) (values x 9)) #(nil :found)))
        (multiple-value-list (every (lambda (x) (values x 9)) #(:found)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Vector quantifier mismatch for ~S" form))))

(test quantifier-empty-sequence-validation
  (m:with-interpreter (vm)
    (dolist (operator '(every some notevery notany))
      (dolist (empty '(nil #() ""))
        (dolist (invalid '(17 #\a t #2A((1)) #0A17))
          (is (eq t (m:evaluate vm `(handler-case
            (,operator (lambda (&rest args) (declare (ignore args)) (error "Predicate called"))
                        ',empty ',invalid)
            (type-error () t))))))))))

(test native-make-sequence
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (type '(list vector simple-vector (vector t) (vector *)
                   (array t 1) (simple-array t (*)) string simple-string base-string simple-base-string
                   (vector character) (array character (*))))
      (dotimes (size 4)
        (let* ((initial (if (subtypep type 'string) #\a :value))
               (form `(make-sequence ',type ,size :initial-element ',initial)))
          (is (equalp (eval form) (m:evaluate vm form)) "MAKE-SEQUENCE mismatch for ~S" form))))
    (dolist (form
      '((make-sequence 'null 0) (make-sequence 'cons 1 :initial-element 7)
        (make-sequence '(simple-vector 2) 2 :initial-element 8)
        (make-sequence '(vector t 2) 2 :initial-element 8)
        (length (make-sequence 'string 3))
        (length (make-sequence 'vector 2))
        (let ((x (list 1))) (let ((v (make-sequence 'vector 3 :initial-element x)))
          (list (eq x (svref v 0)) (eq (svref v 0) (svref v 2)))))
        (let ((a (make-sequence 'vector 0)) (b (make-sequence 'vector 0))) (eq a b))
        (multiple-value-list (make-sequence 'vector 0))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (dolist (source '("(make-sequence 'vector -1)" "(make-sequence 'vector 1.0)"
                      "(make-sequence '(simple-vector 2) 1)" "(make-sequence '(vector * 2) 3)"
                      "(make-sequence 'null 1)" "(make-sequence 'cons 0)"
                      "(make-sequence 'integer 1)" "(make-sequence 'sequence 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))
    (dolist (source '("(make-sequence)" "(make-sequence 'vector)"
                      "(make-sequence 'vector 2 :unknown t)" "(make-sequence 'vector 2 :initial-element)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))))

(test native-sequence-fill-and-replace
  (m:with-interpreter (vm :fuel 1000000000)
    (dolist (input '("aλ🙂" #(#\a #\λ #\🙂) (#\a #\λ #\🙂)))
      (loop for first from 0 to 3 do
        (loop for stop from first to 3 do
          (let ((form `(let ((v (copy-seq ',input)))
                         (let ((r (fill v #\Z :start ,first :end ,stop))) (list v (eq v r))))))
            (is (equalp (eval form) (m:evaluate vm form))))))
      (dolist (source '("BCD" #(#\B #\C #\D) (#\B #\C #\D)))
        (loop for first from 0 to 3 do
          (loop for stop from first to 3 do
            (loop for from from 0 to 3 do
              (loop for to from from to 3 do
                (let ((form `(let ((v (copy-seq ',input)))
                               (let ((r (replace v ',source :start1 ,first :end1 ,stop :start2 ,from :end2 ,to)))
                                 (list v (eq v r))))))
                  (is (equalp (eval form) (m:evaluate vm form)) "REPLACE mismatch for ~S" form)))))))
      (dotimes (first 4)
        (dotimes (from 4)
          (let ((form `(let ((v (copy-seq ',input))) (replace v v :start1 ,first :start2 ,from))))
            ;; Snapshot the host source too: ECL's overlapping list REPLACE
            ;; does not consistently preserve the original source elements.
            (is (equalp (eval `(let ((v (copy-seq ',input)))
                                (replace v (copy-seq v) :start1 ,first :start2 ,from)))
                        (m:evaluate vm form)))))))
    (dolist (form
      '((fill nil 1) (replace nil #()) (replace #() nil)
        (let ((item (list 1)) (v (vector nil nil)))
          (fill v item) (list (eq item (svref v 0)) (eq (svref v 0) (svref v 1))))
        (let ((item (list 1)) (v (list nil nil)))
          (fill v item) (list (eq item (car v)) (eq (car v) (cadr v))))
        (let* ((item (list 1)) (v (vector nil)))
          (replace v (list item)) (eq item (svref v 0)))
        (multiple-value-list (replace (vector 1 2) #(3)))
        (multiple-value-list (fill (vector 1 2) 3))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (dolist (source '("(fill 1 2)" "(fill #(1) 2 :start -1)" "(fill #(1) 2 :end 1.0)"
                      "(fill #(1) 2 :start 2)" "(fill #(1) 2 :start 1 :end 0)"
                      "(fill (copy-seq \"a\") 1)" "(replace 1 nil)" "(replace nil 1)"
                      "(replace #(1) #(2) :start2 -1)" "(replace #(1) #(2) :end1 2)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))
    (dolist (source '("(fill)" "(fill nil)" "(fill nil 1 :unknown t)"
                      "(replace)" "(replace nil)" "(replace nil nil :unknown t)"
                      "(replace nil nil :start1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))))

(test native-sequence-map
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (input '("aλ🙂" #(#\a #\λ #\🙂) (#\a #\λ #\🙂) #() nil))
      (dolist (type '(nil list vector simple-vector string simple-string (vector t) (vector *) (array character (*))))
        (let ((form `(map ',type #'char-upcase ',input)))
          (is (equalp (eval form) (m:evaluate vm form)) "MAP mismatch for ~S" form))))
    (dolist (form
      '((map 'list #'list #(1 2 3) "aλ" '(4 5 6))
        (map 'simple-vector #'+ #(1 2 3) '(4 5))
        (map '(simple-vector 2) #'1+ #(1 2))
        (map '(vector * 2) #'list #(1 2) #(3 4))
        (map 'null #'identity #()) (map 'cons #'1+ #(1))
        (let ((saved nil))
          (list (map 'vector (lambda (&rest x) (push x saved) x) #(1 2) "ab") (reverse saved)))
        (let ((seen nil))
          (list (map nil (lambda (x) (push x seen)) #(1 2 3)) (reverse seen)))
        (map 'vector (lambda (x) (values x 9)) #(1 2))
        (block stop (map 'vector (lambda (x) (when (= x 2) (return-from stop :done)) x) #(1 2 3)))
        (catch 'stop (map 'list (lambda (x) (if (= x 2) (throw 'stop :done) x)) #(1 2 3)))
        (multiple-value-list (map 'vector #'1+ #(1 2)))
        (multiple-value-list (map nil #'1+ #(1 2)))))
      (is (equalp (eval form) (m:evaluate vm form)) "MAP callback mismatch for ~S" form))
    (dolist (source '("(map 'vector #'identity 1)" "(map 'vector #'identity nil 1)"
                      "(map 'integer #'identity #(1))" "(map 'sequence #'identity #(1))"
                      "(map '(simple-vector 2) #'identity #(1))" "(map '(vector * 2) #'cons \"abc\" \"d\")"
                      "(map 'vector 1 #())"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))
    (dolist (source '("(map)" "(map 'vector)" "(map 'vector #'identity)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))))
