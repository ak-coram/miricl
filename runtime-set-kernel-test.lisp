(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-set-duplicate-workers-host
  (dolist (size '(0 1 5 33 65))
    (let ((keys (coerce (loop for i below size collect (mod i 4)) 'vector)))
      (dolist (from-end '(nil t))
        (dolist (hash-test '(nil eql))
          (let* ((marks (make-array size :element-type 'bit :initial-element 0))
                 (removed (miricl-runtime:mark-duplicate-values keys marks #'eql
                            (vector nil from-end hash-test)))
                 (remaining (loop for i below size when (zerop (sbit marks i)) collect (svref keys i))))
            (is (equal (remove-duplicates (coerce keys 'list) :from-end from-end) remaining))
            (is (= removed (- size (length remaining)))))))))
  (let ((calls nil))
    (is (equalp #(1 2 1) (miricl-runtime:duplicate-sequence-keys '(0 1 2 1 4)
                          (lambda (x) (push x calls) (values x :ignored)) 1 4)))
    (is (equal '(1 2 1) (reverse calls))))
  (let ((backing #(9 1 2 1 9)))
    (is (equalp #(1 2 1) (miricl-runtime:duplicate-sequence-keys
                          (make-array 3 :displaced-to backing :displaced-index-offset 1) nil 0 3))))
  (signals type-error
    (miricl-runtime:duplicate-sequence-keys '(1 . :bad) nil 0 2))
  (is (equalp #(1 2) (miricl-runtime:duplicate-sequence-keys '(1 2) nil 0 2))))

(test runtime-set-list-workers-host
  (dolist (left '(nil (1) (1 2 1 3)))
    (dolist (right '(nil (1) (2 3 4)))
      (dolist (key '(nil identity))
        (dolist (test '(eql <))
          (dolist (negate '(nil t))
            (dolist (mode '(1 2 3 4))
              (let* ((controls (vector mode (symbol-function test)
                                       (if negate (symbol-function test) nil) nil))
                     (actual (miricl-runtime:list-set-values left right key controls))
                     (matches (lambda (a b)
                                (let ((truth (funcall test (if key (funcall key a) a)
                                                      (if key (funcall key b) b))))
                                  (if negate (not truth) truth)))))
                ;; An intersection/union may choose either representative of
                ;; a matching pair; SBCL sometimes chooses from the other
                ;; input. Check the pair coverage contract directly instead
                ;; of comparing unrelated implementations' representatives.
                (case mode
                  (1 (is (eq (not (null actual))
                             (every (lambda (a) (some (lambda (b) (funcall matches a b)) right)) left))))
                  (2 (is (every (lambda (x)
                                  (or (and (member x left)
                                           (some (lambda (b) (funcall matches x b)) right))
                                      (and (member x right)
                                           (some (lambda (a) (funcall matches a x)) left)))) actual))
                     (is (every (lambda (a)
                                  (every (lambda (b)
                                           (or (not (funcall matches a b))
                                               (member a actual) (member b actual))) right)) left)))
                  (3 (is (null (set-exclusive-or actual
                                (remove-if (lambda (a) (some (lambda (b) (funcall matches a b)) right)) left)))))
                  (4 (is (subsetp actual (append left right)))
                     (is (every (lambda (a)
                                  (or (member a actual)
                                      (some (lambda (b) (and (funcall matches a b) (member b actual))) right))) left))
                     (is (every (lambda (b)
                                  (or (member b actual)
                                      (some (lambda (a) (and (funcall matches a b) (member a actual))) left))) right)))))))))))
  (let ((left (loop for i below 64 collect i))
        (right (loop for i from 32 below 96 collect i)))
    (is (null (set-exclusive-or (intersection left right)
                               (miricl-runtime:list-set-values left right nil (vector 2 #'eql nil 'eql))))))
  (let ((right (list 1 2)))
    (is (eq right (miricl-runtime:list-set-values 1 right nil (vector 0 #'eql nil nil))))
    (is (eq right (cdr (miricl-runtime:list-set-values 3 right nil (vector 0 #'eql nil nil))))))
  (let ((cycle (list 1 2)))
    (setf (cddr cycle) cycle)
    (signals type-error (miricl-runtime:list-set-values cycle nil nil (vector 3 #'eql nil nil))))
  (signals type-error (miricl-runtime:list-set-values '(1 . :bad) nil nil (vector 2 #'eql nil nil))))

(test runtime-set-sequence-marker-host
  (dolist (size '(0 1 5 33 65))
    (dolist (reverse '(nil t))
      (dolist (limit (remove-duplicates (list 0 1 3 size)))
        (dolist (indexed '(nil t))
          (let* ((source (loop for i below size collect i))
                 (first (min 1 size)) (stop size)
                 (marks (make-array (- stop first) :element-type 'bit :initial-element 0))
                 (count (miricl-runtime:mark-sequence-values nil
                          (if indexed (coerce source 'vector) source) marks
                          (vector 1 nil #'oddp nil first stop reverse limit 0 0 nil)))
                 (remaining (loop for item in source for i from 0
                                  unless (and (<= first i) (< i stop) (= 1 (sbit marks (- i first))))
                                    collect item)))
            (is (equal (remove-if #'oddp source :start first :end stop :count limit :from-end reverse) remaining))
            (is (= count (- size (length remaining)))))))))
  (dolist (reverse '(nil t))
    (let* ((text "aAλΛ🙂a") (marks (make-array (length text) :element-type 'bit :initial-element 0)))
      (miricl-runtime:mark-sequence-values #\a text marks
        (vector 0 nil #'char-equal nil 0 (length text) reverse 2 3 (char-code #\A) #'char-upcase))
      (is (equal (remove #\a text :test #'char-equal :count 2 :from-end reverse)
                 (coerce (loop for i below (length text) when (zerop (sbit marks i)) collect (char text i)) 'string)))))
  (let ((source (list 1 2 3)))
    (signals type-error
      (miricl-runtime:mark-sequence-values nil source (make-array 3 :element-type 'bit :initial-element 0)
        (vector 1 nil (lambda (x) (setf (cdr source) :bad) (oddp x)) nil 0 3 nil 3 0 0 nil))))
  (is (= 1 (miricl-runtime:mark-sequence-values 2 '(1 2 3)
             (make-array 3 :element-type 'bit :initial-element 0) (vector 0 nil #'eql nil 0 3 nil 3 0 0 nil)))))
