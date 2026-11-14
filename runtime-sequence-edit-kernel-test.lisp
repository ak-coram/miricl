(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-marked-removal-host
  (dolist (size '(0 1 5 33 65))
    (dolist (kind '(list vector string bit-vector))
      (let ((source (case kind
                      (list (loop for i below size collect i))
                      (vector (coerce (loop for i below size collect i) 'vector))
                      (string (make-string size :initial-element #\x))
                      (bit-vector (make-array size :element-type 'bit :initial-element 1)))))
        (dolist (first (remove-duplicates (list 0 (floor size 2) size)))
          (let* ((stop size) (marks (make-array (- stop first) :element-type 'bit :initial-element 0)))
            (loop for i below (length marks) when (zerop (mod i 3)) do (setf (sbit marks i) 1))
            (let* ((removed (count 1 marks))
                   (expected (loop for i below size
                                   unless (and (<= first i) (< i stop) (= 1 (sbit marks (- i first))))
                                     collect (elt source i)))
                   (before (copy-seq source))
                   (result (miricl-runtime:remove-marked-values source marks (vector size first stop removed nil))))
              (is (equalp expected (coerce result 'list)))
              (is (equalp before source))
              (when (vectorp source) (is (equal (array-element-type source) (array-element-type result))))))))))
  (let* ((items (list :a :b :c :d :e)) (kept (list (first items) (third items) (fifth items)))
         (third (cddr items)) (fifth (cddddr items))
         (result (miricl-runtime:remove-marked-values items #*01010 #(5 0 5 2 t))))
    (is (equal kept result))
    (is (eq items result)) (is (eq third (cdr result))) (is (eq fifth (cddr result))))
  (let ((items (list :a :b)))
    (is (eq items (miricl-runtime:remove-marked-values items #*00 #(2 0 2 0 nil))))))

(test runtime-kernel-marked-substitution-host
  (dolist (size '(0 1 5 33 65))
    (let* ((marks (make-array size :element-type 'bit :initial-element 0))
           (items (loop for i below size collect i))
           (cells (loop for cell on items collect cell))
           (vector (coerce items 'vector)))
      (loop for i below size when (oddp i) do (setf (sbit marks i) 1))
      (is (eq items (miricl-runtime:substitute-marked-values items marks :new (vector 0 size))))
      (is (eq vector (miricl-runtime:substitute-marked-values vector marks :new (vector 0 size))))
      (is (every #'eq cells (loop for cell on items collect cell)))
      (is (equalp items (coerce vector 'list)))
      (loop for item in items for i from 0 do (is (eql item (if (oddp i) :new i))))))
  (let* ((backing (vector 0 1 2 3 4 5))
         (view (make-array 4 :displaced-to backing :displaced-index-offset 1 :fill-pointer 0)))
    (is (eq view (miricl-runtime:substitute-marked-values view #*1010 :new #(0 4))))
    (is (= 0 (fill-pointer view)))
    (is (equalp #(0 :new 2 :new 4 5) backing))))

(test runtime-kernel-marked-edit-guest
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (dolist (form
                '((let ((s (list 1 2 1 3))) (list (remove 1 s) s))
                  (let* ((s (list 1 2 1 3)) (tail (cdr s)) (result (delete 1 s)))
                    (list result (eq tail result)))
                  (let* ((s (list 1 2 1 3)) (tails (loop for cell on s collect cell)))
                    (nsubstitute 9 1 s)
                    (list s (every #'eq tails (loop for cell on s collect cell))))
                  (let ((s (list 1 2 1 3))) (list (substitute 9 1 s) s))
                  (let ((s (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
                    (list (coerce (remove 1 s) 'list) (array-element-type (remove 1 s))))
                  (let ((s (make-array 4 :fill-pointer 4 :initial-contents '(1 2 1 3))))
                    (list (remove 1 s) (fill-pointer s)))
                  (multiple-value-list (remove 1 '(1 2 3)))
                  (multiple-value-list (substitute 9 1 '(1 2 3)))))
        (is (equalp (eval form) (m:evaluate vm form))))
      ;; Sharing when nothing changes is permitted to differ between CL
      ;; implementations; preserve this target's existing identity behavior.
      (is (equal '(t t) (m:evaluate-string vm
                         "(let ((s (list 1 2)))
                            (list (eq s (remove 42 s)) (eq s (substitute 99 42 s))))")))
      (dolist (source
                '("(remove 1 '(1 . :bad))"
                  "(let ((s (list 1 2))) (setf (cddr s) s) (substitute 9 1 s))"
                  "(let ((s (list 1 2 3)))
                     (remove-if (lambda (v) (setf (cdr s) :bad) (= v 1)) s))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (equal '(2 3) (m:evaluate-string vm "(remove 1 '(1 2 3))"))))))
