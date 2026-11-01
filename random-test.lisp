;;;; Reproducible copies, native integer/float bounds, and readable state.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-random-states
  (m:with-interpreter (vm :fuel 200000000)
    (dolist (form
      '((random-state-p *random-state*)
        (typep *random-state* 'random-state)
        (eq (type-of *random-state*) 'random-state)
        (eq (class-of *random-state*) (find-class 'random-state))
        (multiple-value-bind (yes certain) (subtypep 'random-state t) (and yes certain))
        (let ((state (make-random-state))) (and (not (eq state *random-state*))
          (= (random 100000 state) (random 100000))))
        (let* ((a (make-random-state t)) (b (make-random-state a)))
          (and (not (eq a b)) (loop repeat 100 always (= (random 1000000000 a) (random 1000000000 b)))))
        (let* ((a (make-random-state t)) (b (make-random-state a)))
          (loop for limit in '(1 2 255 256 1073741823 1073741824 4294967296 123456789012345678901234567890)
            always (loop repeat 20 always (eql (random limit a) (random limit b)))))
        (let* ((a (make-random-state t)) (b (make-random-state a)))
          (loop for limit in '(1.0 1000.0 1d0 1000d0 least-positive-single-float least-positive-double-float
                               most-positive-single-float most-positive-double-float)
            always (loop repeat 20 always (eql (random (eval limit) a) (random (eval limit) b)))))
        (let* ((a (make-random-state t)) (*random-state* (make-random-state a)))
          (loop repeat 50 always (= (random 10000) (random 10000 a))))
        (let* ((a (make-random-state)) (b (make-random-state a)))
          (make-random-state t) (= (random 10000 a) (random 10000 b)))
        (let* ((a (make-random-state)) (b (let ((*read-eval* nil)) (read-from-string (write-to-string a :readably t)))))
          (and (random-state-p b) (not (eq a b))
            (loop repeat 100 always (= (random 1000000 a) (random 1000000 b)))))
        (let* ((a (make-random-state)) (*print-base* 2) (*print-radix* t) (*read-base* 7)
               (b (read-from-string (write-to-string a :readably t))))
          (loop repeat 20 always (= (random 10000 a) (random 10000 b))))
        (equal (multiple-value-list (random 1)) '(0))
        (not (random-state-p nil)) (not (random-state-p t))))
      (is (eq t (m:evaluate vm form)) "Failed ~S" form))
    (is (eq t (m:evaluate vm
      '(loop for limit in '(1 2 3 256 1024 1073741824 4294967295 4294967296 123456789012345678901234567890
                           1.0 1d0 least-positive-single-float least-positive-double-float
                           most-positive-single-float most-positive-double-float)
         always (let ((limit (eval limit)))
           (loop repeat 100 for value = (random limit)
             always (and (<= 0 value) (< value limit)
               (if (integerp limit) (integerp value) (eql (float value limit) value)))))))))
    (let ((state (m:evaluate vm '(make-random-state))))
      (is (m:guest-opaque-object-p state))
      (is (eq t (m:evaluate vm `(random-state-p ,state)))))))

(test native-random-errors
  (m:with-interpreter (vm)
    (dolist (source '("(random)" "(random 1 *random-state* nil)" "(make-random-state nil nil)"
                      "(random-state-p)" "(random-state-p nil nil)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(random 0)" "(random -1)" "(random 1/2)" "(random 0d0)" "(random -0.0)"
                      "(random #c(1 2))" "(random nil)" "(random 2 nil)" "(random 2 t)"
                      "(make-random-state 1)" "(let ((*random-state* 1)) (random 2))"
                      "(let ((*random-state* 1)) (make-random-state))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (type-error () t))")))))
    (dolist (source '("#S(unknown 1 2 3 4)" "#S(random-state 0 0 0 0)" "#S(random-state 1 2 3)"
                      "#S(random-state 1 2 3 4 5)" "#S(random-state 1 2 3 -1)"
                      "#S(random-state 1 2 3 4294967296)" "#S(random-state 1 2 3 a)" "#S1"))
      (is (eq t (m:evaluate vm `(handler-case (read-from-string ,source) (reader-error () t))))))
    (is (equal '(nil 31) (m:evaluate vm
      '(let ((*read-suppress* t)) (multiple-value-list (read-from-string "#S(unknown ignored 1 2 3 4 5 6)"))))))))
