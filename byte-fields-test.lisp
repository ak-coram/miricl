;;;; Byte fields across limb boundaries, signed inputs, and generalized places.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-byte-fields
  (m:with-interpreter (vm :fuel 1000000000)
    (let ((numbers (append '(0 1 -1 2 -2 32767 32768 -32768 536870911 -536870912)
                           (loop for bits in '(30 31 45 60 100 200) append
                             (list (ash 1 bits) (1- (ash 1 bits)) (- (ash 1 bits)))))))
      (dolist (size '(0 1 14 15 16 29 30 31 64 128 256))
        (dolist (position '(0 1 14 15 16 29 30 31 64 128 256))
          (dolist (number numbers)
            (dolist (operator '(ldb mask-field ldb-test))
              (let* ((form `(,operator (byte ,size ,position) ,number))
                     (expected (eval form)) (actual (m:evaluate vm form)))
                (is (eql (if (eq operator 'ldb-test) (not (null expected)) expected) actual)
                    "Byte extraction mismatch for ~S" form)))))))
    (dolist (size '(0 1 14 15 16 30 64 128))
      (dolist (position '(0 1 14 15 16 30 64 128))
        (dolist (new '(0 1 -1 32768 -32768 1180591620717411303423))
          (dolist (old '(0 1 -1 32768 -32768 -1267650600228229401496703205377))
            (dolist (operator '(dpb deposit-field))
              (let ((form `(,operator ,new (byte ,size ,position) ,old)))
                (is (= (eval form) (m:evaluate vm form)) "Byte replacement mismatch for ~S" form)))))))
    (dolist (fixture
      '(((let ((x (byte 123456789012345678901234567890 987654321098765432109876543210)))
           (list (byte-size x) (byte-position x)))
         (123456789012345678901234567890 987654321098765432109876543210))
        ((ldb (byte 0 9999999999999999999999) -1) 0)
        ((ldb (byte 1 9999999999999999999999) -1) 1)
        ((ldb (byte 9999999999999999999999 9999999999999999999999) 17) 0)
        ((ldb-test (byte 9999999999999999999999 0) -1) t)
        ((mask-field (byte 9999999999999999999999 9999999999999999999999) 17) 0)
        ((dpb -1 (byte 9999999999999999999999 0) -1) -1)
        ((dpb 0 (byte 9999999999999999999999 0) 17) 0)
        ((deposit-field -1 (byte 9999999999999999999999 9999999999999999999999) -17) -17)
        ((deposit-field 3 (byte 9999999999999999999999 9999999999999999999999) 17) 17)
        ((multiple-value-list (ldb (byte 8 0) (values 511 99))) (255))
        ((multiple-value-list (dpb 1 (byte 1 0) (values 0 99))) (1))))
      (destructuring-bind (form expected) fixture
        (is (equal expected (m:evaluate vm form)))))))

(test native-byte-places
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((x 0)) (list (setf (ldb (byte 4 2) x) 31) x))
        (let ((x 0)) (list (setf (mask-field (byte 4 2) x) 31) x))
        (let ((x (list 0))) (list (setf (ldb (byte 4 2) (car x)) 31) x))
        (let ((x 0)) (list (incf (ldb (byte 4 2) x) 3) x))
        (let ((x 31)) (list (decf (mask-field (byte 4 2) x) 4) x))
        (let ((x 0)) (list (setf (ldb (byte 2 1) (ldb (byte 8 2) x)) 1) x))
        (let ((seen nil) (x (list 0)))
          (list (setf (ldb (progn (push 1 seen) (byte 4 2)) (car (progn (push 2 seen) x)))
                      (progn (push 3 seen) 31)) x (reverse seen)))
        (let ((seen nil) (x (list 0)))
          (list (incf (ldb (progn (push 1 seen) (byte 4 2)) (car (progn (push 2 seen) x)))
                      (progn (push 3 seen) 3)) x (reverse seen)))
        (let ((x (list 0)))
          (macrolet ((place () '(car x))) (incf (ldb (byte 4 2) (place)) 3)) x)
        (let ((x 0)) (multiple-value-list (setf (ldb (byte 4 0) x) (values 31 99))))))
      (is (equal (eval form) (m:evaluate vm form)) "Byte place mismatch for ~S" form))))

(test native-byte-errors
  (m:with-interpreter (vm)
    (dolist (source '("(byte)" "(byte 1)" "(byte 1 2 3)" "(byte-size)" "(byte-position nil nil)"
                      "(ldb)" "(ldb nil)" "(ldb nil 1 2)" "(ldb-test nil)" "(mask-field nil)"
                      "(dpb 1 nil)" "(deposit-field 1 nil)" "(setf (ldb (byte 1 0)) 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(byte -1 0)" "(byte 1 -1)" "(byte 1.0 0)" "(byte nil 0)"
                      "(byte-size nil)" "(byte-position '(1 . -1))" "(ldb nil 1)"
                      "(ldb (byte 1 0) 1.0)" "(ldb-test (byte 0 0) nil)"
                      "(dpb nil (byte 0 0) 0)" "(deposit-field 0 (byte 0 0) nil)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))
