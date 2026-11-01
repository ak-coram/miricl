;;;; Host-oracle coverage of two's-complement operations and radix boundaries.
(in-package #:miricl-test)
(in-suite :miricl)

(defun integer-bit-fixtures ()
  (list 0 1 -1 2 -2 32767 32768 32769 -32768 -32769
        (1- (expt 2 30)) (expt 2 30) (- (expt 2 30))
        (+ (expt 2 60) 1) (- (expt 2 60))
        (+ (expt 2 128) (expt 2 64) 32769)
        (- (+ (expt 2 128) (expt 2 64) 32769))))

(test native-integer-shifts-and-measures
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (number (integer-bit-fixtures))
      (dolist (count '(-300 -130 -129 -128 -65 -64 -61 -60 -46 -45 -44 -31 -30 -29
                       -16 -15 -14 -2 -1 0 1 2 14 15 16 29 30 31 44 45 46 60 61 64 65 128 129 130 300))
        (let ((form `(ash ,number ,count)))
          (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))
      (dolist (name '(integer-length logcount lognot))
        (let ((form `(,name ,number)))
          (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))
      (dolist (index '(0 1 2 14 15 16 29 30 31 44 45 60 64 65 127 128 129 300 1000000))
        (let ((form `(logbitp ,index ,number)))
          (is (eql (eval form) (m:evaluate vm form))))))
    (dolist (number '(0 1 -1 123 -123))
      (dolist (form `((ash ,number ,(- (expt 2 100)))
                      (logbitp ,(expt 2 100) ,number)))
        (is (eql (eval form) (m:evaluate vm form)))))
    (dolist (form '((let ((x (+ (expt 2 128) 1))) (list (ash x 0) (ash x 60) x))
                    (let ((seen nil)) (list (ash (progn (push 1 seen) -3)
                            (progn (push 2 seen) -1)) seen))
                    (multiple-value-list (ash (values 32768 99) 15))
                    (multiple-value-list (integer-length (values -32768 99)))
                    (multiple-value-list (logcount (values -32768 99)))
                    (multiple-value-list (logbitp (values 100 99) -1))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-integer-boole
  (m:with-interpreter (vm :fuel 100000000)
    (let ((operators '(logand logior logxor logeqv lognand lognor logandc1 logandc2 logorc1 logorc2 logtest))
          (codes '(boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2 boole-and boole-ior
                   boole-xor boole-eqv boole-nand boole-nor boole-andc1 boole-andc2 boole-orc1 boole-orc2)))
      (is (= 16 (length (remove-duplicates (m:evaluate vm `(list ,@codes))))))
      (dolist (a (integer-bit-fixtures))
        (dolist (b (integer-bit-fixtures))
          (dolist (name operators)
            (let ((form `(,name ,a ,b)))
              (if (eq name 'logtest)
                (is (eql (not (null (eval form))) (not (null (m:evaluate vm form)))))
                (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))
          (dolist (code codes)
            (let ((form `(boole ,code ,a ,b)))
              (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
      (dolist (name '(logand logior logxor logeqv))
        (dolist (args '(nil (0) (-1) (32768 -32769) (0 -1 32768 -32769 9)))
          (let ((form (cons name args))) (is (eql (eval form) (m:evaluate vm form))))))
      (is (equal '(2 :changed) (m:evaluate vm
        '(let ((saved #'logand))
           (unwind-protect
             (progn (setf (symbol-function 'logand) (lambda (&rest x) (declare (ignore x)) :changed))
                    (list (funcall saved 6 3) (logand 6 3)))
             (setf (symbol-function 'logand) saved)))))))))

(test native-integer-bit-errors
  (m:with-interpreter (vm)
    (dolist (source '("(ash)" "(ash 1)" "(ash 1 2 3)" "(integer-length)" "(integer-length 1 2)"
                      "(logcount)" "(logcount 1 2)" "(logbitp 0)" "(logbitp 0 1 2)"
                      "(lognot)" "(lognot 1 2)" "(boole 1 2)" "(boole 1 2 3 4)"
                      "(lognand 1)" "(lognor)" "(logtest 1)" "(logandc1 1 2 3)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (bad '(nil t #\a "a" 1/2 1.0 #c(1 2) (1)))
      (dolist (form `((ash ',bad 0) (ash 0 ',bad) (integer-length ',bad)
                      (logcount ',bad) (logbitp 0 ',bad) (logbitp ',bad 0) (lognot ',bad)
                      (boole boole-clr ',bad 0) (boole boole-set 0 ',bad)
                      (logand 0 ',bad) (logior -1 ',bad) (logtest 0 ',bad)))
        (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t)))))))
    (dolist (form '((logbitp -1 0) (boole -1 0 0) (boole 16 0 0) (boole nil 0 0)
                    (boole 1.0 0 0)))
      (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t))))))
    (is (eq t (m:evaluate vm
      `(handler-case (ash 1 ,(expt 2 100)) (storage-condition () t)))))
    (is (= 0 (m:evaluate vm `(ash 0 ,(expt 2 100)))))))

(test native-large-integer-text
  (m:with-interpreter (vm)
    (loop for base from 2 to 36 do
      (dolist (width '(0 1 14 15 16 29 30 31 59 60 61 62 127 200 1000 4096))
        (dolist (offset '(-1 0 1))
          (let ((number (+ (ash 1 width) offset)))
            (dolist (signed (list number (- number)))
              (let ((form `(write-to-string ,signed :base ,base :radix nil :readably nil))
                    (expected (concatenate 'string (if (minusp signed) "-" "")
                      (write-to-string (abs signed) :base base :radix nil :readably nil))))
                (is (equal expected (m:evaluate vm form)) "Integer text mismatch for radix ~D, width ~D, offset ~D" base width offset))))))
      (dolist (width '(10 50 100))
        (dolist (offset '(-1 0 1))
          (let* ((number (+ (expt base width) offset))
                 (form `(write-to-string ,number :base ,base :radix nil :readably nil)))
            (is (equal (eval form) (m:evaluate vm form)) "Radix power mismatch for radix ~D, width ~D, offset ~D" base width offset))))
      (let ((number (1- (ash 1 2048))))
        (is (= number (m:evaluate vm `(let ((*read-base* ,base))
          (read-from-string (write-to-string ,number :base ,base :radix nil :readably nil))))))))))

(test native-integer-format-text
  (m:with-interpreter (vm)
    (loop for base from 2 to 36 do
      (dolist (number (list 0 17 -17 (1- (ash 1 200)) (- (1- (ash 1 200)))))
        (let ((form `(format nil "~VR" ,base ,number)))
          (is (equal (eval form) (m:evaluate vm form)) "Integer format mismatch for radix ~D" base))))
    (dolist (control '("~D" "~:D" "~@D" "~:@D" "~30,'0D" "~30,'0,'_,4:D" "~2R" "~16:R" "~36@R"))
      (dolist (number (list 0 17 -17 (1- (expt 10 300)) (- (1- (expt 10 300)))))
        (let ((form `(format nil ,control ,number)))
          (is (equal (eval form) (m:evaluate vm form)) "Integer format mismatch for ~S" form))))))
