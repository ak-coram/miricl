;;;; Float representation, scaling, signed zeros, and format limits.
(in-package #:miricl-test)
(in-suite :miricl)

(defun float-negative-bit-p (number)
  (with-foreign-object (raw :double)
    (setf (mem-ref raw :double) (coerce number 'double-float))
    (logbitp 63 (mem-ref raw :uint64))))

(test native-float-properties
  (m:with-interpreter (vm :fuel 100000000)
    (let ((numbers (list 0.0 -0.0 0d0 -0d0 1.0 -1d0 0.1 0.1d0
                         least-positive-single-float least-positive-double-float
                         (* 3 least-positive-single-float) (* 3 least-positive-double-float)
                         least-positive-normalized-single-float least-positive-normalized-double-float
                         most-positive-single-float most-positive-double-float)))
      (dolist (number numbers)
        ;; Zero exponents and integer significand normalization can differ
        ;; between hosts. Check the HyperSpec's identities with exact rationals.
        (dolist (function '(decode-float integer-decode-float))
          (destructuring-bind (significand exponent sign)
              (m:evaluate vm `(multiple-value-list (,function ,number)))
            (is (= (abs (rational number)) (* (rational significand) (expt 2 exponent))))
            (is (= sign (if (float-negative-bit-p number) -1 1)))
            (is (integerp exponent))
            (if (eq function 'decode-float)
                (progn (is (typep significand (type-of number)))
                       (is (typep sign (type-of number)))
                       (is (if (zerop number) (zerop significand) (<= 0.5 significand)))
                       (is (< significand 1)))
                (is (and (integerp significand) (not (minusp significand)))))))
        (let* ((single (typep number 'single-float)) (digits (if single 24 53))
               (unit (expt 2 (if single -149 -1074)))
               (precision (min digits (integer-length (ceiling (/ (abs (rational number)) unit))))))
          (is (= 2 (m:evaluate vm `(float-radix ,number))))
          (is (= digits (m:evaluate vm `(float-digits ,number))))
          (is (= precision (m:evaluate vm `(float-precision ,number)))))
        (let ((sign (m:evaluate vm `(float-sign ,number))))
          (is (typep sign (type-of number)))
          (is (= sign (if (float-negative-bit-p number) -1 1))))
        (dolist (other numbers)
          ;; Hosts differ in contagion and signed-zero handling here.
          (let ((result (m:evaluate vm `(float-sign ,number ,other))))
            (is (floatp result))
            (is (= (abs (rational other)) (abs (rational result))))
            (is (eq (float-negative-bit-p number) (float-negative-bit-p result)))))
        (is (eq t (m:evaluate vm `(multiple-value-bind (s e sign) (decode-float ,number)
          (and (typep s (type-of ,number)) (typep sign (type-of ,number))
            (or (zerop s) (<= 0.5 s)) (< s 1) (= (scale-float s e) (abs ,number)))))))
        (is (eq t (m:evaluate vm `(multiple-value-bind (s e sign) (integer-decode-float ,number)
          (and (integerp s) (integerp e) (member sign '(-1 1))
            (= (scale-float (float s ,number) e) (abs ,number))))))))
      (dolist (number (list 1.0 -1.0 1d0 -1d0 least-positive-single-float least-positive-double-float
                            least-positive-normalized-single-float least-positive-normalized-double-float))
        (dolist (exponent '(-2098 -1075 -1074 -1023 -1022 -150 -149 -148 -24 -1 0 1 24 127 1023 1074 2098))
          (let* ((single (typep number 'single-float))
                 (unit (expt 2 (if single -149 -1074)))
                 (limit (* (- 2 (expt 2 (if single -23 -52))) (expt 2 (if single 127 1023))))
                 (exact (* (abs (rational number)) (expt 2 exponent)))
                 (expected (if (< exact (expt 2 (if single -126 -1022)))
                               (* (round (/ exact unit)) unit) exact))
                 (form `(handler-case (scale-float ,number ,exponent) (floating-point-overflow () :overflow)))
                 (result (m:evaluate vm form)))
            ;; ECL may return infinity, and CCL mishandles extreme exponents.
            ;; The oracle scales rationals and rounds only at the subnormal boundary.
            (if (> expected limit)
                (is (eq :overflow result) "Float scale mismatch: ~S" form)
                (progn (is (typep result (type-of number)))
                       (is (= expected (abs (rational result))) "Float scale mismatch: ~S" form)
                       (is (eq (float-negative-bit-p number) (float-negative-bit-p result)))))))))
    (dolist (form '((= (scale-float 1d0 (- (expt 2 100))) 0d0)
                    (= (float-sign (scale-float -1d0 (- (expt 2 100)))) -1d0)
                    (= (float-sign (scale-float -0.0 (expt 2 100))) -1.0)
                    (handler-case (scale-float 1d0 (expt 2 100)) (floating-point-overflow () t))
                    (handler-case (scale-float 1.0 128) (floating-point-overflow (c)
                      (and (eq (arithmetic-error-operation c) 'scale-float)
                        (equal (arithmetic-error-operands c) '(1.0 128)))))
                    (equal (multiple-value-list (float-radix (values 1.0 99))) '(2))))
      (is (eq t (m:evaluate vm form))))
    (dolist (function '(decode-float integer-decode-float float-radix float-digits float-precision float-sign scale-float))
      (dolist (value '(nil t 1 1/2 #c(1 2) "x"))
        (is (eq t (m:evaluate vm `(handler-case (,function ,value ,@(when (eq function 'scale-float) '(0)))
          (type-error () t))))))
      (is (eq t (m:evaluate vm `(handler-case (,function) (program-error () t))))))
    (dolist (form '((float-sign 1.0 nil) (scale-float 1.0 1.0) (decode-float 1.0 2.0)
                    (float-sign 1.0 2.0 3.0) (scale-float 1.0)))
      (is (eq t (m:evaluate vm `(handler-case ,form (error () t))))))))

(test float-format-constants
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (format-name '(short single double long))
      (dolist (pattern '("MOST-POSITIVE-~A-FLOAT" "MOST-NEGATIVE-~A-FLOAT"
                         "LEAST-POSITIVE-~A-FLOAT" "LEAST-NEGATIVE-~A-FLOAT"
                         "LEAST-POSITIVE-NORMALIZED-~A-FLOAT" "LEAST-NEGATIVE-NORMALIZED-~A-FLOAT"
                         "~A-FLOAT-EPSILON" "~A-FLOAT-NEGATIVE-EPSILON"))
        (let ((name (find-symbol (format nil pattern format-name) :cl))
              ;; LONG-FLOAT is binary64 in the guest, even on ECL hosts
              ;; with a distinct extended-precision long-float format.
              (host-name (find-symbol (format nil pattern (if (eq format-name 'long) 'double format-name)) :cl)))
          ;; CCL 1.13's binary32 normalized minimum constant is one ULP
          ;; above the IEEE boundary. Derive that boundary independently.
          (let ((expected (if (search "NORMALIZED" pattern)
                              (let* ((double (member format-name '(double long)))
                                     (value (coerce (expt 2 (if double -1022 -126))
                                                    (if double 'double-float 'single-float))))
                                (if (search "NEGATIVE" pattern) (- value) value))
                              (symbol-value host-name))))
            (is (eql expected (m:evaluate vm name))))
          (is (eq t (m:evaluate vm `(constantp ',name))))
          (is (eq t (m:evaluate vm `(eq (symbol-package ',name) (find-package :cl))))))))))

(test pi-constant
  (m:with-interpreter (vm)
    ;; Miricl's LONG-FLOAT is binary64; ECL may use a wider host format for PI.
    (is (= (float pi 1d0) (m:evaluate vm 'pi)))
    (is (equal (multiple-value-list (integer-decode-float (float pi 1d0)))
               (m:evaluate vm '(multiple-value-list (integer-decode-float pi)))))
    (is (eq t (m:evaluate vm '(and (boundp 'pi) (constantp 'pi)
      (typep pi 'long-float) (= (float-digits pi) 53) (< 3.14d0 pi 3.15d0)))))
    (is (= (float pi 1f0) (m:evaluate vm '(float pi 1f0))))
    (is (= 1 (length (multiple-value-list (m:evaluate vm 'pi)))))
    (dolist (form '((set 'pi 3) (let ((pi 3)) pi) (symbol-macrolet ((pi 3)) pi)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))
