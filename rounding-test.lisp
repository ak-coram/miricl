;;;; Exact signs/ties, floating contagion, multiple values, and argument errors.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-expt-zero-and-integral-float-powers
  (m:with-interpreter (vm)
    (dolist (base '(0 1 -1 2 -2 1/2 -1/2 1.0 -1.0 2.0 -2.0 1d0 -1d0 2d0 -2d0
                   #c(1 1) #c(1.0 1.0) #c(1d0 1d0)))
      (dolist (power '(0 0.0 -0.0 0d0 -0d0 #c(0.0 0.0) #c(0d0 0d0)))
        (unless (and (zerop base) (not (integerp power)))
          (let ((form `(let ((result (expt ',base ',power)))
                         (list result (rationalp (realpart result))
                               (typep (realpart result) 'single-float)
                               (typep (realpart result) 'double-float) (complexp result)
                               (length (multiple-value-list (expt ',base ',power)))))))
            (is (equalp (eval form) (m:evaluate vm form)) "EXPT zero mismatch ~S" form))))
      (unless (zerop base)
        (dolist (power '(-4.0 -3.0 -2.0 -1.0 1.0 2.0 3.0 4.0
                         -4d0 -3d0 -2d0 -1d0 1d0 2d0 3d0 4d0))
          (let* ((prototype (if (or (typep (realpart base) 'double-float) (typep power 'double-float)) 1d0 1.0))
                 (exact (funcall (symbol-function 'expt)
                                 (complex (rational (realpart base)) (rational (imagpart base)))
                                 (truncate power)))
                 (expected (if (complexp base)
                               (complex (float (realpart exact) prototype) (float (imagpart exact) prototype))
                               (float exact prototype)))
                 (form `(let ((result (expt ',base ',power)))
                         (list result (typep (realpart result) 'single-float)
                               (typep (realpart result) 'double-float) (complexp result)))))
            ;; An exact integer-power oracle avoids host complex EXP/LOG's
            ;; residual imaginary errors at integral floating exponents.
            (is (equalp (list expected (typep (realpart expected) 'single-float)
                             (typep (realpart expected) 'double-float) (complexp expected))
                        (m:evaluate vm form)) "EXPT integral float mismatch ~S" form)))))
    (dolist (base '(0 0.0 -0.0 0d0 -0d0 #c(0.0 0.0) #c(0d0 0d0)))
      (dolist (power '(1 2 1/3 2.0 2d0 #c(2 3) #c(2.0 0.5) #c(2d0 0.5d0)))
        (let ((form `(let ((expected (* ',base ',power)) (result (expt ',base ',power)))
                       (and (zerop result) (eq (complexp expected) (complexp result))
                         (eq (typep (realpart expected) 'single-float) (typep (realpart result) 'single-float))
                         (eq (typep (realpart expected) 'double-float) (typep (realpart result) 'double-float))))))
          (is (eq t (m:evaluate vm form)) "EXPT zero base mismatch ~S" form))))
    (dolist (source
      '("(handler-case (expt 2 :bad) (type-error () t))"
        "(handler-case (expt :bad 0.0) (type-error () t))"
        "(= (expt -1 1d100) 1d0)"
        "(= (expt 1 -1d100) 1d0)"
        "(= (expt (expt 2 1000) 0d0) 1d0)"))
      (is (eq t (m:evaluate-string vm source)) "EXPT special case mismatch ~A" source))))

(test native-rationalize
  (m:with-interpreter (vm)
    ;; These have simple exact representatives in their float rounding
    ;; intervals. ECL's RATIONALIZE returns the exact binary rational instead.
    (dolist (entry '((0 0) (1 1) (-1 -1) (1/3 1/3) (-11/100 -11/100)
                     (0.0 0) (-0.0 0) (0.5 1/2) (-0.5 -1/2) (0.1 1/10)
                     (-0.1 -1/10) (0.2 1/5) (1.25 5/4) (0.0d0 0) (-0.0d0 0)
                     (0.1d0 1/10) (-0.1d0 -1/10) (0.2d0 1/5) (1.25d0 5/4)))
      (destructuring-bind (value expected) entry
        (is (= expected (m:evaluate vm `(rationalize ,value))) "RATIONALIZE mismatch: ~S" value)))
    (dolist (prototype '(0.0 0.0d0))
      (let ((exponents (if (typep prototype 'double-float) '(-1000 -500 -20 0 20 500 1000)
                           '(-120 -100 -20 0 20 100))))
        (dolist (exponent exponents)
          (dolist (fraction '(1/3 1/10 3/7 101/997 -1/3 -1/10 -3/7 -101/997))
            (let* ((value (scale-float (float fraction prototype) exponent))
                   (result (m:evaluate vm `(rationalize ,value))))
              (is (rationalp result))
              ;; ECL can overflow a large rational denominator while
              ;; converting it back to a float. Check the guest round trip.
              (is (eql value (m:evaluate vm `(float ,result ,prototype)))
                  "RATIONALIZE round trip: ~S => ~S" value result))))))
    (dolist (value (list least-positive-single-float (- least-positive-single-float)
                        least-positive-normalized-single-float
                        (+ least-positive-normalized-single-float least-positive-single-float)
                        least-positive-double-float (- least-positive-double-float)
                        least-positive-normalized-double-float
                        (+ least-positive-normalized-double-float least-positive-double-float)
                        most-positive-single-float most-negative-single-float
                        most-positive-double-float most-negative-double-float))
      (let ((result (m:evaluate vm `(rationalize ,value))))
        (is (rationalp result))
        (is (eql value (m:evaluate vm `(float ,result ,value))) "RATIONALIZE boundary: ~S => ~S" value result)))
    (dolist (source '("(let ((x (/ 7 11))) (eq x (rationalize x)))"
                      "(= (length (multiple-value-list (rationalize (values 0.1 9)))) 1)"
                      "(handler-case (rationalize) (program-error () t))"
                      "(handler-case (rationalize 1 2) (program-error () t))"
                      "(handler-case (rationalize nil) (type-error () t))"
                      "(handler-case (rationalize #c(1 2)) (type-error () t))"))
      (is (eq t (m:evaluate-string vm source))))))

(test native-lcm-and-isqrt
  (m:with-interpreter (vm)
    (dolist (arguments '(() (0) (-17) (0 0) (3 0 5) (0 2 0) (-12 18 35) (1 1 1)))
      (let ((form `(multiple-value-list (lcm ,@arguments))))
        (is (equal (eval form) (m:evaluate vm form)))))
    (loop for bits in '(1 15 16 29 30 31 32 45 60 61 100 200 501 1000 2048) do
      (let ((a (1- (ash 1 bits))) (b (+ (ash 1 (1- bits)) 17)))
        (dolist (form `((lcm ,a ,b) (lcm ,(- a) ,b 35) (isqrt ,a)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (loop for bits from 1 to 200 by 3 for n = (+ (ash 1 bits) 31) do
      (dolist (offset '(-1 0 1))
        (let ((form `(multiple-value-list (isqrt ,(+ (* n n) offset)))))
          (is (equal (eval form) (m:evaluate vm form))))))
    (is (eq t (m:evaluate vm '(loop for i from 0 to 10000 always (= i (isqrt (* i i)))))))
    (dolist (form '((isqrt) (isqrt 0 0)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (dolist (form '((isqrt -1) (isqrt 1/2) (isqrt 1d0) (isqrt nil)
                    (lcm 0 1d0) (lcm 0 nil) (lcm 3/2) (lcm 0 0 #c(1 2))))
      (is (eq t (m:evaluate vm `(handler-case ,form (type-error (c)
        (not (typep (type-error-datum c) (type-error-expected-type c)))))))))))

(test native-conjugation-and-negation
  (m:with-interpreter (vm)
    (dolist (number '(0 17 -17 3/4 -3/4 0.0 -0.0 0d0 -0d0 1.5 -1.5d0
                     #c(3/4 -5/4) #c(0.0 0.0) #c(0d0 -0d0) #c(1d0 2d0)))
      (dolist (operator '(- conjugate))
        (let ((form `(multiple-value-list (,operator ,number))))
          (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))
      (dolist (operator '(- conjugate))
        (let ((form `(let ((result (,operator ,number)))
                       (list (if (floatp (realpart result)) (float-sign (realpart result)) nil)
                             (if (and (complexp result) (floatp (imagpart result)))
                                 (float-sign (imagpart result)) nil)))))
          (is (equal (eval form) (m:evaluate vm form)) "Signed zero mismatch for ~S" form))))
    (dolist (form '((conjugate) (conjugate 1 2)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (dolist (value '(nil a #\a))
      (is (eq t (m:evaluate vm `(handler-case (conjugate ',value) (type-error () t))))))
    (dolist (a '(1/3 -1/3 1000000000000000000000000001/3))
      (dolist (b '(0 1 -1 1000000000000000000000000000))
        (dolist (form `((+ ,a ,b) (+ ,b ,a) (- ,a ,b) (- ,b ,a)))
          (is (equal (eval form) (m:evaluate vm form)) "Reduced ratio mismatch for ~S" form))))))

(test native-normalized-long-division
  (m:with-interpreter (vm)
    (let ((seed 123456789))
      (labels ((bits (count)
                 (setf seed (mod (+ (* seed 6364136223846793005) 1442695040888963407) (ash 1 2048)))
                 (logior (ldb (byte count 0) seed) (ash 1 (1- count))))
               (check (a b)
                 (dolist (signs '((1 1) (-1 1) (1 -1) (-1 -1)))
                   (let ((form `(multiple-value-list (truncate ,(* a (first signs)) ,(* b (second signs))))))
                     (is (equal (eval form) (m:evaluate vm form)) "Division mismatch for ~S" form)))))
        ;; Exercise every normalization shift, estimates at the radix limit,
        ;; and adding the divisor back when the third limb invalidates a trial.
        (loop for shift from 0 below 15 do
          (loop for low in '(1 16383 32767) do
            (let ((b (+ (ash (ash 1 shift) 30) low)))
              (dolist (q '(1 32767 32768 32769 1073741823))
                (dolist (r (list 0 1 (1- b))) (check (+ (* b q) r) b))))))
        (loop repeat 300 for b = (bits (+ 31 (mod seed 700)))
          for a = (bits (+ 31 (mod seed 1500))) do (check a b))))))

(test native-division-and-float-conversion
  (m:with-interpreter (vm :fuel 200000000)
    (dolist (width '(30 31 53 54 60 100 200 500))
      (loop for i from 1 to 20
            for a = (+ (ash 1 width) (* i 104729))
            for b = (- (ash 1 (1- width)) (* i 7919)) do
        (dolist (pair (list (list a b) (list b a) (list (- a) b) (list a (- b)) (list (- a) (- b))))
          (destructuring-bind (x y) pair
            (dolist (op '(floor ceiling truncate round))
              (let ((form `(multiple-value-list (,op ,x ,y))))
                (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
        (is (= (gcd a b) (m:evaluate vm `(gcd ,a ,b))))))
    (dolist (width '(0 1 23 24 25 30 31 52 53 54 60 100))
      (dolist (delta '(-3 -2 -1 0 1 2 3))
        (dolist (sign '(-1 1))
          (let ((n (* sign (+ (ash 1 width) delta))))
            (dolist (prototype '(0.0 0d0))
              (let ((form `(float ,n ,prototype)))
                (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))))))

(defun reference-rounding-result (operator a &optional (b 1))
  ;; Normalize contagion before calling the host, and obtain exact rational
  ;; remainders through the integer-quotient operations. ECL's floating
  ;; quotient variants can otherwise change the remainder's format.
  (let* ((prototype (cond ((or (typep a 'double-float) (typep b 'double-float)) 1d0)
                          ((or (floatp a) (floatp b)) 1.0)))
         (a (if prototype (float a prototype) a))
         (b (if prototype (float b prototype) b))
         (operation (case operator ((floor ffloor mod) 'floor)
                       ((ceiling fceiling) 'ceiling)
                       ((truncate ftruncate rem) 'truncate) ((round fround) 'round))))
    (multiple-value-bind (q r) (funcall operation a b)
      (if (member operator '(mod rem)) (list r)
          (list (if (member operator '(ffloor fceiling ftruncate fround))
                    (float q (or prototype 1.0)) q) r)))))

(test native-real-rounding
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (operator '(floor ceiling truncate round ffloor fceiling ftruncate fround mod rem))
      (dolist (a '(-1073741825 -17 -7/2 -5/2 -3/2 -1 -1/2 0 1/2 1 3/2 5/2 7/2 17 1073741825
                   -3.5 -0.0 0.0 3.5 -3.5d0 0.0d0 3.5d0))
        (dolist (b '(-3 -2 -1 -2/3 2/3 1 2 3 2.0 2.0d0))
          (let* ((form `(multiple-value-list (,operator ,a ,b)))
                 (expected (reference-rounding-result operator a b))
                 (actual (m:evaluate vm form)))
            ;; Host contagion can round a rational divisor before computing
            ;; the remainder. Allow the resulting few floating-point ulps.
            (is (and (= (length expected) (length actual)) (every #'same-number-p expected actual))
                "Mismatch for ~S: expected ~S, received ~S" form expected actual)))))
    (dolist (operator '(floor ceiling truncate round ffloor fceiling ftruncate fround))
      (dolist (a '(-17 -7/2 -5/2 -3/2 -1/2 0 1/2 3/2 5/2 7/2 17 -3.5 3.5d0))
        (let ((form `(multiple-value-list (,operator ,a))))
          ;; Call dynamically: CCL's constant folding of integer CEILING can
          ;; discard the secondary zero remainder.
          (is (equalp (reference-rounding-result operator a) (m:evaluate vm form)) "Mismatch for ~S" form))))
    (dolist (operator '(floor ceiling truncate round ffloor fceiling ftruncate fround mod rem))
      (dolist (a (list (1- (ash 1 100)) (- (1+ (ash 1 100))) (/ (1+ (ash 1 100)) 3)))
        (dolist (b '(7 -7 70000 -70000 7/3 -7/3))
          (let ((form `(multiple-value-list (,operator ,a ,b))))
            (is (equalp (reference-rounding-result operator a b) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (is (equal '(t t 3.0 1/2) (m:evaluate vm
      '(multiple-value-bind (q r) (ffloor 7/2) (list (floatp q) (rationalp r) q r)))))
    (dolist (form
      '((multiple-value-list (floor 1d0 .1d0))
        (multiple-value-list (truncate 1d0 .1d0))
        (multiple-value-list (round 1d0 .1d0))
        (multiple-value-list (fround 3/2))
        (multiple-value-list (ftruncate (expt 2 100)))
        (multiple-value-bind (q r) (floor 7 2d0) (list (integerp q) (typep r 'double-float) q r))
        (multiple-value-bind (q r) (fround 7 2d0) (list (typep q 'double-float) (typep r 'double-float) q r))
        (let ((i 0)) (multiple-value-bind (q r) (floor (incf i) (incf i)) (list i q r)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))

(test native-real-rounding-errors
  (m:with-interpreter (vm)
    (dolist (operator '(floor ceiling truncate round ffloor fceiling ftruncate fround mod rem))
      (dolist (args '(() (1 2 3)))
        (is (eq t (m:evaluate vm `(handler-case (,operator ,@args) (program-error () t))))))
      (dolist (value '(nil a (1 2) #c(1 2)))
        (is (eq t (m:evaluate vm `(handler-case (,operator ',value 2) (type-error () t)))))
        (is (eq t (m:evaluate vm `(handler-case (,operator 2 ',value) (type-error () t))))))
      (dolist (zero '(0 0.0 -0.0 0d0 -0d0))
        (is (eq t (m:evaluate vm `(handler-case (,operator 1 ,zero) (division-by-zero () t)))))))
    (dolist (operator '(mod rem))
      (is (eq t (m:evaluate vm `(handler-case (,operator 1) (program-error () t))))))))

(test native-square-roots-and-signum
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (number '(0 1 -1 17 -17 1/3 -1/3 9/16 -9/16
                     0.0 -0.0 1.5 -1.5 0d0 -0d0 1.5d0 -1.5d0
                     #c(3 4) #c(-3 4) #c(-3 -4) #c(3 -4) #c(0 1) #c(0 -1)
                     #c(3/5 4/5) #c(1.5 2.5) #c(-1.5d0 -2.5d0)
                     #c(0.0 0.0) #c(-0.0 -0.0) #c(-0d0 0d0) #c(0d0 -0d0)))
      (dolist (operator '(sqrt signum))
        (let* ((form `(,operator ,number)) (expected (eval form)))
          ;; Exact roots and unit vectors may stay rational on the host.
          ;; Miricl chooses single floats for these rational inputs.
          (when (and (eq operator 'signum) (complexp number) (rationalp (realpart number)))
            ;; ECL computes these through PHASE, leaving a residual real
            ;; part even for an exact imaginary-axis unit vector.
            (let ((magnitude (sqrt (+ (expt (realpart number) 2) (expt (imagpart number) 2)))))
              (setq expected (complex (/ (realpart number) magnitude) (/ (imagpart number) magnitude)))))
          (when (and (rationalp (realpart number)) (or (eq operator 'sqrt) (complexp number)))
            (setq expected (if (complexp expected)
                               (complex (float (realpart expected) 1.0) (float (imagpart expected) 1.0))
                               (float expected 1.0))))
          (is (same-number-p expected (m:evaluate vm form)) "Root/sign mismatch for ~S" form)
          (is (= 1 (m:evaluate vm `(length (multiple-value-list ,form))))))))
    (dolist (number '(0.0 -0.0 0d0 -0d0 #c(0.0 0.0) #c(-0.0 -0.0) #c(-0d0 0d0) #c(0d0 -0d0)
                     #c(-4d0 0d0) #c(-4d0 -0d0)))
      (dolist (operator '(sqrt signum))
        (let* ((form `(let ((result (,operator ,number)))
                       (list (float-sign (realpart result))
                         (if (complexp result) (float-sign (imagpart result)) nil))))
               ;; SIGNUM preserves zero itself; complex SQRT has +0 real
               ;; part and retains the input imaginary zero's sign. CCL can
               ;; erase or reverse these signs in its zero special cases.
               (zero (when (zerop number)
                       (if (and (eq operator 'sqrt) (complexp number))
                           (complex (float 0 (realpart number)) (imagpart number))
                           number)))
               (expected (if zero
                             (list (float-sign (realpart zero))
                                   (when (complexp zero) (float-sign (imagpart zero))))
                             (eval form))))
          (is (equal expected (m:evaluate vm form)) "Root/sign zero mismatch for ~S" form))))
    (dolist (operator '(sqrt signum))
      (dolist (arguments '(() (1 2)))
        (is (eq t (m:evaluate vm `(handler-case (,operator ,@arguments) (program-error () t))))))
      (dolist (number '(nil a #\a "1"))
        (is (eq t (m:evaluate vm `(handler-case (,operator ',number) (type-error () t)))))))))

(test native-complex-root-extremes
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (number (list #c(1d308 1d308) #c(-1d308 1d308) #c(-1d308 -1d308)
                         #c(1d-300 1d-300) #c(-1d-300 -1d-300)
                         #c(1e30 1e30) #c(-1e-30 -1e-30)))
      (dolist (operator '(sqrt signum abs))
        (let ((form `(,operator ,number)))
          (is (same-number-p (eval form) (m:evaluate vm form)) "Scaled arithmetic mismatch for ~S" form))))
    ;; At the minimum subnormal, dividing by a rounded magnitude loses the
    ;; unit-vector invariant. Check the mathematical result directly here.
    (let ((number (complex least-positive-double-float least-positive-double-float)))
      (is (same-number-p #c(0.7071067811865475d0 0.7071067811865475d0)
        (m:evaluate vm `(signum ,number))))
      (is (plusp (m:evaluate vm `(abs ,number))))
      ;; Scale the known argument 1+i; host complex SQRT can lose accuracy
      ;; when its magnitude calculation involves minimum subnormals.
      (let* ((root (sqrt least-positive-double-float))
             (expected (complex (* root (sqrt (/ (+ (sqrt 2d0) 1) 2)))
                                (* root (sqrt (/ (- (sqrt 2d0) 1) 2))))))
        (is (same-number-p expected (m:evaluate vm `(sqrt ,number))))))
    ;; Unit vectors must remain representable even for arbitrary-size rationals.
    (dolist (component (list (ash 1 2000) (/ 1 (ash 1 2000))))
      (let ((form `(signum (complex ,component ,(- component)))))
        (is (same-number-p #c(0.70710677 -0.70710677) (m:evaluate vm form)))))
    (is (same-number-p 1e30 (m:evaluate vm `(sqrt ,(expt 10 60)))))))
