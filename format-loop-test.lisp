;;;; Guest FORMAT and LOOP, checked against the host Common Lisp implementation.
(in-package #:miricl-test)
(in-suite :miricl)

(test loop-arithmetic-subclause-order
  (m:with-interpreter (vm)
    (dolist (bound '(to upto below downto above))
      (let* ((down (member bound '(downto above)))
             (clauses `((from (progn (push :start order) ,(if down 7 2)))
                        (,bound (progn (push :limit order) ,(if down 2 7)))
                        (by (progn (push :step order) 2)))))
        (dolist (permutation '((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0)))
          (let ((form `(let ((order nil))
                         (list (loop for i ,@(loop for index in permutation append (nth index clauses)) collect i)
                               (reverse order)))))
            (is (equalp (eval form) (m:evaluate vm form)) "Arithmetic LOOP mismatch: ~S" form)))))
    (dolist (form '((loop for x by 2 below 7 collect x)
                    (loop for x by 1/2 below 2 collect x)
                    (loop for x by 0.5 from 0.0 below 2.0 collect x)
                    (loop for x by 2 from #c(0 1) repeat 3 collect x)
                    (loop for x by 2 downfrom #c(5 1) repeat 3 collect x)
                    (let ((x 7)) (loop for x below x from 2 collect x))))
      (is (equalp (eval form) (m:evaluate vm form)) "Arithmetic LOOP edge mismatch: ~S" form))
    ;; The upstream suite's :LOOP-ITERATION-VALUES-IN-FINALLY policy keeps
    ;; the last in-range value. SBCL instead steps beyond the bound, so it
    ;; cannot serve as the reference for this disputed final-value case.
    (is (= 6 (m:evaluate vm
      '(loop for x below 7 from 2 collect x finally (return x)))))
    (is (= 42 (m:evaluate-string vm
      "(macrolet ((loop (&rest forms) (declare (ignore forms)) 42)) (loop :ignored))")))
    (dolist (source '("(loop for x from 0 from 1 repeat 1)"
                      "(loop for x below 3 to 4 repeat 1)"
                      "(loop for x by 1 by 2 repeat 1)"
                      "(loop for x upfrom 1 downto 0 repeat 1)"
                      "(loop for x above 0 repeat 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (is (eq t (m:evaluate-string vm
      "(let ((a (macroexpand-1 '(loop for x below 3 collect x)))
             (b (macroexpand-1 '(loop for x below 3 collect x))))
         (and (not (eq a b)) (equal (eval a) '(0 1 2)) (equal (eval b) '(0 1 2))))")))
    (dolist (name '(loop loop-finish))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))))

(test loop-hash-and-package-iteration
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((table (make-hash-table)))
          (setf (gethash 'a table) 1 (gethash 'b table) 2)
          (loop for value being each hash-value in table sum value))
        (let ((table (make-hash-table)))
          (setf (gethash 'a table) 1 (gethash 'b table) 2)
          (sort (loop for key being the hash-keys of table using (hash-value value)
                      collect (list (symbol-name key) value)) #'string< :key #'car))
        (let ((table (make-hash-table)))
          (setf (gethash '(1 . 2) table) '(3 . 4) (gethash '(5 . 6) table) '(7 . 8))
          (sort (loop for (a . b) of-type (integer . integer) being the hash-keys of table
                      using (hash-value (c . d)) collect (+ a b c d)) #'<))
        (loop for nil being each hash-key of (make-hash-table) count t)
        (let ((table (make-hash-table)))
          (setf (gethash 'a table) 1 (gethash 'b table) 2)
          (list (loop for key being the hash-keys of table using (hash-value nil)
                      do (remhash key table) count t) (hash-table-count table)))
        (let ((table (make-hash-table)) (calls 0))
          (setf (gethash 'a table) 1)
          (list (loop for value being each hash-value of (progn (incf calls) table) sum value) calls))
        (let ((a (make-hash-table)) (b (make-hash-table)))
          (setf (gethash 'a a) 1 (gethash 'b a) 2 (gethash 'x b) 3)
          (loop for x being the hash-values of a and y being the hash-values of b count t))
        (let ((table (make-hash-table)))
          (setf (gethash 'a table) 1)
          (loop for key being the hash-keys of table using (hash-value value)
                initially (assert (hash-table-p table)) collect value))))
      (is (equalp (eval form) (m:evaluate vm form)) "Iterator LOOP mismatch: ~S" form))
    (let ((parent (make-package "MIRICL-LOOP-ITERATOR-PARENT" :use nil)) (child nil))
      (unwind-protect
        (progn
          (intern "PARENT-PRIVATE" parent) (export (intern "PARENT-PUBLIC" parent) parent)
          (setq child (make-package "MIRICL-LOOP-ITERATOR-CHILD" :use (list parent)))
          (intern "CHILD-PRIVATE" child) (export (intern "CHILD-PUBLIC" child) child)
          (m:evaluate-string vm "
            (defpackage \"MIRICL-LOOP-ITERATOR-PARENT\" (:use) (:intern \"PARENT-PRIVATE\") (:export \"PARENT-PUBLIC\"))
            (defpackage \"MIRICL-LOOP-ITERATOR-CHILD\" (:use \"MIRICL-LOOP-ITERATOR-PARENT\")
              (:intern \"CHILD-PRIVATE\") (:export \"CHILD-PUBLIC\"))")
          (dolist (kind '(symbols present-symbols external-symbols))
            (let ((form `(sort (loop for x being the ,kind of "MIRICL-LOOP-ITERATOR-CHILD"
                                    collect (symbol-name x)) #'string<)))
              (is (equal (eval form) (m:evaluate vm form)) "Package LOOP mismatch: ~S" form))))
        (when child (delete-package child)) (delete-package parent)))
    (is (eq t (m:evaluate-string vm
      "(handler-case (loop for x being each symbol of \"MIRICL-MISSING-LOOP-PACKAGE\" count t)
         (package-error () t))")))))

(test loop-types-defaults-and-accumulators
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((i 0)) (loop for nil from 10 to 15 collect (incf i)))
        (let ((i 0)) (loop for nil from 5 above 0 collect (incf i)))
        (loop with a t = 1 return a)
        (loop with (a b) of-type (fixnum fixnum) = '(3 4) return (+ a b))
        (loop with a t and b t return (list a b))
        (loop with (a b c) of-type (fixnum float t) return (list a b c (typep b 'single-float)))
        (loop with nil = (return t) return nil)
        (loop for x in '((a) (b) (c . end)) append x)
        (loop for x in '(a b c) nconc (cons x 'end))
        (loop repeat 2 append '(a . tail) collect :marker)
        (loop for x from 1 to 10 count (< x 5) into foo float
              finally (return (list foo (typep foo 'single-float))))
        (loop for i from 1 to 10 minimize i maximize (- i))
        (loop for i from 1 to 10 maximize (- i) minimize i)
        (loop for i from 5 downto 1 maximize i minimize (- i))
        (let ((x (loop for i in nil sum i of-type single-float))) (list x (type-of x)))
        (let ((x (loop for i in nil sum i of-type double-float))) (list x (type-of x)))
        (loop for i in '(1 2 3) sum i of-type (integer 0 10))
        (let ((it 'z)) (loop for x in '(a b c) when x collect it and collect it))))
      (is (equalp (eval form) (m:evaluate vm form)) "LOOP type/accumulator mismatch: ~S" form))
    ;; CCL fails to initialize an ignored arithmetic variable when FROM is
    ;; omitted. The default start is zero, so there are five iterations.
    (is (equal '(1 2 3 4 5) (m:evaluate vm
      '(let ((i 0)) (loop for nil by 2 below 10 collect (incf i))))))
    ;; The terminating increment must not store an out-of-range value into the
    ;; declared variable. This is also upstream ANSI LOOP.1.39.
    (is (equal '(1 2 3 4 5) (m:evaluate vm '(loop for x of-type (integer 1 5) from 1 to 5 collect x))))
    (is (eq t (m:evaluate vm '(and (integerp multiple-values-limit) (>= multiple-values-limit 20)))))
    (is (= 1000 (m:evaluate vm '(length (multiple-value-list
      (eval (list 'loop 'return (cons 'values (make-list 1000 :initial-element :a)))))))))))

(test format-nested-argument-navigation
  (m:with-interpreter (vm)
    (dolist (form '((format nil "~A~:[~; [~2:*~A]~]" 17 t)
                    (format nil "~A~:[~; [~2:*~A]~]" 17 nil)
                    (format nil "~A~1[unused~;~:*~A~]" "x")
                    (format nil "~A~@[~:*~A/~A~]" "x" "y")
                    (format nil "~A~0[~0[~:*~A~]~]" "x")
                    (format nil "~A~(~:*~A~)" "ABC")
                    (format nil "~A~:(~0@*~A~)" "abc")
                    (format nil "~A~<~:*~A~;~A~>" 17 23)
                    (format nil "~A~<~A~;~2:*~A~>" 17 23)
                    (format nil "~A~{~A~0[~:*~A~]~}" "p" '(1 2 3))
                    (format nil "~A~?" "p" "~A~0[~:*~A~]" '(1 2))
                    (format nil "~D~:[~;~:P~]" 1 t)
                    (multiple-value-list
                      (with-output-to-string (s)
                        (funcall (formatter "~A~:[~; [~2:*~A]~]") s 17 t 23 31)))))
      (let ((expected (eval form)) (actual (m:evaluate vm form)))
        (is (equal expected actual) "Nested FORMAT navigation: ~S expected ~S actual ~S" form expected actual)))
    ;; The ANSI RT reporter prints the original error this way. Reporting must
    ;; not signal a second error while navigating back across the conditional.
    (dolist (form '((missing-printer-frame-function) missing-printer-frame-variable (/ 1 0)
                    (+ 1 'bad) (car 1) (error "original simple error")))
      (is (eq t (m:evaluate vm `(handler-case ,form
                    (error (condition)
                      (let ((text (format nil "~10t~S~:[~; [~2:*~A]~]~%"
                                    condition (typep condition 'condition))))
                        (and (stringp text) (search " [" text) t))))))))))

(defparameter *format-real-field-cases*
  '(("~F" "0.0" "1.0" "0.5" "-0.5" "1.25" "20.0" "-20.0" "0.0" "-0.0" "0.5" "-0.5" "1.25" "0.5" "-0.5" "1.25")
    ("~8F" "     0.0" "     1.0" "     0.5" "    -0.5" "    1.25" "    20.0" "   -20.0" "     0.0" "    -0.0" "     0.5" "    -0.5" "    1.25" "     0.5" "    -0.5" "    1.25")
    ("~3,2F" ".00" "1.00" ".50" "-.50" "1.25" "20.00" "-20.00" ".00" "-.00" ".50" "-.50" "1.25" ".50" "-.50" "1.25")
    ("~2,1F" ".0" "1.0" ".5" "-.5" "1.3" "20.0" "-20.0" ".0" "-.0" ".5" "-.5" "1.3" ".5" "-.5" "1.3")
    ("~4,2@F" "+.00" "+1.00" "+.50" "-.50" "+1.25" "+20.00" "-20.00" "+.00" "-.00" "+.50" "-.50" "+1.25" "+.50" "-.50" "+1.25")
    ("~2,2F" ".00" "1.00" ".50" "-.50" "1.25" "20.00" "-20.00" ".00" "-.00" ".50" "-.50" "1.25" ".50" "-.50" "1.25")
    ("~8,3,2F" "   0.000" " 100.000" "  50.000" " -50.000" " 125.000" "2000.000" "-2000.000" "   0.000" "  -0.000" "  50.000" " -50.000" " 125.000" "  50.000" " -50.000" " 125.000")
    ("~8,3,-2F" "   0.000" "   0.010" "   0.005" "  -0.005" "   0.013" "   0.200" "  -0.200" "   0.000" "  -0.000" "   0.005" "  -0.005" "   0.013" "   0.005" "  -0.005" "   0.013")
    ("~3,2,,'*F" ".00" "***" ".50" "***" "***" "***" "***" ".00" "***" ".50" "***" "***" ".50" "***" "***")
    ("~10,2,,'*,'_@F" "_____+0.00" "_____+1.00" "_____+0.50" "_____-0.50" "_____+1.25" "____+20.00" "____-20.00" "_____+0.00" "_____-0.00" "_____+0.50" "_____-0.50" "_____+1.25" "_____+0.50" "_____-0.50" "_____+1.25")
    ("~E" "0.0e+0" "1.0e+0" "5.0e-1" "-5.0e-1" "1.25e+0" "2.0e+1" "-2.0e+1" "0.0e+0" "-0.0e+0" "5.0e-1" "-5.0e-1" "1.25e+0" "5.0d-1" "-5.0d-1" "1.25d+0")
    ("~6E" "0.0e+0" "1.0e+0" "5.0e-1" "-5.e-1" "1.3e+0" "2.0e+1" "-2.e+1" "0.0e+0" "-.0e+0" "5.0e-1" "-5.e-1" "1.3e+0" "5.0d-1" "-5.d-1" "1.3d+0")
    ("~5E" ".0e+0" "1.e+0" "5.e-1" "-5.e-1" "1.3e+0" "2.e+1" "-2.e+1" ".0e+0" "-.0e+0" "5.e-1" "-5.e-1" "1.3e+0" "5.d-1" "-5.d-1" "1.3d+0")
    ("~4E" ".0e+0" "1.e+0" "5.e-1" "-5.e-1" "1.25e+0" "2.e+1" "-2.e+1" ".0e+0" "-.0e+0" "5.e-1" "-5.e-1" "1.25e+0" "5.d-1" "-5.d-1" "1.25d+0")
    ("~6@E" "+.0e+0" "+1.e+0" "+5.e-1" "-5.e-1" "+1.3e+0" "+2.e+1" "-2.e+1" "+.0e+0" "-.0e+0" "+5.e-1" "-5.e-1" "+1.3e+0" "+5.d-1" "-5.d-1" "+1.3d+0")
    ("~5@E" "+.0e+0" "+1.e+0" "+5.e-1" "-5.e-1" "+1.25e+0" "+2.e+1" "-2.e+1" "+.0e+0" "-.0e+0" "+5.e-1" "-5.e-1" "+1.25e+0" "+5.d-1" "-5.d-1" "+1.25d+0")
    ("~7,2E" "0.00e+0" "1.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "2.00e+1" "-2.00e+1" "0.00e+0" "-.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "5.00d-1" "-5.00d-1" "1.25d+0")
    ("~6,2E" ".00e+0" "1.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "2.00e+1" "-2.00e+1" ".00e+0" "-.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "5.00d-1" "-5.00d-1" "1.25d+0")
    ("~7,2@E" "+.00e+0" "+1.00e+0" "+5.00e-1" "-5.00e-1" "+1.25e+0" "+2.00e+1" "-2.00e+1" "+.00e+0" "-.00e+0" "+5.00e-1" "-5.00e-1" "+1.25e+0" "+5.00d-1" "-5.00d-1" "+1.25d+0")
    ("~,2E" "0.00e+0" "1.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "2.00e+1" "-2.00e+1" "0.00e+0" "-0.00e+0" "5.00e-1" "-5.00e-1" "1.25e+0" "5.00d-1" "-5.00d-1" "1.25d+0")
    ("~,2,,0E" "0.00e+1" "0.10e+1" "0.50e+0" "-0.50e+0" "0.13e+1" "0.20e+2" "-0.20e+2" "0.00e+1" "-0.00e+1" "0.50e+0" "-0.50e+0" "0.13e+1" "0.50d+0" "-0.50d+0" "0.13d+1")
    ("~,2,,2E" "0.0e-1" "10.0e-1" "50.0e-2" "-50.0e-2" "12.5e-1" "20.0e+0" "-20.0e+0" "0.0e-1" "-0.0e-1" "50.0e-2" "-50.0e-2" "12.5e-1" "50.0d-2" "-50.0d-2" "12.5d-1")
    ("~,2,,3E" "0.0e-2" "100.e-2" "500.e-3" "-500.e-3" "125.e-2" "200.e-1" "-200.e-1" "0.0e-2" "-0.0e-2" "500.e-3" "-500.e-3" "125.e-2" "500.d-3" "-500.d-3" "125.d-2")
    ("~,3,,-1E" "0.000e+2" "0.010e+2" "0.050e+1" "-0.050e+1" "0.013e+2" "0.020e+3" "-0.020e+3" "0.000e+2" "-0.000e+2" "0.050e+1" "-0.050e+1" "0.013e+2" "0.050d+1" "-0.050d+1" "0.013d+2")
    ("~6,2,,,'*E" ".00e+0" "******" "******" "******" "******" "******" "******" ".00e+0" "******" "******" "******" "******" "******" "******" "******")
    ("~7,2,,,'*E" "0.00e+0" "1.00e+0" "5.00e-1" "*******" "1.25e+0" "2.00e+1" "*******" "0.00e+0" "-.00e+0" "5.00e-1" "*******" "1.25e+0" "5.00d-1" "*******" "1.25d+0")
    ("~10,1E" "    0.0e+0" "    1.0e+0" "    5.0e-1" "   -5.0e-1" "    1.3e+0" "    2.0e+1" "   -2.0e+1" "    0.0e+0" "   -0.0e+0" "    5.0e-1" "   -5.0e-1" "    1.3e+0" "    5.0d-1" "   -5.0d-1" "    1.3d+0")
    ("~10,1,,,,'*E" "****0.0e+0" "****1.0e+0" "****5.0e-1" "***-5.0e-1" "****1.3e+0" "****2.0e+1" "***-2.0e+1" "****0.0e+0" "***-0.0e+0" "****5.0e-1" "***-5.0e-1" "****1.3e+0" "****5.0d-1" "***-5.0d-1" "****1.3d+0")
    ("~,2,,,,,'^E" "0.00^+0" "1.00^+0" "5.00^-1" "-5.00^-1" "1.25^+0" "2.00^+1" "-2.00^+1" "0.00^+0" "-0.00^+0" "5.00^-1" "-5.00^-1" "1.25^+0" "5.00^-1" "-5.00^-1" "1.25^+0")
    ("~12,2,4E" "  0.00e+0000" "  1.00e+0000" "  5.00e-0001" " -5.00e-0001" "  1.25e+0000" "  2.00e+0001" " -2.00e+0001" "  0.00e+0000" " -0.00e+0000" "  5.00e-0001" " -5.00e-0001" "  1.25e+0000" "  5.00d-0001" " -5.00d-0001" "  1.25d+0000")))

(test format-real-number-fields
  (m:with-interpreter (vm)
    ;; Exact strings preserve Miricl's precision, rounding, and exponent-marker
    ;; choices. Host FORMAT implementations differ, and CCL errors for some of
    ;; these valid fields instead of producing output.
    (dolist (entry *format-real-field-cases*)
      (loop for number in '(0 1 1/2 -1/2 5/4 20 -20 0.0 -0.0 0.5 -0.5 1.25 0.5d0 -0.5d0 1.25d0)
            for expected in (cdr entry) do
        (let* ((form `(format nil ,(car entry) ,number)) (actual (m:evaluate vm form)))
          (is (equal expected actual) "Real field mismatch: ~S expected ~S actual ~S" form expected actual))))
    (dolist (entry '(((format nil "~3F" 0.000001) "0.0")
                     ((format nil "~,2,1,1,'*E" 1d100) "1.00d+100")
                     ((format nil "~,2,1,1E" 1d100) "1.00d+100")
                     ((let ((*print-readably* t) (*print-base* 16) (*print-radix* t))
                        (format nil "~F/~E" 1/2 1/2)) "0.5/5.0e-1")))
      (destructuring-bind (form expected) entry
        (let ((actual (m:evaluate vm form)))
          (is (equal expected actual) "Real edge mismatch: ~S expected ~S actual ~S" form expected actual))))
    ;; Tight default fields permit different precision choices. Keep the ANSI
    ;; suite's choices here; current SBCL uses a different last-digit policy.
    (is (equal "1.0 2.0" (m:evaluate vm '(format nil "~2F ~2F" 1.1 1.9))))
    (is (equal ".0 .0 .0" (m:evaluate vm '(format nil "~2F ~1F ~0F" 0.01 0.01 0.01))))
    ;; K=1 retains one digit before the point when rounding crosses a decade.
    (is (equal "     1.00d+3" (m:evaluate vm '(format nil "~12,2E" 999.999d0))))
    ;; Non-real arguments use ~D's aesthetic decimal fallback.
    (is (equal "(17 23)/(17 23)" (m:evaluate vm '(format nil "~8F/~8E" '(17 23) '(17 23)))))
    (is (equal "0." (m:evaluate vm '(format nil "~0,0F" 0.01))))
    (is (equal "10.0" (m:evaluate vm '(format nil "~,,2F" 0.1))))
    ;; Default precision follows the implementation's free-format printer.
    (dolist (number '(0.00390625 0.0000001 12345.125 -0.00390625 1.23456789d0 1d100 1d-100))
      (is (eq t (m:evaluate vm `(let* ((x ,number) (*read-default-float-format* (type-of x))
                                        (s (format nil "~F" x)))
                                 (= x (read-from-string s)))))))))

(test format-number-names-and-widths
  (m:with-interpreter (vm)
    (dolist (control '("~R" "~:R"))
      (dolist (number '(0 1 2 3 10 11 12 19 20 21 30 40 99 100 101 999 1000 1001
                       1000000 1234000000 -1 -21 -100))
        (let ((form `(format nil ,control ,number)))
          (is (equal (eval form) (m:evaluate vm form)) "Number name mismatch: ~S" form))))
    (dolist (control '("~@R" "~:@R"))
      (dolist (number '(1 4 5 9 10 40 49 90 99 400 499 900 999 1000 1999 3999))
        (let ((form `(format nil ,control ,number)))
          (is (equal (eval form) (m:evaluate vm form)) "Roman numeral mismatch: ~S" form))))
    (dolist (form
      '((format nil "~3,,-1A/~3,,-1S" "ABC" 'ABCD)
        (format nil "~2,,,,1000000000000000000R" 17)
        (let ((*print-base* 16) (*print-radix* t) (*print-readably* t))
          (format nil "~20D" '(17 31)))
        (let ((*print-base* 16) (*print-radix* t)) (format nil "~20D" 1/3))
        (let ((*print-pretty* t) (*print-circle* t))
          (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (setf (cdr x) x) x)))
        (let ((*print-pretty* t) (*print-circle* t))
          (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (cons x x))))
        (let ((*print-pretty* t) (*print-length* 2))
          (format nil "~:<~@{~A~^ ~}~:>" '(1 2 3 4)))
        (let ((*print-pretty* t)) (format nil "~:<~@{~A~^ ~}~:>" '(1 2 . 3)))))
      (is (equal (eval form) (m:evaluate vm form)) "Number/argument formatting mismatch: ~S" form))
    ;; The ANSI suite permits this width extension; current SBCL rejects it.
    (is (equal "1234" (m:evaluate vm '(format nil "~-1000000000000000000D" 1234))))))

(test format-layout-directives
  (m:with-interpreter (vm)
    (dolist (case '(("~10:@<abcdef~>" nil)
                    ;; Segment columns start at zero in Miricl. CCL's
                    ;; temporary justification streams do not track tabs.
                    ("~15,,,'*<AA~4T~;BBBB~;CCCC~>" nil "AA  *BBBB**CCCC")
                    ("~<XXXXXX~;YYYYYYY~^~;ZZZZ~>" nil)
                    ("~<~<XXXXXX~;YYYYYYY~^~>~>" nil)
                    ("~6<abc~;def~^~>" nil)
                    ("~6@<abc~;def~^~>" nil)
                    ("AA~4T~8,,,'*<~%BBBB~,11:;CCCC~;DDDD~>" nil)
                    ("~13,,2<aaa~;bbb~;ccc~>" nil)
                    ("~4@<~>" nil)
                    ("~:<[~;~@{~A~^/~}~:>" ((1 2 3)))
                    ("~:@<~@{~A~^ ~}~:@>" (1 2 3))
                    ("~@<**~@;~@{~A~^       ~}~;XX~:@>" (1 2 3))
                    ("~/pprint-linear/" ((1 2)))
                    ("~<~4:/pprint-tabular/~:>" (((17 23))))))
      (destructuring-bind (control arguments &optional expected) case
        (let ((form `(let ((*print-pretty* t) (*print-right-margin* 3))
                       (format nil ,control ,@(mapcar (lambda (arg) (list 'quote arg)) arguments)))))
          (is (equal (or expected (eval form)) (m:evaluate vm form)) "Format layout mismatch: ~S" form))))
    (dolist (form
      '((format nil "~{~}" (formatter "~A") '(1 2 3))
        (format nil "~1{~}" (formatter "") '(1 2 3))
        (format nil "~:{~}" (formatter "~A") '((1) (2) (3)))
        (format nil "~@?/~A" (formatter "~A") 17 23)
        (let ((*print-escape* nil)) (format nil "~W" #\Space))))
      (is (equal (eval form) (m:evaluate vm form)) "Function format control mismatch: ~S" form))
    (dolist (source '("~<~W~>" "~<~_~>" "~<~I~>" "~<~:T~>" "~W~<X~:;Y~>"))
      (is (eq t (m:evaluate vm `(handler-case (format nil ,source 17) (error () t))))))
    (m:evaluate-string vm "
      (defun cl-user::printer-callback-probe (stream object colon at &rest parameters)
        (declare (ignore object colon at)) (prin1 parameters stream) (values 17 23))")
    (is (equal "(1 2 3 4 5 6 7 8 9 10 -12345678901234567890 #\\X)"
      (m:evaluate-string vm "
        (format nil \"~1,2,3,4,5,6,7,8,9,10,-12345678901234567890,'X/printer-callback-probe/\" 17)")))
    (is (equal "(NIL :MARKER)" (m:evaluate-string vm
      "(format nil \"~,v/printer-callback-probe/\" :marker 17)")))))

(test format-string-destinations
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((target (make-array 20 :element-type 'character :fill-pointer 3 :initial-element #\x)))
          (list (multiple-value-list (format target "~D/~D" 17 23)) target
                (fill-pointer target) (aref target 15)))
        (let ((target (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
          (format target "~A" "λ🙂") (format target (formatter "~A/~A") 17 23)
          (list target (fill-pointer target)))
        (let ((target (make-array 4 :element-type 'base-char :fill-pointer 0 :adjustable t)))
          (format target "~D/~D" 17 23) target)
        (let* ((storage (make-array 20 :element-type 'character :initial-element #\x))
               (target (make-array 12 :element-type 'character :fill-pointer 2
                         :displaced-to storage :displaced-index-offset 3)))
          (format target "~A" "λ🙂") (list target storage))
        (let ((target (make-array 20 :element-type 'character :fill-pointer 3 :initial-element #\Newline)))
          (format target "~&~A" 17) target)
        (let ((target (make-array 10 :element-type 'character :fill-pointer 1 :initial-element #\x)))
          (format target (lambda (stream) (write-string "y" stream) (values 17 23))) target)
        (let ((target (make-array 10 :element-type 'character :fill-pointer 0 :initial-element #\x)))
          (format target "") (list target (fill-pointer target)))
        (let ((target (make-array 20 :element-type 'character :fill-pointer 1 :initial-element #\x)))
          (format target "~A" "prefix") (format target "suffix") target)))
      (is (equalp (eval form) (m:evaluate vm form)) "String destination mismatch: ~S" form))
    ;; Miricl tracks the existing destination's column. Some hosts initialize
    ;; a string output stream at column zero regardless of its initial text.
    (dolist (write '((format target "~&~A" 17)
                     (with-output-to-string (stream target) (format stream "~&~A" 17))))
      (is (equal (format nil "xxx~%17") (m:evaluate vm
        `(let ((target (make-array 20 :element-type 'character :fill-pointer 3 :initial-element #\x)))
           ,write target)))))
    (dolist (form '((format "plain" "~A" 17)
                   (format (make-array 0 :element-type 'character :fill-pointer 0) "overflow")))
      (is (eq t (m:evaluate vm `(handler-case ,form (error () t))))))))

(test formatter-functions
  (m:with-interpreter (vm)
    (dolist (case '(("literal" (1 2 3))
                    ("~A/~S" (17 23 31))
                    ("~A~0^ignored" (17 23))
                    ("~A~^/~A" (17))
                    ("~A~^/~A" (17 23 31))
                    ("~{~A~^,~}" ((1 2 3) 17))
                    ("~@{~A~^,~}" (1 2 3))
                    ("~:{~A~:^;~}" (((1) (2) (3)) 17))
                    ("~[zero~;one~;two~] ~A" (1 17 23))
                    ("~(~A~) ~A" (:HELLO 17 23))
                    ("~A~:*~A~A" (17 23 31))
                    ("~2*~A" (17 23 31 41))
                    ("~A~0@*~A" (17 23 31))
                    ("~?~A" ("~A~^unused" (17) 23 31))
                    ("~@?~A" ("~A" 17 23 31))
                    ("~V,'0D" (5 17 23))
                    ("~#*" (17 23))))
      (destructuring-bind (control arguments) case
        (let ((form `(let ((tail nil))
                       (list (with-output-to-string (stream)
                               (setq tail (apply (formatter ,control) stream ',arguments))) tail))))
          (is (equal (eval form) (m:evaluate vm form)) "Formatter mismatch: ~S" form))))
    (dolist (form
      '((functionp (formatter "~A"))
        (equal (format nil (formatter "~A/~A") 17 23 31) "17/23")
        (equal (multiple-value-list (format nil (lambda (stream)
                 (write-string "x" stream) (values 17 23)))) '("x"))
        (equal (multiple-value-list (format nil (lambda (stream)
                 (write-string "x" stream) (values)))) '("x"))
        (let ((stream (make-string-output-stream)))
          (equal (multiple-value-list (format stream (lambda (stream)
                   (declare (ignore stream)) (values 17 23)))) '(nil)))
        (let ((stream (make-string-output-stream)))
          (and (= 1 (length (multiple-value-list (funcall (formatter "") stream 17))))
               (equal "" (get-output-stream-string stream))))
        (let ((stream (make-string-output-stream)))
          (format stream "prefix") (funcall (formatter "~&~A") stream 17)
          (equal (get-output-stream-string stream) (format nil "prefix~%17")))
        (let ((saved *standard-output*) (stream (make-string-output-stream)))
          (handler-case (funcall (formatter "prefix~A") stream) (error () nil))
          (and (eq saved *standard-output*) (equal (get-output-stream-string stream) "prefix")
               (equal (format nil "~A" 17) "17")))
        (let ((control "~A"))
          (declare (ignore control))
          (handler-case (macroexpand '(formatter control)) (type-error () t)))
        (let ((count 0))
          (handler-case (macroexpand '(formatter (incf count))) (type-error () nil))
          (= count 0))
        (handler-case (macroexpand '(formatter)) (program-error () t))
        (handler-case (macroexpand '(formatter "" "")) (program-error () t))))
      (is (eq t (m:evaluate vm form)) "Formatter invariant: ~S" form))
    (is (eq t (m:evaluate-string vm "
      (progn (defclass formatter-output-probe () ())
        (defparameter *formatter-expected-stream* nil)
        (defmethod print-object ((object formatter-output-probe) stream)
          (declare (ignore object))
          (write-string (if (eq *formatter-expected-stream* *standard-output*) \"bound\" \"wrong\") stream))
        (let ((*formatter-expected-stream* (make-string-output-stream)))
          (funcall (formatter \"~A\") *formatter-expected-stream* (make-instance 'formatter-output-probe))
          (equal (get-output-stream-string *formatter-expected-stream*) \"bound\")))")))))

(test native-constant-numeric-collection
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (end '(-1 0 1 2 3 7/2 5.0d0))
      (dolist (start '(-2 0 2 1/2 1.5d0))
        (dolist (bound '(to upto below))
          (let ((form `(loop for i from ,start ,bound ,end collect '(a))))
            (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
          (let ((form `(loop for i from ,start ,bound ,end collect i)))
            (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))
      (dolist (bound '(to upto below))
        (dolist (value '(i :same))
          (let ((form `(loop for i ,bound ,end collect ,value)))
            (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (dolist (form
      '((loop for i upfrom 1 below 5 collecting :same)
        (loop for i from 1073741821 to 1073741823 collect i)
        (loop for i from 1073741822 below 1073741826 collect i)
        (loop for i from -1073741823 to -1073741821 collect i)
        (loop for i from -1073741826 below -1073741822 collect i)
        (loop for i from 1000000000000000000000000 to 1000000000000000000000002 collect i)
        (loop for i from 1 to 3 collect nil)
        (loop for i from 1 to 3 collect t)
        (loop for i from 1 to 3 collect 42)
        (let ((i 99) (capture nil))
          (list (loop for i below (progn (setq capture (lambda () i)) 4) collect i) (funcall capture)))
        (let ((i 99)) (declare (special i))
          (list (loop for i from 2 to i collect i) i))
        (let ((i 99) (order nil))
          (list (loop for i from (progn (push i order) 2)
                          to (progn (push i order) 4) collect i) order))
        (let ((i 99) (capture nil))
          (list (loop for i from 2 to (progn (setq capture (lambda () i)) 4) collect :x) (funcall capture)))
        (let ((i 99) (capture nil))
          (list (loop for i from 2 below (progn (setq capture (lambda () i)) 4) collect :x) (funcall capture)))
        (let ((i 99) (capture nil))
          (list (loop for i from 5 to (progn (setq capture (lambda () i)) 3) collect :x) (funcall capture)))
        (let ((i 99) (capture nil))
          (list (loop for i from 1.5d0 to (progn (setq capture (lambda () i)) 3.0d0) collect :x) (funcall capture)))
        (let ((x 0)) (list (loop for i from (incf x) to (incf x) collect :x) x))
        (loop for i from 1 to (return :early) collect :x)
        (let ((output (loop for i from 1 to 3 collect '(a)))) (eq (car output) (cadr output)))
        (let ((code (copy-tree '(loop for i from 1 to 3 collect '(a)))))
          (let ((a (eval code))) (setf (nth 6 code) 2) (list a (eval code))))))
      (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    (is (= 20000 (m:evaluate vm '(length (loop for i from 1 to 20000 collect :x)))))
    (is (= 20000 (m:evaluate vm '(length (loop for i below 20000 collect i)))))
    (is (eq t (m:evaluate vm '(handler-case (loop for i from nil to 3 collect :x) (type-error () t)))))
    (is (eq t (m:evaluate vm '(handler-case (loop for i from 1 to nil collect :x) (type-error () t)))))))

(test native-backquote-evaluation
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (source
      '("`nil" "`17" "`(1 2 . 3)"
        "(let ((x 3)) `(a b ,x ,(+ x 1) x))"
        "(let ((x '(a b c))) `(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x)))"
        "(let ((x '(1 2))) `(,@nil ,@x ,@nil 3 ,@x))"
        "(let ((x '(1 2))) `(0 ,@x . 3))"
        "(let ((x '(1 2))) `(0 . ,x))"
        "(let ((x (list 1 2))) `(0 ,.x 3))"
        "(let ((x (list 1 2))) `(,@x ,(progn (setf (car x) 99) 3)))"
        "(let ((x 3)) (eval ``(a ,,x)))"
        "(let ((x '(1 2))) (eval ``(a ,@',x)))"
        "(let ((x 3)) (eval (eval ```(a ,,,x))))"
        "(let ((x 3)) (eval ``(a . ,,x)))"
        "(let ((seen nil)) (list `(,(push 1 seen) ,@(progn (push 2 seen) '(a b)) ,(push 3 seen)) seen))"
        "(multiple-value-list `(,(values 1 2) ,@(values '(3 4) 99)))"
        "(multiple-value-list `,(values 1 2))"
        "(let ((x (list 1 2))) (let ((out `(0 ,@x 3))) (setf (cadr out) 99) (list x out)))"
        "(let ((saved nil)) (dotimes (i 3) (push `(,i a) saved)) (nreverse saved))"
        "(macrolet ((example (x &rest body) `(let ((v ,x)) ,@body v))) (example 3 (incf v)))"))
      (let ((*package* (find-package :miricl-test)))
        (is (equal (eval (read-from-string source)) (clos-eval vm source))
            "Backquote mismatch for ~A" source)))
    (let ((source (format nil "(length `(~{~D ~}))" (loop for i below 10000 collect i))))
      (is (= 10000 (m:evaluate-string vm source))))))

(test native-case-selection
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (case 1))
        (multiple-value-list (case (values 1 2) (1)))
        (multiple-value-list (case 1 (1 (values 2 3))))
        (multiple-value-list (case 2 (1 :bad) (t (values 3 4))))
        (case nil (nil :bad) ((nil) :nil) (otherwise :bad))
        (case t ((t otherwise) :literal) (otherwise :bad))
        (case 'otherwise ((t otherwise) :literal) (otherwise :bad))
        (case 1 (1.0 :bad) ((1 2) :integer) (t :bad))
        (case #\λ ((#\Λ #\λ) :unicode))
        (let ((n 0)) (list (case (incf n) (1 (incf n) n) (otherwise :bad)) n))
        (let ((x 1)) (case x (1 (setq x 9))) x)
        (block nil (case 1 (1 (return (values 3 4)))))
        (catch 'done (case 2 (2 (throw 'done :done))))
        (let ((count 0))
          (let ((*macroexpand-hook* (lambda (fn form env)
                  (if (eq (car form) 'case) (setq count (+ count 1)) nil) (funcall fn form env))))
            (let ((expanded (macroexpand-1 '(case 1 (1 :yes)))))
              (list (eval expanded) (> count 0)))))
        (eval (funcall (macro-function 'case) '(case 2 ((1 2) :expanded) (otherwise :bad)) nil))))
      (is (equal (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
          "CASE mismatch for ~S" form))
    (is (eq :local (m:evaluate vm
      '(macrolet ((case (&rest args) (declare (ignore args)) :local)) (case nil)))))
    (dolist (source '("(macroexpand-1 '(case))" "(macroexpand-1 '(case 1 (t :bad) (1 :bad)))"
                      "(macroexpand-1 '(case 1 (otherwise :bad) (1 :bad)))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))))

(test loop-expansion-reuse
  (m:with-interpreter (vm :fuel 200000000)
    (dolist (form
      '((let ((code (copy-tree '(loop for i from 0 below 3 collect i))))
          (let ((a (eval code)) (b (eval code)))
            (setf (nth 6 code) 2) (list a b (eval code))))
        (let ((code (copy-tree '(loop for i from 0 below 3 collect (+ i 1)))))
          (let ((a (eval code)))
            (setf (third (nth 8 code)) 2) (list a (eval code))))
        (let ((code (copy-tree '(loop for i from 0 below 3 collect (+ i 1)))))
          (eval code) (setf (nth 7 code) 'sum) (list (eval code) (eval code)))
        (progn
          (defun loop-cached-transfer (n)
            (loop for i from 0 below n do (when (= i 2) (return (values :done i)))))
          (list (multiple-value-list (loop-cached-transfer 4))
            (multiple-value-list (loop-cached-transfer 1))
            (multiple-value-list (loop-cached-transfer 4))))
        (let ((code (copy-tree '(loop for i from 0 below 3 sum i))))
          (let ((a (eval code)))
            (dotimes (n 40) (eval (list 'loop 'for 'i 'from 0 'below n 'sum 'i)))
            (list a (eval code))))))
      (is (equal (eval form) (m:evaluate vm form)) "LOOP expansion mismatch for ~S" form))
    ;; Captured cells expose the same final-value policy as FINALLY above.
    (is (equal '((1 1) (3 3 3 3) nil) (m:evaluate vm
      '(progn
         (defun loop-cached-capture (n)
           (let ((saved nil))
             (loop for i from 0 below n do (push (lambda () i) saved))
             (mapcar #'funcall saved)))
         (list (loop-cached-capture 2) (loop-cached-capture 4) (loop-cached-capture 0))))))
    (let* ((body (cons 'progn (append (make-list 300 :initial-element nil) '(7))))
           (form `(let ((code (list 'loop 'for 'i 'from 0 'below 3 'collect ',body)))
                    (list (eval code) (eval code)))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-list-loop-tests
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (loop for x in nil always (error "Not reached")))
        (multiple-value-list (loop for x in nil never (error "Not reached")))
        (loop for x in '(1 2 3) always (values x nil))
        (loop as x in '(nil nil nil) never (values x t))
        (let ((calls 0)) (list (loop for x in '(1 nil 2) always (progn (incf calls) x)) calls))
        (let ((calls 0)) (list (loop for x in '(nil 1 nil) never (progn (incf calls) x)) calls))
        (let ((x '(1 2))) (loop for x in x always x))
        (let ((calls 0))
          (list (loop for x in (progn (incf calls) '(1 2)) always (progn (incf calls) x)) calls))
        (let ((saved nil))
          (loop for x in '(1 2) always (progn (push (lambda () x) saved) t))
          (mapcar #'funcall saved))
        (multiple-value-list (loop for x in '(1 2) always (return (values 3 4))))
        (multiple-value-list (loop for x in '(nil 2) never (return (values 3 4))))
        (loop for x in '(1 2) always (loop-finish))
        (loop for x in '(nil 2) never (loop-finish))
        (block done (loop for x in '(1 2) always (return-from done 9)))
        (let ((x (list 1 2 3)))
          (loop for e in x always (progn (rplacd x nil) e)))))
      (is (equal (eval form) (m:evaluate vm form))))
    (is (equal '(t 0) (m:evaluate-string vm "
      (defvar *list-loop-variable* 0)
      (list (loop for *list-loop-variable* in '(1 2)
                  always (= *list-loop-variable* (symbol-value '*list-loop-variable*)))
            *list-loop-variable*)")))
    (is (equal '(t 0) (m:evaluate-string vm "
      (list (loop for *list-loop-variable* in '(1 2) always (loop-finish))
            *list-loop-variable*)")))
    (is (equal '(9 0) (m:evaluate-string vm "
      (list (catch 'done (loop for *list-loop-variable* in '(1 2) always (throw 'done 9)))
            *list-loop-variable*)")))))

(test native-list-loop-accumulation
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (operator '(collect collecting append appending nconc nconcing))
      (dolist (condition '(nil when if unless))
        (dolist (sequence '(nil (1) (1 2 3 4)))
          (let ((form `(loop as x in ',sequence
                         ,@(when condition `(,condition (oddp x)))
                         ,operator ,(if (member operator '(collect collecting)) '(values x :ignored) '(list x x)))))
            (is (equal (eval form) (m:evaluate vm form)) "Loop accumulation mismatch for ~S" form)))))
    (dolist (form
      '((loop for x in '(nil 1 nil 2) when (values x :ignored) collect it)
        (loop for x in '(nil 1 nil 2) unless x collect it)
        (let ((x '(1 2))) (loop for x in x collect x))
        (let ((seen nil))
          (list (loop for x in (progn (push :sequence seen) '(1 2 3))
                      when (progn (push x seen) (oddp x)) collect (progn (push :body seen) x)) seen))
        (let ((saved nil))
          (loop for x in '(1 2) collect (progn (push (lambda () x) saved) x))
          (mapcar #'funcall saved))
        (loop for x in '(1 2 3) collect (if (= x 3) (loop-finish) x))
        (loop for x in '(1 2 3) when (if (= x 3) (loop-finish) t) collect x)
        (loop for x in '(1 2 3) append (if (= x 3) (loop-finish) (list x)))
        (loop for x in '(1 2 3) nconc (if (= x 3) (loop-finish) (list x)))
        (multiple-value-list (loop for x in '(1 2) collect (return (values 3 4))))
        (multiple-value-list (loop for x in nil collect x))
        (block done (loop for x in '(1 2) collect (return-from done 9)))
        (catch 'done (loop for x in '(1 2) collect (throw 'done (values 3 4))))
        (let* ((a (list 1 2)) (b (list 3 4)) (out (loop for x in (list a b) append x)))
          (setf (car out) :changed) (list a b out (eq out a) (eq (cddr out) b)))
        (let* ((a (list 1 2)) (b (list 3 4)) (out (loop for x in (list a b) nconc x)))
          (list a b out (eq out a) (eq (cddr out) b)))
        (loop for x in '(1 2) collect (loop for y in '(3 4) collect (+ x y)))))
      (is (equal (eval form) (m:evaluate vm form)) "Loop accumulation mismatch for ~S" form))
    (is (equal '((1 2) 0) (m:evaluate-string vm "
      (defvar *native-collect-variable* 0)
      (list (loop for *native-collect-variable* in '(1 2) collect (symbol-value '*native-collect-variable*))
            *native-collect-variable*)")))
    (is (equal '((1) 0) (m:evaluate-string vm "
      (list (loop for *native-collect-variable* in '(1 2)
                  collect (if (= *native-collect-variable* 2) (loop-finish) *native-collect-variable*))
            *native-collect-variable*)")))))

(test format-directives
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (form
             '((format nil "Hello ~A: ~S, ~D!~~" "λ🙂" "quoted" -42)
               (format nil "~:A ~:S" nil nil)
               (format nil "~A ~A ~A" '("hello" (#\λ :key) . "tail") :key '#:|Hi There|)
               (format nil "~10A|~10@A|~2,3,1,'*A" "λ🙂" "λ🙂" "hello")
               (format nil "~V,'0D|~vA|~#D" 6 123 nil "x" 7)
               (format nil "~5,'0D ~:D ~@D ~:@D" -42 123456789 42 -123456)
               (format nil "~B ~O ~X ~36R" -255 255 255 123456789012345678901234567890)
               (format nil "~5D|~5D" "hi" 3/2)
               (format nil "~16,12,'0,'_,2:R" 123456789)
               (format nil "~,2F|~10,3F|~6,1,2,'*F|~,2@F" 3/2 -1.25 123456 1.25)
               (format nil "~C ~:C ~@C" #\λ #\Space #\Newline)
               (format nil "~&a~&b~3%~2~~2|")
               (format nil "λ~5T🙂~2,4@T!")
               (format nil "~A ~*~A ~:*~A ~0@*~A" 10 20 30)
               (format nil "~D item~:P, ~D categor~:@P" 1 2)
               (format nil "~? ~A" "~A/~D" '("nested" 42) "tail")
               (format nil "~@? ~A" "~A/~D" "nested" 42 "tail")
               (format nil "~{~A~^, ~}" '(1 2 3))
               (format nil "~{~A~^, ~}" nil)
               (format nil "~2{~A~}~A" '(1 2 3) "tail")
               (format nil "~@{~D~^,~}" 1 2 3)
               (format nil "~:{~A:~D~^!~}" '(("x" 1) ("y" 2)))
               (format nil "~:{~A~:^, ~}" '((1) (2) (3)))
               (format nil "~:@{~A~:^, ~}" '(1) '(2) '(3))
               (format nil "~{~}" "~A~^, " '(1 2 3))
               (format nil "~{x~:}" nil)
               (format nil "~{[~{~A~^,~}]~^;~}" '((1 2) (3 4)))
               (format nil "~[zero~;one~;two~:;many~]" 2)
               (format nil "~[zero~;one~;two~:;many~]" 99)
               (format nil "~[zero~;one~]" -1)
               (format nil "~:[no~;yes~] ~:[no~;yes~]" nil 42)
               (format nil "~@[~A~]~A" nil "tail")
               (format nil "~@[~A~]~A" "yes" "tail")
               (format nil "~2[zero~;one~;~A~]" "two")
               (format nil "~(~A~)|~:(~A~)|~@(~A~)|~:@(~A~)"
                       "hELLO wORLD" "hELLO wORLD" "hELLO wORLD" "hELLO wORLD")
               (format nil "~A~0^ignored" "stop")
               (format nil "~A~1,2^ continued~2,2^ignored" "start")
               (format nil (lambda (s a b) (format s "~A/~A" a b)) 1 2)
               (format nil (lambda (s) (format s (lambda (out) (format out "~A" (eq s out))))))))
      (is (equal (eval form) (m:evaluate vm form)) "FORMAT mismatch for ~S" form))
    ;; The guest rounds halfway decimal results away from zero; hosts differ.
    (is (equal "2.|-0.00|1.13|1.5" (m:evaluate vm
      '(format nil "~,0F|~,2F|~,2F|~F" 3/2 -0.0d0 1.125d0 3/2))))))

(test loop-sequential-assignment-termination
  (m:with-interpreter (vm)
    (dolist (form
      '((loop for i below 2 for x = (aref "ab" i) collect x)
        (loop for i below 0 for x = (error "exhausted") collect x)
        (loop for i below 0 for x = (error "exhausted") then 1 collect x)
        (loop for i below 2 for x = #\x then (aref "ab" i) collect x)
        (let ((seen nil))
          (loop for i below 2 for x = (progn (push i seen) i) collect x)
          (reverse seen))
        (loop for tail on '(1 2) for x = (car tail) then (car tail) collect x)
        (loop for c across "ab" for x = c collect x)
        (loop repeat 0 for x = (error "exhausted") collect x)
        (loop for i below 2 for (a b) = (list i (1+ i)) collect (list a b))
        (loop for i below 2 and x = 10 then i collect (list i x))
        (loop for i below 0 initially (values) finally (return i))
        (loop for i below 0 for x = 1 initially (values) finally (return x))))
      (is (equal (eval form) (m:evaluate vm form)) "Loop driver mismatch for ~S" form))))

(test loop-clauses
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (form
             '((loop (return 42))
               (loop for i from 1 to 10 sum i)
               (loop for i from 1 below 10 by 2 collect i)
               (loop for i downfrom 10 downto 1 by 3 collect i)
               (loop for i downfrom 10 above 1 by 3 collect i)
               (loop for i to 3 collect i)
               (loop for i from 123456789012345678901234567890 repeat 3 collect i)
               (loop for x in '(1 2 3) collect (* x x))
               (loop for tail on '(1 2 3) collect tail)
               (loop for tail on '(1 . 2) collect tail)
               (loop for tail on '(1 2 3) finally (return tail))
               (loop for x in '(1 2 3) finally (return x))
               (loop for x in '(1 2 3 4) by (lambda (xs) (cdr (cdr xs))) collect x)
               (loop for (a . b) in '((1 . 2) (3 . 4)) collect (+ a b))
               (loop for (a (b c)) in '((1 (2 3)) (4 (5 6))) collect (list a b c))
               (loop for c across "λ🙂" collect c)
               (loop for x = 1 then (* x 2) repeat 5 collect x)
               (loop for (a b) = '(1 2) then (list b (+ a b)) repeat 5 collect a)
               (loop for x from 1 for y = (* x x) collect y while (< x 4))
               (loop for x from 1 and y = 10 then x repeat 4 collect (list x y))
               (let ((x 10)) (loop for x from 1 and y = x repeat 2 collect (list x y)))
               (let ((x 10)) (loop with x = 1 and y = x repeat 1 collect (list x y)))
               (loop with x = 10 repeat 3 do (setq x (1+ x)) finally (return x))
               (loop with x integer repeat 1 collect x)
               (loop with x double-float repeat 1 collect x)
               (loop for x in '(nil t nil t t) count x)
               (loop for x in '(nil t nil t t) count x fixnum)
               (loop for x from 1 to 4 sum x fixnum)
               (loop for x in '(5 -1 3 9) minimize x)
               (loop for x in '(5 -1 3 9) maximize x)
               (loop repeat 0 minimize 42)
               (loop repeat 0 minimize 42 into x finally (return x))
               (loop for x in '((1 2) nil (3 4)) append x)
               (loop for x in '(1 2 3) nconc (list x x))
               (loop for x below 5 collect x into xs sum x into total
                     finally (return (list xs total)))
               (loop for x below 5 when (> x 2) collect x else sum x into small
                     end finally (return small))
               (loop for x below 5 if (> x 2) collect x and collect (* x 2)
                     else collect 0 end)
               (loop for x in '(nil 1 nil 2) when x collect it)
               (loop for x below 5 unless (< x 2) collect x)
               (loop for x below 5 always (< x 10))
               (loop for x below 5 always (< x 3))
               (loop for x below 5 never (> x 10))
               (loop for x in '(nil nil 42 99) thereis x)
               (loop for x below 5 until (= x 3) collect x)
               (loop for x below 5 while (< x 3) collect x)
               (loop for x below 5 when (= x 3) return (values x (* x 2)))
               (let ((state nil))
                 (list (loop initially (setq state 1) repeat 0
                             finally (setq state 2)) state))
               (loop for x below 5 collect x when (= x 2) do (loop-finish)
                     finally (return x))
               (loop named outer for x below 3
                     collect (loop for y below 2 collect (list x y)))))
      (is (equal (multiple-value-list (eval form))
                 (multiple-value-list (m:evaluate vm form))) "LOOP mismatch for ~S" form))
    (is (= 42 (m:evaluate-string vm "(loop named outer do (return-from outer 42))")))
    (is (= 123 (m:evaluate-string vm "(let ((*gensym-counter* 123)) (loop repeat 1 collect 42) *gensym-counter*)")))
    (is (equal '(1 2) (m:evaluate-string vm "(loop :for x :from 1 :to 2 :collect x)")))
    (dolist (source '("(loop for)" "(loop for x around 3)" "(loop for x in)"
                      "(loop repeat)" "(loop collect)" "(loop repeat 1 with x with x)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))
    (is (= 3 (m:evaluate vm '(+ 1 2))))))

(test format-output-and-recovery
  (m:with-interpreter (vm)
    (let ((output (make-string-output-stream)) (result nil) (failed nil))
      (let ((*standard-output* output))
        (setq result (m:evaluate vm '(format t "~{~A~^, ~}~%" '(1 2 3))))
        (m:evaluate vm '(write-string "λ🙂"))
        (handler-case (m:evaluate vm '(progn (format t " before") (error "bad")))
          (m:interpreter-error () (setq failed t))))
      (is (null result))
      (is (eq t failed))
      (is (equal (format nil "1, 2, 3~%λ🙂 before") (get-output-stream-string output))))
    (dolist (form '((format nil "~") (format nil "~A") (format nil "~{~A" '(1))
                    (format nil "~[a~;b" 1) (format nil "~Q" 1)
                    (format nil "~Vd" "bad" 1)))
      (is (= 7 (m:evaluate vm `(handler-case ,form (error () 7)))))
      (is (equal "ok" (m:evaluate vm '(format nil "~A" "ok")))))))

(test format-default-float-rounding
  (m:with-interpreter (vm)
    ;; These binary floats fall exactly halfway between decimal choices at
    ;; the printer's default precision. FORMAT must agree with that printer.
    (dolist (number '(805410.8125f0 297340.5625f0 761223.0625f0 66390.78125f0
                      1275649.125f0 12345.125f0 1038926653435922.25d0
                      768594681492874.625d0))
      (dolist (sign '(1 -1))
        (is (eq t (m:evaluate vm `(let* ((x (* ,number ,sign))
                         (*read-default-float-format* (type-of x))
                         (printed (prin1-to-string x))
                         (marker (position #\e printed :test #'char-equal)))
            (if marker
                (string= (format nil "~e" x)
                  (concatenate 'string (subseq printed 0 (1+ marker))
                    (if (char= (char printed (1+ marker)) #\-) "" "+")
                    (subseq printed (1+ marker))))
                (string= (format nil "~f" x) printed))))))))))
