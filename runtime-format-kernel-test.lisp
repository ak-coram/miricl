(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-format-algorithms
  (dolist (text '("" "/" "abc/def" "λ🙂/tail" "no slash"))
    (loop for start from 0 to (length text) do
      (loop for end from start to (length text) do
        (is (= (or (position #\/ text :start start :end end) -1)
               (miricl-runtime:format-slash-position text start end)))))
    (is (eq (not (null (find #\~ text)))
            (not (miricl-runtime:format-constant-text-p text)))))
  (dolist (text '("~" "x~y" "~~" "λ~🙂"))
    (is (not (miricl-runtime:format-constant-text-p text))))
  (dolist (case '(("0.25" 3 ".25") ("-0.25" 4 "-.25") ("+0.25" 4 "+.25")
                  ("0." 1 ".") ("-0." 2 "-.") ("0.25" 0 ".25")
                  ("0." 0 "0.") ("0.25" 5 "0.25") ("1.25" 3 "1.25")
                  ("" 0 "") ("+" 0 "+") ("0" 0 "0")))
    (destructuring-bind (text width expected) case
      (let* ((before (copy-seq text))
             (result (miricl-runtime:format-leading-zero-text text width t)))
        (is (string= expected result))
        (is (string= before text))
        (if (string= text expected) (is (eq text result))
            (is (not (eq text result)))))
      (is (eq text (miricl-runtime:format-leading-zero-text text width nil)))))
  (dolist (number '(0 -0.0d0 1/8 -1/8 5/2 7/2 -5/2 1/3 0.1d0 123456789012345678901/2))
    (dolist (scale '(-20 -3 -1 0 1 3 20))
      (let ((scaled (* (abs (rational number)) (expt 10 scale))))
        (is (= (round scaled) (miricl-runtime:format-rounded-integer number scale t)))
        (is (= (floor (+ scaled 1/2)) (miricl-runtime:format-rounded-integer number scale nil))))))
  (dolist (case '(("0.0" 0 1) ("-0.0" 0 1) ("1.0" 0 1) ("123.45" 2 5)
                  ("0.0012300" -3 3) ("-1.25d-10" -10 3) ("1.25e10" 10 3)))
    (destructuring-bind (text exponent precision) case
      (is (equalp (vector exponent precision) (miricl-runtime:format-decimal-parts text)))))
  (is (equal '(4) (multiple-value-list (miricl-runtime:format-rounded-integer 7/2 0 t)))))

(test runtime-kernel-format-public-regressions
  (m:with-interpreter (vm)
    (dolist (form '((equal (format nil "~,2F" 1/8) "0.13")
                    (equal (format nil "~,2F" -1/8) "-0.13")
                    (equal (format nil "~,2,-3F" 125) "0.13")
                    (equal (format nil "~,0F" 123456789012345678901/2) "61728394506172839451.")
                    (equal (format nil "~3,2F" 1/8) ".13")
                    (equal (format nil "~4,2F" -1/8) "-.13")
                    (equal (format nil "~,2F" -0.0d0) "-0.00")
                    (equal (format nil "~F" 0.0d0) "0.0")
                    (equal (format nil "~F" 1.25d0) "1.25")
                    (equal (format nil "~,2E" 12.5d0) "1.25d+1")
                    (handler-case (progn (format nil "~/missing" 1) nil) (error () t))
                    (handler-case (progn (format nil "~,,,2001E" 1) nil) (error () t))
                    (equal (format nil "~A" :recovered) "RECOVERED")))
      (is (eq t (m:evaluate-string vm (prin1-to-string form)))))))

(test runtime-kernel-format-raw-values
  (let ((original (fdefinition 'miricl::format-runtime-wat)))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::format-runtime-wat)
                 (lambda ()
                   (miricl::runtime-wat-sequence
                    (funcall original)
                    (miricl::wat-fragment
                     (format nil
                      "(func (export ~S) (param $mode i32) (result i32)
                         (local $saved (ref null eq)) (local $text (ref null eq)) (local $number (ref eq))
                         (local.set $text ~A)
                         (local.set $number (call $ratio (call $int (i32.const 1)) (call $int (i32.const 8))))
                         (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
                         (global.set $value-count (i32.const 2))
                         (local.set $saved (global.get $extra-values))
                         (if (i32.eq (local.get $mode) (i32.const 0))
                           (then (drop (call $format-slash-end
                             (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) ~A)))
                             (i32.const 0) (i32.const 4)))))
                         (if (i32.eq (local.get $mode) (i32.const 1))
                           (then (drop (call $format-leading-zero (local.get $text) (i32.const 3) (i32.const 1)))))
                         (if (i32.eq (local.get $mode) (i32.const 2))
                           (then (drop (call $format-rounded (local.get $number) (i32.const 2) (i32.const 0)))))
                         (if (i32.eq (local.get $mode) (i32.const 3))
                           (then (call $format-real-parts (call $float (i32.const 4) (f64.const -0.0))) (drop) (drop) (drop)))
                         (if (i32.eq (local.get $mode) (i32.const 4))
                           (then (call $format-constant (local.get $text))))
                         (if (i32.eq (local.get $mode) (i32.const 5))
                           (then (call $format-parse
                             (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) ~A)))
                             (i32.const 0) (i32.const 7) (ref.null eq) (i32.const 0))
                             (drop) (drop) (drop) (drop) (drop) (drop) (drop)))
                         (if (i32.eq (local.get $mode) (i32.const 6))
                           (then (call $format-close
                             (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) ~A)))
                             (i32.const 0) (i32.const 9) (i32.const 93) (i32.const 0))
                             (drop) (drop) (drop) (drop)))
                         (i32.and (i32.eq (global.get $value-count) (i32.const 2))
                                  (ref.eq (local.get $saved) (global.get $extra-values))))"
                      "format-values-preserved"
                      (miricl::wat-string (miricl::wat-text-literal "0.13"))
                      (miricl::wat-string (miricl::wat-text-literal "abc/"))
                      (miricl::wat-string (miricl::wat-text-literal "3,'λ:@A"))
                      (miricl::wat-string (miricl::wat-text-literal "~[~A~]~:]")))))))
           (let* ((miricl::*interpreter-only-wasm* nil)
                  (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
             (m:with-interpreter (vm)
               (dolist (mode '(0 1 2 3 4 5 6))
                 (is (= 1 (wt:instance-call (miricl::interpreter-context vm)
                                            (miricl::interpreter-instance vm)
                                            "format-values-preserved" mode)))))))
      (setf (fdefinition 'miricl::format-runtime-wat) original))))

(test runtime-kernel-format-parser
  (labels ((parse (text &optional args scan)
             (miricl-runtime:format-control-values text args (vector 0 (length text) scan 0 nil)))
           (find-close (text code &optional separator)
             (miricl-runtime:format-control-values text nil (vector 0 (length text) t code separator))))
    (dolist (case '(("a" 65 0 0 nil) ("3,-4:@s" 83 1 1 (3 -4))
                    (",+4s" 83 0 0 (nil 4)) ("'λa" 65 0 0 (#\λ))
                    ("'~a" 65 0 0 (#\~)) ("#A" 65 0 0 (0))
                    ("1,2,3,4,5,6,7,8,9,10A" 65 0 0 (1 2 3 4 5 6 7 8 9 10))
                    ("123456789012345678901A" 65 0 0 (123456789012345678901))
                    ("-123456789012345678901A" 65 0 0 (-123456789012345678901))))
      (destructuring-bind (text code colon at parameters) case
        (let ((r (parse text)))
          (is (= (length text) (svref r 0)))
          (is (= code (svref r 1)))
          (is (= colon (svref r 2))) (is (= at (svref r 3)))
          (is (= (length parameters) (svref r 6)))
          (is (equalp (coerce parameters 'vector) (subseq (svref r 4) 0 (svref r 6))))
          (is (every #'null (subseq (svref r 4) (svref r 6))))
          (is (null (svref r 5))))))
    (let* ((arguments (list 17 :left :right))
           (r (parse "v,#,'🙂,-12345678901234567890:@s" arguments)))
      (is (eq (cdr arguments) (svref r 5)))
      (is (equalp (vector 17 2 #\🙂 -12345678901234567890)
                  (subseq (svref r 4) 0 (svref r 6))))
      (is (equal '(1 1) (list (svref r 2) (svref r 3)))))
    (let* ((arguments (list 17 :unused)) (r (parse "vA" arguments t)))
      (is (eq arguments (svref r 5)))
      (is (null (svref (svref r 4) 0))))
    (is (eql 0 (parse "vA")))
    (is (null (parse "v" '(17))))
    (dolist (text '("" "+" "-x" "'" "1" "::A" "@@A" ":@:@A"))
      (is (null (parse text))))
    (let ((cycle (list :cyclic)))
      (setf (cdr cycle) cycle)
      (is (null (parse "#A" cycle))))
    (is (null (parse "vA" :dotted)))
    (dolist (text '("body ~A ~{~A~} ~:]" "~{~[x~;y~]~}~]" "~/FOO~]BAR/~]"
                    "~'~A~]" "~vA~]" "~#A~]"))
      (let* ((mark (search "~]" text :from-end t))
             (mark (if mark mark (search "~:]" text :from-end t)))
             (r (find-close text 93)))
        (is (= mark (svref r 0)))
        (is (= (length text) (svref r 1)))
        (is (= 93 (svref r 2)))
        (is (= (if (search "~:]" text :start2 mark) 1 0) (svref r 3)))))
    (is (equalp #(1 4 59 1) (find-close "x~:;tail~]" 93 t)))
    (dolist (text '("text" "~~]" "~}" "~{~]" "~/missing" "~'" "~::]"))
      (is (null (find-close text 93))))
    (is (= 1 (length (multiple-value-list (parse "A")))))))

(test runtime-kernel-format-parser-boundaries
  (let ((original (fdefinition 'miricl::format-runtime-wat))
        (cases '(("A" -1 1 nil nil :malformed-form)
                 ("A" 0 2 nil nil :malformed-form) ("A" 1 0 nil nil :malformed-form)
                 ("A" -1 1 93 nil :malformed-form) ("A" 0 2 93 nil :malformed-form)
                 ("vA" 0 2 nil nil :wrong-arity) ("::A" 0 3 nil nil :malformed-form)
                 ("plain" 0 5 93 nil :malformed-form)
                 ("#A" 0 2 nil :cycle :malformed-form) ("vA" 0 2 nil :dotted :malformed-form))))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::format-runtime-wat)
                 (lambda ()
                   (miricl::runtime-wat-sequence
                    (funcall original)
                    (miricl::wat-fragment
                     (format nil
                      "(func (export ~S) (param $mode i32) (result i32)
                         (local $saved (ref null eq)) (local $args (ref null eq)) (local $ok i32) (local $expected i32)
                         (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
                         (global.set $value-count (i32.const 2))
                         (local.set $saved (global.get $extra-values))
                         (block $rejected (try_table (catch $transfer $rejected)
                           ~{~A~}
                           (return (i32.const 0))))
                         (local.set $ok
                           (i32.and (i32.and (i32.eq (global.get $value-count) (i32.const 2))
                                            (ref.eq (local.get $saved) (global.get $extra-values)))
                             (i32.eq (global.get $error) (local.get $expected))))
                         (global.set $error (i32.const 0))
                         (global.set $last-condition (ref.null eq))
                         (global.set $transfer-target (ref.null eq))
                         (global.set $transfer-values (ref.null eq))
                         (local.get $ok))"
                      "format-parser-boundary-rejection"
                      (loop for (text first end close argument-kind error-code) in cases for mode from 0 collect
                        (format nil
                         "(if (i32.eq (local.get $mode) (i32.const ~D))
                            (then (local.set $expected (i32.const ~D)) ~A
                              (call ~A ~A (i32.const ~D) (i32.const ~D) ~A ~A) ~A))"
                         mode (miricl::runtime-error-code error-code)
                         (case argument-kind
                           (:cycle "(local.set $args (call $cons (ref.null eq) (ref.null eq))) (struct.set $obj $b (ref.cast (ref $obj) (local.get $args)) (local.get $args))")
                           (:dotted "(local.set $args (call $int (i32.const 42)))")
                           (otherwise ""))
                         (if close "$format-close" "$format-parse")
                         (miricl::wat-string `(ref.as_non_null (struct.get miricl::$obj miricl::$digits
                                               (ref.cast (ref miricl::$obj) ,(miricl::wat-text-literal text)))))
                         first end (if close (format nil "(i32.const ~D)" close) "(local.get $args)")
                         "(i32.const 0)"
                         (format nil "~{~A~}" (make-list (if close 4 7) :initial-element "(drop)")))))))))
           (let* ((miricl::*interpreter-only-wasm* nil)
                  (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
             (m:with-interpreter (vm)
               (loop for mode below (length cases) do
                 (is (= 1 (wt:instance-call (miricl::interpreter-context vm)
                                            (miricl::interpreter-instance vm)
                                            "format-parser-boundary-rejection" mode))
                     "Raw FORMAT boundary case ~D" mode)
                 (is (equal "RECOVERED" (m:evaluate vm '(format nil "~A" :recovered))))))))
      (setf (fdefinition 'miricl::format-runtime-wat) original))))
