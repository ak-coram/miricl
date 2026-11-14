;;;; Reader fast paths are recognizers, not a replacement for the full reader.
(in-package #:miricl-test)
(in-suite :miricl)

(test parse-integer-region-kernel
  (dolist (radix '(2 3 8 10 16 23 36))
    (dolist (integer (list 0 1 -1 35 36 #x3fffffff #x40000000 (1- (ash 1 100)) (- (ash 1 200))))
      (let ((text (format nil "  ~VR  " radix integer)))
        (dolist (junk '(nil t))
          (multiple-value-bind (value position) (parse-integer text :radix radix :junk-allowed junk)
            (is (equal (cons value position)
                       (miricl-runtime:parse-integer-region-value text 0 (length text) (cons radix junk)))))))))
  (dolist (text '("" " " "-" "+" "  -" "  +x" "1 2" "10." "--10" "#x10" "12x" "-0"))
    (loop for first from 0 to (length text) do
      (loop for stop from first to (length text) do
        (multiple-value-bind (value position)
            (parse-integer text :start first :end stop :junk-allowed t)
          (is (equal (cons value position)
                     (miricl-runtime:parse-integer-region-value text first stop '(10 . t))))))))
  (dolist (arguments '((42 0 0 (10)) ("123" 0 3 nil) ("123" :bad 3 (10))
                       ("123" 0 :bad (10)) ("123" -1 3 (10)) ("123" 2 1 (10))
                       ("123" 0 4 (10)) ("123" 0 3 (:bad)) ("123" 0 3 (1))
                       ("123" 0 3 (37))))
    (signals type-error (apply #'miricl-runtime:parse-integer-region-value arguments)))
  (is (equal '(123 . 3) (miricl-runtime:parse-integer-region-value "123" 0 3 '(10)))))

(test reader-small-integer-kernel
  (dolist (radix '(2 3 8 10 16 23 36))
    (dolist (value '(0 1 -1 35 36 127 -128 2147483647 -2147483648 2147483648 -2147483649))
      (let* ((text (format nil "~VR" radix value))
             (expected (if (<= -2147483648 value 2147483647) value nil)))
        (is (eql expected (miricl-runtime:small-reader-integer-value text (length text) radix)))
        (is (eql expected (miricl-runtime:small-reader-integer-value
                           (string-downcase text) (length text) radix))))))
  (dolist (text '("" "+" "-" "." "+." "--1" " 1" "1 " "1/2" "1.0" "1e2" "12x" "١٢٣"))
    (is (null (miricl-runtime:small-reader-integer-value text (length text) 10))))
  (is (= 123 (miricl-runtime:small-reader-integer-value "+123." 5 10)))
  (is (= -123 (miricl-runtime:small-reader-integer-value "-123." 5 10)))
  (is (null (miricl-runtime:small-reader-integer-value "123." 4 16)))
  (is (= 12 (miricl-runtime:small-reader-integer-value "12invalid" 2 10)))
  (is (= 0 (miricl-runtime:small-reader-integer-value "-0" 2 10)))
  (let ((view (make-array 4 :element-type 'character :displaced-to "xx1234yy"
                          :displaced-index-offset 2)))
    (is (= 1234 (miricl-runtime:small-reader-integer-value view 4 10))))
  (dolist (arguments '((42 0 10) ("123" -1 10) ("123" 4 10) ("123" :bad 10)
                       ("123" 3 1) ("123" 3 37) ("123" 3 :bad)))
    (signals type-error (apply #'miricl-runtime:small-reader-integer-value arguments)))
  (is (= 123 (miricl-runtime:small-reader-integer-value "123" 3 10))))

(test reader-float-format-kernel
  (dolist (format '(short-float single-float))
    (is (= 3 (miricl-runtime:reader-float-format-code format))))
  (dolist (format '(double-float long-float))
    (is (= 4 (miricl-runtime:reader-float-format-code format))))
  (signals type-error (miricl-runtime:reader-float-format-code :bad))
  (is (= 3 (miricl-runtime:reader-float-format-code 'single-float))))

(test reader-kernel-public-entry-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (is (eq t (m:evaluate-string vm
        "(and (= 2147483647 (read-from-string \"2147483647\"))
              (= -2147483648 (read-from-string \"-2147483648\"))
              (= 2147483648 (read-from-string \"2147483648\"))
              (= -2147483649 (read-from-string \"-2147483649\"))
              (= 123 (read-from-string \"123.\"))
              (= 10 (let ((*read-base* 16)) (read-from-string \"a\")))
              (eq 'cl-user::|123| (read-from-string \"|123|\"))
              (typep (let ((*read-default-float-format* 'single-float)) (read-from-string \"1.5\")) 'single-float)
              (typep (let ((*read-default-float-format* 'double-float)) (read-from-string \"1.5\")) 'double-float)
              (handler-case (let ((*read-base* 1)) (read-from-string \"123\"))
                (type-error () t))
              (= 42 (read-from-string \"42\")))"))))))
