;;;; Native trimming and integer parsing, compared with the host.
(in-package #:miricl-test)
(in-suite :miricl)

(test native-string-trimming
  (m:with-interpreter (vm)
    (dolist (operator '(string-trim string-left-trim string-right-trim))
      (dolist (string '("" "a" "abba" "abcaba" "Aa" "λ🙂x🙂λ"))
        (dolist (bag '("" "ab" "λ🙂" (#\a #\b) #(#\a #\b)))
          (let ((form `(multiple-value-list (,operator ',bag ,string))))
            (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))
      (dolist (string '(nil :abba #\a))
        (let ((form `(,operator "ab" ',string)))
          (is (equal (eval form) (m:evaluate vm form)))))
      (dolist (form
        `((let* ((backing (copy-seq "Xabλ🙂abY"))
                 (s (make-array 6 :element-type 'character :displaced-to backing :displaced-index-offset 1))
                 (bag (make-array 3 :initial-contents '(#\a #\b #\λ) :fill-pointer 2)))
            (list (,operator bag s) s backing))
          (let ((s (make-array 8 :element-type 'character :initial-contents "abxxbaZZ" :fill-pointer 6)))
            (list (,operator "ab" s) s))
          (let ((s (copy-seq "abba")))
            (let ((result (,operator "ab" s))) (list (stringp result) result s)))))
        (is (equalp (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
      (is (equal "" (m:evaluate vm `(,operator nil (make-array 0 :element-type nil)))))
      (dolist (arguments '(() ("a") ("a" "b" "c")))
        (is (eq t (m:evaluate vm `(handler-case (,operator ,@arguments) (program-error () t)))))))))

(test native-integer-parsing
  (m:with-interpreter (vm)
    (loop for radix from 2 to 36 do
      (dolist (integer (list 0 1 -1 (1- radix) radix (1+ radix) 1073741823 1073741824
                            (1- (ash 1 100)) (- (1+ (ash 1 200)))))
        (let ((string (format nil "~(~VR~)" radix integer)))
          (dolist (source (list string (concatenate 'string "  " string "  ")))
            (let ((form `(multiple-value-list (parse-integer ,source :radix ,radix))))
              (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))))
    (dolist (string '("" " " "-" "+" "  -" "  +x" "1 2" "10." "--10" "#x10" "12x" "-0"))
      (loop for start from 0 to (length string) do
        (loop for end from start to (length string) do
          (let ((form `(multiple-value-list (parse-integer ,string :start ,start :end ,end :junk-allowed t))))
            (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (dolist (form
      '((multiple-value-list (parse-integer "a1234b" :start 2 :start 1 :end 4 :end nil))
        (multiple-value-list (parse-integer "10" :radix 2 :radix 10))
        (multiple-value-list (parse-integer "12x" :junk-allowed t :junk-allowed nil))
        (multiple-value-list (parse-integer "17" :extra nil :allow-other-keys t :allow-other-keys nil))
        (let* ((s (make-array 7 :element-type 'character :initial-contents "x12345y"))
               (d (make-array 5 :element-type 'character :displaced-to s :displaced-index-offset 1))
               (f (make-array 4 :element-type 'character :displaced-to d :displaced-index-offset 1 :fill-pointer 2)))
          (multiple-value-list (parse-integer f)))
        (let ((*read-base* 2) (*read-suppress* t) (*readtable* (copy-readtable nil)))
          (setf (readtable-case *readtable*) :downcase)
          (multiple-value-list (parse-integer "12")))))
      (is (equal (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))
    (is (equal '(nil 0) (m:evaluate vm
      '(multiple-value-list (parse-integer (make-array 0 :element-type nil) :junk-allowed t)))))))

(test native-integer-parsing-errors
  (m:with-interpreter (vm)
    (dolist (string '("" " " "-" "+" "--10" "1 2" "10." "#o12" "#x10" "#2r10" "12x"))
      (is (eq t (m:evaluate vm `(handler-case (parse-integer ,string) (parse-error () t))))))
    (dolist (arguments '(() ("1" :bogus) ("1" :bogus nil) ("1" :unknown t :allow-other-keys nil :allow-other-keys t)))
      (is (eq t (m:evaluate vm `(handler-case (parse-integer ,@arguments) (program-error () t))))))
    (dolist (arguments '((17) (nil) (a) (#\1) ("123" :start nil) ("123" :start -1)
                        ("123" :start 2 :end 1) ("123" :end 4) ("123" :end 100000000000000000000)
                        ("123" :radix nil) ("123" :radix -10) ("123" :radix 1) ("123" :radix 37)
                        ("123" :radix 100000000000000000000)))
      (is (eq t (m:evaluate vm `(handler-case (parse-integer ,@(mapcar (lambda (x) `',x) arguments))
        (type-error (c) (not (typep (type-error-datum c) (type-error-expected-type c)))))))))))
