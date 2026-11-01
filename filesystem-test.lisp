;;;; Streams preserve identity across the Common Lisp host transport boundary.
(in-package #:miricl-test)
(in-suite :miricl)

(test portable-filesystem-handles
  (m:with-interpreter (vm)
    (is (equal '(1 2 3)
      (m:evaluate vm '(multiple-value-list
        (with-open-file (s "host-file" :direction :output)
          (write-line "λ🙂" s) (values 1 2 3))))))
    (let ((stream (m:evaluate vm '(open "host-file"))))
      (is (m:guest-opaque-object-p stream))
      (is (eq stream (m:evaluate vm stream)))
      (is (eq t (m:evaluate vm `(typep ,stream 'file-stream))))
      (is (equal '("λ🙂" nil) (m:evaluate vm `(multiple-value-list (read-line ,stream)))))
      (is (eq t (m:evaluate vm `(close ,stream))))
      (is (null (m:evaluate vm `(open-stream-p ,stream)))))
    (let ((stream (m:evaluate vm
            '(make-concatenated-stream (make-string-input-stream "1") (make-string-input-stream "2")))))
      (is (m:guest-opaque-object-p stream))
      (is (eq stream (m:evaluate vm stream)))
      (is (= 12 (m:evaluate vm `(read ,stream))))
      (is (eq t (m:evaluate vm `(close ,stream)))))
    (let ((stream (m:evaluate vm
            '(make-echo-stream (make-string-input-stream "λ") (make-string-output-stream)))))
      (is (m:guest-opaque-object-p stream))
      (is (eq stream (m:evaluate vm stream)))
      (is (char= #\λ (m:evaluate vm `(read-char ,stream))))
      (is (string= "λ" (m:evaluate vm `(get-output-stream-string (echo-stream-output-stream ,stream)))))
      (is (eq t (m:evaluate vm `(close ,stream)))))))
