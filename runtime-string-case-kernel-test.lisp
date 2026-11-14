;;;; Compare the portable search against the original generated range records.
(in-package #:miricl-test)
(in-suite :miricl)

(defun string-case-reference-table (ranges down ansi)
  (let ((expected (make-array #x110000 :element-type '(unsigned-byte 32))))
    (dotimes (code (length expected)) (setf (aref expected code) code))
    (loop for code from (if down 65 97) to (if down 90 122)
          do (setf (aref expected code) (+ code (if down 32 -32))))
    (loop for (start end delta flags) on ranges by #'cddddr
          unless (and ansi (logbitp 1 flags)) do
            (loop for code from start to end
                  when (zerop (logand (- code start) (logand flags 1)))
                    do (setf (aref expected code) (+ code delta))))
    expected))

(test runtime-kernel-character-case-exhaustive
  (dolist (down '(nil t))
    (let* ((ranges (if down m::*string-downcase-ranges* m::*string-case-ranges*))
           (packed (map 'string #'code-char (m::string-case-table-values ranges))))
      (dolist (ansi '(nil t))
        (let ((expected (string-case-reference-table ranges down ansi)) (mismatch nil))
          (loop for code below (length expected)
                unless (= (aref expected code)
                          (miricl-runtime:character-case-code-value code down packed ansi))
                  do (setq mismatch code) (return))
          (is (null mismatch) "Case mismatch at ~S, DOWN=~S ANSI=~S" mismatch down ansi)))
      (is (= -1 (miricl-runtime:character-case-code-value -1 down packed t))))))

(test runtime-kernel-character-case-table-boundary
  (dolist (bad '((128 130 1) (128 127 1 0) (128 130 1 4)
                 (128 130 800000 0) (128 130 :bad 0)
                 (128 130 1 0 129 132 1 0)))
    (signals error (m::string-case-table-values bad)))
  (let ((cycle (list 128 130 1 0)))
    (setf (cddddr cycle) cycle)
    (signals error (m::string-case-table-values cycle)))
  (is (equal '(128 130 2 0) (m::string-case-table-values '(128 130 1 0))))
  (is (equal '(128 130 1 0) (m::string-case-table-values '(128 130 -1 0)))))

(test runtime-kernel-character-case-public
  (m:with-interpreter (vm)
    ;; Derive expected target behavior from the bootstrap tables; the host's
    ;; Unicode repertoire and nonreciprocal mappings are not fixed inventories.
    (dolist (down '(nil t))
      (let* ((ranges (if down m::*string-downcase-ranges* m::*string-case-ranges*))
             (codes (remove-duplicates
                     (append '(0 9 32 64 65 90 91 96 97 122 123 127 #x10ffff)
                             (loop for (start end delta flags) on ranges by #'cddddr
                                   append (list (1- start) start (min end (1+ start)) end
                                                (min #x10ffff (1+ end))))))))
        (dolist (ansi '(nil t))
          (let* ((expected (string-case-reference-table ranges down ansi))
                 (operation (if ansi (if down 'char-downcase 'char-upcase)
                                (if down 'm::unicode-char-downcase 'm::unicode-char-upcase)))
                 (wanted (mapcar (lambda (code) (aref expected code)) codes)))
            (is (equal wanted
                       (m:evaluate vm `(mapcar (lambda (code) (char-code (,operation (code-char code)))) ',codes))))))))
    (is (eq t (m:evaluate-string vm
      "(and (handler-case (char-upcase 42) (type-error () t))
            (handler-case (char-downcase :bad) (type-error () t))
            (eql #\\A (char-upcase #\\a))
            (eql #\\z (char-downcase #\\Z))
            (= 1 (length (multiple-value-list (char-upcase #\\a)))))")))))
