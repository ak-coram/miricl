;;;; Behavior of the character table algorithms and their public boundaries.
(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-character-ranges
  (let ((digits (map 'string #'code-char m::*character-digit-ranges*))
        (alpha (map 'string #'code-char m::*character-alpha-ranges*)))
    ;; Check every table boundary and its immediate neighbors, including gaps.
    (dolist (code (remove-duplicates
                  (append (loop for code below 128 collect code)
                          (loop for (start end delta) on m::*character-digit-ranges* by #'cdddr
                                append (list (max 0 (1- start)) start end (min #x10ffff (1+ end)))))))
      (let ((expected (digit-char-p (code-char code) 36)))
        (is (= (if expected expected -1)
               (miricl-runtime:character-digit-code-value code digits)))))
    (dolist (code (remove-duplicates
                  (append (loop for code below 128 collect code)
                          (loop for (start end) on m::*character-alpha-ranges* by #'cddr
                                append (list (max 0 (1- start)) start end (min #x10ffff (1+ end)))))))
      (is (eql (not (null (alpha-char-p (code-char code))))
               (miricl-runtime:character-alpha-code-p code alpha))))))

(test runtime-kernel-unicode-character-names
  (dolist (case '(("U0041" 65) ("u+0041" 65) ("U000041" 65)
                  ("U+1F642" 128578) ("u10ffff" 1114111)
                  ("U0000" 0) ("U110000" -1) ("U+FFFFFF" -1)
                  ("U041" -1) ("U+041" -1) ("U0000041" -1)
                  ("U+004G" -1) ("U-0041" -1) ("U+ 041" -1)
                  ("U+٠٠٤١" -1) ("" -1) ("U" -1) ("U+" -1)
                  ("Space" -1) ("Linefeed" -1)))
    (is (= (second case) (miricl-runtime:unicode-character-name-code (first case)))))
  (let ((view (make-array 5 :element-type 'character :displaced-to "xxU0041yy" :displaced-index-offset 2)))
    (is (= 65 (miricl-runtime:unicode-character-name-code view))))
  (m:with-interpreter (vm)
    (dolist (case '(("U0041" 65) ("u+0041" 65) ("U+1f642" 128578)
                    ("u10ffff" 1114111) ("U110000" nil) ("U+004G" nil)
                    ("U041" nil) ("U0000041" nil) ("U+٠٠٤١" nil)))
      (is (eql (second case)
               (m:evaluate vm `(let ((c (name-char ,(first case)))) (and c (char-code c)))))))
    ;; A malformed argument must signal a Lisp condition and allow a later call.
    (is (eq t (m:evaluate vm '(handler-case (name-char 42) (type-error () t)))))
    (is (eql #\A (m:evaluate vm '(name-char "U0041"))))))
