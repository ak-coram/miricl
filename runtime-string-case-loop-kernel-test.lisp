(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-string-case-regions
  (dolist (text '("" "abcDEF" " hello, WORLD 13a!" "aBc-xyZ'Q" "λΛαΑ"))
    (loop for first from 0 to (length text) do
      (loop for stop from first to (length text) do
        (loop for mode from 0 below 3
              for operator in '(string-upcase string-downcase string-capitalize) do
          (dolist (write '(nil t))
            (let* ((input (copy-seq text))
                   (flags (+ (if write 1 0) (case mode (1 2) (2 4) (otherwise 0))))
                   (expected (funcall operator text :start first :end stop))
                   (actual (miricl-runtime:case-string-region-value input first stop
                             (vector flags #'miricl::ansi-character-upcase
                                     #'miricl::ansi-character-downcase #'alphanumericp #'subseq))))
              (is (string= expected actual))
              (is (string= (if write expected text) input))
              (if write (is (eq actual input)))))))))
  (let* ((backing (copy-seq "_aB-cD_"))
         (input (make-array 5 :element-type 'character :displaced-to backing
                             :displaced-index-offset 1 :fill-pointer 4)))
    (is (eq input (miricl-runtime:case-string-region-value input 0 4
                   (vector 1 #'char-upcase #'char-downcase #'alphanumericp #'subseq)))))
  (let ((input (copy-seq "ALREADY UPPER")))
    (is (eq input (miricl-runtime:case-string-region-value input 0 (length input)
                   (vector 0 #'char-upcase #'char-downcase #'alphanumericp #'subseq)))))
  (is (equal '("Abc")
             (multiple-value-list (miricl-runtime:case-string-region-value "aBC" 0 3
                                    (vector 4 #'char-upcase #'char-downcase #'alphanumericp #'subseq))))))

(test runtime-kernel-string-case-sharing-and-recovery
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      (dolist (source
                '("(let* ((source (copy-seq \"abc\")) (result (string-upcase source)))
                     (and (equal result \"ABC\") (equal source \"abc\") (not (eq result source))))"
                  "(let ((source (copy-seq \"ABC\"))) (eq source (string-upcase source)))"
                  "(let* ((backing (copy-seq \"_aB-cD_\"))
                          (source (make-array 5 :element-type 'character :displaced-to backing
                                               :displaced-index-offset 1 :fill-pointer 4)))
                     (and (eq source (nstring-downcase source)) (equal backing \"_ab-cD_\")))"
                  "(equal (string-capitalize \"xABC-DEF\" :start 2 :end 7) \"xABc-DeF\")"
                  "(and (equal (string-upcase 'abc) \"ABC\")
                        (equal (string-downcase #\\A) \"a\")
                        (equal (multiple-value-list (nstring-capitalize (copy-seq \"aBC\"))) '(\"Abc\")))"
                  "(let ((micro (code-char 181)) (sigma (code-char 962)))
                     (and (= 181 (char-code (char (string-upcase (string micro)) 0)))
                          (= 962 (char-code (char (string-upcase (string sigma)) 0)))
                          (equal (string (miricl::unicode-char-upcase micro))
                                 (miricl::unicode-string-upcase (string micro)))
                          (equal (string (miricl::unicode-char-upcase sigma))
                                 (miricl::unicode-string-upcase (string sigma)))))"
                  "(let ((old-up (fdefinition 'char-upcase)) (old-alpha (fdefinition 'alphanumericp))
                         (old-subseq (fdefinition 'subseq)))
                     (unwind-protect
                       (progn (setf (fdefinition 'char-upcase) (lambda (x) (declare (ignore x)) #\\!))
                              (setf (fdefinition 'alphanumericp) (lambda (x) (declare (ignore x)) nil))
                              (setf (fdefinition 'subseq) (lambda (&rest x) (declare (ignore x)) \"!\"))
                              (equal (string-capitalize \"aBC-dEF\") \"Abc-Def\"))
                       (setf (fdefinition 'char-upcase) old-up)
                       (setf (fdefinition 'alphanumericp) old-alpha)
                       (setf (fdefinition 'subseq) old-subseq)))"))
        (is (eq t (m:evaluate-string vm source))))
      (dolist (source '("(string-upcase \"abc\" :start -1)"
                        "(string-downcase \"abc\" :end 4)"
                        "(string-capitalize \"abc\" :start 2 :end 1)"
                        "(nstring-upcase 'abc)" "(nstring-downcase #\\A)"
                        "(string-upcase #(1 2 3))"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case (progn " source " nil) (type-error () t))")))))
      (is (equal "Abc Def" (m:evaluate-string vm "(string-capitalize \"aBC dEF\")"))))))
