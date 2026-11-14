(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-canonical-character-regions
  ;; Every finite region of each input, both directions and both equivalences.
  (dolist (a '("" "a" "aba" "AbA" "λ🙂λ"))
    (dolist (b '("" "a" "baba" "aBaB" "xλ🙂λ"))
      (loop for first1 from 0 to (length a) do
        (loop for stop1 from first1 to (length a) do
          (loop for first2 from 0 to (length b) do
            (loop for stop2 from first2 to (length b) do
              (dolist (reverse '(nil t))
                (dolist (fold '(nil t))
                  (let ((function (if fold #'char-equal #'char=))
                        (mapper (if fold #'char-upcase nil)))
                    (is (eql (search a b :start1 first1 :end1 stop1 :start2 first2 :end2 stop2
                                     :from-end reverse :test function)
                             (miricl-runtime:search-character-regions a b
                               (vector first1 stop1 first2 stop2 reverse mapper))))
                    (dolist (negate '(nil t))
                      (is (eql (apply #'mismatch a b :start1 first1 :end1 stop1 :start2 first2 :end2 stop2
                                       :from-end reverse (list (if negate :test-not :test) function))
                               (miricl-runtime:mismatch-character-regions a b
                                 (vector first1 stop1 first2 stop2
                                         (+ (if negate 1 0) (if reverse 2 0)) mapper))))))))))))))
  (dolist (text '("" "a" "ababA" "λ🙂λ"))
    (loop for first from 0 to (length text) do
      (loop for stop from first to (length text) do
        (dolist (needle '(#\a #\λ 10))
          (dolist (reverse '(nil t))
            (dolist (negate '(nil t))
              (dolist (countp '(nil t))
                (let* ((expected (apply (if countp #'count #'position) needle text
                                        :start first :end stop :from-end reverse
                                        (list (if negate :test-not :test) #'eql)))
                       (actual (miricl-runtime:scan-character-region text
                                 (vector first stop (if (characterp needle) (char-code needle) -1)
                                         (+ (if negate 1 0) (if reverse 2 0) (if countp 4 0)) nil))))
                  (is (eql (if (null expected) -1 expected) actual))))))))))
  (is (equal '(0) (multiple-value-list
                    (miricl-runtime:search-character-regions "a" "a" (vector 0 1 0 1 nil nil))))))

(test runtime-kernel-character-relations
  (dolist (a '("" "a" "aba" "AbA" "λ🙂λ"))
    (dolist (b '("" "a" "baba" "aBaB" "xλ🙂λ"))
      (loop for first1 from 0 to (length a) do
        (loop for stop1 from first1 to (length a) do
          (loop for first2 from 0 to (length b) do
            (loop for stop2 from first2 to (length b) do
              (loop for functions in '((string= string/= string< string> string<= string>=)
                                      (string-equal string-not-equal string-lessp string-greaterp
                                                    string-not-greaterp string-not-lessp))
                    for fold in '(nil t) do
                (loop for function in functions for op from 0 do
                  (let ((expected (funcall function a b :start1 first1 :end1 stop1 :start2 first2 :end2 stop2))
                        (actual (miricl-runtime:compare-character-regions a b
                                  (vector first1 stop1 first2 stop2 op (if fold #'char-upcase nil)))))
                    (if (= op 0)
                        (is (eq (not (null expected)) (not (null actual))))
                        (is (eql expected actual))))))))))))
  (dolist (a '("" "abc" "λ🙂"))
    (dolist (b '("" "abc" "abcd" "λ🙂"))
      (is (eq (not (null (string= a b)))
              (not (null (miricl-runtime:bootstrap-text-equal-p a b))))))))
