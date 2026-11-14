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

(test runtime-kernel-character-raw-values
  ;; These raw helpers return Wasm values, leaving the Lisp value packet to
  ;; their callers. Invoke the boundary directly so public calls cannot hide
  ;; accidental clearing by the new canonical folding callback.
  (let ((original (fdefinition 'miricl::sequence-construction-runtime-wat)))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::sequence-construction-runtime-wat)
                 (lambda ()
                   (miricl::runtime-wat-sequence
                    (funcall original)
                    (miricl::wat-fragment
                     (format nil
                      "(func (export ~S) (param $mode i32) (result i32)
                         (local $saved (ref null eq)) (local $text (ref null eq))
                         (local.set $text ~A)
                         (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
                         (global.set $value-count (i32.const 2))
                         (local.set $saved (global.get $extra-values))
                         (if (i32.eq (local.get $mode) (i32.const 0))
                           (then (drop (call $sequence-character-scan
                             (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $text))))
                             (i32.const 0) (i32.const 4) (i32.const 65) (i32.const 1)
                             (i32.const 0) (i32.const 0) (i32.const 1)))))
                         (if (i32.eq (local.get $mode) (i32.const 1))
                           (then (drop (call $string-comparison
                             (call $cons (local.get $text) (call $cons (local.get $text) (ref.null eq)))
                             (i32.const 6)))))
                         (if (i32.eq (local.get $mode) (i32.const 2))
                           (then (drop (call $bootstrap-string-equal (local.get $text) (local.get $text)))))
                         (i32.and (i32.eq (global.get $value-count) (i32.const 2))
                                  (ref.eq (local.get $saved) (global.get $extra-values))))"
                      "character-values-preserved"
                      (miricl::wat-string (miricl::wat-text-literal "AbAa")))))))
           (let* ((miricl::*interpreter-only-wasm* nil)
                  (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
             (m:with-interpreter (vm)
               (dolist (mode '(0 1 2))
                 (is (= 1 (wt:instance-call (miricl::interpreter-context vm)
                                            (miricl::interpreter-instance vm)
                                            "character-values-preserved" mode)))))))
      (setf (fdefinition 'miricl::sequence-construction-runtime-wat) original))))
