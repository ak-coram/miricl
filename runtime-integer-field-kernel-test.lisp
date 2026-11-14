;;;; Portable extraction and checked public byte-field boundaries.
(in-package #:miricl-test)
(in-suite :miricl)

(test runtime-kernel-integer-field-extraction
  (let ((integers (append '(0 1 -1 2 -2 #x7fffffff #x80000000 #xffffffffffffffff)
                          (loop for bits in '(15 30 63 64 100 201)
                                append (list (ash 1 bits) (1- (ash 1 bits)) (- (ash 1 bits)))))))
    (dolist (integer integers)
      (dolist (size '(0 1 14 15 16 30 31 63 64 65 100 256))
        (dolist (position '(0 1 14 15 16 30 31 63 64 65 100 256))
          (let ((spec (byte size position)))
            (is (= (ldb spec integer)
                   (miricl-runtime:extract-integer-field integer size position nil)))
            (is (eql (not (null (ldb-test spec integer)))
                     (miricl-runtime:extract-integer-field integer size position t))))))))
  (let ((huge (ash 1 100)))
    (is (= 0 (miricl-runtime:extract-integer-field -1 0 huge nil)))
    (is (= 1 (miricl-runtime:extract-integer-field -1 1 huge nil)))
    (is (= 0 (miricl-runtime:extract-integer-field 17 huge huge nil)))
    (is (miricl-runtime:extract-integer-field -1 huge huge t))
    (is (miricl-runtime:extract-integer-field -1 huge 0 t))
    (is (not (miricl-runtime:extract-integer-field 17 huge huge t)))
    (signals storage-condition (miricl-runtime:extract-integer-field -1 huge 0 nil)))
  (dolist (arguments '((nil 0 0 nil) (42 -1 0 nil) (42 nil 0 nil) (42 1 -1 nil)
                       (42 1 nil nil) (42 0 1.0 t)))
    (signals type-error (apply #'miricl-runtime:extract-integer-field arguments)))
  (is (= 3 (miricl-runtime:extract-integer-field 255 2 4 nil))))

(test runtime-kernel-integer-field-public-boundaries
  (let ((miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
    (m:with-interpreter (vm)
      ;; Host BYTE implementations may impose their own count limit. These
      ;; target cases have exact support/sign-extension results independently.
      (dolist (fixture
                '(((ldb (byte 0 (ash 1 100)) -1) 0)
                  ((ldb (byte 1 (ash 1 100)) -1) 1)
                  ((ldb (byte (ash 1 100) (ash 1 100)) 17) 0)
                  ((ldb-test (byte (ash 1 100) (ash 1 100)) -1) t)
                  ((ldb-test (byte (ash 1 100) 0) -1) t)
                  ((ldb-test (byte (ash 1 100) (ash 1 100)) 17) nil)
                  ((mask-field (byte (ash 1 100) (ash 1 100)) 17) 0)
                  ((multiple-value-list (ldb (byte 8 0) (values 511 :extra))) (255))
                  ((multiple-value-list (ldb-test (byte 8 0) (values 511 :extra))) (t))))
        (is (equal (second fixture) (m:evaluate vm (first fixture)))))
      (is (eq t (m:evaluate-string vm
        "(handler-case (ldb (byte (ash 1 100) 0) -1) (storage-condition () t))")))
      (dolist (source '("(ldb-test (byte 0 0) nil)" "(ldb (byte 1 0) 1.0)"
                        "(ldb '(1 . -1) 42)" "(ldb-test '(nil . 0) 42)"))
        (is (eq t (m:evaluate-string vm
                    (concatenate 'string "(handler-case " source " (type-error () t))")))))
      (is (= 15 (m:evaluate-string vm "(ldb (byte 4 4) 255)"))))))
