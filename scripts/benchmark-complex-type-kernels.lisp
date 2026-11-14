;;;; Equivalent target type model for warmed complex workers and SBCL drivers.
(in-package #:cl-user)
(defun complex-type-benchmark-integer-metadata (selector object argument)
  (case selector
    (0 (miricl-runtime:check-type-control-value object #'complex-type-benchmark-integer-metadata) object)
    (2 nil)
    (3 (list-length object))
    (4 (error 'simple-type-error :datum object :expected-type nil
              :format-control "Malformed type control (reason ~D)." :format-arguments (list argument)))))
(defun complex-type-benchmark-numeric (a b environment)
  (let ((result (miricl-runtime:integer-type-query-value a b (vector 1 nil environment)
                                                         #'complex-type-benchmark-integer-metadata)))
    (if (= result 0) (miricl-runtime:real-subtype-query-value a b #'real-type-benchmark-metadata) result)))
(defun complex-type-benchmark-metadata (selector object argument)
  (case selector
    (0 (miricl-runtime:check-type-control-value object #'complex-type-benchmark-metadata) object)
    (1 (if (member object '(complex number t rational integer float single-float double-float)) (cons object t)))
    (2 (miricl-runtime:check-type-control-value object #'complex-type-benchmark-metadata)
       (miricl-runtime:check-type-control-value (car argument) #'complex-type-benchmark-metadata)
       (complex-type-benchmark-numeric object (car argument) (cdr argument)))
    (3 (list-length object))
    (4 (complex-type-benchmark-integer-metadata selector object argument))
    (5 (if (and (consp object) (eq (car object) 'eql))
           (let ((value (cadr object)))
             (cond ((rationalp value) 'rational) ((typep value 'single-float) 'single-float)
                   ((typep value 'double-float) 'double-float) (t (error "Not a real component."))))
           (error "Unsupported benchmark general component ~S." object)))
    (6 (miricl-runtime:check-type-control-value object #'complex-type-benchmark-metadata)
       (miricl-runtime:check-type-control-value (car argument) #'complex-type-benchmark-metadata)
       (if (or (eq object (car argument)) (null object) (eq (car argument) 'real)) 1
           (complex-type-benchmark-numeric object (car argument) (cdr argument))))
    (7 (if (member object '(complex number t rational integer float cons symbol))
           (vector object '(complex number t) t)))))
(defparameter *complex-type-metadata* #'complex-type-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "complex-type/" (first case))) miricl-port-bench:*cases*)
    '(("complex-type/upgrade-integer" upgraded-complex-part-value (list 'integer nil *complex-type-metadata*) :calls 20000)
      ("complex-type/upgrade-float" upgraded-complex-part-value (list 'single-float nil *complex-type-metadata*) :calls 20000)
      ("complex-type/upgrade-empty" upgraded-complex-part-value (list '(integer 2 1) nil *complex-type-metadata*) :calls 5000)
      ("complex-type/upgrade-float-interval" upgraded-complex-part-value (list '(float 16777217 16777217) nil *complex-type-metadata*) :calls 2000)
      ("complex-type/upgrade-eql" upgraded-complex-part-value (list '(eql 1) nil *complex-type-metadata*) :calls 5000)
      ("complex-type/mask-any" complex-type-mask-value (list 'complex nil *complex-type-metadata*) :calls 10000)
      ("complex-type/mask-rational" complex-type-mask-value (list '(complex integer) nil *complex-type-metadata*) :calls 10000)
      ("complex-type/query-equal-representation" complex-subtype-query-value (list '(complex integer) (vector '(complex rational) nil) *complex-type-metadata*) :calls 5000)
      ("complex-type/query-disjoint" complex-subtype-query-value (list '(complex single-float) (vector '(complex double-float) nil) *complex-type-metadata*) :calls 5000)
      ("complex-type/query-original-contained" complex-subtype-query-value (list '(complex (single-float 0 1)) (vector '(complex (real 0 2)) nil) *complex-type-metadata*) :calls 1000)
      ("complex-type/query-original-not-contained" complex-subtype-query-value (list '(complex single-float) (vector '(complex (real 0 1)) nil) *complex-type-metadata*) :calls 1000)
      ("complex-type/query-class" complex-subtype-query-value (list 'complex (vector 'number nil) *complex-type-metadata*) :calls 5000)
      ("complex-type/query-empty-real" complex-subtype-query-value (list '(real 2 1) (vector 'complex nil) *complex-type-metadata*) :calls 1000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun complex-type-diagnostic-wat ()
  (runtime-function-definition '$test-complex-type-install
    (wat-fragment #@(func $test-complex-type-install (export "test-complex-type-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*COMPLEX-TYPE-METADATA*") (global.get $current-package))
        (call $complex-type-metadata-function)))))))

(in-package #:miricl-port-bench)
(defun write-complex-type-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::complex-type-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::complex-type-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::complex-type-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path
            :names '("upgraded-complex-part-value" "complex-type-mask-value" "complex-subtype-query-value")))
      (setf (fdefinition 'miricl::complex-type-runtime-wat) original))))
