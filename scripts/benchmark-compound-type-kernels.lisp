;;;; Matched checked target-service protocol for compound TYPEP and SBCL.
(in-package #:cl-user)
(defun compound-type-benchmark-metadata (selector object argument)
  (case selector
    (0 (miricl-runtime:check-type-control-value argument #'compound-type-benchmark-metadata)
       (if (member (if (consp argument) (car argument) argument) '(and or not member eql satisfies))
           (miricl-runtime:compound-typep-value object argument #'compound-type-benchmark-metadata)
           (if (typep object argument) t nil)))
    (1 (eql object argument))
    (2 (if (funcall (fdefinition argument) object) t nil))
    (3 (list-length object))
    (4 (error 'simple-type-error :datum object :expected-type nil
              :format-control "Malformed type control (reason ~D)." :format-arguments (list argument)))
    (5 (miricl-runtime:check-type-control-value object #'compound-type-benchmark-metadata))))
(defparameter *compound-type-metadata* #'compound-type-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "compound-type/" (first case))) miricl-port-bench:*cases*)
    '(("compound-type/and-empty" compound-typep-value (list 3 '(and) *compound-type-metadata*) :calls 20000)
      ("compound-type/or-empty" compound-typep-value (list 3 '(or) *compound-type-metadata*) :calls 20000)
      ("compound-type/and-success" compound-typep-value (list 3 '(and integer (not string)) *compound-type-metadata*) :calls 10000)
      ("compound-type/and-short" compound-typep-value (list 3 '(and string integer) *compound-type-metadata*) :calls 10000)
      ("compound-type/or-success" compound-typep-value (list 3 '(or string integer) *compound-type-metadata*) :calls 10000)
      ("compound-type/or-short" compound-typep-value (list 3 '(or integer string) *compound-type-metadata*) :calls 10000)
      ("compound-type/not" compound-typep-value (list 3 '(not string) *compound-type-metadata*) :calls 10000)
      ("compound-type/member-empty" compound-typep-value (list 3 '(member) *compound-type-metadata*) :calls 20000)
      ("compound-type/member-last" compound-typep-value (list 8 '(member 1 2 3 4 5 6 7 8) *compound-type-metadata*) :calls 20000)
      ("compound-type/member-miss" compound-typep-value (list 9 '(member 1 2 3 4 5 6 7 8) *compound-type-metadata*) :calls 20000)
      ("compound-type/eql" compound-typep-value (list 3 '(eql 3) *compound-type-metadata*) :calls 20000)
      ("compound-type/satisfies-true" compound-typep-value (list 3 '(satisfies integerp) *compound-type-metadata*) :calls 10000)
      ("compound-type/satisfies-false" compound-typep-value (list 3 '(satisfies stringp) *compound-type-metadata*) :calls 10000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun compound-type-diagnostic-wat ()
  (runtime-function-definition '$test-compound-type-install
    (wat-fragment #@(func $test-compound-type-install (export "test-compound-type-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*COMPOUND-TYPE-METADATA*") (global.get $current-package))
        (call $compound-type-metadata-function)))))))

(in-package #:miricl-port-bench)
(defun write-compound-type-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::type-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::type-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::compound-type-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path :names '("compound-typep-value")))
      (setf (fdefinition 'miricl::type-runtime-wat) original))))
