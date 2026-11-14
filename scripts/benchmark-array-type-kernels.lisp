;;;; Shared warmed driver fixtures for actual array type CL workers.
(in-package #:cl-user)
(defun array-type-benchmark-metadata (selector object environment)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; Explicit target-model operations, never the host's array upgrading.
  (case selector
    (0 (or (cdr (assoc object environment :test #'equal)) object))
    (1 (cond ((or (null object) (eq object 'extended-char)) 3)
             ((member object '(character base-char standard-char)) 1)
             ((or (eq object 'bit) (equal object '(integer 0 1))) 2)
             (t 0)))
    (2 (if (member object '(array simple-array vector simple-vector string simple-string
                           base-string simple-base-string bit-vector simple-bit-vector
                           integer symbol character cons function))
           (cons object t)))
    (3 (list-length object))
    (4 (error 'simple-type-error :datum object :expected-type nil
              :format-control "Malformed array type (reason ~D)." :format-arguments (list environment)))))
(defparameter *array-type-metadata* #'array-type-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "array-type/" (first case))) miricl-port-bench:*cases*)
    '(("array-type/symbol" array-type-query-value
       (list 'array nil (vector 0 0 -1 nil) *array-type-metadata*) :calls 20000)
      ("array-type/vector" array-type-query-value
       (list '(vector bit 17) nil (vector 0 0 -1 nil) *array-type-metadata*) :calls 20000)
      ("array-type/element-upgrade" array-type-query-value
       (list '(array (integer 0 1) (* 2)) nil (vector 0 0 -1 nil) *array-type-metadata*) :calls 10000)
      ("array-type/shape" array-type-query-value
       (list '(7 11 13) nil (vector 1 15 -1 nil) *array-type-metadata*) :calls 20000)
      ("array-type/shape-zero" array-type-query-value
       (list '(536870910 2 0) nil (vector 1 1 -1 nil) *array-type-metadata*) :calls 20000)
      ("array-type/intersection" array-type-query-value
       (list '(and (array * (* 2)) (simple-array bit (1 *))) nil (vector 2 0 -1 nil) *array-type-metadata*) :calls 5000)
      ("array-type/subtype-yes" array-type-query-value
       (list '(array bit (3)) '(array bit (*)) (vector 3 0 -1 nil) *array-type-metadata*) :calls 10000)
      ("array-type/subtype-no" array-type-query-value
       (list '(array character (*)) '(array t (*)) (vector 3 0 -1 nil) *array-type-metadata*) :calls 10000)
      ("array-type/control-validation" check-type-control-value
       (list '(and (array bit (3)) (or (array character (*)) (array nil (*)))) *array-type-metadata*) :calls 10000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun array-type-diagnostic-wat ()
  (runtime-function-definition '$test-array-type-install
    (wat-fragment #@(func $test-array-type-install (export "test-array-type-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*ARRAY-TYPE-METADATA*") (global.get $current-package))
        (call $array-type-metadata-function)))))))

(in-package #:miricl-port-bench)
(defun write-array-type-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::array-type-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::array-type-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::array-type-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path
            :names '("array-type-query-value" "check-type-control-value")))
      (setf (fdefinition 'miricl::array-type-runtime-wat) original))))
