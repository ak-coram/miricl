;;;; Equivalent CLOS metadata operations for SBCL. These host MOP getters are
;;;; benchmark adapters, never assumptions used by the target algorithms.
(in-package #:cl-user)
(defun clos-method-benchmark-metadata (selector object extra)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (case selector
    (0 (sb-mop:class-finalized-p object))
    (1 (sb-mop:finalize-inheritance object))
    (2 (sb-mop:class-default-initargs object))
    (3 (sb-mop:class-slots object))
    ((4 13) (sb-mop:slot-definition-name object))
    (5 (sb-mop:slot-definition-initargs object))
    (6 (sb-mop:slot-boundp-using-class (svref object 0) (svref object 1) (svref object 2)))
    (7 (sb-mop:slot-definition-initfunction object))
    (8 (setf (sb-mop:slot-value-using-class (svref object 0) (svref object 1) (svref object 2)) extra))
    (9 (or (miricl-runtime:list-length-or-cycle object) (error 'type-error :datum object :expected-type 'list)))
    (10 (let ((size (clos-method-benchmark-metadata 9 object nil)))
          (if (oddp size) (error 'program-error) size)))
    (14 (functionp object))
    (11 (class-of object))
    (12 (error 'type-error :datum object :expected-type 'list))))
(defparameter *clos-method-metadata* #'clos-method-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "clos-method/" (first case))) miricl-port-bench:*cases*)
    '(("clos-method/default-supplied" clos-default-initargs-value
       (progn (defclass clos-method-bench-defaults () () (:default-initargs :a 1 :b 2 :c 3))
         (list (find-class 'clos-method-bench-defaults) '(:a 11 :b 22 :c 33) *clos-method-metadata*)) :calls 10000)
      ("clos-method/default-missing" clos-default-initargs-value
       (progn (defclass clos-method-bench-defaults () () (:default-initargs :a 1 :b 2 :c 3))
         (list (find-class 'clos-method-bench-defaults) '(:a 11) *clos-method-metadata*)) :calls 10000)
      ("clos-method/shared-supplied" clos-shared-initialize-value
       (progn (defclass clos-method-bench-slots ()
                ((a :initarg :a :initarg :alias) (b :initarg :b) (c :initarg :c)))
         (let ((class (find-class 'clos-method-bench-slots)))
           (funcall *clos-method-metadata* 1 class nil)
           (list (allocate-instance class) t '(:alias 1 :b 2 :c nil) *clos-method-metadata*))) :calls 5000)
      ("clos-method/shared-bound" clos-shared-initialize-value
       (progn (defclass clos-method-bench-slots ()
                ((a :initarg :a) (b :initarg :b) (c :initarg :c)))
         (list (make-instance 'clos-method-bench-slots :a 1 :b 2 :c 3) t nil *clos-method-metadata*)) :calls 5000)
      ("clos-method/find-slot" clos-find-slot-value
       (progn (defclass clos-method-bench-slots () ((a) (b) (c) (d) (e) (f)))
         (let ((class (find-class 'clos-method-bench-slots)))
           (funcall *clos-method-metadata* 1 class nil)
           (list (funcall *clos-method-metadata* 3 class nil) 'f t *clos-method-metadata*))) :calls 10000)
      ("clos-method/find-slot-generic" clos-find-slot-value
       (progn (defclass clos-method-bench-slots () ((a) (b) (c) (d) (e) (f)))
         (let ((class (find-class 'clos-method-bench-slots)))
           (funcall *clos-method-metadata* 1 class nil)
           (list (funcall *clos-method-metadata* 3 class nil) 'missing nil *clos-method-metadata*))) :calls 5000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun clos-method-diagnostic-wat ()
  (runtime-function-definition '$test-clos-method-install
    (wat-fragment #@(func $test-clos-method-install (export "test-clos-method-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*CLOS-METHOD-METADATA*") (global.get $current-package))
        (call $clos-method-metadata-function)))))))

(in-package #:miricl-port-bench)
(defparameter *clos-method-names*
  '("clos-default-initargs-value" "clos-shared-initialize-value" "clos-find-slot-value"))
(defun write-clos-method-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::$clos-method-metadata-function)))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::$clos-method-metadata-function)
             (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::clos-method-diagnostic-wat))))
           (write-benchmark-image path :catalog catalog :wat-path wat-path :names *clos-method-names*))
      (setf (fdefinition 'miricl::$clos-method-metadata-function) original))))
