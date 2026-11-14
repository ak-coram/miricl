;;;; Equivalent SBCL MOP adapter and shared separate-driver fixtures.
(in-package #:cl-user)
(defun generic-dispatch-benchmark-specializer (object) (sb-mop:intern-eql-specializer object))
(defun generic-dispatch-benchmark-metadata (selector object extra)
  (declare (ignore extra) (optimize (speed 3) (safety 0) (debug 0)))
  (case selector
    (0 (or (miricl-runtime:list-length-or-cycle object)
           (error 'type-error :datum object :expected-type 'list)))
    (1 (sb-mop:eql-specializer-object object))
    (2 (list (loop for method in (sb-mop:generic-function-methods object)
                  for specializer = (first (sb-mop:method-specializers method))
                  when (typep specializer 'sb-mop:eql-specializer) collect specializer)))
    (3 t)
    (4 nil) (5 3) (6 nil)
    (7 (unless (typep object '(simple-vector 7)) (error 'type-error :datum object :expected-type '(simple-vector 7)))
       (let ((mode (svref object 6)))
         (unless (and (typep mode '(integer 0 8191)) (<= (logand mode 7) 4)
                      (= (logand (ash mode -8) 15) (logand (ash mode -8) (ash mode -4) 15)))
           (error 'program-error))))
    (9 (unless (typep object '(integer 0 4)) (error 'program-error)))
    (10 (unless (consp object) (error 'type-error :datum object :expected-type 'cons)))
    (otherwise (error 'program-error))))
(defparameter *generic-dispatch-benchmark-metadata* #'generic-dispatch-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "generic-dispatch/" (first case))) miricl-port-bench:*cases*)
    '(("generic-dispatch/eql-hit" dispatch-eql-key-value
       (list 7 (list (generic-dispatch-benchmark-specializer 1)
                     (generic-dispatch-benchmark-specializer 7)) *generic-dispatch-benchmark-metadata*) :calls 20000)
      ("generic-dispatch/eql-miss" dispatch-eql-key-value
       (list 9 (list (generic-dispatch-benchmark-specializer 1)
                     (generic-dispatch-benchmark-specializer 7)) *generic-dispatch-benchmark-metadata*) :calls 20000)
      ("generic-dispatch/fixed-four" generic-fixed-arity-value
       (list '(a b c d) 4 *generic-dispatch-benchmark-metadata*) :calls 100000)
      ("generic-dispatch/optional" generic-fixed-arity-value
       (list '(a b &optional c) 4 *generic-dispatch-benchmark-metadata*) :calls 100000)
      ("generic-dispatch/cache-global" generic-cache-permitted-value
       (progn (defgeneric generic-dispatch-bench (a)) (list #'generic-dispatch-bench *generic-dispatch-benchmark-metadata*)) :calls 100000)
      ("generic-dispatch/key-four" generic-key-match-value
       (list '(:d :c :b :a) (vector nil '(1 2 3 4) :a :b :c :d 244)
             *generic-dispatch-benchmark-metadata*) :calls 20000)
      ("generic-dispatch/key-eql" generic-key-match-value
       (progn (defgeneric generic-dispatch-bench-eql (a))
         (defmethod generic-dispatch-bench-eql ((a t)) :other)
         (defmethod generic-dispatch-bench-eql ((a (eql 7))) :eql)
         (generic-dispatch-bench-eql 7)
         (list (list (generic-dispatch-benchmark-specializer 7) (find-class 'integer))
               (vector #'generic-dispatch-bench-eql '(7) (find-class 'integer) nil nil nil 273)
               *generic-dispatch-benchmark-metadata*)) :calls 10000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun generic-dispatch-diagnostic-wat ()
  (wat-fragment #@(func $test-generic-dispatch-install (export "test-generic-dispatch-install")
    (drop (call $set-symbol-value
      (call $intern ,(wat-text-literal "*GENERIC-DISPATCH-BENCHMARK-METADATA*") (global.get $current-package))
      (call $generic-dispatch-metadata-function)))
    (drop (call $set-fdefinition
      (call $intern ,(wat-text-literal "GENERIC-DISPATCH-BENCHMARK-SPECIALIZER") (global.get $current-package))
      (call $fdefinition ,(clos-library-reference 'intern-eql-specializer)))))))
(in-package #:miricl-port-bench)
(defparameter *generic-dispatch-names*
  '("dispatch-eql-key-value" "generic-fixed-arity-value" "generic-cache-permitted-value" "generic-key-match-value"))
(defun write-generic-dispatch-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::$generic-dispatch-metadata-function)))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::$generic-dispatch-metadata-function)
             (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::generic-dispatch-diagnostic-wat))))
           (write-benchmark-image path :catalog catalog :wat-path wat-path :names *generic-dispatch-names*))
      (setf (fdefinition 'miricl::$generic-dispatch-metadata-function) original))))
