;;;; Warmed actual real interval workers and equivalent SBCL drivers.
(in-package #:cl-user)
(defun real-type-benchmark-adjacent (value up)
  (sb-int:with-float-traps-masked (:invalid :overflow :underflow)
    (if (typep value 'single-float)
        (let* ((bits (sb-kernel:single-float-bits value))
               (next (if (zerop value) (if up 1 #x80000001)
                         (+ bits (if (eq (plusp value) (not (null up))) 1 -1)))))
          (sb-kernel:make-single-float (if (logbitp 31 next) (- (logand next #xffffffff) #x100000000) next)))
        (let* ((bits (sb-kernel:double-float-bits value))
               (next (if (zerop value) (if up 1 #x8000000000000001)
                         (+ bits (if (eq (plusp value) (not (null up))) 1 -1))))
               (high (ldb (byte 32 32) next)))
          (sb-kernel:make-double-float (if (logbitp 31 high) (- high #x100000000) high) (ldb (byte 32 0) next))))))

(defun real-type-benchmark-metadata (selector object argument)
  (case selector
    (0 (if (and (simple-vector-p object) (= (length object) 2) (eq (svref object 0) :class))
           (cons (svref object 1) t)))
    (1 (let ((next (real-type-benchmark-adjacent object argument)))
         (if (sb-ext:float-infinity-p next) (if argument 1 -1) next)))
    (2 (if (eq argument 'fixnum)
           (and (integerp object) (<= -9223372036854775808 object 9223372036854775807))
           (typep object argument)))
    (3 (list-length object))
    (4 (error 'simple-type-error :datum object :expected-type nil
              :format-control "Malformed real interval control (reason ~D)." :format-arguments (list argument)))
    (5 (if (= object 3) most-positive-single-float most-positive-double-float))))


(defparameter *real-type-metadata* #'real-type-benchmark-metadata)
(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "real-type/" (first case))) miricl-port-bench:*cases*)
    '(("real-type/interval-symbol" real-type-interval-value (list 'real nil *real-type-metadata*) :calls 20000)
      ("real-type/interval-bounds" real-type-interval-value (list '(rational (1/3) (2/3)) nil *real-type-metadata*) :calls 10000)
      ("real-type/float-exact" float-interval-bound-value (list 1 (vector nil 3 t) *real-type-metadata*) :calls 10000)
      ("real-type/float-ratio" float-interval-bound-value (list 1/3 (vector nil 4 t) *real-type-metadata*) :calls 5000)
      ("real-type/float-exclusive" float-interval-bound-value (list 0 (vector t 3 t) *real-type-metadata*) :calls 10000)
      ("real-type/float-clamped" float-interval-bound-value (list (ash 1 1024) (vector nil 3 t) *real-type-metadata*) :calls 5000)
      ("real-type/rational-subtype" real-subtype-query-value (list '(rational (1/3) (2/3)) '(rational 0 1) *real-type-metadata*) :calls 5000)
      ("real-type/ratio-subtype" real-subtype-query-value (list '(rational (0) (1)) 'ratio *real-type-metadata*) :calls 5000)
      ("real-type/singleton-integer" real-subtype-query-value (list '(rational 2 2) '(integer 0 3) *real-type-metadata*) :calls 5000)
      ("real-type/single-subtype" real-subtype-query-value (list '(single-float 1/3 2/3) '(single-float 0 1) *real-type-metadata*) :calls 2000)
      ("real-type/real-subtype" real-subtype-query-value (list '(real (0) (1)) '(real -1 2) *real-type-metadata*) :calls 2000)
      ("real-type/empty-float" real-subtype-query-value (list '(double-float 1/3 1/3) nil *real-type-metadata*) :calls 5000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun real-type-diagnostic-wat ()
  (runtime-wat-sequence
    (runtime-function-definition '$test-real-type-install
      (wat-fragment #@(func $test-real-type-install (export "test-real-type-install")
        (drop (call $set-symbol-value
          (call $intern ,(wat-text-literal "*REAL-TYPE-METADATA*") (global.get $current-package))
          (call $real-type-metadata-function))))))
    (runtime-function-definition '$test-real-type-packets
      (wat-fragment #@(func $test-real-type-packets (export "test-real-type-packets") (param $error-mode i32) (result i32)
        (local $packet (ref null eq)) (local $caught i32) (local $ok i32)
        (local.set $packet (call $cons (call $int (i32.const 21)) (ref.null eq)))
        (global.set $extra-values (local.get $packet)) (global.set $value-count (i32.const 7))
        (block $failed (try_table (catch $transfer $failed)
          (if (local.get $error-mode)
            (then (drop (call $real-type-worker
              (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'real)))
                (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'symbol))) (ref.null eq)))
              (ref.null eq) (i32.const 0))))
            (else
              (call $real-type-interval (call $symbol (i32.const ,(runtime-symbol-id 'real))) (i32.const 0)) (drop) (drop) (drop) (drop)
              (drop (call $float-interval-bound (call $int (i32.const 0)) (i32.const 1) (i32.const 3) (i32.const 1)))
              (drop (call $real-type-worker (call $symbol (i32.const ,(runtime-symbol-id 'rational)))
                (call $symbol (i32.const ,(runtime-symbol-id 'real))) (i32.const 2)))))
          (local.set $caught (i32.const 1))))
        (local.set $ok (i32.and
          (i32.and (ref.eq (global.get $extra-values) (local.get $packet)) (i32.eq (global.get $value-count) (i32.const 7)))
          (i32.eq (local.get $caught) (i32.eqz (local.get $error-mode)))))
        (global.set $error (i32.const 0)) (global.set $last-condition (ref.null eq))
        (global.set $transfer-target (ref.null eq)) (global.set $transfer-values (ref.null eq))
        (call $clear-values) (local.get $ok))))))

(in-package #:miricl-port-bench)
(defun write-real-type-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::real-type-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::real-type-runtime-wat)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::real-type-diagnostic-wat))))
          (write-benchmark-image path :catalog catalog :wat-path wat-path
            :names '("real-type-interval-value" "float-interval-bound-value" "real-subtype-query-value")))
      (setf (fdefinition 'miricl::real-type-runtime-wat) original))))
