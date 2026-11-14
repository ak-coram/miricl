(in-package #:miricl-test)
(in-suite :miricl)

(test rational-interval-continued-fractions
  (let ((operations (vector #'+ #'- #'* #'/ #'truncate))
        (bounds '(0 1/17 1/7 1/3 1/2 2/3 1 11/7 2 17/5 11 123456789012345678901/7)))
    (dolist (a bounds)
      (dolist (b bounds)
        (when (< a b)
          (dolist (inclusive '(nil t))
            (let* ((result (miricl-runtime:rational-in-interval-value a b inclusive operations))
                   ;; A mediant supplies an upper bound for the first suitable
                   ;; denominator, independent of the continued-fraction code.
                   (denominator
                     (loop for d from 1 to (+ (denominator a) (denominator b))
                           for low = (if inclusive (ceiling (* a d)) (1+ (floor (* a d))))
                           for high = (if inclusive (floor (* b d)) (1- (ceiling (* b d))))
                           when (<= low high) return d)))
              (is (rationalp result))
              (is (if inclusive (<= a result b) (< a result b)))
              (is (= denominator (denominator result))))))))
    (dolist (a bounds)
      (is (= a (miricl-runtime:rational-in-interval-value a a t operations)))
      (is (= (ceiling a) (miricl-runtime:rational-in-interval-value a nil t operations)))
      (is (= (1+ (floor a)) (miricl-runtime:rational-in-interval-value a nil nil operations))))))

(test rational-interval-primary-values
  (let* ((ordinary (vector #'+ #'- #'* #'/ #'truncate))
         (operations (map 'vector (lambda (function)
                                    (lambda (&rest args)
                                      (values (apply function args) :discarded))) ordinary)))
    (is (equal '(1/3) (multiple-value-list
                       (miricl-runtime:rational-in-interval-value 3/10 2/5 nil operations))))))

(test rational-interval-raw-boundaries
  (let ((original (fdefinition 'miricl::rounding-runtime-wat)))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::rounding-runtime-wat)
            (lambda ()
              (miricl::runtime-wat-sequence
                (funcall original)
                (miricl::wat-fragment
                  "(func (export \"rational-interval-packet\") (param $mode i32) (result i32)
                     (local $a (ref null eq)) (local $b (ref null eq))
                     (local $saved (ref null eq)) (local $caught i32) (local $inclusive i32)
                     (local.set $a (call $ratio (call $int (i32.const 3)) (call $int (i32.const 10))))
                     (local.set $b (call $ratio (call $int (i32.const 2)) (call $int (i32.const 5))))
                     (if (i32.eq (local.get $mode) (i32.const 1)) (then (local.set $b (ref.null eq))))
                     (if (i32.eq (local.get $mode) (i32.const 2)) (then (local.set $b (local.get $a)) (local.set $inclusive (i32.const 1))))
                     (if (i32.eq (local.get $mode) (i32.const 3)) (then (local.set $a (ref.null eq))))
                     (if (i32.eq (local.get $mode) (i32.const 4)) (then (local.set $a (call $int (i32.const -1)))))
                     (if (i32.eq (local.get $mode) (i32.const 5)) (then (local.set $b (call $int (i32.const 0)))))
                     (if (i32.eq (local.get $mode) (i32.const 6)) (then (local.set $b (local.get $a))))
                     (if (i32.eq (local.get $mode) (i32.const 7)) (then (local.set $b (call $float (i32.const 4) (f64.const 1)))))
                     (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
                     (global.set $value-count (i32.const 2))
                     (local.set $saved (global.get $extra-values))
                     (block $caught (try_table (catch $transfer $caught)
                       (drop (call $rational-in-interval (local.get $a) (local.get $b) (local.get $inclusive)))
                       (local.set $caught (i32.const -1))))
                     (i32.and (i32.eq (local.get $caught) (select (i32.const -1) (i32.const 0) (i32.lt_u (local.get $mode) (i32.const 3))))
                       (i32.and (i32.eq (global.get $value-count) (i32.const 2))
                         (ref.eq (local.get $saved) (global.get $extra-values)))))"))))
          (let* ((miricl::*interpreter-only-wasm* nil)
                 (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
            (m:with-interpreter (vm)
              (dotimes (mode 8)
                (is (= 1 (wt:instance-call (miricl::interpreter-context vm)
                                           (miricl::interpreter-instance vm)
                                           "rational-interval-packet" mode))))
              (is (= 42 (m:evaluate-string vm "(+ 20 22)"))))))
      (setf (fdefinition 'miricl::rounding-runtime-wat) original))))
