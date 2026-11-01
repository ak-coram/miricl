;;;; Real host services remain available through an import-free core image.
(in-package #:miricl-test)
(in-suite :miricl)

(test portable-host-time-services
  (m:with-interpreter (vm)
    (let* ((before (get-universal-time))
           (stamp (m:evaluate vm '(get-universal-time))))
      (is (<= before stamp (get-universal-time))))
    (is (equal '(0 0 0 1 1 1900 0 nil 0)
      (m:evaluate vm '(multiple-value-list (decode-universal-time 0 0)))))
    (is (eq t (m:evaluate-string vm
      "(let ((start (get-internal-real-time)))
         (and (null (sleep 1/50)) (>= (- (get-internal-real-time) start) 15)))")))
    (is (eq t (m:evaluate-string vm
      "(let ((before (get-internal-run-time)))
         (dotimes (i 1000) (* i i)) (>= (get-internal-run-time) before))")))
    (is (equal '(1 2 3) (m:evaluate-string vm
      "(with-output-to-string (*trace-output*)
         (defparameter *time-values* (multiple-value-list (time (values 1 2 3)))))
       *time-values*")))
    (is (= 1 (m:evaluate vm '(length (multiple-value-list (get-internal-real-time))))))
    (is (= 1 (m:evaluate vm '(length (multiple-value-list (get-internal-run-time))))))))
