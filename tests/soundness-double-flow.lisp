;; Exact floating-point snapshots and the ordinary boxed continuation.
(loop for precision in '(single-float double-float) always
  (let* ((zero (coerce 0 precision))
         (negative-zero (- zero))
         (one (coerce 1 precision))
         (large (coerce (expt 2 (float-digits one)) precision)))
    (loop for (speed space) in '((1 2) (3 1)) always
      (let* ((sum
               (miricl-compiler:compile-native
                `(lambda (array count seed observe)
                   (declare (vector array) (type (integer 0 8) count) (type ,precision seed)
                            (optimize (speed ,speed) (space ,space) (safety 1) (debug 0)))
                   (let ((total seed))
                     (declare (type ,precision total))
                     (dotimes (i count (values total :done))
                       (setq total
                             (+ total (the ,precision
                                           (let ((element (aref array i)))
                                             (funcall observe i)
                                             element)))))))))
             (difference
               (miricl-compiler:compile-native
                `(lambda (array count seed)
                   (declare (vector array) (type (integer 0 8) count) (type ,precision seed)
                            (optimize (speed ,speed) (space ,space) (safety 1) (debug 0)))
                   (let ((total seed))
                     (declare (type ,precision total))
                     (do ((i count (1- i))) ((zerop i) total)
                       (setq total (- total (the ,precision (aref array (1- i))))))))))
             (owned (make-array 3 :element-type precision :initial-contents (list one negative-zero (+ one one))))
             (other (make-array 4 :element-type precision
                      :initial-contents (mapcar (lambda (x) (coerce x precision)) '(0 30 40 50))))
             (view (make-array 3 :element-type precision :adjustable t :displaced-to owned))
             (events nil)
             (observe (lambda (i) (push i events) (values :ignored :extra)))
             (change (lambda (i)
                       (funcall observe i)
                       (when (zerop i)
                         (adjust-array view 3 :displaced-to other :displaced-index-offset 1)))))
        (and (equal (multiple-value-list (funcall sum view 2 zero change)) (list (coerce 41 precision) :done))
             (equal events '(1 0))
             (minusp (float-sign (funcall sum (vector negative-zero) 1 negative-zero observe)))
             (minusp (float-sign (funcall sum owned 0 negative-zero observe)))
             (loop for input in (list (vector one (- large))
                                     (make-array 2 :element-type precision
                                       :initial-contents (list one (- large))))
                   always (eql (funcall sum input 2 large observe) zero))
             (eql (funcall difference view 2 zero) (coerce -70 precision))
             (handler-case (progn (funcall difference view 4 zero) nil) (error () t))
             (eql (funcall difference owned 2 (coerce 8 precision)) (coerce 7 precision)))))))
