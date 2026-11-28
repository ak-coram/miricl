;; Two queued interrupts distinguish a discarded parity test from a sign
;; test. Observe delivery before the worker's return can run another poll.
(loop for (operation count) in '((progn 0) (oddp 1) (plusp 2)) always
  (let ((worker (compile-native
                  `(lambda (value arm)
                     (declare (optimize (safety 3) (debug 0)))
                     (when (typep value 'fixnum)
                       (let ((delivered 0))
                         (labels ((queue (callback mark) (funcall callback mark) nil))
                           (declare (notinline queue))
                           (,operation
                             (progn
                               (queue arm (lambda () (incf delivered) (values :first :extra)))
                               value))
                           (values delivered nil :tail)))))))
        (armed 0) (fired 0))
    (let ((results
            (multiple-value-list
              (funcall worker 1073741825
                (lambda (mark)
                  (incf armed)
                  (miricl-threads:without-interrupts
                    (dotimes (index 2)
                      (miricl-threads:interrupt-thread (miricl-threads:current-thread)
                        (lambda () (incf fired) (funcall mark))))))))))
      (miricl-threads:thread-yield)
      (miricl-threads:thread-yield)
      (and (= armed 1) (= fired 2) (equal results (list count nil :tail))))))
