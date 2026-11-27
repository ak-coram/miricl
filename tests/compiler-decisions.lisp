(in-package #:cl-user)

;; Use small programs that exercise both sides of the compiler decisions and
;; check their results before the host reads the collected counters.
(let ((scalar '(lambda ()
                 (declare (optimize (space 2) (speed 1) (debug 1)))
                 (do ((position 0 (logand (+ position 3) 65535)))
                     ((>= position 65530) position))))
      (callback '(lambda (visit)
                   (declare (optimize (space 2) (speed 1) (debug 1)))
                   (do ((position 0 (logand (+ position 3) 65535)))
                       ((>= position 65530) position)
                     (funcall visit position))))
      (stopped-at (* 3 (ceiling 65530 3)))
      (visits 0))
  (assert (= (funcall (miricl-compiler:compile-native scalar)) stopped-at))
  (assert (= (funcall (miricl-compiler:compile-native callback)
               (lambda (position)
                 (assert (= position (* 3 visits)))
                 (incf visits)))
             stopped-at))
  (assert (= visits (ceiling stopped-at 3)))
  (assert (= (funcall (miricl-compiler:compile-native
                       `(lambda () (declare (optimize (space 3) (speed 1) (debug 1)))
                          ,@(cdddr scalar))))
             stopped-at)))

(let ((branch (miricl-compiler:compile-native
                '(lambda (choice)
                   (declare (optimize (debug 1)))
                   (if (if choice nil nil) 0 42)))))
  (assert (= (funcall branch t) (funcall branch nil) 42)))

(let ((inline (miricl-compiler:compile-native
                '(lambda (value)
                   (flet ((increment (x) (1+ x)))
                     (declare (inline increment))
                     (increment value)))))
      (retained (miricl-compiler:compile-native
                  '(lambda (value)
                     (flet ((increment (x) (1+ x)))
                       (declare (notinline increment))
                       (increment value))))))
  (assert (= (funcall inline 41) (funcall retained 41) 42)))

;; An argument that exits the caller's contour prevents the call from reaching
;; its continuation, independently of whether expansion would be allowed.
(dolist (policy '(inline notinline))
  (miricl-compiler:with-compilation-statistics (statistics)
    (let ((function (miricl-compiler:compile-native
                      `(lambda ()
                         (flet ((increment (x) (1+ x)))
                           (declare (,policy increment))
                           (block exit (increment (return-from exit 42))))))))
      (assert (= (funcall function) 42))
      (assert (not (assoc :decision/lexical-inline
                          (miricl-compiler:compilation-statistics-report statistics)))))))

(let ((typed (miricl-compiler:compile-native
               '(lambda (value)
                  (declare (optimize (safety 1) (debug 0)))
                  (1+ (logand value 1023)))))
      (generic (miricl-compiler:compile-native
                 '(lambda (value)
                    (declare (optimize (safety 1) (debug 0)))
                    (1+ value)))))
  (assert (= (funcall typed 7) 8))
  (assert (= (funcall generic 3/2) 5/2))
  (let ((wide (ash 1 80)))
    (assert (= (funcall generic wide) (+ wide 1)))))

(let ((cached (miricl-compiler:compile-native
                '(lambda (value)
                   (declare (optimize (safety 1) (debug 0)))
                   (1+ (1+ (logand value 1023))))))
      (extract (miricl-compiler:compile-native
                 '(lambda (value)
                    (declare (type (signed-byte 32) value) (optimize (safety 0) (debug 0)))
                    (1+ value))))
      (worker (miricl-compiler:compile-native
                '(lambda (array index)
                   (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)))
                   (logand (aref array index) 36028797018963967))))
      (packet (miricl-compiler:compile-native '(lambda (value) (values value value)))))
  (assert (= (funcall cached 7) (funcall extract 8) 9))
  (assert (= (funcall worker (vector 777) 0) 777))
  (assert (equal (multiple-value-list (funcall packet 42)) '(42 42))))

(let ((single (miricl-compiler:compile-native
                '(lambda (value factor)
                   (declare (optimize (debug 1) (space 2) (speed 1)))
                   (* value (the single-float factor)))))
      (double (miricl-compiler:compile-native
                '(lambda (value offset)
                   (declare (type single-float value) (type double-float offset)
                            (optimize (debug 1) (space 2) (speed 1)))
                   (+ value offset))))
      (character (miricl-compiler:compile-native
                   '(lambda (text index)
                      (declare (optimize (debug 1) (space 2) (speed 1)))
                      (char text index)))))
  (assert (eql (funcall single 3 2.0f0) 6.0f0))
  (assert (eql (funcall double 2.0f0 0.5d0) 2.5d0))
  (assert (eql (funcall character "A" 0) #\A)))

(let* ((entries (miricl-compiler:compile-native-module
                  '((defun diagnostic-character (text index)
                      (declare (optimize (speed 1) (space 2) (debug 1)))
                      (char text index))
                    (defun diagnostic-character-code (text index)
                      (declare (optimize (speed 1) (space 2) (debug 1)))
                      (char-code (diagnostic-character text index))))
                  :entry-points '(diagnostic-character-code) :inline nil))
       (function (cdr (assoc 'diagnostic-character-code entries))))
  (assert (= (funcall function "A" 0) 65)))

(let* ((entries (miricl-compiler:compile-native-module
                  '((defun diagnostic-raw-entry ()
                      (declare (optimize (debug 0) (safety 1) (speed 3) (space 1)))
                      (locally (declare (notinline diagnostic-raw-leaf))
                        (+ (diagnostic-raw-leaf 1073741824) 3)))
                    (defun diagnostic-raw-leaf (argument)
                      (declare (optimize (debug 0) (safety 1)))
                      (+ argument 2)))
                  :entry-points '(diagnostic-raw-entry) :inline nil))
       (function (cdr (assoc 'diagnostic-raw-entry entries))))
  (assert (= (funcall function) (+ 1073741824 5))))

(dolist (debug '(1 2))
  (let ((tail (miricl-compiler:compile-native
                `(lambda (callee value)
                   (declare (optimize (debug ,debug)))
                   (funcall callee value)))))
    (assert (equal (multiple-value-list
                    (funcall tail (lambda (value) (values value :retained)) 42))
                   '(42 :retained)))))

(values)
