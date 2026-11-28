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

;; A conditional call allows polling to be batched along the call-free path.
;; The call can still leave the loop nonlocally or return to its ordinary latch.
(miricl-compiler:with-compilation-statistics (statistics)
  (let* ((limit 65530)
         (stop (* 3 (ceiling limit 3)))
         (last-trip (- stop 3))
         (visits nil)
         (worker (miricl-compiler:compile-native
                  `(lambda (visit)
                     (declare (optimize (space 2) (speed 1) (safety 1) (debug 1)))
                     (do ((position 0 (logand (+ position 3) 65535)))
                         ((>= position ,limit) position)
                       (when (= position ,last-trip) (funcall visit)))))))
    (assert (equal (multiple-value-list
                     (catch 'exit-loop
                       (funcall worker (lambda ()
                                         (push :escape visits)
                                         (throw 'exit-loop (values :escaped nil))))))
                   '(:escaped nil)))
    (assert (equal (multiple-value-list
                     (funcall worker (lambda ()
                                       (push :return visits)
                                       (values nil :unused))))
                   (list stop)))
    (assert (equal visits '(:return :escape)))
    (assert (find-if (lambda (row)
                       (and (eq (first row) :decision/loop-strip-mining)
                            (eq (second row) :applied)
                            (plusp (third row))))
                     (miricl-compiler:compilation-statistics-report statistics)))))

;; Keep the exact saved object and result count across each strip. Assigning
;; NIL and a small integer exercises the two constant-reference paths.
(dolist (value '(nil 9))
  (miricl-compiler:with-compilation-statistics (statistics)
    (let ((function (miricl-compiler:compile-native
                     `(lambda (initial)
                        (declare (optimize (space 2) (speed 1) (safety 1) (debug 1)))
                        (let ((saved initial))
                          (do ((position 0 (logand (+ position 3) 65535)))
                              ((>= position 65530) (values position saved))
                            (setq saved ,value)))))))
      (assert (equal (multiple-value-list (funcall function :initial))
                     (list (* 3 (ceiling 65530 3)) value)))
      (assert (find-if (lambda (row)
                         (and (eq (first row) :decision/loop-strip-mining)
                              (eq (second row) :applied)
                              (plusp (third row))))
                       (miricl-compiler:compilation-statistics-report statistics))))))

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

;; Automatic inlining exposes a literal step. Explicit INLINE exposes an
;; arithmetic result that must cross the helper's implicit block exit before
;; the loop can be strip-mined. Check both native compilation paths.
(let ((entry 'diagnostic-block-loop)
      (helper 'diagnostic-block-step)
      (source "/compiler-decisions-block.lisp")
      (portable "/compiler-decisions-block.wasm")
      (native "/compiler-decisions-block-native.wasm")
      (policy '(optimize (space 2) (speed 1) (safety 1) (debug 1)))
      (expected (list (* 3 (ceiling 65530 3)))))
  (flet ((check-decision (statistics rule decision phase)
           (assert (find-if (lambda (row)
                              (and (eq (first row) rule)
                                   (eq (second row) decision)
                                   (plusp (third row))))
                            (miricl-compiler:compilation-statistics-report statistics))
                   () "Expected ~S decision ~S in ~S." rule decision phase))
         (check-entry (function &optional nativep)
           (when nativep (assert (compiled-function-p function)))
           (assert (equal (multiple-value-list (funcall function)) expected))))
    (dolist (directive '(nil inline notinline))
      (let* ((decision (if (eq directive 'notinline) :notinline :applied))
             (strip-decision (if (eq directive 'notinline) :other-call :applied))
             (definitions
              `((defun ,helper ,(when directive '(position))
                  (declare ,policy)
                  ,(if directive '(logand (+ position 3) 65535) 3))
                (defun ,entry ()
                  (declare ,policy ,@(when directive (list (list directive helper))))
                  (do ((position 0 ,(if directive `(,helper position)
                                       `(logand (+ position (,helper)) 65535))))
                      ((>= position 65530) position))))))
        (miricl-compiler:with-compilation-statistics (statistics)
          (let ((entries (miricl-compiler:compile-native-module definitions :entry-points (list entry))))
            (check-decision statistics :decision/lexical-inline decision :sealed-module)
            (check-decision statistics :decision/loop-strip-mining strip-decision :sealed-module)
            (check-entry (cdr (assoc entry entries)) t)))
        (with-open-file (stream source :direction :output :if-exists :supersede)
          (dolist (definition definitions)
            (write definition :stream stream :readably t)
            (terpri stream)))
        (miricl-compiler:with-compilation-statistics (statistics)
          (assert (compile-file source :output-file portable :verbose nil
                                :block-compile t :entry-points (list entry)))
          (check-decision statistics :decision/lexical-inline decision :file-block))
        (load portable :verbose nil)
        (check-entry (fdefinition entry))
        (miricl-compiler:with-compilation-statistics (statistics)
          (miricl-compiler:compile-native-bundle portable native)
          (check-decision statistics :decision/loop-strip-mining strip-decision :native-bundle))
        (fmakunbound entry)
        (load native :verbose nil)
        (check-entry (fdefinition entry) t)
        (assert (not (fboundp helper)))
        (fmakunbound entry)))))

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
