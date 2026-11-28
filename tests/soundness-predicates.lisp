;; A symbolic inclusion proof need not produce a positive runtime SUBTYPEP.
;; Compare both branch and value-group consumers with the same live query.
(progn
  (dolist (source '((or (integer 0 1) (integer 100 200)) (integer 0 7) string))
    (let* ((answer (multiple-value-list (subtypep source 'integer)))
           (branch (compile-native
                     `(lambda ()
                        (let ((type ',source))
                          (or (not (subtypep type 'integer)) :subtype)))))
           (query (compile-native `(lambda () (subtypep ',source 'integer)))))
      (assert (eq (funcall branch) (if (first answer) :subtype t)))
      (assert (equal (multiple-value-list (funcall query)) answer))))
  (values))

;; The false ATOM branch establishes the shape of this SSA value. A callback
;; can change its fields, and a replacement value needs its own check.
(let ((read-live
        (compile-native
          '(lambda (cell update)
             (declare (optimize (safety 1)))
             (if (atom cell) :atom
                 (progn (funcall update cell) (cdr cell))))))
      (read-replacement
        (compile-native
          '(lambda (cell replacement)
             (declare (optimize (safety 1)))
             (if (atom cell) :atom
                 (progn (setq cell replacement) (cdr cell)))))))
  (let ((cell (cons :head :old))
        (replacement (list :head :tail)))
    (assert (equal (multiple-value-list
                     (funcall read-live cell (lambda (value) (setf (cdr value) :new))))
                   '(:new)))
    (assert (handler-case (progn (funcall read-replacement cell :invalid) nil)
              (type-error (condition) (eq (type-error-datum condition) :invalid))))
    (assert (eq (funcall read-replacement cell replacement) (cdr replacement)))))

;; Executed checks can prove a current SSA observation at any safety level.
;; Exercise the immediate, boxed scalar and fallback representations.
(loop for safety from 1 to 3 always
  (let ((guarded
          (compile-native
            `(lambda (value other before)
               (declare (optimize (safety ,safety) (debug 0)))
               (if (and (typep value 'fixnum) (typep other 'fixnum))
                   (progn
                     (funcall before)
                     (values (plusp value) (minusp value) (zerop value)
                             (evenp value) (oddp value) (integerp value)
                             (numberp value) (consp value)
                             (eql value other) (= value other) (/= value other)))
                   :outside))))
        (branch
          (compile-native
            `(lambda (value)
               (declare (optimize (safety ,safety) (debug 0)))
               (cond ((not (integerp value)) :outside)
                     ((minusp value) (if (plusp value) :wrong :negative))
                     ((plusp value) (if (zerop value) :wrong :positive))
                     (t (if (zerop value) :zero :wrong))))))
        (mutated
          (compile-native
            `(lambda (value replacement)
               (declare (optimize (safety ,safety) (debug 0)))
               (when (typep value 'fixnum)
                 (setf value replacement)
                 (evenp value)))))
        (operations
          (compile-native
            `(lambda (value other cell replacement)
               (declare (optimize (safety ,safety) (debug 0)))
               (if (and (typep value 'fixnum) (typep other 'fixnum) (consp cell))
                   (progn
                     (endp cell)
                     (rplaca cell replacement)
                     (values (car cell) (cdr cell) (logior value other)
                             (logxor value other) (lognot value)))
                   :outside))))
        (calls 0))
    (flet ((before () (incf calls) (values :first :second :third))
           (truths (results) (mapcar (lambda (value) (not (null value))) results)))
      (and (equal (truths (multiple-value-list (funcall guarded 0 0 #'before)))
                  '(nil nil t t nil t t nil t t nil))
           (equal (truths (multiple-value-list (funcall guarded -4294967301 -4294967300 #'before)))
                  '(nil t nil nil t t t nil nil nil t))
           (equal (truths (multiple-value-list (funcall guarded 9223372036854775807
                                                             9223372036854775807 #'before)))
                  '(t nil nil nil t t t nil t t nil))
           (eq (funcall guarded (ash 1 90) 0 #'before) :outside)
           (= calls 3)
           (eq (funcall branch (ash 1 90)) :positive)
           (eq (funcall branch (- (ash 1 90))) :negative)
           (eq (funcall branch 0) :zero)
           (eq (funcall branch 1/2) :outside)
           (handler-case (progn (funcall mutated 1 :wrong) nil)
             (type-error (condition) (eq (type-error-datum condition) :wrong)))
           (funcall mutated 1 (ash 1 90))
           (let ((cell (cons nil :tail)) (value -4294967301) (other 4294967300))
             (and (equal (multiple-value-list (funcall operations value other cell :written))
                         (list :written :tail (logior value other)
                               (logxor value other) (lognot value)))
                  (eq (car cell) :written)))))))
