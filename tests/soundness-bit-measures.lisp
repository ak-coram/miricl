;; A dominated FIXNUM observation can support either physical integer form.
;; Updating the binding ends the earlier observation.
(loop for (operation safety immediate boxed) in '((logcount 3 2 3) (integer-length 2 4 33))
      always
  (let ((guarded
          (miricl-compiler:compile-native
            `(lambda (value before)
               (declare (optimize (safety ,safety) (debug 0) (speed 3) (space 1)))
               (if (typep value 'fixnum)
                   (progn (funcall before) (,operation value))
                   :wide))))
        (mutated
          (miricl-compiler:compile-native
            `(lambda (value replacement)
               (declare (optimize (safety ,safety) (debug 0) (speed 3) (space 1)))
               (when (typep value 'fixnum)
                 (setf value replacement)
                 (,operation value)))))
        (effects 0))
    (flet ((before () (incf effects) (values 1 2 3)))
      (and (equal (multiple-value-list (funcall guarded -13 #'before)) (list immediate))
           (equal (multiple-value-list (funcall guarded -4294967300 #'before)) (list boxed))
           (eq (funcall guarded (ash 1 70) #'before) :wide)
           (= effects 2)
           (handler-case (progn (funcall mutated 1 :wrong) nil)
             (type-error (condition) (eq (type-error-datum condition) :wrong)))
           (= (funcall mutated 1 -13) immediate)))))
