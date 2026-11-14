;;;; Generic saved quotient callbacks and the existing digit-table worker.
;;;; Load after benchmark-runtime-kernels and benchmark-character-kernels.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
  (append
   (remove-if-not (lambda (case) (eq (second case) 'character-digit-code-value))
                  miricl-port-bench:*cases*)
   (loop for name in '(floor ceiling truncate round) append
     (loop for (kind a b count) in '(("positive" 13 4 200000)
                                    ("negative" -13 4 200000)
                                    ("tie-even" -14 4 200000)
                                    ("rational" 13/7 4/11 100000)
                                    ("float" 1.25d0 0.5d0 100000)
                                    ("bignum" 1329227995784915872903807060280344576 7 20000))
       collect (list (format nil "rounding/~(~A~)-~A" name kind) 'ordering-predicate
                     `(list ,a ,b nil #',name) :calls count)))))
