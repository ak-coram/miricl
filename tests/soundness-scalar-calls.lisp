;;;; Ignored operands still cross the checked scalar native call boundary.
(in-package :miricl-compiler)

(defparameter *soundness-scalar-workers*
  (map 'vector #'compile-native
       '((lambda ()
           (declare (optimize (debug 1) (safety 1))) 42)
         (lambda (a)
           (declare (ignore a) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b)
           (declare (ignore a b) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b c)
           (declare (ignore a b c) (optimize (debug 1) (safety 1))) 42)
         (lambda (a b c d)
           (declare (ignore a b c d) (optimize (debug 1) (safety 1))) 42))))
(values)

(defun check-wide-native-calls ()
  (dolist (arity '(5 8 16 17) t)
    (let* ((parameters (loop repeat arity collect (gensym)))
           (arguments (loop for index below arity collect index))
           (last (car (last parameters)))
           (worker (compile-native
                     `(lambda ,parameters
                        (declare (optimize (debug 1)))
                        (values ,last ,(first parameters)))))
           (caller (compile-native
                     `(lambda (worker)
                        (declare (optimize (debug 1)))
                        (multiple-value-list (funcall worker ,@arguments)))))
           (tail (compile-native
                   `(lambda (worker)
                      (declare (optimize (debug 1)))
                      (funcall worker ,@arguments))))
           (expected (list (1- arity) 0)))
      (assert (equal expected (multiple-value-list (apply worker arguments))))
      (assert (equal expected (funcall caller worker)))
      (assert (equal expected (multiple-value-list (funcall tail worker))))
      (assert (equal (list arguments) (funcall caller #'list)))
      (assert (handler-case (progn (apply worker (cdr arguments)) nil)
                (program-error () t)))
      (assert (equal expected (funcall caller worker)))))
  ;; An escaping closure and its lexical recursive worker share the wide ABI.
  (let ((worker (funcall
                  (compile-native
                    '(lambda (captured)
                       (declare (optimize (debug 1)))
                       (labels ((step (n a b c d e f g h i j k l m p q)
                                  (if (zerop n)
                                      (values captured a q)
                                      (step (1- n) a b c d e f g h i j k l m p q))))
                         #'step)))
                  :captured)))
    (assert (equal '(:captured 1 15)
                   (multiple-value-list
                     (funcall worker 100000 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))))
  ;; The compiled entry uses the highest operand bit in the wide usage mask.
  (let* ((unit (compile-to-hir
                 '(lambda (a b c d e f g h i j k l m n o p)
                    (declare (ignore a b c d e f g h i j k l m n o) (optimize (debug 1)))
                    p)))
         (entry (first (hir-unit-functions unit))))
    (multiple-value-bind (bytes literals) (emit-native-module unit :entries (list entry))
      (let ((worker (compiler-primitive 17 (cons bytes 0) (first literals) nil)))
        (assert (= 16 (funcall worker 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))))
  ;; A wide call can forward one value through an indirect tail position.
  (let ((worker (compile-native
                  '(lambda (n self a b c)
                     (declare (optimize (debug 1)))
                     (if (zerop n) (+ a b c)
                         (funcall self (1- n) self a b c))))))
    (assert (= 42 (funcall worker 100000 worker 12 14 16))))
  ;; Body sharing must keep equal narrow and wide bodies callable through
  ;; both closure attachment and bundle entry installation.
  (let ((source '(lambda ()
                   (declare (optimize (debug 0)))
                   (list (lambda (a) (declare (ignore a)) :same)
                         (lambda (a b c d e) (declare (ignore a b c d e)) :same)))))
    (flet ((check (narrow wide)
             (assert (eq (funcall narrow nil) :same))
             (assert (eq (funcall wide nil nil nil nil nil) :same))))
      (apply #'check (funcall (compile-native source)))
      (let* ((unit (compile-to-hir source))
             (entries (sort (copy-list (rest (hir-unit-functions unit))) #'< :key #'native-fixed-arity)))
        (multiple-value-bind (bytes literals) (emit-native-module unit :entries entries)
          (apply #'check (loop for pool in literals for index from 0
                               collect (compiler-primitive 17 (cons bytes index) pool nil)))))))
  t)
(values)
