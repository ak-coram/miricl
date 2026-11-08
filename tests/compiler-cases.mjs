// These forms run unchanged in Node and in the browser's Lisp environment.
export const compilerCases = [
  ['arithmetic and branches', `(progn
    (defparameter *compiler-test-function* (miricl-compiler:compile-native
      '(lambda (x) (if x (+ x 2) 0))))
    (and (compiled-function-p *compiler-test-function*)
         (= (funcall *compiler-test-function* 40) 42)
         (= (funcall *compiler-test-function* nil) 0)))`],
  ['shared mutable captures', `(let ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () (incf x)) (lambda () x))))))
    (multiple-value-bind (increment read) (funcall factory 41)
      (and (= (funcall increment) 42) (= (funcall read) 42)
           (= (funcall increment) 43) (= (funcall read) 43))))`],
  ['unassigned transitive captures', `(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (lambda () (lambda () x)))))
      (object (list 1)) (middle (funcall factory object)) (inner (funcall middle)))
    (setf (car object) 42)
    (and (eq (funcall inner) object) (= (car (funcall inner)) 42)))`],
  ['zero and multiple values', `(let ((function (miricl-compiler:compile-native
      '(lambda (flag) (multiple-value-bind (a b c) (if flag (values 1 2) (values))
                       (values a b c))))))
    (and (equal (multiple-value-list (funcall function t)) '(1 2 nil))
         (equal (multiple-value-list (funcall function nil)) '(nil nil nil))))`],
  ['dynamic bindings', `(progn (defvar *compiler-test-special* :outside)
    (let ((function (miricl-compiler:compile-native
        '(lambda (*compiler-test-special*)
           (values *compiler-test-special* (symbol-value '*compiler-test-special*))))))
      (and (equal (multiple-value-list (funcall function :inside)) '(:inside :inside))
           (eq *compiler-test-special* :outside))))`],
  ['loops and keyword arguments', `(let ((function (miricl-compiler:compile-native
      '(lambda (n &key (initial 0))
         (let ((sum initial)) (dotimes (i n sum) (incf sum i)))))))
    (= (funcall function 10 :initial 7) 52))`],
  ['global redefinition', `(progn
    (defun compiler-test-callee (x) (+ x 1))
    (let ((caller (miricl-compiler:compile-native '(lambda (x) (compiler-test-callee x)))))
      (defun compiler-test-callee (x) (+ x 2))
      (= (funcall caller 40) 42)))`],
  ['symbol iteration', `(let ((package (make-package "MIRICL-NATIVE-SYMBOLS" :use nil)))
    (unwind-protect
      (let* ((internal (intern "INTERNAL" package)) (external (intern "EXTERNAL" package))
             (function (miricl-compiler:compile-native
               '(lambda (package)
                  (let ((evaluations 0) (seen nil) (external nil) (all nil))
                    (multiple-value-bind (final count)
                        (do-symbols (symbol (progn (incf evaluations) package)
                                            (values symbol (length seen)))
                          (push symbol seen) (setq symbol :changed))
                      (do-external-symbols (symbol package) (push symbol external))
                      (do-all-symbols (symbol)
                        (when (eq (symbol-package symbol) package) (push symbol all)))
                      (values evaluations final count seen external all)))))))
        (export external package)
        (multiple-value-bind (evaluations final count seen exported all) (funcall function package)
          (and (= evaluations 1) (null final) (= count 2)
               (null (set-exclusive-or seen (list internal external)))
               (equal exported (list external))
               (null (set-exclusive-or all (list internal external))))))
      (delete-package package)))`],
  ['literal cycles and identity', `(let* ((literal (list 1))
      (source (list 'lambda nil (list 'quote literal))))
    (setf (cdr literal) literal)
    (let ((function (miricl-compiler:compile-native source)))
      (and (eq (funcall function) literal) (eq (cdr (funcall function)) literal))))`],
  ['native condition propagation', `(let ((function (miricl-compiler:compile-native '(lambda () (/ 1 0)))))
    (handler-case (progn (funcall function) nil) (division-by-zero () t)))`],
  ['native catches and throws', `(let ((function (miricl-compiler:compile-native
      '(lambda (tag flag) (catch tag (if flag (throw tag (values 1 2)) (values)))))))
    (let ((tag (list :tag)))
      (and (equal (multiple-value-list (funcall function tag t)) '(1 2))
           (null (multiple-value-list (funcall function tag nil))))))`],
  ['native handlers and no-error values', `(let ((function (miricl-compiler:compile-native
      '(lambda (x) (handler-case (if x (/ 1 0) (values 20 22))
                    (division-by-zero (c) (typep c 'division-by-zero))
                    (:no-error (a b) (+ a b)))))))
    (and (eq (funcall function t) t) (= (funcall function nil) 42)))`],
  ['native cleanup on transfers', `(let ((function (miricl-compiler:compile-native
      '(lambda (override) (let ((cleaned nil))
         (values (catch 'done (unwind-protect (throw 'done 1)
                               (setq cleaned t) (if override (throw 'done 2)))) cleaned))))))
    (and (equal (multiple-value-list (funcall function nil)) '(1 t))
         (equal (multiple-value-list (funcall function t)) '(2 t))))`],
  ['native signaling handler', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((seen nil))
         (handler-bind ((simple-condition (lambda (condition) (setq seen (typep condition 'simple-condition)))))
           (signal 'simple-condition)) seen)))))
    (funcall function))`],
  ['native type dispatch', `(let ((function (miricl-compiler:compile-native
      '(lambda (x) (typecase x ((integer 0 10) (values x :small))
                              (string (values (length x) :text)) (t (values)))))))
    (and (equal (multiple-value-list (funcall function 7)) '(7 :small))
         (equal (multiple-value-list (funcall function "abc")) '(3 :text))
         (null (multiple-value-list (funcall function 20)))))`],
  ['native arithmetic loop limits', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((up 0) (down 0))
         (loop for i fixnum from 1 to 5 by 2 do (incf up i))
         (loop for i fixnum downfrom 5 above 0 by 2 do (incf down i))
         (values up down))))))
    (equal (multiple-value-list (funcall function)) '(9 9)))`],
  ['native assertion continuation', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((ready nil))
         (handler-bind ((error (lambda (condition) (declare (ignore condition))
                                 (setq ready t) (invoke-restart 'continue))))
           (assert ready)) ready)))))
    (funcall function))`],
  ['no-error handler scope', `(let ((function (miricl-compiler:compile-native
      '(lambda () (handler-case
          (handler-case 42 (simple-error () :wrong)
            (:no-error (x) (declare (ignore x)) (error "Outside handler scope")))
          (simple-error () :outer))))))
    (eq (funcall function) :outer))`],
  ['cleanup preserves the original condition', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((cleaned nil))
         (handler-case (unwind-protect (/ 1 0) (setq cleaned t) (values 9 8))
           (division-by-zero () (values cleaned :original))))))))
    (equal (multiple-value-list (funcall function)) '(t :original)))`],
];
