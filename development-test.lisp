;;;; Host frontend integration for facilities implemented by the guest.
(in-package #:miricl-test)
(in-suite :miricl)

(test interpreter-compile
  (m:with-interpreter (vm)
    (dolist (source
      '("(let ((results (multiple-value-list (compile nil '(lambda (x y) (values y x))))))
           (and (= (length results) 3) (not (second results)) (not (third results))
                (equal (multiple-value-list (funcall (first results) 1 2)) '(2 1))))"
        "(let ((x (list :literal)) (y (list :literal)))
           (let ((fn (compile nil `(lambda () (values ',x ',y)))))
             (multiple-value-bind (a b) (funcall fn) (and (eq x a) (eq y b) (not (eq a b))))))"
        "(let ((text (copy-seq \"abc\")))
           (eq text (funcall (compile nil `(lambda () ,text)))))"
        "(defun compile-target (x) (values x 9))
         (let ((fn #'compile-target))
           (and (equal (multiple-value-list (compile 'compile-target)) '(compile-target nil nil))
                (compiled-function-p #'compile-target)
                (equal (multiple-value-list (funcall fn 7)) '(7 9))
                (equal (multiple-value-list (compile-target 7)) '(7 9))))"
        "(and (eq (compile 'compile-target '(lambda (x) (+ x 3))) 'compile-target)
              (= (compile-target 4) 7))"
        "(let ((name '(setf compile-place)))
           (and (eq name (compile name '(lambda (value object) (setf (car object) value))))
                (= (setf (compile-place (list 1)) 17) 17)))"
        "(defmacro compile-macro (x) `(list ,x))
         (let ((fn (macro-function 'compile-macro)))
           (and (eq (compile 'compile-macro) 'compile-macro)
                (functionp fn) (compiled-function-p (macro-function 'compile-macro))
                (equal (compile-macro 3) '(3))))"
        "(compile 'compile-macro '(lambda (form env) (declare (ignore env)) (list 'quote (cdr form))))
         (equal (compile-macro 1 2) '(1 2))"
        "(let ((fn #'+)) (eq fn (compile nil fn)))"
        "(let ((x 10)) (let* ((fn (lambda (n) (incf x n))) (compiled (compile nil fn)))
           (and (compiled-function-p compiled) (= (funcall compiled 3) 13)
                (= (funcall fn 2) 15) (= x 15))))"
        "(let ((order nil))
           (compile (progn (push 1 order) nil) (progn (push 2 order) '(lambda () nil)))
           (equal order '(2 1)))"
        "(handler-case (compile) (program-error () t))"
        "(handler-case (compile nil '(lambda () nil) nil) (program-error () t))"
        "(handler-case (compile 17 '(lambda () nil)) (type-error () t))"
        "(handler-case (compile nil 'car) (type-error () t))"
        "(handler-case (compile nil nil) (type-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "COMPILE mismatch: ~A" source))))

(test interpreter-disassemble
  (m:with-interpreter (vm)
    (dolist (source
      '("(let ((fn #'car) (result :unset))
           (let ((text (with-output-to-string (*standard-output*) (setq result (disassemble fn)))))
             (and (null result) (plusp (length text)) (not (null (search \"NATIVE-CALL\" text))))))"
        "(let ((text (with-output-to-string (*standard-output*)
                      (disassemble '(lambda (x y) (cons y x))))))
           (and (search \"INTERPRET\" text) (not (null (search \"LAMBDA\" text)))))"
        "(let ((x 17)) (let ((fn (lambda () (incf x))))
           (with-output-to-string (*standard-output*) (disassemble fn))
           (and (= x 17) (= (funcall fn) 18))))"
        "(defun disassembled-target (x) (+ x 3))
         (let ((fn #'disassembled-target) (compiled (compiled-function-p #'disassembled-target)))
           (with-output-to-string (*standard-output*) (disassemble 'disassembled-target))
           (and (eq fn #'disassembled-target)
                (eq compiled (compiled-function-p #'disassembled-target))))"
        "(defun (setf disassembled-place) (value cell) (setf (car cell) value))
         (let* ((name '(setf disassembled-place)) (fn (fdefinition name)))
           (with-output-to-string (*standard-output*) (disassemble name))
           (eq fn (fdefinition name)))"
        "(defgeneric disassembled-generic (x))
         (let ((fn #'disassembled-generic))
           (and (plusp (length (with-output-to-string (*standard-output*) (disassemble fn))))
                (eq fn #'disassembled-generic)))"
        "(let ((count 0))
           (with-output-to-string (*standard-output*) (disassemble (progn (incf count) #'car)))
           (= count 1))"
        "(handler-case (disassemble) (program-error () t))"
        "(handler-case (disassemble #'car nil) (program-error () t))"
        "(handler-case (disassemble 17) (type-error () t))"
        "(handler-case (disassemble \"car\") (type-error () t))"
        "(handler-case (disassemble '(setf car extra)) (type-error () t))"
        "(handler-case (disassemble 'missing-disassembly-target) (undefined-function () t))"))
      (is (eq t (m:evaluate-string vm source)) "DISASSEMBLE mismatch: ~A" source))))

(test interpreter-trace
  (m:with-interpreter (vm)
    (dolist (source
      '("(and (macro-function 'trace) (not (null (macro-function 'untrace))))"
        "(defun traced-target (x) (values x (1+ x)))
         (let ((fn #'traced-target))
           (unwind-protect
               (let ((text (with-output-to-string (*trace-output*)
                             (trace traced-target)
                             (assert (equal (multiple-value-list (traced-target 17)) '(17 18))))))
                 (and (search \"Enter\" text) (search \"Return\" text) (not (null (search \"17\" text)))))
             (untrace traced-target))
           (eq fn #'traced-target))"
        "(trace traced-target) (trace traced-target)
         (prog1 (equal (trace) '(traced-target)) (untrace) (assert (null (trace))))"
        "(untrace traced-target) (untrace traced-target) (null (trace))"
        "(defun (setf traced-place) (value cell) (setf (car cell) value))
         (unwind-protect
             (let* ((cell (list 1))
                    (text (with-output-to-string (*trace-output*)
                            (trace (setf traced-place)) (assert (= (setf (traced-place cell) 23) 23)))))
               (and (= (car cell) 23) (plusp (length text))
                    (equal (trace) '((setf traced-place)))))
           (untrace))"
        "(defun traced-zero () (values))
         (unwind-protect
             (with-output-to-string (*trace-output*)
               (trace traced-zero) (assert (null (multiple-value-list (traced-zero)))))
           (untrace)) t"
        "(defun traced-recursive (n) (if (zerop n) 1 (* n (traced-recursive (1- n)))))
         (unwind-protect
             (let ((text (with-output-to-string (*trace-output*)
                           (trace traced-recursive) (assert (= 24 (traced-recursive 4))))))
               (and (search \"1: Enter\" text) (not (null (search \"5: Enter\" text)))))
           (untrace))"
        "(defun traced-escape () (throw 'trace-exit (values 7 9)))
         (unwind-protect
             (with-output-to-string (*trace-output*)
               (trace traced-escape)
               (assert (equal '(7 9) (multiple-value-list (catch 'trace-exit (traced-escape)))))
               (assert (equal '(7 9) (multiple-value-list (catch 'trace-exit (traced-escape))))))
           (untrace)) t"
        "(defun traced-target (x) x)
         (trace traced-target)
         (defun traced-target (x) (+ x 10))
         (untrace traced-target) (= 17 (traced-target 7))"
        "(trace traced-target) (fmakunbound 'traced-target) (untrace traced-target)
         (not (fboundp 'traced-target))"
        "(defun traced-target (x) x) (trace traced-target)
         (defun traced-target (x) (values x 10)) (trace traced-target)
         (unwind-protect
             (let ((text (with-output-to-string (*trace-output*)
                           (assert (equal '(7 10) (multiple-value-list (traced-target 7)))))))
               (and (plusp (length text)) (equal (trace) '(traced-target))))
           (untrace))"
        "(defgeneric traced-generic (x)) (defmethod traced-generic ((x integer)) (values x :integer))
         (let ((gf #'traced-generic))
           (unwind-protect
               (progn (trace traced-generic)
                 (eval '(defmethod traced-generic ((x symbol)) (values x :symbol)))
                 (let ((text (with-output-to-string (*trace-output*)
                               (assert (equal '(7 :integer) (multiple-value-list (traced-generic 7))))
                               (assert (equal '(a :symbol) (multiple-value-list (traced-generic 'a)))))))
                   (and (eq gf #'traced-generic) (plusp (length text))
                        (equal (trace) '(traced-generic)))))
             (untrace))
           (and (eq gf #'traced-generic)
                (equal '(7 :integer) (multiple-value-list (traced-generic 7)))))"
        "(trace traced-generic)
         (eval '(defgeneric traced-generic (x)))
         (prog1 (equal (trace) '(traced-generic)) (untrace))"
        "(let* ((gf #'traced-generic) (method (find-method gf nil (list (find-class 'integer)))))
           (unwind-protect
               (progn (trace traced-generic) (remove-method gf method)
                 (let ((text (with-output-to-string (*trace-output*)
                               (assert (equal '(a :symbol) (multiple-value-list (traced-generic 'a)))))))
                   (and (plusp (length text)) (equal (trace) '(traced-generic)))))
             (untrace)))"
        "(defun traced-object (x) (values x x))
         (let ((cell (list nil))) (setf (cdr cell) cell)
           (unwind-protect
               (let ((*print-readably* t) (*print-circle* nil))
                 (with-output-to-string (*trace-output*)
                   (trace traced-object)
                   (multiple-value-bind (a b) (traced-object cell)
                     (assert (and (eq a cell) (eq b cell))))) t)
             (untrace)))"
        "(unwind-protect
             (with-output-to-string (*trace-output*)
               (trace apply values-list format +)
               (assert (equal '(1 2) (multiple-value-list (apply #'values-list '((1 2))))))
               (assert (= 3 (+ 1 2))) (format *trace-output* \"Hello~%\"))
           (untrace)) (null (trace))"
        "(handler-case (eval '(trace 17)) (type-error () t))"
        "(handler-case (eval '(trace missing-trace-target)) (undefined-function () t))"
        "(handler-case (eval '(trace if)) (error () t))"
        "(handler-case (eval '(trace when)) (error () t))"))
      (is (eq t (m:evaluate-string vm source)) "TRACE mismatch: ~A" source))))

(test interpreter-room
  (m:with-interpreter (vm)
    (dolist (source
      '("(let ((minimal (with-output-to-string (*standard-output*) (room nil)))
               (normal (with-output-to-string (*standard-output*) (room)))
               (maximal (with-output-to-string (*standard-output*) (room t))))
           (and (< 0 (length minimal) (length normal) (length maximal))
                (search \"Linear memory: \" minimal)
                (not (null (search \"WasmGC heap: \" minimal)))))"
        "(let ((a (with-output-to-string (*standard-output*) (room)))
               (b (with-output-to-string (*standard-output*) (room :default))))
           (and (search \"Symbol identities allocated\" a)
                (search \"Symbol identities allocated\" b)
                (not (search \"Packages:\" a)) (not (search \"Packages:\" b))))"
        "(let ((output (with-output-to-string (*standard-output*)
                         (let ((*print-base* 16) (*print-radix* t)) (room t)))))
           (plusp (length output)))"
        "(let ((count 0))
           (with-output-to-string (*standard-output*) (room (progn (incf count) nil)))
           (= count 1))"
        "(let* ((before (with-output-to-string (*standard-output*) (room t)))
                (package (make-package \"ROOM-STORAGE-TEST\" :use nil)))
           (unwind-protect
               (not (string= before (with-output-to-string (*standard-output*) (room t))))
             (delete-package package)))"
        "(handler-case (room nil nil) (program-error () t))"
        "(handler-case (room 17) (type-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "ROOM mismatch: ~A" source))))

(test native-multiple-value-prog1
  (m:with-interpreter (vm)
    (dolist (source
      '("(equal (multiple-value-list (multiple-value-prog1 (values 1 2 3) (values 4 5))) '(1 2 3))"
        "(null (multiple-value-list (multiple-value-prog1 (values) (values 4 5))))"
        "(equal (multiple-value-list (multiple-value-prog1 (values 1 2))) '(1 2))"
        "(let ((source (list 1 2 3)))
           (equal (multiple-value-list (multiple-value-prog1 (values-list source) (setf (second source) 99)))
                  '(1 2 3)))"
        "(let ((source (list 1 2 3)))
           (equal (multiple-value-list (multiple-value-prog1 (apply #'values source) (setf (cdr source) nil)))
                  '(1 2 3)))"
        "(let ((object (list 1)))
           (multiple-value-bind (a b) (multiple-value-prog1 (values object object) (setf (car object) 17))
             (and (eq a object) (eq b object) (= (car a) 17))))"
        "(let ((order nil) (*gensym-counter* 77))
           (multiple-value-prog1 (values 1 2) (setq order (cons 1 order))
             (multiple-value-prog1 (values 3 4) (setq order (cons 2 order))) (setq order (cons 3 order)))
           (and (= *gensym-counter* 77) (equal order '(3 2 1))))"
        "(equal (multiple-value-list (catch 'done
                   (multiple-value-prog1 (values 1 2) (throw 'done (values 3 4))))) '(3 4))"
        "(let ((ran nil))
           (and (equal (multiple-value-list (catch 'done
                         (multiple-value-prog1 (throw 'done (values 1 2)) (setq ran t)))) '(1 2))
                (not ran)))"
        "(let ((objects (make-list 512 :initial-element 17)))
           (equal (multiple-value-list (multiple-value-prog1 (values-list objects) (values))) objects))"
        "(handler-case (multiple-value-prog1) (program-error () t))"
        "(not (null (special-operator-p 'multiple-value-prog1)))"))
      (is (eq t (m:evaluate-string vm source)) "MULTIPLE-VALUE-PROG1 mismatch: ~A" source))))

(test native-multiple-value-call
  (m:with-interpreter (vm)
    (dolist (source
      '("(equal (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 5 2))
               '(1 / 2 3 / / 2 1))"
        "(null (multiple-value-call #'list))"
        "(null (multiple-value-list (multiple-value-call #'values (values) (values))))"
        "(equal (multiple-value-list (multiple-value-call #'values (values 1 2) 3 (values 4 5))) '(1 2 3 4 5))"
        "(let ((order nil))
           (multiple-value-call (progn (setq order (cons 0 order)) #'list)
             (progn (setq order (cons 1 order)) (values 1 2))
             (progn (setq order (cons 2 order)) (values 3 4)))
           (equal order '(2 1 0)))"
        "(let ((source (list 1 2)))
           (equal (multiple-value-call #'list (values-list source)
                    (progn (setf (second source) 99) (values 3))) '(1 2 3)))"
        "(equal (multiple-value-list (catch 'done
                  (multiple-value-call #'list (throw 'done (values 7 9)) (error \"Not evaluated\")))) '(7 9))"
        "(equal (multiple-value-list (catch 'done
                  (multiple-value-call (throw 'done (values 7 9)) (error \"Not evaluated\")))) '(7 9))"
        "(let ((objects (make-list 512 :initial-element 17)))
           (equal (multiple-value-call #'list (values-list objects)) objects))"
        "(handler-case (multiple-value-call) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "MULTIPLE-VALUE-CALL mismatch: ~A" source))))

(test variable-documentation-and-core-macros
  (m:with-interpreter (vm)
    (dolist (source
      '("(defvar *documented-variable* 17 \"Original\")
         (and (= *documented-variable* 17)
              (equal (documentation '*documented-variable* 'variable) \"Original\"))"
        "(defvar *documented-variable* (error \"Should not run\") \"Updated\")
         (and (= *documented-variable* 17)
              (equal (documentation '*documented-variable* 'variable) \"Updated\"))"
        "(defvar *documented-variable*)
         (equal (documentation '*documented-variable* 'variable) \"Updated\")"
        "(defparameter *documented-parameter* 1 \"First\")
         (defparameter *documented-parameter* 2 \"Second\")
         (and (= *documented-parameter* 2)
              (equal (documentation '*documented-parameter* 'variable) \"Second\"))"
        "(defconstant documented-constant 19 \"Constant\")
         (and (= documented-constant 19)
              (equal (documentation 'documented-constant 'variable) \"Constant\"))"
        "(equal (multiple-value-list (defconstant documented-multiple-values (values 17 23)))
                '(documented-multiple-values))"
        "(setf (documentation '*documented-variable* 'variable) nil)
         (null (documentation '*documented-variable* 'variable))"
        "(null (documentation '*undocumented-variable* 'variable))"
        "(multiple-value-bind (form expanded) (macroexpand-1 '(lambda (x) x))
           (and expanded (equal form '(function (lambda (x) x)))))"
        "(equal (multiple-value-list (block nil (eval '(quote nil)) (return (values 1 2)))) '(1 2))"
        "(equal (multiple-value-list (eval (list 'block nil (macroexpand-1 '(return (values 1 2)))))) '(1 2))"
        "(eval (macroexpand-1 '(declaim (special *macro-special*))))
         (let ((*macro-special* 7)) (declare (special *macro-special*)) (= *macro-special* 7))"
        "(every #'macro-function '(lambda return declaim defstruct defclass defgeneric defmethod))"))
      (is (eq t (m:evaluate-string vm source)) "Documentation/macro mismatch: ~A" source))
    (dolist (name '(lambda return declaim defstruct defclass defgeneric defmethod))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))))

(test interpreted-load-time-value
  (m:with-interpreter (vm)
    (dolist (source
      '("(equal (multiple-value-list (load-time-value (values 1 2))) '(1))"
        "(equal (multiple-value-list (load-time-value (values))) '(nil))"
        "(progn (setf (symbol-value 'ltv-global) 7)
           (let ((ltv-global 99)) (= (load-time-value ltv-global) 7)))"
        "(let ((*ltv-dynamic* 17)) (declare (special *ltv-dynamic*))
           (= (load-time-value (symbol-value '*ltv-dynamic*)) 17))"
        "(let ((ltv-only-lexical 99))
           (handler-case (load-time-value ltv-only-lexical) (unbound-variable () t)))"
        "(flet ((ltv-hidden-function () 99))
           (handler-case (load-time-value (ltv-hidden-function)) (undefined-function () t)))"
        "(macrolet ((ltv-hidden-macro () '99))
           (handler-case (load-time-value (ltv-hidden-macro)) (undefined-function () t)))"
        "(eq (load-time-value 'ltv-literal t) 'ltv-literal)"
        "(let ((a (load-time-value (list 1) nil)) (b (load-time-value (list 1) nil)))
           (and (not (eq a b)) (equal a b)))"
        "(special-operator-p 'load-time-value)"
        "(handler-case (load-time-value) (program-error () t))"
        "(handler-case (load-time-value 1 nil nil) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "LOAD-TIME-VALUE mismatch: ~A" source))))

(test custom-setf-definitions
  (m:with-interpreter (vm)
    (dolist (source
      '("(defun custom-head (x) (car x))
         (defun update-head (x value) (setf (car x) value))
         (and (eq (defsetf custom-head update-head) 'custom-head)
              (let ((x (list 1))) (and (= (setf (custom-head x) 2) 2) (equal x '(2)))))"
        "(defmacro update-head-macro (x value) `(setf (car ,x) ,value))
         (defsetf custom-head update-head-macro \"Setter documentation\")
         (and (equal (documentation 'custom-head 'setf) \"Setter documentation\")
              (let ((x (list 1))) (and (= (setf (custom-head x) 3) 3) (equal x '(3)))))"
        "(defsetf custom-elt (index seq) (value) `(setf (elt ,seq ,index) ,value))
         (let ((x (list 1 2)) (order nil))
           (and (= (setf (custom-elt (progn (push 1 order) 1) (progn (push 2 order) x))
                         (progn (push 3 order) 9)) 9)
                (equal x '(1 9)) (equal order '(3 2 1))))"
        "(let ((op 'car))
           (defsetf lexical-head (x) (value) (return-from lexical-head `(setf (,op ,x) ,value))))
         (let ((x (list 1))) (and (= (setf (lexical-head x) 7) 7) (= (car x) 7)))"
        "(defsetf multiple-store (x) (a b) `(list ,x ,a ,b))
         (equal (setf (multiple-store 1) (values 2 3)) '(1 2 3))"
        "(defsetf empty-place () (value) `(list ,value))
         (equal (setf (empty-place) 5) '(5))"
        "(define-setf-expander delegated-head (x &environment env)
           (get-setf-expansion `(car ,x) env))
         (let ((x (list 1))) (and (= (setf (delegated-head x) 8) 8) (= (car x) 8)))"
        "(let ((x (list 1))) (macrolet ((head-alias (x) `(car ,x)))
           (and (= (setf (head-alias x) 6) 6) (= (car x) 6))))"
        "(defmacro head-with-expander (x) `(error \"Should not expand\"))
         (define-setf-expander head-with-expander (x &environment env)
           (get-setf-expansion `(car ,x) env))
         (let ((x (list 1))) (and (= (setf (head-with-expander x) 4) 4) (= (car x) 4)))"
        "(let ((x (list 1))) (macrolet ((head-with-expander (x) `(cdr ,x)))
           (setf (head-with-expander x) 2) (equal x '(1 . 2))))"
        "(define-modify-macro custom-incf (&optional (amount 1)) +)
         (let ((x (vector 0 0 0)) (i 0))
           (and (= (custom-incf (aref x (incf i)) (incf i)) 2)
                (= i 2) (= (aref x 1) 2)))"
        "(define-modify-macro custom-appendf (&rest args) append)
         (let ((x '(1))) (and (equal (custom-appendf x '(2) '(3)) '(1 2 3)) (equal x '(1 2 3))))"
        "(null (documentation (macro-function 'custom-incf) 'function))"
        "(let ((x (vector 1 2)) (order nil))
           (and (= (setf (apply #'aref (progn (push 1 order) x) (progn (push 2 order) '(1)))
                         (progn (push 3 order) 9)) 9)
                (= (aref x 1) 9) (equal order '(3 2 1))))"
        "(let ((x (make-array '(2 2) :initial-element 0)))
           (setf (apply #'aref x 1 '(1)) 7) (= (aref x 1 1) 7))"
        "(let ((x (copy-seq #*000))) (setf (apply #'bit x '(1)) 1) (equal x #*010))"
        "(defsetf keyword-place (x &optional (y 10 yp) &key (z 20 zp)) (value)
           \"Long setter\"
           `(list ,x ,y ',yp ,z ',zp ,value))
         (and (equal (setf (keyword-place 1) 7) '(1 10 nil 20 nil 7))
              (equal (setf (keyword-place 1 2 :z 3) 8) '(1 2 t 3 t 8))
              (equal (documentation 'keyword-place 'setf) \"Long setter\"))"
        "(defsetf environment-place (x &environment env) (value)
           (let ((operator (macroexpand '(local-setter-operation) env)))
             `(setf (,operator ,x) ,value)))
         (let ((x (list 1))) (macrolet ((local-setter-operation () 'car))
           (setf (environment-place x) 8) (= (car x) 8)))"
        "(let ((x (list 1)))
           (flet ((custom-head (x) (car x)) ((setf custom-head) (value x) (setf (cdr x) value)))
             (setf (custom-head x) 7))
           (equal x '(1 . 7)))"))
      (is (eq t (m:evaluate-string vm source)) "SETF definition mismatch: ~A" source))))

(test iteration-declarations-and-control
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((let ((n 3)) (dolist (x '(1 2 3) n) (declare (ignore x)) (decf n)))
        (let ((n 0)) (dotimes (i 3 n) (declare (ignore i)) (incf n)))
        (multiple-value-list (dolist (x '(1 2 3) (error "Unreachable")) (return (values x 9))))
        (multiple-value-list (dotimes (i 3 (error "Unreachable")) (return (values i 9))))
        (let ((a nil) (b nil))
          (dolist (x '(1 2 3 4) (list (reverse a) (reverse b)))
            (when (oddp x) (go odd)) (push x a) (go done) odd (push x b) done))
        (let ((a nil)) (dotimes (i 5 (reverse a))
          (when (oddp i) (go skip)) (push i a) skip))
        (let ((x 9)) (dolist (x (list x) x) (declare (ignore x))))
        (let ((x 9)) (dotimes (x (progn (setq x 3) x) x)))
        (let ((x 0) (seen nil))
          (declare (special x))
          (flet ((read-x () x))
            (dolist (x '(1 2 3) (read-x)) (declare (special x)) (push (read-x) seen)))
          (list seen x))
        (let ((x 0) (seen nil))
          (declare (special x))
          (flet ((read-x () x))
            (dotimes (x 3 (read-x)) (declare (special x)) (push (read-x) seen)))
          (list seen x))
        (let ((x :good)) (declare (special x))
          (let ((x :bad)) (dolist (e nil x) (declare (special x)))))
        (dotimes (i -10 (values i 2)) (declare (type fixnum i)))
        (dolist (x '(1 2)) (declare (integer x)))
        (dotimes (i 0) (declare (ignore i)))
        (dolist (x nil) (declare (ignore x)))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
          "Iteration mismatch: ~S" form))
    (dolist (bad '(nil 1.5 a))
      (is (eq t (m:evaluate vm `(handler-case (dotimes (i ',bad) i) (type-error () t))))))))

(test parallel-assignment-and-do
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form
      '((multiple-value-list (psetq))
        (let ((x 1) (y 2)) (list (psetq x y y x) x y))
        (let ((x 1) (y 2) (seen nil))
          (psetq x (progn (push x seen) (values y 99))
                 y (progn (push y seen) x)) (list x y (reverse seen)))
        (do ((x 1 (1+ x)) (y 0 (1+ x))) ((= y 3) x))
        (do* ((x 1 (1+ x)) (y 0 (1+ x))) ((= y 3) x))
        (let ((x 5)) (do ((x 1) (y x)) (t (list x y))))
        (let ((x 5)) (do* ((x 1) (y x)) (t (list x y))))
        (do (x (y)) (t (list x y)))
        (multiple-value-list (do () (t (values 1 2))))
        (multiple-value-list (do* () (t (values))))
        (multiple-value-list (do ((x 0 (1+ x))) (nil) (return (values x 7))))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (dolist (operator '(do do*))
      (dolist (count '(0 1 3 10))
        (dolist (tagged '(nil t))
          (let ((form `(let ((seen nil))
                         (,operator ((i 0 (1+ i))) ((>= i ,count) (values (reverse seen) i))
                           (declare (type fixnum i))
                           ,@(if tagged '((when (oddp i) (go skip)) (push i seen) skip)
                                        '((push i seen)))))))
            (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))))
      (let ((form `(let ((x :outside) (seen nil))
                     (declare (special x))
                     (flet ((read-x () x))
                       (,operator ((x 0 (1+ x))) ((= x 3) (reverse seen))
                         (declare (special x)) (push (read-x) seen))))))
        (is (equalp (eval form) (m:evaluate vm form)))))
    (dolist (source '("(psetq x)" "(psetq 3 1)" "(do ((x 1 2 3)) (t))" "(do* ((3 1)) (t))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))))

(test prog-bindings-and-control
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (prog ()))
        (multiple-value-list (prog* () (values 1 2)))
        (let ((x 5)) (prog ((x 1) (y x)) (return (list x y))))
        (let ((x 5)) (prog* ((x 1) (y x)) (return (list x y))))
        (multiple-value-list (prog () (return (values 1 2))))
        (multiple-value-list (prog* () (return (values))))
        (prog ((i 0)) (declare (fixnum i)) again (incf i) (when (< i 5) (go again)) (return i))
        (let ((x :old)) (declare (special x))
          (list (prog ((x :new)) (declare (special x)) (return (funcall (lambda () x)))) x))))
      (is (equalp (eval form) (m:evaluate vm form))))))

(test native-conditional-expansion
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (when (values t 9) (values 1 2)))
        (multiple-value-list (unless (values nil 9) (values 1 2)))
        (multiple-value-list (when nil (error "Not reached")))
        (multiple-value-list (unless t (error "Not reached")))
        (multiple-value-list (when t))
        (multiple-value-list (unless nil))
        (multiple-value-list (cond))
        (multiple-value-list (cond ((values t 9))))
        (multiple-value-list (cond ((values nil 9)) (t (values 1 2))))
        (let ((calls 0))
          (list (cond ((progn (incf calls) (values 5 9)))
                      ((error "Not reached"))) calls))
        (let ((calls 0))
          (list (cond ((progn (incf calls) nil) (error "Not reached"))
                      ((progn (incf calls) t) (incf calls) calls)) calls))
        (multiple-value-list (block done (cond (t (return-from done (values 1 2))))))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (operator '(when unless cond))
      (is (eq t (m:evaluate vm `(functionp (macro-function ',operator)))))
      (is (equal '(17) (m:evaluate-string vm (format nil
        "(let ((*macroexpand-hook* (lambda (fn form env) '(values 17))))
           (multiple-value-list (~A t)))" operator)))))))

(test native-modifier-expansion
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((x 5)) (list (incf x) (incf x 1/2) (decf x 2) x))
        (let ((x 4294967296)) (list (incf x) (decf x 4294967297) x))
        (let ((x #c(1 2))) (list (incf x #c(2 -1)) (decf x 1/2)))
        (let ((x 1) (calls 0))
          (list (incf x (progn (incf calls) (values 2 99))) x calls))
        (let ((x 1)) (list (incf x (progn (setq x 20) 3)) x))
        (let ((x 5)) (multiple-value-list (incf x (values 3 4))))
        (let ((x (list 5)) (calls 0))
          (list (incf (car (progn (incf calls) x)) 2) x calls))
        (let ((x (list 9)))
          (macrolet ((place () '(car x))) (list (decf (place) 3) x)))
        (let ((x (list 1)) (calls 0))
          (list (incf (car (progn (incf calls) x)) (progn (rplaca x 20) 3)) x calls))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (is (equal '(5 1) (m:evaluate-string vm "
        (let ((x 3) (calls 0))
          (let ((*macroexpand-hook*
                  (lambda (fn form env) (setq calls (1+ calls)) (funcall fn form env))))
            (incf x 2))
          (list x calls))")))
    (is (equal '(6 (t)) (m:evaluate-string vm "
      (defvar *modifier-variable* 4)
      (list (eval (funcall (macro-function 'incf) '(incf *modifier-variable* 2) nil))
            (multiple-value-list (nth-value 1 (macroexpand-1 '(decf *modifier-variable*)))) )")))
    (dolist (source '("(incf)" "(decf x 1 2)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (is (eq t (m:evaluate-string vm
      "(handler-case (let ((x 'bad)) (incf x)) (type-error () t))")))))

(test native-remhash-traversal
  (m:with-interpreter (vm :fuel 2000000000)
    (dolist (test '(eq eql equal equalp))
      (is (equal '(1000 499500 0 (nil nil))
        (m:evaluate vm
          `(let ((table (make-hash-table :test ',test)) (count 0) (sum 0))
             (dotimes (i 1000) (setf (gethash i table) i))
             (maphash (lambda (key value)
                        (incf count) (incf sum value)
                        (unless (remhash key table) (error "Missing current entry"))) table)
             (list count sum (hash-table-count table) (multiple-value-list (gethash 0 table))))))))
    (dolist (form
      '((let ((table (make-hash-table)))
          (setf (gethash 'a table) 1 (gethash 'b table) nil (gethash 'c table) 3)
          (list (remhash 'b table) (remhash 'a table) (remhash 'c table)
                (remhash 'a table) (hash-table-count table)))
        (let ((table (make-hash-table :test 'equal)))
          (setf (gethash (list 1 2) table) nil)
          (list (remhash (list 1 2) table) (remhash (list 1 2) table)))
        (let ((table (make-hash-table :test 'equalp)))
          (setf (gethash "abc" table) 1)
          (list (remhash "ABC" table) (hash-table-count table)))
        (let ((table (make-hash-table)))
          (setf (gethash 1 table) 1)
          (multiple-value-list (remhash 1 table)))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-maphash-callbacks
  (m:with-interpreter (vm)
    (is (equal '(3 t t (nil)) (m:evaluate-string vm "
      (let ((table (make-hash-table)) (saved nil))
        (dotimes (i 3) (setf (gethash i table) (+ i 10)))
        (maphash (lambda (&rest args) (push args saved)) table)
        (list (length saved)
          (every (lambda (args) (and (= (length args) 2) (= (cadr args) (+ 10 (car args))))) saved)
          (every (lambda (key) (member key (mapcar #'car saved))) '(0 1 2))
          (multiple-value-list (maphash (lambda (k v) (values 1 2 3)) table))))")))
    (is (equal '(1 2 3) (m:evaluate-string vm "
      (let ((table (make-hash-table)))
        (dotimes (i 3) (setf (gethash i table) i))
        (maphash (lambda (key value) (setf (gethash key table) (1+ value))) table)
        (list (gethash 0 table) (gethash 1 table) (gethash 2 table)))")))
    (is (equal '(9 0) (m:evaluate-string vm "
      (defvar *hash-callback-binding* 0)
      (let ((table (make-hash-table)))
        (setf (gethash 1 table) t)
        (list (catch 'done
                (let ((*hash-callback-binding* 9))
                  (maphash (lambda (key value) (throw 'done *hash-callback-binding*)) table)))
              *hash-callback-binding*))")))
    (dolist (source '("(maphash 1 (make-hash-table))" "(maphash #'list nil)" "(remhash 1 nil)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test development-proclaim
  (m:with-interpreter (vm)
    (is (eq :good (clos-eval vm
      "(let ((name (gensym)))
         (proclaim `(special ,name))
         (eval `(flet ((read-special () ,name))
                  (let ((,name :good)) (read-special)))))")))
    (is (equal '(nil 1) (m:evaluate-string vm
      "(let ((name (gensym)) (i 0))
         (proclaim `(special ,name))
         (proclaim (progn (incf i) '(optimize)))
         (list (boundp name) i))")))
    (is (eq t (m:evaluate-string vm
      "(let ((name (gensym)))
         (proclaim `(declaration ,name)) (proclaim `(,name)) t)")))
    (dolist (source '("(proclaim)" "(proclaim '(optimize) nil)"
                      "(proclaim nil)" "(proclaim 'special)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))
    (dolist (identifier '(optimize inline notinline type ftype integer declaration special))
      (is (eq t (m:evaluate-string vm (format nil
        "(handler-case (proclaim '(~A . bad-tail)) (error () t))" identifier)))))))

(test indexed-function-environments
  (m:with-interpreter (vm)
    (clos-eval vm "(defmacro indexed-macro () :global)
                          (defvar *indexed-special* 1)
                          (defun indexed-special-reader () *indexed-special*)")
    (is (equal '(:global :local :function :global)
      (clos-eval vm "(list (indexed-macro)
        (let ((x 1)) (macrolet ((indexed-macro () :local)) (indexed-macro)))
        (let ((x 2)) (flet ((indexed-macro () :function)) (indexed-macro)))
        (indexed-macro))")))
    (is (equal '(t t nil t)
      (clos-eval vm "(macrolet ((visible (&environment env)
          (not (null (macro-function 'indexed-macro env)))))
        (list (visible)
          (let ((x 1)) (visible))
          (flet ((indexed-macro () nil)) (visible))
          (macrolet ((indexed-macro () nil)) (visible))))")))
    (is (eq t (clos-eval vm "(labels ((even-local (n)
          (if (= n 0) t (odd-local (1- n))))
        (odd-local (n) (if (= n 0) nil (even-local (1- n)))))
        (let ((x 1) (y 2)) (even-local 12)))")))
    (is (equal '(9 1)
      (clos-eval vm "(list (let ((*indexed-special* 9)) (indexed-special-reader))
                                  (indexed-special-reader))")))
    (clos-eval vm "(defmacro indexed-macro () :changed)")
    (is (eq :changed (clos-eval vm "(indexed-macro)")))
    (clos-eval vm "(defun indexed-macro () :function)")
    (is (null (clos-eval vm "(macro-function 'indexed-macro)")))
    (is (eq :function (clos-eval vm "(indexed-macro)")))
    (clos-eval vm "(fmakunbound 'indexed-macro)")
    (is (null (clos-eval vm "(macro-function 'indexed-macro)")))))

(test development-directory
  (m:with-interpreter (vm)
    (dolist (file '("dir-tests/a.lisp" "dir-tests/alpha.lisp" "dir-tests/a.wasm"
                    "dir-tests/archive.test.lisp" "dir-tests/README" "dir-tests/λ.lisp"
                    "dir-tests/sub/b.lisp" "dir-tests/sub/deep/c.lisp" "elsewhere/a.lisp"))
      (m:evaluate-string vm (format nil "(register-source-file ~S \"\")" file)))
    (dolist (entry
      '(("dir-tests/a.lisp" "/dir-tests/a.lisp")
        ("dir-tests/missing.lisp")
        ("dir-tests/*.lisp" "/dir-tests/a.lisp" "/dir-tests/alpha.lisp"
          "/dir-tests/archive.test.lisp" "/dir-tests/λ.lisp")
        ("dir-tests/a*.lisp" "/dir-tests/a.lisp" "/dir-tests/alpha.lisp" "/dir-tests/archive.test.lisp")
        ("dir-tests/*" "/dir-tests/README")
        ("dir-tests/*.*" "/dir-tests/README" "/dir-tests/a.lisp" "/dir-tests/a.wasm"
          "/dir-tests/alpha.lisp" "/dir-tests/archive.test.lisp" "/dir-tests/λ.lisp")
        ("dir-tests/*/*.lisp" "/dir-tests/sub/b.lisp")
        ("dir-tests/**/*.lisp" "/dir-tests/a.lisp" "/dir-tests/alpha.lisp"
          "/dir-tests/archive.test.lisp" "/dir-tests/sub/b.lisp" "/dir-tests/sub/deep/c.lisp"
          "/dir-tests/λ.lisp")
        ("dir-tests/**/a.lisp" "/dir-tests/a.lisp")
        ("/dir-tests/sub/../a.lisp" "/dir-tests/a.lisp")))
      (is (equal (sort (copy-list (cdr entry)) #'string<)
                 (sort (m:evaluate-string vm
                   (format nil "(mapcar #'namestring (directory ~S))" (car entry))) #'string<))
          "Directory mismatch for ~S" (car entry)))
    (is (equal '(t t t)
      (m:evaluate-string vm "
        (let ((p (make-pathname :directory '(:relative :wild :wild-inferiors)
                                :name :wild :type :wild)))
          (list (equal (pathname-directory p) '(:relative :wild :wild-inferiors))
                (eq (pathname-name p) :wild) (eq (pathname-type p) :wild)))")))
    (is (equal "*.lisp" (m:evaluate-string vm "
      (let ((*default-pathname-defaults* (pathname \"dir-tests/base.lisp\")))
        (namestring (make-pathname :name :wild :type \"lisp\")))")))
    (is (equal '("/dir-tests/sub/b.lisp") (m:evaluate-string vm "
      (let ((*default-pathname-defaults* (pathname \"dir-tests/\")))
        (mapcar #'namestring (directory
          (make-pathname :directory '(:relative :wild) :name :wild :type \"lisp\"))))")))
    (is (equal '("/dir-tests/a.lisp") (m:evaluate-string vm "
      (let ((*default-pathname-defaults* (pathname \"dir-tests/base.lisp\")))
        (mapcar #'namestring (directory \"a\")))")))
    (is (eq t (m:evaluate-string vm "
      (let ((*default-pathname-defaults* (pathname \"dir-tests/\")))
        (every (lambda (p) (and (pathnamep p) (equal p (truename p)) (equal p (probe-file p))))
               (directory \"**/*.*\")))")))
    (is (equal '(t t 4) (m:evaluate-string vm "
      (let ((a (directory \"dir-tests/*.lisp\")) (b (directory \"dir-tests/*.lisp\")))
        (let ((fresh (and (not (eq a b)) (not (eq (cdr a) (cdr b))))))
          (setf (cdr a) nil)
          (list fresh (= (length a) 1) (length b))))")))
    (is (equal '("/dir-tests/a.lisp") (m:evaluate-string vm "
      (let ((p (car (directory \"dir-tests/a.lisp\"))))
        (setf (char (namestring p) 1) #\\z)
        (mapcar #'namestring (directory \"dir-tests/a.lisp\")))")))
    (is (equal '(1 t) (m:evaluate-string vm "
      (list (length (multiple-value-list (directory \"dir-tests/a.lisp\")))
            (null (directory \"dir-tests/missing\" :unused t :allow-other-keys t)))")))
    (is (eq t (m:evaluate-string vm "
      (handler-case (directory 1) (type-error (c)
        (not (typep (type-error-datum c) (type-error-expected-type c)))))")))
    (dolist (source '("(directory)" "(directory \"x\" :unknown t)" "(directory \"x\" :unknown)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (program-error () t))")))))))

(test pathname-string-designators
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm
      "(let ((backing (copy-seq \"XXpathname.txtYYY\")))
         (every (lambda (text)
                  (let ((path (pathname text)))
                    (and (equal (namestring path) \"pathname.txt\")
                         (equal (pathname-name path) \"pathname\")
                         (equal (pathname-type path) \"txt\")
                         (eq path (pathname path)))))
           (list \"pathname.txt\"
             (make-array 15 :element-type 'base-char :initial-contents \"pathname.txtXXX\" :fill-pointer 12)
             (make-array 12 :element-type 'base-char :initial-contents \"pathname.txt\" :adjustable t)
             (make-array 15 :element-type 'character :initial-contents \"pathname.txtXXX\" :fill-pointer 12)
             (make-array 12 :element-type 'character :displaced-to backing :displaced-index-offset 2))))")))))

(test development-compile-file-pathname
  (m:with-interpreter (vm)
    (dolist (entry
      '(("(compile-file-pathname \"src/program.lisp\")" "src/program.wasm")
        ("(compile-file-pathname (pathname \"src/program.lsp\"))" "src/program.wasm")
        ("(compile-file-pathname \"program\")" "program.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :output-file \"new\")" "src/new.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :output-file \"new.bin\")" "src/new.bin")
        ("(compile-file-pathname \"src/program.lisp\" :output-file \"build/\")" "src/build/program.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :output-file (pathname \"/build/\"))" "/build/program.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :output-file \"\")" "src/program.wasm")
        ("(let ((*default-pathname-defaults* (pathname \"/project/default.lisp\")))
            (compile-file-pathname \"src/program.lisp\"))" "/project/src/program.wasm")
        ("(let ((*default-pathname-defaults* (pathname \"/project/default.lisp\")))
            (compile-file-pathname \"\"))" "/project/default.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :verbose nil :print t :external-format :default :other 1)"
         "src/program.wasm")
        ("(compile-file-pathname \"src/program.lisp\" :output-file \"first\" :output-file \"last\")"
         "src/first.wasm")))
      (is (equal (second entry) (m:evaluate-string vm
        (format nil "(namestring ~A)" (first entry))))))
    (is (equal '(t 1 nil nil) (m:evaluate-string vm "
      (let* ((values (multiple-value-list (compile-file-pathname \"missing-source.lisp\")))
             (output (car values)))
        (list (pathnamep output) (length values) (probe-file \"missing-source.lisp\")
              (probe-file output)))")))
    (dolist (source '("(compile-file-pathname 1)"
                      "(compile-file-pathname \"source.lisp\" :output-file nil)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test development-compile-file-path-variables
  (m:with-interpreter (vm)
    (is (equal '(t t nil nil t t) (m:evaluate-string vm "
      (list (boundp 'cl:*compile-file-pathname*) (boundp 'cl:*compile-file-truename*)
            cl:*compile-file-pathname* cl:*compile-file-truename*
            (eq (nth-value 1 (find-symbol \"*COMPILE-FILE-PATHNAME*\" \"COMMON-LISP\")) :external)
            (eq (nth-value 1 (find-symbol \"*COMPILE-FILE-TRUENAME*\" \"COMMON-LISP\")) :external))")))
    (is (equal '((nil nil "/compile-paths.lisp") nil nil) (m:evaluate-string vm "
      (register-source-file \"compile-paths.lisp\"
        \"(defparameter *compile-path-observation*
           (list *compile-file-pathname* *compile-file-truename*
                 (namestring (or *compile-file-truename* *load-truename*))))\")
      (load \"compile-paths.lisp\")
      (list *compile-path-observation* *compile-file-pathname* *compile-file-truename*)")))))

(test development-tailp
  (m:with-interpreter (vm)
    (is (equal '(t t t nil t nil (1 2 3))
      (m:evaluate-string vm "
        (let ((x (list 1 2 3)))
          (list (tailp x x) (tailp (cdr x) x) (tailp nil x)
                (tailp '(2 3) x) (tailp nil nil) (tailp 3 nil) x))")))
    (is (equal '(t nil t nil)
      (m:evaluate-string vm "
        (let ((string (copy-seq \"tail\")))
          (list (tailp 10203040506070 (list* 1 2 (1- 10203040506071)))
                (tailp nil '(1 2 . 3))
                (tailp string (list* 1 2 string))
                (tailp string (list* 1 2 (copy-seq string)))))")))))

(test development-fixnum-limits
  (m:with-interpreter (vm)
    (is (equal '(t t nil nil t t)
      (m:evaluate-string vm "
        (list (typep most-positive-fixnum 'fixnum)
              (typep most-negative-fixnum 'fixnum)
              (typep (1+ most-positive-fixnum) 'fixnum)
              (typep (1- most-negative-fixnum) 'fixnum)
              (typep (1+ most-positive-fixnum) 'bignum)
              (typep (1- most-negative-fixnum) 'bignum))")))
    (is (equal '(t t)
      (m:evaluate-string vm "
        (list (handler-case (set 'most-positive-fixnum 0) (program-error () t))
              (handler-case (set 'most-negative-fixnum 0) (program-error () t)))")))))

(test development-copy-alist
  (m:with-interpreter (vm)
    (is (equal '(t t t t t t t t)
      (m:evaluate-string vm "
        (let* ((key (list 1)) (value (list 2))
               (alist (list (cons key value) nil (cons 3 4)))
               (copy (copy-alist alist))
               (checks (list (not (eq alist copy))
                             (not (eq (car alist) (car copy)))
                             (eq (caar copy) key) (eq (cdar copy) value)
                             (null (cadr copy))
                             (not (eq (caddr alist) (caddr copy))))))
          (rplaca (car copy) 99)
          (rplacd copy nil)
          (append checks (list (equal alist '(((1) 2) nil (3 . 4)))
                               (equal copy '((99 2))))))")))
    (is (null (m:evaluate-string vm "(copy-alist nil)")))
    (is (equal '(t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (alist)
                  (handler-case (copy-alist alist)
                    (type-error (c)
                      (not (typep (type-error-datum c) (type-error-expected-type c))))))
                '(1 ((1 . 2) . 3) (1)))")))))

(test development-butlast
  (m:with-interpreter (vm)
    (is (equal '((1 2) (1 2 3) nil nil)
      (m:evaluate-string vm "
        (list (butlast '(1 2 3 . 4)) (butlast '(1 2 3 . 4) 0)
              (butlast '(1 2 3 . 4) 3)
              (butlast '(1 2 3) 1000000000000000000000000000000))")))
    (is (equal '(t t (1 2 3 . 4))
      (m:evaluate-string vm "
        (let* ((x (list* 1 2 3 4)) (tail (cdr x)) (copy (butlast x 0)))
          (list (not (eq x copy)) (not (eq tail (cdr copy))) x))")))
    (is (equal '(t t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (n)
                  (handler-case (butlast '(1 2 3) n)
                    (type-error (c) (equal (type-error-datum c) n))))
                '(-1 a 1.0 #\\w))")))
    (is (eq t (m:evaluate-string vm "
      (handler-case (butlast 'a 0) (type-error () t))")))))

(test development-nbutlast
  (m:with-interpreter (vm)
    (is (equal '(t t (1 2 3) (4 5))
      (m:evaluate-string vm "
        (let* ((x (list 1 2 3 4 5)) (second (cdr x)) (fourth (cdddr x)))
          (let ((result (nbutlast x 2)))
            (list (eq result x) (eq (cdr x) second) result fourth)))")))
    (is (equal '(nil nil (1 2 3 . 4))
      (m:evaluate-string vm "
        (let ((x (list* 1 2 3 4)))
          (list (nbutlast x 3) (nbutlast x 1000000000000000000000000000000) x))")))
    (is (equal '(nil nil (1 2) (1 2 3))
      (m:evaluate-string vm "
        (list (nbutlast nil) (nbutlast (list 1))
              (nbutlast (list* 1 2 3 4)) (nbutlast (list* 1 2 3 4) 0))")))
    (is (equal '(t t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (n)
                  (handler-case (nbutlast '(1 2 3) n)
                    (type-error (c) (equal (type-error-datum c) n))))
                '(-1 a 1.0 #\\w))")))
    (is (eq t (m:evaluate-string vm "
      (handler-case (nbutlast 1) (type-error () t))")))))

(test development-nreconc
  (m:with-interpreter (vm)
    (is (equal '(t t t (3 2 1 4 5) (4 5))
      (m:evaluate-string vm "
        (let* ((x (list 1 2 3)) (second (cdr x)) (last (cddr x))
               (tail (list 4 5)) (result (nreconc x tail)))
          (list (eq result last) (eq (cdr result) second) (eq (cdddr result) tail)
                result tail))")))
    (is (equal '(10 (2 1 . 10) nil)
      (m:evaluate-string vm "
        (list (nreconc nil 10) (nreconc (list 1 2) 10) (nreconc nil nil))")))
    (is (equal '(t t)
      (m:evaluate-string vm "
        (mapcar (lambda (x)
                  (handler-case (nreconc x nil)
                    (type-error (c)
                      (not (typep (type-error-datum c) (type-error-expected-type c))))))
                (list 'a (cons 1 2)))")))))

(test development-revappend
  (m:with-interpreter (vm)
    (is (equal '(t t t (3 2 1 4 5) (1 2 3) (4 5))
      (m:evaluate-string vm "
        (let* ((x (list 1 2 3)) (second (cdr x)) (tail (list 4 5))
               (result (revappend x tail)))
          (list (not (eq (cddr result) x)) (eq (cdr x) second)
                (eq (cdddr result) tail) result x tail))")))
    (is (equal '(10 (2 1 . 10) nil)
      (m:evaluate-string vm "
        (list (revappend nil 10) (revappend '(1 2) 10) (revappend nil nil))")))
    (is (equal '(t t)
      (m:evaluate-string vm "
        (mapcar (lambda (x)
                  (handler-case (revappend x nil)
                    (type-error (c)
                      (not (typep (type-error-datum c) (type-error-expected-type c))))))
                '(a (1 . 2)))")))))

(test development-list-length
  (m:with-interpreter (vm)
    (is (equal '(0 1 2 3 4 5)
      (m:evaluate-string vm "
        (mapcar #'list-length '(nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))")))
    (is (equal '(nil nil nil)
      (m:evaluate-string vm "
        (let ((x (list 1)) (y (list 2 3)))
          (rplacd x x)
          (rplacd (cdr y) y)
          (list (list-length x) (list-length y)
                (list-length (list* 4 5 6 y))))")))
    (is (equal '(t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (x)
                  (handler-case (list-length x)
                    (type-error (c)
                      (not (typep (type-error-datum c) (type-error-expected-type c))))))
                '(1 (1 . 2) (1 2 . 3)))")))))

(test development-nthcdr
  (m:with-interpreter (vm)
    (is (equal '(t 4 nil nil)
      (m:evaluate-string vm "
        (let ((x '(1 2 3 . 4)))
          (list (eq (nthcdr 0 x) x) (nthcdr 3 x)
                (nthcdr 1000000000000000000000000000000 '(1 2 3))
                (nth 1000000000000000000000000000000 '(1 2 3))))")))
    (is (equal '(t t t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (n)
                  (handler-case (nthcdr n '(1 2 3))
                    (type-error (c) (equal (type-error-datum c) n))))
                '(-10 a 1.0 #\\w -1000000000000000000000000000000))")))
    (is (equal '(t t)
      (m:evaluate-string vm "
        (mapcar (lambda (x)
                  (handler-case (nthcdr 3 x)
                    (type-error (c)
                      (not (typep (type-error-datum c) (type-error-expected-type c))))))
                '(a (1 . 2)))")))))

(test development-last
  (m:with-interpreter (vm)
    (is (equal '(1 . 2) (m:evaluate-string vm "(last (cons 1 2) 2)")))
    (is (= 4 (m:evaluate-string vm "(last '(1 2 3 . 4) 0)")))
    (is (equal '(t t)
      (m:evaluate-string vm "
        (let ((x '(a b c . d)))
          (list (eq (last x 1000000000000000000000000000000) x)
                (eq (last x 2) (cdr x))))")))
    (is (equal '(t t t t t)
      (m:evaluate-string vm "
        (mapcar (lambda (n)
                  (handler-case (last '(a b c) n)
                    (type-error (c) (equal (type-error-datum c) n))))
                '(-1 a 1.0 -1.0 #\\w))")))
    (is (eq t (m:evaluate-string vm "
      (handler-case (last 'a 0) (type-error () t))")))))

(test logical-macro-expansions
  (m:with-interpreter (vm)
    (dolist (form '((and) (or) (and (values)) (or (values))
                    (and (values nil 2) (error "Unreachable"))
                    (or (values 1 2) (error "Unreachable"))
                    (and 1 (values 2 3)) (or nil (values 2 3))
                    (and (values t 17) (values 3 5))
                    (or (values nil 17) (values 3 5))))
      (is (equalp (multiple-value-list (eval form))
                  (m:evaluate vm `(multiple-value-list (eval (macroexpand-1 ',form))))))
      (is (equalp (multiple-value-list (eval form))
                  (multiple-value-list (m:evaluate vm form)))))
    (dolist (operator '(and or))
      (is (eq t (m:evaluate vm `(functionp (macro-function ',operator)))))
      (dolist (args '(nil ((and)) ((and) nil nil)))
        (is (eq t (m:evaluate vm
          `(handler-case (apply (macro-function ',operator) ',args) (program-error () t)))))))
    (is (equal '(2 2) (m:evaluate-string vm
      "(defmacro expand-logical (form) (macroexpand-1 form))
       (let ((i 0)) (list (expand-logical (and (incf i) (incf i))) i))")))
    (is (equal '(1 1) (m:evaluate-string vm
      "(let ((i 0)) (list (expand-logical (or (incf i) (incf i))) i))")))
    (is (= 42 (m:evaluate-string vm "(macrolet ((and (&rest forms) 42)) (and 1 2))")))
    (is (= 43 (m:evaluate-string vm "(macrolet ((or (&rest forms) 43)) (or 1 2))")))))

(test development-source-macros
  (m:with-interpreter (vm)
    (is (equal '(3 3)
      (m:evaluate-string vm "(defmacro twice (x) `(list ,x ,x)) (twice (+ 1 2))")))
    (is (= 42 (m:evaluate-string vm "
      (defmacro expand-here (form &environment env) (macroexpand form env))
      (macrolet ((local () 42)) (expand-here (local)))")))
    (is (equal '(1 2 3)
      (m:evaluate-string vm "(let ((tail '(2 3))) `(1 ,@tail))")))))

(test development-opaque-objects
  (m:with-interpreter (vm)
    (let ((table (m:evaluate-string vm "(make-hash-table :test 'equal)"))
          (path (m:evaluate-string vm "(pathname \"source.lisp\")")))
      (is (m:guest-opaque-object-p table))
      (is (m:guest-opaque-object-p path))
      (is (eq table (m:evaluate vm table)))
      (is (eq path (m:evaluate vm path)))
      (is (= 42 (m:evaluate vm `(setf (gethash "key" ,table) 42))))
      (is (= 42 (m:evaluate vm `(gethash "key" ,table))))
      (is (equal "source.lisp" (m:evaluate vm `(namestring ,path)))))))

(test development-constant-forms
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form '(nil t :keyword 0 -1 1/3 1.25d0 #c(1 2) #\λ "aλ🙂" #(1 2) #*101
                   (quote (a b)) (quote variable) (quote nil) (quote (quote x))))
      (is (eq t (m:evaluate vm `(not (null (constantp ',form)))))))
    (dolist (form '(unbound-constant-test-variable (ordinary-function 1) (setq marker t)))
      (is (null (m:evaluate vm `(constantp ',form)))))
    (is (eq t (m:evaluate-string vm "(defconstant constant-form-value 42) (constantp 'constant-form-value)")))
    (is (eq t (m:evaluate vm '(let ((marker nil))
      (list (constantp '(setq marker t)) marker)
      (null marker)))))
    (is (eq t (m:evaluate vm '(constantp ''x nil))))
    (is (equal '(t) (m:evaluate vm '(multiple-value-list (constantp 42))))))
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (form '((constantp) (constantp 1 nil nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test selection-macros
  (m:with-interpreter (vm)
    (dolist (form
      '((typecase 1 (integer (values :good 2)) (t :bad))
        (typecase 1 (symbol :bad))
        (typecase :x)
        (typecase 1 (t :good) (otherwise :bad))
        (typecase 1 (nil :bad) ((integer 0 2) :good))
        (typecase 1 (integer))
        (etypecase :x (symbol (values)))
        (ecase t (otherwise :bad) ((t) (values :good 2)))
        (ecase 'otherwise (otherwise :good))
        (ecase nil ((nil) :good) (nil :bad))
        (ecase 2 ((1 2) :first) ((2 3) :second))
        (let ((i 0)) (list (typecase (incf i) (integer i)) i))
        (let ((i 0)) (list (ecase (incf i) (1 i)) i))
        (handler-case (etypecase 1 (symbol 2))
          (type-error (c) (list (type-error-datum c)
            (typep 'a (type-error-expected-type c)) (typep 1 (type-error-expected-type c)))))
        (handler-case (ecase 1 (a 2) ((b c) 3))
          (type-error (c) (list (type-error-datum c)
            (typep 'b (type-error-expected-type c)) (typep 1 (type-error-expected-type c)))))
        (handler-case (etypecase 1) (type-error (c) (typep 1 (type-error-expected-type c))))
        (handler-case (ecase nil (nil :bad)) (type-error () :good))
        (let ((x 1) (attempts 0))
          (list (handler-bind ((type-error (lambda (c)
                    (incf attempts) (store-value (if (= attempts 1) 2.5 :good) c))))
                  (ctypecase x (symbol (values x attempts)))) x attempts))
        (let ((x (list :bad)) (attempts 0))
          (list (handler-bind ((type-error (lambda (c)
                    (incf attempts) (store-value :good c))))
                  (ccase (car x) (:good (values 1 2)))) x attempts))
        (let ((x :bad))
          (handler-case (ccase x (:good 1)) (type-error () :escaped))
          (list x (find-restart 'store-value)))
        (let ((x :bad))
          (list (handler-bind ((type-error (lambda (c) (store-value 7 c))))
                  (ctypecase x (integer (find-restart 'store-value)))) x))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
        "Selection mismatch for ~S" form))
    (dolist (name '(typecase etypecase ecase ccase ctypecase))
      (is (eq t (m:evaluate vm `(functionp (macro-function ',name)))))
      (is (eq :shadowed (m:evaluate vm
        `(macrolet ((,name (&rest ignored) (declare (ignore ignored)) :shadowed))
           (,name (error "unreachable"))))))
      (is (eq t (m:evaluate vm `(handler-case (macroexpand-1 '(,name)) (program-error () t))))))))

(test parallel-generalized-assignments
  (m:with-interpreter (vm)
    (dolist (form
      '((psetf)
        (rotatef)
        (let ((x 1) (y 2)) (list (psetf x y y x) x y))
        (let ((x 1) (y 2) (z 3)) (list (rotatef x y z) x y z))
        (let ((x 1) (y 2)) (list (shiftf x y 3) x y))
        (let ((x 1)) (list (rotatef x) x))
        (let ((x (vector 0 1 2 3 4)) (i 0))
          (psetf (aref x (incf i)) (aref x (incf i)) (aref x (incf i)) (aref x (incf i)))
          (list x i))
        (let ((x (vector 0 1 2 3)) (i -1))
          (rotatef (aref x (incf i)) (aref x (incf i)) (aref x (incf i))) (list x i))
        (let ((x (list 1 2)) (y (list 3 4)))
          (rotatef (car x) (cadr y)) (list x y))
        (let (a b c d)
          (list (multiple-value-list (setf (values a b) (values 1 2 3))) a b
            (multiple-value-list (setf (values c d) (values 4))) c d))
        (let ((a 1) (b 2) (c 3) (d 4))
          (list (psetf (values a b) (values c d) (values c d) (values a b)) a b c d))
        (let ((a 1) (b 2) (c 3))
          (list (rotatef (values a b) c) a b c))
        (let ((a 1) (b 2) (c 3) (d 4))
          (list (rotatef (values a b) (values c d)) a b c d))
        (let ((a 1) (b 2) (c 3))
          (list (multiple-value-list (shiftf (values a b) c (values 4 5))) a b c))
        (let ((a 1) (b 2) (c 3))
          (list (multiple-value-list (setf (values (values a b) c) (values 4 5))) a b c))
        (let ((a 1) (b 2)) (list (multiple-value-setq (a b) (values 3)) a b))
        (multiple-value-list (multiple-value-setq () (values 1 2)))
        (multiple-value-list (setf (values) (values 1 2)))
        (let ((a 0) (b 0)) (list (multiple-value-list (multiple-value-setq (a b) (values))) a b))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
        "Parallel assignment mismatch for ~S" form))
    (is (equal '((2) (1)) (m:evaluate-string vm
      "(let ((a 1) (b 2))
         (macrolet ((my-place (x) `(car ,x)))
           (let ((x (list a)) (y (list b)))
             (psetf (my-place x) b (my-place y) a) (list x y))))")))
    (dolist (form '((psetf x) (shiftf) (shiftf x) (multiple-value-setq (1) 2)))
      (is (eq t (m:evaluate vm `(handler-case (macroexpand-1 ',form) (program-error () t))))))))

(test generalized-assignment-values
  (m:with-interpreter (vm)
    (dolist (form
      '((let (x) (multiple-value-list (setf x (values 1 2))))
        (multiple-value-list (setf))
        (let (x y) (multiple-value-list (setf x 0 (values x y) (values 1 2 3))))
        (let ((x 1) (y 2)) (list (psetf x (progn (setq y 9) y) y x) x y))
        (let ((x 1) (y 2)) (list (multiple-value-setq (x x) (values 3 4)) x y))
        (let ((x 1) (y 2)) (declare (special x y)) (list (rotatef x y) x y))
        (let ((x 1) (y 2)) (declare (type integer x y))
          (list (shiftf x y 3) x y))
        (let ((x 1)) (list (multiple-value-list (multiple-value-setq () (incf x))) x))))
      (is (equalp (eval form) (m:evaluate vm form)) "Assignment value mismatch for ~S" form))
    (is (equal '(42 99) (m:evaluate-string vm
      "(defun assignment-value-reader (cell) (car cell))
       (defun (setf assignment-value-reader) (value cell)
         (rplaca cell value) (values value 99))
       (let ((cell (list 0)))
         (multiple-value-list (setf (assignment-value-reader cell) 42)))")))))

(test the-multiple-values-and-places
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (the (values) (values)))
        (multiple-value-list (the (values) (values 1 2 3)))
        (multiple-value-list (the integer (values 1 2 3)))
        (multiple-value-list (the symbol (values)))
        (multiple-value-list (the (values integer symbol) (values 1)))
        (multiple-value-list (the (values integer &optional fixnum) (values 1)))
        (multiple-value-list (the (values &optional fixnum) (values)))
        (multiple-value-list (the (values &rest integer) (values 1 2 3)))
        (multiple-value-list (the (values &rest integer) (values)))
        (multiple-value-list (the (values integer &optional symbol &rest real) (values 1 nil 2 3/4)))
        (multiple-value-list (the (satisfies plusp) (values 1 2 3)))
        (multiple-value-list (the (values (satisfies plusp) (satisfies numberp)) (values 1 2 3)))
        (let ((n 0)) (list (multiple-value-list (the (values integer integer) (values (incf n) (incf n)))) n))
        (let ((x 1)) (list (setf (the integer x) 2) x))
        (let ((x (list 1 2)) (n 0))
          (list (setf (the integer (car (progn (incf n) x))) (incf n)) x n))
        (let ((x 1)) (list (incf (the integer x)) x))
        (let ((x 1) (y 2)) (list (rotatef (the integer x) (the integer y)) x y))
        (let ((x nil) (y nil))
          (list (multiple-value-list (setf (the (values integer symbol) (values x y)) (values 3 'a))) x y))
        (let ((x (list 1 2))) (list (shiftf (the integer (car x)) (the integer (cadr x)) 3) x))))
      (is (equalp (eval form) (m:evaluate vm form)) "THE mismatch for ~S" form))
    ;; Invalid declarations have undefined consequences; retain checked-mode
    ;; diagnostics, including for secondary values and values passed to SETF.
    (dolist (form '((the integer 'bad)
                    (the (values integer integer) (values 1 'bad))
                    (the (values integer integer) (values 1))
                    (the (values &rest integer) (values 1 2 'bad))
                    (let ((x 1)) (setf (the integer x) 'bad))))
      (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t))))))))

(test progv-dynamic-bindings
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (progv nil nil))
        (multiple-value-list (progv '(x) '(1) (values (symbol-value 'x) 2 3)))
        (let ((x 0)) (progv '(x) '(1) (list x (symbol-value 'x))))
        (let ((x 0)) (declare (special x)) (list (progv '(x) '(1) x) x))
        (let ((x 0)) (declare (special x))
          (list (progv '(x) nil (list (boundp 'x) (setq x 2) x)) x))
        (progv '(x y z) '(1) (list (boundp 'x) (boundp 'y) (boundp 'z)))
        (progv '(x) '(1 2 3) (symbol-value 'x))
        (progv '(x) '(1) (progv '(x) '(2) (symbol-value 'x)) (symbol-value 'x))
        (let ((n 0)) (progv (progn (incf n) '(x)) (list (incf n)) (list n (symbol-value 'x))))
        (progv '(x y) '(1) (locally (declare (special x y) (type nil y)) (list x (boundp 'y))))
        (let ((x 0)) (declare (special x))
          (list (catch 'done (progv '(x) '(1) (throw 'done x))) x))
        (let ((x 0)) (declare (special x))
          (list (handler-case (progv '(x) '(1) (error 'simple-error)) (error () x)) x))
        (let ((x 0) (seen nil)) (declare (special x))
          (list (catch 'done (progv '(x) '(1) (unwind-protect (throw 'done x) (push x seen)))) seen x))
        (let ((x 0)) (declare (special x)) (progv '(x) '(1) (makunbound 'x)) x)
        (let ((f (progv '(x) '(1) (function (lambda () (symbol-value 'x)))))) (progv '(x) '(2) (funcall f)))
        (let ((old *package*))
          (list (progv '(*package*) (list (find-package :cl)) (package-name *package*)) (eq old *package*)))
        (let ((x 0)) (declare (special x))
          (progv '(x) nil (handler-case x (unbound-variable (c) (eq (cell-error-name c) 'x)))))))
      (is (equalp (eval form) (m:evaluate vm form)) "PROGV mismatch for ~S" form))
    ;; Exercise the interpreter's unbound package extension without asking
    ;; the host to unbind its package-locked COMMON-LISP variable.
    (is (equal '(nil t) (m:evaluate vm '(list (progv '(*package*) nil (boundp '*package*)) (boundp '*package*)))))
    (is (eq t (m:evaluate vm '(progv '(*package*) nil (handler-case *package* (unbound-variable () t))))))
    (dolist (form '((progv) (progv nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (dolist (form '((progv 1 nil) (progv nil 1) (progv '(x . y) nil) (progv nil '(1 . 2))))
      (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t))))))))

(test lexical-symbol-macros
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (symbol-macrolet ()))
        (multiple-value-list (symbol-macrolet ((x (values 1 2 3))) x))
        (symbol-macrolet ((x 1)) (symbol-macrolet ((x 2)) x))
        (let ((x 10)) (symbol-macrolet ((y x)) (list y (let ((y 20)) y) y)))
        (symbol-macrolet ((x 1) (y x)) y)
        (symbol-macrolet ((x '(x y))) x)
        (symbol-macrolet ((x 1)) (flet ((x () 2)) (list x (x))))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a))) (setq x 3) (incf x) (list x a)))
        (let ((a (vector 10)) (i 0))
          (symbol-macrolet ((x (aref a (progn (incf i) 0)))) (list (setq x (incf i)) i (aref a 0))))
        (let ((a (vector 10)) (i 0))
          (symbol-macrolet ((x (aref a (progn (incf i) 0)))) (list (incf x (incf i)) i (aref a 0))))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a)) (y (cadr a))) (psetq x y y x) a))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a)) (y (cadr a))) (psetf x y y x) a))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a)) (y (cadr a))) (rotatef x y) a))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a)) (y (cadr a))) (list (shiftf x y 3) a)))
        (let ((a (list 1 2))) (symbol-macrolet ((x (car a)) (y (cadr a))) (multiple-value-setq (x y) (values 3 4)) a))
        (let ((a (list 1 2))) (symbol-macrolet ((x (values (car a) (cadr a)))) (setf x (values 3 4)) a))
        (let ((a nil)) (symbol-macrolet ((x (getf a :items))) (list (pushnew 1 x) (pushnew 1 x) a)))
        (let ((a (list (list :a 1)))) (symbol-macrolet ((x (car a))) (list (remf x :a) a)))
        (let ((a 1)) (symbol-macrolet ((x a)) (declare (type integer x)) (list (incf x) a)))
        (let ((a 1)) (symbol-macrolet ((x a)) (list (locally (declare (type integer x)) x) x)))
        (symbol-macrolet ((x 1)) (funcall (let ((x 2)) (lambda () x))))
        (let ((a 1)) (symbol-macrolet ((x a)) (funcall (lambda () x))))
        (symbol-macrolet ((x 1)) (list 'x (boundp 'x) (macro-function 'x)))
        (macrolet ((expand-it (x &environment env) (list 'quote (multiple-value-list (macroexpand x env)))))
          (symbol-macrolet ((x 1)) (list (expand-it x) (let ((x 2)) (expand-it x)))))
        (macrolet ((test-hook (&environment env)
                     (let ((count 0) (result nil))
                       (let ((*macroexpand-hook* (lambda (fn form environment)
                               (setq count (1+ count)) (funcall fn form environment))))
                         (setq result (macroexpand-1 'x env)))
                       (list 'quote (list result count)))))
          (symbol-macrolet ((x 17)) (test-hook)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Symbol macro mismatch for ~S" form))
    ;; MACROEXPAND-1 takes one step. ECL expands this symbol-macro chain fully.
    (is (equal '(y t) (m:evaluate vm
      '(macrolet ((expand-it (x &environment env) (list 'quote (multiple-value-list (macroexpand-1 x env)))))
         (symbol-macrolet ((x y) (y 3)) (expand-it x))))))
    (dolist (form '((symbol-macrolet ((x 1)) (declare (special x)) x)
                    (symbol-macrolet ((t 1)) t)
                    (symbol-macrolet ((*package* 1)) *package*)
                    (symbol-macrolet ((x)) x) (symbol-macrolet ((x 1 2)) x)
                    (let ((a (list 1))) (psetq (car a) 2))))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test global-symbol-macros
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defparameter *symbol-macro-storage* (list 1 2))
      (define-symbol-macro global-alias (car *symbol-macro-storage*))")
    (is (equal '(1 9 1) (m:evaluate-string vm "(list global-alias (let ((global-alias 9)) global-alias) global-alias)")))
    (is (equal '(3 (3 2)) (m:evaluate-string vm "(list (setq global-alias 3) *symbol-macro-storage*)")))
    (is (equal '(4 (4 2)) (m:evaluate-string vm "(list (incf global-alias) *symbol-macro-storage*)")))
    (is (equal '(nil nil) (m:evaluate-string vm "(list (boundp 'global-alias) (macro-function 'global-alias))")))
    (is (eq t (m:evaluate-string vm "(multiple-value-bind (form expandedp) (macroexpand-1 'global-alias)
      (and expandedp (equal form '(car *symbol-macro-storage*))))")))
    (is (= 7 (m:evaluate-string vm "(symbol-macrolet ((global-alias 7)) global-alias)")))
    (is (equal '(4 2) (m:evaluate-string vm "(funcall (lambda () (list global-alias (cadr *symbol-macro-storage*))))")))
    (is (equal '(9 4) (m:evaluate-string vm "(list (symbol-macrolet ((global-alias 9)) global-alias) global-alias)")))
    (is (= 17 (m:evaluate-string vm "(define-symbol-macro global-alias 17) global-alias")))
    (dolist (source '("(define-symbol-macro *symbol-macro-storage* 1)"
                      "(define-symbol-macro t 1)" "(define-symbol-macro missing)"
                      "(funcall (macro-function 'define-symbol-macro))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (program-error () t))")))))))

(test compiler-macro-definitions
  (m:with-interpreter (vm)
    (m:evaluate-string vm "
      (defparameter *cm-expanded* 0)
      (defun cm-add (x &optional (y 1)) (+ x y))
      (define-compiler-macro cm-add (x &optional (y 1)) \"Add arguments.\" (list '+ x y))
      (define-compiler-macro cm-decline (&whole form &rest args)
        (declare (ignore args)) (return-from cm-decline form))
      (defun cm-cell (x) (car x))
      (defun (setf cm-cell) (value x) (setf (car x) value))
      (define-compiler-macro cm-cell (x) (list 'car x))
      (define-compiler-macro (setf cm-cell) (value x)
        (declare (ignorable value x)) \"Set the cell.\" (list 'setf (list 'car x) value))")
    (is (equal '(+ 8 9) (m:evaluate-string vm "(funcall (compiler-macro-function 'cm-add) '(cm-add 8 9) nil)")))
    (is (equal '(+ 8 1) (m:evaluate-string vm "(funcall (compiler-macro-function 'cm-add) '(cm-add 8) nil)")))
    (is (equal '(+ 8 9) (m:evaluate-string vm "(funcall (compiler-macro-function 'cm-add) '(funcall #'cm-add 8 9) nil)")))
    (is (eq t (m:evaluate-string vm "(let ((form (list 'cm-decline 8)))
      (eq form (funcall (compiler-macro-function 'cm-decline) form nil)))")))
    (is (eq t (m:evaluate-string vm "(let ((form '(funcall #'cm-decline 8)))
      (eq form (funcall (compiler-macro-function 'cm-decline) form nil)))")))
    (is (equal '(car most-positive-fixnum) (m:evaluate-string vm "(funcall (compiler-macro-function 'cm-cell) '(cm-cell most-positive-fixnum) nil)")))
    (is (equal '(setf (car most-positive-fixnum) most-negative-fixnum) (m:evaluate-string vm
      "(funcall (compiler-macro-function '(setf cm-cell)) '((setf cm-cell) most-negative-fixnum most-positive-fixnum) nil)")))
    (is (equal '(setf (car most-positive-fixnum) most-negative-fixnum) (m:evaluate-string vm
      "(funcall (compiler-macro-function '(setf cm-cell)) '(funcall #'(setf cm-cell) most-negative-fixnum most-positive-fixnum) nil)")))
    (is (equal '(t nil nil) (m:evaluate-string vm "
      (macrolet ((cm-visible (&environment env)
                   (list 'quote (not (null (compiler-macro-function 'cm-add env))))))
        (list (cm-visible) (flet ((cm-add (x) x)) (cm-visible))
          (macrolet ((cm-add (x) x)) (cm-visible))))")))
    (is (null (m:evaluate-string vm "
      (macrolet ((cm-visible (&environment env)
                   (list 'quote (compiler-macro-function '(setf cm-cell) env))))
        (flet (((setf cm-cell) (v x) (declare (ignore x)) v)) (cm-visible)))")))
    (is (equal '("Add arguments." "Add arguments." "Set the cell.") (m:evaluate-string vm
      "(list (documentation 'cm-add 'compiler-macro)
             (documentation (compiler-macro-function 'cm-add) t)
             (documentation '(setf cm-cell) 'compiler-macro))")))
    (is (equal '("Changed." "Changed." (+ 3 1)) (m:evaluate-string vm "
      (let ((fn (compiler-macro-function 'cm-add)))
        (setf (documentation fn 'function) \"Changed.\")
        (list (documentation fn t) (documentation 'cm-add 'compiler-macro)
              (funcall fn '(cm-add 3) nil)))")))
    (is (eq t (m:evaluate-string vm "(let ((fn (compiler-macro-function 'cm-add)))
      (and (eq fn (setf (compiler-macro-function 'cm-add) fn))
           (eq fn (compiler-macro-function 'cm-add))))")))
    (is (equal '(nil t nil) (m:evaluate-string vm "
      (setf (compiler-macro-function 'cm-cell) nil)
      (list (compiler-macro-function 'cm-cell)
        (functionp (compiler-macro-function '(setf cm-cell)))
        (progn (setf (compiler-macro-function '(setf cm-cell)) nil)
          (compiler-macro-function '(setf cm-cell))))")))
    (is (equal '(9 nil) (m:evaluate-string vm "
      (let ((offset 9))
        (flet ((helper (x) (+ x offset)))
          (define-compiler-macro cm-captured (x) \"Captured.\" (helper x))))
      (let ((fn (compiler-macro-function 'cm-captured)))
        (setf (documentation fn t) nil)
        (list (funcall fn '(cm-captured 0) nil) (documentation fn t)))")))
    (is (= 17 (m:evaluate-string vm "
      (define-compiler-macro cm-environment (x &environment env) (macroexpand x env))
      (macrolet ((cm-expand (&environment env)
                   (list 'quote (funcall (compiler-macro-function 'cm-environment) '(cm-environment alias) env))))
        (symbol-macrolet ((alias 17)) (cm-expand)))")))
    (is (equal '(6 0) (m:evaluate-string vm "
      (define-compiler-macro cm-add (&whole form &rest args)
        (declare (ignore args)) (setq *cm-expanded* (1+ *cm-expanded*)) form)
      (list (cm-add 5) *cm-expanded*)")))
    (is (equal '("literal expansion" nil) (m:evaluate-string vm "
      (define-compiler-macro cm-string () \"literal expansion\")
      (list (funcall (compiler-macro-function 'cm-string) '(cm-string) nil)
            (documentation 'cm-string 'compiler-macro))")))
    (is (= 1 (length (multiple-value-list (m:evaluate-string vm "(compiler-macro-function 'cm-add)")))))
    (is (null (m:evaluate-string vm "(documentation 'never-defined 'function)")))
    (is (equal "Ignored." (m:evaluate-string vm "(setf (documentation 'never-defined 'function) \"Ignored.\")")))
    (dolist (source '("(compiler-macro-function)" "(compiler-macro-function 'cm-add nil nil)"
                      "(define-compiler-macro)" "(define-compiler-macro cm-bad)"
                      "(funcall (macro-function 'define-compiler-macro))"
                      "(funcall (macro-function 'define-compiler-macro) '(define-compiler-macro x ()))"
                      "(funcall (compiler-macro-function 'cm-add) '(cm-add))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(compiler-macro-function '(setf))" "(compiler-macro-function 17)"
                      "(setf (compiler-macro-function '(setf 17)) nil)"
                      "(setf (compiler-macro-function 'cm-add) 17)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string "(handler-case " source " (type-error () t))")))))))

(test function-body-documentation-and-declarations
  (m:with-interpreter (vm)
    (dolist (form
      '((funcall (lambda () "literal"))
        (funcall (lambda (&optional x) (declare (ignore x)) "literal"))
        (funcall (lambda (&key x) (declare (ignore x)) "literal"))
        (funcall (lambda (x) "doc" (declare (type integer x)) (1+ x)) 4)
        (funcall (lambda (x) (declare (type integer x)) "doc" (1+ x)) 4)
        (funcall (lambda (x) (declare (type integer x)) "doc" (declare (ignorable x)) (1+ x)) 4)
        (funcall (lambda (x) "doc" (declare (special x)) (symbol-value 'x)) 17)
        (funcall (lambda (&optional (x 17)) "doc" (declare (special x)) (symbol-value 'x)))
        (progn (defun body-declaration-test () "literal") (body-declaration-test))
        (progn (defun body-declaration-test (x) (declare (special x)) "doc" (symbol-value 'x)) (body-declaration-test 17))
        (progn (defun body-declaration-test (x) "doc" (declare (special x))
                 (return-from body-declaration-test (values (symbol-value 'x) 2)))
          (multiple-value-list (body-declaration-test 17)))
        (progn (defun body-declaration-test (&optional (x 17))
                 (declare (special x)) "doc" (symbol-value 'x)) (body-declaration-test))
        (flet ((local () "literal")) (local))
        (flet ((local (x) (declare (special x)) "doc" (symbol-value 'x))) (local 17))
        (labels ((local (x) (declare (type integer x)) "doc" (declare (ignorable x))
                   (if (zerop x) 17 (local (1- x))))) (local 3))
        (let* ((form '(lambda (x) (declare (type integer x)) "doc" (declare (ignorable x)) (1+ x)))
               (copy (copy-tree form)) (fn (eval form)))
          (list (funcall fn 3) (equal form copy)))))
      (is (equalp (eval form) (m:evaluate vm form)) "Function body mismatch for ~S" form))
    (m:evaluate-string vm "
      (defun documented-body (x) (declare (type integer x)) \"Function doc.\" (1+ x))
      (defmacro documented-macro (x) (declare (ignorable x)) \"Macro doc.\" (list 'quote x))")
    (is (equal '("Function doc." "Function doc." "Macro doc." "Macro doc.")
      (m:evaluate-string vm "(list (documentation 'documented-body 'function)
        (documentation #'documented-body t) (documentation 'documented-macro 'function)
        (documentation (macro-function 'documented-macro) t))")))
    (is (equal '(1 "Closure doc." 2 "Changed." 3 nil)
      (m:evaluate-string vm "(let* ((n 0) (fn (lambda () \"Closure doc.\" (incf n))))
        (list (funcall fn) (documentation fn t)
          (progn (setf (documentation fn t) \"Changed.\") (funcall fn)) (documentation fn 'function)
          (progn (setf (documentation fn t) nil) (funcall fn)) (documentation fn t)))")))
    (is (equal '("Lambda doc." "Local doc." "Label doc.")
      (m:evaluate-string vm "(list (documentation (lambda () (declare (optimize)) \"Lambda doc.\" 17) t)
        (flet ((local () \"Local doc.\" (declare (optimize)) 17)) (documentation #'local t))
        (labels ((local () (declare (optimize)) \"Label doc.\" 17)) (documentation #'local t)))")))
    (is (equal "literal expansion" (m:evaluate-string vm "(defmacro literal-body-macro () \"literal expansion\") (literal-body-macro)")))
    (is (null (m:evaluate-string vm "(documentation (macro-function 'literal-body-macro) t)")))))

(test compilation-unit-and-function-namespaces
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (with-compilation-unit ()))
        (multiple-value-list (with-compilation-unit () (values)))
        (multiple-value-list (with-compilation-unit (:override t) (values 1 2 3)))
        (let ((n 0)) (list (with-compilation-unit (:override (incf n)) (incf n) n) n))
        (let ((n 0)) (list (with-compilation-unit (:override (incf n))) n))
        (let ((n 0)) (with-compilation-unit (:override (incf n))
          (with-compilation-unit (:override (incf n)) (incf n))) n)
        (block done (with-compilation-unit () (return-from done (values 1 2))))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))
          "Compilation unit mismatch for ~S" form))
    (dolist (source '("(flet ((nil () 17)) (nil))"
                      "(labels ((pi (n) (if (zerop n) 17 (pi (1- n))))) (pi 3))"
                      "(macrolet ((nil () 17)) (nil))"
                      "(macrolet ((:local () 17)) (:local))"
                      "(with-hash-table-iterator (nil (make-hash-table)) (if (null (nil)) 17 0))"))
      (is (= 17 (m:evaluate-string vm source))))
    (is (eq t (m:evaluate-string vm "(flet ((pi () 17)) (and (= (pi) 17) (= pi 3.141592653589793d0)))")))
    (dolist (form '((with-compilation-unit) (with-compilation-unit (:override))
                    (with-compilation-unit (:unknown 17))))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test free-special-function-namespace
  (m:with-interpreter (vm)
    (dolist (binding '(flet labels macrolet))
      (let ((form `(let ((x 17)) (declare (special x))
                     (,binding ((x () 23))
                       (declare (special x) (type integer x))
                       (list x (x) (progn (setq x 19) x)))
                     x)))
        (is (equalp (eval form) (m:evaluate vm form)))))
    (is (eq t (m:evaluate-string vm
      "(let ((next 17)) (declare (special next))
         (with-hash-table-iterator (next (make-hash-table))
           (declare (special next) (type integer next))
           (and (= next 17) (null (next)))))")))))

(test macro-expansion-hook-dispatch
  (m:with-interpreter (vm)
    (is (= 17 (m:evaluate-string vm
      "(let ((original (symbol-function 'funcall)) (*macroexpand-hook* #'funcall))
         (unwind-protect
           (progn (setf (symbol-function 'funcall) (lambda (&rest args) (declare (ignore args)) 99))
             (macrolet ((literal () '17)) (literal)))
           (setf (symbol-function 'funcall) original)))")))
    (is (= 23 (m:evaluate-string vm
      "(macrolet ((literal () '17))
         (defmacro hooked-probe (&environment env)
           (let ((*macroexpand-hook* (lambda (fn form env)
                   (declare (ignore fn form env)) 23)))
             (macroexpand '(literal) env)))
         (hooked-probe))")))
    (is (= 1 (m:evaluate-string vm
      "(defparameter *explicit-hook-calls* 0)
       (defun explicit-macro-hook (fn form env)
         (setq *explicit-hook-calls* (1+ *explicit-hook-calls*)) (funcall fn form env))
       (macrolet ((literal () '17))
         (defmacro symbol-hook-probe (&environment env)
           (let ((*macroexpand-hook* 'explicit-macro-hook))
             (macroexpand '(literal) env))
           *explicit-hook-calls*)
         (symbol-hook-probe))")))
    (is (eq t (m:evaluate-string vm
      "(macrolet ((literal () '17))
         (defmacro invalid-hook-probe (&environment env)
           (let ((*macroexpand-hook* 17))
             (handler-case (macroexpand '(literal) env) (error () t))))
         (invalid-hook-probe))")))))

(test constant-macro-expanders
  (m:with-interpreter (vm)
    (dolist (form '((macrolet ((literal () '17)) (literal))
                    (macrolet ((literal () '(values 1 2 3))) (multiple-value-list (literal)))
                    (macrolet ((literal () '(+ x 3))) (let ((x 17)) (literal)))
                    (macrolet ((literal () 'nil)) (literal))
                    (progn (defmacro constant-expansion () '(+ 17 3)) (constant-expansion))
                    ;; Delay expansion so host compilers signal the arity
                    ;; error inside HANDLER-CASE rather than at compile time.
                    (handler-case (eval '(macrolet ((literal () '17)) (literal 1)))
                      (program-error () t))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (is (= 1 (m:evaluate-string vm
      "(let ((count 0))
         (macrolet ((literal () '17))
           (defmacro expand-literal (&environment env)
             (let ((*macroexpand-hook* (lambda (fn form env)
                     (setq count (1+ count)) (funcall fn form env))))
               (macroexpand '(literal) env)))
           (expand-literal)) count)")))))

(test module-loading
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((*modules* nil)) (provide "ONE") (not (null (member "ONE" *modules* :test #'string=))))
        (let ((*modules* nil)) (provide "ONE") (provide :one) (count "ONE" *modules* :test #'string=))
        (let ((*modules* nil)) (provide #\Z) (equal *modules* '("Z")))
        (let ((*modules* nil)) (provide "Case") (provide "CASE") (length *modules*))
        (let ((*modules* nil)) (provide "ONE") (require :one) (values 1 2))
        (let ((*modules* nil)) (provide "ONE") (require "ONE" "missing.lisp") t)
        (let ((*modules* nil))
          (let ((name (make-array 8 :element-type 'character :initial-contents "modname!" :fill-pointer 3)))
            (provide name) (provide "mod") (length *modules*)))
        (let ((*modules* nil)) (provide "ONE")
          (let ((*modules* nil)) (provide "TWO")) (equal *modules* '("ONE")))
        (handler-case (provide 17) (type-error () t))
        (handler-case (require 17) (type-error () t))
        (handler-case (require "missing-unique-module") (error () t))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (is (eq t (m:evaluate-string vm
      "(register-source-file \"module-one.lisp\"
         \"(push 1 *module-load-order*) (provide :one)\")
       (register-source-file \"module-two.lisp\"
         \"(push 2 *module-load-order*) (provide :two)\")
       (defvar *module-load-order* nil)
       (let ((*modules* nil) (*module-load-order* nil))
         (require :combined '(\"module-one.lisp\" \"module-two.lisp\"))
         (require :one \"missing.lisp\")
         (and (equal *module-load-order* '(2 1))
              (member \"ONE\" *modules* :test #'string=)
              (not (null (member \"TWO\" *modules* :test #'string=)))))")))
    (is (eq t (m:evaluate-string vm
      "(let ((*modules* nil) (*module-load-order* nil))
         (require :one \"module-one.lisp\")
         (equal *module-load-order* '(1)))")))
    (is (eq t (m:evaluate-string vm
      "(let ((*modules* nil))
         (handler-case (require :absent \"missing-module-file.lisp\") (file-error () t)))")))
    (dolist (form '((provide) (provide "one" "two") (require) (require "one" nil nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test function-source-introspection
  (m:with-interpreter (vm)
    (dolist (source
      '("(= 3 (length (multiple-value-list (function-lambda-expression #'+))))"
        "(multiple-value-bind (source closure name) (function-lambda-expression #'+)
           (declare (ignore closure)) (and (null source) (eq name '+)))"
        "(let ((x 17))
           (multiple-value-bind (source closure name) (function-lambda-expression (lambda () x))
             (declare (ignore name)) (and closure (eq (car source) 'lambda))))"
        "(let* ((fn (lambda (x) (+ x 3))) (source (function-lambda-expression fn)))
           (= (funcall (eval (list 'function source)) 17) 20))"
        "(let* ((fn (lambda (a &optional (b (+ a 1)) bp &rest rest)
                       (list a b bp rest)))
                (copy (eval (list 'function (function-lambda-expression fn)))))
           (and (equal (funcall fn 17) (funcall copy 17))
                (equal (funcall fn 17 23 t 31) (funcall copy 17 23 t 31))))"
        "(let* ((fn (lambda (&key (a 17) (b (+ a 3)) &aux (c (+ b 3))) (list a b c)))
                (copy (eval (list 'function (function-lambda-expression fn)))))
           (and (equal (funcall fn) (funcall copy))
                (equal (funcall fn :a 23) (funcall copy :a 23))))"
        "(progn (defun source-named (x) (declare (integer x)) \"Source doc.\"
                   (return-from source-named (+ x 3)))
           (multiple-value-bind (source closure name) (function-lambda-expression #'source-named)
             (declare (ignore closure))
             (let ((copy (eval (list 'function source))))
               (and (eq name 'source-named) (= (funcall copy 17) 20)
                    (equal (documentation copy t) \"Source doc.\")))))"
        "(let ((fn (lambda ())))
           (setf (documentation fn t) \"Added doc.\")
           (let ((copy (eval (list 'function (function-lambda-expression fn)))))
             (and (null (funcall copy)) (equal (documentation copy t) \"Added doc.\"))))"
        "(let* ((fn (lambda () \"Literal result.\"))
                (copy (eval (list 'function (function-lambda-expression fn)))))
           (equal (funcall copy) \"Literal result.\"))"
        "(= 3 (length (multiple-value-list (function-lambda-expression (constantly 17)))))"
        "(progn (defgeneric introspected-generic (x))
           (= 3 (length (multiple-value-list (function-lambda-expression #'introspected-generic)))))"
        "(let ((count 0)) (function-lambda-expression (progn (incf count) #'+)) (= count 1))"))
      (is (eq t (m:evaluate-string vm source)) "Function source mismatch: ~A" source))
    (dolist (form '((function-lambda-expression) (function-lambda-expression #'+ nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (dolist (value '(nil 17 (lambda () 17) +))
      (is (eq t (m:evaluate vm
        `(let ((object ',value))
           (handler-case (function-lambda-expression object)
             (type-error (c) (and (eq (type-error-datum c) object)
                                  (eq (type-error-expected-type c) 'function)))))))))))

(test object-load-forms
  (m:with-interpreter (vm)
    (m:evaluate-string vm "
      (defparameter *load-form-initializations* 0)
      (defclass load-form-parent () ((inherited :initarg :inherited)))
      (defclass load-form-node (load-form-parent)
        ((value :initarg :value :initform (incf *load-form-initializations*))
         (missing) (link) (shared :allocation :class :initform 7)))
      (defstruct load-form-structure (value 11 :read-only t) link)
      (defun copy-through-load-forms (object &rest options)
        (multiple-value-bind (creation initialization)
            (apply #'make-load-form-saving-slots object options)
          (let ((copy (eval creation)))
            (eval (subst copy object initialization)) copy)))")
    (dolist (source
      '("(let* ((object (make-instance 'load-form-node :value 17 :inherited 23))
                (copy (copy-through-load-forms object)))
           (and (not (eq object copy)) (eq (class-of object) (class-of copy))
                (= (slot-value copy 'value) 17) (= (slot-value copy 'inherited) 23)
                (not (slot-boundp copy 'missing)) (not (slot-boundp copy 'link))))"
        "(let* ((object (make-instance 'load-form-node))
                (count *load-form-initializations*) (copy (copy-through-load-forms object)))
           (and (= count *load-form-initializations*) (= (slot-value copy 'value) count)))"
        "(let* ((object (make-instance 'load-form-node :value 17 :inherited 23))
                (copy (copy-through-load-forms object :slot-names '(inherited))))
           (and (= (slot-value copy 'inherited) 23) (not (slot-boundp copy 'value))))"
        "(let ((copy (copy-through-load-forms (make-instance 'load-form-node) :slot-names nil)))
           (and (not (slot-boundp copy 'value)) (not (slot-boundp copy 'inherited))))"
        "(let ((object (make-instance 'load-form-node :value 17)))
           (slot-makunbound object 'value)
           (not (slot-boundp (copy-through-load-forms object) 'value)))"
        "(let* ((value (list 17)) (object (make-instance 'load-form-node :value value))
                (copy (copy-through-load-forms object)))
           (eq value (slot-value copy 'value)))"
        "(let ((object (make-instance 'load-form-node)))
           (setf (slot-value object 'link) object)
           (let ((copy (copy-through-load-forms object))) (eq copy (slot-value copy 'link))))"
        "(let* ((object (make-load-form-saving-slots (make-instance 'load-form-node)))
                (copy (eval object))) (not (slot-boundp copy 'value)))"
        "(let* ((object (make-instance 'load-form-node))
                (forms (multiple-value-list (make-load-form-saving-slots object))))
           (setf (slot-value object 'shared) 31)
           (eval (subst (eval (car forms)) object (cadr forms)))
           (= (slot-value object 'shared) 31))"
        "(let* ((object (make-instance 'load-form-node))
                (forms (multiple-value-list (make-load-form-saving-slots object :slot-names '(shared)))))
           (setf (slot-value object 'shared) 37)
           (eval (subst (eval (car forms)) object (cadr forms)))
           (= (slot-value object 'shared) 31))"
        "(let* ((object (make-load-form-structure :value 123)) (copy (copy-through-load-forms object)))
           (and (not (eq object copy)) (typep copy 'load-form-structure)
                (= (load-form-structure-value copy) 123)))"
        "(let ((object (make-load-form-structure)))
           (setf (load-form-structure-link object) object)
           (let ((copy (copy-through-load-forms object))) (eq copy (load-form-structure-link copy))))"
        "(let ((object (make-instance 'load-form-node)))
           (= 2 (length (multiple-value-list (make-load-form-saving-slots object :environment nil)))))"
        "(let ((class (find-class 'load-form-node))) (eq class (eval (make-load-form class))))"
        "(let ((class (find-class 'integer))) (eq class (eval (make-load-form class nil))))"
        "(progn (defclass custom-load-form () ())
           (defmethod make-load-form ((object custom-load-form) &optional environment)
             (declare (ignore environment)) (values '(list 17) '(list 23)))
           (equal (mapcar #'eval (multiple-value-list (make-load-form (make-instance 'custom-load-form))))
                  '((17) (23))))"
        "(let ((class (find-class 'load-form-node)))
           (setf (find-class 'load-form-node) nil)
           (unwind-protect (handler-case (make-load-form class) (error () t))
             (setf (find-class 'load-form-node) class)))"))
      (is (eq t (m:evaluate-string vm source)) "Load forms mismatch: ~A" source))
    (dolist (form '((make-load-form (make-instance 'load-form-node))
                    (make-load-form (make-load-form-structure))
                    (make-load-form (make-condition 'simple-error))))
      (is (eq t (m:evaluate vm `(handler-case ,form (error () t))))))
    (dolist (form '((make-load-form-saving-slots)
                   (make-load-form-saving-slots (make-instance 'load-form-node) :slot-names)
                   (make-load-form-saving-slots (make-instance 'load-form-node) :invalid t)
                   (make-load-form) (make-load-form (find-class 'integer) nil nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test implementation-environment-metadata
  (m:with-interpreter (vm)
    (dolist (name '(lisp-implementation-type lisp-implementation-version machine-type machine-version
                   machine-instance software-type software-version short-site-name long-site-name))
      (is (eq t (m:evaluate vm `(let ((values (multiple-value-list (,name))))
                                (and (= (length values) 1) (typep (car values) '(or string null)))))))
      (is (eq t (m:evaluate vm `(handler-case (,name nil) (program-error () t))))))
    (is (equal "Miricl" (m:evaluate vm '(lisp-implementation-type))))
    (is (equal "WebAssembly" (m:evaluate vm '(software-type))))))

(test object-descriptions
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((stream (make-string-output-stream)))
          (list (multiple-value-list (describe 17 stream)) (plusp (length (get-output-stream-string stream)))))
        (let ((*standard-output* (make-string-output-stream)))
          (list (multiple-value-list (describe 17)) (plusp (length (get-output-stream-string *standard-output*)))))
        (let ((*standard-output* (make-string-output-stream)))
          (list (multiple-value-list (describe 17 nil)) (plusp (length (get-output-stream-string *standard-output*)))))
        (let* ((stream (make-string-output-stream))
               (*terminal-io* (make-two-way-stream (make-string-input-stream "") stream)))
          (list (multiple-value-list (describe 17 t)) (plusp (length (get-output-stream-string stream)))))
        (handler-case (describe) (program-error () t))
        (handler-case (describe 17 nil nil) (program-error () t))
        (handler-case (describe 17 23) (type-error () t))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (is (eq t (m:evaluate-string vm
      "(progn
         (defclass described-object () ((value :initarg :value)))
         (defmethod describe-object ((object described-object) stream)
           (assert (and (streamp stream) (output-stream-p stream)))
           (format stream \"Value: ~A~%\" (slot-value object 'value)) (values 17 23))
         (let ((stream (make-string-output-stream)))
           (and (null (multiple-value-list (describe (make-instance 'described-object :value 41) stream)))
                (equal (get-output-stream-string stream) (format nil \"Value: 41~%\")))))")))
    (is (eq t (m:evaluate-string vm
      "(progn
         (defclass description-link () ((name :initarg :name) (next :initarg :next :initform nil)))
         (defmethod describe-object ((object description-link) stream)
           (write-string (slot-value object 'name) stream) (terpri stream)
           (when (slot-value object 'next) (describe (slot-value object 'next) stream)))
         (let* ((a (make-instance 'description-link :name \"A\"))
                (b (make-instance 'description-link :name \"B\" :next a)))
           (setf (slot-value a 'next) b)
           (equal (with-output-to-string (stream) (describe a stream))
                  (format nil \"A~%  B~%    #<Description truncated>~%\"))))")))
    (is (eq t (m:evaluate-string vm
      "(let ((root nil))
         (dotimes (i 8) (setq root (make-instance 'description-link :name \"node\" :next root)))
         (let ((text (with-output-to-string (stream) (describe root stream))))
           (and (search \"truncated\" text) (< (length text) 200))))")))
    (is (eq t (m:evaluate-string vm
      "(progn
         (defclass description-error () ((fail :initform t)))
         (defmethod describe-object ((object description-error) stream)
           (when (slot-value object 'fail) (error \"Description failed\"))
           (write-string \"Recovered\" stream))
         (let ((object (make-instance 'description-error)))
           (with-output-to-string (stream)
             (handler-case (describe object stream) (error () nil)))
           (setf (slot-value object 'fail) nil)
           (equal (with-output-to-string (stream) (describe object stream)) \"Recovered\")))")))
    (is (eq t (m:evaluate-string vm
      "(let ((list (list 17)))
         (setf (cdr list) list)
         (let ((text (with-output-to-string (stream) (describe list stream))))
           (and (plusp (length text)) (< (length text) 200))))")))
    (is (eq t (m:evaluate-string vm
      "(let ((*print-readably* t))
         (with-output-to-string (stream) (describe (make-hash-table) stream)) *print-readably*)")))
    (is (eq t (m:evaluate-string vm
      "(not (null (find-method #'describe-object nil (list (find-class 'standard-object) (find-class t)) nil)))")))
    (is (eq t (m:evaluate-string vm
      "(not (null (search \"Lambda:\"
         (with-output-to-string (stream) (describe (lambda (x) x) stream)))))")))))

(test repl-history-variable-bindings
  (m:with-interpreter (vm)
    (dolist (name '(* ** *** + ++ +++ - / // ///))
      (is (null (m:evaluate vm name)))
      (is (eq t (m:evaluate vm `(let ((,name 17)) (= (symbol-value ',name) 17))))))))

(test native-apply-spreading
  (m:with-interpreter (vm)
    (dolist (form
      '((destructuring-bind (x . y) '(17 . 23) (list x y))
        (destructuring-bind ((x . y)) '((17 . 23)) (list x y))
        (destructuring-bind (&whole (a . b) c . d) '(1 . 2) (list a b c d))
        (destructuring-bind (x &rest (y z)) '(1 2 3) (values x y z))
        (destructuring-bind (&whole (a &rest tail) &optional (x a)) '(17)
          (list a tail x))
        (destructuring-bind (x &rest (y . z)) '(1 2 . 3) (list x y z))
        (destructuring-bind ((&whole (a &rest tail) &optional (x a))) '((17))
          (list a tail x))
        (handler-case (destructuring-bind (x nil y) '(1 2 3) nil) (program-error () t))
        (progn (defmacro dotted-optional (&optional ((x . y) '(17 . 23)))
                 (list 'list (list 'quote x) (list 'quote y)))
          (list (dotted-optional) (dotted-optional (1 2))))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (dolist (form
      '((multiple-value-list (apply #'values nil))
        (multiple-value-list (apply #'values '(1 2 3)))
        (multiple-value-list (apply #'values 1 2 '(3 4)))
        (apply #'list 1 2 nil)
        (apply #'cons 1 '(2))
        (apply 'cons '(1 2))
        (apply #'apply #'list 1 '(2 (3 4)))
        (let ((x 17)) (apply (lambda (y) (+ x y)) '(23)))
        (let ((x (list (list 17) (list 23))))
          (apply (lambda (a b) (and (eq a (car x)) (eq b (cadr x)))) x))
        (let ((x (list 1 2 3))) (list (apply #'list 17 x) x))
        (let ((count 0))
          (list (apply (progn (incf count) #'list)
                   (progn (incf count) count) (progn (incf count) (list count))) count))
        (apply (lambda (&key (x 17)) x) '(:x 23 :x 31))
        (apply (lambda (&key (x 17)) x) :allow-other-keys t '(:other 99 :x 23))
        (apply #'+ (make-list 1000 :initial-element 1))
        (handler-case (eval '(apply)) (program-error () t))
        (handler-case (eval '(apply #'list)) (program-error () t))
        (handler-case (eval '(apply #'cons nil)) (program-error () t))
        (handler-case (eval '(apply #'cons '(1 2 3))) (program-error () t))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (dolist (length '(1 2 17))
      (is (eq t (m:evaluate vm
        `(let ((args (make-list ,length)))
           (setf (cdr (last args)) args)
           (handler-case (apply #'list args) (type-error () t)))))))
    (dolist (args '(17 (1 2 . 3)))
      (is (eq t (m:evaluate vm `(handler-case (apply #'list ',args) (type-error () t))))))
    (is (eq t (m:evaluate-string vm
      "(progn (defun apply-target (x) (+ x 1))
         (flet ((apply-target (x) (+ x 3)))
           (and (= (apply 'apply-target '(17)) 18)
                (= (apply #'apply-target '(17)) 20))))")))))

(test multiple-value-input-snapshots
  (m:with-interpreter (vm)
    (dolist (operator '(values-list (apply #'values)))
      (let ((expression (if (symbolp operator) '(values-list x) '(apply #'values x))))
        (dolist (form
          `((let* ((x (list 1 2 3)) (y (multiple-value-list ,expression))) (setf (cadr x) 99) y)
            (let* ((x (list 1 2 3)) (y (multiple-value-list ,expression))) (setf (cadr y) 99) x)
            (let ((x (list 1 2 3)))
              (multiple-value-list (multiple-value-prog1 ,expression (setf (cadr x) 99))))
            (let ((x (list 1 2 3)))
              (multiple-value-list (unwind-protect ,expression (setf (cadr x) 99))))
            (let ((x nil)) (multiple-value-list ,expression))
            (let ((x (list 17))) (multiple-value-list ,expression))
            (let ((x (list (list 17) (list 23))))
              (multiple-value-bind (a b) ,expression (and (eq a (car x)) (eq b (cadr x)))))))
          (is (equalp (eval form) (m:evaluate vm form)) "Value snapshot mismatch: ~S" form))))
    (dolist (form '((multiple-value-list (values-list (values '(1 2) '(3 4))))
                    (let ((calls 0)) (list (multiple-value-list (values-list (progn (incf calls) '(1 2)))) calls))
                    (multiple-value-list (values))
                    (multiple-value-list (values 1 (values 2 3)))))
      (is (equalp (eval form) (m:evaluate vm form))))
    (is (eq t (m:evaluate-string vm
      "(equal (flet ((values (&rest args) (list args)))
                (multiple-value-list (values 1 2 3))) '(((1 2 3))))")))
    (dolist (object '(17 (1 2 . 3)))
      (is (eq t (m:evaluate vm
        `(let ((object ',object))
           (handler-case (values-list object)
             (type-error (c) (and (eq object (type-error-datum c))
                                  (not (typep object (type-error-expected-type c)))))))))))
    (dolist (length '(1 2 3 4 17))
      (is (eq t (m:evaluate vm
        `(let ((object (make-list ,length)))
           (setf (cdr (last object)) object)
           (handler-case (values-list object)
             (type-error (c) (and (eq object (type-error-datum c))
                                  (not (typep object (type-error-expected-type c)))))))))))
    (is (eq t (m:evaluate-string vm
      "(let ((original (symbol-function 'values)))
         (unwind-protect
           (equal (values (progn (setf (symbol-function 'values) (lambda (&rest args) args)) 17) 23) '(17 23))
           (setf (symbol-function 'values) original)))")))))

(test native-complement-functions
  (m:with-interpreter (vm)
    (dolist (form
      '((multiple-value-list (funcall (complement #'identity) nil))
        (multiple-value-list (funcall (complement #'identity) t))
        (multiple-value-list (funcall (complement (lambda () (values)))))
        (multiple-value-list (funcall (complement (lambda () (values nil 17 23)))) )
        (multiple-value-list (funcall (complement (lambda () (values t nil nil)))))
        (let ((seen nil))
          (let ((fn (complement (lambda (&rest args) (setq seen args) nil))))
            (list (funcall fn 1 2 3) seen)))
        (let ((calls 0))
          (let ((fn (complement (progn (incf calls) (lambda (x) (incf calls) x)))))
            (list (funcall fn (progn (incf calls) nil)) calls)))
        (let ((fn (complement (lambda (&key (x 17)) (= x 17)))))
          (list (funcall fn) (funcall fn :x 23) (funcall fn :x 17 :x 23)))
        (block done (funcall (complement (lambda () (return-from done (values 17 23))))))
        (handler-case (complement) (program-error () t))
        (handler-case (complement #'null nil) (program-error () t))
        (handler-case (funcall (complement #'identity)) (program-error () t))
        (handler-case (funcall (complement #'identity) 1 2) (program-error () t))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (is (eq t (m:evaluate-string vm
      "(let ((fn (complement #'null))) (and (compiled-function-p fn)
         (typep fn 'compiled-function) (eq (type-of fn) 'compiled-function)))")))
    ;; Retain the existing symbol-designator extension and late lookup.
    (is (eq t (m:evaluate-string vm
      "(defun complement-target (x) x)
       (let ((fn (complement 'complement-target)))
         (setf (symbol-function 'complement-target) (lambda (x) (not x)))
         (funcall fn t))")))))

(test native-constantly-functions
  (m:with-interpreter (vm)
    (dolist (form
      '((let ((fn (constantly 17))) (list (funcall fn) (funcall fn 1 2 3) (apply fn '(1 2 3 4))))
        (multiple-value-list (funcall (constantly nil)))
        (multiple-value-list (funcall (constantly (values 17 23))))
        (let* ((value (list 17)) (fn (constantly value)))
          (setf (car value) 23) (eq value (funcall fn)))
        (let ((x 17)) (declare (special x))
          (let ((fn (constantly x))) (setq x 23) (list (funcall fn) x)))
        (let ((calls 0))
          (let ((fn (constantly (incf calls))))
            (list (funcall fn (incf calls) (incf calls)) calls)))
        (let ((fn (constantly (values)))) (multiple-value-list (funcall fn)))
        (let ((a (constantly 1)) (b (constantly 2))) (list (funcall a) (funcall b)))
        (handler-case (constantly) (program-error () t))
        (handler-case (constantly 1 2) (program-error () t))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (is (eq t (m:evaluate-string vm
      "(let ((fn (constantly 17))) (and (compiled-function-p fn)
         (typep fn 'compiled-function) (eq (type-of fn) 'compiled-function)))")))))

(test native-compiled-function-types
  (m:with-interpreter (vm)
    (dolist (source
      '("(compiled-function-p #'+)"
        "(compiled-function-p (fdefinition '(setf symbol-value)))"
        "(not (compiled-function-p (fdefinition '(setf car))))"
        "(not (compiled-function-p (lambda () 17)))"
        "(not (compiled-function-p '(lambda () 17)))"
        "(not (compiled-function-p '+))"
        "(eq (type-of #'+) 'compiled-function)"
        "(eq (class-name (class-of #'+)) 'function)"
        "(eq (type-of (lambda () 17)) 'function)"
        "(multiple-value-bind (a b) (subtypep 'compiled-function 'function) (and a b))"
        "(multiple-value-bind (a b) (subtypep 'compiled-function (find-class 'function)) (and a b))"
        "(multiple-value-bind (a b) (subtypep 'function 'compiled-function) (and (not a) b))"
        "(multiple-value-bind (a b) (subtypep (find-class 'function) 'compiled-function) (and (not a) b))"
        "(multiple-value-bind (a b) (subtypep 'compiled-function t) (and a b))"
        "(multiple-value-bind (a b) (subtypep 'compiled-function 'compiled-function) (and a b))"
        "(every (lambda (object) (eq (not (null (compiled-function-p object)))
                                  (typep object 'compiled-function)))
           (list nil t 17 1.5 #\\a \"text\" '(1 2) #'+ (lambda () 17) (fdefinition '(setf car))))"
        "(progn (defgeneric interpreted-generic (x))
           (defmethod interpreted-generic ((x t)) (when x 17))
           (and (not (compiled-function-p #'interpreted-generic))
                (typep #'interpreted-generic (type-of #'interpreted-generic))
                (= (interpreted-generic t) 17)))"
        "(macrolet ((literal () '17))
           (defmacro compiled-expander-check (&environment env)
             (not (null (compiled-function-p (macro-function 'literal env)))))
           (compiled-expander-check))"
        "(let ((calls 0)) (and (not (compiled-function-p (progn (incf calls) nil))) (= calls 1)))"))
      (is (eq t (m:evaluate-string vm source)) "Compiled function mismatch: ~A" source))
    (dolist (form '((compiled-function-p) (compiled-function-p nil nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))))

(test abbreviated-compound-and-class-declarations
  (m:with-interpreter (vm)
    (dolist (source
      '("(let ((x 17)) (declare ((integer 0 20) x)) x)"
        "(let ((x (vector 17 23))) (declare ((simple-array t (2)) x)) (aref x 0))"
        "(eval (list 'let '((x 17)) (list 'declare (list (find-class 'integer) 'x)) 'x))"
        "(let ((fn #'+)) (declare (compiled-function fn)) (funcall fn 8 9))"
        "(locally (declare ((integer 0 20) never-bound-compound-declaration)) 17)"))
      (is (= 17 (m:evaluate-string vm source))))
    (is (equal '(nil t) (m:evaluate-string vm "(multiple-value-list (subtypep 'compiled-function nil))")))
    (is (eq t (m:evaluate-string vm "(handler-case
      (let ((x 23)) (declare ((integer 0 20) x)) x) (type-error () t))")))))

(test free-variable-type-declarations
  (m:with-interpreter (vm)
    (dolist (source
      '("(locally (declare (type integer never-bound-free-type)) 17)"
        "(funcall (lambda (x) (declare (type (simple-array t nil) unused-free-array)) (1+ x)) 16)"
        "(progn (defparameter *free-type-value* 17)
           (locally (declare (type integer *free-type-value*))
             (setq *free-type-value* 18) (1- *free-type-value*)))"
        "(progn (defparameter *free-type-value* 17)
           (funcall (locally (declare (type integer *free-type-value*))
             (lambda () *free-type-value*))))"
        "(progn (defvar *free-type-cell* (list 17))
           (define-symbol-macro free-typed-alias (car *free-type-cell*))
           (locally (declare (type integer free-typed-alias)) free-typed-alias))"))
      (is (= 17 (m:evaluate-string vm source))))
    (dolist (source
      '("(locally (declare (type integer absent-type-reference)) absent-type-reference)"
        "(funcall (locally (declare (type integer absent-type-reference))
           (lambda () absent-type-reference)))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (unbound-variable () t))")))))
    ;; Checked execution retains local assertions across dynamic reads/writes.
    (dolist (source
      '("(progn (defparameter *free-type-value* 17)
           (locally (declare (type integer *free-type-value*)) (setq *free-type-value* 'bad)))"
        "(progn (defparameter *free-type-value* 'bad)
           (locally (declare (type integer *free-type-value*)) *free-type-value*))"
        "(let ((x 17)) (declare (special x) (type integer x)) (setq x 'bad))"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error () t))")))))))

(test value-macro-expansion-functions
  (m:with-interpreter (vm)
    (dolist (form '((multiple-value-list (values))
                    (multiple-value-list (values 1 2 3))
                    (nth-value 2 (values 1 2 3 4))
                    (let ((n 0)) (list (nth-value (incf n) (values (incf n) (incf n))) n))))
      (is (equalp (eval form) (m:evaluate vm form)))
      (is (equalp (eval form) (m:evaluate vm `(eval (macroexpand ',form))))))
    (dolist (name '(multiple-value-list nth-value))
      (is (eq t (m:evaluate vm `(and (macro-function ',name) (fboundp ',name)
                                   (not (special-operator-p ',name))))))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))
    (is (eq t (m:evaluate-string vm "(let ((events nil))
      (and (null (nth-value (progn (push :index events) (expt 2 100))
                    (progn (push :values events) (values 1 2))))
           (equal events '(:values :index))))")))
    (is (equal '(3 (1 2)) (m:evaluate-string vm "
      (setf (macro-function 'copied-values-macro) (macro-function 'multiple-value-list))
      (list (nth-value 2 (values 1 2 3)) (copied-values-macro (values 1 2)))")))
    (is (= 17 (m:evaluate-string vm "(macrolet ((nth-value (&rest ignored)
      (declare (ignore ignored)) 17)) (nth-value 0 (values 1 2)))")))
    (is (eq t (m:evaluate-string vm "(let ((counter *gensym-counter*))
      (and (multiple-value-list (values 1 2)) (nth-value 1 (values 1 2))
        (macroexpand '(nth-value 1 (values 1 2))) (= counter *gensym-counter*)))")))))

(test core-definition-macro-functions
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm "(let ((counter *gensym-counter*))
      (defvar *native-definition-counter* 17)
      (defparameter *native-definition-counter* 23)
      (defun native-definition-counter () *native-definition-counter*)
      (in-package :cl-user)
      (and (= (native-definition-counter) 23) (= counter *gensym-counter*)))")))
    (dolist (name '(defvar defparameter destructuring-bind in-package))
      (is (eq t (m:evaluate vm `(and (macro-function ',name) (fboundp ',name)))))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))
    (is (eq t (m:evaluate-string vm "(progn
      (makunbound '*expanded-var*) (makunbound '*expanded-unbound*)
      (eval (macroexpand '(defvar *expanded-unbound*)))
      (eval (macroexpand '(defvar *expanded-var* 17 \"First\")))
      (eval (macroexpand '(defvar *expanded-var* (error \"must not run\") \"Second\")))
      (and (not (boundp '*expanded-unbound*)) (= *expanded-var* 17)
        (string= (documentation '*expanded-var* 'variable) \"Second\")
        (equal (multiple-value-list (defparameter *expanded-var* (values 23 31))) '(*expanded-var*))
        (= *expanded-var* 23)))")))
    (is (= 29 (m:evaluate-string vm "(let ((value 28))
      (flet ((initializer () (1+ value)))
        (defparameter *lexical-definition-value* (initializer))) *lexical-definition-value*)")))
    (is (equal '(23 24) (m:evaluate-string vm "(progn
      (setf (symbol-value 'formerly-lexical-variable) 17)
      (let ((formerly-lexical-variable 23))
        (defparameter formerly-lexical-variable (1+ formerly-lexical-variable))
        (list formerly-lexical-variable (symbol-value 'formerly-lexical-variable))))")))
    (is (equal '(3 7) (m:evaluate-string vm "(multiple-value-list
      (eval (macroexpand '(destructuring-bind ((a &optional (b 2)) . rest) '((1) 7)
        (declare (type integer a b)) (values (+ a b) (car rest))))))")))
    (is (eq t (m:evaluate-string vm "(progn
      (unless (find-package \"EXPANDED-PACKAGE\") (make-package \"EXPANDED-PACKAGE\" :use '(\"CL\")))
      (let ((*package* *package*))
        (and (eq (eval (macroexpand '(in-package :expanded-package))) (find-package \"EXPANDED-PACKAGE\"))
          (eq *package* (find-package \"EXPANDED-PACKAGE\")))))")))
    (is (eq t (m:evaluate-string vm "(handler-case (in-package :no-such-expanded-package)
      (package-error () t))")))
    (is (= 31 (m:evaluate-string vm "(progn
      (setf (macro-function 'copied-defvar) (macro-function 'defvar))
      (copied-defvar *copied-variable-definition* 31) *copied-variable-definition*)")))
    (is (eq t (m:evaluate-string vm "(progn (proclaim '(special *native-proclamation*))
      (not (boundp '*native-proclamation*)))")))))

(test function-definition-macro-functions
  (m:with-interpreter (vm)
    (dolist (name '(defun defmacro))
      (is (eq t (m:evaluate vm `(and (macro-function ',name) (fboundp ',name)))))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))
    (is (eq t (m:evaluate-string vm "(progn
      (eval (macroexpand '(defun expanded-definition (x &optional (y 2))
        \"expanded doc\" (declare (type integer x y))
        (return-from expanded-definition (values (+ x y) y)))))
      (and (equal (multiple-value-list (expanded-definition 17)) '(19 2))
           (string= (documentation 'expanded-definition 'function) \"expanded doc\")))")))
    (is (= 42 (m:evaluate-string vm "(let ((base 40))
      (flet ((increment () (+ base 2))) (defun captured-definition () (increment))))
      (captured-definition)")))
    (is (equal '(31 31) (m:evaluate-string vm "(progn
      (defun (setf expanded-setter) (value cell)
        (return-from expanded-setter (progn (rplaca cell value) value)))
      (let ((cell (list 1))) (list (funcall #'(setf expanded-setter) 31 cell) (car cell))))")))
    (is (eq t (m:evaluate-string vm "(progn
      (eval (macroexpand '(defmacro expanded-macro (x) \"macro doc\" (list '1+ x))))
      (let ((documented (and (= (expanded-macro 16) 17)
        (string= (documentation 'expanded-macro 'function) \"macro doc\"))))
        (defun expanded-macro (x) (1- x))
        (and documented (not (macro-function 'expanded-macro)) (= (expanded-macro 18) 17))))")))
    (is (= 29 (m:evaluate-string vm "(let ((value 29))
      (defmacro captured-definition-macro () (list 'quote value))) (captured-definition-macro)")))
    (is (eq t (m:evaluate-string vm "(progn
      (defconstant constant-and-macro 17)
      (defmacro constant-and-macro () 23)
      (and (= constant-and-macro 17) (= (constant-and-macro) 23)))")))
    (is (eq t (m:evaluate-string vm "(progn
      (setf (macro-function 'copied-function-definition) (macro-function 'defun))
      (copied-function-definition copied-defined-function () 17)
      (= (copied-defined-function) 17))")))))

(test correctable-check-handler-retries
  (m:with-interpreter (vm)
    (dolist (conditionp '(nil t))
      (is (= 6 (m:evaluate vm `(let ((x 0))
        (handler-bind ((type-error (lambda (condition) (incf x) (continue ,(when conditionp 'condition)))))
          (assert (> x 5) nil 'type-error) x))))))
    (is (equal '(17 3) (m:evaluate-string vm "(let ((x :bad) (calls 0))
      (handler-bind ((type-error (lambda (condition) (incf calls)
          (store-value (if (< calls 3) :bad 17) condition))))
        (check-type x integer) (list x calls)))")))))

(test pathname-printing-controls
  (m:with-interpreter (vm)
    (dolist (source
      '("(every (lambda (p) (string= (write-to-string p :readably nil :escape t)
           (concatenate 'string \"#P\" (write-to-string (namestring p) :readably nil :escape t))))
           (list (make-pathname :type \"txt\") (make-pathname :type :wild)
                 (make-pathname :version :wild) (make-pathname :version :newest)))"
        "(let* ((p (make-pathname :name \"data\" :version 23))
                 (q (read-from-string (write-to-string p :readably t))))
           (and (string= (pathname-name q) \"data\") (= (pathname-version q) 23)))"
        "(let ((*read-eval* nil)) (handler-case
           (write-to-string (make-pathname :version 23) :readably t)
           (print-not-readable () t)))"
        "(let ((*read-eval* nil)) (string= (write-to-string
           (make-pathname :type \"txt\") :readably nil :escape t)
           (concatenate 'string \"#P\" (write-to-string \".txt\" :escape t))))"))
      (is (eq t (m:evaluate-string vm source))))))

(test assignment-and-binding-macro-functions
  (m:with-interpreter (vm)
    (dolist (name '(setf multiple-value-bind defconstant))
      (is (eq t (m:evaluate vm `(and (macro-function ',name) (fboundp ',name)))))
      (dolist (arguments '(nil (nil) (nil nil nil)))
        (is (eq t (m:evaluate vm `(handler-case (apply (macro-function ',name) ',arguments)
                                   (program-error () t)))))))
    (dolist (source
      '("(equal (multiple-value-list (eval (macroexpand '(multiple-value-bind (a b c)
           (values 17 23) (declare (type integer a b)) (values c b a))))) '(nil 23 17))"
        "(null (multiple-value-list (eval (macroexpand '(multiple-value-bind () (values 1 2) (values))))))"
        "(string= (eval (macroexpand '(multiple-value-bind () nil \"result\"))) \"result\")"
        "(progn (defvar *expanded-binding-scope* 17)
           (defun read-expanded-binding-scope () *expanded-binding-scope*)
           (equal (multiple-value-list (eval (macroexpand '(multiple-value-bind
             (*expanded-binding-scope* other) (values (read-expanded-binding-scope) 23)
             (declare (special *expanded-binding-scope*))
             (values (read-expanded-binding-scope) other))))) '(17 23)))"
        "(macrolet ((binding-result () 23))
           (setf (macro-function 'copied-binding) (macro-function 'multiple-value-bind))
           (= (copied-binding (x) (binding-result) x) 23))"
        "(progn (setf (macro-function 'copied-assignment) (macro-function 'setf))
           (let ((cell (list 1 2)) (events nil))
             (and (= (copied-assignment (car (progn (push :place events) cell))
                        (progn (push :value events) 17) (cadr cell) 23) 23)
                  (equal cell '(17 23)) (equal events '(:value :place)))))"
        "(progn (defparameter *expanded-setf-cell* (list 1 2))
           (and (equal (multiple-value-list (eval (macroexpand '(setf
             (values (car *expanded-setf-cell*) (cadr *expanded-setf-cell*)) (values 17 23))))) '(17 23))
             (equal *expanded-setf-cell* '(17 23))))"
        "(macrolet ((local-place (cell) (list 'car cell)))
           (setf (macro-function 'copied-local-assignment) (macro-function 'setf))
           (let ((cell (list 1))) (copied-local-assignment (local-place cell) 17) (= (car cell) 17)))"
        "(progn (eval (macroexpand '(defconstant expanded-constant 17 \"constant doc\")))
           (defconstant expanded-constant 17)
           (and (constantp 'expanded-constant) (= expanded-constant 17)
             (string= (documentation 'expanded-constant 'variable) \"constant doc\")))"
        "(progn (setf (macro-function 'copied-constant) (macro-function 'defconstant))
           (let ((value 23)) (copied-constant expanded-lexical-constant value))
           (= expanded-lexical-constant 23))"
        "(let ((counter *gensym-counter*) (cell (list 1)))
           (setf (car cell) 17) (multiple-value-bind (x) 23 (+ x (car cell)))
           (= counter *gensym-counter*))"))
      (is (eq t (m:evaluate-string vm source)) "Macro expansion mismatch: ~A" source))))
