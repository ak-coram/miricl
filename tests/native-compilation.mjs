import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  check("(fboundp 'miricl-compiler:compile-native)");
  const lispDisassembly = vm.evaluate(`(defun disassembly-square (x) (* x x))
    (disassemble #'disassembly-square)`, {eof: true}).output;
  assert.match(lispDisassembly, /Packed Lisp code: [1-9][0-9]* bytes/);
  check(`(eq miricl-compiler::*compiler-state* :cold)`);
  check(`(let ((function #'disassembly-square))
    (with-output-to-string (*standard-output*) (disassemble function))
    (eq function #'disassembly-square))`);
  if (process.argv.includes('--bootstrap')) {
    check(`(multiple-value-bind (installed unsupported) (miricl-compiler::compile-compiler)
      (and (>= (length installed) 93) (null unsupported)
           (null (function-lambda-expression #'miricl-compiler::wasm-byte))
           (null (function-lambda-expression #'miricl-compiler::hir-emit))
           (null (function-lambda-expression #'miricl-compiler::hir-form))
           (null (function-lambda-expression #'miricl-compiler:compile-native))))`);
  }
  check(`(progn
    (defparameter *native* (miricl-compiler:compile-native '(lambda (x) (if x (+ x 2) 0))))
    (and (compiled-function-p *native*) (= (funcall *native* 40) 42) (= (funcall *native* nil) 0)))`);
  // Carry across limb boundaries, the native small-integer limit, and the
  // signed i32 boundary; larger integers must still use exact arithmetic.
  const integers = [0n, 1n, -1n, 32767n, 32768n, -32768n,
    1073741823n, -1073741823n, 1073741824n, -2147483648n, 1n << 80n];
  vm.evaluate(`(defparameter *startup-add* (miricl-compiler:compile-native '(lambda (a b) (+ a b))))`);
  for (const a of integers) for (const b of integers)
    check(`(eql (funcall *startup-add* ${a} ${b}) ${a + b})`);
  vm.evaluate(`(defparameter *bounded-arithmetic* (miricl-compiler:compile-native
    '(lambda (a b) (list (- a b) (* a b) (< a b) (> a b) (<= a b) (>= a b) (= a b) (/= a b)))))`);
  for (const a of [...integers, -32767n, 256n, -17n, 2147483647n]) for (const b of integers) {
    const expected = [a - b, a * b, a < b, a > b, a <= b, a >= b, a === b, a !== b]
      .map(value => typeof value === 'boolean' ? (value ? 't' : 'nil') : value).join(' ');
    check(`(equal (funcall *bounded-arithmetic* ${a} ${b}) '(${expected}))`);
  }
  for (let count = 1; count <= 3; count++) {
    const items = Array(count).fill('x').join(' ');
    check(`(let* ((f (miricl-compiler:compile-native '(lambda (x)
                   (let ((result (list ${items}))) (values result :done)))))
                 (item (cons :item nil)) (a (funcall f item)) (b (funcall f item)))
      (and (= (length a) ${count}) (every (lambda (x) (eq x item)) a)
           (not (eq a b)) (progn (setf (car a) :changed) (eq (car b) item))
           (eq (nth-value 1 (funcall f item)) :done)
           (let ((original (symbol-function 'list)))
             (unwind-protect
               (progn (setf (symbol-function 'list) (lambda (&rest args) (declare (ignore args)) :replacement))
                      (eq (funcall f item) :replacement))
               (setf (symbol-function 'list) original)))))`);
  }
  check(`(= (length (multiple-value-list (miricl-compiler:compile-native '(lambda (x) x)))) 1)`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
    (multiple-value-call (lambda (a b) (values (lambda () (incf a)) (lambda () a) b))
      (values x 19))))))
    (multiple-value-bind (write read value) (funcall f 7)
      (and (= (funcall write) 8) (= (funcall read) 8) (= value 19))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (items)
    (multiple-value-call (lambda (&rest args) (when args (setf (car args) :changed)) args)
      (values-list items))))) (items (list 1 2 3)))
    (and (equal (funcall f items) '(:changed 2 3)) (equal items '(1 2 3))
         (null (funcall f nil))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda ()
    (multiple-value-call (lambda (a) a) (values))))))
    (handler-case (progn (funcall f) nil) (program-error () t)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda ()
    (multiple-value-call (lambda (a) a) (values 1 2))))))
    (handler-case (progn (funcall f) nil) (program-error () t)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda ()
    (flet ((apply (&rest args) (declare (ignore args)) :shadowed))
      (multiple-value-call (lambda (&rest args) args) (values 1 2)))))))
    (equal (funcall f) '(1 2)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda ()
    (block done (multiple-value-call (lambda (x) (declare (special x))
      (return-from done (values x 19))) (values 7)))))))
    (let ((x 100)) (declare (special x))
      (and (equal (multiple-value-list (funcall f)) '(7 19)) (= x 100))))`);
  for (const predicate of ['numberp', 'integerp', 'rationalp', 'realp', 'complexp', 'floatp',
    'symbolp', 'listp', 'stringp']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x) (if (${predicate} x) t nil)))))
      (every (lambda (x) (eq (funcall f x) (not (null (${predicate} x)))))
        '(nil 42 1/2 1.0f0 1.0d0 #c(1 2) :symbol (1 . 2) "text" #\\a #(1))))`);
  }
  for (const operator of ['=', '/=']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x y) (if (${operator} x y) t nil)))))
      (and (every (lambda (pair) (eq (apply f pair) (not (null (apply #'${operator} pair)))))
        '((42 42) (42 43) (1/2 0.5d0) (#c(2 0) 2.0f0) (#c(2 3) #c(2 4))
          (0.0d0 -0.0d0) (123456789012345678901 123456789012345678902)))
        (handler-case (progn (funcall f :bad 1) nil) (type-error () t))))`);
  }
  for (const accessor of ['caar', 'cadr', 'cdar', 'cddr', 'caaar', 'cadadr', 'cddddr', 'tenth']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x) (list (${accessor} x))))))
      (and (equal (funcall f nil) '(nil))
        (equal (funcall f '#1=(#1# . #1#)) (list (${accessor} '#1#)))
        (handler-case (progn (funcall f 19) nil) (type-error () t))))`);
  }
  for (const accessor of ['bit', 'sbit']) {
    check(`(let ((read (miricl-compiler:compile-native
                      '(lambda (a i) (if (= (${accessor} a i) 1) :one :zero))))
                (write (miricl-compiler:compile-native
                      '(lambda (a i value) (list (setf (${accessor} a i) value)))))
                (a (make-array 70 :element-type 'bit :initial-element 0)))
      (and (loop for i in '(0 1 31 32 63 69) always
             (and (eq (funcall read a i) :zero) (equal (funcall write a i 1) '(1))
                  (eq (funcall read a i) :one) (equal (funcall write a i 0) '(0))))
           (handler-case (progn (funcall read a -1) nil) (type-error () t))
           (handler-case (progn (funcall read a 70) nil) (type-error () t))
           (handler-case (progn (funcall read a 1073741823) nil) (type-error () t))
           (handler-case (progn (funcall read a 1073741824) nil) (type-error () t))
           (handler-case (progn (funcall write a -1 1) nil) (type-error () t))
           (handler-case (progn (funcall write a 70 1) nil) (type-error () t))
           (handler-case (progn (funcall write a 0 2) nil) (type-error () t))))`);
  }
  check(`(let* ((name (gensym "LATE-FUNCTION"))
                (f (miricl-compiler:compile-native
                    (list 'lambda '(run) (list 'when 'run
                      (list 'dotimes '(i 3) (list name 'i) (list name 'i)))))))
    (and (null (funcall f nil))
      (handler-case (progn (funcall f t) nil) (undefined-function () t))
      (progn (setf (symbol-function name) (lambda (x) x)) (null (funcall f t)))
      (progn (fmakunbound name)
        (handler-case (progn (funcall f t) nil) (undefined-function () t)))))`);
  check(`(progn (defun loop-current-definition (x) x)
    (let ((f (miricl-compiler:compile-native
      '(lambda () (loop for i below 4 collect
         (list (loop-current-definition i) (loop-current-definition i))
         do (when (= i 1) (setf (symbol-function 'loop-current-definition)
                               (lambda (x) (+ x 10)))))))))
      (equal (funcall f) '((0 0) (1 1) (12 12) (13 13)))))`);
  check(`(let* ((read (miricl-compiler:compile-native '(lambda (a i) (list (bit a i)))))
               (base (make-array 70 :element-type 'bit :initial-element 1))
               (view (make-array 35 :element-type 'bit :displaced-to base :displaced-index-offset 31
                                 :fill-pointer 0)))
    (setf (bit base 63) 0)
    (equal (funcall read view 32) '(0)))`);
  check(`(let* ((calls 0)
    (f (miricl-compiler:compile-native '(lambda (args)
      (apply (lambda (a b) (values (lambda () (incf a)) (lambda () a) b)) args)))))
    (multiple-value-bind (write read value) (funcall f (progn (incf calls) (list 7 19)))
      (let ((other (nth-value 1 (funcall f '(100 200)))))
        (and (= calls 1) (= value 19) (= (funcall write) 8)
          (= (funcall read) 8) (= (funcall other) 100)))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (args)
      (apply (lambda (a b) (+ a b)) args)))))
    (and (handler-case (progn (funcall f '(1)) nil) (program-error () t))
      (handler-case (progn (funcall f '(1 2 3)) nil) (program-error () t))
      (handler-case (progn (funcall f '(1 . 2)) nil) (type-error () t))
      (handler-case (progn (funcall f '#1=(1 . #1#)) nil) (type-error () t))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda () (apply (lambda () (values)) nil)))))
    (null (multiple-value-list (funcall f))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (args)
    (apply (lambda (&rest items) (values (length items) (lambda () items))) args)))))
    (and (multiple-value-bind (size read) (funcall f '(1 :two "three"))
      (and (= size 3) (equal (funcall read) '(1 :two "three"))))
      (= (funcall f nil) 0)
      (handler-case (progn (funcall f '(1 . 2)) nil) (type-error () t))
      (handler-case (progn (funcall f '#1=(1 . #1#)) nil) (type-error () t))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (args)
    (let ((count 0)) (values (apply (lambda (x) x) (progn (incf count) args)) count))))))
    (equal (multiple-value-list (funcall f '(19))) '(19 1)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (args)
    (block done
      (apply (lambda (x) "Documentation." (declare (special x))
        (return-from done (values x 19))) args))))))
    (let ((x 100)) (declare (special x))
      (and (equal (multiple-value-list (funcall f '(7))) '(7 19)) (= x 100))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (args)
    (flet ((apply (function arguments) (declare (ignore function arguments)) :local))
      (apply (lambda (x) x) args))))))
    (eq (funcall f '(7)) :local))`);
  for (let arity = 1; arity <= 4; arity++) {
    const parameters = ['a', 'b', 'c', 'd'].slice(0, arity).join(' ');
    const arguments_ = ['nil', ':symbol', "'(1 . 2)", '"text"'].slice(0, arity).join(' ');
    vm.evaluate(`(defparameter *fixed-callee* (miricl-compiler:compile-native
      '(lambda (${parameters}) (list ${parameters}))))
      (defparameter *fixed-caller* (miricl-compiler:compile-native
        '(lambda () (equal (funcall *fixed-callee* ${arguments_}) (list ${arguments_})))) )`);
    check('(funcall *fixed-caller*)');
    check(`(equal (apply *fixed-callee* (list ${arguments_})) (list ${arguments_}))`);
    vm.evaluate(`(setq *fixed-caller* (miricl-compiler:compile-native
      '(lambda () (handler-case (progn (funcall *fixed-callee*) nil) (program-error () t)))))`);
    check('(funcall *fixed-caller*)');
  }
  check(`(let* ((factory (miricl-compiler:compile-native '(lambda () (lambda () 17))))
                 (a (funcall factory)) (b (funcall factory)))
    (and (eq a b) (= (funcall a) 17)))`);
  check(`(progn (defparameter *closure-dynamic* 7)
    (let* ((factory (miricl-compiler:compile-native '(lambda () (lambda () *closure-dynamic*))))
           (a (let ((*closure-dynamic* 11)) (funcall factory)))
           (b (let ((*closure-dynamic* 19)) (funcall factory))))
      (and (eq a b) (= (funcall a) 7) (= (let ((*closure-dynamic* 13)) (funcall b)) 13))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (block done (funcall (lambda () (return-from done 17))))))))
    (and (= (funcall function) 17) (= (funcall function) 17)))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (lambda () (values (lambda () (incf x)) (lambda () x))))))
      (a (funcall factory 0)) (b (funcall factory 100))
      (write-a (nth-value 0 (funcall a))) (read-a (nth-value 1 (funcall a)))
      (write-b (nth-value 0 (funcall b))))
    (and (eq write-a (nth-value 0 (funcall a))) (eq read-a (nth-value 1 (funcall a)))
      (= (funcall write-a) 1) (= (funcall read-a) 1)
      (= (funcall write-b) 101) (= (funcall read-a) 1)))`);
  const moduleSize = Number(vm.evaluate(`(length (miricl-compiler:emit-native-module
    (miricl-compiler:compile-to-hir '(lambda (x) (if x (+ x 2) 0)))))`, {eof: true}).output.trim());
  assert.ok(moduleSize > 0);
  assert.match(vm.evaluate('(disassemble *native*)', {eof: true}).output,
    new RegExp(`Wasm module: ${moduleSize} bytes`));
  check(`(let* ((maker (miricl-compiler:compile-native '(lambda (x) (lambda () x))))
               (closure (funcall maker 42)))
    (and (= (funcall closure) 42)
      (= (miricl-compiler::compiler-primitive 26 maker)
         (miricl-compiler::compiler-primitive 26 closure))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &aux (before (lambda () x)) (x (1+ x))
                    (between (lambda () x)) (x (1+ x)))
       (list (funcall before) (funcall between) x)))))
    (equal (funcall function 40) '(40 41 42)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &rest tail) (values (car tail) (cdr tail) x)))))
    (and (equal (multiple-value-list (funcall function 1 2 3)) '(2 (3) 1))
         (equal (multiple-value-list (funcall function 1)) '(nil nil 1))
         (handler-case (progn (funcall function) nil) (program-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (let ((x (+ x 1)) (y x))
      (let* ((y (+ y x)) (z y)) (setq x (+ x z) y (+ x y)) (values x y z)))))))
    (equal (multiple-value-list (funcall function 10)) '(32 53 21)))`);
  check(`(let ((zero (miricl-compiler:compile-native '(lambda () (values))))
              (many (miricl-compiler:compile-native '(lambda (flag) (if flag (values nil 2 3) (values))))))
    (and (null (multiple-value-list (funcall zero)))
         (null (multiple-value-list (funcall many nil)))
         (equal (multiple-value-list (funcall many t)) '(nil 2 3))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (multiple-value-prog1 (values 1 2 3) (values 4 5))))))
    (equal (multiple-value-list (funcall function)) '(1 2 3)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (multiple-value-bind (a b c) (if x (values 1 2) (values))
                   (values a b c))))))
    (and (equal (multiple-value-list (funcall function t)) '(1 2 nil))
         (equal (multiple-value-list (funcall function nil)) '(nil nil nil))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (list (if x (values 1 2 3) (values))
                      (if x (values) (values nil 4)))))))
    (and (equal (funcall function t) '(1 nil)) (equal (funcall function nil) '(nil nil))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (case x (nil :never) ((nil) (values 1 2)) ((:a :b) (values)) ((t) 9) (otherwise 11))))))
    (and (equal (multiple-value-list (funcall function nil)) '(1 2))
         (null (multiple-value-list (funcall function :a)))
         (= (funcall function t) 9) (= (funcall function :other) 11)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (ecase x ((nil) 1) (t 2) (otherwise 3))))))
    (and (= (funcall function nil) 1) (= (funcall function t) 2) (= (funcall function 'otherwise) 3)
         (handler-case (progn (funcall function :missing) nil)
           (type-error (c) (eq (type-error-datum c) :missing)))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (flet ((values (x) (list x))) (values 42))))))
    (equal (funcall function) '(42)))`);
  check(`(let* ((literal (list 1 2 2 3))
    (function (miricl-compiler:compile-native (list 'lambda '(x) (list 'member 'x (list 'quote literal))))))
    (and (eq (funcall function 2) (cdr literal)) (eq (funcall function 1) literal)
         (null (funcall function 9)) (null (funcall function 2.0))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (flet ((member (a b) (declare (ignore a b)) :local)) (member x '(1 2)))))))
    (eq (funcall function 1) :local))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (multiple-value-bind (x y) (values (+ x 1) x :discarded)
                   (values x y))))))
    (equal (multiple-value-list (funcall function 41)) '(42 41)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (v) (setf (aref v 0) (values 42 :discarded)) (aref v 0)))))
    (= (funcall function (vector 0)) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (values (car x) (cdr x))))))
    (and (equal (multiple-value-list (funcall function '(1 . 2))) '(1 2))
         (equal (multiple-value-list (funcall function nil)) '(nil nil))
         (handler-case (progn (funcall function 42) nil) (type-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (list (car x)))))
              (original (symbol-function 'car)))
    (unwind-protect
      (progn (setf (symbol-function 'car) (lambda (x) (declare (ignore x)) :redefined))
             (equal (funcall function '(1)) '(:redefined)))
      (setf (symbol-function 'car) original)))`);
  check(`(let ((unit (miricl-compiler:compile-to-hir '(lambda (v) (setf (aref v 0) 42)))))
    (= (length (miricl-compiler:hir-unit-functions unit)) 1))`);
  check(`(progn (defun native-empty () (values))
    (let ((function (miricl-compiler:compile-native '(lambda () (list (native-empty) (floor 17 5))))))
      (equal (multiple-value-list (funcall function)) '((nil 3)))))`);
  check(`(progn
    (defparameter *native-events* nil)
    (defun native-event (x) (push x *native-events*) (values x :ignored))
    (let ((function (miricl-compiler:compile-native
      '(lambda () (list (native-event 1) (native-event 2) (native-event 3))))))
      (and (equal (funcall function) '(1 2 3)) (equal *native-events* '(3 2 1)))))`);
  check(`(progn
    (defun native-helper (x) (+ x 1))
    (defparameter *native-caller* (miricl-compiler:compile-native '(lambda (x) (native-helper x))))
    (defun native-helper (x) (+ x 2))
    (= (funcall *native-caller* 40) 42))`);
  check(`(let* ((literal (list "é𝄞"))
    (function (miricl-compiler:compile-native (list 'lambda nil (list 'quote literal)))))
    (and (eq (funcall function) literal) (eq (funcall function) (funcall function))))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (* x x)))))
    (= (funcall function 12345678901234567890) (* 12345678901234567890 12345678901234567890)))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (incf x) x))))
    (= (funcall function 41) 42))`);
  check(`(let ((factory (miricl-compiler:compile-native
    '(lambda (x) (values (lambda () (incf x)) (lambda () x))))))
    (multiple-value-bind (increment read) (funcall factory 41)
      (and (= (funcall increment) 42) (= (funcall read) 42)
           (= (funcall increment) 43) (= (funcall read) 43))))`);
  check(`(let* ((factory (miricl-compiler:compile-native '(lambda (x) (lambda () (lambda () x)))))
    (middle (funcall factory 42)) (inner (funcall middle))) (= (funcall inner) 42))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () x) (lambda () x)))))
      (object (list 1)))
    (multiple-value-bind (first second) (funcall factory object)
      (setf (car object) 42)
      (and (eq (funcall first) object) (eq (funcall second) object)
           (= (car (funcall second)) 42)
           (null (funcall (funcall factory nil))))))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (let ((read (lambda () x))) (setq x 42) read))))
      (read (funcall factory 0)))
    (= (funcall read) 42))`);
  check(`(let ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () (lambda () (incf x))) (lambda () x))))))
    (multiple-value-bind (middle read) (funcall factory 41)
      (let ((increment (funcall middle)))
        (and (= (funcall increment) 42) (= (funcall read) 42)
             (= (funcall increment) 43) (= (funcall read) 43)))))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (labels ((even (n) (if (zerop n) x (odd (1- n))))
                           (odd (n) (if (zerop n) nil (even (1- n)))))
                    #'even))))
      (even (funcall factory :yes)))
    (and (eq (funcall even 10) :yes) (null (funcall even 9))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (labels ((factorial (n) (if (= n 0) 1 (* n (factorial (1- n)))))) (factorial x))))))
    (= (funcall function 10) 3628800))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (multiple-value-call #'list (values 1 2) (values) (values nil 3))))))
    (equal (funcall function) '(1 2 nil 3)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &optional (y (+ x 1) supplied) (z (+ y 1)) &rest tail &aux (sum (+ x y z)))
       (values sum supplied tail)))))
    (and (equal (multiple-value-list (funcall function 1)) '(6 nil nil))
         (equal (multiple-value-list (funcall function 1 10)) '(22 t nil))
         (equal (multiple-value-list (funcall function 1 10 20 30 40)) '(31 t (30 40)))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (&optional (x (error "Default must be skipped") supplied)) (list x supplied)))))
    (and (equal (funcall function nil) '(nil t))
         (handler-case (progn (funcall function 1 2) nil) (program-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (block outer (block inner (return-from outer (values 1 2))) (error "Unreachable"))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (flet ((local (x) (declare (ignore x)) (return-from local (values 1 2)))) (local 0))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () ((lambda (x) (the (values integer integer) (values x 2))) 1)))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (the (values) (values))))))
    (null (multiple-value-list (funcall function))))`);
  check(`(progn (defmacro native-shadow (x) (declare (ignore x)) :global)
    (let ((function (miricl-compiler:compile-native
      '(lambda () (flet ((native-shadow (x) (+ x 1))) (native-shadow 41))))))
      (= (funcall function) 42)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (eval-when (:compile-toplevel :load-toplevel) (error "Must not execute"))
                (eval-when (:execute) (values 1 2))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((sum 0)) (tagbody (go next) (error "Unreachable")
      loop (incf sum) next (if (< sum 10) (go loop))) sum)))))
    (= (funcall function) 10))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((sum 0)) (tagbody out (tagbody (go done) out (error "Shadowed") done)
      (incf sum) (if (< sum 3) (go out))) sum)))))
    (= (funcall function) 3))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (n) (let ((sum 0)) (dotimes (i n sum) (incf sum i)))))))
    (= (funcall function 10) 45))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (macrolet ((twice (form) (list '+ form form))) (twice x))))))
    (= (funcall function 21) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (symbol-macrolet ((first (car x))) (setq first 42) first)))))
    (let ((cell (list 0))) (and (= (funcall function cell) 42) (= (car cell) 42))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (symbol-macrolet ((x (error "Shadowed"))) (let ((x 42)) x))))))
    (= (funcall function 0) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () ((lambda (x y) (declare (ignore x)) "doc" (declare (ignore y)) "value") 1 2)))))
    (equal (funcall function) "value"))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (destructuring-bind (&whole (a . b) c . d) '(1 . 2) (list a b c d))))))
    (equal (funcall function) '(1 2 1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((x 0)) (macrolet ((local-tag () 'tag))
      (tagbody (tagbody (go tag) (local-tag) (setq x 1)) tag)) x)))))
    (= (funcall function) 0))`);
  check(`(= (funcall (miricl-compiler:compile-native
    '(lambda () (block escaped (funcall (lambda () (return-from escaped 1))))))) 1)`);
  check(`(handler-case (progn (funcall *native* 1 2) nil) (program-error () t))`);
  check(`(progn
    (defparameter *native-error* (miricl-compiler:compile-native '(lambda () (/ 1 0)) :name 'native-error))
    (handler-case (progn (funcall *native-error*) nil) (division-by-zero () t)))`);
  check(`(multiple-value-bind (expression closure name) (function-lambda-expression *native-error*)
    (and (null expression) closure (eq name 'native-error)))`);
  assert.throws(() => vm.evaluate('(funcall *native-error*)'), error =>
    error instanceof MiriclError && error.backtrace.includes('NATIVE-ERROR'));
  check(`(let ((function (miricl-compiler:compile-native '(lambda () (/ 1 0) 42))))
    (handler-case (progn (funcall function) nil) (division-by-zero () t)))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda () native-unbound-variable 42))))
    (handler-case (progn (funcall function) nil) (unbound-variable () t)))`);
  check(`(multiple-value-bind (bytes literals)
    (miricl-compiler:emit-native-module
      (miricl-compiler:compile-to-hir '(lambda () "Discarded literal" 42)))
    (and (< (length bytes) 250) (equalp literals #(42))))`);
  check('(= (funcall *native* 40) 42)');
  check(`(multiple-value-bind (bytes literals)
    (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '(lambda (x) (+ x 2))))
    (with-open-file (s "/native-code.wasm" :direction :output :element-type '(unsigned-byte 8))
      (write-sequence bytes s)) (and (vectorp bytes) (vectorp literals)))`);
  const bytes = await vm.readFile('/native-code.wasm');
  assert.ok(WebAssembly.validate(bytes));
  const imports = WebAssembly.Module.imports(await WebAssembly.compile(bytes));
  assert.ok(imports.length > 0 && imports.every(x => x.module === 'miricl.runtime'));
  assert.equal(imports.some(x => /native-cell|native-close|native-capture/.test(x.name)), false,
    'Uncaptured variables must not import heap-cell or closure operations.');
  assert.ok(bytes.length < 600, `Small native module grew to ${bytes.length} bytes.`);
  for (const source of ['(lambda (x) (lambda () x))', '(lambda (x) (lambda () (lambda () x)))']) {
    check(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (s "/native-captures.wasm" :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8)) (write-sequence bytes s)) t)`);
    const module = await WebAssembly.compile(await vm.readFile('/native-captures.wasm'));
    assert.equal(WebAssembly.Module.imports(module).some(x => /^native-cell/.test(x.name)), false,
      'Unassigned captures must carry the original value without allocating or reading binding cells.');
  }
  assert.equal((await vm.evaluateAsync(`(progn (defparameter *native-loop-started* nil)
    (let ((worker (miricl-threads:make-thread (miricl-compiler:compile-native
        '(lambda () (setq *native-loop-started* t) (loop))))))
      (loop until *native-loop-started* do (miricl-threads:thread-yield))
      (miricl-threads:destroy-thread worker)
      (handler-case (progn (miricl-threads:join-thread worker) nil)
        (miricl-threads:abnormal-exit () (not (miricl-threads:thread-alive-p worker))))))`)).output, 'T\n',
    'Compiled loops without calls must retain a scheduling safepoint.');
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (x &optional (y 2) &rest rest &key (z (+ x y) supplied) &allow-other-keys &aux (sum (+ x y z)))
         (values sum supplied rest)))))
    (and (equal (multiple-value-list (funcall function 10)) '(24 nil nil))
         (equal (multiple-value-list (funcall function 10 3 :z 4 :ignored 5))
                '(17 t (:z 4 :ignored 5)))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (&key ((custom-key x) 7 xp) (y (+ x 1))) (values x y xp)))))
    (and (equal (multiple-value-list (funcall function)) '(7 8 nil))
         (equal (multiple-value-list (funcall function 'custom-key nil :y 9)) '(nil 9 t))
         (equal (multiple-value-list (funcall function 'custom-key 1 'custom-key 2)) '(1 2 t))
         (handler-case (progn (funcall function :unknown 1) nil) (program-error () t))
         (handler-case (progn (funcall function :y) nil) (program-error () t))
         (equal (multiple-value-list (funcall function :unknown 1 :allow-other-keys t)) '(7 8 nil))))`);
  check(`(progn (defparameter *native-special* 1) (defun native-read-special () *native-special*)
    (let ((function (miricl-compiler:compile-native
      '(lambda () (values (let ((*native-special* 7))
                            (list *native-special* (native-read-special) (symbol-value '*native-special*)))
                          *native-special*)))))
      (and (equal (multiple-value-list (funcall function)) '((7 7 7) 1)) (= *native-special* 1))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (*native-special*)
        (values *native-special* (let ((*native-special* 3) (previous *native-special*)) previous)
          (let* ((*native-special* 4) (previous *native-special*)) previous) *native-special*)))))
    (and (equal (multiple-value-list (funcall function 2)) '(2 2 4 2)) (= *native-special* 1)))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (values (block done (let ((*native-special* 8)) (return-from done *native-special*)))
                    (progn (tagbody (let ((*native-special* 9)) (go done)) done) *native-special*))))))
    (and (equal (multiple-value-list (funcall function)) '(8 1)) (= *native-special* 1)))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((native-local-special 42)) (declare (special native-local-special))
                    (symbol-value 'native-local-special))))))
    (and (= (funcall function) 42) (not (boundp 'native-local-special))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((*native-special* 9)) (error "Unwind native specials"))))))
    (and (handler-case (progn (funcall function) nil) (error () (= *native-special* 1)))
         (= *native-special* 1)))`);
  check(`(let ((package *package*) (function (miricl-compiler:compile-native
      '(lambda () (let ((*package* (find-package "KEYWORD"))) (symbol-package (intern "NATIVE-KEYWORD")))))))
    (and (eq (funcall function) (find-package "KEYWORD")) (eq *package* package)))`);
  assert.equal((await vm.evaluateAsync(`
    (let* ((function (miricl-compiler:compile-native '(lambda (box)
              (setf (cdr box) t)
              (loop repeat 1000000 until (car box))
              (values (car box) (cdr box)))))
           (box (cons nil nil))
           (worker (miricl-threads:make-thread
             (lambda () (loop until (cdr box) do (miricl-threads:thread-yield))
               (setf (car box) 19)))))
      (and (equal (multiple-value-list (funcall function box)) '(19 t))
           (= (miricl-threads:join-thread worker) 19)))`, {eof: true})).output, 'T\n',
    'Native loop checkpoints must let a waiting thread run and preserve multiple values.');
  console.log(`Native compilation: ${bytes.length}-byte module; arithmetic, branches, lexical cells, rest arguments, macros, literal identity, multiple values, redefinition, arity, conditions and thread preemption pass.`);
} finally {vm.close();}
