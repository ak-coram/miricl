// Observable compiler capabilities beyond ANSI conformance. WASI cannot load
// generated code at runtime and is exempt from this entire optimization suite.
// node tests/optimizations.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const [image = 'miricl.wasm', ...extra] = process.argv.slice(2);
if (extra.length) throw new Error('Expected [native IMAGE.wasm]; WASI optimization tests are excluded.');
const vm = await Miricl.instantiate(await fs.readFile(image));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = (name, source) => assert.equal(evaluate(source), 'T\n', name);
const define = (name, source) => evaluate(`(setf (symbol-function '${name})
  (miricl-compiler:compile-native '${source} :name '${name})) (values)`);
// This depth exceeds ordinary host stacks. Success, not a timing or code-shape
// threshold, establishes the tail-call capability.
const depth = 100000;
const policy = '(declare (optimize (speed 1) (space 3) (debug 0) (safety 1)))';
try {
  evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  define('optimization-tail-count', `(lambda (n acc) ${policy}
    (if (zerop n) (values acc nil :done)
      (optimization-tail-count (1- n) (1+ acc))))`);
  check('deep self tail calls preserve multiple values',
    `(equal (multiple-value-list (optimization-tail-count ${depth} 0)) '(${depth} nil :done))`);
  define('optimization-tail-even', `(lambda (n) ${policy}
    (if (zerop n) t (optimization-tail-odd (1- n))))`);
  define('optimization-tail-odd', `(lambda (n) ${policy}
    (if (zerop n) nil (optimization-tail-even (1- n))))`);
  check('mutual tail calls', `(and (optimization-tail-even ${depth})
    (not (optimization-tail-odd ${depth})))`);
  define('optimization-tail-four', `(lambda (n acc token empty) ${policy}
    (if (zerop n)
        (if empty (values) (values acc nil token :four 5 6 7 8 9))
        (optimization-tail-four (1- n) (1+ acc) token empty)))`);
  check('deep four-argument calls preserve zero and overflow values', `(and
    (equal (multiple-value-list (optimization-tail-four ${depth} 0 :retained nil))
           '(${depth} nil :retained :four 5 6 7 8 9))
    (null (multiple-value-list (optimization-tail-four ${depth} 0 :retained t))))`);
  define('optimization-tail-indirect', `(lambda (n function) ${policy}
    (if (zerop n) (values 42 :indirect) (funcall function (1- n) function)))`);
  check('indirect tail calls through function and symbol designators', `(and
    (equal (multiple-value-list (optimization-tail-indirect ${depth} #'optimization-tail-indirect)) '(42 :indirect))
    (equal (multiple-value-list (optimization-tail-indirect ${depth} 'optimization-tail-indirect)) '(42 :indirect)))`);
  // File preparation must retain indirect tail calls and the ordinary SETF
  // calling convention when its resolved forms become native Wasm.
  vm.mountFile('/optimization-prepared.lisp', `
    (in-package :cl-user)
    (defun optimization-prepared-tail (n function) ${policy}
      (if (zerop n) (values 42 nil :prepared)
        (funcall function (1- n) function)))
    (defun optimization-prepared-call (function) ${policy}
      (let ((effects nil))
        (values (multiple-value-list
          (funcall (progn (push :function effects) (values function :ignored))
            (progn (push :argument effects) (values 40 :ignored))))
          (reverse effects))))
    (defun optimization-prepared-store (bits) ${policy}
      (let ((effects nil))
        (let ((value (setf (sbit (progn (push :array effects) bits)
                           (progn (push :index effects) 0))
                      (progn (push :value effects) 1))))
          (values value (reverse effects)))))
    (defun optimization-prepared-shadow () ${policy}
      (flet ((funcall (x) (values (+ x 2) :shadow))) (funcall 40)))`);
  evaluate(`(compile-file "/optimization-prepared.lisp" :verbose nil :block-compile nil)
    (miricl-compiler:compile-native-bundle "/optimization-prepared.wasm" "/optimization-prepared-native.wasm")
    (load "/optimization-prepared-native.wasm") (values)`);
  check('prepared indirect tail calls preserve depth and multiple values', `(and
    (equal (multiple-value-list (optimization-prepared-tail ${depth} #'optimization-prepared-tail)) '(42 nil :prepared))
    (equal (multiple-value-list (optimization-prepared-tail ${depth} 'optimization-prepared-tail)) '(42 nil :prepared)))`);
  check('prepared indirect calls preserve operands, values and lexical shadowing', `(and
    (equal (multiple-value-list (optimization-prepared-call (lambda (x) (values (+ x 2) nil :tail))))
      '((42 nil :tail) (:function :argument)))
    (equal (multiple-value-list (optimization-prepared-call (lambda (x) (declare (ignore x)) (values))))
      '(nil (:function :argument)))
    (equal (multiple-value-list (optimization-prepared-shadow)) '(42 :shadow))
    (let ((bits (make-array 1 :element-type 'bit :initial-element 0)))
      (and (equal (multiple-value-list (optimization-prepared-store bits)) '(1 (:array :index :value)))
           (= (sbit bits 0) 1))))`);
  define('optimization-tail-factory', `(lambda (offset) ${policy}
    (labels ((walk (n acc) (if (zerop n) (+ offset acc) (walk (1- n) (1+ acc))))) #'walk))`);
  check('tail-recursive escaped closure',
    `(= (funcall (optimization-tail-factory 7) ${depth} 0) (+ ${depth} 7))`);
  console.log('Optimizations: deep self, mutual, indirect and closure tail calls pass.');

  evaluate(`(defparameter *optimization-generic-remaining* 0)
    (defparameter *optimization-generic-empty* nil)
    (defgeneric optimization-generic-zero ())
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-zero () ${policy}
        (if (zerop *optimization-generic-remaining*)
            (if *optimization-generic-empty* (values) (values 17 nil :generic))
            (progn (decf *optimization-generic-remaining*)
                   (optimization-generic-zero-step))))))) (values)`);
  define('optimization-generic-zero-step', `(lambda () ${policy}
    (optimization-generic-zero))`);
  check('deep zero-argument generic calls preserve zero and multiple values', `(and
    (let ((*optimization-generic-remaining* ${depth}) (*optimization-generic-empty* nil))
      (and (equal (multiple-value-list (optimization-generic-zero-step)) '(17 nil :generic))
           (zerop *optimization-generic-remaining*)))
    (let ((*optimization-generic-remaining* ${depth}) (*optimization-generic-empty* t))
      (and (null (multiple-value-list (optimization-generic-zero-step)))
           (zerop *optimization-generic-remaining*))))`);

  evaluate(`(defgeneric optimization-generic-context (n token))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-context ((n number) token) ${policy}
        (values n token :next))
      (defmethod optimization-generic-context ((n integer) token) ${policy}
        (if (zerop n)
            (values (lambda () (call-next-method)) :context)
            (optimization-generic-context-step (1- n) token)))))) (values)`);
  define('optimization-generic-context-step', `(lambda (n token) ${policy}
    (funcall #'optimization-generic-context n token))`);
  check('deep indirect generic calls retain escaping next-method context',
    `(multiple-value-bind (next marker) (optimization-generic-context-step ${depth} :retained)
       (and (eq marker :context)
            (equal (multiple-value-list (funcall next)) '(0 :retained :next))))`);
  console.log('Optimizations: deep generic calls preserve values and escaping next-method context.');

  evaluate(`(defgeneric optimization-generic-three (n acc token))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-three ((n integer) acc token) ${policy}
        (if (zerop n) (values acc nil token :generic :three)
          (optimization-generic-three-step (1- n) (1+ acc) token)))))) (values)`);
  define('optimization-generic-three-step', `(lambda (n acc token) ${policy}
    (apply #'optimization-generic-three n (list acc token)))`);
  check('deep three-argument generic calls through APPLY preserve overflow values',
    `(equal (multiple-value-list (optimization-generic-three-step ${depth} 0 :retained))
            '(${depth} nil :retained :generic :three))`);

  evaluate(`(defgeneric optimization-generic-one (n))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-one ((n integer)) ${policy}
        (if (zerop n) :original (optimization-generic-one-step (1- n))))))) (values)`);
  define('optimization-generic-one-step', `(lambda (n) ${policy}
    (optimization-generic-one n))`);
  check('deep unary generic calls', `(eq (optimization-generic-one-step ${depth}) :original)`);
  evaluate(`(funcall (miricl-compiler:compile-native '(lambda ()
    (defmethod optimization-generic-one ((n integer)) ${policy}
      (if (zerop n) (values :replacement nil :current)
        (optimization-generic-one-step (1- n))))))) (values)`);
  check('warmed generic tail calls observe method replacement', `(equal
    (multiple-value-list (optimization-generic-one-step ${depth})) '(:replacement nil :current))`);
  console.log('Optimizations: generic tail APPLY, overflow values and live method replacement pass.');

  define('optimization-tail-apply', `(lambda (n &rest results) ${policy}
    (if (zerop n) (values-list results) (apply #'optimization-tail-apply (1- n) results)))`);
  check('tail APPLY preserves value groups', `(equal
    (multiple-value-list (optimization-tail-apply ${depth} 17 nil 23)) '(17 nil 23))`);
  define('optimization-tail-values', `(lambda (n) ${policy}
    (if (zerop n) (values)
      (multiple-value-call #'optimization-tail-values (values (1- n)) (values))))`);
  check('tail MULTIPLE-VALUE-CALL preserves zero values',
    `(null (multiple-value-list (optimization-tail-values ${depth})))`);
  define('optimization-tail-exit', `(lambda (n) ${policy}
    (if (zerop n) (throw :optimization-exit (values 17 :exit))
      (optimization-tail-exit (1- n))))`);
  check('outer dynamic extent survives deep tail calls', `(let ((cleaned nil))
    (and (equal (multiple-value-list
      (catch :optimization-exit
        (unwind-protect (optimization-tail-exit ${depth}) (setf cleaned t)))) '(17 :exit))
      cleaned))`);
  console.log('Optimizations: tail dispatch preserves value groups, nonlocal exits and outer cleanup.');

  // Retaining many cheap function references must not inflate every suspended
  // recursive frame. Exercise actual recursive completion and effects through
  // the ordinary async adapter, without prescribing caches, locals or opcodes.
  const names = Array.from({length: 80}, (_, i) => `optimization-literal-effect-${i}`);
  const repetitions = 8, recursiveDepth = 2048;
  const calls = Array.from({length: repetitions}, () => names.map(name => `(${name} n)`).join(' ')).join('\n');
  evaluate('(defparameter *optimization-literal-effects* 0) (values)');
  define('optimization-literal-effect', `(lambda (x) (declare (ignore x))
    (incf *optimization-literal-effects*) nil)`);
  evaluate(`(dolist (name '(${names.join(' ')}))
    (setf (symbol-function name) #'optimization-literal-effect)) (values)`);
  define('optimization-literal-recur', `(lambda (n)
    (declare (type (integer 0 4096) n) (inline zerop 1- 1+)
      (optimize (safety 0) (debug 0)))
    (if (zerop n) 0
      (let ((answer (optimization-literal-recur (1- n)))) ${calls} (1+ answer))))`);
  const pressure = await vm.evaluateAsync(`(let ((answer (optimization-literal-recur ${recursiveDepth})))
    (and (= answer ${recursiveDepth})
      (= *optimization-literal-effects* ${recursiveDepth * names.length * repetitions})))`, {eof: true});
  assert.equal(pressure.output, 'T\n', 'Literal-heavy retained frames preserve recursion capacity and effects.');
  console.log('Optimizations: literal-heavy non-tail recursion completes without excessive retained-frame pressure.');

  // Repeated substantial regions should pay for their private helpers while
  // retaining callback effects, ordinary primary values and the final values.
  check('module outlining shares large regions without changing effects or values', `(let*
    ((definitions (loop for i below 10 collect
       (list 'defun (intern (format nil "OPTIMIZATION-OUTLINE-~D" i)) '(x callback)
         '(declare (optimize (space 3) (speed 1) (debug 0)))
         (list 'let* '((a (funcall callback x x x x x x))
                       (b (funcall callback a a a a a a))
                       (c (funcall callback b b b b b b)))
           (list 'values 'c i)))))
     (plain (let ((miricl-compiler::*native-outlining-budget* 0))
       (multiple-value-list (miricl-compiler:compile-native-module definitions :inline nil))))
     (shared (multiple-value-list (miricl-compiler:compile-native-module definitions :inline nil))))
    (and (< (length (second shared)) (length (second plain)))
      (loop for variant in (list plain shared) always
        (loop for entry in (first variant) for i from 0 always
          (let ((effects nil))
            (and (equal (multiple-value-list
                   (funcall (cdr entry) 40
                     (lambda (&rest arguments)
                       (push (first arguments) effects)
                       (values (apply #'+ arguments) :discarded))))
                 (list 8640 i))
                 (equal effects '(1440 240 40))))))))`);
  console.log('Optimizations: whole-module outlining reduces repeated code and preserves callback effects and multiple values.');

  // Declared hot paths deliberately exercise inlining and scalar lowering.
  // The assertions concern legal input, effects and values, not a prescribed
  // lowering strategy or the optimizer's private representation.
  define('optimization-inlined-capture', `(lambda (initial)
    (declare (optimize (speed 3) (space 1)))
    (let ((state initial))
      (flet ((advance (amount) (incf state amount)))
        (declare (inline advance))
        (let ((observe (lambda () state)))
          (values (advance 2) (funcall observe) (advance 3) (funcall observe))))))`);
  check('inlined writes retain shared closure cells', `(equal
    (multiple-value-list (optimization-inlined-capture 7)) '(9 9 12 12))`);
  define('optimization-inlined-check', `(lambda (v i)
    (declare (optimize (speed 3) (space 1) (safety 0)))
    (flet ((checked (v i)
      (declare (optimize (safety 3)) (type simple-vector v) (fixnum i)) (svref v i)))
      (declare (inline checked)) (checked v i)))`);
  check('inlining preserves the callee safety contract', `(and
    (= (optimization-inlined-check #(42) 0) 42)
    (handler-case (progn (optimization-inlined-check #(42) 1) nil) (type-error () t))
    (= (optimization-inlined-check #(43) 0) 43))`);

  define('optimization-mutable-loop', `(lambda (cell n)
    (declare (optimize (speed 3) (space 1) (safety 0))
      (type cons cell) (type (integer 0 20) n) (inline car not consp))
    (let ((sum 0) (effects 0))
      (dotimes (i n)
        (when (car (progn (incf effects) cell)) (incf sum))
        (setf (car cell) (not (car cell))))
      (values sum effects (car cell))))`);
  check('loop motion and reuse respect heap mutation and operand effects', `(and
    (equal (multiple-value-list (optimization-mutable-loop (list t) 0)) '(0 0 t))
    (equal (multiple-value-list (optimization-mutable-loop (list t) 5)) '(3 5 nil))
    (equal (multiple-value-list (optimization-mutable-loop (list nil) 6)) '(3 6 nil)))`);
  console.log('Optimizations: inlined captures and safety, and mutation/effect-sensitive loops pass.');
} finally {vm.close();}
