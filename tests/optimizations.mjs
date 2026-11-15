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
  define('optimization-tail-indirect', `(lambda (n function) ${policy}
    (if (zerop n) (values 42 :indirect) (funcall function (1- n) function)))`);
  check('indirect tail calls through function and symbol designators', `(and
    (equal (multiple-value-list (optimization-tail-indirect ${depth} #'optimization-tail-indirect)) '(42 :indirect))
    (equal (multiple-value-list (optimization-tail-indirect ${depth} 'optimization-tail-indirect)) '(42 :indirect)))`);
  define('optimization-tail-factory', `(lambda (offset) ${policy}
    (labels ((walk (n acc) (if (zerop n) (+ offset acc) (walk (1- n) (1+ acc))))) #'walk))`);
  check('tail-recursive escaped closure',
    `(= (funcall (optimization-tail-factory 7) ${depth} 0) (+ ${depth} 7))`);
  console.log('Optimizations: deep self, mutual, indirect and closure tail calls pass.');

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
