// Deep native tail calls must not grow the Wasm stack. Check dispatch and
// Common Lisp value/extent semantics as well as successful deep recursion.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(fs.readFileSync(process.argv[2] ?? 'miricl-precompiled.wasm'));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);
const define = (name, source) => evaluate(`(setf (symbol-function '${name})
  (miricl-compiler:compile-native '${source} :name '${name})) (values)`);
try {
  evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  define('tail-count', `(lambda (n acc)
    (if (zerop n) (values acc nil :done) (tail-count (1- n) (1+ acc))))`);
  check("(equal (multiple-value-list (tail-count 100000 0)) '(100000 nil :done))");
  console.log('PASS deep self tail calls and multiple values');

  define('tail-even', '(lambda (n) (if (zerop n) t (tail-odd (1- n))))');
  define('tail-odd', '(lambda (n) (if (zerop n) nil (tail-even (1- n))))');
  check('(and (tail-even 100000) (not (tail-odd 100000)))');
  define('tail-indirect', `(lambda (n function)
    (if (zerop n) (values 42 :indirect) (funcall function (1- n) function)))`);
  check("(equal (multiple-value-list (tail-indirect 100000 #'tail-indirect)) '(42 :indirect))");
  check("(equal (multiple-value-list (tail-indirect 100000 'tail-indirect)) '(42 :indirect))");
  console.log('PASS mutual recursion and function/symbol designators');

  define('tail-apply', `(lambda (n &rest values)
    (if (zerop n) (values-list values)
        (apply #'tail-apply (1- n) values)))`);
  check("(equal (multiple-value-list (tail-apply 100000 17 nil 23)) '(17 nil 23))");
  define('tail-via-apply-function', `(lambda (n dispatcher)
    (if (zerop n) (values)
        (funcall dispatcher #'tail-via-apply-function (1- n) (list dispatcher))))`);
  check("(null (multiple-value-list (tail-via-apply-function 100000 #'apply)))");
  console.log('PASS APPLY, spread arguments and zero values');

  define('tail-multiple-value-call', `(lambda (n)
    (if (zerop n) (values 17 nil 23)
      (multiple-value-call #'tail-multiple-value-call (values (1- n)) (values))))`);
  check("(equal (multiple-value-list (tail-multiple-value-call 100000)) '(17 nil 23))");
  define('tail-keywords', `(lambda (n &key (value 17))
    (if (zerop n) value (tail-keywords (1- n) :value (1+ value))))`);
  check('(= (tail-keywords 10000 :value 7) 10007)');
  define('tail-primary', '(lambda (n) (values (tail-count n 0)))');
  check("(equal (multiple-value-list (tail-primary 100000)) '(100000))");
  // Connecting the debugger must not inhibit optimized, non-debug functions.
  vm.vm['debugger-connected'].value = 1;
  try {check('(= (tail-count 100000 0) 100000)');}
  finally {vm.vm['debugger-connected'].value = 0;}
  console.log('PASS MULTIPLE-VALUE-CALL, keyword arguments and primary-value truncation');

  define('tail-closure-factory', `(lambda (offset)
    (labels ((again (n acc) (if (zerop n) (+ offset acc) (again (1- n) (1+ acc)))))
      #'again))`);
  check('(= (funcall (tail-closure-factory 7) 100000 0) 100007)');
  evaluate('(defparameter *tail-redefined* nil) (values)');
  define('tail-redefined', `(lambda (n)
    (if (zerop n) :original
      (progn (when (= n 1) (setf (symbol-function 'tail-redefined) (lambda (x) (list :new x))))
             (tail-redefined (1- n)))))`);
  check("(equal (tail-redefined 3) '(:new 0))");
  define('tail-order', '(lambda (f a b) (funcall f a b))');
  check(`(let ((order nil))
    (and (equal (tail-order (progn (push 1 order) #'list)
                  (progn (push 2 order) :a) (progn (push 3 order) :b)) '(:a :b))
      (equal order '(3 2 1))))`);
  console.log('PASS closures, global redefinition and argument order');

  evaluate('(defparameter *tail-special* :outer) (defparameter *tail-cleanup* nil) (values)');
  define('tail-observe-special', '(lambda () *tail-special*)');
  define('tail-special', '(lambda () (let ((*tail-special* :inner)) (tail-observe-special)))');
  check('(and (eq (tail-special) :inner) (eq *tail-special* :outer))');
  define('tail-special-branch', `(lambda (n)
    (block finished
      (if (zerop n)
          (let ((*tail-special* :inner)) (tail-observe-special))
          (tail-special-branch (1- n)))))`);
  check('(and (eq (tail-special-branch 100000) :inner) (eq *tail-special* :outer))');
  define('tail-after-special', `(lambda (n)
    (let ((*tail-special* :inner)) (tail-observe-special))
    (if (zerop n) (tail-observe-special) (tail-after-special (1- n))))`);
  check('(eq (tail-after-special 100000) :outer)');
  define('tail-captured-branch', `(lambda (n)
    (if (zerop n)
        (block finished (funcall (lambda () (return-from finished (values 31 :branch)))))
        (multiple-value-call #'tail-captured-branch (values (1- n)) (values))))`);
  check("(equal (multiple-value-list (tail-captured-branch 100000)) '(31 :branch))");
  define('tail-cleanup', `(lambda (n)
    (unwind-protect (tail-count n 0) (push :cleaned *tail-cleanup*)))`);
  check(`(and (equal (multiple-value-list (tail-cleanup 100000)) '(100000 nil :done))
    (equal *tail-cleanup* '(:cleaned)))`);
  define('tail-throw', '(lambda (n) (if (zerop n) (throw :tail-exit (values 17 23)) (tail-throw (1- n))))');
  check("(equal (multiple-value-list (catch :tail-exit (tail-throw 100000))) '(17 23))");
  define('tail-block-exit', `(lambda (n)
    (block finished
      (labels ((walk (remaining)
                 (if (zerop remaining) (return-from finished (values 31 :block))
                     (walk (1- remaining)))))
        (walk n))))`);
  check("(equal (multiple-value-list (tail-block-exit 100000)) '(31 :block))");
  define('tail-error', '(lambda (n) (if (zerop n) (error "Tail error") (tail-error (1- n))))');
  check('(handler-case (tail-error 100000) (simple-error () t))');
  check(`(and (handler-case (tail-order #'car 1 2) (program-error () t))
    (handler-case (tail-indirect 1 'no-such-tail-function) (undefined-function () t))
    (handler-case (apply #'tail-apply 1 '(17 . 23)) (type-error () t))
    (= (tail-count 17 0) 17))`);
  console.log('PASS path-specific dynamic scopes, cleanup, nonlocal exits and error recovery');

  check(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '(lambda (f x) (funcall f x))))
    (declare (ignore literals))
    (with-open-file (stream "/tail-call.wasm" :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede) (write-sequence bytes stream)) t)`);
  const code = await vm.readFile('/tail-call.wasm');
  const module = new WebAssembly.Module(code);
  assert.ok(WebAssembly.Module.imports(module).some(entry => entry.name === 'native-tail-call'));
  check(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir
        '(lambda (f x) (declare (optimize (debug 2))) (funcall f x))))
    (declare (ignore literals))
    (with-open-file (stream "/debug-call.wasm" :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede) (write-sequence bytes stream)) t)`);
  const debug = new WebAssembly.Module(await vm.readFile('/debug-call.wasm'));
  assert.ok(!WebAssembly.Module.imports(debug).some(entry => entry.name === 'native-tail-call'));
  console.log('General native tail calls pass without experimental flags.');
} finally {vm.close();}
