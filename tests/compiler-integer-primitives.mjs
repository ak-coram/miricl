import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const parseContracts = process.argv.includes('--parse-contracts');
const sourceCompiler = process.argv.includes('--source') && !parseContracts;
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const quoteLisp = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
const check = source => assert.equal(vm.evaluate(parseContracts
  ? `(with-input-from-string (in ${quoteLisp(source)})
       (let ((end (gensym))) (loop for form = (read in nil end) until (eq form end) finally (return t))))`
  : source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('hir-types.lisp', 'utf8'));
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    for (const [operator, parameters, cases] of [
      ['integer-length', 'x', `((0 0) (-1 0) (1 1) (-4 2) (9223372036854775808 64)
        (-9223372036854775809 64) (1267650600228229401496703205376 101))`],
      ['ash', 'x n', `((1 64 18446744073709551616) (-1 64 -18446744073709551616)
        (-17 -2 -5) (1267650600228229401496703205376 -95 32)
        (-1 -1267650600228229401496703205376 -1) (1 -1267650600228229401496703205376 0))`],
      ['logand', 'x y', `((0 -1 0) (-1 18446744073709551616 18446744073709551616)
        (-18446744073709551616 18446744073709551615 0)
        (1267650600228229401496703205375 18446744073709551615 18446744073709551615))`],
    ]) {
      check(`(let ((f (miricl-compiler:compile-native
          '(lambda (${parameters}) (declare (optimize (safety ${safety})) (inline ${operator}))
             (${operator} ${parameters})))))
        (every (lambda (test)
          (equal (multiple-value-list (apply f (butlast test))) (list (car (last test))))) '${cases}))`);
      check(`(let ((f (miricl-compiler:compile-native
          '(lambda (${parameters}) (declare (optimize (safety ${safety})) (inline ${operator}))
             (${operator} ${parameters}) :done))))
        (and (handler-case (progn (funcall f :bad ${parameters.includes(' ') ? '0' : ''}) nil) (type-error () t))
             ${parameters.includes(' ') ? '(handler-case (progn (funcall f 0 :bad) nil) (type-error () t))' : 't'}))`);
    }
  }
  check(`(let ((events nil)
        (f (miricl-compiler:compile-native '(lambda (callback)
             (declare (inline ash logand integer-length))
             (values (integer-length (ash (funcall callback :value -17) (funcall callback :count -2)))
                     (logand (funcall callback :left -1) (funcall callback :right 255)))))))
    (and (equal (multiple-value-list (funcall f (lambda (event value)
                  (push event events) (values value :extra)))) '(3 255))
         (equal (reverse events) '(:value :count :left :right))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (flag value)
             (declare (inline integer-length ash logand))
             (if flag (logand (ash value -1) (integer-length value)) :skipped)))))
    (and (eq :skipped (funcall f nil :bad))
         (handler-case (progn (funcall f t :bad) nil) (type-error () t))
         (= 0 (funcall f t 3))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (value)
             (declare (inline integerp ash logand))
             (and (integerp (ash value 0)) (integerp (logand 0 value)))))))
    (and (handler-case (progn (funcall f :bad) nil) (type-error () t))
         (funcall f (ash 1 100))))`);
  // Primitive replacement would also change the live source compiler's own
  // numeric dependencies. Run these contracts against the sealed compiler.
  if (!sourceCompiler) {
    for (const [operator, parameters, args] of [
      ['integer-length', 'x', '7'], ['ash', 'x y', '7 1'], ['logand', 'x y', '7 3'],
    ]) {
      check(`(progn
        (defparameter *integer-ordinary* (miricl-compiler:compile-native
          '(lambda (${parameters}) (${operator} ${parameters}))))
        (defparameter *integer-notinline* (miricl-compiler:compile-native
          '(lambda (${parameters}) (declare (notinline ${operator})) (${operator} ${parameters}))))
        (defparameter *integer-captured* (miricl-compiler:compile-native
          '(lambda (${parameters}) (declare (inline ${operator})) (${operator} ${parameters}))))
        (let ((original (symbol-function '${operator})) (expected (${operator} ${args})))
          (unwind-protect
            (progn
              (setf (symbol-function '${operator})
                (lambda (&rest arguments) (declare (ignore arguments)) (values :replacement :extra)))
              (and (equal (multiple-value-list (funcall *integer-ordinary* ${args})) '(:replacement :extra))
                   (equal (multiple-value-list (funcall *integer-notinline* ${args})) '(:replacement :extra))
                   (equal (multiple-value-list (funcall *integer-captured* ${args})) (list expected))
                   (let ((fresh (miricl-compiler:compile-native
                          '(lambda (${parameters}) (declare (inline ${operator})) (${operator} ${parameters})))))
                     (equal (multiple-value-list (funcall fresh ${args})) '(:replacement :extra)))))
            (setf (symbol-function '${operator}) original))))`);
    }
  }
} finally { vm.close(); }
console.log(parseContracts ? 'Exact integer primitive test forms parse.'
  : `Exact integer primitives: scalar/bignum values, shifts, checks, effects and value counts pass (${sourceCompiler ? 'source compiler' : 'sealed compiler, including definition contracts'}).`);
