import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const sourceCompiler = process.argv.includes('--source');
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
                 (declare (inline list-length) (optimize (safety ${safety}))) (list-length x)))))
      (and (every (lambda (n) (equal (multiple-value-list (funcall f (make-list n))) (list n)))
                  '(0 1 2 17 64))
           (let ((cycle (list 1 2 3)))
             (setf (cdr (last cycle)) cycle)
             (equal (multiple-value-list (funcall f cycle)) '(nil)))
           (handler-case (progn (funcall f '(1 . 2)) nil) (type-error () t))
           (handler-case (progn (funcall f :bad) nil) (type-error () t))
           (= (funcall f '(a b)) 2)))`);
  }
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
               (declare (inline list-length)) (list-length x) :done))))
    (and (handler-case (progn (funcall f '(1 . 2)) nil) (type-error () t))
         (eq :done (funcall f nil))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
               (declare (inline list-length))
               (let ((first (list-length x))) (setf (cdr x) nil)
                 (values first (list-length x)))))))
    (equal (multiple-value-list (funcall f (list 1 2 3))) '(3 1)))`);
  check(`(let ((events nil)
        (f (miricl-compiler:compile-native '(lambda (callback)
               (declare (inline list-length)) (list-length (funcall callback))))))
    (and (= (funcall f (lambda () (push :call events) (values '(a b) :extra))) 2)
         (equal events '(:call))))`);
  for (const args of ['', 'x x']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
                 (declare (inline list-length)) (list-length ${args})))))
      (handler-case (progn (funcall f nil) nil) (program-error () t)))`);
  }
  if (!sourceCompiler) {
    check(`(let ((ordinary (miricl-compiler:compile-native '(lambda (x) (list-length x))))
          (requested-inline (miricl-compiler:compile-native '(lambda (x) (declare (inline list-length)) (list-length x))))
          (dynamic (miricl-compiler:compile-native '(lambda (x) (declare (notinline list-length)) (list-length x))))
          (original (symbol-function 'list-length)))
      (unwind-protect
        (progn (setf (symbol-function 'list-length)
                 (lambda (x) (declare (ignore x)) (values :replacement :extra)))
          (and (equal (multiple-value-list (funcall ordinary nil)) '(:replacement :extra))
               (equal (multiple-value-list (funcall dynamic nil)) '(:replacement :extra))
               ;; LIST-LENGTH is a library closure; lacking a canonical
               ;; definition proof, INLINE must keep the replaceable call.
               (equal (multiple-value-list (funcall requested-inline nil)) '(:replacement :extra))))
        (setf (symbol-function 'list-length) original)))`);
  }
} finally { vm.close(); }
console.log('LIST-LENGTH: proper/circular lists, malformed input/recovery, discarded errors, mutation, effects and values pass.');
