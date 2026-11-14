import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (process.argv.includes('--source')) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (let n = 0; n <= 4; n++) {
    const args = Array.from({length: n}, (_, i) => `a${i}`).join(' ');
    check(`(progn
      (defparameter *vector-primary-${n}* (miricl-compiler:compile-native
        '(lambda (f ${args}) (list (funcall f ${args})))))
      (defparameter *vector-full-${n}* (miricl-compiler:compile-native
        '(lambda (f ${args}) (funcall f ${args}))))
      (defparameter *vector-drop-${n}* (miricl-compiler:compile-native
        '(lambda (f ${args}) (funcall f ${args}) :done))) t)`);
    check(`(let* ((cell (list :shared))
                   (cycle (cons nil nil))
                   (objects (list nil t :symbol 0 (ash 1 120) 3/7 1.25d0 #c(1 2)
                                  #\\x "text" cell cycle (vector 1) #'car (make-hash-table))))
      (setf (cdr cycle) cycle)
      (every (lambda (object)
        (let* ((arguments (make-list ${n} :initial-element object))
               (a (car (apply *vector-primary-${n}* #'vector arguments)))
               (b (car (apply *vector-primary-${n}* #'vector arguments)))
               (full (multiple-value-list (apply *vector-full-${n}* #'vector arguments))))
          (and (simple-vector-p a) (= (length a) ${n}) (not (eq a b))
               (= (length full) 1) (equalp (car full) a)
               (every (lambda (element) (eq element object)) a)
               (eq (apply *vector-drop-${n}* #'vector arguments) :done)
               (if (> ${n} 0)
                   (progn (setf (svref a 0) :changed) (eq (svref b 0) object)) t)))) objects))`);
    const values = Array.from({length: n}, (_, i) => i + 1).join(' ');
    const list = `(${values})`;
    // Compile all wrappers before replacing a primitive the source compiler uses.
    check(`(let ((saved (symbol-function 'vector)))
      (unwind-protect
        (progn
          (setf (symbol-function 'vector) (lambda (&rest args) (values args :extra)))
          (let ((a (car (funcall *vector-primary-${n}* (symbol-function 'vector) ${values})))
                (b (car (funcall *vector-primary-${n}* (symbol-function 'vector) ${values}))))
            (and (equal a '${list}) (equal b '${list})
                 (if (> ${n} 0) (not (eq a b)) t)
                 (equal (multiple-value-list
                         (funcall *vector-full-${n}* (symbol-function 'vector) ${values}))
                        '(${list} :extra))
                 (equalp (car (funcall *vector-primary-${n}* saved ${values})) #${list}))))
        (setf (symbol-function 'vector) saved)))`);
    check(`(and
      (handler-case (progn (funcall *vector-primary-${n}* :not-a-function ${values}) nil)
        (error () t))
      (handler-case (progn (funcall *vector-primary-${n}*
        (lambda (unexpected) unexpected) ${values}) ${n === 1 ? 't' : 'nil'})
        (program-error () ${n === 1 ? 'nil' : 't'}))
      (simple-vector-p (car (funcall *vector-primary-${n}* #'vector ${values})))
      (equal (multiple-value-list
        (multiple-value-prog1 (values :kept :second)
          (funcall *vector-primary-${n}* #'vector ${values}))) '(:kept :second)))`);
  }
  check(`(let ((events nil)
    (f (miricl-compiler:compile-native '(lambda (f a b c d)
      (list (funcall f (funcall a) (funcall b) (funcall c) (funcall d)))))))
    (and (equalp (car (funcall f #'vector
      (lambda () (push :a events) (values :a :extra))
      (lambda () (push :b events) (values :b :extra))
      (lambda () (push :c events) (values :c :extra))
      (lambda () (push :d events) (values :d :extra)))) #(:a :b :c :d))
      (equal events '(:d :c :b :a))))`);
  if (vm.vm['test-vector-primary-packet']) {
    for (let n = 0; n <= 4; n++) assert.equal(vm.vm['test-vector-primary-packet'](n), 1);
  }
} finally { vm.close(); }
console.log('Saved VECTOR calls: arities 0–4, arbitrary objects, freshness, sharing, values, effects, replacement and recovery pass.');
