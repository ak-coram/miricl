import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  check(`(list (reduce #'+ nil) (reduce #'+ '(7))
               (reduce #'list '(1 2 3)) (reduce #'list '(1 2 3) :from-end t)
               (reduce #'list nil :initial-value nil))`, '(0 7 ((1 2) 3) (1 (2 3)) NIL)');
  check(`(let ((seen nil))
     (list (reduce #'+ '(1 2 3) :key (lambda (x) (push x seen) (values x :extra))
                   :initial-value 10 :from-end t) (reverse seen)))`, '(16 (3 2 1))');
  check(`(list (multiple-value-list (reduce (lambda () (values 17 :extra)) nil))
               (multiple-value-list (reduce (lambda (a b) (values (+ a b) :extra)) '(1 2 3))))`, '((17) (6))');
  check(`(let ((s (list :shared)))
     (and (eq s (reduce #'list (list s))) (eq s (reduce #'list nil :initial-value s))))`, 'T');
  check(`(let ((items (list 1 2 3)) (seen nil))
     (let ((result (reduce (lambda (a b) (push (list a b) seen) (setf (car items) 99) (+ a b))
                           items :from-end t)))
       (list result (reverse seen))))`, '(104 ((2 3) (99 5)))');
  check(`(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))))
     (reduce #'+ items :key (lambda (x) (setf (fill-pointer items) 0) x)))`, '6');
  for (const source of ["(reduce #'+ '(1 . :bad))",
    "(let ((s (list 1 2))) (setf (cddr s) s) (reduce #'+ s))",
    "(let ((args (list #'+ '(1 2) :from-end t))) (setf (cddddr args) args) (apply #'reduce args))",
    "(let ((s (list 1 2 3))) (reduce #'+ s :key (lambda (x) (setf (cdr s) :bad) x)))"]) {
    check(`(handler-case ${source} (type-error () :type))`, ':TYPE');
  }
  check(`(let ((s (list 1 2 3))) (reduce #'+ s :key (lambda (x) (setf (cdr s) s) x)))`, '3');
  check(`(reduce #'+ '(1 2 3))`, '6');
  console.log('Reduction kernels: order, initial/empty cases, values, mutation, boundaries and recovery pass.');
} finally { vm.close(); }
