import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  check(`(list (count 2 '(1 2 3 2 4)) (count 2 #(1 2 3 2 4) :from-end t)
               (count-if #'oddp '(1 2 3 4)) (count-if-not #'oddp '(1 2 3 4)))`, '(2 2 2 2)');
  check(`(let ((seen nil)) (list (count 2 '((1) (2) (3) (4)) :test #'< :from-end t
        :key (lambda (x) (push x seen) (values (car x) :extra))) (reverse seen)))`, '(2 ((4) (3) (2) (1)))');
  check(`(list (count #\\a "Abλa🙂" :test #'char-equal)
               (count #\\a "Abλa🙂" :test-not #'char-equal :from-end t)
               (count 955 "aλbλ🙂λ" :key #'char-code))`, '(2 3 3)');
  check(`(multiple-value-list (count-if (lambda (x) (values (oddp x) :extra)) '(1 2 3)))`, '(2)');
  check(`(let ((items (list 1 2 3)) (seen nil))
     (list (count-if (lambda (x) (push x seen) (setf (car items) 99) (= x 99)) items :from-end t)
           (reverse seen)))`, '(1 (3 2 99))');
  check(`(let ((items (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))))
     (count-if (lambda (x) (setf (fill-pointer items) 0) (oddp x)) items))`, '2');
  for (const source of ["(count 1 '(1 . :bad))",
    "(let ((s (list 1 2))) (setf (cddr s) s) (count 3 s))",
    "(let ((s (list 1 2 3))) (count-if (lambda (x) (setf (cdr s) :bad) x) s))",
    "(let ((args (list 1 '(1 2) :from-end t))) (setf (cddddr args) args) (apply #'count args))"]) {
    check(`(handler-case ${source} (type-error () :type))`, ':TYPE');
  }
  check(`(let ((s (list 1 2 3))) (count-if (lambda (x) (declare (ignore x)) (setf (cdr s) s) t) s))`, '3');
  check(`(count 1 '(1 2 1))`, '2');
  console.log('Count kernels: region modes, Unicode, order, values, mutation, boundaries and recovery pass.');
} finally { vm.close(); }
