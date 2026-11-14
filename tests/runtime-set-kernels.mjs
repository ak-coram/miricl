import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
let checked = 0;
try {
  for (const [source, expected] of [
    ["(remove-duplicates '(1 2 1 3 2))", '(1 3 2)'],
    ["(remove-duplicates '(1 2 1 3 2) :from-end t)", '(1 2 3)'],
    ["(remove-duplicates '(0 1 2 1 3 2 9) :start 1 :end 6)", '(0 1 3 2 9)'],
    ['(remove-duplicates "aAλΛ🙂a" :test #\'char-equal)', '"Λ🙂a"'],
    ['(remove-duplicates "aAλΛ🙂a" :test #\'char-equal :from-end t)', '"aλ🙂"'],
    ["(remove-duplicates #(1 2 1 3 2))", '#(1 3 2)'],
    ["(remove-duplicates '((1 a) (2 b) (1 c)) :key #'car)", '((2 B) (1 C))'],
    ["(remove-duplicates '(1 2 1) :test-not #'/=)", '(2 1)'],
    ["(let* ((x (list 1 2 1)) (tail (cdr x)) (out (delete-duplicates x))) (list out (eq tail out)))", '((2 1) T)'],
    ["(let ((x (list 1 2))) (list (eq x (remove-duplicates x)) x))", '(T (1 2))'],
    ["(multiple-value-list (remove-duplicates '(1 2 1) :key (lambda (x) (values x :extra))))", '((2 1))'],
    ["(sort (intersection '(1 2 3) '(2 3 4)) #'<)", '(2 3)'],
    ["(sort (set-difference '(1 2 3) '(2 3 4)) #'<)", '(1)'],
    ["(sort (union '(1 2 3) '(2 3 4)) #'<)", '(1 2 3 4)'],
    ["(subsetp '(1 2) '(3 2 1))", 'T'],
    ["(subsetp '(1 4) '(3 2 1))", 'NIL'],
    ["(subsetp '(1 2) '(3 4) :test #'<)", 'T'],
    ["(sort (set-difference '(1 2 3) '(2 3) :test-not #'/=) #'<)", '(1)'],
    ["(sort (intersection '((1 a) (2 b)) '((2 x)) :key #'car) #'< :key #'car)", '((2 B))'],
    ["(let ((b (list 1 2))) (list (eq b (adjoin 1 b)) (eq b (cdr (adjoin 3 b)))))", '(T T)'],
    ["(length (intersection (loop for i below 64 collect i) (loop for i from 32 below 96 collect i)))", '32'],
    ["(sort (set-exclusive-or '(1 2 3) '(2 3 4)) #'<)", '(1 4)'],
    ["(multiple-value-list (subsetp '(1) '(2) :test (lambda (a b) (values (< a b) :extra))))", '(T)'],
    ["(handler-case (remove-duplicates '(1 . :bad)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((x (list 1 2))) (setf (cddr x) x) (remove-duplicates x)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (intersection '(1 . :bad) nil) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((x (list 1 2))) (setf (cddr x) x) (union x nil)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (remove-duplicates '(1 2 1) :key (lambda (x) (declare (ignore x)) (error \"callback\"))) (error () :caught))", ':CAUGHT'],
    ["(remove-duplicates '(1 2 1))", '(2 1)'],
  ]) {
    assert.equal(vm.evaluate(source).output.trim(), expected, source);
    checked++;
  }
} finally {vm.close();}
console.log(`Set kernels: ${checked} public duplicate, direction, identity, set, boundary and recovery cases pass.`);
