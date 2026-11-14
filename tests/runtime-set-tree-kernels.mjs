import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
let checked = 0;
try {
  for (const [source, expected] of [
    ["(subst :new 1 '(1 (2 1) . 1))", '(:NEW (2 :NEW) . :NEW)'],
    ["(subst-if :new #'numberp '(1 (2 . a) nil))", '(:NEW (:NEW . A) NIL)'],
    ["(subst-if-not :new #'consp '(1 (2 . a) nil))", '(:NEW (:NEW . :NEW) :NEW . :NEW)'],
    ["(sublis '((1 . :new) (a . :letter)) '(1 (2 1) . a))", '(:NEW (2 :NEW) . :LETTER)'],
    ["(sublis '((1 . :small) (5 . :medium) (10 . :large)) '(0 3 8 20) :test (lambda (a b) (and (numberp a) (numberp b) (< a b))))", '(:SMALL :MEDIUM :LARGE 20)'],
    ["(let* ((tree (list (list 1 2) 3)) (untouched (car tree)) (out (subst 9 3 tree))) (list tree out (eq untouched (car out))))", '(((1 2) 3) ((1 2) 9) T)'],
    ["(let* ((tree (list 1 2 3)) (tail (cdr tree)) (out (nsubst 9 2 tree))) (list out (eq tree out) (eq tail (cdr out))))", '((1 9 3) T T)'],
    ["(let ((tree (list 1 2 3))) (eq tree (sublis '((:absent . :new)) tree)))", 'T'],
    ["(multiple-value-list (subst-if :new (lambda (x) (values (eql x 1) :extra)) '(1 2)))", '((:NEW 2))'],
    ["(let ((tree (list 1))) (setf (cdr tree) tree) (subst :new tree tree :test #'eq))", ':NEW'],
    ["(handler-case (let ((tree (list 1))) (setf (cdr tree) tree) (subst :new :absent tree)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((tree (list 1))) (setf (car tree) tree) (subst :new :absent tree)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((tree (list 1 2 3))) (subst-if :new (lambda (x) (if (eql x 1) (setf (cdr tree) tree)) nil) tree)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (sublis '(1) '(1 2)) (type-error () :caught))", ':CAUGHT'],
    ["(subst :new 2 '(1 2 3))", '(1 :NEW 3)'],
    ["(copy-tree '(1 (2 3) . 4))", '(1 (2 3) . 4)'],
    ["(let* ((shared (list 1 2)) (tree (list shared shared)) (copy (copy-tree tree))) (list (equal tree copy) (eq tree copy) (eq shared (car copy)) (eq (car copy) (cadr copy))))", '(T NIL NIL NIL)'],
    ["(handler-case (let ((tree (list 1))) (setf (cdr tree) tree) (copy-tree tree)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((tree (list 1))) (setf (car tree) tree) (copy-tree tree)) (type-error () :caught))", ':CAUGHT'],
    ["(copy-tree '(1 2 3))", '(1 2 3)'],
    ['(reverse "aλ🙂bc")', '"cb🙂λa"'],
    ['(let* ((s (copy-seq "aλ🙂bc")) (out (reverse s))) (list s out (eq s out)))', '("aλ🙂bc" "cb🙂λa" NIL)'],
    ['(let* ((s (copy-seq "aλ🙂bc")) (out (nreverse s))) (list s out (eq s out)))', '("cb🙂λa" "cb🙂λa" T)'],
  ]) {
    assert.equal(vm.evaluate(source).output.trim(), expected, source);
    checked++;
  }
} finally {vm.close();}
console.log(`Tree set kernels: ${checked} ordered substitution, sharing, mutation, cycle and recovery cases pass.`);
