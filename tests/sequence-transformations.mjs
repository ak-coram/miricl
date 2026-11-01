#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  assert.equal(evaluate.runInContext(context, {timeout: 2000}), 'T\n', source);
};
check(`(equal (substitute 9 1 '(1 2 1) :count 1 :from-end t) '(1 2 9))`);
check(`(equal (substitute-if 9 #'oddp '(1 2 3)) '(9 2 9))`);
check(`(equal (substitute-if-not 9 #'oddp '(1 2 3)) '(1 9 3))`);
check(`(let* ((s (list 1 2 1)) (tail (cdr s)))
  (and (eq s (nsubstitute 9 1 s)) (eq tail (cdr s)) (equal s '(9 2 9))))`);
check(String.raw`(let* ((base (copy-seq "aAλΛ🙂a"))
              (s (make-array 4 :element-type 'character :displaced-to base :displaced-index-offset 1)))
  (and (eq s (nsubstitute-if #\z #'upper-case-p s)) (string= base "azλz🙂a")))`);
check(`(let ((s (copy-seq #*1010)))
  (and (eq s (nsubstitute-if-not 1 #'oddp s)) (equalp s #*1111)))`);
check(`(let ((s (loop for i below 20000 collect i)))
  (and (null (mismatch s s :from-end t))
    (equal (substitute 99 19999 s :count 1 :from-end t) (append (butlast s) '(99)))))`);
check(`(and (= 1 (mismatch "xλ🙂y" "Λ🙂y" :from-end t :test 'char-equal))
  (equal (multiple-value-list (mismatch '(1 2) '(1 3))) '(1)))`);
check(`(let ((text (make-string 100000 :initial-element #\\a))
             (pattern (make-string 1000 :initial-element #\\a)))
  (setf (char pattern 999) #\\b)
  (and (null (search pattern text)) (null (search pattern text :from-end t))))`);
check(`(and (= 4 (search "aλ" "aΛaλaΛ" :start2 1 :from-end t :test 'char-equal))
  (= 4 (search "" "abcd" :from-end t)) (= 2 (search "abab" "ababab" :from-end t)))`);
check(`(let ((out (make-array 5 :initial-element 9 :fill-pointer 0)))
  (and (eq out (map-into out #'1+ '(1 2 3))) (= (fill-pointer out) 3)
    (equalp out #(2 3 4)) (= (aref out 4) 9)))`);
check(`(let ((n 0) (out (make-array 3 :initial-element 0 :fill-pointer 0)))
  (and (eq out (map-into out (lambda () (incf n)))) (= n 3) (equalp out #(1 2 3))))`);
check(`(handler-case (substitute 9 1 '(1 2) :count 1/2) (type-error () t))`);
check(`(handler-case (search nil nil :start1 nil) (type-error () t))`);
check(`(equal (sublis '((1 . a) (2 . b)) '(1 (2 . 1))) '(a (b . a)))`);
check(`(let* ((tree (list 1 2)) (tail (cdr tree)))
  (and (eq tree (nsublis '((1 . a)) tree)) (eq tail (cdr tree)) (equal tree '(a 2))))`);
check(`(and (equal (subst 9 1 '(1 (2 . 1))) '(9 (2 . 9)))
  (equal (subst-if 9 #'numberp '(1 a (2 . b))) '(9 a (9 . b)))
  (equal (subst-if-not 9 #'consp '(1 . 2)) '(9 . 9)))`);
check(`(and (equal (nsubst 9 1 (copy-tree '(1 2))) '(9 2))
  (equal (nsubst-if 9 #'numberp (copy-tree '(1 a))) '(9 a))
  (equal (nsubst-if-not 9 #'consp (copy-tree '(1 . 2))) '(9 . 9)))`);
check(`(and (tree-equal '(1 (2 . 3)) '(1 (2 . 3)))
  (not (tree-equal '(1) 1 :test (lambda (a b) (declare (ignore a b)) t))))`);
check(`(let ((*read-suppress* t))
  (equal (multiple-value-list (read-from-string "NO-SUCH-PACKAGE:::X")) '(nil 19)))`);
check(`(let ((*read-suppress* t))
  (equal (multiple-value-list (read-from-string "#999999999999999999S(bad)")) '(nil 25)))`);
check(`(let ((n 0) (*read-suppress* t) (*read-eval* nil))
  (and (null (read-from-string "#.(setq n 1)")) (zerop n)))`);
check(`(handler-case (let ((*read-suppress* t)) (read-from-string "#<")) (reader-error () t))`);
check('(= 42 (+ 20 22))');
check(`(let* ((base (vector 99 3 1 2 98))
               (view (make-array 3 :displaced-to base :displaced-index-offset 1)))
  (and (eq view (stable-sort view #'<)) (equalp base #(99 1 2 3 98))))`);
check(`(equal (stable-sort (list '(2 a) '(1 b) '(2 c) '(1 d)) #'< :key #'car)
  '((1 b) (1 d) (2 a) (2 c)))`);
check(`(equal (merge 'list (list '(1 a) '(2 b)) (list '(1 c) '(2 d)) #'< :key #'car)
  '((1 a) (1 c) (2 b) (2 d)))`);
check(`(let ((a (make-array 6 :initial-contents '(3 1 2 9 8 7) :fill-pointer 3)))
  (and (eq a (sort a #'<)) (equalp a #(1 2 3)) (= (aref a 3) 9)))`);
check(`(handler-case (merge '(vector * 3) (list 1 2) (list 3 4) #'<) (type-error () t))`);
console.log('Native sequence and tree operations: storage, bounds, long lists, repetitive strings, fill pointers, values, and recovery pass.');
