#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm'));
const quote = text => `"${text.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
for (const count of [0, 1, 10, 11, 21, 22, 31, 32, 33, 63, 64, 65, 129, 4097]) {
  const expected = 'aλ🙂'.repeat(count);
  assert.equal(vm.evaluate(`(string= (format nil (lambda (s)
    (dotimes (i ${count}) (write-string "aλ🙂" s)))) ${quote(expected)})`).output, 'T\n');
  assert.equal(vm.evaluate(`(string= (format nil (lambda (s)
    (write-string "" s) (write-string ${quote(expected)} s) (write-string "" s)
    (write-string "end" s))) ${quote(expected + 'end')})`).output, 'T\n');
}
assert.equal(vm.evaluate(`(string= (format nil (lambda (s)
  (write-string "λ🙂" s) (fresh-line s) (fresh-line s) (write-string "x" s))) "λ🙂
x")`).output, 'T\n');
assert.equal(vm.evaluate(`(string= (format nil (lambda (s)
  (catch 'done (write-string "before" s) (throw 'done nil)) (write-string "after" s))) "beforeafter")`).output, 'T\n');
vm.evaluate(`(defclass output-buffer-object () ())
  (defmethod print-object ((x output-buffer-object) s)
    (declare (ignore x)) (dotimes (i 10000) (write-string "x" s)))`);
assert.equal(vm.evaluate(`(string= (write-to-string (make-instance 'output-buffer-object))
  (make-string 10000 :initial-element #\\x))`).output, 'T\n');
assert.equal(vm.evaluate(`(format t "aλ🙂") (format t "b")`).output, 'aλ🙂NIL\nbNIL\n');
assert.equal(vm.evaluate('(format t "fresh")').output, 'freshNIL\n');
for (const source of [
  `(let ((s (make-string-output-stream)))
    (write-string "first" s) (let ((a (get-output-stream-string s)))
      (write-string "second" s)
      (and (string= a "first") (string= (get-output-stream-string s) "second")
           (string= (get-output-stream-string s) ""))))`,
  `(let ((s (make-string-output-stream)))
    (and (streamp s) (typep s 'string-stream) (output-stream-p s) (not (input-stream-p s))
         (open-stream-p s) (close s) (not (open-stream-p s))
         (handler-case (write-char #\\a s) (stream-error (c) (eq s (stream-error-stream c))))))`,
  `(let ((x (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (and (equal (multiple-value-list (with-output-to-string (s x)
      (write-string "aλ🙂" s) (values 1 2))) '(1 2)) (string= x "aλ🙂")))`,
  `(let ((x (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (and (= 17 (catch 'done (with-output-to-string (s x)
      (write-string "before" s) (throw 'done 17)))) (string= x "before")))`,
]) assert.equal(vm.evaluate(source).output, 'T\n');
console.log('Guest output buffers: growth boundaries, Unicode, columns, callback streams, public string streams, closing, nonlocal exits, and output reset pass.');
