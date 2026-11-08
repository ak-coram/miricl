#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source, {eof: true}).output');
const run = source => {
  context.source = source;
  try { return evaluate.runInContext(context, {timeout: 2000}); }
  catch (error) { vm.vm.unwind(); throw error; }
};
const quote = text => `"${text.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
for (const [source, expected] of [
  ["(write-to-string '#1=(17 . #1#) :circle t :pretty nil :escape nil)", '#1=(17 . #1#)'],
  ["(write-to-string '#1=(#1#) :circle t :pretty nil)", '#1=(#1#)'],
  ["(write-to-string '#1=#(#1#) :circle t :pretty nil)", '#1=#(#1#)'],
  ["(write-to-string '#1=#2A((#1#)) :circle t :pretty nil)", '#1=#2A((#1#))'],
  ['(let ((x (vector))) (write-to-string (vector x x) :circle t :pretty nil))', '#(#1=#() #1#)'],
  ['(let ((x (make-symbol "X"))) (write-to-string (vector x x) :circle t :pretty nil))', '#(#1=#:X #1#)'],
  ['(let ((x (copy-seq "abc"))) (write-to-string (vector x x) :circle t :pretty nil))', '#(#1="abc" #1#)'],
  ['(let ((x (copy-seq #*01))) (write-to-string (vector x x) :circle t :pretty nil))', '#(#1=#*01 #1#)'],
  ['(let ((x (list 2 3))) (write-to-string (list (cons 1 x) x) :circle t :pretty nil))', '((1 . #1=(2 3)) #1#)'],
]) {
  assert.equal(run(source), `${quote(expected)}\n`, source);
}
for (const form of [
  "(let* ((x '#1=(17 . #1#)) (y (read-from-string (write-to-string x :circle t :pretty nil)))) (and (= (car y) 17) (eq y (cdr y))))",
  "(let* ((x '#1=#(#1#)) (y (read-from-string (write-to-string x :circle t :pretty nil)))) (eq y (aref y 0)))",
  "(let* ((x '#1=#2A((#1#))) (y (read-from-string (write-to-string x :circle t :pretty nil)))) (eq y (aref y 0 0)))",
  "(handler-case (write-to-string (copy-readtable nil) :readably t :circle t) (print-not-readable (c) (readtablep (print-not-readable-object c))))",
  "(let ((x (make-hash-table))) (handler-case (write-to-string x :readably t) (print-not-readable (c) (eq x (print-not-readable-object c)))))",
]) assert.equal(run(form), 'T\n', form);
assert.equal(run("(write-to-string '#1=(17 . #1#) :circle t :pretty nil)"), '"#1=(17 . #1#)"\n');
run(`(defclass circle-print-node () ((next :initarg :next :accessor circle-next)))
  (defmethod print-object ((x circle-print-node) s) (format s "<~A>" (circle-next x)))`);
assert.equal(run(`(let ((x (make-instance 'circle-print-node :next nil)))
  (setf (circle-next x) x) (write-to-string x :circle t :pretty nil))`), '"#1=<#1#>"\n');
run(`(defmethod print-object ((x circle-print-node) s)
  (write-string "#(" s)
  (prin1 (write-to-string (vector (circle-next x) (circle-next x)) :circle t :pretty nil) s)
  (write-string " " s) (prin1 (circle-next x) s)
  (write-string " " s) (prin1 (circle-next x) s) (write-string ")" s))`);
assert.equal(run(`(write-to-string (make-instance 'circle-print-node :next (vector)) :circle t :pretty nil)`),
  `${quote('#("#(#1=#() #1#)" #1=#() #1#)')}\n`);
run(`(defmethod print-object ((x circle-print-node) s) (declare (ignore s)) (throw 'stop (circle-next x)))`);
assert.equal(run(`(catch 'stop (write-to-string (make-instance 'circle-print-node :next t) :circle t))`), 'T\n');
assert.equal(run("(write-to-string '#1=(17 . #1#) :circle t :pretty nil)"), '"#1=(17 . #1#)"\n');
console.log('Guest circular printing: lists, vectors, arrays, sharing, reader round trips, readable errors, and recovery pass.');
