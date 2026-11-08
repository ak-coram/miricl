#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const calls = 10000;
const fixtures = '(defparameter *place-plist* (list :left 1 :right 2)) (defparameter *place-list* (list 1 2)) (defparameter *place-left* 1) (defparameter *place-right* 2) (defparameter *place-vector* (vector 1 2))';
const workloads = [
  ['setf-car', '(setf (car *place-list*) 1)'],
  ['setf-aref', '(setf (aref *place-vector* 0) 1)'],
  ['psetq', '(psetq *place-left* *place-right* *place-right* *place-left*)'],
  ['psetf', '(psetf *place-left* *place-right* *place-right* *place-left*)'],
  ['rotatef', '(rotatef *place-left* *place-right*)'],
  ['shiftf', '(shiftf *place-left* *place-right* *place-left*)'],
  ['multiple-value-setq', '(multiple-value-setq (*place-left* *place-right*) (values *place-right* *place-left*))'],
  ['rotatef-elt', '(rotatef (elt *place-vector* 0) (elt *place-vector* 1))'],
  ['psetf-aref', '(psetf (aref *place-vector* 0) (aref *place-vector* 1) (aref *place-vector* 1) (aref *place-vector* 0))'],
];
workloads.push(
  ['getf', '(getf *place-plist* :right)'],
  ['setf-getf', '(setf (getf *place-plist* :left) 1)'],
  ['remf', '(progn (setf (getf *place-plist* :extra) 3) (remf *place-plist* :extra))'],
  ['pushnew', '(pushnew 1 *place-list*)'],
  ['pushnew-keyed', "(pushnew 1 *place-list* :test #'eql)"],
);
const body = form => `(progn (dotimes (i ${calls}) ${form}) (list *place-left* *place-right*))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, form] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (assert (equal (funcall fn) '(1 2)))
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 50) (funcall fn))
          (* 20.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  vm.evaluate("(defmacro benchmark-list () '*place-list*) (defmacro benchmark-plist () '*place-plist*)");
  const milliseconds = {};
  for (const [name, form] of workloads) {
    const measured = args.includes('--general-places')
      ? form.replaceAll('*place-list*', '(benchmark-list)').replaceAll('*place-plist*', '(benchmark-plist)') : form;
    vm.evaluate(`(defun place-run () ${body(measured)})`);
    const run = () => assert.equal(vm.evaluate('(place-run)').output, '(1 2)\n');
    for (let warmup = 0; warmup < 5; warmup++) run();
    const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
    assert.equal(vm.evaluate("(and (equal *place-plist* '(:left 1 :right 2)) (equal *place-list* '(1 2)))").output, 'T\n');
    assert.equal(vm.evaluate("(equal (coerce *place-vector* 'list) '(1 2))").output, 'T\n');
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
