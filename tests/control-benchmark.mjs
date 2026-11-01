#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const calls = 10000;
const fixtures = `(defparameter *control-special* 0)
  (defparameter *control-result* nil)
  (defparameter *constructed-function* nil)
  (defparameter *control-operator* 'setq)
  (defparameter *control-constant* (constantly 17))
  (defparameter *control-complement* (complement #'null))
  (defparameter *control-values* '(1 2 3))
  (defparameter *control-many-values* (make-list 64 :initial-element 17))
  (defparameter *control-reader* (lambda () (values *control-special* 2 3)))
  (defparameter *control-documented-reader* (lambda () "Documented reader." (values *control-special* 2 3)))`;
const workloads = [
  ['and-values', '(and t (multiple-value-list (funcall *control-reader*)))', '(0 2 3)'],
  ['or-values', '(or nil (multiple-value-list (funcall *control-reader*)))', '(0 2 3)'],
  ['constantly-create', '(progn (setq *constructed-function* (constantly *control-special*)) (functionp *constructed-function*))', 't'],
  ['constantly-call', '(funcall *control-constant*)', '17'],
  ['constantly-variadic', '(funcall *control-constant* 1 2 3 4 5)', '17'],
  ['complement-create', '(progn (setq *constructed-function* (complement *control-reader*)) (functionp *constructed-function*))', 't'],
  ['complement-call', '(funcall *control-complement* 17)', 't'],
  ['fboundp-special', '(not (null (fboundp *control-operator*)))', 't'],
  ['special-operator-p', '(not (null (special-operator-p *control-operator*)))', 't'],
  ['fdefinition-special', '(progn (fdefinition *control-operator*) t)', 't'],
  ['documented-reader', '(multiple-value-list (funcall *control-documented-reader*))', '(0 2 3)'],
  ['multiple-values-control', '(multiple-value-list (funcall *control-reader*))', '(0 2 3)'],
  ['values-list', '(multiple-value-list (values-list *control-values*))', '(1 2 3)'],
  ['apply-values', "(multiple-value-list (apply #'values *control-values*))", '(1 2 3)'],
  ['apply-prefix', "(apply #'list 1 2 *control-values*)", '(1 2 1 2 3)'],
  ['many-values-list', '(length (multiple-value-list (values-list *control-many-values*)))', '64'],
  ['the-primary', '(multiple-value-list (the integer (funcall *control-reader*)))', '(0 2 3)'],
  ['the-values', '(multiple-value-list (the (values integer integer &rest integer) (funcall *control-reader*)))', '(0 2 3)'],
  ['let-special', '(let ((*control-special* 1)) (multiple-value-list (funcall *control-reader*)))', '(1 2 3)'],
  ['progv-special', "(progv '(*control-special*) '(1) (multiple-value-list (funcall *control-reader*)))", '(1 2 3)'],
  ['progv-unbound', "(progv '(*control-special*) nil (not (boundp '*control-special*)))", 't'],
];
const body = form => `(progn (dotimes (i ${calls}) (setq *control-result* ${form})) *control-result*)`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, form, expected] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (assert (equal (funcall fn) '${expected}))
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 100) (assert (equal (funcall fn) '${expected})))
          (* 10.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
      (assert (= *control-special* 0)))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures); const milliseconds = {};
  for (const [name, form, expected] of workloads) {
    vm.evaluate(`(defun control-run () ${body(form)})`);
    const run = () => assert.equal(vm.evaluate(`(equal (control-run) '${expected})`).output, 'T\n', name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
    assert.equal(vm.evaluate('(= *control-special* 0)').output, 'T\n');
  }
  let retainedConstantBytes;
  if (args.includes('--retain-constants')) {
    assert.equal(typeof global.gc, 'function', 'Use --expose-gc for retained constants.');
    vm.evaluate(`(defparameter *retained-constants* (make-array ${calls}))`);
    global.gc(); const before = process.memoryUsage().heapUsed;
    vm.evaluate(`(dotimes (i ${calls}) (setf (aref *retained-constants* i) (constantly *control-special*)))`);
    global.gc(); retainedConstantBytes = process.memoryUsage().heapUsed - before;
    assert.equal(vm.evaluate(`(every (lambda (fn) (= (funcall fn 1 2 3) 0)) *retained-constants*)`).output, 'T\n');
  }
  let retainedComplementBytes;
  if (args.includes('--retain-complements')) {
    assert.equal(typeof global.gc, 'function', 'Use --expose-gc for retained complements.');
    vm.evaluate(`(defparameter *retained-complements* (make-array ${calls}))`);
    global.gc(); const before = process.memoryUsage().heapUsed;
    vm.evaluate(`(dotimes (i ${calls}) (setf (aref *retained-complements* i) (complement #'null)))`);
    global.gc(); retainedComplementBytes = process.memoryUsage().heapUsed - before;
    assert.equal(vm.evaluate(`(every (lambda (fn) (funcall fn 17)) *retained-complements*)`).output, 'T\n');
  }
  console.log(JSON.stringify({binary, calls, milliseconds, retainedConstantBytes, retainedComplementBytes}, null, 2));
}
