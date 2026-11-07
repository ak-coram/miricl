#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const options = process.argv.slice(2);
const binary = options.find(option => !option.startsWith('--')) ?? 'miricl-interpreter.wasm';
const vm = await Miricl.instantiate(fs.readFileSync(binary));
vm.evaluate(`(defparameter *symbol-anchor* (make-symbol "anchor"))
  (defparameter *symbol-closure*
    (let ((name (gensym))) (eval (list 'let (list (list name 42)) (list 'lambda nil name)))))
  (defun churn-symbols (n) (dotimes (i n) (make-symbol "temporary")) nil)
  (defun churn-gensyms (n) (dotimes (i n) (gensym)) nil)`);
vm.evaluate('(churn-symbols 1000) (churn-gensyms 1000)');
global.gc?.();
const before = process.memoryUsage().heapUsed;
const results = {};
for (const name of ['churn-symbols', 'churn-gensyms']) {
  const start = performance.now();
  assert.equal(vm.evaluate(`(${name} 100000)`).output, 'NIL\n');
  results[name] = {iterations: 100000, milliseconds: performance.now() - start};
}
assert.equal(vm.evaluate(`(and (eq *symbol-anchor* *symbol-anchor*)
  (string= (symbol-name *symbol-anchor*) "anchor") (= (funcall *symbol-closure*) 42))`).output, 'T\n');
if (options.includes('--register-late')) {
  assert.equal(vm.evaluate(`(handler-case (eval (make-symbol "late-unbound"))
    (unbound-variable (c) (string= (symbol-name (cell-error-name c)) "late-unbound")))`).output, 'T\n');
}
if (options.includes('--bind-late')) {
  assert.equal(vm.evaluate(`(defparameter *late-value* 42)
    (defun late-function () 42) (defmacro late-macro () 42)
    (deftype late-type () 'integer) (defclass late-class () ())
    (define-symbol-macro late-symbol-macro *late-value*)
    (define-compiler-macro late-function () "Late compiler macro." 42)
    (and (functionp (compiler-macro-function 'late-function)) (= late-symbol-macro 42) (= *late-value* 42) (= (late-function) 42) (= (late-macro) 42)
      (typep 1 'late-type) (typep (make-instance 'late-class) 'late-class))`).output.trim().endsWith('T'), true);
}
if (options.includes('--redefine-compiler-macro')) {
  assert.equal(vm.evaluate(`(dotimes (i 1000)
    (define-compiler-macro late-function () "Replacement compiler macro." 42))
    (= (funcall (compiler-macro-function 'late-function) '(late-function) nil) 42)`).output.trim().endsWith('T'), true);
}
global.gc?.();
console.log(JSON.stringify({binary, explicitGC: typeof global.gc === 'function',
  heapGrowthBytes: process.memoryUsage().heapUsed - before, results}, null, 2));
