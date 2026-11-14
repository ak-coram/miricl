// SBCL-style block boundaries, entry points and lexical call binding.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const bytes = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const builder = await Miricl.instantiate(bytes), target = await Miricl.instantiate(bytes);
const interpreted = process.argv.includes('--interpreted');
async function evaluate(vm, form) {return (await vm.evaluateAsync(form, {eof: true})).output.trim();}
let serial = 0;
async function compile(source, options = '', moreSources = []) {
  const name = `/block-${++serial}`;
  builder.mountFile(`${name}.lisp`, source);
  for (let i = 0; i < moreSources.length; i++) builder.mountFile(`${name}-${i}.lisp`, moreSources[i]);
  const input = moreSources.length
    ? `(list "${name}.lisp" ${moreSources.map((_, i) => `"${name}-${i}.lisp"`).join(' ')})`
    : `"${name}.lisp"`;
  await evaluate(builder, `(compile-file ${input} ${options})`);
  target.mountFile(`${name}.wasm`, await builder.readFile(`${name}.wasm`));
  await evaluate(target, `(load "${name}.wasm")`);
}
async function check(name, source) {
  const result = await evaluate(target, source);
  assert.equal(result, 'T', name); console.log(`PASS ${name}`);
}
try {
  for (const vm of [builder, target]) await evaluate(vm, `(setq miricl-compiler:*compile-evaluations* nil)
    ${interpreted ? '(setq miricl-compiler::*compiler-state* :interpreted)' : ''} (values)`);
  await check('interpreter ignores block boundaries', `(progn
    (declaim (miricl-compiler:start-block interpreted-entry))
    (defun interpreted-helper () 17)
    (defun interpreted-entry () (interpreted-helper))
    (declaim (miricl-compiler:end-block))
    (setf (fdefinition 'interpreted-helper) (lambda () 42))
    (= (interpreted-entry) 42))`);
  for (const vm of [builder, target]) await evaluate(vm, `
    (defpackage :block-test (:use :cl)) (in-package :block-test) (values)`);
  await compile('(defun dynamic-helper () 42) (defun dynamic-entry () (dynamic-helper))', ':block-compile nil :entry-points \'(ignored)');
  await check(`NIL retains global redefinition`, `
    (progn (setf (fdefinition 'dynamic-helper) (lambda () 99)) (= (dynamic-entry) 99))`);
  await compile('(defun sealed-helper () 42) (defun sealed-entry () (sealed-helper))', ':block-compile t');
  await check(`T seals calls and exports all by default`, `
    (progn (setf (fdefinition 'sealed-helper) (lambda () 99))
      (and (= (sealed-entry) 42) (= (sealed-helper) 99)))`);
  await evaluate(target, '(defun private-helper () :previous)');
  await compile(`(defun private-helper () (values 42 :second))
    (defun public-entry () "Public documentation" (private-helper))
    (defun hidden () :hidden)`, ':block-compile t :entry-points \'(public-entry)');
  // The packed HIR representation currently omits function documentation.
  await check(`entry points and private definitions`, `
    (and (equal (multiple-value-list (public-entry)) '(42 :second))
         (eq (private-helper) :previous) (not (fboundp 'hidden))
         t)`);
  await compile(`(declaim (miricl-compiler:start-block first-entry))
    (defun first-helper () 17) (defun first-entry () (first-helper))
    (declaim (miricl-compiler:start-block second-entry))
    (defun second-helper () 23) (defun second-entry () (second-helper))
    (declaim (miricl-compiler:end-block))
    (defun outside-entry () (second-entry))`, ':block-compile :specified');
  await check(`explicit boundaries and outside calls`, `
    (progn (setf (fdefinition 'second-entry) (lambda () 99))
      (and (= (first-entry) 17) (= (outside-entry) 99)
           (not (fboundp 'first-helper)) (not (fboundp 'second-helper))))`);
  await compile(`(defmacro at-definition () 17)
    (defun early-expansion () (at-definition))
    (defmacro at-definition () 23)
    (defun late-expansion () (at-definition))`, ':block-compile t');
  await check(`macro expansion follows source order`, '(and (= (early-expansion) 17) (= (late-expansion) 23))');
  await compile(`(defparameter *block-order* nil)
    (push (fboundp 'ordered-entry) *block-order*)
    (defun ordered-entry () (forward-helper))
    (push (ordered-entry) *block-order*)
    (defun forward-helper () 42)`, ':block-compile t :entry-points \'(ordered-entry)');
  await check(`load effects and forward calls`, `(and (= (first *block-order*) 42) (null (second *block-order*)))`);
  await compile('(defun across-files () (other-file))', ':block-compile t :entry-points \'(across-files)', ['(defun other-file () 42)']);
  await check(`one block spans input files`, '(and (= (across-files) 42) (not (fboundp \'other-file)))');
  await compile(`(defun (setf sealed-place) (value object) (setf (car object) value))
    (defun set-entry (object) (setf (sealed-place object) 42))`, ':block-compile t');
  await check(`SETF function names retain their lexical binding`, `(let ((object (list 0)))
    (setf (fdefinition '(setf sealed-place)) (lambda (value object) (declare (ignore value object)) :changed))
    (and (= (set-entry object) 42) (= (car object) 42)))`);
  await compile(`(macrolet ((local-value () 42)) (defun lexical-definition () (local-value)))
    (defun calls-lexical-definition () (lexical-definition))`, ':block-compile t');
  await check(`non-null lexical DEFUN environment stays outside the block`, `(progn
    (setf (fdefinition 'lexical-definition) (lambda () 99))
    (= (calls-lexical-definition) 99))`);
  builder.mountFile('/valid-block.lisp', '(defun recovered-block () 42)');
  for (const options of [':block-compile :bad', ':block-compile t :entry-points \'(missing)',
      ':block-compile t :entry-points \'(x . y)', ':block-compile t :entry-points \'#1=(x . #1#)']) {
    assert.equal(await evaluate(builder, `(handler-case (progn (compile-file "/valid-block.lisp" ${options}) nil)
      (miricl-compiler::compiler-input-error () t))`), 'T', options);
  }
  await compile('(defun recovered-block () 42)', ':block-compile t');
  await check('malformed input rejection leaves compiler usable', '(= (recovered-block) 42)');
} finally {builder.close(); target.close();}
