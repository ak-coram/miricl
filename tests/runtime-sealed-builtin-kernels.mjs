import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl.wasm';
const compilerFree = process.argv.includes('--interpreter');
const vm = await Miricl.instantiate(await fs.readFile(image));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (process.argv.includes('--source'))
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
  // Every expression invokes public runtime workers with ordinary target data.
  // Compare before/after in JS so rebinding EQUAL cannot affect the assertion.
  for (const [name, replacement, expression] of [
    ['realp', '(lambda (&rest args) (declare (ignore args)) nil)', "(subtypep '(real 1 2) 'real)"],
    ['rational', '(lambda (&rest args) (declare (ignore args)) 0)', "(subtypep '(rational 1/2 1/2) 'integer)"],
    ['float', '(lambda (&rest args) (declare (ignore args)) 0.0)', "(subtypep '(single-float 1 1) nil)"],
    ['vector', '(lambda (&rest args) (declare (ignore args)) (error "REBOUND-VECTOR"))', "(subtypep '(integer 1 2) 'integer)"],
    ['make-array', '(lambda (&rest args) (declare (ignore args)) nil)', "(subtypep '(and integer (real (0) (3))) '(integer 1 2))"],
    ['abs', '(lambda (&rest args) (declare (ignore args)) 0)', "(subtypep '(unsigned-byte 100) '(signed-byte 101))"],
    ['signum', '(lambda (&rest args) (declare (ignore args)) 0)', "(subtypep '(signed-byte 65) '(signed-byte 64))"],
    ['/=', '(lambda (&rest args) (declare (ignore args)) nil)', "(subtypep '(array bit (2 3)) '(array bit (* *)))"],
    ['equal', '(lambda (&rest args) (declare (ignore args)) nil)', "(subtypep '(array bit (2 3)) '(array bit (* *)))"],
    ['floor', '(lambda (&rest args) (declare (ignore args)) 0)', "(subtypep '(rational (0) (1)) 'ratio)"],
    ['ceiling', '(lambda (&rest args) (declare (ignore args)) 0)', "(subtypep '(rational (0) (1)) 'ratio)"],
  ]) {
    const baseline = vm.evaluate(`(multiple-value-list ${expression})`).output;
    vm.evaluate(`(setq *sealed-audit-saved* (fdefinition '${name})) (values)`);
    try {
      vm.evaluate(`(setf (fdefinition '${name}) ${replacement}) (values)`);
      assert.equal(vm.evaluate(`(multiple-value-list ${expression})`).output, baseline, `${name}: sealed runtime dependency`);
    } finally { vm.evaluate(`(setf (fdefinition '${name}) *sealed-audit-saved*) (values)`); }
  }
  // ERROR remains a sealed implementation dependency; user source can still
  // call its replacement explicitly through the ordinary global binding.
  check(`(let ((saved #'error))
    (unwind-protect
      (progn (setf (fdefinition 'error) (lambda (&rest args) (declare (ignore args)) :replacement))
        (handler-case (progn (subtypep '(real :bad 2) 'real) nil) (type-error () t)))
      (setf (fdefinition 'error) saved)))`);

  if (!compilerFree) {
    // Compile before replacement. The ordinary compiler and NOTINLINE retain
    // their global-binding contract, and saved replacement objects stay live.
    check(`(progn
      (defparameter *sealed-ordinary* (miricl-compiler:compile-native '(lambda (x) (realp x))))
      (defparameter *sealed-notinline* (miricl-compiler:compile-native
        '(lambda (x) (declare (notinline realp)) (realp x))))
      (defparameter *sealed-dynamic* (miricl-compiler:compile-native
        '(lambda (name x) (funcall (fdefinition name) x))))
      (defparameter *sealed-callback* (miricl-compiler:compile-native
        '(lambda (function x) (funcall function x))))
      (and (funcall *sealed-ordinary* 1) (funcall *sealed-notinline* 1)))`);
    check(`(let* ((original #'realp)
                  (replacement (lambda (x) (declare (ignore x)) (values :replacement :extra)))
                  (alias (make-symbol "SEALED-USER-ALIAS")))
      (unwind-protect
        (progn (setf (fdefinition 'realp) replacement (fdefinition alias) replacement)
          (and (eq (fdefinition 'realp) replacement)
               (eq (fdefinition alias) replacement)
               (equal (multiple-value-list (funcall *sealed-ordinary* 1)) '(:replacement :extra))
               (equal (multiple-value-list (funcall *sealed-notinline* 1)) '(:replacement :extra))
               (equal (multiple-value-list (funcall *sealed-dynamic* 'realp 1)) '(:replacement :extra))
               (equal (multiple-value-list (funcall *sealed-callback* replacement 1)) '(:replacement :extra))
               (equal (multiple-value-list (funcall *sealed-callback* original 1)) '(t))
               (progn (setf (fdefinition 'realp) original)
                 (equal (multiple-value-list (funcall *sealed-callback* (fdefinition alias) 1)) '(:replacement :extra)))))
        (setf (fdefinition 'realp) original) (fmakunbound alias)))`);
  }
  check(`(equal (multiple-value-list (subtypep '(real 1 2) 'real)) '(t t))`);
  console.log('Sealed builtin dependencies: actual runtime workers, original objects, dynamic user calls, NOTINLINE, aliases, callbacks and values pass.');
} finally { vm.close(); }
