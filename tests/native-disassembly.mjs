import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const vm = await createCompilerTestVM(image);
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(`(not (null ${source}))`), 'T\n', source);
try {
  check('(= (miricl-compiler::compiler-primitive 33 nil) 1)');
  evaluate(`(miricl-compiler::ensure-compiler)
    (setq miricl-compiler:*compile-evaluations* nil)`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir '(lambda (x) (+ x 1))))
               (function (first (miricl-compiler::hir-unit-functions unit))))
    (and (= (miricl-compiler::hir-function-debug-level function) 1)
      (null (nth-value 2 (miricl-compiler:emit-native-module unit)))))`);
  for (const level of [0, 1]) {
    check(`(let ((fn (miricl-compiler:compile-native '(lambda (x)
        (declare (optimize (debug ${level}))) (+ x 1)))))
      (and (null (miricl-compiler::compiler-primitive 32 fn))
        (not (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble fn))))
        (= (funcall fn 41) 42)))`);
  }
  evaluate(`(setf (fdefinition 'inspectable)
    (miricl-compiler:compile-native '(lambda (x)
      (declare (optimize (debug 2))) (values (+ x 1) (- x 1)))))`);
  // Disassembly needs neither the JavaScript service nor the native loader.
  const services = ['javascript-services', 'native-services'].map(name => vm.vm[name]);
  const saved = services.map(table => table.get(0));
  services.forEach(table => table.set(0, null));
  try {
    check(`(let ((fn #'inspectable) (result :unset))
      (let ((text (with-output-to-string (*standard-output*) (setq result (disassemble 'inspectable)))))
        (and (null result) (eq fn #'inspectable)
          (search "Wasm instructions for function" text)
          (search "local.get" text) (search "call $native-" text)
          (equal (multiple-value-list (inspectable 10)) '(11 9)))))`);
  } finally { services.forEach((table, i) => table.set(0, saved[i])); }
  evaluate(`(defparameter *original-listing*
    (with-output-to-string (*standard-output*) (disassemble #'inspectable)))
    (format nil "~10000A" "scratch")
    (miricl-compiler:compile-native '(lambda () :another-module))`);
  check(`(equal *original-listing* (with-output-to-string (*standard-output*) (disassemble #'inspectable)))`);
  check(`(let ((factory (miricl-compiler:compile-native '(lambda (x)
      (declare (optimize (debug 2))) (lambda (y) (incf x y))))))
    (let* ((first (funcall factory 10)) (second (funcall factory 20)) (before (funcall first 1)))
      (and (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble first)))
        (= before 11) (= (funcall first 2) 13) (= (funcall second 3) 23))))`);
  check(`(let ((factory (miricl-compiler:compile-native '(lambda ()
      (declare (optimize (debug 2)))
      (values (lambda (x) (declare (optimize (debug 0))) (+ x 1)) (lambda (x) (+ x 2)))))))
    (multiple-value-bind (plain debug) (funcall factory)
      (and (null (miricl-compiler::compiler-primitive 32 plain))
        (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble debug)))
        (= (funcall plain 10) 11) (= (funcall debug 10) 12))))`);
  check(`(let ((factory (miricl-compiler:compile-native '(lambda ()
      (lambda (x) (declare (optimize (debug 2))) (+ x 1))))))
    (and (null (miricl-compiler::compiler-primitive 32 factory))
      (search "Wasm instructions"
        (with-output-to-string (*standard-output*) (disassemble (funcall factory))))))`);
  evaluate(`(declaim (optimize (debug 2)))
    (setf (fdefinition 'global-debug) (miricl-compiler:compile-native '(lambda (x) (+ x 1))))
    (setf (fdefinition 'local-override) (miricl-compiler:compile-native
      '(lambda (x) (declare (optimize (debug 0))) (+ x 1))))
    (declaim (optimize (debug 1)))`);
  check(`(and (miricl-compiler::compiler-primitive 32 #'global-debug) (null (miricl-compiler::compiler-primitive 32 #'local-override)))`);
  for (const body of [
    '(locally (declare (optimize (debug 2))) (+ x 1))',
    '(let ((y x)) (declare (optimize (debug 2))) (+ y 1))',
    '(multiple-value-bind (y) x (declare (optimize (debug 2))) (+ y 1))',
    '(if x -1000 1000)', '(car x)',
    '(block done (funcall (lambda () (return-from done x))))',
  ]) {
    check(`(let ((fn (miricl-compiler:compile-native '(lambda (x)
      ${body.startsWith('(locally') || body.startsWith('(let') || body.startsWith('(multiple-value-bind')
        ? '' : '(declare (optimize (debug 2)))'} ${body}))))
      (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble fn))))`);
  }
  vm.mountFile('/debug-source.lisp', `
    (defun debug-bundle-factory (x)
      (declare (optimize (debug 2)))
      (lambda (y) (incf x y)))
    (defun debug-bundle-plain (x) (+ x 1))`);
  evaluate(`(compile-file "/debug-source.lisp") (load "/debug-source.wasm")
    (miricl-compiler:compile-native-bundle "/debug-source.wasm" "/debug-bundle.wasm")`);
  const fresh = await createCompilerTestVM(image);
  try {
    const initialCompilerState = fresh.evaluate('miricl-compiler::*compiler-state*').output;
    fresh.mountFile('/debug-bundle.wasm', await vm.readFile('/debug-bundle.wasm'));
    const output = fresh.evaluate(`(progn (load "/debug-bundle.wasm")
      (let ((fn (debug-bundle-factory 10)))
        (and (search "Wasm instructions"
               (with-output-to-string (*standard-output*) (disassemble #'debug-bundle-factory)))
          (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble fn)))
          (= (funcall fn 1) 11) (= (funcall fn 2) 13)
          (null (miricl-compiler::compiler-primitive 32 #'debug-bundle-plain)))))`, {eof: true}).output;
    assert.equal(output, 'T\n');
    assert.equal(fresh.evaluate('miricl-compiler::*compiler-state*').output, initialCompilerState);
  } finally { fresh.close(); }
  evaluate(`(setq miricl-compiler:*compile-evaluations* t)
    (defun automatic-debug (x) (declare (optimize (debug 2))) (+ x 1))
    (defun interactive-debug (x) (declare (optimize (debug 3))) (+ x 1))`);
  check(`(and (null (function-lambda-expression #'automatic-debug))
    (function-lambda-expression #'interactive-debug)
    (search "Wasm instructions" (with-output-to-string (*standard-output*) (disassemble #'automatic-debug)))
    (= (automatic-debug 10) 11) (= (interactive-debug 10) 11))`);
} finally { vm.close(); }
console.log('Native disassembly: CL-only decoding/output, DEBUG policy, closures, scratch lifetime and instruction cases pass.');
