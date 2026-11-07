import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const bytes = fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm');
const vm = await Miricl.instantiate(bytes);
const fresh = await Miricl.instantiate(bytes);
const check = (machine, source, expected = 'T') =>
  assert.equal(machine.evaluate(source, {eof: true}).output, expected + '\n', source);
try {
  vm.mountFile('/printers.lisp', `
    (defstruct (printed (:print-function print-printed)) (value 7))
    (defun print-printed (object stream depth)
      (format stream "<~D/~D>" (printed-value object) depth))
    (defstruct (inherited (:include printed)))
    (defstruct (defaulted (:include printed) (:print-object)))
    (defstruct (two-args (:print-object (lambda (object stream)
      (format stream "two/~D" (two-args-value object))))) (value 9))
  `);
  check(vm, '(pathnamep (compile-file "/printers.lisp"))');
  fresh.mountFile('/printers.wasm', await vm.readFile('/printers.wasm'));
  check(fresh, '(load "/printers.wasm")');
  check(fresh, `(and (equal (write-to-string (make-printed)) "<7/0>")
    (equal (write-to-string (list (make-printed))) "(<7/1>)")
    (equal (write-to-string (make-inherited)) "<7/0>")
    (equal (write-to-string (make-defaulted)) "#S(DEFAULTED :VALUE 7)")
    (equal (write-to-string (make-two-args)) "two/9"))`);
  check(vm, `(let ((prefix "captured"))
    (defstruct (captured (:print-object (lambda (object stream)
      (declare (ignore object)) (write-string prefix stream))))) t)`);
  check(vm, '(write-to-string (make-captured))', '"captured"');
  check(vm, `(and
    (handler-case (eval '(defstruct (bad (:type list) (:print-object)))) (program-error () t))
    (handler-case (eval '(defstruct (bad (:print-object) (:print-function)))) (program-error () t)))`);
  console.log('DEFSTRUCT printers: depth, named/lambda printers, inheritance, default reset, lexical capture, and portable FASLs pass.');
} finally { vm.close(); fresh.close(); }
