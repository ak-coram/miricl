import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const vm = await Miricl.instantiate(image);
const check = async source => assert.equal((await vm.evaluateAsync(source, {eof: true})).output, 'T\n', source);
try {
  await vm.evaluateAsync(`
    (defpackage :pln-target-a (:use :cl) (:export :item))
    (defpackage :pln-target-b (:use :cl) (:export :item))
    (defpackage :pln-global (:use :cl))
    (defpackage :pln-owner-a (:use :cl)
      (:local-nicknames (:short :pln-target-a) (:pln-global :pln-target-a)))
    (defpackage :pln-owner-b (:use :cl) (:local-nicknames (:short :pln-target-b)))
    (values)`, {eof: true});
  await check(`(and (null (find-package "SHORT"))
    (eq (let ((*package* (find-package "PLN-OWNER-A"))) (find-package "SHORT")) (find-package "PLN-TARGET-A"))
    (eq (let ((*package* (find-package "PLN-OWNER-B"))) (find-package "SHORT")) (find-package "PLN-TARGET-B"))
    (eq (let ((*package* (find-package "PLN-OWNER-A"))) (find-package "PLN-GLOBAL")) (find-package "PLN-TARGET-A"))
    (eq (let ((*package* (find-package "PLN-OWNER-A"))) (find-package (find-package "PLN-TARGET-B"))) (find-package "PLN-TARGET-B"))
    (null (let ((*package* (find-package "PLN-OWNER-A"))) (find-package "short")))
    (equal (package-nicknames "PLN-TARGET-A") nil))`);
  await check(`(let ((*package* (find-package "PLN-OWNER-A")))
    (and (eq (read-from-string "short:item") (intern "ITEM" "PLN-TARGET-A"))
      (eq (read-from-string "short::private") (intern "PRIVATE" "PLN-TARGET-A"))))`);
  await check(`(and
    (handler-case (eval '(defpackage :pln-invalid (:local-nicknames (:short)))) (program-error () t))
    (handler-case (eval '(defpackage :pln-invalid (:local-nicknames (:short :pln-target-a :extra)))) (program-error () t)))`);
  vm.mountFile('/pln.lisp', `(defpackage :pln-fasl (:use :cl) (:local-nicknames (:short :pln-target-a)))
    (in-package :pln-fasl) (defun item-symbol () 'short:item)`);
  await vm.evaluateAsync('(compile-file "/pln.lisp" :output-file "/pln.wasm") (values)', {eof: true});
  const fasl = await vm.readFile('/pln.wasm');
  const fresh = await Miricl.instantiate(image);
  try {
    fresh.mountFile('/pln.wasm', fasl);
    const result = await fresh.evaluateAsync(`(defpackage :pln-target-a (:use :cl) (:export :item))
      (load "/pln.wasm")
      (eq (funcall (intern "ITEM-SYMBOL" "PLN-FASL")) (intern "ITEM" "PLN-TARGET-A"))`, {eof: true});
    assert.ok(result.output.endsWith('T\n'));
  } finally {fresh.close();}
  console.log('Package local nicknames: scoped reader and FIND-PACKAGE lookup, global shadowing, validation and fresh FASL load passed.');
} finally {vm.close();}
