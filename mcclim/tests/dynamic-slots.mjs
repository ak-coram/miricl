// Exercise upstream McCLIM's dynamic slots with miricl cooperative threads.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? new URL('../../miricl.wasm', import.meta.url)));
async function evaluate(source) {return (await vm.evaluateAsync(source, {eof: true})).output.trim();}
try {
  await evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  for (const name of ['platform', 'mop', 'garbage']) {
    vm.mountFile(`/${name}.lisp`, await fs.readFile(new URL(`../compatibility/${name}.lisp`, import.meta.url)));
    await evaluate(`(load "/${name}.lisp") (values)`);
  }
  // Supply the small CLIM-SYS threading interface used by this upstream file.
  await evaluate(`(defpackage :clim-sys (:use :cl)
      (:export :make-lock :with-lock-held :current-process))
    (in-package :clim-sys)
    (defun make-lock () (miricl-threads:make-lock))
    (defmacro with-lock-held ((lock) &body body)
      (list* 'miricl-threads:with-lock-held (list lock) body))
    (defun current-process () (miricl-threads:current-thread))
    (in-package :cl-user) (values)`);
  vm.mountFile('/dynamic-vars.lisp', await fs.readFile(new URL('../upstream/Silex/system/dynamic-vars.lisp', import.meta.url)));
  await evaluate(`(load "/dynamic-vars.lisp")
    (clim-sys/dynamic-vars::define-class-with-dynamic-slots test-dynamic ()
      ((value :dynamic :tls :initform 17 :accessor test-value)))
    (defparameter *test-dynamic* (make-instance 'test-dynamic)) (values)`);
  assert.equal(await evaluate(`(and (= (test-value *test-dynamic*) 17)
    (= (slot-value *test-dynamic* 'value) 17))`), 'T');
  assert.equal(await evaluate(`(let
      ((first (miricl-threads:make-thread (lambda ()
         (setf (test-value *test-dynamic*) 42)
         (miricl-threads:thread-yield) (test-value *test-dynamic*))))
       (second (miricl-threads:make-thread (lambda () (test-value *test-dynamic*)))))
    (equal (list (miricl-threads:join-thread first) (miricl-threads:join-thread second)
                 (test-value *test-dynamic*)) '(42 17 17)))`), 'T');
  assert.equal(await evaluate(`(and
    (= (clim-sys/dynamic-vars::slot-dlet (((*test-dynamic* 'value) 99))
         (test-value *test-dynamic*)) 99)
    (= (test-value *test-dynamic*) 17))`), 'T');
  console.log('PASS upstream dynamic slots: raw access, accessor/slot-value agreement, thread isolation and binding restoration');
} finally {vm.close();}
