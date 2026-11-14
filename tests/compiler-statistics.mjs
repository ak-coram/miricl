import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {enableCompilationStatistics, disableCompilationStatistics, readCompilationStatistics} from '../scripts/compilation-statistics.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  check('(null (compilation-statistics-report))');
  const emit = async () => {
    vm.evaluate(`(multiple-value-bind (bytes literals)
      (let ((*gensym-counter* 0)) (emit-native-module (compile-to-hir
        '(lambda (x) (declare (optimize (safety 0) (debug 0)) (inline consp car))
          (if (consp x) (car x) nil)))))
      (defparameter *statistics-test-function* (compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/statistics-test.wasm" :direction :output :if-exists :supersede
        :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    return vm.readFile('/statistics-test.wasm');
  };
  const ordinary = await emit();
  enableCompilationStatistics(vm);
  const measured = await emit();
  assert.deepEqual(measured, ordinary, 'Collecting statistics must not change generated code');
  const rows = await readCompilationStatistics(vm);
  const count = (rule, detail) => rows.find(row => row.rule === rule && row.detail === detail)?.count ?? 0;
  assert.ok(count('proof/branch-premise', 'cons') > 0);
  assert.ok(count('optimization/inline-primitive', 'car-cons') > 0);
  assert.ok(count('lowering/wasm-output-bytes', '') > 0);
  check(`(and (eql (funcall *statistics-test-function* '(17)) 17)
    (null (funcall *statistics-test-function* 7)))`);
  disableCompilationStatistics(vm);
  check('(null (compilation-statistics-report))');
  check(`(and (equal (multiple-value-list (with-compilation-statistics (counts) (values 11 :extra))) '(11 :extra))
    (with-compilation-statistics (outer)
      (record-compilation-statistic :test nil 3)
      (with-compilation-statistics (inner)
        (record-compilation-statistic :test nil 7)
        (assert (= (gethash '(:test nil) inner) 7)))
      (= (gethash '(:test nil) outer) 3)))`);
  check(`(and
    (handler-case (progn (compilation-statistics-report 7) nil) (compiler-input-error () t))
    (let ((bad (make-hash-table :test 'eq)) (key (list :cycle nil)))
      (setf (cddr key) key (gethash key bad) 1)
      (handler-case (progn (compilation-statistics-report bad) nil) (compiler-input-error () t)))
    (with-compilation-statistics (counts)
      (record-compilation-statistic :recovered nil 1)
      (plusp (third (first (compilation-statistics-report counts))))))`);
  console.log('Compiler statistics: opt-in collection, unchanged output, proof/application counters, nested scopes, values, checked reports and recovery pass.');
} finally {vm.close();}
