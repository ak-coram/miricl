import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  check(`(list (ldb (byte 0 (ash 1 100)) -1)
               (ldb (byte 1 (ash 1 100)) -1)
               (ldb (byte (ash 1 100) (ash 1 100)) 17)
               (ldb-test (byte (ash 1 100) 0) -1)
               (ldb-test (byte (ash 1 100) (ash 1 100)) 17)
               (mask-field (byte (ash 1 100) (ash 1 100)) 17))`, '(0 1 0 T NIL 0)');
  check(`(list (ldb (byte 64 0) -9223372036854775808)
               (ldb (byte 64 64) (1- (ash 1 128)))
               (ldb (byte 3 0) -8) (ldb (byte 4 0) -8)
               (ldb-test (byte 3 0) -8) (ldb-test (byte 4 0) -8))`,
        '(9223372036854775808 18446744073709551615 0 8 NIL T)');
  check(`(list (multiple-value-list (ldb (byte 8 0) (values 511 :extra)))
               (multiple-value-list (ldb-test (byte 8 0) (values 511 :extra))))`, '((255) (T))');
  check(`(let ((seen nil))
    (list (ldb (progn (push :spec seen) (byte 4 2))
               (progn (push :integer seen) 255)) (reverse seen)))`, '(15 (:SPEC :INTEGER))');
  check(`(handler-case (ldb (byte (ash 1 100) 0) -1) (storage-condition () :storage))`, ':STORAGE');
  for (const source of ['(ldb-test (byte 0 0) nil)', '(ldb (byte 1 0) 1.0)',
                        "(ldb '(1 . -1) 42)", "(ldb-test '(nil . 0) 42)",
                        '(let ((spec (cons 1 nil))) (setf (cdr spec) spec) (ldb spec 42))']) {
    check(`(handler-case ${source} (type-error () :type))`, ':TYPE');
  }
  check('(ldb (byte 4 4) 255)', '15');
  console.log('Integer field extraction: exact windows, huge counts, sign extension, effects, values and recovery pass.');
} finally { vm.close(); }
