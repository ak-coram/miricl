import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2]));
try {
  vm.vm['test-array-upgrade-install']();
  for (const mode of [0, 1]) assert.equal(vm.vm['test-array-upgrade-packets'](mode), 1, `packet mode ${mode}`);
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const args of [
    '5 nil nil', '-1 nil nil', '1.0 nil nil', '1000000000000000000000000000000 nil nil',
    '2 nil 0', '2 #() 0', '2 #(1 1) 0', '2 #(0 1 t) 0',
    '2 #(1 0 t) 0', '2 #(1 -1 t) 0', '2 #(1 1 :bad) 0',
    '2 #(1 #() t) 0', '2 #(1 1 t) #(1 0 nil)',
  ]) {
    check(`(handler-case (progn (funcall *array-upgrade-metadata* ${args}) nil) (error () t))`);
    check(`(= (funcall *array-upgrade-metadata* 2 #(1 1 t) 1) 0)`);
  }
  for (const args of ['0 nil', '0 nil nil nil'])
    check(`(handler-case (progn (funcall *array-upgrade-metadata* ${args}) nil) (error () t))`);
  check(`(= (funcall *array-upgrade-metadata* 2 #(-1 100000000000000000000000000000 t) 0) -1)`);
  check(`(= (funcall *array-upgrade-metadata* 2 0 #(1 100000000000000000000000000000 nil)) -1)`);
  check(`(eq (funcall *array-upgrade-metadata* 0 'bit nil) 'bit)`);
  check(`(equalp (funcall *array-upgrade-metadata* 1 '(integer 0 1) nil) #(0 1 1))`);
  check(`(= (+ 20 22) 42)`);
  console.log('Array upgrading checked metadata, endpoint representation, packet preservation and recovery pass.');
} finally { vm.close(); }
