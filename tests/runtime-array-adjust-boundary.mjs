import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2]));
try {
  vm.vm['test-array-adjust-install']();
  for (const mode of [0, 1]) assert.equal(vm.vm['test-array-adjust-packets'](mode), 1, `packet mode ${mode}`);
  for (const mode of [0, 1, 2, 3]) assert.equal(vm.vm['test-array-adjust-forged'](mode), 1, `forged header mode ${mode}`);
  if (vm.vm['test-array-adjust-existing-cycle']) {
    assert.equal(vm.vm['test-array-adjust-existing-cycle'](), 1, 'existing target cycle and pending packet');
  }
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const args of [
    '13 nil nil', '-1 nil nil', '1.0 nil nil', '1000000000000000000000000000000 nil nil',
    '0 nil nil', '0 42 nil', '2 nil nil', '3 #(a) #()', '3 #(a) #(1 t nil nil -1 nil nil 0)',
    '3 #(a) #(1 t nil nil 8 nil nil 0)', '3 #(a) #(1 t nil nil 0 nil :bad 0)',
    '3 #(a) #(1 t nil nil 0 nil nil :bad)',
    '4 #(a) #()', '4 #(a) #(#(b) -1 0 1)', '4 #(a) #(#(b) 0 0 2)',
    '4 #(a) #(#(b) 1 0 1)', '4 #(a) #(#(b) 0 1 1)', '4 #(a) #(#*1 0 0 1)',
    '5 #(a) #(b)', '6 #(a) nil', '7 #(a) 0', '8 "a" 3', '8 #*1 :bad',
    '11 #(a) nil', '11 #(a) (cons -1 :bad)', '11 #(a) (cons 1 :bad)',
  ]) {
    check(`(handler-case (progn (funcall *array-adjust-metadata* ${args}) nil) (error () t))`);
    check(`(equalp (funcall *array-adjust-metadata* 0 #(a) nil) #(0 1 nil -1))`);
  }
  for (const args of ['0 nil', '0 nil nil nil'])
    check(`(handler-case (progn (funcall *array-adjust-metadata* ${args}) nil) (error () t))`);
  check(`(let* ((a (make-array 2 :adjustable t :initial-element :old))
                  (b (make-array 2 :adjustable t :displaced-to a)))
    (and (handler-case (progn (funcall *array-adjust-metadata* 5 a b) nil) (error () t))
         (eq :old (aref a 0)) (= 2 (array-total-size a))))`);
  check(`(let ((a (make-array 1 :adjustable t)) (b (make-array '(1 1) :adjustable t)))
    (handler-case (progn (funcall *array-adjust-metadata* 5 a b) nil) (error () t)))`);
  check(`(let ((a (vector 0 1 2 3 4)))
    (and (eq a (funcall *array-adjust-metadata* 4 a (vector a 1 0 4))) (equalp a #(0 0 1 2 3))))`);
  check(`(= (+ 20 22) 42)`);
  console.log('Array adjustment checked metadata, arity, mutation rejection, overlap, packets and recovery pass.');
} finally { vm.close(); }
