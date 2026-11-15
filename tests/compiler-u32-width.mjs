// Internal width pricing must match actual unsigned Wasm serialization, including
// the unsigned high half, without changing module profitability at LEB tiers.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (ensure-compiler) (values)');
  check(`(let ((boundaries '(0 1 127 128 129 16383 16384 2097151 2097152
                            268435455 268435456 2147483647 2147483648 4294967295)))
    (and (every (lambda (number)
                  (= (wasm-u32-width number)
                     (length (with-wasm-bytes (wasm-u32 number))))) boundaries)
         (let ((state 1))
           (dotimes (i 256 t)
             (setq state (mod (+ (* state 1664525) 1013904223) 4294967296))
             (unless (= (wasm-u32-width state)
                        (length (with-wasm-bytes (wasm-u32 state))))
               (return nil))))))`);
  // Overflowing projected local slots must supply no cache proof, even though
  // ordinary checked emission remains available. No huge allocation is needed.
  check(`(every (lambda (limit) (null (native-loop-literals nil t limit)))
                '(-1 1/2 4294967296 1267650600228229401496703205376))`);
  // The cache sum, rather than only LOCAL-START, is guarded. A real persistent
  // literal collected from HIR pushes the maximum index past unsigned32.
  check(`(let* ((unit (compile-to-hir '(lambda () (list :width-cache :width-cache))))
                (function (first (hir-unit-functions unit)))
                (*wasm-live-values* (make-array (1+ (hir-function-value-count function)) :initial-element t)))
    (null (native-loop-literals (hir-function-blocks function) nil 4294967295)))`);
  vm.evaluate(`(defparameter *width-call* (compile-native
    '(lambda (x) (list x :a :a :a :a :a :a)))) (values)`);
  check(`(equal (funcall *width-call* 42) '(42 :a :a :a :a :a :a))`);
  console.log('Unsigned width pricing agrees with serialized u32 boundaries and samples; projected local overflow falls back and subsequent compilation recovers.');
} finally {vm.close();}
