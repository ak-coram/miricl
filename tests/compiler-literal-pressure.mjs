import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (values)');
  // Cache roots are a bounded resource, shared with the ordinary frame. More
  // distinct cheap operands must not cause proportional function-wide growth.
  check(`(let ((counts nil))
    (dolist (size '(24 96))
      (let* ((names (loop for i below size collect (intern (format nil "*PRESSURE-~D*" i))))
             (hot (car (last names)))
             (unit (miricl-compiler:compile-to-hir
                     (list 'lambda nil (list 'declare (cons 'special names))
                       (cons 'values (append (loop repeat 8 append names)
                                             (make-list 16 :initial-element hot))))))
             (function (first (miricl-compiler::hir-unit-functions unit)))
             (miricl-compiler::*wasm-live-values*
               (make-array (1+ (miricl-compiler::hir-function-value-count function)) :initial-element t)))
        (let ((blocks (miricl-compiler::hir-function-blocks function)))
          (let ((selected (miricl-compiler::native-loop-literals blocks t)))
            (unless (eq (first selected) hot) (error "Lost the highest-benefit cache"))
            (push (length selected) counts))
          (unless (null (miricl-compiler::native-loop-literals blocks t 1048576))
            (error "Cached operands in an exhausted frame allowance")))))
    (and (plusp (first counts)) (= (first counts) (second counts))
         (< (first counts) 24)))`);
  // A recursive call before the first cached operand still retains the fixed
  // physical frame. Check that this case gets no ENTRY caches at all.
  check(`(let* ((unit (miricl-compiler:compile-to-hir
                     '(lambda (x) (pressure-callee x)
                        (values :late :late :late :late :late :late))))
               (function (first (miricl-compiler::hir-unit-functions unit)))
               (miricl-compiler::*wasm-live-values*
                 (make-array (1+ (miricl-compiler::hir-function-value-count function)) :initial-element t)))
    (null (miricl-compiler::native-loop-literals
            (miricl-compiler::hir-function-blocks function) t)))`);

  const names = Array.from({length: 80}, (_, i) => `pressure-effect-${i}`);
  const repetitions = 8;
  const calls = Array.from({length: repetitions}, () => names.map(name => `(${name} n)`).join(' ')).join('\n');
  vm.evaluate(`(defparameter *pressure-effects* 0)
    (setf (fdefinition 'pressure-effect) (miricl-compiler:compile-native
      '(lambda (x) (declare (ignore x)) (incf *pressure-effects*) nil)))
    (dolist (name '(${names.join(' ')})) (setf (symbol-function name) #'pressure-effect))
    (setf (fdefinition 'pressure-recur) (miricl-compiler:compile-native
      '(lambda (n) (declare (type (integer 0 4096) n) (inline zerop 1- 1+)
                           (optimize (safety 0) (debug 0)))
         (if (zerop n) 0
           (let ((answer (pressure-recur (1- n)))) ${calls} (1+ answer)))))) (values)`);
  // Use the ordinary browser-compatible async adapter and its default stack.
  const depth = 1024;
  const result = await vm.evaluateAsync(`(let ((answer (pressure-recur ${depth})))
    (and (= answer ${depth}) (= *pressure-effects* ${depth * names.length * repetitions})))`);
  assert.equal(result.output, 'T\n');
  console.log('Literal caches stay bounded, rematerialize across retained calls, and preserve recursive results and effects.');
} finally {vm.close();}
