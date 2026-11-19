// Restore the actual image in a fresh matching VM. This is independent of any
// retained development REPL and never creates DOM resources.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../../scripts/miricl.mjs';
import {applicationCheck} from '../image.mjs';
const image = process.argv[2] ?? 'build/lem/miricl-lem.wasm';
const vm = await Miricl.instantiate(await fs.readFile(image), {onOutput: text => process.stdout.write(text)});
try {
  await vm.evaluateAsync(applicationCheck, {eof: true});
  const result = await vm.evaluateAsync(`
    (progn
    (let ((session (miricl-ide-services:make-session)))
      (unwind-protect
           (progn
             (assert (not (miricl-ide-services:session-worker-alive-p session)))
             (dolist (case '(("(values)" nil) ("(values nil)" (nil)) ("(values 7 nil :tail)" (7 nil :tail))))
               (let ((request (miricl-ide-services:submit session :eval (first case))))
                 (loop repeat 3000 while (miricl-ide-services:session-worker-alive-p session)
                       do (sleep 0.01))
                 (assert (not (miricl-ide-services:session-worker-alive-p session)))
                 (assert (eq :ok (miricl-ide-services:request-state request)))
                 (assert (equal (second case) (miricl-ide-services:request-values request))))))
        (miricl-ide-services:close-session session)
        (loop repeat 3000 while (miricl-ide-services:session-worker-alive-p session) do (sleep 0.01))
        (assert (not (miricl-ide-services:session-worker-alive-p session)))))
    (assert (null (lem-lisp-mode/connection:current-connection)))
    t)`, {eof: true});
  assert.equal(result.output, 'T\n');
  assert.ok(vm.vm['core-checkpoint-safe'](), 'The restored application has no live worker thread.');
  console.log('LEM fresh restore: ready compiler, real ASDF graph, lazy provider, values and quiescent cleanup pass.');
} finally {vm.close();}
