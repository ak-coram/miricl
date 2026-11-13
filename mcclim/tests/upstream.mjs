// Run McCLIM's own ASDF-discovered FiveAM suite in the delivered Wasm image.
// node mcclim/tests/upstream.mjs [image.wasm] [FiveAM dependency directory] [suite]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from '../../scripts/miricl.mjs';
import {mcclimFiles, root} from '../files.mjs';

const image = process.argv[2] ?? path.join(root, 'build/miricl-mcclim.wasm');
const dependencies = process.argv[3] ?? path.join(root, 'build/test-deps');
const suite = process.argv[4] ?? ':mcclim';
const report = suite === ':mcclim' ? 'upstream-results' : `upstream-${suite.replace(/[^a-z0-9]+/giu, '-')}`;
const output = path.join(root, `build/${report}.sexp`);
const vm = await Miricl.instantiate(await fs.readFile(image),
  {onOutput: text => process.stdout.write(text)});
async function evaluate(source) {
  const result = await vm.evaluateAsync(source, {eof: true});
  if (result.output) process.stdout.write(result.output);
  return result;
}
async function mount(directory, guest) {
  for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
    if (entry.name.startsWith('.')) continue;
    const source = path.join(directory, entry.name), target = `${guest}/${entry.name}`;
    if (entry.isDirectory()) await mount(source, target);
    else if (entry.isFile()) vm.mountFile(target, await fs.readFile(source));
  }
}
try {
  for (const [name, bytes] of await mcclimFiles()) vm.mountFile(name, bytes);
  for (const name of ['fiveam', 'asdf-flv', 'trivial-backtrace'])
    await mount(path.join(dependencies, name), `/test-deps/${name}`);
  vm.mountFile('/miricl-asdf.wasm', await fs.readFile(path.join(root, '../miricl-asdf.wasm')));
  await evaluate(`(load "/miricl-asdf.wasm")
    (setq miricl-compiler:*compile-evaluations* nil)
    (asdf:initialize-source-registry
      '(:source-registry (:tree "/mcclim/") (:tree "/test-deps/")
        :ignore-inherited-configuration))
    (asdf:initialize-output-translations
      '(:output-translations (t "/test-cache/") :ignore-inherited-configuration))
    (asdf:load-asd "/mcclim/mcclim.asd")
    (asdf:register-immutable-system "mcclim")
    (asdf:register-immutable-system "alexandria")
    (asdf:load-system "mcclim/test")
    (values)`);
  await evaluate(`
    (setq miricl-compiler:*compile-evaluations* t)
    (defmethod fiveam::run-test-lambda :around ((test fiveam::test-case))
      (let ((previous-tail (last fiveam::result-list)))
        (prog1 (call-next-method)
          (let* ((new-results (if previous-tail (cdr previous-tail) fiveam::result-list))
                 (failures (remove-if-not #'fiveam::test-failure-p new-results)))
            (when failures
              (fiveam:explain! failures)
              (dolist (result failures)
                (when (typep result 'fiveam::unexpected-test-failure)
                  (let ((condition (fiveam::actual-condition result)))
                    (when (typep condition 'type-error)
                      (let ((*print-readably* nil) (*print-level* 4) (*print-length* 8))
                        (format t "~&Datum ~S expected ~S~%"
                          (type-error-datum condition)
                          (type-error-expected-type condition))))))))))))
    (progn
      (defparameter cl-user::*upstream-results*
        (fiveam:run (let ((*read-eval* nil))
                     (read-from-string ${JSON.stringify(suite)}))))
      (fiveam:explain! cl-user::*upstream-results*)
      (values))
    (with-open-file (stream "/upstream-results.sexp" :direction :output
                            :if-exists :supersede)
      (let ((*print-circle* t) (*print-readably* nil))
        (write (mapcar (lambda (result)
                        (list :test (fiveam::name (fiveam::test-case result))
                              :status (type-of result)
                              :reason (fiveam::reason result)))
                      cl-user::*upstream-results*) :stream stream))
      (values))
    (values)`);
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, await vm.readFile('/upstream-results.sexp'));
  const status = await evaluate(`(and cl-user::*upstream-results*
    (nth-value 0 (fiveam:results-status cl-user::*upstream-results*)))`);
  assert.equal(status.output.trim(), 'T', `Upstream McCLIM suite failed; see ${output}`);
} catch (error) {
  console.error(error.message, error.output ?? '', error.backtrace ?? '');
  process.exitCode = 1;
} finally {
  vm.close();
}
