// node mcclim/build.mjs [silex|core|browser|demo] [runtime.wasm]
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from '../scripts/miricl.mjs';
import {mcclimFiles, root} from './files.mjs';
const mode = process.argv[2] ?? 'core';
if (!['silex', 'core', 'browser', 'demo'].includes(mode)) throw new Error('Expected silex, core, browser or demo.');
const runtime = await fs.readFile(process.argv[3] ?? path.join(root, '../miricl.wasm'));
const files = await mcclimFiles();
const hash = createHash('sha256').update(runtime);
for (const [name, bytes] of [...files].sort(([a], [b]) => a.localeCompare(b))) hash.update(name).update(bytes);
const cacheDirectory = path.join(root, 'build/cache', hash.digest('hex'));
const cachedFiles = new Set();
const libraryParts = [];
const vm = await Miricl.instantiate(runtime, {onOutput: text => {
  for (const match of text.matchAll(/Loading #P"(\/cache\/[^"\n]+\.wasm)"/gu)) cachedFiles.add(match[1]);
  process.stdout.write(text);
}});
async function evaluate(source) {
  const result = await vm.evaluateAsync(source, {eof: true});
  if (result.output) process.stdout.write(result.output);
}
try {
  for (const [name, bytes] of files) vm.mountFile(name, bytes);
  async function mountCache(directory, guest) {
    for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
      const file = path.join(directory, entry.name), name = `${guest}/${entry.name}`;
      if (entry.isDirectory()) await mountCache(file, name);
      else vm.mountFile(name, await fs.readFile(file));
    }
  }
  try {await mountCache(cacheDirectory, '/cache');} catch (error) {if (error.code !== 'ENOENT') throw error;}
  vm.mountFile('/miricl-asdf.wasm', await fs.readFile(path.join(root, '../miricl-asdf.wasm')));
  await evaluate('(load "/miricl-asdf.wasm") (setq miricl-compiler:*compile-evaluations* nil) (values)');
  for (const name of ['platform', 'mop', 'gray-streams', 'garbage']) {
    const file = `/port/compatibility/${name}`;
    await evaluate(`(compile-file "${file}.lisp") (load "${file}.wasm") (values)`);
    libraryParts.push(await vm.readFile(`${file}.wasm`));
  }
  await evaluate(`
    (asdf:initialize-source-registry '(:source-registry (:tree "/mcclim/") (:tree "/deps/") (:tree "/port/backend/") :ignore-inherited-configuration))
    (asdf:initialize-output-translations '(:output-translations (t "/cache/") :ignore-inherited-configuration))
    (defparameter *mcclim-original-load* (symbol-function 'load))
    (setf (symbol-function 'load) (lambda (file &rest args)
      (format t "~&Loading ~A~%" file) (finish-output)
      (handler-bind ((unbound-variable (lambda (c) (format t "~&Unbound: ~S in ~A~%" (cell-error-name c) file)))
                     (undefined-function (lambda (c) (format t "~&Undefined: ~S in ~A~%" (cell-error-name c) file))))
        ${process.env.MCCLIM_TRACE ? `(if (search "/system/dynamic-vars.wasm" (namestring file))
          (let ((*package* *package*))
            (with-open-file (stream "/mcclim/Silex/system/dynamic-vars.lisp")
              (loop for form = (read stream nil :eof) until (eq form :eof) do
                (let ((*print-length* 3) (*print-level* 3)) (format t "~&Source form: ~S~%" form))
                (setq cl-user::*mcclim-top-form* form) (eval form))))
          (apply *mcclim-original-load* file args))` : '(apply *mcclim-original-load* file args)'})))
    (defparameter *mcclim-top-form* nil)
    (labels ((find-private (tree)
               (cond ((and (symbolp tree) (string= (symbol-name tree) "%COMPILE-TOP-LEVEL")) tree)
                     ((consp tree) (or (find-private (car tree)) (find-private (cdr tree)))))))
      (let* ((symbol (find-private (function-lambda-expression #'compile-file)))
             (original (symbol-function symbol)))
        (setf (symbol-function symbol) (lambda (form &rest arguments)
          (setq cl-user::*mcclim-top-form* form) (apply original form arguments)))))
    (values)`);
  const system = {silex: 'silex', core: 'clim-core', browser: 'mcclim-browser', demo: 'clim-examples'}[mode];
  if (process.env.MCCLIM_METHOD_TRACE) await evaluate(`
    (defmethod add-method :around ((gf standard-generic-function) (method standard-method))
      (handler-bind ((error (lambda (condition)
          (let ((*print-readably* nil) (*print-length* 12) (*print-level* 3))
            (format t "~&ADD-METHOD ~S: ~S; GF ~S; method ~S; specializers ~S~%"
              (miricl-mop:generic-function-name gf) (type-of condition)
              (miricl-mop:generic-function-lambda-list gf) (miricl-mop:method-lambda-list method)
              (miricl-mop:method-specializers method))))))
        (call-next-method))) (values)`);
  await evaluate(`(asdf:load-system "${system}") (values)`);
  for (const file of cachedFiles) libraryParts.push(await vm.readFile(file));
  const library = path.join(root, `build/mcclim-${mode}-library.wasm`);
  await fs.mkdir(path.dirname(library), {recursive: true});
  await fs.writeFile(library, Buffer.concat(libraryParts.map((bytes, index) =>
    Buffer.from(index === 0 ? bytes : bytes.subarray(8)))));
  console.log(`Wrote standalone library ${library} (${libraryParts.length} FASLs, no ASDF).`);
  if (mode === 'core') {
    // Development checkpoint; the distributable image will be rebuilt from
    // the library's FASLs without the build-time ASDF environment.
    const target = path.join(root, 'build/mcclim-core.wasm');
    await fs.writeFile(target, vm.saveImage());
    console.log(`Wrote development checkpoint ${target}.`);
  }
  console.log(`McCLIM ${mode} loaded successfully.`);
} catch (error) {
  console.error(error.message, error.output ?? '', error.backtrace ?? '');
  try {console.error((await vm.evaluateAsync('(let ((*print-length* 100) (*print-level* 6)) (prin1 cl-user::*mcclim-top-form*) (values))', {eof: true})).output);} catch {}
  process.exitCode = 1;
} finally {
  for (const name of cachedFiles) {
    const target = path.join(cacheDirectory, name.slice('/cache/'.length));
    await fs.mkdir(path.dirname(target), {recursive: true});
    await fs.writeFile(target, await vm.readFile(name));
  }
  vm.close();
}
