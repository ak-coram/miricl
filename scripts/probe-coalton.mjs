import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from './miricl.mjs';
import {coaltonFiles} from './coalton-files.mjs';
import {createHash} from 'node:crypto';
const output = process.argv[3] ?? 'build/coalton/compile.json';
const mode = process.argv[4] ?? 'full';
if (!['full', 'compiler'].includes(mode)) throw new Error('Expected full or compiler build mode.');
const bootstrapPath = process.env.MIRICL_COALTON_BOOTSTRAP;
const bootstrap = bootstrapPath ? await fs.readFile(bootstrapPath) : null;
const cache = process.env.MIRICL_COALTON_CACHE_DIRECTORY ?? 'build/coalton/cache';
const report = {mode, stages: [], compilations: [], loads: [], fileTimings: []};
const activeFiles = [];
let stage;
const image = await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm');
const sources = await coaltonFiles();
const cacheHash = createHash('sha256').update(image);
if (bootstrap) {
  const metadata = JSON.parse(await fs.readFile(path.join(path.dirname(bootstrapPath), 'optimization.json'), 'utf8'));
  if (metadata.imageSha256 !== createHash('sha256').update(image).digest('hex')
      || metadata.coreSha256 !== createHash('sha256').update(bootstrap).digest('hex')
      || metadata.backend !== 'native-wasm' || metadata.scope !== 'compiler')
    throw new Error('Expected a native Coalton compiler bootstrap for this runtime.');
  cacheHash.update(bootstrap);
  report.bootstrap = {file: bootstrapPath, sha256: metadata.coreSha256};
}
for (const [name, bytes] of [...sources].sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0))
  cacheHash.update(name).update('\0').update(bytes);
const cacheKey = cacheHash.digest('hex');
const reuseCache = process.env.MIRICL_COALTON_CACHE === '1';
let previousKey;
try {previousKey = (await fs.readFile(path.join(cache, 'key.txt'), 'utf8')).trim();} catch (error) {
  if (error.code !== 'ENOENT') throw error;
}
if (previousKey !== cacheKey) await fs.rm(cache, {recursive: true, force: true});
await fs.mkdir(cache, {recursive: true});
await fs.writeFile(path.join(cache, 'key.txt'), cacheKey + '\n');
const cacheWrites = new Map();
async function saveFasl(file, previous) {
  const bytes = await vm.readFile(file);
  await previous;
  const target = path.join(cache, file.slice('/build/cache/'.length));
  await fs.mkdir(path.dirname(target), {recursive: true});
  await fs.writeFile(`${target}.tmp`, bytes);
  await fs.rename(`${target}.tmp`, target);
}
const vm = await Miricl.instantiate(image, {
  imports: {coaltonBuild: {event(kind, file) {
    if (kind === 'finished') {
      const {operation, start} = activeFiles.pop();
      const milliseconds = Math.round(performance.now() - start);
      report.fileTimings.push({operation, file, milliseconds});
      console.log(`Coalton ${operation} finished: ${file} (${milliseconds} ms)`);
      return;
    }
    activeFiles.push({operation: kind, file, start: performance.now()});
    report[kind === 'compile' ? 'compilations' : 'loads'].push(file);
    console.log(`Coalton ${kind}: ${file}`);
    if (kind === 'load' && file.startsWith('/build/cache/') && file.endsWith('.wasm'))
      cacheWrites.set(file, saveFasl(file, cacheWrites.get(file)).then(() => true, error => {
        console.warn(`Coalton cache ${file}: ${error.message}`);
        return false;
      }));
  }}},
});
const evaluate = async (name, source) => {
  stage = name; console.log(`Coalton: ${name}`); const start = performance.now();
  const heartbeat = setInterval(() => {
    const active = activeFiles.at(-1);
    console.log(`Coalton: still working on ${active ? `${active.operation} ${active.file}` : stage}`);
  }, 30000);
  try {
    const result = await vm.evaluateAsync(source, {eof: true});
    report.stages.push({name, milliseconds: Math.round(performance.now() - start), output: result.output});
    return result;
  } finally {clearInterval(heartbeat);}
};
try {
  for (const [name, bytes] of sources) vm.mountFile(name, bytes);
  async function mountCache(directory, guest) {
    for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
      const file = path.join(directory, entry.name), name = guest + '/' + entry.name;
      if (entry.isDirectory()) await mountCache(file, name);
      else if (entry.name.endsWith('.wasm')) vm.mountFile(name, await fs.readFile(file));
    }
  }
  if (reuseCache && previousKey === cacheKey) await mountCache(cache, '/build/cache');
  if (bootstrap) {
    vm.mountFile('/coalton-bootstrap.wasm', bootstrap);
    await evaluate('native compiler bootstrap', '(declaim (declaration values)) (load "/coalton-bootstrap.wasm") (values)');
    report.loads.push('/coalton-bootstrap.wasm');
  } else {
    vm.mountFile('/miricl-asdf.wasm', await fs.readFile('miricl-asdf.wasm'));
    await evaluate('ASDF', '(load "/miricl-asdf.wasm") (values)');
    report.loads.push('/miricl-asdf.wasm');
  }
  await evaluate('configuration', `
    (declaim (declaration values))
    (asdf:initialize-source-registry '(:source-registry (:directory "/coalton/")
      (:directory "/coalton/source-error/") (:tree "/quicklisp/") :ignore-inherited-configuration))
    (asdf:initialize-output-translations '(:output-translations (t "/build/cache/") :ignore-inherited-configuration))
    (js:define-import coalton-build-event ("coaltonBuild" "event") (:string :string) :void)
    (defparameter *coalton-original-compile* (symbol-function 'compile-file))
    (defparameter *coalton-original-load* (symbol-function 'load))
    (defparameter *coalton-top-form* nil)
    (labels ((find-private (tree name)
               (cond ((and (symbolp tree) (string= (symbol-name tree) name)) tree)
                     ((consp tree) (or (find-private (car tree) name) (find-private (cdr tree) name))))))
      (let* ((symbol (find-private (function-lambda-expression #'compile-file) "%COMPILE-TOP-LEVEL"))
             (original (symbol-function symbol)))
        (setf (symbol-function symbol) (lambda (form &rest args)
          (setq cl-user::*coalton-top-form* form) (apply original form args)))))
    (setf (symbol-function 'compile-file) (lambda (file &rest args)
      (let ((name (namestring file)))
        (coalton-build-event "compile" name)
        (unwind-protect (apply *coalton-original-compile* file args)
          (coalton-build-event "finished" name)))))
    (setf (symbol-function 'load) (lambda (file &rest args)
      (let ((name (if (streamp file) "<stream>" (namestring file))))
        (coalton-build-event "load" name)
        (unwind-protect (apply *coalton-original-load* file args)
          (coalton-build-event "finished" name)))))
    (values)`);
  if (bootstrap) await evaluate('preloaded compiler dependencies', `
    (unless (member "coalton-compiler" (asdf:already-loaded-systems) :test #'equal)
      (let* ((compiler (asdf:find-system "coalton-compiler"))
             (systems (list (list (asdf:component-name compiler) (asdf:component-version compiler)))))
        (dolist (component (asdf:required-components compiler :other-systems t))
          (let ((system (asdf:component-system component)))
            (pushnew (list (asdf:component-name system) (asdf:component-version system)) systems :test #'equal)))
        (dolist (system systems)
          (asdf:clear-system (first system))
          (asdf:register-immutable-system (first system) :version (second system)))))
    ;; The native prefix is a fixed build input. Source discovery must not
    ;; replace it with fresh portable definitions while compiling libraries.
    (dolist (name (asdf:already-loaded-systems)) (asdf:register-immutable-system name))
    (values)`);
  await evaluate('load Coalton', `(asdf:load-system "${mode === 'compiler' ? 'coalton-compiler' : 'coalton'}") (values)`);
  if (mode === 'full') await evaluate('smoke test', '(in-package :coalton-user) (coalton (+ 20 22))');
  await evaluate('preloaded system registry', `
    (cl:in-package :cl-user)
    (with-open-file (stream "/coalton-bundle-init.lisp" :direction :output :if-exists :supersede)
      (write '(in-package :cl-user) :stream stream) (terpri stream)
      (dolist (name (asdf:already-loaded-systems))
        (write (list '${mode === 'compiler' ? 'asdf:register-immutable-system' : 'asdf:register-preloaded-system'} name :version
                 (asdf:component-version (asdf:find-system name))) :stream stream)
        (terpri stream)))
    (compile-file "/coalton-bundle-init.lisp" :output-file "/coalton-bundle-init.wasm")
    (load "/coalton-bundle-init.wasm") (values)`);
  const parts = [];
  const units = [...new Set(report.loads.filter(file => file.endsWith('.wasm')))];
  for (const file of units) parts.push(Buffer.from(await vm.readFile(file)).subarray(8));
  const portable = Buffer.concat([Buffer.from([0,97,115,109,1,0,0,0]), ...parts]);
  const module = new WebAssembly.Module(portable);
  if (WebAssembly.Module.imports(module).length || WebAssembly.Module.exports(module).length
      || !WebAssembly.Module.customSections(module, 'miricl.fasl').length)
    throw new Error('Expected an import-free portable Coalton FASL bundle.');
  const bundlePath = path.join(path.dirname(output), mode === 'compiler' ? 'compiler-portable.wasm' : 'coalton-portable.wasm');
  await fs.mkdir(path.dirname(bundlePath), {recursive: true}); await fs.writeFile(bundlePath, portable);
  report.bundle = {portableBytes: portable.length, units,
    sha256: createHash('sha256').update(portable).digest('hex')};
  report.passed = true;
} catch (error) {
  report.passed = false; report.stage = stage;
  report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};
  try { report.form = (await vm.evaluateAsync('(let ((*print-length* 30) (*print-level* 8)) (write *coalton-top-form* :readably nil))', {eof: true})).output; } catch {}
  try {report.readerDiagnostic = (await vm.evaluateAsync(`
    (cl:in-package :cl-user)
    (format t "Features: ~S~%" *features*)
    (format t "SB locks: ~S~%" (eclector.reader:evaluate-feature-expression
      coalton-impl/parser/reader:*coalton-eclector-client* :sb-package-locks))
    (dolist (text '("#+sb-package-locks (sb-ext:lock-package x) 42"
                   "#+absent (no-such-package:foo) 42"))
      (handler-case (format t "Conditional: ~S~%" (eclector.reader:read-from-string text))
        (error (c) (format t "Conditional ERROR: ~A~%" c))))
    (let* ((text "(DEFINE X X))") (source (coalton-impl/source:make-source-string text)))
      (with-input-from-string (stream text)
        (coalton-impl/parser/reader:with-coalton-reader-context stream
          (dotimes (i 3)
            (format t "Before ~S: ~S ~S~%" i (file-position stream) (peek-char t stream nil nil))
            (handler-case
              (multiple-value-bind (form present eof)
                  (coalton-impl/parser/reader:maybe-read-form stream source)
                (format t "Result ~S ~S ~S ~S~%" (and form (concrete-syntax-tree:raw form)) present eof (file-position stream)))
              (error (c) (format t "ERROR ~A~%" c)))))))`, {eof: true})).output;} catch {}
  console.error(error.message, error.backtrace);
  process.exitCode = 1;
} finally {
  await Promise.all(cacheWrites.values());
  for (const file of new Set(report.compilations.map(file => '/build/cache' + file.replace(/\.(?:lisp|ct)$/u, '.wasm')))) {
    if (!await cacheWrites.get(file)) try {await saveFasl(file);} catch {}
  }
  vm.close(); await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
