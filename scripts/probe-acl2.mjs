// Bootstrap the pinned ACL2 sources inside MIRICL, bounded by a worker timeout.
// No native ACL2 image or host Lisp participates in this probe.
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {gzipSync, brotliCompressSync} from 'node:zlib';
import {Worker, isMainThread, parentPort, workerData} from 'node:worker_threads';
import {Miricl} from './miricl.mjs';
import {portAcl2Source, acl2Adaptations} from '../acl2/host.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const upstream = path.join(root, 'vendor/acl2');

if (!isMainThread) {
  let vm, stage, definition;
  const port = workerData.mode !== 'upstream';
  const stages = [];
  const files = [];
  const units = [];
  const evaluate = async (name, source) => {
    stage = name;
    parentPort.postMessage({stage});
    const start = performance.now();
    const result = await vm.evaluateAsync(source, {eof: true});
    stages.push({name, milliseconds: Math.round(performance.now() - start), output: result.output});
    parentPort.postMessage({completed: stages.at(-1)});
    return result;
  };
  try {
    vm = await Miricl.instantiate(fs.readFileSync(workerData.image), {imports: {acl2Probe: {
      file(kind, name) {
        const file = {kind, name}; files.push(file);
        parentPort.postMessage({file});
      },
      definition(name) {
        definition = name;
        parentPort.postMessage({definition});
      },
    }}});
    // The system is self-contained in the repository root. Community Books
    // are a separate distribution problem and are deliberately not mounted.
    let sourceBytes = 0;
    for (const entry of fs.readdirSync(upstream, {withFileTypes: true})) {
      if (!entry.isFile() || entry.name === '.git') continue;
      const bytes = fs.readFileSync(path.join(upstream, entry.name));
      vm.mountFile(`/acl2/${entry.name}`, port
        ? portAcl2Source(entry.name, bytes, {revision: workerData.revision}) : bytes);
      sourceBytes += bytes.length;
    }
    if (port)
      vm.mountFile('/acl2/miricl-host.lisp', fs.readFileSync(path.join(root, 'acl2/miricl-host.lisp')));
    await evaluate('runtime', '(list (lisp-implementation-type) (lisp-implementation-version) *features* most-positive-fixnum)');
    await evaluate('file diagnostics', `
      (js:define-import cl-user::acl2-probe-file ("acl2Probe" "file") (:string :string) :void)
      (js:define-import cl-user::acl2-probe-definition ("acl2Probe" "definition") (:string) :void)
      (defparameter cl-user::*acl2-probe-load* (symbol-function 'load))
      (defparameter cl-user::*acl2-probe-compile-file* (symbol-function 'compile-file))
      (defparameter cl-user::*acl2-probe-compile-native* (symbol-function 'miricl-compiler:compile-native))
      (defparameter cl-user::*acl2-probe-form* nil)
      (setf (symbol-function 'miricl-compiler:compile-native)
        (lambda (form &rest arguments)
          (setq cl-user::*acl2-probe-form* form)
          (cl-user::acl2-probe-definition
            (format nil "~S" (or (getf arguments :name) :anonymous)))
          (apply cl-user::*acl2-probe-compile-native* form arguments)))
      (setf (symbol-function 'load)
        (lambda (file &rest arguments)
          (cl-user::acl2-probe-file "load" (if (streamp file) "<stream>" (namestring file)))
          (apply cl-user::*acl2-probe-load* file arguments)))
      (setf (symbol-function 'compile-file)
        (lambda (file &rest arguments)
          (cl-user::acl2-probe-file "compile" (namestring file))
          (apply cl-user::*acl2-probe-compile-file* file arguments)))
      (values)`);
    await evaluate('loader', `
      (setq *default-pathname-defaults* #P"/acl2/")
      (pushnew :cltl2 *features*)
      ${port ? '(pushnew :unix *features*)' : ''}
      (load "init.lisp")`);
    await evaluate('host suitability', '(acl2::check-suitability-for-acl2)');
    await evaluate('compile system', '(acl2::compile-acl2)');
    const loadStart = files.length;
    await evaluate('load system', '(acl2::load-acl2)');
    if (workerData.mode === 'fasl') {
      // ACL2 normally loads each source before compiling it: its macros call
      // functions defined in the same source. The raw system is now loaded.
      // COMPILE-FILE writes portable units, without native Wasm code.
      const start = files.length;
      const loads = files.slice(loadStart).filter(file => file.kind === 'load')
        .map(file => path.posix.basename(file.name));
      await evaluate('portable core sources', `
        (dolist (name acl2::*acl2-files*)
          (cl-user::acl2-probe-file "portable-source" (namestring (make-pathname :name name :type "lisp"))))
        (values)`);
      const sources = files.slice(start).filter(file => file.kind === 'portable-source').map(file => file.name);
      const compiled = new Map();
      const unitDirectory = workerData.bundle.replace(/\.wasm$/, '') + '.units';
      fs.mkdirSync(unitDirectory, {recursive: true});
      for (const source of sources) {
        const name = path.posix.basename(source, '.lisp') + '.wasm';
        const target = `/acl2/${name}`;
        await evaluate(`compile portable ${source}`, `
          (let ((*readtable* acl2::*acl2-readtable*))
            (multiple-value-bind (file warnings failure)
                (compile-file ${JSON.stringify(source)} :output-file ${JSON.stringify(target)})
              (declare (ignore file warnings))
              (when failure (error "ACL2 portable compilation failed."))))
          (values)`);
        const bytes = Buffer.from(await vm.readFile(target));
        const file = path.join(unitDirectory, name);
        fs.writeFileSync(file, bytes);
        const unit = {source, file, bytes: bytes.length};
        units.push(unit); parentPort.postMessage({unit});
        compiled.set(path.posix.basename(source), bytes);
      }
      // LOAD-ACL2 defers DEFPKGS until after its other system files. Preserve
      // the actual load order rather than assuming compilation order matches.
      const order = loads.filter(name => compiled.has(name));
      if (new Set(order).size !== sources.length || order.length !== sources.length)
        throw new Error('ACL2 compilation units do not match its system load order.');
      const parts = order.map(name => compiled.get(name));
      const bundle = Buffer.concat([Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]),
        ...parts.map(part => Buffer.from(part).subarray(8))]);
      const module = new WebAssembly.Module(bundle);
      if (!parts.length || WebAssembly.Module.imports(module).length || WebAssembly.Module.exports(module).length
          || WebAssembly.Module.customSections(module, 'miricl.fasl').length !== parts.length)
        throw new Error('Invalid portable ACL2 FASL bundle.');
      fs.mkdirSync(path.dirname(workerData.bundle), {recursive: true});
      fs.writeFileSync(workerData.bundle, bundle);
      parentPort.postMessage({report: {passed: true, stages, sourceBytes, files, definition,
        validation: 'portable-unit-structure-only', initialized: false, units,
        bundle: {file: workerData.bundle, units: order, rawBytes: bundle.length,
          gzipBytes: gzipSync(bundle, {level: 9}).length,
          brotliBytes: brotliCompressSync(bundle).length,
          sha256: createHash('sha256').update(bundle).digest('hex')}}});
    } else {
      await evaluate('initialize logical world', `
        (unless (acl2::initialize-acl2) (error "ACL2 world initialization did not complete."))
        (with-open-file (stream acl2::*acl2-status-file*)
          (unless (eq (read stream) :initialized) (error "ACL2 did not mark its world initialized.")))`);
      // LD must admit actual logic events; raw Lisp arithmetic is insufficient.
      await evaluate('definitions and proof', `
        (acl2::ld '((acl2::logic)
                    (acl2::defun acl2::miricl-double (acl2::x)
                      (declare (acl2::xargs :guard (acl2::rationalp acl2::x)))
                      (+ acl2::x acl2::x))
                    (acl2::defthm acl2::miricl-double-correct
                      (acl2::implies (acl2::rationalp acl2::x)
                               (equal (acl2::miricl-double acl2::x) (* 2 acl2::x)))))
                  :ld-error-action :error :ld-skip-proofsp nil)
        (unless (equal (acl2::miricl-double 21) 42) (error "ACL2 evaluation failed."))
        (unless (acl2::getpropc 'acl2::miricl-double-correct 'acl2::theorem nil
                                (acl2::w acl2::*the-live-state*))
          (error "ACL2 did not admit the theorem."))
        (values)`);
      parentPort.postMessage({report: {passed: true, stages, sourceBytes, files, definition}});
    }
  } catch (error) {
    // Keep the compiler input when a host stack trap bypasses Lisp handlers.
    // Diagnostic printing is best effort: the original failure stays primary.
    let compilerInput;
    try {
      // Interpret the diagnostic so automatic compilation cannot replace the
      // saved failing input with this printer's own lambda expression.
      compilerInput = vm?.evaluate(`(locally (declare (optimize (debug 3)))
        (let ((*print-readably* t) (*print-circle* t) (*print-pretty* nil))
          (write cl-user::*acl2-probe-form*) (values)))`, {eof: true}).output;
    } catch {}
    parentPort.postMessage({report: {passed: false, stage, stages, files, definition, units,
      compilerInput,
      failure: {message: error.message, output: error.output, backtrace: error.backtrace}}});
  } finally {vm?.close();}
} else if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const image = path.resolve(process.argv[2] ?? path.join(root, 'miricl-precompiled.wasm'));
  const output = path.resolve(process.argv[3] ?? path.join(root, 'build/acl2/probe.json'));
  const mode = process.argv[4] ?? 'upstream';
  if (!['upstream', 'port', 'fasl'].includes(mode)) throw new Error(`Unknown ACL2 probe mode: ${mode}`);
  const bundle = path.resolve(process.argv[5] ?? path.join(path.dirname(output), 'acl2.wasm'));
  const revision = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
  const imageSha256 = createHash('sha256').update(fs.readFileSync(image)).digest('hex');
  const timeout = Number(process.env.MIRICL_ACL2_TIMEOUT_MS ?? (mode === 'upstream' ? 300000 : 3600000));
  if (!Number.isFinite(timeout) || timeout <= 0) throw new Error('Expected a positive ACL2 timeout.');
  const worker = new Worker(new URL(import.meta.url), {workerData: {image, mode, revision, bundle}});
  const node = {version: process.version, v8: process.versions.v8, execArgv: process.execArgv,
    workerResourceLimits: worker.resourceLimits};
  console.log(`ACL2 Node configuration: ${JSON.stringify(node)}`);
  let stage, definition;
  const stages = [];
  const files = [];
  const units = [];
  fs.mkdirSync(path.dirname(output), {recursive: true});
  const persist = () => fs.writeFileSync(output + '.progress.json', JSON.stringify({stage, definition,
    stages, files, units, revision, image, imageSha256, mode, node}, null, 2) + '\n');
  const timer = setTimeout(() => {void worker.terminate();}, timeout);
  const heartbeat = setInterval(() => {
    persist(); console.log(`ACL2: still working on ${stage}${definition ? ` (${definition})` : ''}`);
  }, 30000);
  const report = await new Promise(resolve => {
    worker.on('message', message => {
      if (message.report) resolve(message.report);
      else if (message.completed) {stages.push(message.completed); persist();}
      else if (message.unit) {units.push(message.unit); persist(); console.log(`ACL2 FASL: ${message.unit.source}: ${message.unit.bytes} bytes`);}
      else if (message.definition) definition = message.definition;
      else if (message.file) {files.push(message.file); console.log(`ACL2 ${message.file.kind}: ${message.file.name}`);}
      else {stage = message.stage; console.log(`ACL2: ${stage}`);}
    });
    worker.on('error', error => resolve({passed: false, stage, stages, files, definition, units, failure: {message: error.message}}));
    worker.on('exit', code => resolve({passed: false, stage, stages, files, definition, units,
      failure: {message: `Worker exited (${code}); time limit ${timeout} ms.`}}));
  }).finally(() => {clearTimeout(timer); clearInterval(heartbeat);});
  fs.mkdirSync(path.dirname(output), {recursive: true});
  const adaptations = mode === 'upstream' ? [] : acl2Adaptations;
  fs.writeFileSync(output, JSON.stringify({...report, revision, image, imageSha256, mode, adaptations, node}, null, 2) + '\n');
  console.log(`${report.passed ? 'PASS' : 'FAIL'} ACL2 ${mode === 'fasl' ? 'portable FASL build' : 'bootstrap'}. Report: ${output}`);
  if (report.bundle) console.log(JSON.stringify(report.bundle));
  if (report.failure) console.error(report.failure);
  process.exitCode = report.passed ? 0 : 1;
}
