// Retain the complete interactive compiler, macros and standard library.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {Miricl} from './miricl.mjs';
const [image = 'miricl-precompiled.wasm', input = 'build/coalton/coalton-portable.wasm',
  output = 'build/coalton/coalton.wasm'] = process.argv.slice(2);
const scope = process.env.MIRICL_COALTON_SCOPE ?? 'full';
if (!['full', 'compiler'].includes(scope)) throw new Error('Expected full or compiler scope.');
const prefixPath = process.env.MIRICL_COALTON_BOOTSTRAP;
const prefix = prefixPath ? await fs.readFile(prefixPath) : null;
let bytes = await fs.readFile(input);
const asdf = await fs.readFile('miricl-asdf.wasm');
// Older probe outputs recorded LOAD calls only after ASDF was loaded. Keep
// those build inputs usable while ensuring the standalone core includes ASDF.
if (!prefix && !bytes.subarray(8, asdf.length).equals(asdf.subarray(8)))
  bytes = Buffer.concat([asdf, bytes.subarray(8)]);
const runtime = await fs.readFile(image);
const revision = execFileSync('git', ['-C', 'vendor/coalton', 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
if (prefix) {
  const metadata = JSON.parse(await fs.readFile(path.join(path.dirname(prefixPath), 'optimization.json'), 'utf8'));
  if (metadata.imageSha256 !== createHash('sha256').update(runtime).digest('hex')
      || metadata.coreSha256 !== createHash('sha256').update(prefix).digest('hex')
      || metadata.backend !== 'native-wasm' || metadata.scope !== 'compiler' || metadata.revision !== revision)
    throw new Error('Expected a native compiler bootstrap for this runtime and source revision.');
  if (!bytes.subarray(8, prefix.length).equals(prefix.subarray(8)))
    throw new Error('The library build must begin with the supplied native compiler bootstrap.');
  bytes = Buffer.concat([bytes.subarray(0, 8), bytes.subarray(prefix.length)]);
}
let count = 0, currentFile = '', phase = 'initialize runtime';
const timings = {};
const started = performance.now();
const vm = await Miricl.instantiate(runtime, {imports: {
  coaltonBuild: {progress(file, compiled) {
    count = compiled;
    if (file !== currentFile || count % 500 === 0) console.log(`Coalton native: ${file}, ${count} initializers`);
    currentFile = file;
  }}}});
let phaseStart = started;
const heartbeat = setInterval(() => {
  const detail = phase === 'native bundle' ? `, ${count} initializers` : '';
  console.log(`Coalton: still working on ${phase}${detail} (${Math.round((performance.now() - phaseStart) / 1000)} s)`);
}, 30000);
async function evaluatePhase(name, source) {
  phase = name;
  phaseStart = performance.now();
  await vm.evaluateAsync(source, {eof: true});
  const milliseconds = Math.round(performance.now() - phaseStart);
  timings[name] = milliseconds;
  console.log(`Coalton ${name}: ${milliseconds} ms`);
}
try {
  if (process.env.MIRICL_COALTON_DIAGNOSTICS === '1') await vm.evaluateAsync(`
    (defmethod make-load-form :around ((object t) &optional environment)
      (declare (ignore environment))
      (handler-bind ((error (lambda (condition)
                             (declare (ignore condition))
                             (let ((*print-length* 6) (*print-level* 3) (*print-circle* t) (*print-readably* nil))
                               (format t "~&Cannot save object of type ~S: ~S~%" (type-of object) object)))))
        (call-next-method)))
    (defmethod slot-missing :around ((class t) (object t) (slot-name t) (operation t) &optional new-value)
      (declare (ignore new-value))
      (let ((*print-length* 6) (*print-level* 3) (*print-circle* t) (*print-readably* nil))
        (format t "~&Missing slot ~S in ~S (~S), operation ~S, class ~S~%"
          slot-name object (type-of object) operation (class-name class)))
      (call-next-method)) (values)`, {eof: true});
  if (prefix) {
    vm.mountFile('/coalton-bootstrap.wasm', prefix);
    await evaluatePhase('load bootstrap', '(declaim (declaration values)) (load "/coalton-bootstrap.wasm") (cl:values)');
  }
  const inputBytes = bytes.length;
  vm.mountFile('/coalton-input.wasm', bytes);
  bytes = null;
  const start = performance.now();
  await evaluatePhase('compaction', `
    (declaim (declaration values))
    (progn
      (miricl-compiler:shake-file "/coalton-input.wasm" "/coalton-output.wasm"
        :strip nil :report "/coalton-retention.txt") (cl:values))`);
  await evaluatePhase('release portable input', '(delete-file "/coalton-input.wasm") (cl:values)');
  if (scope === 'compiler')
    await evaluatePhase('load compacted compiler', '(load "/coalton-output.wasm") (cl:values)');
  await evaluatePhase('native bundle', `
    (js:define-import coalton-native-progress ("coaltonBuild" "progress") (:string :i32) :void)
    (handler-bind ((error (lambda (condition)
                           (let ((*print-length* 10) (*print-level* 4) (*print-circle* t))
                             (format t "~&Native compilation failed: ~A~%" condition)))))
      (miricl-compiler:compile-native-bundle "/coalton-output.wasm" "/coalton-output.wasm"
        :load ${scope === 'compiler' ? 'nil' : 't'} :progress (lambda (file count)
          (coalton-native-progress (if file (princ-to-string file) "<opaque unit>") count))))
    (cl:values)`);
  phase = 'write native bundle';
  phaseStart = performance.now();
  let native = await vm.readFile('/coalton-output.wasm');
  if (prefix) native = Buffer.concat([prefix, native.subarray(8)]);
  new WebAssembly.Module(native);
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, native);
  await fs.writeFile(path.join(path.dirname(output), 'runtime.wasm'), runtime);
  await fs.writeFile(path.join(path.dirname(output), 'retention.txt'), await vm.readFile('/coalton-retention.txt'));
  await fs.writeFile(path.join(path.dirname(output), 'optimization.json'), JSON.stringify({
    revision, imageSha256: createHash('sha256').update(runtime).digest('hex'),
    coreSha256: createHash('sha256').update(native).digest('hex'), runtimeFile: 'runtime.wasm',
    inputBytes, outputBytes: native.length, milliseconds: Math.round(performance.now() - start),
    timings,
    scope, nativeCompilerBootstrap: prefixPath ?? null,
    retainedInterface: scope === 'compiler' ? 'Coalton compiler and dependencies' : 'complete interactive Coalton compiler and standard library',
    optimizer: 'miricl-compiler:shake-file + compile-native-bundle', backend: 'native-wasm', strip: false,
  }, null, 2) + '\n');
  console.log(`Native Coalton: ${inputBytes} → ${native.length} bytes (${output}).`);
} catch (error) {
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(path.join(path.dirname(output), 'optimization-failure.json'), JSON.stringify({
    phase, timings, file: currentFile, initializers: count, milliseconds: Math.round(performance.now() - started),
    message: error.message, output: error.output, backtrace: error.backtrace,
  }, null, 2) + '\n');
  throw error;
} finally {clearInterval(heartbeat); vm.close();}
