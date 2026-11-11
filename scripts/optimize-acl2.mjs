// Compile ACL2's complete dynamic interface to native Wasm.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';
import {execFileSync} from 'node:child_process';
import {startWasmProfile} from './wasm-profile.mjs';
const [imagePath = 'miricl-precompiled.wasm', input = 'build/acl2/acl2.wasm',
  output = 'build/acl2/acl2-native.wasm'] = process.argv.slice(2);
const upstream = path.resolve('vendor/acl2');
const revision = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const image = await fs.readFile(imagePath), portable = await fs.readFile(input);
let stage = 'runtime', count = 0;
const started = performance.now();
const heartbeat = setInterval(() => console.log(`ACL2 native: ${stage} (${count} initializers, ${Math.round((performance.now() - started) / 1000)} s)`), 30000);
const vm = await Miricl.instantiate(image, {imports: {acl2Build: {
  progress(file, compiled) {count = compiled; if (count % 500 === 0) console.log(`ACL2 native: ${file}, ${count} initializers`);},
}}});
const stages = [];
async function evaluate(name, source) {
  stage = name; console.log(`ACL2 native: ${stage}`); const start = performance.now();
  const profile = process.env.MIRICL_ACL2_PROFILE_DIRECTORY
    && new RegExp(process.env.MIRICL_ACL2_PROFILE_PHASE ?? 'compile native code').test(name)
    ? await startWasmProfile(vm, image,
      path.join(process.env.MIRICL_ACL2_PROFILE_DIRECTORY, name.replaceAll(/[^a-z0-9-]+/gi, '-'))) : null;
  let result, milliseconds;
  try {result = await vm.evaluateAsync(source, {eof: true}); milliseconds = Math.round(performance.now() - start);}
  finally {if (profile) await profile();}
  stages.push({name, milliseconds});
  return result;
}
try {
  for (const entry of await fs.readdir(upstream, {withFileTypes: true})) {
    if (!entry.isFile() || entry.name === '.git') continue;
    vm.mountFile(`/acl2/${entry.name}`, portAcl2Source(entry.name,
      await fs.readFile(path.join(upstream, entry.name)), {revision}));
  }
  vm.mountFile('/acl2/miricl-host.lisp', await fs.readFile('acl2/miricl-host.lisp'));
  vm.mountFile('/acl2-input.wasm', portable);
  await evaluate('loader', `
    (setq *default-pathname-defaults* #P"/acl2/")
    (pushnew :cltl2 *features*) (pushnew :unix *features*)
    (load "init.lisp") (acl2::proclaim-optimize) (values)`);
  await evaluate('compact dynamic environment', `
    (progn
      (miricl-compiler:shake-file "/acl2-input.wasm" "/acl2-output.wasm"
        :strip nil :report "/acl2-retention.txt") (values))`);
  await evaluate('compile native code', `
    (js:define-import cl-user::acl2-native-progress ("acl2Build" "progress") (:string :i32) :void)
    (miricl-compiler:compile-native-bundle "/acl2-output.wasm" "/acl2-output.wasm"
      :load t :progress (lambda (file count)
        (cl-user::acl2-native-progress (if file (princ-to-string file) "<opaque unit>") count))) (values)`);
  const native = await vm.readFile('/acl2-output.wasm');
  new WebAssembly.Module(native);
  await fs.mkdir(path.dirname(output), {recursive: true}); await fs.writeFile(output, native);
  await fs.writeFile(path.join(path.dirname(output), 'runtime.wasm'), image);
  await fs.writeFile(path.join(path.dirname(output), 'retention.txt'), await vm.readFile('/acl2-retention.txt'));
  const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
  await fs.writeFile(path.join(path.dirname(output), 'optimization.json'), JSON.stringify({
    revision, imageSha256: sha256(image), coreSha256: sha256(native), backend: 'native-wasm', runtimeFile: 'runtime.wasm',
    inputBytes: portable.length, outputBytes: native.length, stages, initializers: count,
    initializedWorld: false, retainedInterface: 'complete dynamic ACL2 prover',
    optimizer: 'miricl-compiler:shake-file + compile-native-bundle', strip: false,
  }, null, 2) + '\n');
  console.log(`Native ACL2: ${native.length} bytes (${output}).`);
} catch (error) {console.error(`ACL2 native failed during ${stage}:`, error.message, error.backtrace); throw error;}
finally {clearInterval(heartbeat); vm.close();}
