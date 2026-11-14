// Exercise the real native payload on the default stack, before the expensive
// logical-world bootstrap. Frontend protocol fixtures do not cover this load.
import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';
const [imagePath = 'miricl.wasm', corePath = 'build/acl2/acl2-native.wasm',
  reportPath = 'build/acl2/native-load.json'] = process.argv.slice(2);
const report = {passed: false, validation: 'native-core-load', initializedWorld: false};
let vm;
const start = performance.now();
try {
  const [image, core, metadata] = await Promise.all([
    fs.readFile(imagePath), fs.readFile(corePath),
    fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8').then(JSON.parse)]);
  const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
  assert.equal(metadata.backend, 'native-wasm');
  assert.equal(sha256(image), metadata.imageSha256);
  assert.equal(sha256(core), metadata.coreSha256);
  report.imageSha256 = metadata.imageSha256; report.coreSha256 = metadata.coreSha256;
  vm = await Miricl.instantiate(image);
  for (const entry of await fs.readdir('vendor/acl2', {withFileTypes: true})) {
    if (entry.isFile() && entry.name !== '.git')
      vm.mountFile(`/acl2/${entry.name}`, portAcl2Source(entry.name,
        await fs.readFile(path.join('vendor/acl2', entry.name)), {revision: metadata.revision}));
  }
  vm.mountFile('/acl2/miricl-host.lisp', await fs.readFile('acl2/miricl-host.lisp'));
  vm.mountFile('/acl2.core', core);
  await vm.evaluateAsync(`
    (setq *default-pathname-defaults* #P"/acl2/")
    (pushnew :cltl2 *features*) (pushnew :unix *features*)
    (load "init.lisp") (acl2::proclaim-optimize)
    (let ((*readtable* acl2::*acl2-readtable*)) (load "/acl2.core"))
    (values)`, {eof: true});
  report.loadMilliseconds = Math.round(performance.now() - start);
  assert.equal((await vm.evaluateAsync(`
    (and (every (lambda (fn) (plusp (miricl-compiler::compiler-primitive 26 fn)))
                (list #'acl2::alistp #'acl2::remove-adjacent-duplicates
                      #'acl2::remove-adjacent-duplicates-eq))
         (equal (acl2::remove-adjacent-duplicates-eq '(a a b b a . tail)) '(a b a))
         (let* ((input (loop for i below 100000 collect (floor i 2)))
                (result (acl2::remove-adjacent-duplicates-eq input)))
           (and (= (length result) 50000) (= (car (last result)) 49999))))`, {eof: true})).output, 'T\n');
  report.passed = true;
  console.log(`ACL2 native core: default-stack load ${report.loadMilliseconds} ms; native entries and 100,000-element package-import deduplication pass. Logical-world startup is tested separately.`);
} catch (error) {
  report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};
  throw error;
} finally {
  vm?.close(); await fs.mkdir(path.dirname(reportPath), {recursive: true});
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
}
