// Validate the native prover itself, including logical-world initialization
// and proof checking. This deliberately uses no frontend protocol fixtures.
import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {createAcl2} from '../scripts/acl2.mjs';
import {portAcl2Source} from '../acl2/host.mjs';
const [imagePath = 'miricl-precompiled.wasm', corePath = 'build/acl2/acl2-native.wasm',
  reportPath = 'build/acl2/native-tests.json', checkpointPath] = process.argv.slice(2);
const report = {passed: false, stages: []};
let session, checkpoint, stage = 'inputs';
const started = performance.now();
const heartbeat = setInterval(() => console.log(`ACL2 native test: ${stage} (${Math.round((performance.now() - started) / 1000)} s)`), 30000);
try {
  const [image, core, optimization] = await Promise.all([
    fs.readFile(imagePath), fs.readFile(corePath),
    fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8').then(JSON.parse)]);
  const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
  assert.equal(optimization.backend, 'native-wasm');
  assert.equal(sha256(core), optimization.coreSha256);
  assert.equal(sha256(image), optimization.imageSha256);
  report.revision = optimization.revision;
  report.imageSha256 = sha256(image); report.coreSha256 = sha256(core);
  const files = [];
  for (const entry of await fs.readdir('vendor/acl2', {withFileTypes: true})) {
    if (!entry.isFile() || entry.name === '.git') continue;
    files.push([`/acl2/${entry.name}`, portAcl2Source(entry.name,
      await fs.readFile(path.join('vendor/acl2', entry.name)), {revision: optimization.revision})]);
  }
  files.push(['/acl2/miricl-host.lisp', await fs.readFile('acl2/miricl-host.lisp')]);
  session = await createAcl2(image, core, files, {progress(name) {
    stage = name;
    report.stages.push({name, milliseconds: Math.round(performance.now() - started)});
    console.log(`ACL2 native test: ${name}`);
  }});
  report.startupMilliseconds = Math.round(performance.now() - started);
  if (checkpointPath) {
    stage = 'save initialized world';
    checkpoint = session.vm.saveImage();
    session.close(); session = null;
    const restoring = performance.now();
    stage = 'restore initialized world';
    session = await createAcl2(checkpoint, null, []);
    report.checkpoint = {sha256: sha256(checkpoint), bytes: checkpoint.length,
      restoreMilliseconds: Math.round(performance.now() - restoring)};
  }
  stage = 'native entry';
  assert.equal((await session.vm.evaluateAsync(`
    (plusp (miricl-compiler::compiler-primitive 26 #'acl2::alistp))`, {eof: true})).output, 'T\n');
  stage = 'arithmetic';
  const arithmetic = await session.evaluate('(+ 20 22)');
  assert.equal(arithmetic.failed, false); assert.match(arithmetic.output, /\b42\b/u);
  stage = 'definition and proof';
  const proof = await session.evaluate(`
    (defun miricl-double (x)
      (declare (xargs :guard (rationalp x))) (+ x x))
    (defthm miricl-double-correct
      (implies (rationalp x) (equal (miricl-double x) (* 2 x))))`);
  assert.equal(proof.failed, false, proof.output);
  stage = 'false theorem rejection';
  const rejected = await session.evaluate('(defthm miricl-double-false (equal (miricl-double 1) 3))');
  assert.equal(rejected.failed, true, rejected.output);
  stage = 'recovery';
  const recovered = await session.evaluate('(miricl-double 21)');
  assert.equal(recovered.failed, false); assert.match(recovered.output, /\b42\b/u);
  if (checkpointPath) {
    await fs.mkdir(path.dirname(checkpointPath), {recursive: true});
    await fs.writeFile(checkpointPath, checkpoint);
    console.log(`ACL2 restored its initialized world in ${report.checkpoint.restoreMilliseconds} ms.`);
  }
  report.passed = true;
  console.log(`ACL2 native prover: startup ${report.startupMilliseconds} ms; arithmetic, theorem admission, false theorem rejection and recovery pass.`);
} catch (error) {
  report.stage = stage;
  report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};
  throw error;
} finally {
  clearInterval(heartbeat); session?.close();
  await fs.mkdir(path.dirname(reportPath), {recursive: true});
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
}
