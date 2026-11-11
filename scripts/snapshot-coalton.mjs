// Prepare an initialized image and validate it in a fresh VM before packaging.
// node scripts/snapshot-coalton.mjs RUNTIME CORE OUTPUT.wasm
import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {gzipSync} from 'node:zlib';
import {createCoalton} from './coalton.mjs';

const [runtimePath = 'miricl-precompiled.wasm', corePath = 'build/coalton/coalton.wasm',
  output = 'build/coalton/coalton-image.wasm'] = process.argv.slice(2);
const [runtime, core, optimization] = await Promise.all([
  fs.readFile(runtimePath), fs.readFile(corePath),
  fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8').then(JSON.parse),
]);
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
assert.equal(optimization.backend, 'native-wasm');
assert.equal(optimization.scope, 'full');
assert.equal(optimization.imageSha256, hash(runtime));
assert.equal(optimization.coreSha256, hash(core));
await fs.mkdir(path.dirname(output), {recursive: true});
const report = {passed: false, revision: optimization.revision,
  runtimeSha256: hash(runtime), coreSha256: hash(core), node: process.version,
  nativeDownloadBytes: gzipSync(runtime, {level: 9}).length + gzipSync(core, {level: 9}).length};
let session;
try {
  let started = performance.now();
  session = await createCoalton(runtime, core, {progress: message => console.log(message)});
  report.nativeStartupMilliseconds = Math.round(performance.now() - started);
  started = performance.now();
  const snapshot = session.saveImage();
  report.saveMilliseconds = Math.round(performance.now() - started);
  session.close(); session = null;
  await fs.writeFile(output, snapshot);
  report.snapshotSha256 = hash(snapshot); report.snapshotBytes = snapshot.length;
  report.snapshotDownloadBytes = gzipSync(snapshot, {level: 9}).length;
  console.log(`Saved Coalton image: ${snapshot.length} bytes; restoring in a fresh VM.`);
  started = performance.now();
  session = await createCoalton(snapshot, null, {progress: message => console.log(message)});
  report.restoreMilliseconds = Math.round(performance.now() - started);
  assert.equal((await session.evaluate('(+ 20 22)', {mode: 'expression'})).output.trim(), '42');
  await session.evaluate('(define (snapshot-square x) (* x x))', {mode: 'definitions'});
  assert.equal((await session.evaluate('(snapshot-square 12)', {mode: 'expression'})).output.trim(), '144');
  await assert.rejects(() => session.evaluate('(+ 1 "text")', {mode: 'expression'}));
  assert.equal((await session.evaluate('(snapshot-square 7)', {mode: 'expression'})).output.trim(), '49');
  report.passed = true;
  console.log(`Coalton snapshot: native startup ${report.nativeStartupMilliseconds} ms; restore ${report.restoreMilliseconds} ms; compile, evaluate and error recovery passed.`);
} catch (error) {
  report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};
  throw error;
} finally {
  session?.close();
  await fs.writeFile(output + '.json', JSON.stringify(report, null, 2) + '\n');
}
