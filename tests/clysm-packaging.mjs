// Check the standalone artifact's source-only bootstrap and matching adapter.
// node tests/clysm-packaging.mjs [build/clysm/site/index.html]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {gunzipSync} from 'node:zlib';
import {clysmFiles, stringList} from '../scripts/clysm-files.mjs';
import {decodeFilesystem} from '../scripts/filesystem-archive.mjs';

assert.deepEqual(stringList('; comment\n("src/package" "src/wasm")', 'sample'),
  ['src/package', 'src/wasm']);
for (const input of ['()', '("same" "same")',
  '("valid" . "tail")', '#.(error "executed")', '("valid") ("extra")',
  '("valid" #1=("cycle" . #1#))']) {
  assert.throws(() => stringList(input, 'untrusted sample'), /source names|Duplicate/);
}
const fixture = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-clysm-manifest-'));
try {
  const upstream = path.join(fixture, 'clysm/upstream');
  await fs.mkdir(path.join(upstream, 'tools'), {recursive: true});
  await fs.writeFile(path.join(upstream, 'tools/runtime-files.lisp'), '("boot")');
  for (const source of ["(defparameter *files* '(\"../escape\"))",
    "(defparameter *files* '(\"/absolute\"))",
    "(defparameter *files* '(\"src/../escape\"))",
    "(defparameter *files* '(\"src/ok\" #.(error \"executed\")))"]) {
    await fs.writeFile(path.join(upstream, 'load.lisp'), source);
    await assert.rejects(clysmFiles(fixture), /source manifest|source names|invalid path/);
  }
} finally {await fs.rm(fixture, {recursive: true, force: true});}

const page = process.argv[2] ?? 'build/clysm/site/index.html';
const html = await fs.readFile(page, 'utf8');
const match = /<script\b(?=[^>]*\bid="build-data")(?=[^>]*\btype="application\/json")[^>]*>([\s\S]*?)<\/script>/u.exec(html);
assert.ok(match, 'The page must embed its bootstrap resources.');
const payload = JSON.parse(match[1]);
assert.equal(payload.metadata.bootstrap, 'browser-miricl-source');
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
const image = gunzipSync(Buffer.from(payload.image, 'base64'));
assert.equal(sha256(image), payload.metadata.imageSha256);
const module = new WebAssembly.Module(image);
assert.equal(WebAssembly.Module.customSections(module, 'miricl.adapter.gz').length, 1,
  'The MIRICL image must own its matching adapter.');
const compressedArchive = await fs.readFile(path.join(path.dirname(page), 'filesystem.bin.gz'));
assert.equal(html.includes(compressedArchive.toString('base64')), false,
  'Keep the source filesystem outside the HTML.');
const archive = gunzipSync(compressedArchive);
assert.equal(sha256(archive), payload.metadata.sourcesSha256);
const files = decodeFilesystem(archive);
assert.ok(files.length > 0);
const names = new Set(files.map(([name]) => name));
assert.ok(names.has('/clysm-bootstrap.lisp'));
assert.ok(names.has('/clysm/compiler-files.lisp'));
assert.ok(names.has('/clysm/tools/runtime-files.lisp'));
assert.ok(names.has('/clysm/lib/web-main.lisp'));
assert.ok(names.has('/clysm/lib/live-image.lisp'));
for (const [name, bytes] of files) {
  assert.match(name, /^\/(?:clysm\/(?:[a-z0-9-]+\/)*[a-z0-9-]+\.lisp|clysm-bootstrap\.lisp)$/u,
    `Unexpected bootstrap resource ${name}`);
  assert.notDeepEqual(Array.from(bytes.subarray(0, 4)), [0, 97, 115, 109],
    `Clysm must be built on page load, not supplied as ${name}.`);
}
assert.deepEqual(new Set(payload.metadata.sourceFiles), names);
for (const [manifest, prefix] of [['/clysm/compiler-files.lisp', '/clysm/'],
  ['/clysm/tools/runtime-files.lisp', '/clysm/runtime/']]) {
  const bytes = files.find(([name]) => name === manifest)[1];
  for (const name of stringList(new TextDecoder().decode(bytes), manifest))
    assert.ok(names.has(`${prefix}${name}.lisp`), `Missing source ${prefix}${name}.lisp`);
}
assert.ok(typeof payload.worker === 'string' && payload.worker.length > 0);
assert.doesNotMatch(html, /@@[A-Z_]+@@/u);
console.log('Clysm packaging: source-only bootstrap, complete source manifests and matching embedded adapter pass.');
