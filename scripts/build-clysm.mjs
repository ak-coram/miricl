// Package a source-only, offline Clysm REPL. Clysm is compiled by MIRICL
// inside the browser at page load, never by this packaging script.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {gzipSync, gunzipSync} from 'node:zlib';
import {clysmFiles} from './clysm-files.mjs';
import {encodeFilesystem} from './filesystem-archive.mjs';

const root = path.resolve(import.meta.dirname, '..');
const [imagePath = 'miricl.wasm', output = 'build/clysm/site', ...options] = process.argv.slice(2);
if (options.length) throw new Error('Usage: node scripts/build-clysm.mjs [MIRICL.wasm] [OUTPUT-DIRECTORY]');
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
const moduleURL = source => 'data:text/javascript;base64,' + Buffer.from(source).toString('base64');
const replaceOnce = (source, marker, replacement) => {
  if (source.split(marker).length !== 2) throw new Error(`Expected exactly one ${marker} marker.`);
  return source.replace(marker, () => replacement);
};
const [image, inventory, template, workerSource, loader, filesystem, jsffi, common, host, license] = await Promise.all([
  fs.readFile(imagePath), clysmFiles(root),
  fs.readFile(path.join(root, 'clysm/index.html'), 'utf8'),
  fs.readFile(path.join(root, 'clysm/worker.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'scripts/miricl-loader.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'scripts/filesystem-archive.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'clysm/upstream/host/jsffi.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'clysm/upstream/host/common.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'clysm/upstream/host/web/clysm.mjs'), 'utf8'),
  fs.readFile(path.join(root, 'LICENSE'), 'utf8'),
]);
const module = new WebAssembly.Module(image);
const adapters = WebAssembly.Module.customSections(module, 'miricl.adapter.gz');
if (adapters.length !== 1) throw new Error('Expected a MIRICL image carrying its matching compressed JavaScript adapter.');
const revision = execFileSync('git', ['-C', path.join(root, 'clysm/upstream'), 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const bundledCommon = replaceOnce(common, "from './jsffi.mjs'", `from '${moduleURL(jsffi)}'`);
const bundledHost = replaceOnce(host, "from '../common.mjs'", `from '${moduleURL(bundledCommon)}'`);
// Refuse newly introduced static dependencies rather than publishing a page
// that only works beside the source checkout. Dynamic JS interop is intentional.
for (const [name, text] of [['jsffi', jsffi], ['common', bundledCommon], ['browser host', bundledHost]]) {
  for (const match of text.matchAll(/^\s*import\s+(?:[\s\S]*?\sfrom\s*)?['"]([^'"]+)['"]\s*;/gmu)) {
    if (!match[1].startsWith('data:text/javascript;base64,'))
      throw new Error(`Unbundled Clysm ${name} import: ${match[1]}`);
  }
}
let worker = replaceOnce(workerSource, '@@MIRICL_LOADER_URL@@', moduleURL(loader));
worker = replaceOnce(worker, '@@FILESYSTEM_URL@@', moduleURL(filesystem));
worker = replaceOnce(worker, '@@CLYSM_HOST_URL@@', moduleURL(bundledHost));
const archive = encodeFilesystem(inventory.files);
const compressedImage = gzipSync(image, {level: 9});
const compressedArchive = gzipSync(archive, {level: 9});
const metadata = {
  upstream: 'https://github.com/cxxxr/clysm', revision,
  bootstrap: 'browser-miricl-source',
  imageSha256: sha256(image), imageBytes: image.length, imageGzipBytes: compressedImage.length,
  adapterBytes: gunzipSync(new Uint8Array(adapters[0])).length,
  adapterGzipBytes: adapters[0].byteLength,
  sourcesSha256: sha256(archive), sourceBytes: archive.length, sourceGzipBytes: compressedArchive.length,
  sourceGzipSha256: sha256(compressedArchive),
  sourceFiles: inventory.files.map(([name]) => name),
  hostJavaScriptBytes: Buffer.byteLength(jsffi + common + host),
};
const payload = JSON.stringify({metadata, image: compressedImage.toString('base64'),
  filesystem: 'filesystem.bin.gz', worker: Buffer.from(worker).toString('base64'),
  notices: {'MIRICL license': license}}).replaceAll('<', '\\u003c');
const html = replaceOnce(template, '@@CLYSM_PAYLOAD@@', payload);
if (/@@[A-Z_]+@@/u.test(html)) throw new Error('Unresolved Clysm page marker.');
await fs.mkdir(output, {recursive: true});
await fs.writeFile(path.join(output, 'index.html'), html);
await fs.writeFile(path.join(output, 'filesystem.bin.gz'), compressedArchive);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({...metadata,
  filesystem: 'filesystem.bin.gz', pageBytes: Buffer.byteLength(html),
  pageGzipBytes: gzipSync(html, {level: 9}).length}, null, 2) + '\n');
console.log(`Packaged Clysm browser bootstrap: ${path.join(output, 'index.html')} (${Buffer.byteLength(html)} bytes).`);
console.log(`MIRICL image ${image.length} raw / ${compressedImage.length} gzip bytes; sources ${archive.length} raw / ${compressedArchive.length} gzip bytes; adapter ${adapters[0].byteLength} gzip bytes (included in image).`);
