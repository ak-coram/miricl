// Package the ACL2 browser frontend, runtime, native core and guest sources.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {gzipSync} from 'node:zlib';
import {portAcl2Source, acl2Adaptations} from '../acl2/host.mjs';
import {encodeFilesystem} from './filesystem-archive.mjs';
import {versionAssets} from './version-assets.mjs';

const root = path.resolve(import.meta.dirname, '..');
const [imagePath = 'miricl-precompiled.wasm', corePath = 'build/acl2/acl2-native.wasm',
  output = 'build/acl2/site', ...options] = process.argv.slice(2);
const preview = options.length === 1 && options[0] === '--frontend-only';
const initialized = options.length === 3 && options[0] === '--initialized-image';
if (options.length && !preview && !initialized) throw new Error('Expected --frontend-only or --initialized-image IMAGE TEST-REPORT.');
const booksDirectory = path.join(path.dirname(path.resolve(output)), 'books');
const books = JSON.parse(await fs.readFile(path.join(booksDirectory, 'manifest.json'), 'utf8'));
const upstream = path.join(root, 'vendor/acl2');
const revision = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
if (books.revision !== revision) throw new Error('The math books and ACL2 core sources must use the same revision.');
const image = await fs.readFile(imagePath), core = preview ? null : await fs.readFile(corePath);
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
let optimization;
if (core) {
  new WebAssembly.Module(core);
  optimization = JSON.parse(await fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8'));
  if (optimization.backend !== 'native-wasm' || optimization.coreSha256 !== sha256(core)
      || optimization.imageSha256 !== sha256(image) || optimization.revision !== revision)
    throw new Error('Expected a native ACL2 core built for this runtime and source revision.');
}
let checkpoint;
if (initialized) {
  const [saved, report] = await Promise.all([fs.readFile(options[1]), fs.readFile(options[2], 'utf8').then(JSON.parse)]);
  if (!report.passed || report.revision !== revision || report.imageSha256 !== sha256(image)
      || report.coreSha256 !== sha256(core) || report.checkpoint?.sha256 !== sha256(saved))
    throw new Error('Expected an ACL2 image that passed the restored prover tests.');
  checkpoint = saved;
}
const files = [];
if (!checkpoint) {
  for (const entry of await fs.readdir(upstream, {withFileTypes: true})) {
    if (!entry.isFile() || entry.name === '.git') continue;
    files.push([`/acl2/${entry.name}`, Buffer.from(portAcl2Source(entry.name,
      await fs.readFile(path.join(upstream, entry.name)), {revision}))]);
  }
  files.push(['/acl2/miricl-host.lisp', await fs.readFile(path.join(root, 'acl2/miricl-host.lisp'))]);
}
await fs.mkdir(output, {recursive: true});
const assets = ['index.html', 'examples.mjs', 'worker.mjs', 'app.mjs', 'session-store.mjs',
  'miricl.mjs', 'acl2.mjs', 'filesystem-archive.mjs'];
assets.push('acl2-books.mjs', 'dev-filesystem.mjs');
const data = checkpoint ? {'acl2-image.wasm.gz': gzipSync(checkpoint, {level: 9})} : {
  'miricl.wasm': image, 'filesystem.bin.gz': gzipSync(encodeFilesystem(files), {level: 9}),
  ...(core ? {'acl2.wasm': core} : {}),
};
assets.push(...Object.keys(data));
await Promise.all([
  ...['miricl.wasm', 'filesystem.bin.gz', 'acl2.wasm', 'acl2-image.wasm.gz'].map(name =>
    data[name] ? fs.writeFile(path.join(output, name), data[name]) : fs.rm(path.join(output, name), {force: true})),
  ...['index.html', 'examples.mjs', 'worker.mjs'].map(name => fs.copyFile(path.join(root, 'acl2', name), path.join(output, name))),
  ...['app.mjs', 'session-store.mjs'].map(name => fs.copyFile(path.join(root, 'app-common', name), path.join(output, name))),
  ...['miricl.mjs', 'acl2.mjs', 'acl2-books.mjs', 'dev-filesystem.mjs', 'filesystem-archive.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  fs.copyFile(path.join(upstream, 'LICENSE'), path.join(output, 'LICENSE')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
await fs.mkdir(path.join(output, 'books'), {recursive: true});
for (const name of ['manifest.json', 'filesystem.bin.gz'])
  await fs.copyFile(path.join(booksDirectory, name), path.join(output, 'books', name));
const escape = text => String(text).replace(/[&<>"']/gu, char =>
  ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const groups = books.selection.mathDirectories.map(prefix => {
  const entries = books.books.filter(book => book.path.startsWith(prefix + '/'));
  if (!entries.length) return '';
  return `<details><summary>${escape(prefix)} (${entries.length})</summary><ul class="book-list">${entries.map(book =>
    `<li><code>${escape(book.path)}</code> <small>${book.bytes.toLocaleString('en-US')} bytes</small></li>`).join('')}</ul></details>`;
}).join('\n');
const catalog = `<p>${books.books.length} selected math books · ${(books.archive.bytes / 1048576).toFixed(2)} MiB source download. <a href="books/manifest.json">Full file manifest</a> · <a href="books/filesystem.bin.gz" download>Download source artifact</a></p>${groups}`;
const page = path.join(output, 'index.html');
const startupNote = checkpoint ? '' : `<p class="preview-note">${preview
  ? 'Frontend preview. This build does not include the ACL2 prover.'
  : 'Development preview. The native prover initializes its logical world in the browser, which can take several minutes.'}</p>`;
await fs.writeFile(page, (await fs.readFile(page, 'utf8')).replace('<!-- BOOK-CATALOG -->', catalog)
  .replace('<!-- STARTUP-NOTE -->', startupNote));
const assetVersion = await versionAssets(output, assets);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({revision, assetVersion,
  coreAvailable: Boolean(core), validation: checkpoint ? 'restored-native-prover' : preview ? 'frontend-only' : 'bootstrap-unverified',
  imageSha256: sha256(checkpoint ?? image), coreSha256: core ? sha256(core) : null,
  backend: optimization?.backend ?? null, initializedWorld: Boolean(checkpoint),
  ...(checkpoint ? {imageBytes: checkpoint.length, imageDownloadBytes: data['acl2-image.wasm.gz'].length} : {}),
  coreBytes: core?.length ?? null, adaptations: acl2Adaptations,
  books: {count: books.books.length, archiveBytes: books.archive.bytes,
    sha256: books.archive.sha256, maxFileBytes: books.selection.maxFileBytes, loading: 'on-demand'},
}, null, 2) + '\n');
console.log(`Built ACL2 ${preview ? 'frontend preview' : 'browser distribution'} in ${output}`);
