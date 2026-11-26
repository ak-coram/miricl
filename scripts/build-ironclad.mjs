// Compile Ironclad to native Wasm with the compiler in the supplied image.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {createHash} from 'node:crypto';
import {gzipSync} from 'node:zlib';
import {loadMiricl} from './miricl-loader.mjs';
import {collectCompilationStatistics} from './compilation-statistics.mjs';

const root = path.resolve(import.meta.dirname, '..');
const upstream = path.join(root, 'vendor/ironclad');
const app = path.join(root, 'ironclad');
const [imageFile = path.join(root, 'miricl.wasm'), output = path.join(root, 'build/ironclad/site')] = process.argv.slice(2);
const run = promisify(execFile);
const sha = bytes => createHash('sha256').update(bytes).digest('hex');
const [image, buildSource, {stdout: revisionText}, {stdout: sourceList}] = await Promise.all([
  fs.readFile(imageFile),
  fs.readFile(path.join(app, 'build.lisp')),
  run('git', ['-C', upstream, 'rev-parse', '--verify', 'HEAD']),
  run(process.env.SBCL ?? 'sbcl', ['--noinform', '--script', path.join(app, 'sources.lisp'), upstream]),
]);
const revision = revisionText.trim();
const files = sourceList.trim().split(/\r?\n/u);
if (!files.length || files.some(name => !/^src\/(?:[\w-]+\/)*[\w-]+\.lisp$/u.test(name)))
  throw new Error('Unexpected Ironclad source manifest.');
const sources = await Promise.all(files.map(name => fs.readFile(path.join(upstream, name))));
let nativeFile;
const vm = await loadMiricl(image, {imports: {ironcladBuild: {
  progress(file, count) {
    if (file !== nativeFile) console.log(`Ironclad native: ${file} (${count} initializers)`);
    nativeFile = file;
  },
}}});
const parts = [Buffer.from([0, 97, 115, 109, 1, 0, 0, 0])];
let core;
try {
  vm.mountFile('/ironclad-build.lisp', buildSource);
  await vm.evaluateAsync('(load "/ironclad-build.lisp") (values)', {eof: true});
  await collectCompilationStatistics(vm, {workload: 'ironclad', phase: 'compile and load portable files'},
    async () => {
      for (const [index, name] of files.entries()) {
        const guest = `/ironclad/${name}`;
        vm.mountFile(guest, sources[index]);
        try {
          await vm.evaluateAsync(`(ironclad-build-compile-file ${JSON.stringify(guest)} "/ironclad-part.wasm")`, {eof: true});
          const bytes = Buffer.from(await vm.readFile('/ironclad-part.wasm'));
          console.log(`${name} ${bytes.length} bytes`);
          parts.push(bytes.subarray(8));
        } catch (error) {
          throw new Error(`Could not compile and load ${name}: ${error.message}`, {cause: error});
        }
      }
    });
  vm.mountFile('/ironclad-portable.wasm', Buffer.concat(parts));
  parts.length = 0;
  core = await collectCompilationStatistics(vm, {workload: 'ironclad', phase: 'emit native bundle'},
    async () => {
      try {
        await vm.evaluateAsync('(ironclad-build-native-bundle "/ironclad-portable.wasm" "/ironclad-native.wasm")', {eof: true});
        return await vm.readFile('/ironclad-native.wasm');
      } catch (error) {
        throw new Error(`Could not emit the native Ironclad bundle (${nativeFile ?? 'before the first initializer'}): ${error.message}`,
          {cause: error});
      }
    });
} finally { vm.close(); }
await fs.mkdir(output, {recursive: true});
for (const filename of ['index.html', 'app.mjs', 'worker.mjs', 'runner.lisp'])
  await fs.copyFile(path.join(app, filename), path.join(output, filename));
await fs.copyFile(path.join(root, 'scripts/miricl-loader.mjs'), path.join(output, 'miricl-loader.mjs'));
await fs.copyFile(path.join(upstream, 'benchmark/benchmark-implementation.lisp'), path.join(output, 'upstream-benchmarks.lisp'));
await fs.copyFile(path.join(upstream, 'LICENSE'), path.join(output, 'IRONCLAD-LICENSE'));
await fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE'));
const runtimeGzip = gzipSync(image, {level: 9});
const coreGzip = gzipSync(core, {level: 9});
await fs.writeFile(path.join(output, 'miricl.wasm.gz'), runtimeGzip);
await fs.writeFile(path.join(output, 'ironclad.wasm.gz'), coreGzip);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({
  revision, backend: 'native-wasm', imageSha256: sha(image), coreSha256: sha(core),
}, null, 2) + '\n');
console.log(`Built Ironclad benchmarks in ${output}`);
console.log(`MIRICL runtime ${image.length} bytes (${runtimeGzip.length} gzip), native Ironclad ${core.length} bytes (${coreGzip.length} gzip).`);
