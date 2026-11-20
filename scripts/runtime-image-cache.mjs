// Exact CI distribution cache identity. Host dependency metadata comes from the
// actual setup Lisp, without bootstrapping a guest compiler. Application sources
// and page contents are independent: pages are always regenerated afterwards.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {pathToFileURL} from 'node:url';
const run = promisify(execFile), root = path.resolve(import.meta.dirname, '..');
export const runtimeArtifacts = ['miricl.js', 'miricl-loader.mjs', 'miricl.wasm', 'miricl-interpreter.wasm', 'miricl-cold.wasm',
  'miricl-repl.wasm', 'miricl-asdf.wasm', 'runtime/miricl.js', 'runtime/version-assets.mjs',
  'runtime/compress-images.mjs', 'runtime/miricl-loader.mjs', 'build/asdf/asdf.lisp'];
const sha = bytes => createHash('sha256').update(bytes).digest('hex');
const read = name => fs.readFile(path.resolve(root, name));
export async function runtimeInputKey({workspace = root, execute = run} = {}) {
  const read = name => fs.readFile(path.resolve(workspace, name));
  const run = execute, root = workspace;
  const hash = createHash('sha256').update(JSON.stringify({node: process.versions, platform: process.platform, arch: process.arch}));
  const add = (name, bytes) => hash.update(name + '\0' + bytes.length + '\0').update(bytes);
  // Every root compiler/runtime source is an input. Avoid whole-repository hashes
  // so an application-only edit can reuse the matching compiler distribution.
  const tracked = (await run('git', ['ls-files', '-z'], {cwd: root, maxBuffer: 4 * 1024 * 1024})).stdout.split('\0');
  for (const name of tracked.filter(name => /^[^/]+\.(?:lisp|asd)$/.test(name)).sort()) add(name, await read(name));
  const scripts = new Set(['build-wasm.lisp', 'html-builder.lisp', 'build-runtime-pages.lisp', 'runtime-cache-host-inputs.lisp',
    'runtime-image-cache.mjs', 'build-asdf.mjs', 'prepare-compiler-image.mjs', 'compiler-image-bootstrap.lisp',
    'build-adapter.mjs', 'miricl.mjs', 'dev-repl.mjs', 'dev-filesystem.mjs', 'repl.mjs', 'host-filesystem.mjs',
    'terminal-debugger.mjs', 'repl-bridge.mjs', 'compilation-statistics.mjs', 'version-assets.mjs', 'compress-images.mjs',
    'miricl-loader.mjs']);
  for (const name of tracked.filter(name => /^scripts\/(?:repl[^/]*\.lisp|ports\/.*\.lisp)$/.test(name)))
    add(name, await read(name));
  // Include static and dynamic relative JS import closure of the build tools.
  async function module(name) {
    const file = `scripts/${name}`, bytes = await read(file); add(file, bytes);
    for (const match of bytes.toString().matchAll(/(?:from\s*|import\s*\()\s*['"](\.\/[^'"]+\.mjs)['"]/g)) {
      const relative = path.posix.normalize(path.posix.join(path.posix.dirname(name), match[1]));
      scripts.add(relative);
    }
  }
  // A processed set keeps imported modules from being counted twice.
  const processed = new Set();
  for (const name of scripts) if (!processed.has(name)) {processed.add(name); await module(name);}
  add('setup-miricl', await read('.github/actions/setup-miricl/action.yml'));
  add('package-local-nicknames', await read('compatibility/package-local-nicknames.lisp'));
  for (const name of tracked.filter(name => name.startsWith('patches/asdf/')).sort()) add(name, await read(name));
  const directory = path.join(root, 'vendor/asdf');
  const git = async args => (await run('git', ['-C', directory, ...args], {maxBuffer: 8 * 1024 * 1024})).stdout;
  const pin = (await run('git', ['ls-files', '--stage', '--', 'vendor/asdf'], {cwd: root})).stdout;
  const revision = (await git(['rev-parse', 'HEAD'])).trim();
  if (!pin.startsWith(`160000 ${revision} 0\t`)) throw new Error('ASDF checkout does not match the indexed pin.');
  await git(['diff', '--quiet', 'HEAD', '--']); add('asdf-pin', Buffer.from(pin));
  for (const name of (await git(['ls-files', '-z'])).split('\0').filter(Boolean).sort()) {
    const file = path.join(directory, name), stat = await fs.lstat(file);
    if (stat.isDirectory()) continue; // Optional ASDF development submodules.
    if (!stat.isFile() && !stat.isSymbolicLink()) throw new Error(`Invalid ASDF input ${name}`);
    add(`asdf/${name}`, stat.isSymbolicLink() ? Buffer.from(await fs.readlink(file)) : await fs.readFile(file));
  }
  const host = JSON.parse(await read('build/runtime-host-inputs.json'));
  if (!host.implementation || !host.asdf || !Array.isArray(host.systems)) throw new Error('Missing host dependency identity.');
  add('host-versions', Buffer.from(JSON.stringify(host)));
  for (const system of host.systems) for (const file of system.files) add(`${system.name}/${file}`, await fs.readFile(file));
  // The installed C API, rather than a mutable download URL alone, is an input.
  add('libwasmtime.so', await read('libwasmtime.so'));
  return hash.digest('hex');
}
export async function runtimeOutputRows(workspace = root) {
  workspace = await fs.realpath(workspace);
  const read = name => fs.readFile(path.resolve(workspace, name));
  const rows = [];
  for (const file of runtimeArtifacts) {
    const stat = await fs.lstat(path.resolve(workspace, file));
    if (!stat.isFile() || stat.size < 1 || stat.size > 256 * 1024 * 1024 ||
        await fs.realpath(path.resolve(workspace, file)) !== path.resolve(workspace, file))
      throw new Error(`Invalid runtime cache artifact size/type: ${file}`);
    const bytes = await read(file);
    const header = bytes.subarray(0, 8);
    if (!bytes.length || (file.endsWith('.wasm') && ![Buffer.from([0,97,115,109,1,0,0,0]), Buffer.from([0,97,115,109,13,0,1,0])].some(magic => header.equals(magic))))
      throw new Error(`Invalid runtime cache artifact: ${file}`);
    rows.push({file, bytes: bytes.length, sha256: sha(bytes)});
  }
  if (rows.find(row => row.file === 'miricl.wasm').bytes >= 1048576) throw new Error('Default compiler image exceeds the raw 1 MiB budget.');
  return rows;
}
export async function verifyRuntimeDistribution(workspace, key) {
  workspace = await fs.realpath(workspace);
  const file = path.join(workspace, 'build/runtime-images.json'), stat = await fs.lstat(file);
  if (!stat.isFile() || stat.size < 1 || stat.size > 1024 * 1024 || await fs.realpath(file) !== file) throw new Error('Invalid runtime cache manifest size/type.');
  const expected = JSON.parse(await fs.readFile(file, 'utf8'));
  const rows = await runtimeOutputRows(workspace);
  if (expected.key !== key || JSON.stringify(expected.rows) !== JSON.stringify(rows))
    throw new Error('Runtime image cache inputs or output digests do not match.');
}
if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
const mode = process.argv[2];
await fs.mkdir(path.join(root, 'build'), {recursive: true});
if (mode === 'key') {
  const key = await runtimeInputKey();
  await fs.writeFile(path.join(root, 'build/runtime-images.inputs.json'), JSON.stringify({key}) + '\n');
  if (process.env.GITHUB_OUTPUT) await fs.appendFile(process.env.GITHUB_OUTPUT, `key=${key}\n`);
  console.log(`Runtime image input key: ${key}`);
} else if (mode === 'record' || mode === 'verify') {
  const {key} = JSON.parse(await read('build/runtime-images.inputs.json'));
  const rows = await runtimeOutputRows();
  if (mode === 'record') await fs.writeFile(path.join(root, 'build/runtime-images.json'), JSON.stringify({key, rows}, null, 2) + '\n');
  else await verifyRuntimeDistribution(root, key);
  console.log(`Runtime image cache ${mode}: matching complete artifacts and raw compiler budget checked.`);
} else throw new Error('Usage: node scripts/runtime-image-cache.mjs key|record|verify');

}
