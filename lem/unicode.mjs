// Only upstream Unicode source DATA is generated on the host. Application
// compilation, including these generated forms, belongs to the Wasm compiler.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {spawn, execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {root, inspectCheckout, relativeName} from './files.mjs';
const run = promisify(execFile);
export const unicodeNames = ['lists.lisp', 'hash-tables.lisp', 'methods.lisp'];
const pins = { 'cl-unicode': 'lem/deps/cl-unicode', 'cl-ppcre': 'vendor/asdf/ext/cl-ppcre',
  'flexi-streams': 'lem/deps/flexi-streams', 'trivial-gray-streams': 'lem/deps/trivial-gray-streams'};
export const digest = bytes => createHash('sha256').update(bytes).digest('hex');
export const lispString = value => '"' + value.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
export async function atomicWrite(file, bytes) {
  await fs.mkdir(path.dirname(file), {recursive: true});
  const temporary = `${file}.${process.pid}.tmp`;
  try {await fs.writeFile(temporary, bytes); await fs.rename(temporary, file);}
  finally {await fs.rm(temporary, {force: true});}
}
async function trackedSources(directory) {
  const {stdout} = await run('git', ['-C', directory, 'ls-files', '-z'], {maxBuffer: 4 * 1024 * 1024});
  const files = new Map();
  for (const name of stdout.split('\0').filter(Boolean).sort()) {
    relativeName(name);
    const file = path.join(directory, name), stat = await fs.lstat(file);
    if (!stat.isFile() || stat.size > 16 * 1024 * 1024 || await fs.realpath(file) !== file)
      throw new Error(`Invalid tracked Unicode generator source: ${file}`);
    files.set(name, await fs.readFile(file));
  }
  return files;
}
export async function unicodeData(directory, {workspace = root, onOutput = text => process.stdout.write(text)} = {}) {
  directory = path.resolve(directory);
  const executable = process.env.LEM_SBCL ?? 'sbcl';
  const localHeap = process.env.LEM_SBCL_DYNAMIC_SPACE_SIZE;
  if (localHeap !== undefined && !/^[1-9][0-9]{0,6}$/.test(localHeap))
    throw new Error('LEM_SBCL_DYNAMIC_SPACE_SIZE must be a positive number of MiB.');
  const heapArguments = localHeap ? ['--dynamic-space-size', localHeap] : [];
  const {stdout: version} = await run(executable, ['--version']);
  const hash = createHash('sha256').update(version).update(await fs.readFile(import.meta.filename));
  const pristine = new Map();
  for (const [name, relative] of Object.entries(pins)) {
    const source = path.join(workspace, relative), state = await inspectCheckout(source);
    if (state.revision !== state.gitlink || state.status || state.repository !== state.registeredRepository)
      throw new Error(`Unicode generator requires a clean matching submodule: ${relative}`);
    hash.update(name).update(state.gitlink).update(state.repository);
    const files = await trackedSources(source); pristine.set(name, files);
    for (const [file, bytes] of files) hash.update(file + '\0' + bytes.length + '\0').update(bytes);
  }
  const inputKey = hash.digest('hex'), target = path.join(directory, inputKey);
  const manifestFile = path.join(target, 'generated.json');
  try {
    const manifest = JSON.parse(await fs.readFile(manifestFile, 'utf8'));
    if (manifest.inputKey !== inputKey) throw new Error('Unicode data cache input mismatch.');
    const files = new Map();
    for (const name of unicodeNames) {
      const file = path.join(target, name), stat = await fs.lstat(file);
      if (!stat.isFile() || stat.size < 1 || stat.size > 16 * 1024 * 1024) throw new Error(`Invalid cached Unicode data: ${name}`);
      const bytes = await fs.readFile(file);
      if (digest(bytes) !== manifest.outputs?.[name]) throw new Error(`Unicode data cache digest mismatch: ${name}`);
      files.set(`/deps/cl-unicode/${name}`, bytes);
    }
    return {files, inputKey, host: version.trim(), outputs: manifest.outputs};
  } catch (error) {if (error.code !== 'ENOENT') throw error;}
  await fs.mkdir(directory, {recursive: true});
  const temporary = await fs.mkdtemp(path.join(directory, 'generate-'));
  try {
    for (const [name, files] of pristine) for (const [relative, bytes] of files) {
      const file = path.join(temporary, name, relative);
      await fs.mkdir(path.dirname(file), {recursive: true}); await fs.writeFile(file, bytes);
    }
    const unicode = path.join(temporary, 'cl-unicode') + '/';
    const script = path.join(temporary, 'generate.lisp');
    await fs.writeFile(script, `(require :asdf)
(asdf:initialize-source-registry '(:source-registry ${Object.keys(pins).map(name => `(:directory ${lispString(path.join(temporary, name) + '/')})`).join(' ')} :ignore-inherited-configuration))
(asdf:initialize-output-translations '(:output-translations (t ${lispString(path.join(temporary, 'cache') + '/')}) :ignore-inherited-configuration))
(asdf:load-asd ${lispString(unicode + 'cl-unicode.asd')})
(assert (equal (asdf:system-source-directory "cl-unicode/build") (pathname ${lispString(unicode)})))
(asdf:load-system "cl-unicode/build")
`);
    await new Promise((resolve, reject) => {
      const child = spawn(executable, [...heapArguments, '--noinform', '--no-sysinit', '--no-userinit', '--non-interactive', '--load', script],
        {stdio: ['ignore', 'pipe', 'pipe']});
      child.stdout.on('data', bytes => onOutput(bytes.toString()));
      child.stderr.on('data', bytes => onOutput(bytes.toString()));
      child.on('error', reject);
      child.on('close', code => code === 0 ? resolve() : reject(new Error(`Unicode DATA generator exited ${code}.`)));
    });
    const files = new Map(), outputs = {};
    for (const name of unicodeNames) {
      const file = path.join(unicode, name), stat = await fs.lstat(file);
      if (!stat.isFile() || stat.size < 1 || stat.size > 16 * 1024 * 1024) throw new Error(`Invalid generated Unicode data: ${name}`);
      const bytes = await fs.readFile(file);
      outputs[name] = digest(bytes); files.set(`/deps/cl-unicode/${name}`, bytes);
      await atomicWrite(path.join(target, name), bytes);
    }
    await atomicWrite(manifestFile, JSON.stringify({inputKey, host: version.trim(), outputs}, null, 2) + '\n');
    return {files, inputKey, host: version.trim(), outputs};
  } finally {await fs.rm(temporary, {recursive: true, force: true});}
}
