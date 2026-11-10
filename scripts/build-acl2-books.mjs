// Source-only Community Books artifact. cert.pl scans dependencies; it never
// runs ACL2, make, certification or a native compiler here.
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import {execFileSync} from 'node:child_process';
import {gzipSync} from 'node:zlib';
import {createHash} from 'node:crypto';
import {mathDirectories, maxBookBytes, parseBookDependencies, selectBooks} from '../acl2/books-policy.mjs';
import {encodeFilesystem} from './filesystem-archive.mjs';

const root = path.resolve(import.meta.dirname, '..');
const upstream = path.join(root, 'vendor/acl2'), directory = path.join(upstream, 'books');
const output = process.argv[2] ?? 'build/acl2/books';
const revision = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const names = execFileSync('git', ['-C', directory, 'ls-files', '-z'], {maxBuffer: 16 * 1024 * 1024})
  .toString().split('\0').filter(Boolean);
const sizes = new Map(await Promise.all(names.map(async name => [name, (await fs.stat(path.join(directory, name))).size])));
const candidates = names.filter(name => name.endsWith('.lisp') && !name.includes('/.sys/')
  && mathDirectories.some(prefix => name.startsWith(prefix + '/'))).sort();
const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-acl2-books-'));
let selection;
try {
  const makefile = path.join(temporary, 'dependencies.mk');
  const log = execFileSync('perl', ['build/cert.pl', '-b', directory, '-m', '--smakefile', '-o', makefile,
    ...candidates], {cwd: directory, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024});
  await fs.mkdir(output, {recursive: true});
  await fs.writeFile(path.join(output, 'dependency-scan.log'), log);
  const text = await fs.readFile(makefile + '.lsp', 'utf8');
  const blocked = new Set();
  for (const match of text.matchAll(/\( :CERT_PL_(USES_[A-Z0-9_]+|SBCL_ONLY|CCL_ONLY)\s+([^)]*)\)/gu))
    for (const item of match[2].matchAll(/"([^"]+)"/gu)) blocked.add(item[1]);
  selection = selectBooks(candidates, parseBookDependencies(text), sizes, blocked);
} finally {await fs.rm(temporary, {recursive: true, force: true});}
// Retain license notices for every included source directory and its ancestors.
const selected = new Set(selection.files);
for (const name of selection.files) {
  for (let parent = path.posix.dirname(name); parent !== '.'; parent = path.posix.dirname(parent))
    for (const license of ['LICENSE', 'LICENSE.txt', 'COPYING', '3BSD-mod.txt'])
      if (sizes.has(`${parent}/${license}`)) selected.add(`${parent}/${license}`);
}
for (const name of ['LICENSE', '3BSD-mod.txt', 'GPL2']) if (sizes.has(name)) selected.add(name);
const files = await Promise.all([...selected].sort().map(async name => {
  if (sizes.get(name) > maxBookBytes) throw new Error(`Oversized artifact file: ${name}`);
  return [`/acl2/books/${name}`, await fs.readFile(path.join(directory, name))];
}));
const archive = gzipSync(encodeFilesystem(files), {level: 9});
await fs.writeFile(path.join(output, 'filesystem.bin.gz'), archive);
await fs.writeFile(path.join(output, 'manifest.json'), JSON.stringify({revision,
  selection: {mathDirectories, maxFileBytes: maxBookBytes, sizeUnit: 'decimal MB', dependencyScanner: 'upstream cert.pl'},
  validation: 'source-selection-only; browser certification remains unverified',
  books: selection.books, excluded: selection.excluded,
  files: files.map(([name, data]) => ({path: name, bytes: data.length})),
  archive: {file: 'filesystem.bin.gz', bytes: archive.length,
    sha256: createHash('sha256').update(archive).digest('hex')},
}, null, 2) + '\n');
console.log(`Selected ${selection.books.length}/${candidates.length} math books; ${files.length} source/support files; ${archive.length} compressed bytes. Excluded ${selection.excluded.length} books.`);
