import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {gzipSync, gunzipSync} from 'node:zlib';
import {Miricl} from '../scripts/miricl.mjs';
import {Acl2Books} from '../scripts/acl2-books.mjs';
import {encodeFilesystem, decodeFilesystem} from '../scripts/filesystem-archive.mjs';
import {maxBookBytes, mathDirectories, parseBookDependencies, selectBooks} from '../acl2/books-policy.mjs';

const graph = parseBookDependencies(`( :DEPENDENCY-MAP
 ("math/good.cert" "math/good.lisp" "utility.cert")
 ("utility.cert" "utility.lisp" "build/defrec-certdeps/STATE.certdep")
 ("math/large.cert" "math/large.lisp")
 ("math/transitive.cert" "math/transitive.lisp" "math/large.cert")
 ("math/host.cert" "math/host.lisp")
 ("math/missing.cert" "math/missing.lisp" "absent.cert"))`);
const sizes = new Map(['good', 'large', 'transitive', 'host', 'missing'].map(name => [`math/${name}.lisp`, 100]));
sizes.set('utility.lisp', maxBookBytes); sizes.set('math/large.lisp', maxBookBytes + 1);
const selected = selectBooks([...sizes.keys()].filter(name => name.startsWith('math/')), graph, sizes,
  new Set(['math/host.cert']));
assert.deepEqual(selected.books, [{path: 'math/good', bytes: 100}]);
assert.deepEqual(selected.files, ['math/good.lisp', 'utility.lisp']);
assert.equal(selected.excluded.length, 4);
assert.match(selected.excluded.find(book => book.path === 'math/transitive.lisp').reason, /exceeds 2 MB/);
assert.throws(() => parseBookDependencies('bad scanner output'), /dependency map/);

const directory = process.argv[3] ?? 'build/acl2/books';
const manifest = JSON.parse(fs.readFileSync(path.join(directory, 'manifest.json'), 'utf8'));
const archive = fs.readFileSync(path.join(directory, 'filesystem.bin.gz'));
const unpacked = decodeFilesystem(gunzipSync(archive));
assert.equal(archive.length, manifest.archive.bytes);
assert.equal(unpacked.length, manifest.files.length);
assert.equal(new Set(manifest.books.map(book => book.path)).size, manifest.books.length);
for (const book of manifest.books) {
  assert.ok(mathDirectories.some(prefix => book.path.startsWith(prefix + '/')), book.path);
  assert.ok(unpacked.some(([name]) => name === `/acl2/books/${book.path}.lisp`), book.path);
}
for (const [name, bytes] of unpacked) {
  assert.ok(bytes.length <= maxBookBytes, name);
  assert.equal(bytes.length, manifest.files.find(file => file.path === name).bytes);
  assert.ok(!/\.(?:cert|wasm|fasl)$/u.test(name), 'Only source/support files are bundled');
}
assert.ok(manifest.books.some(book => book.path === 'arithmetic/factorial'));

const originalFetch = globalThis.fetch;
let downloads = 0;
const source = gzipSync(encodeFilesystem([
  ['/acl2/books/math/answer.lisp', new TextEncoder().encode('(defparameter *book-answer* 42)')],
]));
globalThis.fetch = async url => {
  assert.equal(String(url), 'https://example.test/books/filesystem.bin.gz'); downloads++;
  await new Promise(resolve => setTimeout(resolve, 5));
  return new Response(source);
};
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-precompiled.wasm'));
const books = new Acl2Books(vm, new URL('https://example.test/books/filesystem.bin.gz'));
try {
  assert.equal(downloads, 0);
  assert.equal(await books.stat('acl2/other'), null);
  assert.equal((await books.stat('acl2/books')).kind, 2);
  assert.equal(downloads, 0, 'World initialization must not download books');
  const [first, second] = await Promise.all([books.stat('acl2/books/math/answer.lisp'), books.list('acl2/books')]);
  assert.equal(first.kind, 5); assert.deepEqual(second, [['math', true]]);
  assert.equal(downloads, 1, 'Concurrent accesses share one download');
  const result = await vm.evaluateAsync(`
    (load "/acl2/books/math/answer.lisp")
    (assert (= *book-answer* 42))
    (assert (probe-file "/acl2/books/math/answer.lisp"))
    (assert (file-write-date "/acl2/books/math/answer.lisp"))
    (assert (= 1 (length (directory "/acl2/books/math/*.lisp"))))
    (compile-file "/acl2/books/math/answer.lisp" :output-file "/acl2/books/math/answer.wasm")
    (setq *book-answer* 0)
    (load "/acl2/books/math/answer.wasm")
    (assert (= *book-answer* 42))
    (with-open-file (s "/acl2/books/math/new.cert" :direction :output) (write-line "session certificate fixture" s))
    (assert (probe-file "/acl2/books/math/new.cert"))
    (values)`, {eof: true});
  assert.equal(downloads, 1);
  assert.ok((await vm.readFile('/acl2/books/math/answer.wasm')).length > 8);
  assert.equal(result.quit, false);
} finally {books.close(); vm.close(); globalThis.fetch = originalFetch;}
console.log(`ACL2 books: ${manifest.books.length} math roots, source-only archive, 2 MB limit and transitive exclusions; lazy download, real Lisp file access, on-demand compilation and writable overlay pass.`);
