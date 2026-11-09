import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {MountedFilesystem} from '../scripts/mounted-filesystem.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const encode = text => new TextEncoder().encode(text);
const provider = new MountedFilesystem(vm, [
  ['/archive/main.lisp', encode('(load "archive/nested/child.lisp") (defparameter *archive-main* (+ *archive-child* 1))')],
  ['/archive/nested/child.lisp', encode('(defparameter *archive-child* 41)')],
  ['/archive/space λ.bin', Uint8Array.of(0, 255, 128, 65)],
  ['/archive/empty', new Uint8Array()],
]);
const check = async (source, expected = 'T') =>
  assert.equal((await vm.evaluateAsync(source, {eof: true})).output, expected + '\n', source);
try {
  assert.equal(provider.loaded.size, 0);
  await check('(and (probe-file "/archive/main.lisp") (integerp (file-write-date "/archive/main.lisp")))');
  assert.equal(provider.loaded.size, 0, 'Metadata must not copy file contents into the guest.');
  await check('(sort (mapcar #\'namestring (directory "/archive/**/*.lisp")) #\'string<)',
    '("/archive/main.lisp" "/archive/nested/child.lisp")');
  await check('(sort (mapcar #\'namestring (directory "/archive/*/")) #\'string<)', '("/archive/nested/")');
  await check('(load "/archive/main.lisp") *archive-main*', 'T\n42');
  assert.equal(provider.loaded.size, 2);
  await check(`(with-open-file (s "/archive/space λ.bin" :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte s nil nil) while byte collect byte))`, '(0 255 128 65)');
  await check('(with-open-file (s "/archive/empty") (null (read-char s nil nil)))');
  await check(`(progn
    (with-open-file (s "/archive/space λ.bin" :element-type '(unsigned-byte 8)
                     :direction :output :if-exists :append) (write-byte 7 s))
    (with-open-file (s "/archive/space λ.bin" :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte s nil nil) while byte collect byte)))`, '(0 255 128 65 7)');
  await check(`(progn (rename-file "/archive/space λ.bin" "/archive/renamed.bin")
    (and (null (probe-file "/archive/space λ.bin")) (probe-file "/archive/renamed.bin") t))`);
  await check(`(progn (delete-file "/archive/nested/child.lisp")
    (and (null (probe-file "/archive/nested/child.lisp"))
         (equal (mapcar #'namestring (directory "/archive/**/*.lisp")) '("/archive/main.lisp"))))`);
  vm.mountFile('/archive/main.lisp', '(defparameter *archive-main* :overlay)');
  await check('(load "/archive/main.lisp") *archive-main*', 'T\n:OVERLAY');
  provider.close();
  assert.equal(vm.vm['filesystem-services'].get(0), null);
  assert.equal(provider.loaded.size, 0);
  console.log('Archive files: lazy contents, metadata, directories, nested LOAD, Unicode/binary files, empty files, overlay writes, rename and deletion pass.');
} finally {provider.close(); vm.close();}
