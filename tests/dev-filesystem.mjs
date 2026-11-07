// Host file I/O uses ordinary Lisp APIs and the existing green-thread driver.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {DevFilesystem} from '../scripts/dev-filesystem.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
const fetch = globalThis.fetch;
const encoder = new TextEncoder();
const files = new Map([
  ['host/main.lisp', encoder.encode('(defparameter *host-value* 42) (load (merge-pathnames "nested/child.lisp" *load-truename*))')],
  ['host/nested/child.lisp', encoder.encode('(incf *host-value*)')],
  ['host/space λ.bin', Uint8Array.of(0, 255, 128, 65)],
]);
const directories = new Set(['', 'host', 'host/nested', 'host/empty']);
const requests = [], delayed = new Set();
let aborted = 0, unixTime = 1700000000;
globalThis.fetch = async (url, {signal}) => {
  const match = new URL(url).pathname.match(/^\/__miricl__\/fs\/(stat|read|list)\/(.*)$/u);
  assert.ok(match, String(url));
  const [, operation, encoded] = match;
  const name = decodeURIComponent(encoded);
  requests.push([operation, name]);
  if (delayed.has(name)) await new Promise((resolve, reject) => {
    const timer = setTimeout(resolve, 100);
    signal.addEventListener('abort', () => {
      clearTimeout(timer); aborted++; reject(new Error('Aborted'));
    }, {once: true});
  });
  if (!files.has(name) && !directories.has(name)) return new Response(null, {status: 404});
  if (operation === 'read') return new Response(files.get(name));
  if (operation === 'stat') {
    const bytes = new Uint8Array(24), view = new DataView(bytes.buffer);
    view.setUint32(0, directories.has(name) ? 2 : 5, true);
    view.setUint32(4, 1, true); view.setBigInt64(8, BigInt(unixTime), true);
    return new Response(bytes);
  }
  const chunks = [];
  for (const child of [...directories, ...files.keys()]) {
    const prefix = name ? name + '/' : '';
    if (child === name || !child.startsWith(prefix)) continue;
    const entry = child.slice(prefix.length);
    if (entry.includes('/')) continue;
    const text = encoder.encode(entry), bytes = new Uint8Array(8 + text.length);
    const view = new DataView(bytes.buffer);
    view.setUint32(0, directories.has(child) ? 2 : 5, true);
    view.setUint32(4, text.length, true); bytes.set(text, 8); chunks.push(bytes);
  }
  chunks.push(new Uint8Array(8));
  return new Response(new Blob(chunks));
};

let provider;
const check = async (source, expected = 'T') => {
  provider?.refresh();
  assert.equal((await vm.evaluateAsync(source, {eof: true})).output, expected + '\n', source);
};
try {
  assert.equal(vm.vm['filesystem-services'].get(0), null);
  await check('(null (probe-file "host/main.lisp"))');
  provider = new DevFilesystem(vm);
  await check('(progn (load "host/main.lisp") (= *host-value* 43))');
  await check('(file-write-date "host/main.lisp")', '3908988800');
  const changed = setTimeout(() => unixTime++, 10);
  await check(`(let ((before (file-write-date "host/main.lisp")))
    (sleep .03) (> (file-write-date "host/main.lisp") before))`);
  clearTimeout(changed);
  await check(`(list (namestring (truename "host/empty")) (probe-file "missing"))`,
    '("/host/empty/" NIL)');
  await check(`(sort (mapcar #'namestring (directory "host/*/")) #'string<)`,
    '("/host/empty/" "/host/nested/")');
  await check(`(sort (mapcar #'namestring (directory "host/**/*.lisp")) #'string<)`,
    '("/host/main.lisp" "/host/nested/child.lisp")');
  await check(`(with-open-file (s "host/space λ.bin" :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte s nil nil) while byte collect byte))`, '(0 255 128 65)');
  await check('(load "missing" :if-does-not-exist nil)', 'NIL');
  const before = requests.length;
  assert.equal(vm.evaluate(`(handler-case (probe-file "host/main.lisp") (file-error () t))`).output, 'T\n');
  assert.equal(requests.length, before, 'Synchronous evaluation must not start asynchronous I/O.');
  vm.mountFile('local.lisp', '(defparameter *local-value* 7)');
  vm.mountFile('local.bin', Uint8Array.of(3, 4));
  assert.equal(vm.evaluate('(load "local.lisp") *local-value*').output, 'T\n7\n');
  await check(`(with-open-file (s "local.bin" :element-type '(unsigned-byte 8)) (= (read-byte s) 3))`);
  await check(`(progn
    (with-open-file (s "host/space λ.bin" :direction :output :if-exists :supersede
        :element-type '(unsigned-byte 8)) (write-byte 17 s))
    (with-open-file (s "host/space λ.bin" :element-type '(unsigned-byte 8))
      (and (= (read-byte s) 17) (= (file-length s) 1))))`);
  assert.deepEqual(files.get('host/space λ.bin'), Uint8Array.of(0, 255, 128, 65),
    'Compiler output and edits must stay in the browser overlay.');

  delayed.add('host/main.lisp');
  await check(`(let ((progress nil)
                    (reader (miricl-threads:make-thread (lambda () (probe-file "host/main.lisp")))))
    (sleep .01) (setf progress t)
    (and progress (pathnamep (miricl-threads:join-thread reader))))`);
  await check(`(let ((reader (miricl-threads:make-thread (lambda () (probe-file "host/main.lisp")))))
    (sleep .01) (miricl-threads:destroy-thread reader)
    (handler-case (miricl-threads:join-thread reader) (miricl-threads:abnormal-exit () t)))`);
  assert.ok(aborted > 0, 'Interrupting a file wait cancels its fetch.');
  assert.equal(provider.requests.size, 0);
  delayed.clear();
  await check(`(= (miricl-threads:join-thread (miricl-threads:make-thread (lambda () 42))) 42)`);
  delayed.add('host/main.lisp');
  const closing = check('(handler-case (probe-file "host/main.lisp") (file-error () t))');
  const closeTimer = setTimeout(() => provider.close(), 10);
  await closing;
  clearTimeout(closeTimer);
  assert.equal(provider.requests.size, 0);
  assert.equal(vm.vm['filesystem-services'].get(0), null);
  await check('(null (probe-file "host/main.lisp"))');
  console.log('Development files: normal file APIs, Unicode/binary data, nested LOAD, directories, overlay writes, async waits, and cancellation pass.');
} finally {
  provider?.close(); vm.close(); globalThis.fetch = fetch;
}
