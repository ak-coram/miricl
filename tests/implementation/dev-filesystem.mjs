// Host file I/O uses ordinary Lisp APIs and the existing green-thread driver.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {Miricl} from '../../scripts/miricl.mjs';
import {DevFilesystem} from '../../scripts/dev-filesystem.mjs';
import {HostFilesystem} from '../../scripts/host-filesystem.mjs';
import {enableCompilationStatistics, disableCompilationStatistics, readCompilationStatistics,
  collectCompilationStatistics} from '../../scripts/compilation-statistics.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'), {
  imports: {filesystemTest: {advanceDate() { unixTime++; }}},
});
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
  await check(`(progn
    (js:define-import advance-metadata-date ("filesystemTest" "advanceDate") () :void)
    (let ((before (file-write-date "host/main.lisp")))
      (advance-metadata-date)
      (sleep .03) (> (file-write-date "host/main.lisp") before)))`);
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
  await check(`(progn
    (delete-file "host/space λ.bin")
    (and (null (probe-file "host/space λ.bin"))
      (handler-case (progn (file-write-date "host/space λ.bin") nil) (file-error () t))
      (null (directory "host/*.bin"))
      (progn (rename-file "host/nested/child.lisp" "replacement.lisp")
        (null (probe-file "host/nested/child.lisp")))
      (probe-file "host/nested/replacement.lisp")
      (progn (with-open-file (s "host/space λ.bin" :direction :output)
               (write-string "new" s))
        (probe-file "host/space λ.bin"))
      (= (length (directory "host/*.bin")) 1)))`);
  assert.ok(files.has('host/nested/child.lisp'), 'Renames stay in the overlay.');

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
  const directory = fs.mkdtempSync(join(tmpdir(), 'miricl-compiler-statistics-'));
  const previousDirectory = process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY;
  const readFile = vm.readFile;
  const initialCount = 3, increment = 2;
  const counter = rows => rows.find(row => row.rule === 'collector-test' && row.detail === 'operation');
  try {
    provider = new HostFilesystem(vm, directory);
    enableCompilationStatistics(vm);
    vm.evaluate(`(defparameter *host-statistics-table* miricl-compiler::*compilation-statistics*)
      (miricl-compiler::record-compilation-statistic :collector-test :operation ${initialCount}) (values)`);
    assert.equal(counter(await readCompilationStatistics(vm))?.count, initialCount);
    await check(`(and (eq *host-statistics-table* miricl-compiler::*compilation-statistics*)
      (null miricl-compiler::*statistics-report-snapshot*))`);
    vm.evaluate(`(miricl-compiler::record-compilation-statistic :collector-test :operation ${increment}) (values)`);
    const readbackError = new Error('Injected statistics readback failure');
    vm.readFile = async () => {vm.readFile = readFile; throw readbackError;};
    await assert.rejects(readCompilationStatistics(vm), error => error === readbackError);
    await check(`(and (eq *host-statistics-table* miricl-compiler::*compilation-statistics*)
      (null miricl-compiler::*statistics-report-snapshot*))`);
    assert.equal(counter(await readCompilationStatistics(vm))?.count, initialCount + increment);
    disableCompilationStatistics(vm);

    process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY = directory;
    let operations = 0;
    const operationResult = {collected: true};
    assert.equal(await collectCompilationStatistics(vm,
      {workload: 'host-filesystem-test', phase: 'collector'}, async () => {
        operations++;
        await vm.evaluateAsync(`(miricl-compiler::record-compilation-statistic
          :collector-test :operation ${initialCount}) (values)`);
        return operationResult;
      }), operationResult);
    assert.equal(operations, 1);
    const reports = fs.readdirSync(directory).filter(name => name.endsWith('.json'))
      .map(name => JSON.parse(fs.readFileSync(join(directory, name), 'utf8')));
    const report = reports.find(record => record.workload === 'host-filesystem-test' && record.phase === 'collector');
    assert.equal(report?.status, 'complete');
    assert.equal(counter(report.counters)?.count, initialCount);
    await check(`(and (null miricl-compiler::*compilation-statistics*)
      (null miricl-compiler::*statistics-report-snapshot*) (= (+ 20 22) 42))`);
  } finally {
    vm.readFile = readFile;
    if (previousDirectory === undefined) delete process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY;
    else process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY = previousDirectory;
    disableCompilationStatistics(vm);
    provider.close();
    fs.rmSync(directory, {recursive: true, force: true});
  }
  console.log('Compiler statistics: async host files, preserved counters, failed-read recovery, and collection lifecycle pass.');
  console.log('Development files: normal file APIs, Unicode/binary data, nested LOAD, directories, overlay writes, async waits, and cancellation pass.');
} finally {
  provider?.close(); vm.close(); globalThis.fetch = fetch;
}
