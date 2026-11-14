import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {spawn, spawnSync} from 'node:child_process';
import {Miricl} from '../scripts/miricl.mjs';
import {threadCases} from './thread-cases.mjs';

const binary = process.argv[2] ?? 'miricl.wasm';
const errors = [];
const callbackFixture = {
  save(good, bad) {this.good = good; this.bad = bad;},
  parked() {this.cleanupEntered = true;},
};
const vm = await Miricl.instantiate(await fs.readFile(binary), {
  imports: {threadCallbacks: callbackFixture}, onThreadError: error => errors.push(error),
});
assert.ok(vm.threads, 'The runtime must expose JavaScript Promise Integration.');
for (const [name, source] of threadCases) {
  let timer;
  const result = await Promise.race([
    vm.evaluateAsync(source, {eof: true}),
    new Promise((_, reject) => {timer = setTimeout(() => reject(new Error(`Timed out: ${name}`)), 10000);})
  ]).finally(() => clearTimeout(timer));
  assert.equal(result.output, 'T\n', name);
  console.log(`PASS ${name}`);
}
assert.equal(vm.evaluate('(+ 20 22)').output, '42\n', 'Synchronous evaluation remains usable.');
await vm.evaluateAsync('(defparameter *background-worker* (miricl-threads:make-thread (lambda () (sleep 0.01) 17)))');
assert.equal(vm.evaluate('(+ 20 22)').output, '42\n');
assert.equal((await vm.evaluateAsync('(miricl-threads:join-thread *background-worker*)')).output, '17\n');
console.log('PASS synchronous evaluation alongside a background thread');

// Application builders transfer compiled files after async LOAD finishes.
// Let the host clock advance beyond that evaluation's time slice, then make
// transfers large enough to visit cooperative runtime loops. A direct host
// export must not try to suspend through its ordinary JavaScript entry.
const originalHostService = vm.hostService.bind(vm);
let clockAdvance = 0;
vm.hostService = (operation, argument) => originalHostService(operation, argument)
  + (operation === 1 ? clockAdvance : 0);
const transferPath = '/thread-transfer.bin';
const transferBytes = Uint8Array.from({length: 4096}, (_, index) => index % 256);
await vm.evaluateAsync('(progn (miricl-threads:thread-yield) (values))');
clockAdvance += 1000;
vm.mountFile(transferPath, transferBytes);
for (const _ of transferBytes) assert.deepEqual(await vm.readFile(transferPath), transferBytes);
await vm.evaluateAsync(`(defparameter *transfer-worker*
  (miricl-threads:make-thread (lambda () (sleep 0.01) 42))) (values)`);
clockAdvance += 1000;
assert.deepEqual(await vm.readFile(transferPath), transferBytes);
assert.equal((await vm.evaluateAsync('(miricl-threads:join-thread *transfer-worker*)')).output, '42\n');
clockAdvance += 1000;
assert.deepEqual(await vm.readFile(transferPath), transferBytes);
console.log('PASS file transfers after async evaluation and background work');

// A DOM-style callback can arrive while the last active Lisp thread is
// sleeping in UNWIND-PROTECT cleanup, with a pending nonlocal transfer.
await vm.evaluateAsync(`
  (js:define-import save-thread-callbacks ("threadCallbacks" "save") (:value :value) :void)
  (js:define-import mark-thread-cleanup ("threadCallbacks" "parked") () :void)
  (defparameter *external-callback-count* 0)
  (save-thread-callbacks
    (js:callback (lambda (value)
      (incf *external-callback-count*)
      (unless (= *print-base* 10) (error "Callback inherited a parked thread's bindings."))
      (+ (js:from-js value) 2)))
    (js:callback (lambda () (error "External callback failure."))))
  (defparameter *callback-cleanup-worker*
    (miricl-threads:make-thread (lambda ()
      (let ((*print-base* 16))
        (catch 'callback-cleanup-exit
          (unwind-protect (throw 'callback-cleanup-exit 42)
            (mark-thread-cleanup) (sleep 0.15)))))))
  (values)`);
const callbackDeadline = performance.now() + 5000;
while (!callbackFixture.cleanupEntered) {
  assert.ok(performance.now() < callbackDeadline, 'Worker did not enter cleanup.');
  await new Promise(resolve => setTimeout(resolve, 1));
}
clockAdvance += 1000;
for (const _ of transferBytes) assert.deepEqual(await vm.readFile(transferPath), transferBytes);
assert.equal(callbackFixture.good(40), 42);
assert.throws(() => callbackFixture.bad(), /External callback failure/);
assert.equal(callbackFixture.good(41), 43);
assert.equal((await vm.evaluateAsync('(miricl-threads:join-thread *callback-cleanup-worker*)')).output, '42\n');
assert.equal(vm.evaluate('*external-callback-count*').output, '2\n');
console.log('PASS external callbacks isolate bindings and errors while preserving a parked nonlocal transfer');

const churn = '(dotimes (i 100) (miricl-threads:join-thread (miricl-threads:make-thread (lambda () (miricl-threads:thread-yield)))))';
await vm.evaluateAsync(churn);
await new Promise(resolve => setTimeout(resolve, 5));
const scratch = vm.vm['thread-scratch-memory'].buffer.byteLength;
await vm.evaluateAsync(churn);
await new Promise(resolve => setTimeout(resolve, 5));
assert.equal(vm.vm['thread-scratch-memory'].buffer.byteLength, scratch, 'Retired scratch buffers must be reused.');
assert.deepEqual(errors, [], 'No thread may escape its Lisp error boundary.');
console.log('PASS repeated thread creation reuses scratch storage');

// A previous large host transfer must not turn every later safepoint into a
// copy of that memory's full capacity. No Lisp data lives in the unused tail.
vm.vm.memory.grow(256);
await vm.evaluateAsync(churn);
assert.equal(vm.vm['thread-scratch-memory'].buffer.byteLength, scratch,
  'Thread snapshots must scale with live scratch bytes, not linear memory capacity.');
console.log('PASS thread snapshots ignore unused transport capacity');

const run = vm.threads.run;
vm.threads.run = async () => {throw new Error('simulated host trap');};
assert.equal((await vm.evaluateAsync(`(let ((worker (miricl-threads:make-thread (lambda () 42))))
  (handler-case (miricl-threads:join-thread worker) (miricl-threads:abnormal-exit () :host-error)))`)).output, ':HOST-ERROR\n');
vm.threads.run = run;
assert.equal(errors.length, 1);
assert.match(errors[0].message, /simulated host trap/);
console.log('PASS host exceptions terminate the worker and wake joiners');
vm.close();

if (process.argv[3]) {
  const command = ['run', '-W', 'stack-switching=y', '-S', 'p3=y', process.argv[3]];
  const session = threadCases.map(([name, source]) =>
    `(assert ${source} () ${JSON.stringify(name)})`).join('\n') + '\n(format t "GREEN-THREADS-PASSED~%")\n:quit\n';
  const wasi = spawnSync(process.env.WASMTIME ?? 'wasmtime',
    command,
    {input: session, encoding: 'utf8', timeout: 30000});
  assert.ifError(wasi.error);
  assert.equal(wasi.status, 0, wasi.stdout + wasi.stderr);
  assert.doesNotMatch(wasi.stdout, /Error:/);
  assert.match(wasi.stdout, /GREEN-THREADS-PASSED/);
  console.log('PASS Wasmtime green threads');

  // Keep stdin open: a background thread must run while the REPL waits for input.
  const child = spawn(process.env.WASMTIME ?? 'wasmtime',
    command, {stdio: ['pipe', 'pipe', 'pipe']});
  let output = '', diagnostics = '', timer;
  const exited = new Promise((resolve, reject) => {
    child.on('error', reject);
    child.on('exit', code => code === 0 ? resolve() : reject(new Error(diagnostics)));
  });
  try {
    await new Promise((resolve, reject) => {
      timer = setTimeout(() => reject(new Error('WASI background output timed out.')), 10000);
      child.stdout.on('data', data => {
        output += data;
        if (output.includes('IDLE-WORKER-PASSED')) resolve();
      });
      child.stderr.on('data', data => {diagnostics += data;});
      child.stdin.write('(progn (miricl-threads:make-thread (lambda () (sleep 0.01) (format t "IDLE-WORKER-PASSED~%"))) (values))\n');
    });
    clearTimeout(timer);
    // Quitting also stops a worker that would otherwise run forever.
    child.stdin.end('(miricl-threads:make-thread (lambda () (loop)))\n:quit\n');
    await Promise.race([exited, new Promise((_, reject) => {
      timer = setTimeout(() => reject(new Error('WASI quit timed out.')), 10000);
    })]);
    assert.doesNotMatch(output, /Error:/);
    console.log('PASS Wasmtime idle input and shutdown with a background worker');
  } finally {
    clearTimeout(timer);
    child.kill();
  }
}
