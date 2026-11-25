import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {spawn, spawnSync} from 'node:child_process';
import {Miricl} from '../../scripts/miricl.mjs';
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

// A discarded proved ENDP keeps its checkpoint. Snapshot the captured cell
// directly after it: a following callback would introduce another poll and
// conceal removal of the operation's original checkpoint.
assert.equal((await vm.evaluateAsync(`
  (multiple-value-bind (worker mark)
      (funcall (miricl-compiler:compile-native
        '(lambda ()
           (declare (optimize (space 3) (speed 1) (safety 1) (debug 0)))
           (let ((delivered nil))
             (values
               (lambda (arm)
                 (labels ((empty (callback) (funcall callback) nil))
                   (declare (notinline empty))
                   (endp (empty arm)) (values delivered nil :tail)))
               (lambda () (setq delivered t) (values :clobber :extra)))))))
    (equal (multiple-value-list
      (funcall worker (lambda ()
        (miricl-threads:without-interrupts
          (miricl-threads:interrupt-thread (miricl-threads:current-thread) mark)))))
      '(t nil :tail)))`, {eof: true})).output, 'T\n',
  'Discarded proved ENDP retains queued delivery and complete values.');
console.log('PASS discarded proved ENDP checkpoint');

// APPLY's fixed route shares its final spine; packet calls capture its values
// before selected-callee scheduling. Queue while synchronous delivery is off,
// then enter only the observed application with the scheduler active.
const captureApplyObject = expression => {
  const previous = vm.javascript.callback;
  let object;
  vm.javascript.callback = function (value) {object = value; return previous.call(this, value);};
  try {vm.evaluate(`(js:callback ${expression}) (values)`, {eof: true});}
  finally {vm.javascript.callback = previous;}
  assert.ok(object);
  return object;
};
const applyObject = expression => vm.vm['native-call0'](captureApplyObject(`(lambda () ${expression})`));
const applyTargets = [
  captureApplyObject("(miricl-compiler:compile-native '(lambda (x) x))"),
  captureApplyObject("(miricl-compiler:compile-native '(lambda (x &optional y) (declare (ignore y)) x))"),
];
const applyBuiltin = applyObject("#'apply"), applyBefore = applyObject(':before'), applyAfter = applyObject(':after');
vm.evaluate(`(defparameter *thread-apply-queue*
  (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%THREAD"))) (values)`, {eof: true});
for (const applySelected of applyTargets)
for (const fixed of [true, false]) for (const repair of [false, true]) {
  vm.evaluate(`(defparameter *thread-apply-arguments* (cons :before ${repair ? ':dotted' : 'nil'}))
    (defparameter *thread-apply-mutate* (miricl-compiler:compile-native '(lambda ()
      ${repair ? '(setf (cdr *thread-apply-arguments*) nil)' : '(setf (car *thread-apply-arguments*) :after)'})))
    (defparameter *thread-apply-noop* (miricl-compiler:compile-native '(lambda ()))) (values)`, {eof: true});
  const arguments_ = applyObject('*thread-apply-arguments*');
  const packet = vm.vm['native-arguments'](2);
  vm.vm['native-argument-set'](packet, 0, applySelected);
  vm.vm['native-argument-set'](packet, 1, arguments_);
  for (const ordinary of [true, false]) {
    // Restore the same input before each route. Prevalidation repair follows
    // the fixed route's first observation; CAR mutation follows validation.
    vm.evaluate(`(setf (car *thread-apply-arguments*) :before
      (cdr *thread-apply-arguments*) ${repair ? ':dotted' : 'nil'})
      (funcall *thread-apply-queue* 16 nil nil)
      (funcall *thread-apply-queue* 12 (funcall *thread-apply-queue* 0 nil nil) *thread-apply-mutate*)
      ${Array(repair ? (fixed ? 1 : 0) : (fixed ? 2 : 1)).fill(`(funcall *thread-apply-queue* 12
        (funcall *thread-apply-queue* 0 nil nil) *thread-apply-noop*)`).join('\n')}
      (funcall *thread-apply-queue* 13 0 nil) (values)`, {eof: true});
    vm.vm['thread-enter'](0); vm.vm['thread-main-state'](0);
    vm.vm['thread-active'].value = 1;
    let result;
    try {
      result = ordinary ? (fixed ? vm.vm['native-call2'](applyBuiltin, applySelected, arguments_)
        : vm.vm['native-call'](null, applyBuiltin, packet))
        : vm.vm['native-apply-list'](applySelected, arguments_, fixed ? 3 : 0);
    } finally {
      vm.vm['thread-active'].value = 0;
      vm.vm['thread-save'](); vm.vm['thread-main-state'](1);
    }
    if (!fixed) result = vm.javascript.link(0, result);
    assert.equal(result, !repair && fixed ? applyAfter : applyBefore,
      'Queued work repairs before validation and distinguishes shared values from captured packet values.');
    assert.equal(vm.vm['native-argument'](arguments_, 0), repair ? applyBefore : applyAfter,
      'The queued mutation ran during the observed application.');
  }
}
console.log('PASS APPLY queued list validation, shared selection and packet snapshots');

vm.evaluate(`(defparameter *thread-list-eq*
    (symbol-function (miricl-compiler::compiler-primitive 10 1 "%LIST-EQ")))
  (defparameter *thread-list-left* (list :head :middle :left-before))
  (defparameter *thread-list-right* (list :head :middle :right-before))
  (defvar *thread-list-mutated* nil)
  (defparameter *thread-list-mutate*
    (lambda ()
      (setf (caddr *thread-list-left*) :after (caddr *thread-list-right*) :after
            (cadr *thread-list-left*) :replayed (cddr *thread-list-left*) :stale)
      (setq *thread-list-mutated* t)
      (values :callback :noise))) (values)`, {eof: true});
const listEq = applyObject('*thread-list-eq*'), listTrue = applyObject('t');
const listLeft = applyObject('*thread-list-left*'), listRight = applyObject('*thread-list-right*');
// Keep work pending past the first compared pair. The mutation changes the
// second pair already compared and the third pair still to be read.
vm.evaluate(`(funcall *thread-apply-queue* 16 nil nil)
  (funcall *thread-apply-queue* 12 (funcall *thread-apply-queue* 0 nil nil) *thread-list-mutate*)
  (dotimes (i 2)
    (funcall *thread-apply-queue* 12 (funcall *thread-apply-queue* 0 nil nil) (lambda ())))
  (funcall *thread-apply-queue* 13 0 nil) (values)`, {eof: true});
vm.vm['thread-enter'](0); vm.vm['thread-main-state'](0);
vm.vm['thread-active'].value = 1;
try {
  assert.equal(vm.vm['native-call2'](listEq, listLeft, listRight), listTrue);
} finally {
  vm.vm['thread-active'].value = 0;
  vm.vm['thread-save'](); vm.vm['thread-main-state'](1);
}
assert.equal(vm.evaluate(`(and *thread-list-mutated*
  (handler-case (progn (funcall *thread-list-eq* '(:same . :left) '(:same . :right)) nil)
    (type-error (condition) (eq (type-error-datum condition) :left)))
  (handler-case (progn (funcall *thread-list-eq* '(:same :next) '(:same . :right)) nil)
    (type-error (condition) (eq (type-error-datum condition) :right)))
  (equal (multiple-value-list (funcall *thread-list-eq* '(:same :after) '(:same :after))) '(t)))`,
  {eof: true}).output, 'T\n', 'List comparison resumes on live tails and recovers from ordered type errors.');
console.log('PASS list comparison retains progress across queued mutation and recovers from dotted input');

// A symbol's binding is selected after the application checkpoint. Keep the
// symbol and operand fixed while queued work replaces the actual callee.
vm.evaluate(`(defvar *thread-fixed-fired* nil)
  (defparameter *thread-fixed-before*
    (miricl-compiler:compile-native '(lambda (x) (declare (ignore x)) (values :before nil :tail))))
  (defparameter *thread-fixed-after*
    (miricl-compiler:compile-native '(lambda (x) (declare (ignore x)) (values :after nil :tail))))
  (defparameter *thread-fixed-rebind*
    (miricl-compiler:compile-native '(lambda ()
      (setq *thread-fixed-fired* t)
      (setf (symbol-function 'thread-fixed-designator) *thread-fixed-after*)
      (values :callback :noise)))) (values)`, {eof: true});
const fixedDesignator = applyObject("'thread-fixed-designator");
const fixedAfter = applyObject(':after'), fixedExpected = applyObject("'(:after nil :tail)");
const fixedEqual = applyObject("#'equal"), fixedTrue = applyObject('t');
for (const route of ['full', 'primary', 'scalar']) {
  const primary = route !== 'full';
  vm.evaluate(`(setq *thread-fixed-fired* nil)
    (setf (symbol-function 'thread-fixed-designator) *thread-fixed-before*)
    (funcall *thread-apply-queue* 16 nil nil)
    (funcall *thread-apply-queue* 12 (funcall *thread-apply-queue* 0 nil nil) *thread-fixed-rebind*)
    (funcall *thread-apply-queue* 13 0 nil) (values)`, {eof: true});
  vm.vm['thread-enter'](0); vm.vm['thread-main-state'](0);
  vm.vm['thread-active'].value = 1;
  let result;
  try {
    result = route === 'scalar' ? vm.vm['native-call1'](fixedDesignator, null)
      : vm.vm['native-fixed-call'](fixedDesignator, null, null, null, null, primary ? 9 : 1);
  } finally {
    vm.vm['thread-active'].value = 0;
    vm.vm['thread-save'](); vm.vm['thread-main-state'](1);
  }
  assert.equal(primary ? result : vm.vm['native-call2'](fixedEqual, result, fixedExpected),
    primary ? fixedAfter : fixedTrue,
    'The application selects the rebound native callee and preserves its demanded values.');
  assert.equal(vm.evaluate('(and *thread-fixed-fired* (eq (symbol-function \'thread-fixed-designator) *thread-fixed-after*))').output,
    'T\n', 'The queued rebinding actually ran during the observed application.');
}
vm.evaluate("(fmakunbound 'thread-fixed-designator) (values)");
console.log('PASS fixed and scalar symbol calls select live bindings after queued work and preserve values');
// Fixed array access observes adjustment before reading dimensions or backing.
vm.evaluate(`(defvar *thread-rank1-fired* nil)
  (defvar *thread-rank1-base* nil)
  (defparameter *thread-rank1-adjust*
    (miricl-compiler:compile-native '(lambda ()
      (adjust-array *thread-rank1-base* 4 :initial-contents '(91 92 93 94))
      (setq *thread-rank1-fired* t)
      (values :callback :noise)))) (values)`, {eof: true});
const rank1Aref = applyObject("#'aref"), rank1Setter = applyObject("(fdefinition '(setf aref))");
const rank1Index = applyObject('3'), rank1Value = applyObject('-9');
for (const [access, expected] of [
  [array => vm.vm['native-call2'](rank1Aref, array, rank1Index), 94n],
  [array => vm.vm['native-aref-i31-index'](array, 3), 94n],
  [array => vm.vm['native-call3'](rank1Setter, rank1Value, array, rank1Index), -9n],
]) {
  vm.evaluate(`(setq *thread-rank1-fired* nil
      *thread-rank1-base* (make-array 2 :element-type 'fixnum :adjustable t :initial-contents '(10 20)))
    (values)`, {eof: true});
  const rank1Array = applyObject('*thread-rank1-base*');
  vm.evaluate(`(funcall *thread-apply-queue* 16 nil nil)
    (funcall *thread-apply-queue* 12 (funcall *thread-apply-queue* 0 nil nil) *thread-rank1-adjust*)
    (funcall *thread-apply-queue* 13 0 nil) (values)`, {eof: true});
  vm.vm['thread-enter'](0); vm.vm['thread-main-state'](0);
  vm.vm['thread-active'].value = 1;
  let result;
  try {
    result = access(rank1Array);
  } finally {
    vm.vm['thread-active'].value = 0;
    vm.vm['thread-save'](); vm.vm['thread-main-state'](1);
  }
  assert.equal(vm.javascript.integer(result), expected,
    'The fixed array operation uses the dimension and backing installed at its checkpoint.');
  assert.equal(vm.javascript.integer(vm.vm['native-call2'](rank1Aref, rank1Array, rank1Index)),
    expected, 'The current backing retains the expected element.');
  assert.equal(vm.evaluate(`(and *thread-rank1-fired*
    (= (array-dimension *thread-rank1-base* 0) 4))`).output, 'T\n',
    'The queued adjustment fired before the observed operation.');
}
console.log('PASS fixed rank-one array access observes queued dimension and backing replacement');
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
for (let transfer = 0; transfer < 8; transfer++) assert.deepEqual(await vm.readFile(transferPath), transferBytes);
await vm.evaluateAsync(`(defparameter *transfer-worker*
  (miricl-threads:make-thread (lambda () (sleep 0.01) 42))) (values)`);
clockAdvance += 1000;
assert.deepEqual(await vm.readFile(transferPath), transferBytes);
assert.equal((await vm.evaluateAsync('(miricl-threads:join-thread *transfer-worker*)')).output, '42\n');
clockAdvance += 1000;
assert.deepEqual(await vm.readFile(transferPath), transferBytes);
console.log('PASS file transfers after async evaluation and background work');

// Native installation reenters Wasm synchronously to transfer computed integer
// bytes. Force cooperative slices to expire, including the rejection path,
// then check that ordinary scheduling and multiple values still work.
const transferHostService = vm.hostService;
vm.hostService = (operation, argument) => originalHostService(operation, argument)
  + (operation === 1 ? (clockAdvance += 1000) : 0);
try {
  assert.equal((await vm.evaluateAsync(`(let ((miricl-compiler:*compile-evaluations* nil))
    (let* ((unit (miricl-compiler:compile-to-hir '(lambda (x) (values (+ x 2) nil :compiled))))
           (worker (miricl-threads:make-thread (lambda () (miricl-threads:thread-yield) :ran))))
      (multiple-value-bind (bytes literals) (miricl-compiler:emit-native-module unit)
        (let* ((computed (map 'vector (lambda (byte) (+ (ash 1 40) (- byte (ash 1 40)))) bytes))
               (rejected (handler-case
                   (progn (miricl-compiler::compiler-primitive 17 #(0) #() nil) nil)
                 (error (condition) (plusp (length (princ-to-string condition)))))))
          (and rejected
            (loop repeat 8 always
              (equal (multiple-value-list
                (funcall (miricl-compiler::compiler-primitive 17 (copy-seq computed) literals nil) 40))
                '(42 nil :compiled)))
            (progn (miricl-threads:thread-yield) t)
            (eq (miricl-threads:join-thread worker) :ran))))))`, {eof: true})).output, 'T\n');
} finally {
  vm.hostService = transferHostService;
}
console.log('PASS native installation and rejection preserve synchronous reentry and later scheduling');

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
      (values (+ (js:from-js value) 2) :callback :extra)))
    (js:callback (lambda () (error "External callback failure."))))
  (defparameter *callback-cleanup-worker*
    (miricl-threads:make-thread (lambda ()
      (let ((*print-base* 16))
        (catch 'callback-cleanup-exit
          (unwind-protect (throw 'callback-cleanup-exit (values 42 nil :third :fourth nil))
            (mark-thread-cleanup) (sleep 0.15)))))))
  (values)`);
const callbackDeadline = performance.now() + 5000;
while (!callbackFixture.cleanupEntered) {
  assert.ok(performance.now() < callbackDeadline, 'Worker did not enter cleanup.');
  await new Promise(resolve => setTimeout(resolve, 1));
}
clockAdvance += 1000;
for (let transfer = 0; transfer < 8; transfer++) assert.deepEqual(await vm.readFile(transferPath), transferBytes);
assert.equal(callbackFixture.good(40), 42);
assert.throws(() => callbackFixture.bad(), /External callback failure/);
assert.equal(callbackFixture.good(41), 43);
assert.equal((await vm.evaluateAsync('(multiple-value-list (miricl-threads:join-thread *callback-cleanup-worker*))')).output,
  '(42 NIL :THIRD :FOURTH NIL)\n');
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
