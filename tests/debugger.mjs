// Interpreter debugging through Lisp hooks and independent asynchronous stops.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const bytes = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const events = [];
const waiters = new Set();
const output = [];
const vm = await Miricl.instantiate(bytes, {onOutput: text => output.push(text), onDebugger: event => {
  events.push(event);
  if (process.env.MIRICL_DEBUG_TEST_LOG) console.error(event.type, event.stop?.threadName, event.stop?.condition.message, event.stop?.level);
  for (const waiter of waiters) waiter.check();
}});
function eventAfter(start, predicate) {
  return new Promise((resolve, reject) => {
    const timeoutError = new Error('Debugger event timed out.');
    const waiter = {check() {
      const event = events.slice(start).find(predicate);
      if (event) {clearTimeout(timer); waiters.delete(waiter); resolve(event);}
    }};
    const timer = setTimeout(() => {waiters.delete(waiter); reject(timeoutError);}, 15000);
    waiters.add(waiter); waiter.check();
  });
}
const stopped = (start, predicate = () => true) => eventAfter(start, event => event.type === 'stopped' && predicate(event.stop)).then(event => event.stop);
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);
const frame = (stop, name) => stop.frames.find(frame => frame.inspectable && frame.name.endsWith(`::${name}`));
try {
  check(`(null (miricl-debugger:frames))`);
  check(`(progn
    (defvar *debug-visits* 0)
    (defun debug-quiet (x) (+ x 1))
    (let ((miricl-debugger:*step-hook* (lambda (frame) (declare (ignore frame)) (incf *debug-visits*))))
      (debug-quiet 40)
      (locally (declare (optimize (debug 0))) (debug-quiet 1))
      (locally (declare (optimize (debug 1))) (debug-quiet 1))
      (locally (declare (optimize (debug 2))) (debug-quiet 1)))
    (zerop *debug-visits*))`);
  check(`(progn
    (defvar *debug-old-frame* nil)
    (defun debug-hooked (x)
      (declare (optimize (debug 3)))
      (let ((y (+ x 2))) (values x y)))
    (let ((miricl-debugger:*step-hook*
            (lambda (frame)
              (when (equal (miricl-debugger:frame-form frame) '(values x y))
                (setq *debug-old-frame* frame)
                (unless (equal (miricl-debugger:frame-locals frame) '((y . 42) (x . 40))) (error "Wrong locals."))
                (miricl-debugger:eval-in-frame '(setq x 41) frame)))))
      (and (equal (multiple-value-list (debug-hooked 40)) '(41 42))
           (null (miricl-debugger:frames))
           (handler-case (progn (miricl-debugger:frame-form *debug-old-frame*) nil) (control-error () t)))))`);
  check(`(let ((seen nil) (*debugger-hook* (lambda (condition hook)
      (declare (ignore condition hook)) (invoke-restart 'continue))))
    (declare (ignore seen))
    (equal (multiple-value-list (locally (declare (optimize (debug 3)))
      (cerror "Continue" "Handled by Lisp hook") (values 4 5))) '(4 5)))`);
  check(`(and (null (catch 'exit (locally (declare (optimize (debug 3))) (throw 'exit nil))))
              (null (miricl-debugger:frames)))`);

  evaluate(`(defun debug-leaf (x)
      (declare (optimize (debug 3)))
      (let ((y (+ x 2)))
        (restart-case (error "Repair this value")
          (use-value (replacement) :report "Use a replacement value." (values replacement y)))))
    (defun debug-root (x) (declare (optimize (debug 3))) (debug-leaf x))`);
  let start = events.length;
  const execution = vm.evaluateAsync('(progn (print :before-debug) (values)) (debug-root 40)');
  let stop = await stopped(start);
  assert.equal(stop.threadId, 0);
  assert.equal(stop.level, 1);
  assert.equal(stop.condition.message, 'Repair this value');
  assert.match(stop.backtrace, /\n   0: COMMON-LISP:ERROR\n/);
  assert.ok(frame(stop, 'DEBUG-LEAF'));
  assert.ok(frame(stop, 'DEBUG-ROOT'));
  assert.equal(stop.frames[0].inspectable, false);
  assert.equal(stop.restarts[0].description, 'Use a replacement value.');
  await assert.rejects(stop.resume(), /restart/);
  await assert.rejects(stop.evaluate(0, 'x'), /no interpreter/);
  const old = stop;
  start = events.length;
  assert.deepEqual(await stop.evaluate(frame(stop, 'DEBUG-LEAF').id, '(format t "inspected") (values x y)'),
    {values: ['NIL'], output: 'inspected'}); // One expression is read, as in READ-FROM-STRING.
  stop = await stopped(start, stop => stop.sessionId === old.sessionId);
  await assert.rejects(old.evaluate(frame(old, 'DEBUG-LEAF').id, 'x'), /no longer active/);
  start = events.length;
  assert.deepEqual(await stop.evaluate(frame(stop, 'DEBUG-LEAF').id, '(values (incf x) y)'), {values: ['41', '42'], output: ''});
  stop = await stopped(start, stop => stop.sessionId === old.sessionId);
  assert.equal((await stop.locals(frame(stop, 'DEBUG-LEAF').id)).find(binding => binding.name === 'X').value, '41');
  await stop.invokeRestart(stop.restarts[0].id, {argumentsSource: '(list x)'});
  const completed = await execution;
  assert.equal(output.join('') + completed.output, '\n:BEFORE-DEBUG 41\n42\n');
  assert.equal(vm.debuggerSessions().length, 0);

  // Errors in a command are reported at the same debugger level.
  start = events.length;
  const commandErrors = vm.evaluateAsync('(debug-root 7)');
  stop = await stopped(start);
  start = events.length;
  await assert.rejects(stop.evaluate(frame(stop, 'DEBUG-LEAF').id, '(error "command failure")'), /command failure/);
  stop = await stopped(start);
  assert.equal(stop.level, 1);
  assert.equal(stop.result.error, 'command failure');
  const commandSession = stop.sessionId;
  for (const argumentsSource of ['10', "'(1 . 2)", "'#1=(1 . #1#)"]) {
    start = events.length;
    await assert.rejects(stop.invokeRestart(0, {argumentsSource}), /Restart arguments must evaluate to a proper list/);
    stop = await stopped(start);
    assert.equal(stop.sessionId, commandSession);
    assert.equal(stop.level, 1);
    assert.match(stop.result.error, /For one argument, use \(list value\)/);
  }
  await stop.invokeRestart(0, {argumentsSource: '(list 9)'});
  assert.equal((await commandErrors).output, '9\n9\n');

  // Explicit debugger entry during frame evaluation nests on the same thread.
  start = events.length;
  const nestedExecution = vm.evaluateAsync('(debug-root 10)');
  const parent = await stopped(start);
  start = events.length;
  const nestedEvaluation = parent.evaluate(frame(parent, 'DEBUG-LEAF').id,
    '(restart-case (invoke-debugger (make-condition \'simple-error :format-control "nested")) (nested-continue () 123))');
  const nested = await stopped(start, stop => stop.level === 2);
  assert.equal(nested.threadId, parent.threadId);
  assert.equal(nested.parentSessionId, parent.sessionId);
  await assert.rejects(parent.invokeRestart(0), /busy|nested/);
  start = events.length;
  await nested.invokeRestart(nested.restarts.find(restart => restart.name === 'NESTED-CONTINUE').id);
  assert.deepEqual(await nestedEvaluation, {values: ['123'], output: ''});
  stop = await stopped(start, stop => stop.sessionId === parent.sessionId);
  assert.deepEqual(stop.frames.map(frame => frame.name), parent.frames.map(frame => frame.name));
  await stop.invokeRestart(0, {argumentsSource: '(list 12)'});
  assert.equal((await nestedExecution).output, '12\n12\n');

  // Independent errors park their own threads; the main thread can join both.
  evaluate(`(defun debug-worker (value) (declare (optimize (debug 3)))
    (restart-case (error "Worker ~D" value) (use-value (replacement) replacement)))`);
  start = events.length;
  const workers = vm.evaluateAsync(`(let ((one (miricl-threads:make-thread (lambda () (debug-worker 1)) :name "Worker one"))
      (two (miricl-threads:make-thread (lambda () (debug-worker 2)) :name "Worker two")))
    (list (miricl-threads:join-thread one) (miricl-threads:join-thread two)))`);
  const one = await stopped(start, stop => stop.threadName === 'Worker one');
  const two = await stopped(start, stop => stop.threadName === 'Worker two');
  assert.notEqual(one.threadId, two.threadId);
  assert.notEqual(one.sessionId, two.sessionId);
  assert.equal(one.level, 1); assert.equal(two.level, 1);
  assert.equal(vm.debuggerSessions().length, 2);
  await two.invokeRestart(0, {argumentsSource: '(list 22)'});
  assert.equal(vm.debuggerSessions().length, 1);
  start = events.length;
  assert.deepEqual(await one.evaluate(frame(one, 'DEBUG-WORKER').id, 'value'), {values: ['1'], output: ''});
  stop = await stopped(start, stop => stop.threadId === one.threadId);
  await stop.invokeRestart(0, {argumentsSource: '(list 11)'});
  assert.equal((await workers).output, '(11 22)\n');

  // Function breakpoints and stepping use the same stop API.
  evaluate(`(defun debug-step (x) (declare (optimize (debug 3))) (let ((y (+ x 1))) (* y 2)))
    (miricl-debugger:set-breakpoint 'debug-step)`);
  start = events.length;
  const stepping = vm.evaluateAsync('(debug-step 20)');
  stop = await stopped(start);
  assert.equal(stop.canStep, true);
  start = events.length;
  await stop.step('into');
  stop = await stopped(start);
  assert.equal(stop.canStep, true);
  start = events.length;
  await stop.step('into');
  stop = await stopped(start);
  start = events.length;
  await stop.step('over');
  stop = await stopped(start);
  await stop.resume();
  assert.equal((await stepping).output, '42\n');
  evaluate('(miricl-debugger:clear-breakpoints)');
  start = events.length;
  const stepMacro = vm.evaluateAsync('(step (+ 20 22))');
  stop = await stopped(start);
  assert.ok(stop.frames.some(frame => frame.inspectable));
  await stop.resume();
  assert.equal((await stepMacro).output, '42\n');
  assert.equal(vm.debuggerSessions().length, 0);
  console.log('Debugger: default policy, live forms/locals, frame evaluation, restarts, nesting, concurrent threads, breakpoints, stepping and stale handles pass.');
} finally {
  vm.close();
}
