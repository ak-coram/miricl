import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {setTimeout as delay} from 'node:timers/promises';

const image = process.argv[2] ?? 'miricl.wasm';
for (const command of ['scripts/repl.mjs', ...(process.argv[3] ? [process.argv[3]] : [])]) {
  const child = spawn(process.execPath, [command, image], {stdio: ['pipe', 'pipe', 'pipe']});
  let output = '', diagnostics = '';
  child.stdout.on('data', chunk => {output += chunk;});
  child.stderr.on('data', chunk => {diagnostics += chunk;});
  const exited = new Promise(resolve => child.on('exit', (code, signal) => resolve({code, signal})));
  async function waitFor(predicate) {
    const deadline = Date.now() + 15000;
    while (!predicate()) {
      if (child.exitCode !== null || child.signalCode !== null || Date.now() >= deadline)
        assert.fail(`Terminal debugger did not respond.\n${output}\n${diagnostics}`);
      await delay(10);
    }
  }
  const send = line => child.stdin.write(line + '\n');
  try {
    send('(defun terminal-debug (x) (declare (optimize (debug 3))) (cerror "Continue" "terminal condition") (+ x 2)) (terminal-debug 40)');
    await waitFor(() => output.includes('debug[0:1]>'));
    assert.match(output, /\n   0: COMMON-LISP:CERROR\n   1: COMMON-LISP-USER::TERMINAL-DEBUG/);
    send('locals');
    await waitFor(() => output.includes('X = 40'));
    send('eval (incf x)');
    await waitFor(() => output.includes('41\n'));
    send('0');
    await waitFor(() => output.includes('\n43\n'));
    send(`(defun terminal-worker (value) (declare (optimize (debug 3)))
      (restart-case (error "Worker ~D" value) (use-value (replacement) replacement)))`.replaceAll('\n', ' '));
    send(`(progn
      (defparameter *worker-one* (miricl-threads:make-thread (lambda () (terminal-worker 1)) :name "Terminal one"))
      (defparameter *worker-two* (miricl-threads:make-thread (lambda () (terminal-worker 2)) :name "Terminal two"))
      (values))`.replaceAll('\n', ' '));
    await waitFor(() => output.includes('Debugger: Terminal one') && output.includes('Debugger: Terminal two'));
    const one = Number(/Debugger: Terminal one .*session (\d+)/.exec(output)[1]);
    const two = Number(/Debugger: Terminal two .*session (\d+)/.exec(output)[1]);
    assert.notEqual(one, two);
    send('sessions');
    await waitFor(() => /Terminal one \(#\d+\), level 1\n/.test(output));
    send(`session ${two}`); send('restart 0 (list 22)');
    await waitFor(() => output.includes(`[Debugger session ${two} exited]`));
    send(`session ${one}`); send('restart 0 (list 11)');
    send('(list (miricl-threads:join-thread *worker-one*) (miricl-threads:join-thread *worker-two*))');
    await waitFor(() => output.includes('(11 22)\n'));
    send(':quit');
    child.stdin.end();
    assert.deepEqual(await exited, {code: 0, signal: null});
    assert.equal(diagnostics, '');
    console.log(`Terminal debugger (${command}): locals, evaluation, numbered restarts, two threads and session selection pass.`);
  } finally {
    if (child.exitCode === null && child.signalCode === null) child.kill('SIGKILL');
    await exited;
  }
}
