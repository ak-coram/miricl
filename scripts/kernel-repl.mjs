// Persistent host/guest REPLs with file queues, so tools need no interactive
// stdin writes. Each request is ordinary Lisp source; sessions keep definitions.
import fs from 'node:fs/promises';
import {createWriteStream} from 'node:fs';
import path from 'node:path';
import {spawn} from 'node:child_process';
import {setTimeout as delay} from 'node:timers/promises';
import {Miricl} from './miricl.mjs';

const [action, name, ...args] = process.argv.slice(2);
if (!/^[a-z0-9-]+$/.test(name ?? '')) throw Error('Supply a session name: lowercase letters, digits, hyphens');
// Workspace files are shared by sandboxed clients and the approved host
// process; their private /tmp mounts need not refer to the same directory.
const directory = path.resolve('build', 'kernel-repl', name);
await fs.mkdir(directory, {recursive: true});
const quoteLisp = value => '"' + value.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';

if (action === 'submit') {
  const id = `${Date.now()}-${process.pid}`;
  const source = await fs.readFile(args[0], 'utf8');
  const temporary = path.join(directory, `${id}.tmp`);
  await fs.writeFile(temporary, source);
  await fs.rename(temporary, path.join(directory, `${id}.lisp`));
  console.log(id);
} else if (action === 'result') {
  const id = args[0];
  if (!/^\d+-\d+$/.test(id ?? '')) throw Error('Invalid request id');
  try {
    console.log(await fs.readFile(path.join(directory, `${id}.result`), 'utf8'));
  } catch (error) {
    if (error.code !== 'ENOENT') throw error;
    console.log('Request is pending.');
  }
} else if (action === 'start') {
  const [kind, imagePath] = args;
  let host, vm;
  const serverLock = await fs.open(path.join(directory, 'server.lock'), 'wx');
  await serverLock.writeFile(String(process.pid));
  const log = createWriteStream(path.join(directory, 'server.log'), {flags: 'a'});
  const report = data => { process.stdout.write(data); log.write(data); };
  if (kind === 'host') {
    host = spawn(process.env.SBCL ?? 'sbcl', ['--noinform', '--disable-debugger',
      '--load', process.env.QUICKLISP_SETUP ?? '/home/ak/quicklisp/setup.lisp',
      '--eval', '(require :asdf)', '--eval', '(asdf:load-asd (truename "miricl.asd"))',
      '--eval', '(ql:quickload :miricl)'], {stdio: ['pipe', 'pipe', 'pipe']});
    host.stdout.on('data', report);
    host.stderr.on('data', report);
    host.on('exit', code => { console.error(`SBCL exited: ${code}`); process.exitCode = code ?? 1; });
  } else if (kind === 'wasm') {
    if (!imagePath) throw Error('Supply the cold image path');
    vm = await Miricl.instantiate(await fs.readFile(imagePath), {directory: process.cwd()});
    vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  } else throw Error('Session kind must be host or wasm');
  console.log(`Queue REPL ready: ${name} (${kind})`);
  try {
    while (!host || host.exitCode === null) {
      const requests = (await fs.readdir(directory)).filter(file => file.endsWith('.lisp')).sort();
      for (const request of requests) {
        const input = path.join(directory, request);
        const output = input.replace(/\.lisp$/, '.result');
        try { await fs.access(output); continue; } catch (error) { if (error.code !== 'ENOENT') throw error; }
        if (host) {
          // WITH-OPEN-FILE closes/flushes the answer before the server advances.
          // A condition aborts only this request, keeping the REPL available.
          host.stdin.write(`(with-open-file (out ${quoteLisp(output + '.tmp')} :direction :output :if-exists :supersede) (let ((*standard-output* out) (*error-output* out) (*trace-output* out) (*package* (find-package :cl-user))) (handler-case (load ${quoteLisp(input)} :print t) (error (e) (format out "~%REQUEST ERROR: ~A~%" e))))) (rename-file ${quoteLisp(output + '.tmp')} ${quoteLisp(output)})\n`);
          while (host.exitCode === null) {
            try { await fs.access(output); break; } catch (error) { if (error.code !== 'ENOENT') throw error; }
            await delay(50);
          }
        } else {
          try {
            const result = await vm.evaluateAsync(await fs.readFile(input, 'utf8'));
            await fs.writeFile(output, result.output);
          } catch (error) { await fs.writeFile(output, `REQUEST ERROR: ${error.stack}\n`); }
        }
        console.log(`Finished ${request}`);
      }
      await delay(100);
    }
  } finally {
    vm?.close();
    log.end();
    await serverLock.close();
    await fs.unlink(path.join(directory, 'server.lock'));
  }
} else throw Error('Usage: kernel-repl.mjs start NAME host|wasm [IMAGE], submit NAME FILE, result NAME REQUEST');
