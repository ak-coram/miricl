import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCoalton} from '../scripts/coalton.mjs';
import {examples} from '../coalton/examples.mjs';

const session = await createCoalton(await fs.readFile(process.argv[2] ?? 'miricl.wasm'),
  process.argv.includes('--snapshot') ? null : await fs.readFile(process.argv[3] ?? 'build/coalton/coalton.wasm'), {
    replPackage: 'COMMON-LISP-USER', progress: message => console.log(`Coalton REPL session: ${message}`)});
try {
  const check = async (source, expected, mode = 'lisp') =>
    assert.equal((await session.evaluate(source, {mode})).output.trim(), expected, source);
  await check('(+ 20 22)', '42');
  await check('(defun repl-square (x) (* x x)) (repl-square 12)', 'REPL-SQUARE\n144');
  await check('(values 20 22) (values) (repl-square 7)', '20\n22\n49');
  await session.evaluate('(defpackage :repl-test (:use :cl)) (in-package :repl-test)');
  await check('(defparameter *counter* 40) (incf *counter* 2)', '*COUNTER*\n42');
  await assert.rejects(() => session.evaluate('(in-package :cl-user) (error "expected error")'));
  await check('(repl-square 8)', '64');
  await session.evaluate('(define (repl-add2 x) (+ x 2))', {mode: 'definitions'});
  await check('(repl-add2 40)', '42', 'expression');
  await check('(repl-square 9)', '81');
  const failures = [];
  for (const example of examples) {
    try {
      const result = await session.evaluate(example.code, {mode: example.mode});
      assert.ok(result.output.includes(example.expected), `${example.id}: ${result.output}`);
      for (const expected of example.checks ?? []) assert.ok(result.output.includes(expected), example.id);
      console.log(`PASS Coalton documentation example: ${example.title}`);
    } catch (error) {
      failures.push({example: example.id, message: error.message, output: error.output});
      await session.evaluate('(cl:in-package :cl-user)');
    }
  }
  assert.deepEqual(failures, []);
  await check('(+ 20 22)', '42');
  await assert.rejects(() => session.evaluate('(+ 1 "text")', {mode: 'expression'}));
  await check('(repl-square 10)', '100');
  console.log('Lisp/Coalton REPL session: persistent packages, definitions, multiple values, error recovery and all documentation examples passed.');
} finally {session.close();}
