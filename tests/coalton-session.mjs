import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCoalton, coaltonInput} from '../scripts/coalton.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const snapshot = process.argv.includes('--snapshot');
const core = snapshot ? null : await fs.readFile(process.argv[3] ?? 'build/coalton/coalton.wasm');
assert.throws(() => coaltonInput('42', 'invalid'), /mode/);
const start = performance.now();
const session = await createCoalton(image, core);
console.log(`Coalton ${snapshot ? 'snapshot restore' : 'fresh native bundle'} startup: ${Math.round(performance.now() - start)} ms`);
try {
  const check = async (source, expected, mode = 'lisp') => {
    const result = await session.evaluate(source, {mode});
    assert.equal(result.output.trim(), expected, source);
  };
  await check('(+ 20 22)', '42', 'expression');
  await check('(cl:+ 10 11) (cl:+ 20 22)', '21\n42');
  await check('(cl:values 20 22) (cl:values) (cl:values 42)', '20\n22\n42');
  await check('(cl:progn (cl:write-string "é\\\\path \\"quoted\\"𝄞") (cl:values))', 'é\\path "quoted"𝄞');
  await check('(cl:in-package :cl-user) (cl:+ 20 22)', '#<PACKAGE "COMMON-LISP-USER">\n42');
  await check(`(cl:progn (cl:defparameter cl-user::form :user-form)
    (cl:defparameter cl-user::eof :user-eof) (cl:defparameter cl-user::value :user-value)
    (cl:values))`, '');
  await check('(cl:values cl-user::form cl-user::eof cl-user::value)', ':USER-FORM\n:USER-EOF\n:USER-VALUE');
  await check('(+ 20 22)', '42', 'expression');
  await session.evaluate('(define (tour-square x) (* x x))', {mode: 'definitions'});
  await check('(tour-square 12)', '144', 'expression');
  await session.evaluate(`(declare tour-factorial (Integer -> Integer))
    (define (tour-factorial n) (if (== n 0) 1 (* n (tour-factorial (- n 1)))))`, {mode: 'definitions'});
  await check('(tour-factorial 30)', '265252859812191058636308480000000', 'expression');
  await assert.rejects(() => session.evaluate('(+ 1 "text")', {mode: 'expression'}));
  await check('(tour-square 7)', '49', 'expression');
  await check('(map (fn (x) (+ x 1)) (make-list 1 2 3))', '(2 3 4)', 'expression');
  await check(`(cl:and (asdf:find-system "coalton" nil)
      (cl:plusp (miricl-compiler::compiler-primitive 26
        (cl:symbol-function 'coalton-impl/entry:compile-coalton-toplevel))))`, 'COMMON-LISP:T');
  console.log('Coalton session: real native compiler, persistent definitions, recursion, bignums, type errors and recovery passed.');
} finally {session.close();}
