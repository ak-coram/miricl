#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const binary = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const a = await Miricl.instantiate(binary);
const b = await Miricl.instantiate(binary);
const check = source => assert.equal(a.evaluate(source).output, 'T\n', source);
assert.notEqual(a.evaluate('(write-to-string *random-state*)').output,
  b.evaluate('(write-to-string *random-state*)').output, 'Host entropy must seed separate instances');
// Published xoshiro128** transition from the state (1, 2, 3, 4).
assert.equal(a.evaluate(`(let ((s #S(random-state 1 2 3 4)))
  (random 1 s) (write-to-string s))`).output, '"#S(RANDOM-STATE #x7 #x0 #x402 #x3000)"\n');
check(`(let* ((s (make-random-state t)) (copy (make-random-state s)))
  (loop for bound in '(1 2 3 1024 1000000000 4294967296 123456789012345678901234567890 1.0 1d0)
    always (loop repeat 100 always (eql (random bound s) (random bound copy)))))`);
check(`(let* ((s (make-random-state t))
              (copy (let ((*read-eval* nil)) (read-from-string (write-to-string s :readably t)))))
  (loop repeat 100 always (= (random 1000000000 s) (random 1000000000 copy))))`);
check(`(let ((s #S(random-state 1 2 3 4)) (bins (make-array 10 :initial-element 0)))
  (dotimes (i 100000) (incf (aref bins (random 10 s))))
  (every (lambda (n) (< 9000 n 11000)) bins))`);
check(`(let ((s #S(random-state 1 2 3 4)))
  (loop for limit in (list least-positive-single-float least-positive-double-float
                           most-positive-single-float most-positive-double-float)
    always (loop repeat 100 for r = (random limit s) always (and (<= 0 r) (< r limit)))))`);
check(`(handler-case (random 1 nil) (type-error () t))`);
check(`(handler-case (random 1/2) (type-error () t))`);
check(`(handler-case (read-from-string "#S(random-state 0 0 0 0)") (reader-error () t))`);
console.log('Random: host seeding, generator transition, state copies, readable syntax, bounds, and distribution pass.');
