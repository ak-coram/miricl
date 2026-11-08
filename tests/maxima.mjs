// The shipped manual examples execute against the optimized dynamic image.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createMaxima} from '../scripts/maxima.mjs';
import {maximaProbeFiles} from '../scripts/probe-maxima.mjs';
import {examples} from '../maxima/examples.mjs';
const image = process.argv[2] ?? 'miricl.wasm';
const core = process.argv[3] ?? 'build/maxima/maxima-optimized.wasm';
const session = await createMaxima(fs.readFileSync(image), fs.readFileSync(core), maximaProbeFiles(true));
try {
  assert.equal(session.vm.evaluate(`(and (compiled-function-p #'maxima::meval)
    (boundp 'cl-user::*maxima-build-time*))`).output, 'T\n');
  assert.doesNotMatch(session.startup, /unable to set up the help system/);
  for (const example of examples) {
    const result = await session.evaluate(example.code);
    assert.ok(result.results.at(-1)?.text.includes(example.expected), `${example.id}: ${JSON.stringify(result)}`);
    if (example.expected === '0') assert.equal(result.results.at(-1).text, '0');
    assert.doesNotMatch(result.output, /Maxima encountered a Lisp error|incorrect syntax/);
    console.log(`PASS Maxima manual example: ${example.title}`);
  }
  const result = await session.evaluate('f(x):=x^2$ f(7);');
  assert.equal(result.results.at(-1).text, '49');
  assert.equal((await session.evaluate('%+1;')).results.at(-1).text, '50');
  console.log('Maxima optimized image: manual examples, share autoloading, definitions and session state pass.');
} finally {session.close();}
