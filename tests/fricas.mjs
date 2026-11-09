// Documented mathematics and complete selected upstream unit-test files.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {createFriCAS} from '../scripts/fricas.mjs';
import {fricasFiles} from '../scripts/fricas-files.mjs';
import {examples} from '../fricas/examples.mjs';
const [image = 'miricl.wasm', core = 'build/fricas/fricas-optimized.wasm', native = 'build/fricas/native',
  output = 'build/fricas/tests.json'] = process.argv.slice(2);
const report = {passed: false, examples: [], upstream: []};
const {files} = fricasFiles(path.resolve(native));
let session;
try {
  session = await createFriCAS(fs.readFileSync(image), fs.readFileSync(core), files);
  assert.equal(session.vm.evaluate("(compiled-function-p #'boot::|parseAndInterpret|)").output, 'T\n');
  for (const example of examples) {
    console.log(`FriCAS example: ${example.title}`);
    const result = await session.evaluate(example.code);
    report.examples.push({id: example.id, ...result});
    assert.ok(result.output.includes(example.expected), `${example.id}: ${result.output}`);
    for (const text of example.checks ?? []) assert.ok(result.output.includes(text), `${example.id}: missing ${text}`);
    if (example.pattern) assert.match(result.output, new RegExp(example.pattern));
    assert.doesNotMatch(result.output, /System error|Internal Error|not implemented|cannot find|not available/i);
    console.log(`PASS FriCAS manual example: ${example.title}`);
  }
  assert.match((await session.evaluate('persist_value := 13\npersisted(x) == x + persist_value\npersisted(29)')).output, /\)\s+42\s+Type:/);
  assert.match((await session.evaluate('% + 1')).output, /\)\s+43\s+Type:/);
  assert.match((await session.evaluate(')help clear')).output, /clear/i);
  session.close(); session = null;
  for (const name of ['array', 'table', 'ord']) {
    console.log(`FriCAS upstream: ${name}.input`);
    // Fresh state prevents an earlier suite's counters from hiding omissions.
    session = await createFriCAS(fs.readFileSync(image), fs.readFileSync(core), files);
    const source = fs.readFileSync(new URL(`../vendor/fricas/src/input/${name}.input`, import.meta.url), 'utf8');
    const result = await session.evaluate(source);
    const failures = Number(result.output.match(/unexpected failures:\s*(\d+)/)?.[1] ?? NaN);
    const total = Number(result.output.match(/total tests:\s*(\d+)/)?.[1] ?? NaN);
    report.upstream.push({name, total, failures, ...result});
    assert.equal(failures, 0, `${name}: ${result.output}`);
    assert.equal(total, (source.match(/^test(?:Equals|True|False|LibraryError)\(/gm) ?? []).length, `${name}: incomplete suite`);
    assert.ok(total > 0);
    console.log(`PASS FriCAS ${name}: ${total} upstream cases`);
    session.close(); session = null;
  }
  report.passed = true;
} catch (error) {
  report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};
  throw error;
} finally {
  session?.close(); fs.mkdirSync(path.dirname(output), {recursive: true});
  fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
}
