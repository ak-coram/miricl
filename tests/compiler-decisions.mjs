import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {loadMiricl} from '../scripts/miricl-loader.mjs';
import {collectCompilationStatistics, readCompilationStatistics} from '../scripts/compilation-statistics.mjs';

const [image = 'miricl.wasm'] = process.argv.slice(2);
const statsEnv = 'MIRICL_COMPILER_STATISTICS_DIRECTORY';
const temporary = process.env[statsEnv] ? null : await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-decisions-'));
if (temporary) process.env[statsEnv] = temporary;
let vm;
try {
  vm = await loadMiricl(await fs.readFile(image));
  vm.mountFile('/compiler-decisions.lisp', await fs.readFile(new URL('compiler-decisions.lisp', import.meta.url)));
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  await collectCompilationStatistics(vm, {workload: 'compiler-decisions', phase: 'Decision fixtures',
    workloadScope: 'diagnostic', compilationMode: 'native'}, async () => {
    await vm.evaluateAsync('(load "/compiler-decisions.lisp") (values)', {eof: true});
    const rows = await readCompilationStatistics(vm);
    function has(rule, detail) {
      return rows.some(row => row.rule === rule && row.count > 0 && detail(row.detail));
    }
    const applied = detail => detail === 'applied' || detail.startsWith('applied/');
    for (const family of ['loop-strip-mining', 'constant-branch', 'lexical-inline', 'checked-integer-arithmetic',
      'native-tail-call', 'raw-integer-operand', 'reusable-scalar-worker']) {
      assert.ok(has(`decision/${family}`, applied), `${family} applies`);
      assert.ok(has(`decision/${family}`, detail => !applied(detail)), `${family} records a rejection`);
      assert.ok(has(`decision-coverage/${family}`, detail => detail === 'measured'), `${family} records coverage`);
    }
    assert.ok(has('decision/loop-strip-mining', detail => detail === 'other-call'), 'loop with a callback');
    assert.ok(has('decision-coverage/loop-strip-mining', detail => detail === 'body-policy'), 'loop skipped at SPACE 3');
    assert.ok(has('decision/lexical-inline', detail => detail === 'notinline'), 'local NOTINLINE veto');
    assert.ok(has('decision/native-tail-call', detail => detail === 'debug-policy'), 'tail call debug veto');
    for (const result of ['i64', 'f32', 'f64', 'character'])
      assert.ok(has('decision/reusable-scalar-worker', detail => detail === `applied/${result}`), `${result} worker claim`);
    assert.ok(has('decision/private-scalar-call', detail => detail === 'applied/i64'), 'sealed call reaches i64 worker');
    for (const rule of ['lowering/wasm-input-bytes', 'lowering/wasm-output-bytes', 'size-units/wasm-body-optimization'])
      assert.ok(has(rule, detail => detail === ''), `Wasm optimizer reports ${rule}`);
    for (const field of ['input', 'output', 'units'])
      assert.ok(has(`size-${field}/wasm-outlining`, detail => detail === ''), `Wasm outlining reports ${field}`);
  });
  console.log('Compiler decisions: applied and rejected opportunities, coverage, and runtime results.');
} finally {
  vm?.close();
  if (temporary) {
    delete process.env[statsEnv];
    await fs.rm(temporary, {recursive: true, force: true});
  }
}
