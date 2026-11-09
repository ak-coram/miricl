// Measure the built production files and independently load the application.
// This is repository tooling; applications need only the shipped WASI REPL.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from './miricl.mjs';
import {measureFile} from './wasm-size.mjs';

const directory = path.resolve(process.argv[2] ?? 'game-of-life');
const file = name => path.join(directory, name);
const application = file('build/game-of-life.wasm');
const reportPath = file('build/sizes.json');
const interpreter = await fs.readFile(file('build/miricl.wasm'));
const report = {application: 'game-of-life', backend: 'native-wasm',
  entryPoints: ['LIFE:START'], measurements: [], timings: {}};
const retention = await fs.readFile(file('build/retention.txt'), 'utf8');
const nodes = /Retained (\d+) of (\d+) definition/.exec(retention);
const records = /Graph: (\d+) -> (\d+) records/.exec(retention);
assert.ok(nodes && records, 'Size reports must distinguish definition pruning from graph coalescing.');
report.reachability = {retainedNodes: Number(nodes[1]), totalNodes: Number(nodes[2])};
report.graph = {inputRecords: Number(records[1]), outputRecords: Number(records[2])};

// A fresh production interpreter has the built-in compiler and no ASDF. Verify the
// persistent set operations, previous-world sharing and printable REPL state.
const initializeStarted = performance.now();
const production = await Miricl.instantiate(interpreter);
report.timings.initializeMs = Math.round(performance.now() - initializeStarted);
try {
  const loadStarted = performance.now();
  production.mountFile('/application.wasm', await fs.readFile(application));
  await production.evaluateAsync('(load "/application.wasm")', {eof: true});
  report.timings.loadMs = Math.round(performance.now() - loadStarted);
  assert.equal(production.evaluate(`(let* ((a (life:seed :blinker))
    (b (life:next-generation a)) (c (life:next-generation b)))
    (and (fset2:equal? (life:world-cells a) (life:world-cells c))
         (not (fset2:equal? (life:world-cells a) (life:world-cells b)))
         (= (life:world-generation a) 0) (= (life:world-generation c) 2)
         (= (length (life:ascii-grid a)) 400)
         (notany #'find-package '("ASDF" "UIOP"))
         (fboundp 'miricl-compiler:compile-native)))`, {eof: true}).output, 'T\n');
  // Fixed, nontrivial population and generation counts make runtime timings
  // comparable while checking the result independently of any wall-time limit.
  const stepStarted = performance.now();
  assert.equal((await production.evaluateAsync(`
    (loop with world = (life:seed :r-pentomino)
          repeat 10 do (setq world (life:next-generation world))
          finally (return (list (life:world-generation world)
                                (fset2:size (life:world-cells world)))))`, {eof: true})).output,
    '(10 11)\n');
  report.timings.tenGenerationsMs = Math.round(performance.now() - stepStarted);
  const asciiStarted = performance.now();
  assert.equal((await production.evaluateAsync(`
    (loop repeat 10 always (= (length (life:ascii-grid)) 400))`, {eof: true})).output, 'T\n');
  report.timings.tenAsciiGridsMs = Math.round(performance.now() - asciiStarted);
} finally {production.close();}

for (const [role, name] of [['application', 'build/game-of-life.wasm'],
  ['interpreter', 'build/miricl.wasm'], ['adapter', 'build/miricl.js'],
  ['html', 'build/index.html'],
  ['developmentRepl', 'miricl-repl.wasm']]) {
  const measurement = {role, ...measureFile(file(name))};
  report.measurements.push(measurement);
  console.log(`${role}: ${measurement.rawBytes} raw; ${measurement.gzipBytes} gzip; ${measurement.brotliBytes} Brotli bytes`);
}
report.production = {};
for (const size of ['rawBytes', 'gzipBytes', 'brotliBytes'])
  report.production[size] = report.measurements.filter(m => m.role !== 'developmentRepl')
    .reduce((sum, m) => sum + m[size], 0);
report.validation = 'Fresh production load, immutable blinker, ten R-pentomino generations (11 cells), ASCII grid, built-in compiler, no ASDF';
await fs.writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
console.log(`Wrote ${reportPath}`);
console.log('Timings (ms):', report.timings);
