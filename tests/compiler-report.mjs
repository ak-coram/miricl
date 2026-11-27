import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {Script} from 'node:vm';
import {collectCompilerReport, renderCompilerReport, validateCompilerReport} from '../scripts/build-compiler-report.mjs';
import {buildDecisionViews, decisionViewScript, latestStageRecords} from '../scripts/compiler-decision-report.mjs';
import {optimizationCatalog, optimizationCategories, optimizationRawRules} from '../scripts/compiler-optimization-catalog.mjs';

const project = new URL('../', import.meta.url);
const categoryIds = new Set(optimizationCategories.map(category => category.id));
const entriesById = new Set(optimizationCatalog.map(entry => entry.id));
const decisionIds = optimizationCatalog.flatMap(entry => entry.rules);
assert.equal(categoryIds.size, optimizationCategories.length);
assert.equal(entriesById.size, optimizationCatalog.length);
assert.equal(new Set(decisionIds).size, decisionIds.length);
for (const {parent} of optimizationCategories) assert.ok(parent === null || categoryIds.has(parent));
for (const {category} of optimizationCatalog) assert.ok(categoryIds.has(category));
const sourceRawRules = new Set();
const sourceDecisionIds = new Set();
const sourceCoverageIds = new Set();
for (const file of fs.readdirSync(project).filter(file => file.endsWith('.lisp'))) {
  const source = fs.readFileSync(new URL(file, project), 'utf8');
  for (const [,rule] of source.matchAll(/:((?:optimization|planning|proof|inference|lowering|size-input|size-output|size-units)\/[a-z][a-z0-9-]*)/gi)) {
    assert.ok(Object.hasOwn(optimizationRawRules, rule.toLowerCase()), `${file} has uncataloged raw counter ${rule}`);
    sourceRawRules.add(rule.toLowerCase());
  }
  for (const [,kind,id] of source.matchAll(/:(decision|decision-coverage)\/([a-z][a-z0-9-]*)/gi)) {
    assert.ok(decisionIds.includes(id.toLowerCase()), `${file} has uncataloged ${kind} ${id}`);
    const sourceIds = kind.toLowerCase() === 'decision' ? sourceDecisionIds : sourceCoverageIds;
    sourceIds.add(id.toLowerCase());
  }
}
assert.deepEqual(sourceRawRules, new Set(Object.keys(optimizationRawRules)));
assert.deepEqual(sourceDecisionIds, new Set(decisionIds));
assert.deepEqual(sourceCoverageIds, new Set(decisionIds));

const loop = 'loop-strip-mining', branch = 'constant-branch', inline = 'lexical-inline';
const decide = (family, detail, count) => ({rule: 'decision/' + family, detail, count});
const coverage = (family, detail, count) => ({rule: 'decision-coverage/' + family, detail, count});
const stage = (workload, phase, counters, overrides = {}) => ({
  kind: 'compiler-statistics', schemaVersion: 1, id: workload + '/' + phase,
  job: 'application-' + workload, attempt: '1', workload, phase, counters, status: 'complete',
  collectionKind: 'compiler', compilationMode: 'native', ...overrides,
});
const records = [
  stage('alpha', 'abandoned first attempt', [decide(loop, 'applied', 80)]),
  stage('alpha', 'native build', [decide(loop, 'other-call', 8), decide(loop, 'applied', 2),
    coverage(loop, 'measured', 3), decide(inline, 'applied', 4)], {attempt: '2'}),
  stage('alpha', 'mixed setup', [decide(loop, 'applied', 3)], {attempt: '2', compilationMode: 'mixed'}),
  stage('beta', 'native build', [decide(loop, 'applied', 1), coverage(branch, 'measured', 2)]),
  stage('beta', 'incomplete pass', [decide(loop, 'other-call', 10)], {status: 'failed'}),
  stage('gamma', 'native build', [coverage(loop, 'module-debug', 12)]),
  stage('lem', 'native build', [], {status: 'skipped'}),
  stage('lem', 'portable ASDF', [decide(branch, 'unknown-test', 7)],
    {compilationMode: 'portable', recordType: 'measurement'}),
  stage('lem', 'artifact size', [decide(branch, 'applied', 200)],
    {compilationMode: 'portable', collectionKind: 'measurement', recordType: 'measurement'}),
  stage('compiler-bootstrap', 'bootstrap native functions', [decide(branch, 'unknown-test', 5)], {workloadScope: 'compiler'}),
  stage('compiler-bootstrap', 'sealed compiler module', [decide(loop, 'applied', 23)], {workloadScope: 'compiler'}),
  stage('compiler-replay', 'Frozen compiler sources (SPACE 2)', [decide(branch, 'applied', 2), decide(loop, 'other-call', 4)],
    {workloadScope: 'compiler'}),
  stage('compiler-replay', 'failed replay', [decide(loop, 'applied', 50)],
    {workloadScope: 'compiler', status: 'failed'}),
  stage('compiler-decisions', 'smoke test', [decide(loop, 'applied', 42)], {workloadScope: 'diagnostic'}),
  stage('rerun-without-artifact', 'old artifact', [decide(loop, 'applied', 55)]),
  stage('legacy', '</th><script>INJECTION</script>', [{rule: 'optimization/hir-constant-branch', detail: '', count: 5}],
    {collectionKind: undefined, compilationMode: undefined, timingIncludesCollection: true}),
];
const jobs = [
  {name: 'Application / alpha', run_attempt: 2, conclusion: 'success'},
  {name: 'Application / beta', run_attempt: 1, conclusion: 'success'},
  {name: 'Application / clysm browser bootstrap', run_attempt: 1, conclusion: 'success'},
  {name: 'Application / rerun-without-artifact', run_attempt: 2, conclusion: 'failure'},
];
const report = {schemaVersion: 1, kind: 'compiler-statistics-report', records, jobs};
const originalRecords = structuredClone(records);
const db = buildDecisionViews(report);
const view = mode => db.views.find(view => view.mode === mode);
const rule = (mode, family) => view(mode).groups.flatMap(group => group.decisions).find(row => row.family === family);
const app = (mode, family, name) => rule(mode, family).apps.find(row => row.name === name);
const outcomes = row => [row.candidates, row.applied, row.rejected];

assert.equal(db.defaultMode, 'all');
assert.deepEqual(outcomes(rule('native', loop).total), [11, 3, 8]);
assert.deepEqual(outcomes(view('native').total), [15, 7, 8]);
assert.deepEqual(outcomes(view('native').apps.find(row => row.name === 'alpha')), [14, 6, 8]);
assert.deepEqual(outcomes(view('native').groups.find(group => group.id === 'scheduler-passes').apps.find(row => row.name === 'beta')), [1, 1, 0]);
assert.equal(rule('native', loop).total.reasons.get('other-call'), 8);
assert.equal(rule('native', loop).total.coverage.get('module-debug'), 12);
assert.equal(app('native', loop, 'gamma').observed, false);
assert.equal(app('native', loop, 'clysm').jobResult, 'success');
assert.equal(app('native', branch, 'beta').observed, true);
assert.deepEqual(outcomes(app('native', branch, 'beta')), [0, 0, 0]);
assert.equal(app('native', branch, 'lem').observed, false);
assert.equal(app('all', branch, 'legacy').observed, false);
assert.deepEqual(outcomes(rule('portable', branch).total), [7, 0, 7]);
assert.deepEqual(outcomes(rule('mixed', loop).total), [3, 3, 0]);
assert.deepEqual(outcomes(rule('all', loop).total), [14, 6, 8]);
assert.deepEqual(outcomes(rule('all', loop).compiler), [27, 23, 4]);
assert.deepEqual(outcomes(view('all').compiler), [34, 25, 9]);
assert.deepEqual(outcomes(view('native').compiler), outcomes(view('all').compiler));
assert.equal(view('portable').compiler.observed, false);
assert.deepEqual(outcomes(view('all').groups.find(group => group.id === 'scheduler-passes').compiler), [27, 23, 4]);
const replay = phase => view('all').compiler.stages.find(stage => stage.record.phase === phase);
assert.deepEqual(outcomes(replay('Frozen compiler sources (SPACE 2)')), [6, 2, 4]);
assert.deepEqual(outcomes(replay('failed replay')), [50, 50, 0]);
assert.deepEqual(outcomes(rule('all', loop).diagnostic), [42, 42, 0]);
assert.ok(!latestStageRecords(records, jobs).some(record => ['old artifact', 'abandoned first attempt'].includes(record.phase)));
const diagnostic = stage('compiler-decisions', 'prior diagnostics', [decide(loop, 'applied', 7)],
  {job: 'core-checks', workloadScope: 'diagnostic'});
assert.deepEqual(latestStageRecords([diagnostic], [{name: 'Core checks beyond ANSI', run_attempt: 2}]), []);

const html = renderCompilerReport(report);
assert.deepEqual(report.records, originalRecords);
assert.match(html, /value="all" selected/);
assert.match(html, /11<\/td><td>3<\/td><td>8<\/td><td>27\.27%/);
assert.match(html, /All applications total/);
assert.match(html, /Compiler workload total<\/th><td>34<\/td><td>25<\/td><td>9<\/td>/);
assert.match(html, /Compiler workload by phase/);
assert.match(html, /Compiler workload by optimization/);
assert.match(html, /bootstrap native functions<small>Compiler bootstrap · Native · observed/);
assert.match(html, /sealed compiler module<small>Compiler bootstrap · Native · observed/);
assert.match(html, /Frozen compiler sources \(SPACE 2\)<small>Compiled compiler replay · Native · observed/);
assert.match(html, /failed replay<small>Compiled compiler replay · Native · failed · excluded from decision totals/);
assert.match(html, /Polling and loop batching by application/);
assert.match(html, /alpha by phase/);
assert.match(html, /Top rejection reason/);
assert.match(html, /Fewest opportunities/);
assert.match(html, /Superseded by a newer job attempt/);
assert.match(html, /compiler-statistics\.json" download/);
assert.doesNotMatch(html, /<script>INJECTION/);
new Script(html.split('<script>').at(-1).split('</script>')[0]);

const badStage = stage('corrupt', 'incomplete JSON', [null]);
const invalidReport = {...report, records: [null, badStage, records[1]]};
const checkedReport = validateCompilerReport(invalidReport);
assert.deepEqual(checkedReport.records, [records[1]]);
assert.equal(checkedReport.diagnostics.length, 2);
assert.match(renderCompilerReport(invalidReport), /invalid compiler statistics record/);
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-compiler-report-'));
try {
  fs.writeFileSync(path.join(directory, 'good.json'), JSON.stringify(records[1]));
  fs.writeFileSync(path.join(directory, 'copy.json'), JSON.stringify(records[1]));
  fs.writeFileSync(path.join(directory, 'bad.json'), JSON.stringify(badStage));
  fs.writeFileSync(path.join(directory, 'bad-run.json'), JSON.stringify({...records[1], runId: {toString: null}}));
  const collected = collectCompilerReport(directory);
  assert.deepEqual(collected.records, [records[1]]);
  assert.deepEqual(new Set(collected.diagnostics), new Set([
    'bad.json: invalid compiler statistics record', 'bad-run.json: invalid compiler statistics record',
  ]));
  const malformedContext = collectCompilerReport(directory, {runId: {toString: null}});
  assert.deepEqual(malformedContext.records, [records[1]]);
  assert.ok(malformedContext.diagnostics.includes('Invalid report metadata field runId'));
} finally {
  fs.rmSync(directory, {recursive: true, force: true});
}

function testControls() {
  const control = value => ({value, addEventListener(event, update) { this[event] = update; }});
  const mode = control('native'), sort = control('rejected'), reason = control(''), search = control('');
  const row = (sortName, sortCandidates, sortRejected, sortRate, reasons) => ({
    dataset: {sortName, sortCandidates, sortRejected, sortRate, reasons},
  });
  const frequent = row('Frequent', 100, 70, 0.3, 'other-call');
  const rare = row('Rare', 1, 0, 1, '');
  const zero = row('Zero', 0, 0, -1, '');
  const unobserved = row('Unobserved', -1, -1, -1, '');
  const rows = [rare, unobserved, frequent, zero];
  const body = {rows, append(...rows) { this.rows = rows; }};
  const searchItems = ['Scalar worker', 'Checked comparison'].map(label => ({dataset: {search: label}}));
  const category = {querySelector() { return searchItems.find(item => !item.hidden); }};
  const selectors = {'[data-sort-rows]': [body], '[data-decision-filter]': rows,
    '[data-catalog-item]': searchItems, '[data-catalog-category]': [category]};
  const native = {dataset: {decisionMode: 'native'}, querySelectorAll(selector) { return selectors[selector]; }};
  const mixed = {dataset: {decisionMode: 'mixed'}, querySelectorAll() { return []; }};
  const nodes = {'decision-mode': mode, 'decision-sort': sort, 'decision-reason': reason,
    'decision-catalog-search': search, 'decision-controls': {}};
  const document = {getElementById(id) { return nodes[id]; },
    querySelectorAll(selector) { return selector === '[data-decision-mode]' ? [native, mixed] : []; }};
  new Script(decisionViewScript).runInNewContext({document});
  assert.deepEqual(Array.from(body.rows), [frequent, rare, zero, unobserved]);
  assert.equal(mixed.hidden, true);
  sort.value = 'fewest';
  sort.change();
  assert.deepEqual(Array.from(body.rows), [zero, rare, frequent, unobserved]);
  reason.value = 'other-call';
  reason.change();
  assert.equal(rare.hidden, true);
  assert.equal(frequent.hidden, false);
  search.value = 'checked';
  search.input();
  assert.equal(searchItems[0].hidden, true);
  assert.equal(searchItems[1].hidden, false);
  assert.equal(category.open, true);
  mode.value = 'mixed';
  mode.change();
  assert.equal(native.hidden, true);
  assert.equal(mixed.hidden, false);
}
testControls();

const historicalOnly = renderCompilerReport({...report, records: records.filter(record => record.workload === 'legacy')});
assert.match(historicalOnly, /value="all" selected/);
assert.match(historicalOnly, /No application decisions were observed in this mode/);
assert.match(historicalOnly, /optimization\/hir-constant-branch/);

const raw = (rule, count, detail = '') => ({rule, detail, count});
const sizePair = (input, output, batches) => [
  raw('lowering/wasm-input-bytes', input), raw('lowering/wasm-output-bytes', output),
  ...(batches === undefined ? [] : [raw('size-units/wasm-body-optimization', batches)]),
];
const scalar = 'reusable-scalar-worker';
const sizeReport = {...report, jobs: [], records: [
  stage('alpha', 'worker emission', [decide(scalar, 'applied/i64', 5), decide(scalar, 'applied', 1),
    decide(scalar, 'applied/f32', 2), decide(scalar, 'applied/character', 4),
    decide(scalar, 'wrapper/identity', 2), decide(scalar, 'wrapper', 1),
    raw('optimization/character-code-fusion', 3),
    raw('optimization/unboxed-result-read', 10), ...sizePair(100, 70, 2),
    raw('size-input/wasm-outlining', 200), raw('size-output/wasm-outlining', 150), raw('size-units/wasm-outlining', 1)]),
  stage('beta', 'legacy batch', sizePair(20, 25)),
  stage('beta', 'unmatched counters', [raw('lowering/wasm-input-bytes', 99)]),
  stage('beta', 'different counter details',
    [raw('lowering/wasm-input-bytes', 99, 'source'), raw('lowering/wasm-output-bytes', 50, 'target')]),
  stage('beta', 'incomplete batch', [raw('optimization/unboxed-result-read', 1234), ...sizePair(90, 0, 2)], {status: 'failed'}),
  stage('beta', 'artifact only', sizePair(490, 20, 4), {collectionKind: 'measurement'}),
  stage('beta', 'portable batch', [...sizePair(10, 0, 1), coverage('raw-integer-operand', 'measured', 4)],
    {compilationMode: 'portable'}),
  stage('beta', 'mixed worker emission', [decide(scalar, 'applied/f64', 3)], {compilationMode: 'mixed'}),
  stage('compiler-bootstrap', 'compiled batch',
    [decide(scalar, 'applied/character', 7), raw('optimization/unboxed-result-read', 21), ...sizePair(50, 44, 1)],
    {workloadScope: 'compiler'}),
]};
const enriched = buildDecisionViews(sizeReport);
const enrichedNative = enriched.views.find(view => view.mode === 'native');
const allMechanisms = enrichedNative.groups.flatMap(group => group.decisions);
const scalarWorker = allMechanisms.find(row => row.family === scalar);
assert.deepEqual(outcomes(scalarWorker.total), [15, 12, 3]);
assert.deepEqual(outcomes(enrichedNative.total), outcomes(scalarWorker.total));
assert.deepEqual(outcomes(enrichedNative.compiler), outcomes(scalarWorker.compiler));
assert.deepEqual([...scalarWorker.total.reasons], [['wrapper', 3]]);
assert.deepEqual([...scalarWorker.total.appliedVariants], [['i64', 5], ['f32', 2], ['character', 4]]);
assert.deepEqual([...scalarWorker.total.rejectedVariants], [['wrapper/identity', 2]]);
const rawOperand = allMechanisms.find(row => row.family === 'raw-integer-operand');
assert.equal(rawOperand.total.observed, false);
assert.equal(rawOperand.events.find(event => event.rule === 'optimization/unboxed-result-read').count, 10);
assert.equal(rawOperand.compiler.events.find(event => event.rule === 'optimization/unboxed-result-read').count, 21);
assert.equal(rawOperand.apps.find(app => app.name === 'beta').events.length, 0);
const window = (view, id) => view.sizes.find(window => window.id === id);
const body = view => window(view, 'wasm-body-optimization');
assert.deepEqual(body(enrichedNative).total, {input: 120, output: 95, units: 2, observed: 2, withUnits: 1, saved: 25});
assert.equal(body(enrichedNative).compiler.saved, 6);
assert.equal(body(enrichedNative).combined.saved, 31);
assert.equal(body(enrichedNative).apps.find(app => app.name === 'beta').saved, -5);
assert.equal(body(enriched.views.find(view => view.mode === 'portable')).total.saved, 10);
assert.equal(window(enrichedNative, 'wasm-outlining').combined.saved, 50);
const enrichedHTML = renderCompilerReport(sizeReport);
assert.match(enrichedHTML, /Optimization catalog/);
assert.match(enrichedHTML, /Raw scalar result locals and boxing · No decision coverage/);
assert.match(enrichedHTML, /Applied variants.*i64 <b>5<\/b>/);
assert.match(enrichedHTML, /Rejection variants.*wrapper \/ identity <b>2<\/b>/);
assert.match(enrichedHTML, /cached raw result reads/);
assert.match(enrichedHTML, /Emitted nonliteral integer operand reads <b>4<\/b>/);
assert.match(enrichedHTML, /filter by the recorded stage label/);
assert.match(enrichedHTML, /Wasm size savings/);
assert.match(enrichedHTML, /<td>20<\/td><td>25<\/td><td>-5<\/td>\s*<td>-25%<\/td>/);
assert.match(enrichedHTML, /Some byte pairs were recorded without a batch count/);

// Inspect the overview tables in each mode. The same labels also occur in the catalog.
const modeHTML = mode => enrichedHTML.split(`<div data-decision-mode="${mode}"`)[1].split('<div data-decision-mode=')[0];
const categoryTable = (mode, scope) => modeHTML(mode).split(`<h2>${scope}</h2>`)[1].split('</table>')[0];
const unboxingRow = (table, label) => table.match(new RegExp(`<tr data-sort-name="${label} unboxing"[^]*?</tr>`))?.[0];
for (const mode of enriched.views.map(view => view.mode)) {
  for (const scope of ['All applications', 'Compiler workload']) {
    const table = categoryTable(mode, scope);
    const integer = unboxingRow(table, 'Integer');
    const floating = unboxingRow(table, 'Floating-point');
    const character = unboxingRow(table, 'Character');
    assert.ok(integer, `${mode} ${scope} is missing integer unboxing`);
    for (const [category, row] of [['float', floating], ['character', character]]) {
      assert.ok(row, `${mode} ${scope} is missing ${category} unboxing`);
      assert.match(row, /No type-specific decision coverage/);
      const catalogTarget = `catalog-${mode}-${category}-lowering`;
      assert.ok(row.includes(`href="#${catalogTarget}"`));
      assert.ok(modeHTML(mode).includes(`<details id="${catalogTarget}"`));
      const workerTarget = row.match(/href="#([^"]+)" data-open-details>Recorded reusable worker ABI claims/)?.[1];
      const workerDestination = scope === 'All applications'
        ? `id="${workerTarget}" class="decision-breakdown">\n<summary>Reusable native scalar workers`
        : `id="${workerTarget}" data-sort-name="Reusable native scalar workers"`;
      assert.ok(modeHTML(mode).includes(workerDestination));
    }
  }
}
const applicationUnboxing = (mode, label) => unboxingRow(categoryTable(mode, 'All applications'), label);
assert.match(applicationUnboxing('native', 'Integer'), /claims<\/a> · i64 5/);
assert.match(applicationUnboxing('native', 'Floating-point'), /claims<\/a> · f32 2 · f64 0/);
assert.match(applicationUnboxing('all', 'Floating-point'), /claims<\/a> · f32 2 · f64 3/);
assert.match(applicationUnboxing('mixed', 'Floating-point'), /claims<\/a> · f32 0 · f64 3/);
assert.match(applicationUnboxing('portable', 'Floating-point'), /claims<\/a> · Unobserved/);
assert.match(applicationUnboxing('portable', 'Integer'), /<td>0<\/td><td>0<\/td><td>0<\/td>/);
assert.match(applicationUnboxing('native', 'Character'), /claims<\/a> · character 4/);
assert.match(unboxingRow(categoryTable('native', 'Compiler workload'), 'Character'), /claims<\/a> · character 7/);
assert.match(modeHTML('native'), /catalog-native-character-lowering[^]*?CHAR and CHAR-CODE fusion · No decision coverage · Raw events/);

const malformedMetadata = {
  ...report,
  commit: {toString: null},
  runURL: {toString: null},
  generatedAt: {toString: null},
  records: [
    stage('corrupt', 'invalid mode', [], {compilationMode: {toString: null}}),
    stage('corrupt', 'invalid attempt', [], {attempt: {toString: null}}),
    stage('corrupt', 'invalid failure', [], {failure: {toString: null}}),
    stage('corrupt', 'invalid reason', [], {reason: {toString: null}}),
    stage('corrupt', 'invalid job type', [], {job: {toString: null}}),
    stage('corrupt', 'invalid job identifier', [], {job: 'application-alpha!spurious'}),
    stage('corrupt', 'invalid scope', [], {workloadScope: 'typo'}),
    stage('corrupt', 'invalid collection', [], {collectionKind: 'typo'}),
    stage('corrupt', 'invalid record type', [], {recordType: {toString: null}}),
    stage('corrupt', 'invalid timing', [], {timingIncludesCollection: 'false'}),
    records[1],
  ],
  jobs: [null, {name: 'Application / corrupt', run_attempt: {toString: null}},
    {name: 'Application / alpha!malformed', run_attempt: 3},
    {name: 'Other job', html_url: {toString: null}}, ...jobs],
  diagnostics: ['Artifact could not be downloaded', {toString: null}],
};
const checkedMetadata = validateCompilerReport(malformedMetadata);
assert.deepEqual(checkedMetadata.records, [records[1]]);
assert.deepEqual(checkedMetadata.jobs, jobs);
const metadataHTML = renderCompilerReport(malformedMetadata);
assert.match(metadataHTML, /invalid compiler statistics record/);
assert.match(metadataHTML, /invalid CI job record/);
assert.match(metadataHTML, /Artifact could not be downloaded/);
assert.match(metadataHTML, /invalid report diagnostic/);
assert.match(metadataHTML, /Invalid report metadata field commit/);
assert.match(metadataHTML, /Invalid report metadata field runURL/);
assert.match(metadataHTML, /Invalid report metadata field generatedAt/);
assert.match(metadataHTML, /native build/);
assert.match(metadataHTML, /Application \/ beta/);
assert.doesNotMatch(metadataHTML, /Superseded by a newer job attempt/);

const equivalentCopy = Object.fromEntries(Object.entries(records[1]).reverse());
const duplicated = validateCompilerReport({...report, records: [records[1], equivalentCopy,
  {...records[1], counters: [decide(loop, 'applied', 200)]}, records[3]]});
assert.deepEqual(duplicated.records, [records[1], records[3]]);
assert.deepEqual(duplicated.diagnostics, [`Conflicting record ${records[1].id}`]);
assert.deepEqual(validateCompilerReport(duplicated), duplicated);
console.log('Compiler decision report aggregation and rendering passed.');
