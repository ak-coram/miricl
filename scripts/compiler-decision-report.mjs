import {optimizationCatalog, optimizationCategories, optimizationRawRules} from './compiler-optimization-catalog.mjs';

const uncataloged = {id: 'other-decisions', label: 'Other decisions'};
const categoriesById = new Map([...optimizationCategories, uncataloged].map(item => [item.id, item]));
const decisionFamilies = new Set(optimizationCatalog.flatMap(entry => entry.rules));

const sizeWindows = {
  'wasm-body-optimization': {
    label: 'Local Wasm body optimization',
    description: 'Encoded function bodies before and after the local optimizer, including emission attempts that may later be discarded.',
    input: 'lowering/wasm-input-bytes',
    output: 'lowering/wasm-output-bytes',
  },
  'wasm-outlining': {
    label: 'Typed Wasm outlining',
    description: 'Encoded function bodies before and after outlining, including added helpers. Module framing, imports and type sections fall outside this window.',
  },
};

export const decisionModes = {
  all: 'All compilation modes',
  native: 'Native',
  portable: 'Portable',
  mixed: 'Mixed native and portable',
};

export function compilationMode(record) {
  return Object.hasOwn(decisionModes, record.compilationMode) && record.compilationMode !== 'all'
    ? record.compilationMode : record.compilationMode === 'none' ? 'none' : 'unknown';
}

export function isCompilerCollection(record) {
  if (record.collectionKind != null) return record.collectionKind === 'compiler';
  return record.timingIncludesCollection === true || record.counters.length > 0;
}

const workloadScope = record => record.workloadScope ??
  (record.workload === 'compiler-bootstrap' ? 'compiler' : 'application');
export const applicationJob = job => /^Application \/ ([\w-]+)(?: [\w-]+)*$/.exec(job.name)?.[1];
const latestAttempt = value => {
  const attempt = Number(value);
  return Number.isSafeInteger(attempt) && attempt > 0 ? attempt : 0;
};

export function latestStageRecords(records, jobs) {
  const recordedAttempts = new Map(), ciAttempts = new Map();
  const remember = (attempts, key, attempt) => attempts.set(key, Math.max(attempts.get(key) ?? 0, attempt));
  const stageKey = record => JSON.stringify([record.job, record.workload]);
  for (const job of jobs) {
    const application = applicationJob(job);
    if (application) remember(ciAttempts, 'application-' + application, latestAttempt(job.run_attempt));
    if (job.name === 'Build shared runtime and standalone pages')
      remember(ciAttempts, 'runtime', latestAttempt(job.run_attempt));
    if (job.name === 'Core checks beyond ANSI')
      remember(ciAttempts, 'core-checks', latestAttempt(job.run_attempt));
  }
  for (const record of records) {
    if (record.job) remember(recordedAttempts, stageKey(record), latestAttempt(record.attempt));
  }
  return records.filter(record => {
    const attempt = latestAttempt(record.attempt);
    return !record.job || !attempt || attempt >= Math.max(recordedAttempts.get(stageKey(record)) ?? 0,
      ciAttempts.get(record.job) ?? 0);
  });
}

const humanize = name => name.replace(/-/g, ' ');
const empty = () => ({observed: false, applied: 0, rejected: 0, reasons: new Map(),
  appliedVariants: new Map(), rejectedVariants: new Map(), coverage: new Map()});
const increment = (map, key, count) => map.set(key, add(map.get(key) ?? 0, count));
function add(a, b) {
  const count = a + b;
  if (!Number.isSafeInteger(count) || count < 0) throw new Error('Counter total exceeds safe integer range.');
  return count;
}

function summarize(parts) {
  const result = empty();
  for (const part of parts) {
    result.observed ||= part.observed;
    result.applied = add(result.applied, part.applied);
    result.rejected = add(result.rejected, part.rejected);
    for (const [reason, count] of part.reasons) increment(result.reasons, reason, count);
    for (const [variant, count] of part.appliedVariants) increment(result.appliedVariants, variant, count);
    for (const [variant, count] of part.rejectedVariants) increment(result.rejectedVariants, variant, count);
    for (const [item, count] of part.coverage) increment(result.coverage, item, count);
  }
  return {...result, candidates: add(result.applied, result.rejected)};
}

function readStage(record) {
  const outcomes = new Map(), counters = new Map();
  if (!isCompilerCollection(record) || record.status === 'cached' || record.status === 'skipped') {
    return {record, outcomes, counters};
  }
  for (const {rule, detail, count} of record.counters) {
    if (!counters.has(rule)) counters.set(rule, new Map());
    increment(counters.get(rule), detail, count);
    const decision = /^(decision|decision-coverage)\/(.+)$/.exec(rule);
    if (!decision) continue;
    const [, kind, family] = decision;
    if (!outcomes.has(family)) outcomes.set(family, empty());
    const result = outcomes.get(family);
    if (kind === 'decision-coverage') increment(result.coverage, detail, count);
    else {
      result.observed = true;
      const [base, variant] = detail.split(/\/(.*)/, 2);
      if (base === 'applied') {
        result.applied = add(result.applied, count);
        if (variant) increment(result.appliedVariants, variant, count);
      } else {
        result.rejected = add(result.rejected, count);
        increment(result.reasons, base, count);
        if (variant) increment(result.rejectedVariants, detail, count);
      }
    }
    if (kind === 'decision-coverage' && detail === 'measured') result.observed = true;
  }
  return {record, outcomes, counters};
}

function summarizeEvents(stages, descriptors) {
  const events = new Map();
  for (const {counters} of stages) for (const descriptor of descriptors) {
    const {rule, detail} = descriptor;
    for (const [item, count] of counters.get(rule) ?? []) {
      if (detail !== undefined && detail !== item) continue;
      const key = JSON.stringify([rule, item]);
      const old = events.get(key);
      events.set(key, {...descriptor, detail: item, count: add(old?.count ?? 0, count)});
    }
  }
  return [...events.values()].sort((a, b) => a.label.localeCompare(b.label) || a.detail.localeCompare(b.detail));
}

function summarizeSize(parts) {
  let input = 0, output = 0, units = 0, observed = 0, withUnits = 0;
  for (const part of parts) {
    input = add(input, part.input);
    output = add(output, part.output);
    units = add(units, part.units);
    observed = add(observed, part.observed);
    withUnits = add(withUnits, part.withUnits);
  }
  return {input, output, units, observed, withUnits, saved: input - output};
}

function stageSize(counters, inputRule, outputRule, unitsRule) {
  const pairs = [];
  for (const [detail, input] of counters.get(inputRule) ?? []) {
    const output = counters.get(outputRule)?.get(detail);
    if (output === undefined) continue;
    const units = counters.get(unitsRule)?.get(detail);
    pairs.push({input, output, units: units ?? 0, observed: 1, withUnits: units === undefined ? 0 : 1});
  }
  return summarizeSize(pairs);
}

const completed = stages => stages.filter(({record}) => record.status === 'complete');

export function buildDecisionViews(report, records = latestStageRecords(report.records, report.jobs)) {
  const applications = new Set(report.jobs.map(applicationJob).filter(Boolean));
  const jobResults = new Map();
  for (const job of report.jobs) {
    const application = applicationJob(job);
    if (application && latestAttempt(job.run_attempt) >= latestAttempt(jobResults.get(application)?.run_attempt))
      jobResults.set(application, job);
  }
  for (const record of report.records) {
    if (workloadScope(record) === 'application') applications.add(record.workload);
  }
  const names = [...applications].sort((a, b) => a.localeCompare(b));
  const stages = records.map(readStage);
  const definitions = new Map(optimizationCatalog.map(entry => [entry.id, entry]));
  const windows = new Map(Object.entries(sizeWindows));
  for (const {outcomes, counters} of stages) {
    for (const family of outcomes.keys()) {
      if (!decisionFamilies.has(family) && !definitions.has(family)) definitions.set(family, {
        id: family, label: humanize(family), category: uncataloged.id, unit: 'candidates', rules: [family],
      });
    }
    for (const rule of counters.keys()) {
      const id = /^size-(?:input|output)\/(.+)$/.exec(rule)?.[1];
      if (id && !windows.has(id)) windows.set(id, {label: humanize(id)});
    }
  }
  const views = Object.keys(decisionModes).map(mode => {
    const selected = stages.filter(({record}) => mode === 'all' || compilationMode(record) === mode);
    const compilerStages = selected.filter(({record}) => workloadScope(record) === 'compiler');
    const diagnosticStages = selected.filter(({record}) => workloadScope(record) === 'diagnostic');
    const applicationStages = names.map(name => selected.filter(({record}) =>
      workloadScope(record) === 'application' && record.workload === name));
    const summarizeWorkload = (name, stages, rules) => {
      const breakdown = stages.filter(({record}) =>
        isCompilerCollection(record) || record.status === 'cached' || record.status === 'skipped')
        .map(({record, outcomes}) => ({record,
          ...summarize(rules === undefined ? outcomes.values() : rules.map(rule => outcomes.get(rule) ?? empty()))}));
      return {name, stages: breakdown, ...summarize(completed(breakdown))};
    };
    const categories = new Map();
    for (const [id, definition] of definitions) {
      const {rules, category} = definition;
      const descriptors = (definition.rawRules ?? []).map(rule => ({
        rule,
        label: humanize(rule.split('/').at(-1)),
        ...Object.hasOwn(optimizationRawRules, rule) && optimizationRawRules[rule],
      }));
      const workload = (name, stages) => ({
        ...summarizeWorkload(name, rules.length ? stages : [], rules),
        events: summarizeEvents(completed(stages), descriptors),
      });
      const apps = names.map((name, index) => {
        const job = jobResults.get(name);
        return {...workload(name, applicationStages[index]), jobResult: job?.conclusion || job?.status};
      });
      const compiler = workload('Compiler workload', compilerStages);
      const diagnostic = workload('Diagnostic workload', diagnosticStages);
      const total = summarize(apps);
      const row = {family: id, ...definition, apps, compiler, diagnostic, total,
        measuredApps: apps.filter(app => app.observed).length,
        events: summarizeEvents(completed(applicationStages.flat()), descriptors)};
      if (!categories.has(category)) categories.set(category, []);
      categories.get(category).push(row);
    }
    const summarizeApplications = rows => names.map((name, index) => ({
      name, ...summarize(rows.map(row => row.apps[index])),
    }));
    const groups = [...categoriesById.values()].filter(category => categories.has(category.id)).map(category => {
      const {id, label, parent, workerResults} = category, decisions = categories.get(id);
      const apps = summarizeApplications(decisions);
      return {id, name: label, parent, workerResults, decisions, apps, total: summarize(apps), compiler: summarize(decisions.map(row => row.compiler)),
        measuredApps: apps.filter(app => app.observed).length};
    });
    const sizes = [...windows].map(([id, definition]) => {
      const {input = 'size-input/' + id, output = 'size-output/' + id} = definition;
      const sizeWorkload = (name, stages) => {
        const phases = completed(stages).map(({record, counters}) => ({
          record, ...stageSize(counters, input, output, 'size-units/' + id),
        })).filter(row => row.observed);
        return {name, phases, ...summarizeSize(phases)};
      };
      const apps = names.map((name, index) => sizeWorkload(name, applicationStages[index]));
      const compiler = sizeWorkload('Compiler workload', compilerStages);
      const diagnostic = sizeWorkload('Diagnostic workload', diagnosticStages);
      return {id, ...definition, apps, compiler, diagnostic, total: summarizeSize(apps),
        combined: summarizeSize([...apps, compiler])};
    });
    const apps = summarizeApplications(groups);
    return {mode, groups, apps, sizes, total: summarize(apps), measuredApps: apps.filter(app => app.observed).length,
      compiler: summarizeWorkload('Compiler workload', compilerStages)};
  });
  return {defaultMode: 'all', views, applicationCount: names.length,
    hasDiagnostics: report.records.some(record => workloadScope(record) === 'diagnostic')};
}

const formatCount = count => count.toLocaleString('en-US');
const formatPercent = ratio => {
  if (ratio > 0 && ratio < 0.0001) return '<0.01%';
  if (ratio > 0.9999 && ratio < 1) return '>99.99%';
  return ratio.toLocaleString('en-US', {style: 'percent', maximumFractionDigits: 2});
};
const rate = row => row.candidates ? row.applied / row.candidates : -1;
const metricCells = (row, escape) => row.observed
  ? `<td>${formatCount(row.candidates)}</td><td>${formatCount(row.applied)}</td><td>${formatCount(row.rejected)}</td><td>${row.candidates ? escape(formatPercent(rate(row))) : '—'}</td>`
  : '<td colspan="4" class="muted-cell">Unobserved</td>';
const rankedReasons = row => [...row.reasons].filter(([, count]) => count > 0)
  .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]));
const rowAttributes = (label, row, escape) => `data-sort-name="${escape(label)}" data-sort-candidates="${row.observed ? row.candidates : -1}"
    data-sort-rejected="${row.observed ? row.rejected : -1}" data-sort-rate="${rate(row)}" data-reasons="${escape(rankedReasons(row).map(([name]) => name).join(' '))}"`;
const summaryHeader = '<th scope="col">Candidates</th><th scope="col">Applied</th><th scope="col">Rejected</th><th scope="col">Applied rate</th>';
const countList = (counts, escape) => `<ul class="counts">${[...counts].map(([label, count]) =>
  `<li>${escape(humanize(label.replaceAll('/', ' / ')))} <b>${formatCount(count)}</b></li>`).join('')}</ul>`;
function outcomeList(row, escape) {
  const reasons = rankedReasons(row);
  return [
    reasons.length ? countList(reasons, escape) : '',
    row.appliedVariants.size ? `<small>Applied variants</small>${countList(row.appliedVariants, escape)}` : '',
    row.rejectedVariants.size ? `<small>Rejection variants</small>${countList(row.rejectedVariants, escape)}` : '',
  ].join('') || '—';
}
const topReason = (row, escape) => {
  const [entry] = rankedReasons(row);
  return entry ? `${escape(humanize(entry[0]))} <b>${formatCount(entry[1])}</b>` : '—';
};

const loopCoverage = {
  measured: 'Wasm bodies scanned',
  'module-measured': 'Wasm modules admitted',
  'module-debug': 'Wasm modules excluded by debug policy',
  'module-bootstrap': 'Wasm modules excluded during bootstrap',
  'module-fixed-call-emitter': 'Wasm modules excluded by fixed-call emission',
  'module-closed-workers': 'Wasm modules excluded by closed workers',
  'module-no-poll': 'Wasm modules without a scheduling poll',
  'body-no-structured-plan': 'Wasm bodies without a structured plan',
  'body-dynamic-cleanup': 'Wasm bodies excluded by dynamic cleanup',
  'body-policy': 'Wasm bodies excluded by optimization policy',
  'body-coalesced-entry': 'Wasm bodies with a coalesced entry',
  'body-size': 'Wasm bodies excluded by size',
  'body-module-fuel': 'Wasm bodies excluded by the module work budget',
  'body-exception-control': 'Wasm bodies excluded by exception control',
};
const branchCoverage = {
  measured: 'HIR functions with completed branch analysis',
  debug: 'HIR functions excluded by debug policy',
  'optimization-barrier': 'HIR functions with an optimization barrier',
  'analysis-budget': 'HIR functions excluded by the analysis budget',
};
const coverageLabels = {
  'loop-strip-mining': loopCoverage,
  'constant-branch': branchCoverage,
  'lexical-inline': {measured: 'Source HIR units examined for lexical calls'},
  'checked-integer-arithmetic': {
    measured: 'Eligible functions with completed candidate selection',
    'selection-budget': 'Eligible functions excluded by the selection budget',
    debug: 'Functions excluded by debug policy',
    'compilation-speed': 'Functions excluded by compilation-speed policy',
    'optimization-barrier': 'Functions with an optimization barrier',
    'planner-unavailable': 'Functions where the planner was unavailable',
  },
  'reusable-scalar-worker': {measured: 'Functions offered for a reusable scalar worker'},
  'private-scalar-call': {measured: 'Private call sites audited for scalar transport'},
  'native-tail-call': {measured: 'Native functions whose reachable tail-call sites were audited'},
  'raw-integer-operand': {measured: 'Emitted nonliteral integer operand reads'},
};

function coverageList(family, row, escape) {
  if (!row.coverage.size) return '—';
  const labels = Object.hasOwn(coverageLabels, family) ? coverageLabels[family] : {};
  return `<ul class="counts">${[...row.coverage].sort(([a], [b]) =>
    (a === 'measured' ? -1 : b === 'measured' ? 1 : a.localeCompare(b)))
    .map(([item, count]) => `<li>${escape(Object.hasOwn(labels, item) ? labels[item] : humanize(item))} <b>${formatCount(count)}</b></li>`).join('')}</ul>`;
}

function workloadStatus(row) {
  const measured = row.stages.filter(stage => stage.record.status === 'complete' && stage.observed).length;
  if (measured) return `${formatCount(measured)} stage${measured === 1 ? '' : 's'} observed`;
  if (row.coverage.size) return 'Coverage only';
  if (!row.stages.length) return 'No stage in this view';
  if (row.stages.every(stage => stage.record.status === 'cached')) return 'Cached build';
  if (row.stages.every(stage => stage.record.status === 'skipped')) return 'Skipped';
  if (row.stages.every(stage => ['partial', 'failed'].includes(stage.record.status))) return 'Incomplete stages';
  return 'No family coverage';
}

function renderEvents(decision, escape) {
  if (!decision.rawRules?.length) return '';
  const scopes = [{name: 'All applications total', events: decision.events}, ...decision.apps,
    decision.compiler, decision.diagnostic];
  return `<section><h3>Raw events</h3><p>Event counts describe the named observations in completed stages.</p></section>
<div class="table-wrap"><table><thead><tr><th scope="col">Workload</th><th scope="col">Event</th><th scope="col">Detail</th><th scope="col">Unit</th><th scope="col">Total</th></tr></thead><tbody>
${scopes.filter(scope => scope.name !== 'Diagnostic workload' || scope.events.length)
    .flatMap(scope => scope.events.length ? scope.events.map(event =>
      `<tr><th scope="row">${escape(scope.name)}</th><td>${escape(event.label)}<small>${escape(event.rule)}</small></td>
<td>${escape(event.detail || 'All')}</td><td>${escape(event.unit)}</td><td>${formatCount(event.count)}</td></tr>`)
      : [`<tr><th scope="row">${escape(scope.name)}</th><td colspan="4" class="muted-cell">No raw events recorded</td></tr>`]).join('')}
</tbody></table></div>`;
}

function renderStages(family, workload, escape, title = `${workload.name} by phase`) {
  if (!workload.stages.length) return '';
  return `<details class="phase-breakdown"><summary>${escape(title)}</summary>
<div class="table-wrap"><table><thead><tr><th scope="col">Phase</th>${summaryHeader}<th scope="col">Reasons and variants</th>${family ? '<th scope="col">Pass coverage</th>' : ''}</tr></thead>
<tbody>${workload.stages.map(stage => {
    const {record} = stage, mode = compilationMode(record);
    const compilerWorkload = workloadScope(record) === 'compiler'
      ? (record.workload === 'compiler-replay' ? 'Compiled compiler replay'
        : record.workload === 'compiler-bootstrap' ? 'Compiler bootstrap' : humanize(record.workload))
      : '';
    const status = record.status !== 'complete' ? `${record.status} · excluded from decision totals`
      : stage.observed ? 'observed' : stage.coverage.size ? 'coverage only' : 'no family coverage';
    return `<tr><th scope="row">${escape(record.phase)}<small>${escape([compilerWorkload, decisionModes[mode] ?? mode, status].filter(Boolean).join(' · '))}</small>
      ${record.failure || record.reason ? `<small>${escape(record.failure ?? record.reason)}</small>` : ''}</th>
      ${metricCells(stage, escape)}<td>${outcomeList(stage, escape)}</td>${family ? `<td>${coverageList(family, stage, escape)}</td>` : ''}</tr>`;
  }).join('')}</tbody></table></div></details>`;
}

function renderMechanism(decision, id, escape) {
  const {family, total, apps} = decision;
  const instrumented = decision.rules.length > 0;
  const covered = [total, decision.compiler, decision.diagnostic].some(row => row.observed || row.coverage.size);
  const eventsObserved = decision.events.length || decision.compiler.events.length || decision.diagnostic.events.length;
  return `<details id="${id}" class="decision-breakdown">
<summary>${escape(decision.label)}${instrumented ? ' by application' : ''}${covered ? '' : ' · No decision coverage'}${eventsObserved ? ' · Raw events' : ''}</summary>
<section>${decision.description ? `<p>${escape(decision.description)}</p>` : ''}
<p class="muted">${decision.scope ? escape(decision.scope) + ' · ' : ''}${instrumented ? 'Candidate unit' : 'Site'} · ${escape(decision.unit)}.</p>
</section>
${instrumented ? `<div class="table-wrap"><table><thead><tr><th scope="col">Application</th>${summaryHeader}<th scope="col">Reasons and variants</th><th scope="col">Pass coverage</th></tr></thead>
<tbody><tr class="group-row"><th scope="row">All applications</th>${metricCells(total, escape)}<td>${outcomeList(total, escape)}</td><td>${coverageList(family, total, escape)}</td></tr></tbody>
<tbody data-sort-rows>${apps.map(app => `<tr ${rowAttributes(app.name, app, escape)}><th scope="row">${escape(app.name)}
<small>${workloadStatus(app)}${app.jobResult ? ' · CI ' + escape(app.jobResult) : ''}</small></th>
${metricCells(app, escape)}<td>${outcomeList(app, escape)}</td><td>${coverageList(family, app, escape)}</td></tr>`).join('')}</tbody></table></div>
${apps.map(app => renderStages(family, app, escape)).join('')}` : ''}
${renderEvents(decision, escape)}</details>`;
}

function renderCategoryApplications(category, id, escape) {
  return `<details id="${id}" class="decision-breakdown"${category.open ? ' open' : ''}>
<summary>${escape(category.name)} by application</summary>
<div class="table-wrap"><table><thead><tr><th scope="col">Application</th>${summaryHeader}<th scope="col">Top rejection reason</th></tr></thead>
<tbody><tr class="group-row"><th scope="row">All applications</th>${metricCells(category.total, escape)}<td>${topReason(category.total, escape)}</td></tr></tbody>
<tbody data-sort-rows>${category.apps.map(app => `<tr ${rowAttributes(app.name, app, escape)}><th scope="row">${escape(app.name)}</th>
${metricCells(app, escape)}<td>${topReason(app, escape)}</td></tr>`).join('')}</tbody></table></div></details>`;
}

function renderOtherScope(rows, scope, title, escape, ruleIds) {
  return `<details class="other-scope"><summary>${title}</summary><div class="table-wrap">
<table><thead><tr><th scope="col">Optimization</th>${summaryHeader}<th scope="col">Reasons and variants</th><th scope="col">Pass coverage</th></tr></thead>
<tbody data-sort-rows>${rows.map(row => `<tr${ruleIds ? ` id="${ruleIds.get(row.family)}-${scope}"` : ''} ${rowAttributes(row.label, row[scope], escape)} data-decision-filter><th scope="row">${escape(row.label)}<small>${workloadStatus(row[scope])}</small></th>
${metricCells(row[scope], escape)}<td>${outcomeList(row[scope], escape)}</td><td>${coverageList(row.family, row[scope], escape)}</td></tr>`).join('')}</tbody></table></div>
${rows.map(row => renderStages(row.family, row[scope], escape, row.label + ' by phase')).join('')}</details>`;
}

function renderCompilerWorkload(view, categoryRows, rules, escape, ruleIds) {
  const {compiler} = view;
  const overview = row => `${metricCells(row, escape)}<td>${topReason(row, escape)}</td>`;
  return `<section class="scope-heading"><h2>Compiler workload</h2><small>${decisionModes[view.mode]}</small>
${compiler.observed ? '' : '<p>No compiler workload decisions were observed in this mode.</p>'}</section>
<div class="table-wrap"><table><thead><tr><th scope="col">Category</th>${summaryHeader}<th scope="col">Top rejection reason</th></tr></thead>
<tbody><tr class="group-row"><th scope="row">Compiler workload total</th>${overview(compiler)}</tr></tbody>
<tbody data-sort-rows>${categoryRows}</tbody></table></div>
${renderStages(undefined, compiler, escape)}
${renderOtherScope(rules, 'compiler', 'Compiler workload by optimization', escape, ruleIds)}`;
}

function renderCatalog(view, ruleIds, escape) {
  const byId = new Map(view.groups.map(group => [group.id, group]));
  function branch(category) {
    const group = byId.get(category.id);
    const rows = group?.decisions ?? [];
    const children = optimizationCategories.filter(child => child.parent === category.id);
    if (!rows.length && !children.length) return '';
    return `<details id="catalog-${view.mode}-${category.id}" class="catalog-category" data-catalog-category><summary>${escape(category.label)}</summary>
${rows.map(row => `<div data-catalog-item data-search="${escape([category.label, row.label, row.scope, row.description, row.unit].filter(Boolean).join(' '))}">
${renderMechanism(row, ruleIds.get(row.family), escape)}</div>`).join('')}
${children.map(branch).join('')}</details>`;
  }
  return `<section class="scope-heading" id="catalog-${view.mode}"><h2>Optimization catalog</h2>
<p>Browse compiler and runtime mechanisms by category. Entries describe their scope and show decision coverage and raw events where available.</p></section>
${[...optimizationCategories.filter(category => category.parent === null), uncataloged].map(branch).join('')}`;
}

const sizeSummaryHeader = '<th scope="col">Measured batches</th><th scope="col">Input bytes</th><th scope="col">Output bytes</th><th scope="col">Bytes saved</th><th scope="col">Savings</th>';
const signedBytes = count => count.toLocaleString('en-US', {signDisplay: 'exceptZero'});
function formatSavings(ratio) {
  if (ratio && Math.abs(ratio) < 0.0001) return ratio < 0 ? '−<0.01%' : '+<0.01%';
  return ratio.toLocaleString('en-US', {style: 'percent', maximumFractionDigits: 2, signDisplay: 'exceptZero'});
}
const sizeCells = (row, escape) => row.observed
  ? `<td>${row.withUnits ? formatCount(row.units) : '—'}${row.withUnits && row.withUnits < row.observed ? '<small>Partial batch count</small>' : ''}</td>
<td>${formatCount(row.input)}</td><td>${formatCount(row.output)}</td><td>${signedBytes(row.saved)}</td>
<td>${row.input ? escape(formatSavings(row.saved / row.input)) : '—'}</td>`
  : '<td colspan="5" class="muted-cell">No paired measurement</td>';

function renderSizeWindows(view, hasDiagnostics, escape) {
  const row = (title, data, total = false) => `<tr${total ? ' class="group-row"' : ''}><th scope="row">${escape(title)}</th>${sizeCells(data, escape)}</tr>`;
  return `<section class="scope-heading" id="sizes-${view.mode}"><h2>Wasm size savings</h2>
<p>Each row compares matched byte counts from completed stages in the selected mode. Savings measure input minus output within each pass window. Windows can overlap and measure pass activity before final module layout and packaging.</p></section>
${view.sizes.map(window => {
    const scopes = [...window.apps, window.compiler, ...(hasDiagnostics ? [window.diagnostic] : [])];
    return `<details class="size-window"${window.combined.observed || window.diagnostic.observed ? ' open' : ''}><summary>${escape(window.label)}</summary>
<section><p>${escape(window.description || 'Recorded input and output bytes for this pass.')}</p>
${window.combined.observed && window.combined.withUnits < window.combined.observed ? '<p class="muted">Some byte pairs were recorded without a batch count.</p>' : ''}</section>
<div class="table-wrap"><table><thead><tr><th scope="col">Workload</th>${sizeSummaryHeader}</tr></thead><tbody>
${row('All applications and compiler', window.combined, true)}
${row('All applications total', window.total, true)}
${scopes.map(workload => row(workload.name, workload)).join('')}</tbody></table></div>
${scopes.filter(scope => scope.phases.length).map(scope => `<details class="phase-breakdown"><summary>${escape(scope.name)} size by phase</summary>
<div class="table-wrap"><table><thead><tr><th scope="col">Phase</th>${sizeSummaryHeader}</tr></thead><tbody>
${scope.phases.map(phase => row(phase.record.phase + ' · ' + (decisionModes[compilationMode(phase.record)] ?? compilationMode(phase.record)), phase)).join('')}
</tbody></table></div></details>`).join('')}</details>`;
  }).join('')}`;
}

export function renderDecisionViews(model, escape) {
  const {defaultMode, views, applicationCount, hasDiagnostics} = model;
  const reasons = new Set();
  for (const view of views) for (const group of view.groups) for (const row of group.decisions)
    for (const scope of [row.total, row.compiler, row.diagnostic])
      for (const [reason] of rankedReasons(scope)) reasons.add(reason);
  return `<section id="decisions"><h2>Optimization decisions</h2>
<p>Workload totals use completed stages in the selected mode. Applied rate divides total applications of a rule by all its applied and rejected decisions.</p>
<p>Candidate counts and rejection reasons show where a rule is rarely attempted or frequently blocked. Category and application totals sum decisions across passes, which can examine different kinds of candidates. Pass coverage counts examined and excluded compiler work separately.</p>
<p>Reusable worker ABI claims are shown by result type beside the unboxing categories. The Native scalar workers and calls category counts these claims in its decision totals.</p>
<p class="muted">A zero means the pass was observed. Unobserved entries include cached, skipped and uninstrumented stages. All modes includes each stage once. Native, Portable and Mixed filter by the recorded stage label. Counters cover all compilation within the stage, including incidental compilation in the hosting VM.</p></section>
<div class="toolbar" id="decision-controls" hidden>
<label>Mode <select id="decision-mode">${Object.entries(decisionModes).map(([mode, label]) => `<option value="${mode}"${mode === defaultMode ? ' selected' : ''}>${label}</option>`).join('')}</select></label>
<label>Sort <select id="decision-sort"><option value="rejected">Most rejected</option><option value="candidates">Most opportunities</option><option value="fewest">Fewest opportunities</option><option value="rate">Lowest applied rate</option><option value="name">Name</option></select></label>
<label>Rejection reason <select id="decision-reason"><option value="">All families</option>${[...reasons].sort()
    .map(reason => `<option value="${escape(reason)}">${escape(humanize(reason))}</option>`).join('')}</select></label>
<label>Catalog <input id="decision-catalog-search" type="search" placeholder="Find an optimization"></label></div>
${views.map(view => {
    const allMechanisms = view.groups.flatMap(group => group.decisions);
    const ruleIds = new Map(allMechanisms.map((row, index) => [row.family, `decision-${view.mode}-${index}`]));
    const measuredGroups = view.groups.filter(group => group.decisions.some(row => row.rules.length));
    const overviewGroups = view.groups.filter(group => group.workerResults?.length || measuredGroups.includes(group));
    const rules = allMechanisms.filter(row => row.rules.length);
    const categoryIds = new Map(measuredGroups.map((group, index) => [group.id, `category-${view.mode}-${index}`]));
    const worker = rules.find(row => row.family === 'reusable-scalar-worker');
    const overview = (row, count) => `${metricCells(row, escape)}<td>${topReason(row, escape)}</td><td>${count} / ${applicationCount}</td>`;
    const columns = summaryHeader + '<th scope="col">Top rejection reason</th><th scope="col">Applications observed</th>';
    const categoryRows = scope => overviewGroups.map(group => {
      const applications = scope === 'total', row = group[scope], workerResults = group.workerResults ?? [];
      const decisionId = categoryIds.get(group.id);
      const catalogId = `catalog-${view.mode}-${group.id}`;
      const link = (id, label) => `<a href="#${id}" data-open-details>${escape(label)}</a>`;
      const heading = applications || workerResults.length
        ? link((applications && decisionId) || catalogId, group.name) : escape(group.name);
      const catalogLink = applications && decisionId && workerResults.length ? `<small>${link(catalogId, 'Catalog')}</small>` : '';
      const workerId = `${ruleIds.get(worker.family)}${applications ? '' : '-compiler'}`;
      const variants = workerResults.length ? `<small>${link(workerId, 'Recorded reusable worker ABI claims')}${worker[scope].observed
        ? workerResults.map(variant => ` · ${escape(variant)} ${formatCount(worker[scope].appliedVariants.get(variant) ?? 0)}`).join('')
        : ' · Unobserved'}</small>` : '';
      const metrics = decisionId ? `${metricCells(row, escape)}<td>${topReason(row, escape)}</td>${applications
        ? `<td>${group.measuredApps} / ${applicationCount}</td>` : ''}`
        : `<td colspan="${applications ? 6 : 5}" class="muted-cell">No type-specific decision coverage</td>`;
      return `<tr ${rowAttributes(group.name, row, escape)}><th scope="row">${heading}${catalogLink}${variants}</th>${metrics}</tr>`;
    }).join('');
    return `<div data-decision-mode="${view.mode}"${view.mode === defaultMode ? '' : ' hidden'}>
<section><nav aria-label="Optimization report sections"><a href="#catalog-${view.mode}">Optimization catalog</a>
<a href="#sizes-${view.mode}">Wasm size savings</a></nav></section>
<section class="scope-heading"><h2>All applications</h2><small>${decisionModes[view.mode]}</small>${view.total.observed ? ''
      : '<p>No application decisions were observed in this mode.</p>'}</section>
<div class="table-wrap"><table><thead><tr><th scope="col">Category</th>${columns}</tr></thead>
<tbody><tr class="group-row"><th scope="row"><a href="#category-${view.mode}-all" data-open-details>All applications total</a></th>
${overview(view.total, view.measuredApps)}</tr></tbody>
<tbody data-sort-rows>${categoryRows('total')}</tbody></table></div>
<div class="table-wrap"><table><thead><tr><th scope="col">Optimization</th>${columns}</tr></thead>
<tbody data-sort-rows>${rules.map(row => `<tr ${rowAttributes(row.label, row.total, escape)} data-decision-filter>
<th scope="row"><a href="#${ruleIds.get(row.family)}" data-open-details>${escape(row.label)}</a><small>${escape(categoriesById.get(row.category)?.label)}</small></th>
${overview(row.total, row.measuredApps)}</tr>`).join('')}</tbody></table></div>
${renderCategoryApplications({name: 'All decisions', total: view.total, apps: view.apps, open: true}, `category-${view.mode}-all`, escape)}
${measuredGroups.map(group => renderCategoryApplications(group, categoryIds.get(group.id), escape)).join('')}
${renderCompilerWorkload(view, categoryRows('compiler'), rules, escape, ruleIds)}
${hasDiagnostics ? renderOtherScope(rules, 'diagnostic', 'Diagnostic workload', escape) : ''}
${renderSizeWindows(view, hasDiagnostics, escape)}
${renderCatalog(view, ruleIds, escape)}</div>`;
  }).join('')}`;
}

export const decisionViewScript = `
const modeSelect = document.getElementById('decision-mode');
const sortSelect = document.getElementById('decision-sort');
const reasonSelect = document.getElementById('decision-reason');
const catalogSearch = document.getElementById('decision-catalog-search');
const decisionViews = [...document.querySelectorAll('[data-decision-mode]')];
function filterCatalog(view) {
  const search = catalogSearch.value.trim().toLowerCase();
  for (const item of view.querySelectorAll('[data-catalog-item]'))
    item.hidden = !item.dataset.search.toLowerCase().includes(search);
  for (const category of view.querySelectorAll('[data-catalog-category]')) {
    category.hidden = !category.querySelector('[data-catalog-item]:not([hidden])');
    if (search && !category.hidden) category.open = true;
  }
}
function updateDecisions() {
  const mode = modeSelect.value, sort = sortSelect.value, reason = reasonSelect.value;
  for (const view of decisionViews) {
    view.hidden = view.dataset.decisionMode !== mode;
    if (view.hidden) continue;
    for (const group of view.querySelectorAll('[data-sort-rows]')) {
      const rows = [...group.rows];
      rows.sort((a, b) => {
        const nameOrder = a.dataset.sortName.localeCompare(b.dataset.sortName);
        if (sort === 'rate') {
          const rateA = Number(a.dataset.sortRate), rateB = Number(b.dataset.sortRate);
          return (rateA < 0 ? 2 : rateA) - (rateB < 0 ? 2 : rateB) || nameOrder;
        }
        if (sort === 'rejected') return Number(b.dataset.sortRejected) - Number(a.dataset.sortRejected) || nameOrder;
        if (sort === 'candidates') return Number(b.dataset.sortCandidates) - Number(a.dataset.sortCandidates) || nameOrder;
        if (sort === 'fewest') {
          const aCount = Number(a.dataset.sortCandidates), bCount = Number(b.dataset.sortCandidates);
          return (aCount < 0 ? Infinity : aCount) - (bCount < 0 ? Infinity : bCount) || nameOrder;
        }
        return nameOrder;
      });
      group.append(...rows);
    }
    for (const row of view.querySelectorAll('[data-decision-filter]'))
      row.hidden = !!reason && !row.dataset.reasons.split(' ').includes(reason);
    filterCatalog(view);
  }
}
document.getElementById('decision-controls').hidden = false;
for (const control of [modeSelect, sortSelect, reasonSelect]) control.addEventListener('change', updateDecisions);
catalogSearch.addEventListener('input', () => filterCatalog(decisionViews.find(view => !view.hidden)));
for (const link of document.querySelectorAll('[data-open-details]')) link.addEventListener('click', () => {
  const target = document.getElementById(link.hash.substring(1));
  const activeView = target.closest('[data-decision-mode]');
  if (target.hidden && target.matches('[data-decision-filter]')) {
    reasonSelect.value = '';
    updateDecisions();
  } else if (activeView && (target.hidden || target.closest('[data-catalog-item]')?.hidden)) {
    catalogSearch.value = '';
    filterCatalog(activeView);
  }
  for (let details = target.closest('details'); details; details = details.parentElement.closest('details'))
    details.open = true;
});
updateDecisions();
`;
