// Opt-in native compiler diagnostics. Timing with collection enabled is not a
// replacement for the ordinary uninstrumented/disabled-path measurements.
export function enableCompilationStatistics(vm) {
  vm.evaluate(`(unless (fboundp 'miricl-compiler:compilation-statistics-report)
    (error "This image does not support compiler statistics."))
    (defparameter miricl-compiler::*compilation-statistics* (make-hash-table :test 'equal)) (values)`);
}

export function disableCompilationStatistics(vm) {
  vm.evaluate('(defparameter miricl-compiler::*compilation-statistics* nil) (values)');
}

export async function readCompilationStatistics(vm) {
  // DEFPARAMETER with an atomic initializer uses the base evaluator, even
  // when ordinary evaluations compile. Suspend collection before compiling
  // any report formatting, then restore it without counting the profiler.
  vm.evaluate(`(defparameter miricl-compiler::*statistics-report-snapshot* miricl-compiler::*compilation-statistics*)
    (defparameter miricl-compiler::*compilation-statistics* nil) (values)`);
  try {
  // Encode labels as code points: this keeps JSON escaping independent of
  // Lisp printer settings and works for arbitrary symbol names/Unicode.
  vm.evaluate(`(with-open-file (out "/__compiler-statistics.json" :direction :output :if-exists :supersede)
    (write-char #\\[ out)
    (let ((first t))
      (dolist (row (miricl-compiler:compilation-statistics-report miricl-compiler::*statistics-report-snapshot*))
        (unless first (write-char #\\, out)) (setq first nil)
        (write-char #\\[ out)
        (dolist (label (list (symbol-name (first row))
                            (if (second row) (princ-to-string (second row)) "")))
          (write-char #\\[ out)
          (loop for char across label for first = t then nil do
            (unless first (write-char #\\, out)) (format out "~D" (char-code char)))
          (write-string "]," out))
        (format out "~D]" (third row))))
    (write-char #\\] out)) (values)`);
  const rows = JSON.parse(new TextDecoder().decode(await vm.readFile('/__compiler-statistics.json')));
  const label = points => points.map(point => String.fromCodePoint(point)).join('').toLowerCase();
  return rows.map(([rule, detail, count]) => ({rule: label(rule), detail: label(detail), count}))
    .sort((a, b) => a.rule.localeCompare(b.rule) || a.detail.localeCompare(b.detail));
  } finally {
    vm.evaluate(`(defparameter miricl-compiler::*compilation-statistics* miricl-compiler::*statistics-report-snapshot*)
      (defparameter miricl-compiler::*statistics-report-snapshot* nil) (values)`);
  }
}

export async function writeCompilationStatistics(record, directory = process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY) {
  if (!directory) return;
  const fs = await import('node:fs/promises'), path = await import('node:path');
  const {randomUUID} = await import('node:crypto');
  const report = {schemaVersion: 1, kind: 'compiler-statistics', id: randomUUID(),
    generatedAt: new Date().toISOString(), commit: process.env.GITHUB_SHA ?? '',
    runId: process.env.GITHUB_RUN_ID ?? '', attempt: process.env.GITHUB_RUN_ATTEMPT ?? '',
    job: process.env.MIRICL_STATISTICS_JOB ?? process.env.GITHUB_JOB ?? 'local',
    node: process.version, ...record};
  await fs.mkdir(directory, {recursive: true});
  const label = `${report.workload}-${report.phase}`.replace(/[^a-z0-9_-]+/gi, '-').slice(0, 100);
  const file = path.join(directory, `${label}-${report.id}.json`);
  await fs.writeFile(file + '.tmp', JSON.stringify(report, null, 2) + '\n');
  await fs.rename(file + '.tmp', file);
  return report;
}

export async function collectCompilationStatistics(vm, metadata, operation) {
  if (!process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY) return operation();
  const marker = await writeCompilationStatistics({...metadata, status:'partial', counters:[],
    failure:'Stage started; final counters have not been received.'});
  let status = 'failed', failure, counters = [], elapsedMs;
  enableCompilationStatistics(vm);
  const started = performance.now();
  try {
    const result = await operation();
    status = 'complete';
    return result;
  } catch (error) {failure = error.message; throw error;}
  finally {
    elapsedMs = performance.now() - started;
    try {counters = await readCompilationStatistics(vm);}
    catch (error) {failure = [failure, `Counter collection: ${error.message}`].filter(Boolean).join('; '); status = 'partial';}
    finally {
      try {disableCompilationStatistics(vm);}
      catch (error) {failure = [failure, `Stopping collection: ${error.message}`].filter(Boolean).join('; '); status = 'partial';}
    }
    await writeCompilationStatistics({...marker, ...metadata, status, elapsedMs, generatedAt:new Date().toISOString(),
      timingIncludesCollection: true, counters, failure});
  }
}
