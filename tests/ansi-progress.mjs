const duration = milliseconds => {
  const seconds = Math.floor(milliseconds / 1000);
  const minutes = Math.floor(seconds / 60);
  return minutes ? `${minutes}m ${seconds % 60}s` : `${seconds}s`;
};
const fraction = (done, total) => `${done}/${total} (${total ? Math.floor(100 * done / total) : 0}%)`;

export function formatProgress(progress, elapsedMs = progress.elapsedMs) {
  const elapsed = `elapsed ${duration(elapsedMs)}`;
  const totals = `pass ${progress.passed}, fail ${progress.failed}, blocked ${progress.blocked}, timeout ${progress.timedOut}`;
  switch (progress.phase) {
    case 'initialize': return `ANSI starting WASM worker | ${elapsed}`;
    case 'load': return `ANSI loading ${fraction(progress.loadedForms, progress.totalForms)} forms | ${progress.loadErrors} load errors | ${elapsed} | ${progress.file}`;
    case 'recover': return `ANSI recovering ${fraction(progress.replayedOperations, progress.totalReplayOperations)} setup operations | ${progress.testsProcessed}/${progress.totalTests} tests finished | ${elapsed}`;
    case 'test': return `ANSI testing ${fraction(progress.testsProcessed, progress.totalTests)} | ${totals} | ${elapsed} | ${progress.test ?? progress.file}`;
    default: return `ANSI ${progress.phase} | ${progress.testsProcessed}/${progress.totalTests} tests | ${totals} | ${elapsed}`;
  }
}

// The timer keeps the display alive during a slow guest call or recovery.
// Redirected output uses periodic plain lines; terminals reuse one line.
export function progressReporter(stream = process.stderr) {
  let latest;
  const render = () => {
    if (!latest) return;
    const line = formatProgress(latest, Date.now() - latest.startedAt);
    if (stream.isTTY) stream.write(`\r\x1b[2K${line.slice(0, (stream.columns || 160) - 1)}`);
    else stream.write(`${line}\n`);
  };
  const timer = setInterval(render, stream.isTTY ? 1000 : 5000);
  return {
    update(progress) {
      const changedPhase = latest?.phase !== progress.phase;
      latest = {...progress};
      if (changedPhase) render();
    },
    stop() {
      clearInterval(timer);
      if (stream.isTTY && latest) stream.write('\n');
    },
  };
}
