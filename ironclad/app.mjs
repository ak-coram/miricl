const get = id => document.getElementById(id);
const suites = get('suites');
const rows = get('rows');
const runButton = get('run');
const stopButton = get('stop');
const exportButton = get('export');
const workload = get('workload');
const status = get('status');
const error = get('error');
const progress = get('progress');
const groupNames = new Map([
  ['ciphers', 'Ciphers'], ['digests', 'Digests'], ['macs', 'MACs'], ['modes', 'Cipher modes'],
  ['diffie-hellman', 'Key exchanges'], ['message-encryptions', 'Public-key encryption'],
  ['signatures', 'Signatures'], ['verifications', 'Signature verification'],
]);
const settings = {
  quick: {blocks: 1, iterations: 1, samples: 1},
  standard: {blocks: 4, iterations: 3, samples: 3},
  extended: {blocks: 32, iterations: 10, samples: 5},
};
const throughput = new Intl.NumberFormat(undefined, {maximumSignificantDigits: 3});
const results = new Map();
let cases = [];
let build;
let worker;
let ready = false;
let running = false;
let active;
let group = 'digests';
let done = 0;
let total = 0;
let failures = 0;

const key = ({group, name}) => `${group}/${name}`;
const isData = group => ['ciphers', 'digests', 'macs', 'modes'].includes(group);
const names = () => cases.filter(item => item.group === group);
const median = speeds => [...speeds].sort((a, b) => a - b)[Math.floor(speeds.length / 2)];
function speedLabel(value, group) {
  if (!isData(group)) return `${throughput.format(value)} ops/s`;
  if (value < 1048576) return `${throughput.format(value / 1024)} KiB/s`;
  return `${throughput.format(value / 1048576)} MiB/s`;
}
const textCell = (tag, value, className = '') => Object.assign(document.createElement(tag), {textContent: value, className});

function render() {
  const configurations = settings[workload.value];
  const amount = isData(group) ? `${configurations.blocks * 32} KiB` : `${configurations.iterations} operations`;
  get('method').textContent = `${amount} per sample · ${configurations.samples} retained ${configurations.samples === 1 ? 'sample' : 'samples'}`;
  runButton.disabled = !ready || running;
  stopButton.disabled = !running;
  workload.disabled = running || !ready;
  exportButton.disabled = ![...results.values()].some(item => item.speeds);
  for (const button of suites.querySelectorAll('button')) button.setAttribute('aria-current', String(button.dataset.group === group));
  get('group-title').textContent = groupNames.get(group);
  const groupCases = names();
  const peak = Math.max(1, ...groupCases.map(item => {
    const result = results.get(key(item));
    return result?.speeds ? median(result.speeds) : 0;
  }));
  const fragment = document.createDocumentFragment();
  for (const item of groupCases) {
    const id = key(item);
    const result = results.get(id);
    const row = document.createElement('tr');
    const label = textCell('th', item.name.toUpperCase());
    label.scope = 'row';
    const speed = result?.speeds ? median(result.speeds) : null;
    const inProgress = active === id;
    row.dataset.active = String(inProgress);
    row.append(label, textCell('td', inProgress ? 'Running…' : speed ? speedLabel(speed, group) : result?.message ? 'Failed' : '—', 'number'));
    const barCell = textCell('td', '', 'scale');
    if (speed) {
      const bar = document.createElement('div');
      bar.style.width = `${100 * speed / peak}%`;
      bar.setAttribute('aria-hidden', 'true');
      barCell.append(bar);
    }
    row.append(barCell, textCell('td', result?.speeds ? (isData(group) ? `${result.work / 1024} KiB` : `${result.work} ops`) : '—', 'work'));
    const action = document.createElement('td');
    const button = textCell('button', 'Run', 'row-action');
    button.type = 'button';
    button.disabled = !ready || running;
    button.setAttribute('aria-label', `Run ${item.name.toUpperCase()}`);
    button.addEventListener('click', () => start([item]));
    action.append(button);
    row.append(action);
    fragment.append(row);
    if (result?.message && !inProgress) {
      const diagnostic = document.createElement('tr');
      const cell = textCell('td', result.message, 'detail');
      cell.colSpan = 5;
      diagnostic.append(cell);
      fragment.append(diagnostic);
    }
  }
  rows.replaceChildren(fragment);
}

function populateSuites() {
  const items = [];
  for (const [id, title] of groupNames) {
    const count = cases.filter(item => item.group === id).length;
    if (!count) continue;
    const item = document.createElement('li');
    const button = textCell('button', title);
    button.type = 'button';
    button.dataset.group = id;
    button.setAttribute('aria-label', `${title}, ${count} cases`);
    button.append(textCell('span', String(count)));
    button.addEventListener('click', () => { group = id; render(); });
    item.append(button);
    items.push(item);
  }
  suites.replaceChildren(...items);
}

function showError(message) {
  error.textContent = message;
  error.hidden = false;
}

function connect() {
  ready = false;
  runButton.textContent = 'Run suite';
  status.textContent = 'Loading Ironclad…';
  error.hidden = true;
  const connection = new Worker(new URL('worker.mjs', import.meta.url), {type: 'module'});
  worker = connection;
  const failed = message => {
    connection.terminate();
    worker = undefined;
    ready = running = false;
    active = undefined;
    progress.hidden = true;
    status.textContent = 'Could not complete the run.';
    showError(message);
    render();
    runButton.textContent = 'Retry';
    runButton.disabled = false;
  };
  connection.onerror = event => { if (worker === connection) failed(event.message || 'Benchmark worker could not start.'); };
  connection.onmessage = ({data}) => {
    if (worker !== connection) return;
    switch (data.type) {
      case 'ready':
        ({cases, ...build} = data);
        get('revision').textContent = `Ironclad ${build.revision} · Native Wasm · MIRICL ${build.imageSha256.slice(0, 12)}`;
        populateSuites();
        ready = true;
        runButton.textContent = 'Run suite';
        status.textContent = 'Ready';
        break;
      case 'case-start':
        active = key(data);
        status.textContent = `${done + 1} / ${total} · ${data.name.toUpperCase()}`;
        break;
      case 'result':
      case 'case-error':
        if (data.type === 'case-error') failures++;
        results.set(key(data), {...data, measuredAt: new Date().toISOString()});
        active = undefined;
        progress.value = ++done;
        break;
      case 'done':
        running = false;
        status.textContent = `${done - failures} measured${failures ? ` · ${failures} failed` : ''}`;
        break;
      case 'error':
        failed(data.message);
        return;
    }
    render();
  };
  connection.postMessage({type: 'load'});
  render();
}

function start(selection) {
  if (!ready || running || !selection.length) return;
  done = failures = 0;
  total = selection.length;
  running = true;
  error.hidden = true;
  progress.hidden = false;
  progress.max = total;
  progress.value = 0;
  status.textContent = 'Starting benchmarks…';
  worker.postMessage({type: 'run', selection, ...settings[workload.value]});
  render();
}

runButton.addEventListener('click', () => worker ? start(names()) : connect());
stopButton.addEventListener('click', () => {
  worker.terminate();
  worker = undefined;
  active = undefined;
  ready = running = false;
  progress.hidden = true;
  status.textContent = `Stopped · ${done - failures} measured`;
  render();
  runButton.textContent = 'Reload worker';
  runButton.disabled = false;
});
workload.addEventListener('change', render);
exportButton.addEventListener('click', () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify({revision: build.revision, backend: build.backend,
    imageSha256: build.imageSha256, coreSha256: build.coreSha256,
    userAgent: navigator.userAgent, results: [...results.values()]}, null, 2) + '\n'], {type: 'application/json'}));
  const link = document.createElement('a');
  link.href = url;
  link.download = 'ironclad-benchmarks.json';
  link.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
});
connect();
