const cells = [...document.querySelectorAll('.cell')];
const status = document.querySelector('#status'), startup = document.querySelector('#startup');
const cancel = document.querySelector('#cancel');
const defaults = new Map(cells.map(cell => [cell.id, {
  source: cell.querySelector('textarea').value, mode: cell.querySelector('.mode').value}]));
const storageKey = 'miricl-coalton-tour-v1';
let worker, ready = false, active = null;
function saved() {try {return JSON.parse(localStorage.getItem(storageKey) ?? '{}');} catch {return {};}}
function save() {
  try {localStorage.setItem(storageKey, JSON.stringify(Object.fromEntries(cells.map(cell =>
    [cell.id, {source: cell.querySelector('textarea').value, mode: cell.querySelector('.mode').value}]))));} catch {}
}
function controls() {
  for (const cell of cells) cell.querySelector('.run').disabled = !ready || active !== null;
  cancel.hidden = active === null;
}
function result(cell, output, error = false) {
  const view = cell.querySelector('.result');
  view.textContent = output || '(No values)'; view.hidden = false; view.classList.toggle('error', error);
}
function run(cell) {
  if (!ready || active !== null) return;
  active = cell; controls(); save();
  status.textContent = `Evaluating ${cell.id.replace('-', ' ')}…`;
  cell.querySelector('.timing').textContent = 'Running…';
  result(cell, '');
  worker.postMessage({type: 'evaluate', id: cell.id,
    source: cell.querySelector('textarea').value, mode: cell.querySelector('.mode').value});
}
function restart() {
  worker?.terminate();
  if (active) {result(active, 'Evaluation cancelled. The session was restarted.', true); active.querySelector('.timing').textContent = '';}
  active = null; ready = false; controls();
  startup.textContent = ''; startup.hidden = true; status.textContent = 'Downloading Coalton…';
  worker = new Worker(new URL('./worker.mjs', import.meta.url), {type: 'module'});
  worker.onmessage = ({data}) => {
    if (data.type === 'status') status.textContent = data.message;
    if (data.type === 'output') {
      if (active) {
        const view = active.querySelector('.result');
        if (view.textContent === '(No values)') view.textContent = '';
        view.textContent += data.output;
      } else {startup.hidden = false; startup.textContent += data.output;}
    }
    if (data.type === 'ready') {
      ready = true; status.textContent = 'Ready';
      if (data.output) {startup.hidden = false; startup.textContent += data.output;}
      controls();
    }
    if (data.type === 'result' || data.type === 'error') {
      const cell = active;
      if (cell && data.id === cell.id) {
        const prior = cell.querySelector('.result').textContent;
        result(cell, (prior === '(No values)' ? '' : prior) + (data.output ?? '')
          + (data.type === 'error' ? '\n' + data.message : ''), data.type === 'error');
        cell.querySelector('.timing').textContent = data.milliseconds === undefined ? 'Error' : `${Math.round(data.milliseconds)} ms`;
        active = null; status.textContent = 'Ready';
      } else if (data.type === 'error') {
        ready = false; status.textContent = 'Coalton failed to start';
        startup.hidden = false; startup.textContent += [data.output, data.message, data.backtrace].filter(Boolean).join('\n');
      }
      controls();
    }
  };
  worker.onerror = event => {
    if (active) result(active, event.message, true);
    active = null; ready = false; status.textContent = 'Worker error — restart to retry'; controls();
  };
  worker.postMessage({type: 'start'});
}
const edits = saved();
for (const cell of cells) {
  const editor = cell.querySelector('textarea'), mode = cell.querySelector('.mode');
  if (typeof edits[cell.id]?.source === 'string') editor.value = edits[cell.id].source;
  if (['lisp', 'expression', 'definitions'].includes(edits[cell.id]?.mode)) mode.value = edits[cell.id].mode;
  editor.addEventListener('input', save); mode.addEventListener('change', save);
  editor.addEventListener('keydown', event => {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {event.preventDefault(); run(cell);}
  });
  cell.querySelector('.run').addEventListener('click', () => run(cell));
  cell.querySelector('.restore').addEventListener('click', () => {
    editor.value = defaults.get(cell.id).source; mode.value = defaults.get(cell.id).mode; save();
  });
}
document.querySelector('#restart').addEventListener('click', restart);
cancel.addEventListener('click', restart);
restart();
