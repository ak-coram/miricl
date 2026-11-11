import {examples, config} from './examples.mjs';
import {newSession, sessionStore, sharedSessionURL, sessionFromURL} from './session-store.mjs';
const $ = selector => document.querySelector(selector);
const source = $('#source'), answers = $('#answers'), status = $('#status'), evaluate = $('#evaluate');
const mode = $('#mode');
const defaultMode = config.defaultMode ?? 'lisp';
function inputMode(entry) {return mode ? {mode: entry.mode ?? defaultMode} : {};}
function restoreInput(entry) {
  source.value = entry.source; answers.value = entry.answers ?? '';
  if (mode) mode.value = entry.mode ?? defaultMode;
  saveDraft(); source.focus();
}
function saveDraft() {
  if (!config.draftKey) return;
  try {localStorage.setItem(config.draftKey, JSON.stringify({source: source.value, ...inputMode({mode: mode?.value})}));} catch {}
}
if (config.draftKey) {
  try {
    const draft = JSON.parse(localStorage.getItem(config.draftKey) ?? 'null');
    if (typeof draft?.source === 'string') source.value = draft.source;
    if (mode && config.modes?.includes(draft?.mode)) mode.value = draft.mode;
  } catch {}
  source.addEventListener('input', saveDraft); mode?.addEventListener('change', saveDraft);
}
const history = $('#history'), prompt = $('#prompt'), answerText = $('#answer-text');
const startupPanel = $('#startup-panel');
function startupOutput(text) {
  if (!startupPanel || !text) return;
  const output = $('#startup-output');
  output.textContent = (output.textContent + text).slice(-12000);
  $('#startup-details').hidden = false;
}
function startupError(message, details = '') {
  if (!startupPanel) return;
  startupPanel.hidden = false; startupPanel.dataset.state = 'error';
  $('#startup-message').textContent = `${config.name} could not continue. Restart to try a fresh session.`;
  startupOutput([message, details].filter(Boolean).join('\n'));
  $('#startup-details').open = true;
}
let storageError, storage;
try {storage = localStorage;} catch (error) {storageError = error;}
const store = sessionStore(storage ?? {getItem: () => null, setItem: () => {throw storageError;}}, error => {
  storageError = error;
  $('#save-status').textContent = 'Browser storage is unavailable. Export to keep this session.';
}, config.storageKey);
let current = structuredClone(store.current() ?? newSession());
for (const entry of current.entries) if (entry.state === 'running') entry.state = 'cancelled';
let worker, active, activeNode, ready = false, busy = false, viewing = current.entries.length > 0;
let recording = true, replay = null, evaluated = false;
let starting = false, readyAction;
const debuggerUI = config.createDebugger?.({send: data => worker?.postMessage(data),
  onStatus: text => setStatus(text), isBusy: () => busy});
const inputHistory = [];
let inputHistoryIndex = 0, inputDraft = '';
function sendEvaluation(entry) {
  evaluated = true;
  worker.postMessage({type: 'evaluate', source: entry.source, answers: entry.answers ?? '', ...inputMode(entry)});
}
const setStatus = text => {status.textContent = text;};
function controls() {
  evaluate.disabled = !ready || busy || viewing;
  $('#resume').disabled = !ready || busy;
  $('#resume-bar').hidden = !viewing;
  $('#entry-count').textContent = `${current.entries.length} input${current.entries.length === 1 ? '' : 's'}`;
  $('#empty').hidden = current.entries.length > 0;
  if ($('#stop')) $('#stop').disabled = !busy;
  if ($('#clear')) $('#clear').disabled = busy;
}
function savedList() {
  $('#sessions').replaceChildren();
  const sessions = store.list();
  if (!sessions.length) {const item = document.createElement('li'); item.textContent = 'No saved sessions yet.'; $('#sessions').append(item);}
  for (const session of sessions) {
    const item = document.createElement('li'), button = document.createElement('button');
    button.className = 'session-open';
    button.type = 'button'; button.textContent = session.name; button.setAttribute('aria-current', String(session.id === current.id));
    const detail = document.createElement('small');
    detail.textContent = `${session.entries.length} inputs · ${new Date(session.updated).toLocaleDateString()}`;
    button.append(detail); button.onclick = () => openSession(session.id);
    const copy = document.createElement('button'); copy.type = 'button'; copy.className = 'session-copy'; copy.textContent = 'Copy link';
    copy.setAttribute('aria-label', `Copy link to ${session.name}`); copy.title = 'Copy share link';
    copy.onclick = () => copySessionLink(session);
    item.append(button, copy); $('#sessions').append(item);
  }
}
function save() {
  current.updated = Date.now();
  if (recording && current.entries.some(entry => entry.recorded !== false)
      && store.save({...current, entries: current.entries.filter(entry => entry.recorded !== false)})) $('#save-status').textContent = 'Saved in this browser.';
  else if (!storageError) $('#save-status').textContent = recording ? 'Recording is on.' : 'Recording is paused. Export to keep new inputs.';
  savedList(); controls();
}
function append(text, className = '', node = activeNode) {
  const pre = document.createElement('pre'); pre.textContent = text; pre.className = className; node.append(pre);
  history.scrollTop = history.scrollHeight;
}
function renderEntry(entry, index) {
  const node = document.createElement('li'), heading = document.createElement('div'); heading.className = 'entry-head';
  const label = document.createElement('span'); label.textContent = `input ${String(index + 1).padStart(2, '0')}`;
  if (mode) label.textContent += ` · ${config.modeLabels?.[entry.mode ?? defaultMode] ?? entry.mode ?? defaultMode}`;
  const reuse = document.createElement('button'); reuse.type = 'button'; reuse.textContent = 'Edit'; reuse.setAttribute('aria-label', `Edit input ${index + 1}`);
  reuse.onclick = () => restoreInput(entry);
  heading.append(label, reuse); node.append(heading); history.append(node); append(entry.source, 'input', node);
  for (const output of entry.outputs ?? []) append(output.text, output.className, node);
  if (entry.state === 'cancelled') append(config.cancelledMessage ?? 'Calculation cancelled.', 'error', node);
  if (entry.milliseconds !== undefined) {const time = document.createElement('div'); time.className = 'entry-time'; time.textContent = `${entry.milliseconds} ms`; node.append(time);}
  return node;
}
function render() {
  history.replaceChildren(); current.entries.forEach(renderEntry); $('#session-name').value = current.name; savedList(); controls();
}
function cancel() {
  if (active?.state === 'running') {active.state = 'cancelled'; append(config.cancelledMessage ?? 'Calculation cancelled.', 'error'); save();}
  active = null; activeNode = null; replay = null; readyAction = undefined; busy = false; prompt.hidden = true;
}
function start(onReady) {
  worker?.terminate(); debuggerUI?.reset(); ready = false; busy = false; evaluated = false; controls(); setStatus('Loading assets…');
  starting = true; readyAction = onReady;
  if (startupPanel) {
    startupPanel.hidden = false; startupPanel.dataset.state = 'loading';
    $('#startup-message').textContent = `Starting ${config.name}…`;
    $('#startup-output').textContent = ''; $('#startup-details').hidden = true; $('#startup-details').open = false;
  }
  const running = worker = config.createWorker ? config.createWorker() : new Worker('./worker.mjs', {type: 'module'});
  running.onmessage = ({data}) => {
    if (worker !== running) return;
    if (debuggerUI?.message(data)) return;
    if (data.type === 'status') {setStatus(data.message); if (startupPanel && !ready) $('#startup-message').textContent = data.message;}
    if (data.type === 'ready') {
      starting = false; ready = true; if (startupPanel) startupPanel.hidden = true;
      controls(); setStatus(viewing ? 'Saved transcript — Resume to continue' : 'Ready');
      const action = readyAction; readyAction = undefined; action?.();
    }
    if (data.type === 'prompt') {
      if (replay) {
        const recorded = replay.entries[replay.index].replies?.[replay.replyIndex++];
        if (recorded !== undefined) {worker.postMessage({type: 'answer', text: recorded}); return;}
      }
      $('#question').textContent = data.question; prompt.hidden = false; answerText.value = ''; answerText.focus(); setStatus('Waiting for an answer');
    }
    if (data.type === 'output' && data.output) {
      if (!ready) startupOutput(data.output);
      else if (active && !replay) recordOutput(data.output);
      else if (config.backgroundOutput && !replay && current.entries.length)
        recordOutput(data.output, '', current.entries.at(-1), history.lastElementChild);
    }
    if (data.type === 'result') {
      if (data.quit) {
        ready = false; running.terminate();
        if (replay) {replay = null; busy = false; viewing = true; controls(); setStatus('Session ended. Restart to continue.'); return;}
      }
      if (replay) {
        if (data.failed) {replay = null; busy = false; viewing = true; controls(); setStatus(`Resume stopped: ${config.rejectedMessage ?? 'Input failed.'}`);}
        else {replay.index++; replayNext();}
        return;
      }
      if (active) {
        if (data.output) recordOutput(data.output);
        for (const result of data.results ?? []) recordOutput(`(${result.label.toLowerCase()}) ${result.text}`, 'result');
        if (data.failed) recordOutput(data.errorMessage ?? config.rejectedMessage ?? 'Input failed.', 'error');
        if (data.notice) recordOutput(data.notice, 'notice');
        active.state = data.failed ? 'error' : 'complete'; active.milliseconds = data.milliseconds;
        const time = document.createElement('div'); time.className = 'entry-time'; time.textContent = `${data.milliseconds} ms`; activeNode.append(time);
        active = null; activeNode = null; busy = false; save(); setStatus(data.status ?? (data.failed ? (config.rejectedMessage ?? 'Input failed.') : `${data.milliseconds} ms`)); source.focus();
      }
    }
    if (data.type === 'error') {
      if (!ready || data.fatal) {starting = false; ready = false; readyAction = undefined; startupError(data.message, [data.output, data.backtrace].filter(Boolean).join('\n'));}
      if (replay) {replay = null; busy = false; viewing = true; controls(); setStatus(`Resume stopped: ${data.message}`); return;}
      if (active) {
        if (data.output) recordOutput(data.output);
        recordOutput(data.message, 'error');
        if (data.backtrace) recordOutput(data.backtrace, 'error');
        active.state = 'error'; active = null; activeNode = null; busy = false; save();
      }
      setStatus(data.message); controls();
    }
  };
  running.onerror = event => {
    if (worker !== running) return;
    starting = false; ready = false; readyAction = undefined; busy = false; replay = null;
    if (active) {recordOutput(event.message, 'error'); active.state = 'error'; active = null; activeNode = null; save();}
    startupError(event.message); setStatus(event.message); controls();
  };
  running.postMessage({type: 'start'});
}
function prepareSession() {
  // Reading a transcript does not touch the Lisp world. Reuse both an idle VM
  // and an in-progress startup until an input has actually been evaluated.
  if (worker && !evaluated && (ready || starting)) {
    controls(); if (ready) setStatus(viewing ? 'Saved transcript — Resume to continue' : 'Ready');
  } else start();
}
function recordOutput(text, className = '', entry = active, node = activeNode) {
  const previous = entry.outputs.at(-1);
  if (className === '' && previous?.className === className && node.lastElementChild?.tagName === 'PRE') {
    previous.text += text; node.lastElementChild.textContent = previous.text;
    history.scrollTop = history.scrollHeight;
  } else {entry.outputs.push({text, className}); append(text, className, node);}
  save();
}
function openSession(id) {
  cancel(); const session = store.select(id); if (!session) return;
  current = structuredClone(session); viewing = current.entries.length > 0; render(); prepareSession();
}
function fresh(forceRestart = false) {
  cancel(); current = newSession(); viewing = false; source.value = ''; answers.value = '';
  if (mode) mode.value = defaultMode;
  saveDraft(); render(); if (forceRestart) start(); else prepareSession();
}
function replayNext() {
  if (!replay) return;
  if (replay.index >= replay.entries.length) {replay = null; busy = false; viewing = false; controls(); setStatus('Session restored'); source.focus(); return;}
  const entry = replay.entries[replay.index]; replay.replyIndex = 0;
  setStatus(`Restoring input ${replay.index + 1}/${replay.entries.length}…`);
  sendEvaluation(entry);
}
$('#resume').onclick = () => {
  cancel(); const entries = current.entries.filter(entry => entry.state === 'complete');
  const restore = () => {busy = true; replay = {entries, index: 0, replyIndex: 0}; controls(); replayNext();};
  if (ready && !evaluated) restore(); else start(restore);
};
$('#form').onsubmit = event => {
  event.preventDefault(); if (evaluate.disabled || !source.value.trim()) return;
  const input = (config.allowSelection && source.value.slice(source.selectionStart, source.selectionEnd)) || source.value;
  if (!input.trim()) return;
  if (inputHistory.at(-1) !== input) inputHistory.push(input);
  inputHistoryIndex = inputHistory.length; inputDraft = source.value;
  active = {source: input, answers: answers.value, replies: [], outputs: [], state: 'running', recorded: recording};
  Object.assign(active, inputMode({mode: mode?.value})); saveDraft();
  current.entries.push(active); activeNode = renderEntry(active, current.entries.length - 1);
  busy = true; save(); setStatus(config.busyStatus ?? 'Calculating…');
  sendEvaluation(active);
};
source.onkeydown = event => {
  if (event.isComposing) return;
  if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {event.preventDefault(); $('#form').requestSubmit();}
  else if (config.inputHistory && event.altKey && ['ArrowUp', 'ArrowDown'].includes(event.key)) {
    event.preventDefault(); if (!inputHistory.length) return;
    if (inputHistoryIndex === inputHistory.length) inputDraft = source.value;
    inputHistoryIndex = Math.max(0, Math.min(inputHistory.length, inputHistoryIndex + (event.key === 'ArrowUp' ? -1 : 1)));
    source.value = inputHistoryIndex === inputHistory.length ? inputDraft : inputHistory[inputHistoryIndex];
    source.setSelectionRange(source.value.length, source.value.length); saveDraft();
  }
};
$('#new').onclick = () => fresh();
$('#restart').onclick = () => fresh(true);
if ($('#stop')) $('#stop').onclick = () => fresh(true);
if ($('#clear')) $('#clear').onclick = () => {
  if (busy) return;
  current.entries = []; viewing = false; render();
  if (recording) store.save(current);
};
$('#record').onclick = () => {recording = !recording; $('#record').setAttribute('aria-pressed', String(recording)); $('#record').textContent = recording ? 'Recording' : 'Record'; save();};
$('#session-name').oninput = () => {current.name = $('#session-name').value.trim() || 'Untitled session'; save();};
$('#remove').onclick = () => {store.remove(current.id); fresh();};
$('#export').onclick = () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify({version: 1, session: current}, null, 2)], {type: 'application/json'}));
  const link = document.createElement('a'); link.href = url; link.download = `${current.name.replace(/[^\p{L}\p{N}_-]+/gu, '-').slice(0, 80) || `${config.name.toLowerCase()}-session`}.json`;
  link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
};
async function copySessionLink(session) {
  const url = sharedSessionURL(session, location.href, config.storageKey);
  try {
    await navigator.clipboard.writeText(url);
    if ($('#share-link')) $('#share-link').hidden = true;
    $('#save-status').textContent = 'Session link copied.';
  } catch {
    let field = $('#share-link');
    if (!field) {
      field = document.createElement('textarea'); field.id = 'share-link'; field.readOnly = true;
      field.rows = 3; field.setAttribute('aria-label', 'Session share link'); $('#save-status').after(field);
    }
    field.hidden = false; field.value = url; field.focus(); field.select();
    $('#save-status').textContent = 'Select and copy this session link.';
  }
}
$('#share').onclick = () => copySessionLink(current);
function importSharedSession() {
  let session;
  try {session = sessionFromURL(location.href, config.storageKey, config.modes ?? []);}
  catch (error) {$('#save-status').textContent = error.message; return false;}
  if (!session) return false;
  cancel(); current = session; viewing = true;
  const saved = store.save(current);
  render();
  if (saved) {
    $('#save-status').textContent = 'Shared session saved in this browser. Resume to continue.';
    // Consume the fragment so reloading does not import another copy.
    const url = new URL(location.href), fragment = new URLSearchParams(url.hash.slice(1));
    fragment.delete('session'); url.hash = fragment.toString();
    window.history.replaceState(window.history.state, '', url);
  }
  prepareSession(); return true;
}
function answer() {
  const text = answerText.value;
  if (replay) {replay.entries[replay.index].replies ??= []; replay.entries[replay.index].replies.push(text); save();}
  else if (active) {active.replies.push(text); save();}
  prompt.hidden = true; setStatus(config.busyStatus ?? 'Calculating…'); worker.postMessage({type: 'answer', text});
}
$('#answer').onclick = answer;
answerText.onkeydown = event => {if (event.key === 'Enter') {event.preventDefault(); answer();}};
for (const example of examples) {
  const button = document.createElement('button'); button.type = 'button'; button.className = 'example'; button.dataset.example = example.id;
  const category = document.createElement('span'); category.textContent = example.category; button.append(category, example.title);
  button.onclick = () => {
    restoreInput({source: example.code, mode: example.mode});
    $('#example-note').replaceChildren(document.createTextNode(example.note + ' '));
    const link = document.createElement('a'); link.href = `${config.manualBase}${example.manual}`; link.target = '_blank'; link.rel = 'noopener'; link.textContent = 'Documentation ↗';
    $('#example-note').append(link); $('#example-note').hidden = false; source.focus();
  };
  $('#examples').append(button);
}
window.addEventListener('pagehide', save);
window.addEventListener('hashchange', importSharedSession);
render(); if (!importSharedSession()) start();
