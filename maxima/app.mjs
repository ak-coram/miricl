import {examples, config} from './examples.mjs';
import {newSession, sessionStore} from './session-store.mjs';
const $ = selector => document.querySelector(selector);
const source = $('#source'), answers = $('#answers'), status = $('#status'), evaluate = $('#evaluate');
const history = $('#history'), prompt = $('#prompt'), answerText = $('#answer-text');
let storageError, storage;
try {storage = localStorage;} catch (error) {storageError = error;}
const store = sessionStore(storage ?? {getItem: () => null, setItem: () => {throw storageError;}}, error => {
  storageError = error;
  $('#save-status').textContent = 'Browser storage is unavailable. Export to keep this session.';
}, config.storageKey);
let current = structuredClone(store.current() ?? newSession());
for (const entry of current.entries) if (entry.state === 'running') entry.state = 'cancelled';
let worker, active, activeNode, ready = false, busy = false, viewing = current.entries.length > 0;
let recording = true, replay = null;
const setStatus = text => {status.textContent = text;};
function controls() {
  evaluate.disabled = !ready || busy || viewing;
  $('#resume').disabled = !ready || busy;
  $('#resume-bar').hidden = !viewing;
  $('#entry-count').textContent = `${current.entries.length} input${current.entries.length === 1 ? '' : 's'}`;
  $('#empty').hidden = current.entries.length > 0;
}
function savedList() {
  $('#sessions').replaceChildren();
  const sessions = store.list();
  if (!sessions.length) {const item = document.createElement('li'); item.textContent = 'No saved sessions yet.'; $('#sessions').append(item);}
  for (const session of sessions) {
    const item = document.createElement('li'), button = document.createElement('button');
    button.type = 'button'; button.textContent = session.name; button.setAttribute('aria-current', String(session.id === current.id));
    const detail = document.createElement('small');
    detail.textContent = `${session.entries.length} inputs · ${new Date(session.updated).toLocaleDateString()}`;
    button.append(detail); button.onclick = () => openSession(session.id); item.append(button); $('#sessions').append(item);
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
  const reuse = document.createElement('button'); reuse.type = 'button'; reuse.textContent = 'Edit'; reuse.setAttribute('aria-label', `Edit input ${index + 1}`);
  reuse.onclick = () => {source.value = entry.source; answers.value = entry.answers ?? ''; source.focus();};
  heading.append(label, reuse); node.append(heading); history.append(node); append(entry.source, 'input', node);
  for (const output of entry.outputs ?? []) append(output.text, output.className, node);
  if (entry.state === 'cancelled') append('Calculation cancelled.', 'error', node);
  if (entry.milliseconds !== undefined) {const time = document.createElement('div'); time.className = 'entry-time'; time.textContent = `${entry.milliseconds} ms`; node.append(time);}
  return node;
}
function render() {
  history.replaceChildren(); current.entries.forEach(renderEntry); $('#session-name').value = current.name; savedList(); controls();
}
function cancel() {
  if (active?.state === 'running') {active.state = 'cancelled'; append('Calculation cancelled.', 'error'); save();}
  active = null; activeNode = null; replay = null; busy = false; prompt.hidden = true;
}
function start(onReady) {
  worker?.terminate(); ready = false; busy = false; controls(); setStatus('Loading assets…');
  const running = worker = new Worker('./worker.mjs', {type: 'module'});
  running.onmessage = ({data}) => {
    if (worker !== running) return;
    if (data.type === 'status') setStatus(data.message);
    if (data.type === 'ready') {ready = true; controls(); setStatus(viewing ? 'Saved transcript — Resume to continue' : 'Ready'); onReady?.();}
    if (data.type === 'prompt') {
      if (replay) {
        const recorded = replay.entries[replay.index].replies?.[replay.replyIndex++];
        if (recorded !== undefined) {worker.postMessage({type: 'answer', text: recorded}); return;}
      }
      $('#question').textContent = data.question; prompt.hidden = false; answerText.value = ''; answerText.focus(); setStatus('Waiting for an answer');
    }
    if (data.type === 'output' && active && !replay && data.output) recordOutput(data.output);
    if (data.type === 'result') {
      if (replay) {replay.index++; replayNext(); return;}
      if (active) {
        if (data.output) recordOutput(data.output);
        for (const result of data.results) recordOutput(`(${result.label.toLowerCase()}) ${result.text}`, 'result');
        active.state = 'complete'; active.milliseconds = data.milliseconds;
        const time = document.createElement('div'); time.className = 'entry-time'; time.textContent = `${data.milliseconds} ms`; activeNode.append(time);
        active = null; activeNode = null; busy = false; save(); setStatus(`${data.milliseconds} ms`); source.focus();
      }
    }
    if (data.type === 'error') {
      const text = [data.output, data.message].filter(Boolean).join('\n');
      if (replay) {replay = null; busy = false; viewing = true; controls(); setStatus(`Resume stopped: ${data.message}`); return;}
      if (active) {recordOutput(text, 'error'); active.state = 'error'; active = null; activeNode = null; busy = false; save();}
      setStatus(data.message); controls();
    }
  };
  running.onerror = event => {ready = false; busy = false; setStatus(event.message); controls();};
  running.postMessage({type: 'start'});
}
function recordOutput(text, className = '') {
  const previous = active.outputs.at(-1);
  if (className === '' && previous?.className === className && activeNode.lastElementChild?.tagName === 'PRE') {
    previous.text += text; activeNode.lastElementChild.textContent = previous.text;
    history.scrollTop = history.scrollHeight;
  } else {active.outputs.push({text, className}); append(text, className);}
  save();
}
function openSession(id) {
  cancel(); const session = store.select(id); if (!session) return;
  current = structuredClone(session); viewing = current.entries.length > 0; render(); start();
}
function fresh() {cancel(); current = newSession(); viewing = false; source.value = ''; answers.value = ''; render(); start();}
function replayNext() {
  if (!replay) return;
  if (replay.index >= replay.entries.length) {replay = null; busy = false; viewing = false; controls(); setStatus('Session restored'); source.focus(); return;}
  const entry = replay.entries[replay.index]; replay.replyIndex = 0;
  setStatus(`Restoring input ${replay.index + 1}/${replay.entries.length}…`);
  worker.postMessage({type: 'evaluate', source: entry.source, answers: entry.answers ?? ''});
}
$('#resume').onclick = () => {
  cancel(); const entries = current.entries.filter(entry => entry.state === 'complete');
  start(() => {busy = true; replay = {entries, index: 0, replyIndex: 0}; controls(); replayNext();});
};
$('#form').onsubmit = event => {
  event.preventDefault(); if (evaluate.disabled || !source.value.trim()) return;
  active = {source: source.value, answers: answers.value, replies: [], outputs: [], state: 'running', recorded: recording};
  current.entries.push(active); activeNode = renderEntry(active, current.entries.length - 1);
  busy = true; save(); setStatus('Calculating…'); worker.postMessage({type: 'evaluate', source: active.source, answers: active.answers});
};
source.onkeydown = event => {if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {event.preventDefault(); $('#form').requestSubmit();}};
$('#new').onclick = fresh;
$('#restart').onclick = fresh;
$('#record').onclick = () => {recording = !recording; $('#record').setAttribute('aria-pressed', String(recording)); $('#record').textContent = recording ? 'Recording' : 'Record'; save();};
$('#session-name').oninput = () => {current.name = $('#session-name').value.trim() || 'Untitled session'; save();};
$('#remove').onclick = () => {store.remove(current.id); fresh();};
$('#export').onclick = () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify({version: 1, session: current}, null, 2)], {type: 'application/json'}));
  const link = document.createElement('a'); link.href = url; link.download = `${current.name.replace(/[^\p{L}\p{N}_-]+/gu, '-').slice(0, 80) || `${config.name.toLowerCase()}-session`}.json`;
  link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
};
function answer() {
  const text = answerText.value;
  if (replay) {replay.entries[replay.index].replies ??= []; replay.entries[replay.index].replies.push(text); save();}
  else if (active) {active.replies.push(text); save();}
  prompt.hidden = true; setStatus('Calculating…'); worker.postMessage({type: 'answer', text});
}
$('#answer').onclick = answer;
answerText.onkeydown = event => {if (event.key === 'Enter') {event.preventDefault(); answer();}};
for (const example of examples) {
  const button = document.createElement('button'); button.type = 'button'; button.className = 'example'; button.dataset.example = example.id;
  const category = document.createElement('span'); category.textContent = example.category; button.append(category, example.title);
  button.onclick = () => {
    source.value = example.code; answers.value = ''; $('#example-note').replaceChildren(document.createTextNode(example.note + ' '));
    const link = document.createElement('a'); link.href = `${config.manualBase}${example.manual}`; link.target = '_blank'; link.rel = 'noopener'; link.textContent = `Manual: ${example.source} ↗`;
    $('#example-note').append(link); $('#example-note').hidden = false; source.focus();
  };
  $('#examples').append(button);
}
window.addEventListener('pagehide', save);
render(); start();
