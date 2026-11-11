// Debugger dialog shared by the standalone and Coalton REPLs.
export function createReplDebugger({send, onStatus, isBusy}) {
  let debuggerState;
  const debuggerSessions = new Map();
  const debuggerPane = document.getElementById('debugger');
  const debugFrame = document.getElementById('debug-frame');
  const debugThreads = document.getElementById('debug-threads');
  const debugViews = new Map();

  function debugView(sessionId = debuggerState?.sessionId) {
    if (!debugViews.has(sessionId)) debugViews.set(sessionId, {history: [], expression: '', arguments: ''});
    return debugViews.get(sessionId);
  }
  function saveDebugView() {
    if (!debuggerState) return;
    Object.assign(debugView(), {frame: debugFrame.value,
      expression: document.getElementById('debug-expression').value,
      arguments: document.getElementById('debug-arguments').value});
  }
  function showDebugFrame() {
    const frame = debuggerState?.frames.find(frame => frame.id === Number(debugFrame.value));
    document.getElementById('debug-form').textContent = frame?.form ?? '';
    document.getElementById('debug-locals').textContent = frame
      ? frame.locals.map(({name, value}) => `  ${name} = ${value}`).join('\n') : '';
    if (debuggerState) debugView().frame = debugFrame.value;
  }
  function showDebugHistory() {
    document.getElementById('debug-result').replaceChildren(...debugView().history.map(entry => {
      const item = document.createElement('div'); item.className = 'debug-evaluation';
      const input = document.createElement('pre'); input.className = 'input';
      input.textContent = `> ${entry.source}`;
      const result = document.createElement('pre');
      result.textContent = entry.result === undefined ? 'Evaluating…' : entry.result;
      item.append(input, result); return item;
    }));
  }
  function showDebugError() {
    const error = debugView().error;
    const message = document.getElementById('debug-command-error');
    message.hidden = !error;
    message.textContent = error ? `${error.op === 'restart' ? 'Restart failed' : 'Debugger command failed'}: ${error.message}` : '';
    document.getElementById('debug-arguments').setAttribute('aria-invalid', String(error?.op === 'restart'));
  }
  function updateDebugControls() {
    const waiting = !!debugView().command;
    for (const button of document.querySelectorAll('#debug-restarts button, [data-debug], #debug-abort'))
      button.disabled = waiting;
    document.getElementById('debug-evaluate').disabled = waiting || !debuggerState.frames.some(frame => frame.inspectable);
  }
  function debugCommand(command) {
    if (!debuggerState || debugView().command) return;
    const view = debugView();
    view.command = {...command, stopId: debuggerState.stopId};
    view.error = undefined;
    showDebugError();
    if (command.op === 'eval') {
      const entry = {source: command.source};
      view.history.push(entry); view.pending = entry;
      showDebugHistory();
    }
    updateDebugControls();
    send({type: 'debug-command', sessionId: debuggerState.sessionId,
      stopId: debuggerState.stopId, command});
  }
  function threadSessions() {
    const threads = new Map();
    for (const state of debuggerSessions.values()) {
      if (!threads.has(state.threadId) || threads.get(state.threadId).level < state.level)
        threads.set(state.threadId, state);
    }
    return [...threads.values()];
  }
  function showDebugger(state) {
    saveDebugView();
    const wasPaused = !!debuggerState;
    debuggerState = state;
    debuggerPane.hidden = !state;
    if (!state) {
      debugThreads.replaceChildren();
      if (debuggerPane.open) debuggerPane.close();
      if (wasPaused) onStatus(isBusy() ? 'Running…' : 'Ready');
      return;
    }
    const view = debugView();
    if (!debuggerPane.open) debuggerPane.showModal();
    document.getElementById('debug-title').textContent = `Debugger · level ${state.level}`;
    debugThreads.replaceChildren(...threadSessions().map(thread => {
      const button = document.createElement('button');
      button.id = `debug-thread-${thread.threadId}`;
      button.setAttribute('role', 'tab');
      button.setAttribute('aria-controls', 'debug-panel');
      button.setAttribute('aria-selected', String(thread.threadId === state.threadId));
      button.tabIndex = thread.threadId === state.threadId ? 0 : -1;
      button.textContent = `${thread.threadName} (#${thread.threadId})`;
      button.onclick = () => showDebugger(debuggerSessions.get(thread.sessionId));
      button.onkeydown = event => {
        const tabs = [...debugThreads.children], index = tabs.indexOf(button);
        const next = event.key === 'ArrowRight' ? (index + 1) % tabs.length
          : event.key === 'ArrowLeft' ? (index + tabs.length - 1) % tabs.length
          : event.key === 'Home' ? 0 : event.key === 'End' ? tabs.length - 1 : -1;
        if (next >= 0) { event.preventDefault(); tabs[next].click(); debugThreads.children[next].focus(); }
      };
      return button;
    }));
    document.getElementById('debug-panel').setAttribute('aria-labelledby', `debug-thread-${state.threadId}`);
    document.getElementById('debug-condition').textContent = `${state.condition.type}: ${state.condition.message}`;
    document.getElementById('debug-stack').textContent = state.backtrace;
    const inspectable = state.frames.filter(frame => frame.inspectable);
    debugFrame.replaceChildren(...inspectable.map(({id, name}) => {
      const option = document.createElement('option');
      option.value = String(id); option.textContent = `${String(id).padStart(2, ' ')}: ${name}`; return option;
    }));
    if (inspectable.some(frame => String(frame.id) === view.frame)) debugFrame.value = view.frame;
    document.getElementById('debug-unavailable').hidden = inspectable.length > 0;
    document.getElementById('debug-steps').hidden = !state.canStep;
    document.getElementById('debug-expression').value = view.expression;
    document.getElementById('debug-arguments').value = view.arguments;
    showDebugHistory();
    showDebugError();
    document.getElementById('debug-restarts').replaceChildren(...state.restarts.map(({id, name, description}) => {
      const button = document.createElement('button');
      button.textContent = `${String(id).padStart(2, ' ')}: ${name} — ${description}`;
      button.onclick = () => debugCommand({op: 'restart', frame: id, source: document.getElementById('debug-arguments').value});
      return button;
    }));
    showDebugFrame();
    updateDebugControls();
    onStatus(`Paused · ${state.threadName} · debugger level ${state.level}`);
  }
  function debuggerEvent(event) {
    let selected = debuggerState?.sessionId;
    if (event.type === 'stopped') {
      debuggerSessions.set(event.stop.sessionId, event.stop);
      const view = debugViews.get(event.stop.sessionId);
      if (view && event.stop.result) {
        const result = event.stop.result;
        if (view.pending) {
          view.pending.result = [result.output, result.error ? `Error: ${result.error}` : result.values.join('\n')]
            .filter(Boolean).join('\n');
          view.pending = undefined;
        } else if (result.error) {
          view.error = {op: view.command?.op ?? view.error?.op, message: result.error};
        }
        view.command = undefined;
      }
      if (selected === undefined || event.stop.parentSessionId === selected) selected = event.stop.sessionId;
    } else {
      debuggerSessions.delete(event.sessionId);
      if (selected === event.sessionId) selected = event.parentSessionId;
    }
    showDebugger(debuggerSessions.get(selected) ?? threadSessions()[0]);
    if (event.type === 'closed') debugViews.delete(event.sessionId);
  }
  debuggerPane.oncancel = event => event.preventDefault();
  debugFrame.onchange = showDebugFrame;
  document.getElementById('debug-abort').onclick = () => debugCommand({op: 'abort'});
  document.getElementById('debug-evaluate').onclick = () => debugCommand({op: 'eval',
    frame: Number(debugFrame.value), source: document.getElementById('debug-expression').value});
  for (const button of document.querySelectorAll('[data-debug]'))
    button.onclick = () => debugCommand({op: button.dataset.debug});

  return {
    reset() {showDebugger(undefined); debuggerSessions.clear(); debugViews.clear();},
    message(data) {
      if (data.type === 'debugger') {debuggerEvent(data.event); return true;}
      if (data.type === 'debug-command-error') {
          const view = debugViews.get(data.sessionId);
          // Lisp command errors also arrive in a refreshed stop. Ignore the
          // duplicate rejection, and replies belonging to an older command.
          if (!view?.command || view.command.stopId !== data.stopId) return true;
          if (view.pending) { view.pending.result = `Error: ${data.message}`; view.pending = undefined; }
          else view.error = {op: view.command.op, message: data.message};
          view.command = undefined;
          if (debuggerState?.sessionId === data.sessionId) {
            showDebugHistory(); showDebugError(); updateDebugControls();
          }

        return true;
      }
      if (['result', 'error'].includes(data.type) && !debuggerSessions.size) showDebugger(undefined);
      return false;
    },
  };
}
