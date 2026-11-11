export function sessionStore(storage, onError = () => {}, key = 'miricl.maxima.sessions.v1') {
  let sessions = [], active;
  try {
    const saved = JSON.parse(storage.getItem(key) ?? 'null');
    if (saved?.version === 1 && Array.isArray(saved.sessions)) {
      sessions = saved.sessions.filter(item => typeof item.id === 'string' && typeof item.name === 'string'
        && Array.isArray(item.entries)).map(item => ({...item, entries: item.entries
          .filter(entry => entry && typeof entry.source === 'string')
          .map(entry => ({...entry, answers: typeof entry.answers === 'string' ? entry.answers : '',
            replies: (Array.isArray(entry.replies) ? entry.replies : []).filter(text => typeof text === 'string'),
            outputs: (Array.isArray(entry.outputs) ? entry.outputs : []).filter(output => output && typeof output.text === 'string'),
          }))}));
      active = saved.active;
    }
  } catch (error) {onError(error);}
  const write = () => {
    try {storage.setItem(key, JSON.stringify({version: 1, active, sessions})); return true;}
    catch (error) {onError(error); return false;}
  };
  return {
    list: () => [...sessions].sort((a, b) => b.updated - a.updated),
    current: () => sessions.find(item => item.id === active),
    save(session) {
      const index = sessions.findIndex(item => item.id === session.id);
      if (index < 0) sessions.push(structuredClone(session)); else sessions[index] = structuredClone(session);
      active = session.id;
      return write();
    },
    select(id) {active = id; write(); return this.current();},
    remove(id) {sessions = sessions.filter(item => item.id !== id); if (active === id) active = undefined; write();},
  };
}
export function newSession() {
  return {id: crypto.randomUUID(), name: 'Untitled session', created: Date.now(), updated: Date.now(), entries: []};
}

// Only transcript data belongs in a share link. Local IDs and recording flags
// must not overwrite an existing session or hide inputs when the link is saved.
function sharedTranscript(session, modes) {
  const invalid = () => {throw new Error('Invalid shared session.');};
  if (!session || typeof session.name !== 'string' || !Array.isArray(session.entries)) invalid();
  const strings = value => {
    if (!Array.isArray(value) || value.some(text => typeof text !== 'string')) invalid();
    return [...value];
  };
  return {
    name: session.name,
    created: Number.isFinite(session.created) ? session.created : Date.now(),
    updated: Number.isFinite(session.updated) ? session.updated : Date.now(),
    entries: session.entries.map(entry => {
      if (!entry || typeof entry.source !== 'string'
          || !['complete', 'error', 'cancelled', 'running'].includes(entry.state)
          || (entry.answers !== undefined && typeof entry.answers !== 'string')
          || (entry.mode !== undefined && !modes.includes(entry.mode))
          || !Array.isArray(entry.outputs)) invalid();
      return {source: entry.source, answers: entry.answers ?? '', replies: strings(entry.replies ?? []),
        outputs: entry.outputs.map(output => {
          if (!output || typeof output.text !== 'string'
              || (output.className !== undefined && typeof output.className !== 'string')) invalid();
          return {text: output.text, className: output.className ?? ''};
        }),
        state: entry.state === 'running' ? 'cancelled' : entry.state,
        ...(entry.mode !== undefined ? {mode: entry.mode} : {}),
        ...(Number.isFinite(entry.milliseconds) ? {milliseconds: entry.milliseconds} : {}),
      };
    }),
  };
}

export function sharedSessionURL(session, href, application) {
  const payload = JSON.stringify({version: 1, application,
    session: sharedTranscript(session, ['lisp', 'expression', 'definitions'])});
  const bytes = new TextEncoder().encode(payload);
  let binary = '';
  // Avoid spreading an entire transcript onto the JavaScript call stack.
  for (let offset = 0; offset < bytes.length; offset += 8192)
    binary += String.fromCharCode(...bytes.subarray(offset, offset + 8192));
  const encoded = btoa(binary).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/u, '');
  const url = new URL(href);
  url.hash = `session=${encoded}`;
  return url.href;
}

export function sessionFromURL(href, application, modes = []) {
  const encoded = new URLSearchParams(new URL(href).hash.slice(1)).get('session');
  if (encoded === null) return null;
  if (!/^[A-Za-z0-9_-]+$/u.test(encoded)) throw new Error('Invalid shared session encoding.');
  let payload;
  try {
    const binary = atob(encoded.replaceAll('-', '+').replaceAll('_', '/'));
    payload = JSON.parse(new TextDecoder('utf-8', {fatal: true}).decode(Uint8Array.from(binary, char => char.charCodeAt(0))));
  } catch {throw new Error('Invalid shared session encoding.');}
  if (payload?.version !== 1) throw new Error('Unsupported shared session version.');
  if (payload.application !== application) throw new Error('This shared session belongs to another application.');
  return {...newSession(), ...sharedTranscript(payload.session, modes)};
}
