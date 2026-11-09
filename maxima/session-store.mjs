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
