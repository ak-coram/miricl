import assert from 'node:assert/strict';
import {newSession, sessionStore, sharedSessionURL, sessionFromURL} from '../maxima/session-store.mjs';

const application = 'miricl.coalton.sessions.v1';
const modes = ['lisp', 'expression', 'definitions'];
const session = {...newSession(), name: 'Notebook λ 🙂 <script>literal</script>', entries: [
  {source: '(define greeting "世界")', mode: 'definitions', answers: '', replies: [],
    outputs: [{text: '世界\n', className: 'result'}], state: 'complete', milliseconds: 12, recorded: false},
  {source: 'prompt', mode: 'lisp', answers: 'positive;', replies: ['λ🙂'],
    outputs: [{text: 'A question?'}], state: 'error'},
  {source: '(loop)', mode: 'expression', outputs: [], state: 'running'},
]};
const href = 'https://ak-coram.github.io/miricl/coalton/?view=worksheet#old';
const url = sharedSessionURL(session, href, application);
assert.match(url, /\/miricl\/coalton\/\?view=worksheet#session=[A-Za-z0-9_-]+$/u);
const restored = sessionFromURL(url, application, modes);
assert.equal(restored.name, session.name);
assert.equal(restored.created, session.created);
assert.equal(restored.updated, session.updated);
assert.notEqual(restored.id, session.id);
assert.notEqual(sessionFromURL(url, application, modes).id, restored.id);
const {recorded, ...first} = session.entries[0];
assert.deepEqual(restored.entries[0], first);
assert.deepEqual(restored.entries[1].replies, ['λ🙂']);
assert.equal(restored.entries[1].answers, 'positive;');
assert.equal(restored.entries[1].outputs[0].className, '');
assert.equal(restored.entries[2].state, 'cancelled');
assert.equal(session.entries[2].state, 'running');
assert.equal(sessionFromURL(href, application, modes), null);
assert.throws(() => sessionFromURL(url, 'miricl.maxima.sessions.v1'), /another application/u);
assert.throws(() => sessionFromURL(url, application), /Invalid shared session/u);

const payloadURL = value => new URL('#session=' + Buffer.from(JSON.stringify(value)).toString('base64url'), href).href;
const payload = {version: 1, application, session};
assert.throws(() => sessionFromURL(payloadURL({...payload, version: 2}), application, modes), /version/u);
for (const name of [null, 42, {}])
  assert.throws(() => sessionFromURL(payloadURL({...payload, session: {...session, name}}), application, modes), /Invalid shared session/u);
for (const entry of [null, {}, {...first, source: []}, {...first, replies: [42]}, {...first, answers: {}},
  {...first, outputs: [{text: {}}]}, {...first, state: 'unknown'}, {...first, mode: 'unknown'}])
  assert.throws(() => sessionFromURL(payloadURL({...payload, session: {...session, entries: [entry]}}), application, modes), /Invalid shared session/u);
for (const encoded of ['', '!!!', 'A', '____', Buffer.from('{').toString('base64url')])
  assert.throws(() => sessionFromURL(new URL('#session=' + encoded, href).href, application, modes), /encoding/u);
const large = {...newSession(), name: 'Large notebook', entries: [{source: 'λ🙂'.repeat(100000),
  outputs: [{text: '世界'.repeat(100000)}], state: 'complete'}]};
assert.equal(sessionFromURL(sharedSessionURL(large, href, application), application).entries[0].source, large.entries[0].source);

let stored;
const store = sessionStore({getItem: () => stored, setItem: (_, value) => {stored = value;}}, assert.fail);
store.save(session); store.save(restored);
assert.equal(store.list().length, 2, 'Import preserves the original session, even when its name matches.');
assert.equal(store.current().id, restored.id);
assert.equal(sessionStore({getItem: () => stored}, assert.fail).current().name, session.name);
console.log('Session sharing: Unicode, transcript metadata, modes, prompts, new IDs, persistence, large payloads and malformed links pass.');
