// Optional CPU profiling for compiler/application benchmarks. Attribute Wasm
// frames by module bytes and function index, including independently compiled
// modules whose indices overlap the runtime's indices.
import fs from 'node:fs/promises';
import path from 'node:path';
import inspector from 'node:inspector';
import {createHash} from 'node:crypto';

const hash = bytes => createHash('sha256').update(bytes).digest('hex');

function functionNames(owner, image) {
  const vm = owner.vm, types = [];
  const roots = Object.entries(vm).find(([name]) => name.startsWith('core-roots:'));
  if (!roots) return new Map();
  for (const [name, read] of Object.entries(vm)) {
    const [prefix, id, signature] = name.split(':');
    if (signature && ['core-array', 'core-read'].includes(prefix))
      types[Number(id)] = {signature, read: vm[`core-read:${id}`] ?? read,
        array: prefix === 'core-array', length: read};
  }
  const layout = object => types[vm['core-object-type'](object)];
  const fields = object => layout(object).read(object);
  const text = object => {
    const data = fields(object)[3], type = layout(data), result = [];
    for (let i = 0; i < type.length(data); i++) result.push(type.read(data, i));
    let value = '';
    for (let i = 0; i < result.length; i += 4096)
      value += String.fromCodePoint(...result.slice(i, i + 4096));
    return value;
  };
  const core = hash(owner.imageBase ?? image), modules = new Map([[core, new Map()]]), codes = new Map();
  for (const [name, value] of Object.entries(vm))
    if (typeof value === 'function') modules.get(core).set(value.name, name);
  const queue = [...roots[1]()], seen = new Set();
  for (let i = 0; i < queue.length; i++) {
    const object = queue[i];
    if (object === null || typeof object !== 'object' || seen.has(object)) continue;
    seen.add(object);
    const type = layout(object);
    if (!type) continue;
    if (type.array) {
      if (type.signature.toLowerCase() === 'r')
        for (let j = 0; j < type.length(object); j++) queue.push(type.read(object, j));
    } else {
      const values = type.read(object), entry = type.signature.indexOf('e');
      if (entry >= 0 && values[6] && owner.javascript.kind(values[6]) === 6) {
        const code = values[3];
        let digest = core;
        if (code !== null) {
          digest = codes.get(code);
          if (!digest) {
            const storage = layout(code), bytes = new Uint8Array(storage.length(code));
            for (let j = 0; j < bytes.length; j++) bytes[j] = storage.read(code, j);
            digest = hash(bytes); codes.set(code, digest);
          }
        }
        if (!modules.has(digest)) modules.set(digest, new Map());
        modules.get(digest).set(values[entry].name, text(fields(values[6])[4]));
      }
      for (let j = 0; j < values.length; j++)
        if (type.signature[j].toLowerCase() === 'r') queue.push(values[j]);
    }
  }
  return modules;
}

export async function startWasmProfile(vm, image, filename) {
  const session = new inspector.Session(); session.connect();
  const post = (method, params = {}) => new Promise((resolve, reject) =>
    session.post(method, params, (error, result) => error ? reject(error) : resolve(result)));
  await post('Profiler.enable');
  // Long application builds otherwise exceed the inspector's JSON message
  // limit before Profiler.stop can return its response.
  await post('Profiler.setSamplingInterval', {interval: 10000});
  await post('Profiler.start');
  return async () => {
    try {
      const {profile} = await post('Profiler.stop');
      await fs.mkdir(path.dirname(filename), {recursive: true});
      await fs.writeFile(filename + '.cpuprofile', JSON.stringify(profile));
      const modules = functionNames(vm, image), scripts = new Map();
      session.on('Debugger.scriptParsed', ({params}) => scripts.set(params.scriptId, params));
      await post('Debugger.enable');
      const nodes = new Map(profile.nodes.map(node => [node.id, node])), weights = new Map();
      profile.samples.forEach((id, i) => {
        const script = nodes.get(id).callFrame.scriptId;
        weights.set(script, (weights.get(script) ?? 0) + profile.timeDeltas[i]);
      });
      const names = new Map();
      for (const [id] of [...weights].sort((a, b) => b[1] - a[1]).slice(0, 128)) {
        if (!scripts.get(id)?.url.startsWith('wasm:')) continue;
        const {bytecode} = await post('Debugger.getScriptSource', {scriptId: id});
        if (bytecode) names.set(id, modules.get(hash(Buffer.from(bytecode, 'base64'))));
      }
      const self = new Map(), inclusive = new Map();
      for (const node of profile.nodes) {
        const frame = node.callFrame, index = frame.functionName.match(/^wasm-function\[(\d+)\]$/)?.[1];
        const name = index && names.get(frame.scriptId)?.get(index);
        if (name) frame.functionName = name;
        else if (index) frame.functionName += ` @ ${frame.url || frame.scriptId}`;
        for (const child of node.children ?? []) nodes.get(child).parent = node;
      }
      profile.samples.forEach((id, i) => {
        let node = nodes.get(id);
        const milliseconds = profile.timeDeltas[i] / 1000;
        self.set(node.callFrame.functionName, (self.get(node.callFrame.functionName) ?? 0) + milliseconds);
        const seen = new Set();
        while (node) {
          const name = node.callFrame.functionName;
          if (!seen.has(name)) inclusive.set(name, (inclusive.get(name) ?? 0) + milliseconds);
          seen.add(name); node = node.parent;
        }
      });
      for (const node of profile.nodes) delete node.parent;
      const ranked = table => [...table].sort((a, b) => b[1] - a[1]).slice(0, 80)
        .map(([name, milliseconds]) => ({name, milliseconds}));
      const summary = {self: ranked(self), inclusive: ranked(inclusive), modules: modules.size};
      await fs.writeFile(filename + '.cpuprofile', JSON.stringify(profile));
      await fs.writeFile(filename + '.json', JSON.stringify(summary, null, 2) + '\n');
      return summary;
    } finally {session.disconnect();}
  };
}
