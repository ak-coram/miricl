// Browser/Node adapter: all Lisp reading, evaluation, and printing happens in WASM.
// An exported, typed import wrapper can be placed in the core's service table.
// This leaves the core import-free and lets other hosts install native services.
const serviceName = [7, ...new TextEncoder().encode('service')];
const serviceBridge = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,7,1,96,2,127,124,1,124,
  2,16,1,4,104,111,115,116,...serviceName,0,0,
  7,11,1,...serviceName,0,0,
]));
const javascriptBridge = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,10,1,96,4,127,111,111,111,2,111,127,
  2,16,1,4,104,111,115,116,...serviceName,0,0,
  7,11,1,...serviceName,0,0,
]));
const nativeCompilerBridge = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,8,1,96,3,111,111,111,1,111,
  2,16,1,4,104,111,115,116,...serviceName,0,0,
  7,11,1,...serviceName,0,0,
]));

// JSPI preserves the evaluator's native Wasm stack while the common scheduler
// runs another Lisp thread. No Lisp objects leave this WebAssembly instance.
const threadBridge = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,4,1,96,0,0,
  2,16,1,4,104,111,115,116,...serviceName,0,0,
  7,11,1,...serviceName,0,0,
]));

class GreenThreads {
  constructor(owner) {
    this.owner = owner;
    this.vm = owner.vm;
    this.waiting = new Map();
    this.running = false;
    this.initialized = false;
    const dispatch = () => { this.scheduled = false; if (!this.closed) this.pump(); };
    if (typeof globalThis.setImmediate === 'function') {
      this.postTask = () => globalThis.setImmediate(dispatch);
    } else if (typeof MessageChannel === 'function') {
      this.channel = new MessageChannel();
      this.channel.port1.onmessage = dispatch;
      this.postTask = () => this.channel.port2.postMessage(null);
    } else {
      this.postTask = () => setTimeout(dispatch, 0);
    }
    const bridge = new WebAssembly.Instance(threadBridge, {
      host: {service: new WebAssembly.Suspending(() => this.suspend())}
    });
    this.vm['thread-services'].set(0, bridge.exports.service);
    this.run = WebAssembly.promising(this.vm['thread-run']);
    this.evaluateSource = WebAssembly.promising(this.vm['evaluate-source']);
  }
  startSegment() {
    this.segment = (this.segment ?? 0) + 1;
    this.running = true;
    this.owner.activeStart = performance.now();
    this.owner.sleepTime = 0;
  }
  endSegment() {
    if (this.owner.activeStart !== null)
      this.owner.runTime += performance.now() - this.owner.activeStart - this.owner.sleepTime;
    this.owner.activeStart = null;
    this.running = false;
  }
  flushOutput() {
    if (!this.owner.onOutput || !this.owner.javascript) return;
    const output = this.owner.javascript.link(8);
    if (output !== null) this.owner.onOutput(this.owner.javascript.text(output));
  }
  suspend() {
    const id = this.vm['thread-current-id']();
    this.endSegment();
    this.flushOutput();
    return new Promise(resolve => {
      this.waiting.set(id, resolve);
      this.schedule();
    });
  }
  close() {
    this.closed = true;
    clearTimeout(this.timer);
    this.waiting.clear();
    this.channel?.port1.close();
    this.channel?.port2.close();
  }
  schedule(delay = 0) {
    if (this.closed) return;
    if (this.timer !== undefined) clearTimeout(this.timer);
    if (delay <= 0) {
      if (!this.scheduled) { this.scheduled = true; this.postTask(); }
    } else {
      this.timer = setTimeout(() => { this.timer = undefined; if (!this.closed) this.pump(); }, Math.min(delay, 2147483647));
    }
  }
  pump() {
    if (this.running) return;
    const id = this.vm['thread-next']();
    if (id < 0) {
      const delay = this.vm['thread-delay']();
      if (delay >= 0) this.schedule(delay);
      return;
    }
    this.vm['thread-enter'](id);
    this.vm['thread-active'].value = 1;
    this.startSegment();
    const resume = this.waiting.get(id);
    if (resume) {
      this.waiting.delete(id);
      resume();
    } else {
      this.run(id).then(() => {
        this.endSegment();
        this.flushOutput();
        this.vm['thread-retire'](id);
        this.schedule();
      }, error => {
        this.endSegment();
        this.vm['thread-active'].value = 0;
        const failure = this.owner.failure(error);
        this.vm['thread-failed'](id);
        this.flushOutput();
        this.vm['thread-retire'](id);
        this.schedule();
        this.owner.onThreadError?.(failure);
      });
    }
  }
  async evaluate(source, {eof = false} = {}, restart = false) {
    if (this.closed) throw new Error('The interpreter has been closed.');
    if (this.main) throw new Error('An evaluation is already running.');
    if (this.owner.javascript?.depth > 0) throw new Error('Cannot start an evaluation inside a Lisp callback.');
    this.main = true;
    if (!this.initialized) {
      this.vm['thread-save']();
      this.initialized = true;
    }
    this.vm['thread-enter'](0);
    this.vm['thread-main-state'](0);
    this.vm['thread-active'].value = 1;
    const bytes = this.owner.encoder.encode(source);
    const missing = bytes.length - this.vm.memory.buffer.byteLength;
    if (missing > 0) this.vm.memory.grow(Math.ceil(missing / 65536));
    new Uint8Array(this.vm.memory.buffer).set(bytes);
    this.vm.error.value = 0;
    this.startSegment();
    try {
      const length = await this.evaluateSource(restart ? -1 : bytes.length);
      const status = this.vm['text-status'].value;
      if (eof && status === 1) throw new MiriclError('Incomplete input.', 16);
      return {output: this.owner.text(length), incomplete: status === 1, quit: status === 2};
    } catch (error) {
      if (error instanceof MiriclError) throw error;
      this.vm['thread-active'].value = 0;
      throw this.owner.failure(error);
    } finally {
      this.endSegment();
      this.vm['thread-save']();
      this.vm['thread-main-state'](1);
      this.vm['thread-active'].value = 1;
      this.main = false;
      this.schedule();
    }
  }
}

// Weak entries retain their payload through an ephemeron keyed by the weak
// target. In particular, a value-to-key cycle must not keep a weak key alive.
class WeakHashTables {
  constructor(host) {
    this.host = host;
    this.vm = host.vm;
    this.tables = new WeakMap();
    this.iterators = new WeakSet();
  }
  bridge(op, table, left = null, right = null) {
    return this.vm['weak-hash-bridge'](op, table, left, right);
  }
  mode(table) { return this.bridge(0, table); }
  create(table, weakness) {
    if (typeof WeakRef !== 'function' || typeof FinalizationRegistry !== 'function')
      throw new Error('This JavaScript host does not support weak references.');
    const name = this.host.text(this.host.link(7, weakness));
    if (!['KEY', 'VALUE'].includes(name)) throw new Error('Unsupported weak hash table weakness.');
    this.bridge(name === 'KEY' ? 3 : 4, table);
    this.state(table);
    return table;
  }
  state(table) {
    let state = this.tables.get(table);
    if (state) return state;
    const mode = this.mode(table);
    if (mode !== 1 && mode !== 2) throw new TypeError('Expected a weak hash table.');
    this.hasTables = true;
    state = {mode, buckets: new Map(), ephemerons: new WeakMap()};
    state.registry = new FinalizationRegistry(record => {
      record.active = false;
      const bucket = state.buckets.get(record.hash);
      if (!bucket) return;
      const remaining = bucket.filter(item => item.active);
      if (remaining.length) state.buckets.set(record.hash, remaining);
      else state.buckets.delete(record.hash);
    });
    this.tables.set(table, state);
    return state;
  }
  target(record) { return record.weak ? record.weak.deref() : record.target; }
  pair(state, record) {
    if (!record.active) return;
    if (!record.weak) return record.pair;
    const target = record.weak.deref();
    if (target === undefined) return;
    return state.ephemerons.get(target)?.get(record);
  }
  remove(state, record) {
    const target = this.target(record);
    if (record.weak && target !== undefined) {
      const entries = state.ephemerons.get(target);
      entries?.delete(record);
      if (!entries?.size) state.ephemerons.delete(target);
      state.registry.unregister(record);
    }
    record.active = false;
    record.pair = undefined;
    record.target = undefined;
  }
  find(table, key) {
    const state = this.state(table), hash = this.bridge(1, table, key);
    const bucket = state.buckets.get(hash);
    if (bucket) for (const record of bucket) {
      const pair = this.pair(state, record);
      if (!pair) {this.remove(state, record); continue;}
      if (pair[0] === key || this.bridge(2, table, key, pair[0]))
        return {state, hash, record, pair};
    }
    return {state, hash};
  }
  set(table, key, value) {
    const found = this.find(table, key), {state, hash} = found;
    let record = found.record;
    if (record) {key = found.pair[0]; this.remove(state, record);}
    else {
      record = {hash};
      const bucket = state.buckets.get(hash) ?? [];
      bucket.push(record); state.buckets.set(hash, bucket);
    }
    const target = state.mode === 1 ? key : value;
    const pair = [key, value];
    record.active = true;
    if (target !== null && (typeof target === 'object' || typeof target === 'function')) {
      record.weak = new WeakRef(target);
      let entries = state.ephemerons.get(target);
      if (!entries) state.ephemerons.set(target, entries = new Map());
      entries.set(record, pair);
      state.registry.register(target, record, record);
    } else {
      // NIL and immediate Wasm integers cannot be weakly collected.
      record.weak = undefined; record.target = target; record.pair = pair;
    }
    return value;
  }
  delete(table, key) {
    const {state, record, hash} = this.find(table, key);
    if (!record) return false;
    this.remove(state, record);
    const remaining = state.buckets.get(hash).filter(item => item.active);
    if (remaining.length) state.buckets.set(hash, remaining); else state.buckets.delete(hash);
    return true;
  }
  clear(table) {
    const state = this.state(table);
    for (const bucket of state.buckets.values()) for (const record of bucket) this.remove(state, record);
    state.buckets.clear();
    return table;
  }
  *entries(table) {
    const state = this.state(table);
    for (const bucket of state.buckets.values()) for (const record of bucket) {
      const pair = this.pair(state, record);
      if (pair) yield pair;
    }
  }
  count(table) {
    const state = this.state(table);
    let count = 0;
    for (const [hash, bucket] of state.buckets) {
      const live = [];
      for (const record of bucket) {
        if (this.pair(state, record)) {live.push(record); count++;}
        else this.remove(state, record);
      }
      if (live.length) state.buckets.set(hash, live); else state.buckets.delete(hash);
    }
    return count;
  }
  list(table) {
    let list = null;
    for (const [key, value] of this.entries(table))
      list = this.host.link(2, this.host.link(2, key, value), list);
    return list;
  }
  iterator(table) {
    const state = this.state(table);
    // Snapshot metadata, rather than keys or values, so an iterator doesn't
    // turn all its unvisited entries into strong references.
    const iterator = {state, records: [...state.buckets.values()].flat(), index: 0};
    this.iterators.add(iterator);
    return this.host.wrap(iterator);
  }
  next(value) {
    const iterator = this.host.unwrap(value);
    if (!this.iterators.has(iterator)) throw new TypeError('Expected a weak hash table iterator.');
    while (iterator.index < iterator.records.length) {
      const pair = this.pair(iterator.state, iterator.records[iterator.index++]);
      if (pair) return this.host.link(2, pair[0], pair[1]);
    }
    return null;
  }
  restore(table, entries) {
    for (let scan = entries; scan !== null; scan = this.host.link(1, scan)) {
      const entry = this.host.link(0, scan);
      this.set(table, this.host.link(0, entry), this.host.link(1, entry));
    }
    this.bridge(5, table);
  }
}

// The bridge retains GC references directly. No integer object registry or
// finalization protocol is needed, including for retained Lisp callbacks.
class JavaScriptHost {
  get depth() { return this.vm['javascript-depth'].value; }
  set depth(value) { this.vm['javascript-depth'].value = value; }

  constructor(owner, imports) {
    this.owner = owner;
    this.vm = owner.vm;
    this.imports = imports;
    this.callbacks = new WeakMap();
    this.bindings = new WeakMap();
    this.weakHashTables = new WeakHashTables(this);
    this.depth = 0;
    this.void = Symbol('void');
    this.null = this.link(4);
    this.undefined = this.link(5);
    this.true = this.link(6);
    const bridge = new WebAssembly.Instance(javascriptBridge, {host: {service: (...args) => this.service(...args)}});
    this.vm['javascript-services'].set(0, bridge.exports.service);
  }

  link(op, a = null, b = null) { return this.vm['javascript-link'](op, a, b); }
  kind(value) { return this.vm['javascript-info'](0, value); }
  wrap(value) { return this.vm['javascript-wrap'](value); }
  unwrap(value) { return this.vm['javascript-unwrap'](value); }
  bool(value) { return this.vm['javascript-make'](0, value ? 1 : 0, 0, 0); }
  number(value) { return this.vm['javascript-make'](1, 0, 0, value); }

  data(value) {
    const [start, length] = this.vm['javascript-data'](value);
    // CURSOR can be unaligned, so use DataView rather than a typed array view.
    return {view: new DataView(this.vm.memory.buffer, start, length * 4), length};
  }

  text(value) {
    const {view, length} = this.data(value);
    const chunks = [];
    for (let start = 0; start < length; start += 4096) {
      const codes = [];
      for (let i = start; i < Math.min(start + 4096, length); i++) codes.push(view.getUint32(i * 4, true));
      chunks.push(String.fromCodePoint(...codes));
    }
    return chunks.join('');
  }

  makeData(op, sign, data) {
    if (data.length > 0x3fffffff) throw new RangeError('JavaScript conversion is too large.');
    const start = this.vm['javascript-buffer'](data.length * 4);
    const view = new DataView(this.vm.memory.buffer, start, data.length * 4);
    data.forEach((code, i) => view.setUint32(i * 4, code, true));
    return this.vm['javascript-make'](op, sign, data.length, 0);
  }

  makeText(value) { return this.makeData(2, 0, Array.from(value, c => c.codePointAt(0))); }
  makeInteger(value) {
    const sign = value < 0n ? -1 : value > 0n ? 1 : 0;
    let magnitude = value < 0n ? -value : value;
    const digits = [];
    do { digits.push(Number(magnitude & 32767n)); magnitude >>= 15n; } while (magnitude);
    return this.makeData(3, sign, digits);
  }

  integer(value) {
    const {view, length} = this.data(value);
    let result = 0n;
    for (let i = length; i > 0; i--) result = (result << 15n) | BigInt(view.getUint32((i - 1) * 4, true));
    return this.vm['javascript-info'](2, value) < 0 ? -result : result;
  }

  toJS(value) {
    if (value === null) return false;
    if (value === this.true) return true;
    if (value === this.null) return null;
    if (value === this.undefined) return undefined;
    switch (this.kind(value)) {
      case 47: return this.unwrap(value);
      case 1: {
        const integer = this.integer(value);
        return integer >= -9007199254740991n && integer <= 9007199254740991n ? Number(integer) : integer;
      }
      case 2: case 3: case 4: return this.vm['javascript-info'](1, value);
      case 10: case 35: {
        // General/displaced character arrays use the runtime's STRING-CONTENTS.
        if (this.kind(value) === 35 && !this.vm['javascript-info'](3, value))
          throw new TypeError('Convert arrays explicitly with js:array.');
        return this.text(value);
      }
      default: throw new TypeError('Expected a JavaScript value, string, real, boolean, or JS null/undefined sentinel.');
    }
  }

  fromJS(value, original) {
    if (value === null) return this.null;
    switch (typeof value) {
      case 'undefined': return this.undefined;
      case 'boolean': return this.bool(value);
      case 'number': return this.number(value);
      case 'bigint': return this.makeInteger(value);
      case 'string': return this.makeText(value);
      default: return original ?? this.wrap(value);
    }
  }

  list(value, convert = x => this.toJS(x)) {
    const result = [];
    for (let tail = value; tail !== null; tail = this.link(1, tail)) result.push(convert(this.link(0, tail)));
    return result;
  }

  callback(function_) {
    let callback = this.callbacks.get(function_);
    if (callback) return callback;
    callback = (...args) => {
      const depth = this.depth;
      const nested = depth > 0;
      const parked = !nested && this.owner.threads?.initialized && this.vm['thread-callback-enter']
        ? this.vm['thread-callback-enter']() : null;
      this.depth = depth + 1;
      let result, outputText;
      try {
        let arguments_ = null;
        for (let i = args.length - 1; i >= 0; i--) arguments_ = this.link(2, this.wrap(args[i]), arguments_);
        result = this.toJS(this.link(3, function_, arguments_));
        if (!nested && this.owner.onOutput) {
          const output = this.link(8);
          if (output !== null) outputText = this.text(output);
        }
      } catch (cause) {
        if (nested) throw cause;
        throw this.owner.failure(cause);
      } finally {
        this.depth = depth;
        if (parked !== null) this.vm['thread-enter'](parked);
      }
      if (outputText) this.owner.onOutput(outputText);
      return result;
    };
    this.callbacks.set(function_, callback);
    return callback;
  }

  check(type, value) {
    if (type === 'VALUE') return value;
    if (type === 'STRING' && typeof value === 'string') return value;
    if (type === 'BOOLEAN' && typeof value === 'boolean') return value;
    if (['NUMBER', 'F32', 'F64'].includes(type) && typeof value === 'number' && Number.isFinite(value)) {
      const result = type === 'F32' ? Math.fround(value) : value;
      if (Number.isFinite(result)) return result;
    }
    if (['BIGINT', 'I64', 'I32', 'U32'].includes(type) &&
        (typeof value === 'bigint' || typeof value === 'number' && Number.isSafeInteger(value))) {
      const integer = BigInt(value);
      if (type === 'BIGINT' || type === 'I64' && integer >= -(1n << 63n) && integer < (1n << 63n)) return integer;
      if (type === 'I32' && integer >= -2147483648n && integer <= 2147483647n ||
          type === 'U32' && integer >= 0n && integer <= 4294967295n) return Number(integer);
    }
    throw new TypeError(`Value does not satisfy JavaScript binding type :${type.toLowerCase()}.`);
  }

  importCall(descriptor, arguments_) {
    let binding = this.bindings.get(descriptor);
    if (!binding) {
      const [namespace, name, parameters, result] = this.list(descriptor, x => x);
      const moduleName = this.text(namespace), memberName = this.text(name);
      const receiver = Object.hasOwn(this.imports, moduleName) ? this.imports[moduleName]
        : moduleName === '' ? globalThis : globalThis[moduleName];
      const function_ = receiver?.[memberName];
      if (typeof function_ !== 'function') throw new TypeError(`Missing JavaScript import ${moduleName}.${memberName}.`);
      const symbolName = x => this.text(this.link(7, x));
      binding = {receiver, function_, parameters: this.list(parameters, symbolName), result: symbolName(result)};
      this.bindings.set(descriptor, binding);
    }
    const args = this.list(arguments_, x => x);
    if (args.length !== binding.parameters.length) throw new TypeError('Wrong JavaScript import arity.');
    for (let i = 0; i < args.length; i++) {
      const type = binding.parameters[i];
      const numeric = ['NUMBER', 'F32', 'F64'].includes(type) && [1, 2, 3, 4].includes(this.kind(args[i]));
      args[i] = this.check(type, numeric ? this.vm['javascript-info'](1, args[i]) : this.toJS(args[i]));
    }
    const value = Reflect.apply(binding.function_, binding.receiver, args);
    if (binding.result === 'VOID') return this.void;
    const checked = this.check(binding.result, value);
    if (binding.result === 'VALUE') return this.wrap(checked);
    if (['I32', 'U32', 'I64', 'BIGINT'].includes(binding.result)) return this.makeInteger(BigInt(checked));
    if (binding.result === 'F32') return this.vm['javascript-make'](4, 0, 0, checked);
    return this.fromJS(checked);
  }

  service(op, a, b, c) {
    const depth = this.depth;
    this.depth = depth + 1;
    try {
      const value = this.operation(op, a, b, c);
      return value === this.void ? [null, 2] : [value, 0];
    } catch (cause) {
      // Lisp transfers must retain their target and unwind through Lisp cleanup.
      if (cause instanceof WebAssembly.Exception || cause instanceof WebAssembly.RuntimeError) throw cause;
      return [cause, 1];
    } finally { this.depth = depth; }
  }

  operation(op, a, b, c) {
    switch (op) {
      case 2: return this.wrap(a === null ? globalThis : globalThis[this.toJS(a)]);
      case 3: return this.wrap(this.toJS(a)[this.toJS(b)]);
      case 4: {
        if (!Reflect.set(this.toJS(a), this.toJS(b), this.toJS(c))) throw new TypeError('JavaScript property is not writable.');
        return c;
      }
      case 5: return this.wrap(Reflect.apply(this.toJS(a), this.toJS(b), this.list(c)));
      case 6: {
        const receiver = this.toJS(a);
        return this.wrap(Reflect.apply(receiver[this.toJS(b)], receiver, this.list(c)));
      }
      case 7: return this.wrap(Reflect.construct(this.toJS(a), this.list(c)));
      case 8: return this.kind(a) === 47 ? a : this.wrap(this.toJS(a));
      case 9: return this.fromJS(this.unwrap(a), a);
      case 10: return this.makeText(typeof this.toJS(a));
      case 11: return this.bool(this.toJS(a) === this.toJS(b));
      case 12: return this.wrap(this.callback(a));
      case 13: return this.wrap(this.list(a));
      case 14: return this.importCall(a, b);
      case 15: return this.weakHashTables.create(a, b);
      case 16: {
        const mode = this.weakHashTables.mode(a);
        return this.makeInteger(BigInt(mode));
      }
      case 17: {
        const pair = this.weakHashTables.find(a, b).pair;
        return pair ? this.link(2, pair[0], pair[1]) : null;
      }
      case 18: return this.weakHashTables.set(a, b, c);
      case 19: return this.bool(this.weakHashTables.delete(a, b));
      case 20: return this.makeInteger(BigInt(this.weakHashTables.count(a)));
      case 21: return this.weakHashTables.clear(a);
      case 22: return this.weakHashTables.list(a);
      case 23: return this.weakHashTables.iterator(a);
      case 24: return this.weakHashTables.next(a);
      default: throw new RangeError('Unknown JavaScript bridge operation.');
    }
  }
}

export class MiriclError extends Error {
  constructor(message, code, output = '', backtrace = '', cause) {
    super(message, {cause});
    this.name = 'MiriclError';
    this.code = code;
    this.output = output;
    this.backtrace = backtrace;
    if (backtrace) this.stack += `\n${backtrace}`;
  }
}

// A handle belongs to one paused activation, never to the frontend's selection.
class DebuggerStop {
  #owner;
  constructor(owner, snapshot) { this.#owner = owner; Object.assign(this, snapshot); }
  get snapshot() { return {...this}; }
  locals(frameId) {
    const frame = this.frames.find(frame => frame.id === frameId);
    this.#owner.assertDebugStop(this);
    if (!frame?.inspectable) return Promise.reject(new Error('This frame has no interpreter debugging information.'));
    return Promise.resolve(frame.locals);
  }
  evaluate(frameId, source) { return this.#owner.debugCommand(this, {op: 'eval', frame: frameId, source}); }
  invokeRestart(restartId, {argumentsSource = ''} = {}) {
    return this.#owner.debugCommand(this, {op: 'restart', frame: restartId, source: argumentsSource});
  }
  step(mode) { return this.#owner.debugCommand(this, {op: mode}); }
  resume() { return this.#owner.debugCommand(this, {op: 'resume'}); }
  abort() { return this.#owner.debugCommand(this, {op: 'abort'}); }
}

const coreSectionName = new TextEncoder().encode('miricl.core');

class CoreWriter {
  bytes = new Uint8Array(1024);
  position = 0;
  reserve(length) {
    const end = this.position + length;
    if (end <= this.bytes.length) return;
    const bytes = new Uint8Array(Math.max(end, this.bytes.length * 2));
    bytes.set(this.bytes);
    this.bytes = bytes;
  }
  append(bytes) {
    this.reserve(bytes.length);
    this.bytes.set(bytes, this.position);
    this.position += bytes.length;
  }
  word(value) {
    this.reserve(5);
    while (value >= 128) {
      this.bytes[this.position++] = (value & 127) | 128;
      value >>>= 7;
    }
    this.bytes[this.position++] = value;
  }
  wide(value) {
    this.word(value === 0n ? 0 : 1);
    if (value === 0n) return;
    this.reserve(8);
    new DataView(this.bytes.buffer).setBigInt64(this.position, value, true);
    this.position += 8;
  }
  result() {return this.bytes.subarray(0, this.position);}
}

class CoreReader {
  constructor(bytes) {
    this.bytes = bytes;
    this.view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    this.position = 0;
  }
  take(length) {
    const end = this.position + length;
    if (end > this.bytes.length) throw new Error('Truncated Miricl image.');
    const value = this.bytes.subarray(this.position, end);
    this.position = end;
    return value;
  }
  word() {
    const first = this.bytes[this.position++];
    if (first < 128) return first;
    if (first === undefined) throw new Error('Truncated Miricl image.');
    let value = first & 127;
    for (let shift = 7; shift <= 28; shift += 7) {
      const byte = this.bytes[this.position++];
      if (byte === undefined) throw new Error('Truncated Miricl image.');
      if (shift === 28 && byte > 15) throw new Error('Invalid integer in Miricl image.');
      value |= (byte & 127) << shift;
      if (byte < 128) return value >>> 0;
    }
    throw new Error('Invalid integer in Miricl image.');
  }
  wide() {
    const marker = this.word();
    if (marker === 0) return 0n;
    if (marker !== 1) throw new Error('Invalid numeric field in Miricl image.');
    if (this.position + 8 > this.bytes.length) throw new Error('Truncated Miricl image.');
    const value = this.view.getBigInt64(this.position, true);
    this.position += 8;
    return value;
  }
  array(type) {
    const length = this.word();
    const packed = type === 'i' ? this.word() : 0;
    if (packed > 1) throw new Error('Invalid array encoding in Miricl image.');
    if (length > this.bytes.length - this.position) throw new Error('Invalid array length in Miricl image.');
    return {length, bytes: packed ? this.take(length) : null};
  }
  fieldMask(length) {
    const mask = this.word();
    if (mask >= 2 ** length) throw new Error('Invalid object field mask in Miricl image.');
    return mask;
  }
  field(type) {return type === 'l' || type === 'L' ? this.wide() : this.word();}
}

function* wasmSections(bytes) {
  const reader = new CoreReader(bytes);
  reader.take(8);
  while (reader.position < bytes.length) {
    const start = reader.position;
    const kind = reader.word();
    const payload = reader.take(reader.word());
    yield {kind, payload, start, end: reader.position};
  }
}

function coreImageParts(bytes) {
  let payload, start, end;
  for (const section of wasmSections(bytes)) {
    if (section.kind !== 0) continue;
    const reader = new CoreReader(section.payload);
    const name = reader.take(reader.word());
    if (name.length !== coreSectionName.length || !name.every((byte, i) => byte === coreSectionName[i])) continue;
    if (payload) throw new Error('Duplicate Miricl image sections.');
    payload = section.payload.subarray(reader.position);
    ({start, end} = section);
  }
  if (!payload) return {base: bytes};
  const base = new Uint8Array(bytes.length - (end - start));
  base.set(bytes.subarray(0, start));
  base.set(bytes.subarray(end), start);
  return {base, payload};
}

const coreSigned = value => ((value << 1) ^ (value >> 31)) >>> 0;
const coreUnsigned = value => (value >>> 1) ^ -(value & 1);
const coreReference = type => type === 'r' || type === 'R';
// Reference IDs occupy 1..2^31-1; zero remains NIL. Rotate that domain around
// the owner and encode a signed distance, so nearby objects take fewer bytes.
// Wrapping keeps the complete original ID range representable in a u32 word.
const coreRelativeId = (id, owner) => {
  if (!owner) return id;
  let distance = id - owner;
  if (distance > 0x3fffffff) distance -= 0x7fffffff;
  else if (distance < -0x3fffffff) distance += 0x7fffffff;
  return coreSigned(distance) + 1;
};
const coreAbsoluteId = (id, owner) => {
  if (!owner) return id;
  let absolute = owner + coreUnsigned(id - 1);
  if (absolute <= 0) absolute += 0x7fffffff;
  else if (absolute > 0x7fffffff) absolute -= 0x7fffffff;
  return absolute;
};
const coreEntryIndex = entry => {
  const name = entry?.name;
  if (!/^\d+$/.test(name) || Number(name) > 0xffffffff)
    throw new Error('Missing Wasm function index in Miricl image.');
  return Number(name);
};

function coreLayout(vm) {
  const types = [];
  let roots;
  for (const [name, read] of Object.entries(vm)) {
    const [prefix, key, signature] = name.split(':');
    if (prefix === 'core-roots') roots = {signature: key, read};
    if (signature && (prefix === 'core-array' || prefix === 'core-read')) {
      types[Number(key)] = {signature, array: prefix === 'core-array', read: vm[`core-read:${key}`] ?? read,
        length: prefix === 'core-array' ? read : null, create: vm[`core-new:${key}`], set: vm[`core-set:${key}`],
        entry: signature.indexOf('e')};
    }
  }
  if (!roots) throw new Error('This runtime does not support Miricl images.');
  return {roots, types};
}

function weakImageEntries(vm, {roots, types}, weakTables) {
  const snapshots = new Map();
  if (!weakTables?.hasTables) return snapshots;
  const reachable = new Set(), queue = [], tables = [];
  function enqueue(value) {
    if (value === null || typeof value === 'number' || reachable.has(value)) return;
    reachable.add(value); queue.push(value);
  }
  roots.read().forEach((value, i) => {if (coreReference(roots.signature[i])) enqueue(value);});
  let cursor = 0;
  function trace() {
    while (cursor < queue.length) {
      const object = queue[cursor++], type = types[vm['core-object-type'](object)];
      if (!type) throw new Error('The image contains a thread or a host object that cannot be restored.');
      if (type.array) {
        if (coreReference(type.signature)) for (let i = 0; i < type.length(object); i++) enqueue(type.read(object, i));
      } else {
        const values = type.read(object);
        const weak = typeof values[0] === 'number' && (values[0] & 63) === 29 && values[1] < 0;
        if (weak) tables.push({object, mode: -values[1], pairs: [...weakTables.entries(object)], kept: new Set()});
        for (let i = 0; i < type.signature.length; i++)
          if (coreReference(type.signature[i]) && !(weak && (i === 5 || i === 6))) enqueue(values[i]);
      }
    }
  }
  trace();
  // Ephemeron values can make further keys reachable. Iterate to a fixed
  // point, preserving strong application roots and discarding weak-only cycles.
  let progress;
  do {
    progress = false;
    for (const table of tables) for (const pair of table.pairs) {
      if (table.kept.has(pair)) continue;
      const target = pair[table.mode === 1 ? 0 : 1];
      if (target !== null && typeof target !== 'number' && !reachable.has(target)) continue;
      table.kept.add(pair); enqueue(pair[0]); enqueue(pair[1]); progress = true;
    }
    trace();
  } while (progress);
  for (const table of tables) {
    let entries = null;
    for (const [key, value] of table.kept)
      entries = weakTables.host.link(2, weakTables.host.link(2, key, value), entries);
    snapshots.set(table.object, entries);
  }
  return snapshots;
}

function captureCore(vm, base, weakTables) {
  const {roots, types} = coreLayout(vm);
  const weakEntries = weakImageEntries(vm, {roots, types}, weakTables);
  const writer = new CoreWriter();
  const objects = [];
  const ids = new Map();
  const headers = types.map(() => new Map());
  let headerCount = 0;
  function writeHeader(kind, mask, prefix = 0) {
    const key = mask * 128 + prefix;
    const known = headers[kind].get(key);
    if (known !== undefined) {writer.word(known); return;}
    headers[kind].set(key, ++headerCount);
    writer.word(0);
    writer.word(kind);
    if (!types[kind].array) writer.word(mask);
    if (!types[kind].array && (mask & 1) && types[kind].signature[0].toLowerCase() === 'i')
      writer.word(prefix);
  }
  function field(type, value, owner = 0) {
    switch (type.toLowerCase()) {
      case 'r': {
        if (value === null) {writer.word(0); return;}
        if (typeof value === 'number') {writer.word(coreSigned(value) * 2 + 1); return;}
        let id = ids.get(value);
        if (id === undefined) {
          objects.push(value);
          id = objects.length;
          ids.set(value, id);
        }
        writer.word(coreRelativeId(id, owner) * 2);
        return;
      }
      case 'i': writer.word(coreSigned(value)); return;
      case 'l': writer.wide(value); return;
      case 'e': writer.word(coreEntryIndex(value)); return;
    }
  }
  writer.word(2);
  roots.read().forEach((value, i) => field(roots.signature[i], value));
  const countOffset = writer.position;
  writer.append(new Uint8Array(4));
  for (let i = 0; i < objects.length; i++) {
    const object = objects[i];
    const kind = vm['core-object-type'](object);
    const type = types[kind];
    if (!type) throw new Error('The image contains a thread or a host object that cannot be restored.');
    if (type.array) {
      writeHeader(kind, 0);
      const length = type.length(object);
      writer.word(length);
      if (type.signature === 'i') {
        // Strings, HIR and native code share the runtime's i32 storage type.
        // Pack octets without signed-integer expansion; preserve arbitrary
        // i32 arrays with the ordinary encoding when any element is wider.
        const start = writer.position;
        writer.word(1);
        writer.reserve(length);
        let index = 0;
        for (; index < length; index++) {
          const value = type.read(object, index);
          if ((value >>> 8) !== 0) break;
          writer.bytes[writer.position++] = value;
        }
        if (index === length) continue;
        writer.position = start;
        writer.word(0);
      }
      for (let index = 0; index < length; index++) field(type.signature, type.read(object, index), i + 1);
    } else {
      const values = type.read(object);
      if (weakEntries.has(object)) {values[5] = weakEntries.get(object); values[6] = null;}
      // Most object slots are NIL or zero. Preserve exact numeric bits (so
      // negative zero is present), and always retain native entry identities.
      let mask = 0;
      for (let index = 0; index < type.signature.length; index++) {
        const kind = type.signature[index], value = values[index];
        if (coreReference(kind) ? value !== null : kind === 'e' || (value !== 0 && value !== 0n))
          mask += 2 ** index;
      }
      if (type.signature.length > 32) throw new Error('Too many object fields in Miricl image layout.');
      // A common first i32 field is the runtime tag/identity word. Factor
      // its repeated low bits into the header while preserving every bit.
      const scalar = (mask & 1) && type.signature[0].toLowerCase() === 'i';
      const first = scalar ? coreSigned(values[0]) : 0;
      writeHeader(kind, mask, first & 127);
      if (scalar) writer.word(first >>> 7);
      for (let index = scalar ? 1 : 0; index < type.signature.length; index++)
        if (mask & (1 << index)) field(type.signature[index], values[index], i + 1);
    }
  }
  new DataView(writer.bytes.buffer).setUint32(countOffset, objects.length, true);
  const header = new CoreWriter();
  header.word(0);
  header.word(1 + coreSectionName.length + writer.position);
  header.word(coreSectionName.length);
  header.append(coreSectionName);
  const output = new Uint8Array(base.length + header.position + writer.position);
  output.set(base);
  output.set(header.result(), base.length);
  output.set(writer.result(), base.length + header.position);
  return output;
}

function coreNativeEntries(vm, bytes, indices) {
  for (const {kind, payload, start, end} of wasmSections(bytes)) {
    if (kind !== 7) continue;
    const previous = new CoreReader(payload);
    const count = previous.word();
    const exports = new CoreWriter();
    exports.word(count + indices.size);
    exports.append(payload.subarray(previous.position));
    for (const index of indices) {
      const name = new TextEncoder().encode(`core-entry-${index}`);
      exports.word(name.length);
      exports.append(name);
      exports.word(0);
      exports.word(index);
    }
    const patched = new CoreWriter();
    patched.append(bytes.subarray(0, start));
    patched.word(7);
    patched.word(exports.position);
    patched.append(exports.result());
    patched.append(bytes.subarray(end));
    const module = new WebAssembly.Module(patched.result());
    const instance = new WebAssembly.Instance(module, {'miricl.runtime': vm});
    return new Map([...indices].map(index => [index, instance.exports[`core-entry-${index}`]]));
  }
  throw new Error('A saved native module has no export section.');
}

function restoreCore(vm, payload) {
  const {roots, types} = coreLayout(vm);
  const reader = new CoreReader(payload);
  const descriptor = (type, mask, prefix) => {
    const {signature, entry} = type;
    if (entry >= 0 && !(mask & (1 << entry))) throw new Error('Missing native entry in Miricl image.');
    return {...type, prefix,
      defaults: Array.from(signature, kind => kind === 'l' || kind === 'L' ? 0n : 0),
      slots: Array.from(signature, (_, i) => i).filter(i => mask & (1 << i))};
  };
  const scalar = (word, prefix) => {
    if (word > 0x1ffffff) throw new Error('Invalid scalar field in Miricl image.');
    return word * 128 + prefix;
  };
  const fields = ({signature, defaults, slots, prefix}, owner = 0) => {
    const values = defaults.slice();
    for (const i of slots) {
      let value = reader.field(signature[i]);
      if (coreReference(signature[i]) && value && !(value & 1))
        value = coreAbsoluteId(value >>> 1, owner) * 2;
      values[i] = value;
    }
    if (prefix !== undefined) values[0] = scalar(values[0], prefix);
    return values;
  };
  if (reader.word() !== 2) throw new Error('Unsupported Miricl image format.');
  const rootValues = fields(descriptor(roots, 0xffffffff));
  const size = reader.take(4);
  const count = new DataView(size.buffer, size.byteOffset, 4).getUint32(0, true);
  if (count > payload.length - reader.position) throw new Error('Invalid object count in Miricl image.');
  const offsets = new Uint32Array(count + 1);
  const kinds = new Uint32Array(count + 1);
  const headers = [null];
  const objects = new Array(count + 1);
  const native = new Map();
  for (let id = 1; id <= count; id++) {
    let kind = reader.word();
    if (!kind) {
      const type = types[reader.word()];
      if (!type) throw new Error('Invalid object type in Miricl image.');
      const mask = type.array ? 0 : reader.fieldMask(type.signature.length);
      const prefix = !type.array && (mask & 1) && type.signature[0].toLowerCase() === 'i' ? reader.word() : undefined;
      if (prefix > 127) throw new Error('Invalid scalar prefix in Miricl image.');
      kind = headers.length;
      headers.push(descriptor(type, mask, prefix));
    }
    const type = headers[kind];
    if (!type) throw new Error('Invalid object header in Miricl image.');
    kinds[id] = kind;
    offsets[id] = reader.position;
    if (type.array) {
      const {length, bytes} = reader.array(type.signature);
      if (!bytes) for (let i = 0; i < length; i++) reader.field(type.signature);
      objects[id] = type.create(length);
    } else if (type.entry >= 0) {
      const values = fields(type, id);
      // Native closures carry the code bytes in the common object's digits slot.
      const code = values[3];
      if (!native.has(code)) native.set(code, new Set());
      native.get(code).add(values[type.entry]);
    } else {
      for (const i of type.slots) {
        const value = reader.field(type.signature[i]);
        if (i === 0 && type.prefix !== undefined) scalar(value, type.prefix);
      }
    }
  }
  if (reader.position !== payload.length) throw new Error('Unexpected data after Miricl image.');
  for (const [code, indices] of native) {
    if (code === 0) {
      const table = vm['core-compiler-functions'];
      const entries = new Map();
      for (let i = 0; table && i < table.length; i++) {
        const entry = table.get(i);
        entries.set(coreEntryIndex(entry), entry);
      }
      native.set(code, entries);
    } else {
      const id = code >>> 1;
      if ((code & 1) || id > count || !headers[kinds[id]].array || headers[kinds[id]].signature !== 'i')
        throw new Error('Invalid native code reference in Miricl image.');
      reader.position = offsets[id];
      const array = reader.array('i');
      const bytes = array.bytes ?? new Uint8Array(array.length);
      if (!array.bytes) for (let i = 0; i < bytes.length; i++) {
        const value = coreUnsigned(reader.word());
        if (value < 0 || value > 255) throw new Error('Invalid native code byte in Miricl image.');
        bytes[i] = value;
      }
      native.set(code, coreNativeEntries(vm, bytes, indices));
    }
  }
  function field(type, value, values, owner = 0) {
    if (coreReference(type)) {
      if (value === 0) return null;
      if (value & 1) return coreUnsigned(value >>> 1);
      const object = objects[coreAbsoluteId(value >>> 1, owner)];
      if (object === undefined) throw new Error('Invalid object reference in Miricl image.');
      return object;
    }
    if (type === 'i' || type === 'I') return coreUnsigned(value);
    if (type === 'e') {
      const entry = native.get(values[3])?.get(value);
      if (!entry) throw new Error('Invalid native entry in Miricl image.');
      return entry;
    }
    return value;
  }
  const constructing = new Uint8Array(count + 1);
  const pending = [];
  for (let id = 1; id <= count; id++) {
    if (objects[id] !== undefined) continue;
    pending.push(id);
    while (pending.length) {
      const next = pending.at(-1), type = headers[kinds[next]];
      constructing[next] = 1;
      reader.position = offsets[next];
      const values = fields(type, next);
      let dependency = 0;
      for (let i = 0; i < values.length; i++) {
        const reference = values[i];
        if (type.signature[i] !== 'r' || reference === 0 || reference & 1) continue;
        if (objects[reference >>> 1] !== undefined) continue;
        dependency = reference >>> 1;
        if (dependency > count || constructing[dependency]) throw new Error('Invalid immutable reference in Miricl image.');
        break;
      }
      if (dependency) {pending.push(dependency); continue;}
      const args = [];
      for (let i = 0; i < values.length; i++) {
        const kind = type.signature[i];
        if (kind === kind.toLowerCase()) args.push(field(kind, values[i], values));
      }
      objects[next] = type.create(...args);
      constructing[next] = 0;
      pending.pop();
    }
  }
  for (let id = 1; id <= count; id++) {
    const type = headers[kinds[id]], object = objects[id];
    reader.position = offsets[id];
    if (type.array) {
      const {length, bytes} = reader.array(type.signature);
      if (bytes) for (let i = 0; i < length; i++) type.set(object, i, bytes[i]);
      else for (let i = 0; i < length; i++)
        type.set(object, i, field(type.signature, reader.field(type.signature), undefined, id));
    } else {
      const values = fields(type, id), args = [];
      for (let i = 0; i < values.length; i++) {
        const kind = type.signature[i];
        if (kind !== kind.toLowerCase()) args.push(field(kind, values[i]));
      }
      type.set(object, ...args);
    }
  }
  vm['core-set-roots'](...rootValues.map((value, i) => field(roots.signature[i], value)));
  const weak = [];
  for (let id = 1; id <= count; id++) {
    const type = headers[kinds[id]];
    if (type.array) continue;
    const object = objects[id], values = type.read(object);
    if (typeof values[0] === 'number' && (values[0] & 63) === 29 && values[1] < 0)
      weak.push({table: object, entries: values[5]});
  }
  return weak;
}

export class Miricl {
  constructor(instance, {imports = {}, onOutput, onThreadError, onDebugger} = {}, payload) {
    this.vm = instance.exports;
    this.onOutput = onOutput;
    this.onThreadError = onThreadError;
    this.onDebugger = onDebugger;
    this.debugSessions = new Map();
    this.encoder = new TextEncoder();
    this.decoder = new TextDecoder('utf-8', {fatal: true});
    this.realOrigin = performance.now();
    this.runTime = 0;
    this.activeStart = null;
    this.sleepTime = 0;
    const cpuUsage = globalThis.process?.cpuUsage;
    this.cpuUsage = cpuUsage && (() => {const {user, system} = cpuUsage(); return (user + system) / 1000;});
    this.cpuOrigin = this.cpuUsage?.() ?? 0;
    if (this.vm['host-services']) {
      const bridge = new WebAssembly.Instance(serviceBridge, {host: {service: (op, value) => this.hostService(op, value)}});
      this.vm['host-services'].set(0, bridge.exports.service);
    }
    this.vm.initialize();
    const restoredWeak = payload ? restoreCore(this.vm, new Uint8Array(payload)) : [];
    if (this.vm['native-services']) {
      this.nativeModules = new WeakMap();
      const bridge = new WebAssembly.Instance(nativeCompilerBridge, {host: {service: (code, literals, name) => {
        const storage = this.vm['native-code-storage']?.(code) ?? code;
        let compiled = this.nativeModules.get(storage);
        if (!compiled) {
          const [start, length] = this.vm['native-code-bytes'](code);
          const module = new WebAssembly.Module(new Uint8Array(this.vm.memory.buffer, start, length));
          compiled = {instance: new WebAssembly.Instance(module, {'miricl.runtime': this.vm}), length,
            code: this.vm['core-native-code']?.(storage, start, length) ?? null};
          this.nativeModules.set(storage, compiled);
        }
        const index = this.vm['native-code-entry']?.(code) ?? -1;
        const install = this.vm['core-native-function'] ?? this.vm['native-function'];
        const exportName = index < 0 ? 'entry' : `entry${index}`;
        let function_ = install(compiled.instance.exports[exportName],
          literals, name, compiled.length, compiled.code);
        for (let arity = 0; arity <= 4; arity++) {
          const direct = compiled.instance.exports[`direct-${exportName}-${arity}`];
          if (direct) {
            function_ = this.vm['native-attach-direct-root'](function_, direct, arity);
            const usage = compiled.instance.exports[`argument-usage-${exportName}`]?.value;
            if (typeof usage === 'number') function_ = this.vm['native-set-argument-usage'](function_, usage);
            break;
          }
        }
        return function_;
      }}});
      this.vm['native-services'].set(0, bridge.exports.service);
    }
    if (onDebugger !== undefined && typeof onDebugger !== 'function')
      throw new TypeError('onDebugger must be a function.');
    const debuggerImports = onDebugger ? {miriclDebugger: {
      stop: (data, reply) => this.debugStopped(data, reply),
      close: sessionId => this.debugClosed(sessionId),
    }} : {};
    if (this.vm['javascript-services'])
      this.javascript = new JavaScriptHost(this, {...imports, ...debuggerImports, miriclCore: {
        saveImage: filename => {
          try {this.mountFile(filename, this.saveImage()); return '';}
          catch (error) {return error.message;}
        },
      }});
    for (const {table, entries} of restoredWeak) this.javascript.weakHashTables.restore(table, entries);
    if (this.vm['thread-services'] && typeof WebAssembly.Suspending === 'function')
      this.threads = new GreenThreads(this);
    if (onDebugger && this.threads && this.vm['debugger-connected'])
      this.vm['debugger-connected'].value = 1;
    if (this.vm['seed-random']) {
      const seed = globalThis.crypto.getRandomValues(new Uint32Array(4));
      this.vm['seed-random'](...seed);
    }
  }

  static async instantiate(bytes, options) {
    const isModule = Object.prototype.toString.call(bytes) === '[object WebAssembly.Module]';
    const sections = isModule ? WebAssembly.Module.customSections(bytes, 'miricl.core') : [];
    if (sections.length > 1) throw new Error('Duplicate Miricl image sections.');
    const {base, payload} = isModule ? {base: bytes, payload: sections[0]} : coreImageParts(
        ArrayBuffer.isView(bytes) ? new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength) : new Uint8Array(bytes));
    const result = await WebAssembly.instantiate(base);
    const interpreter = new Miricl(result.instance ?? result, options, payload);
    if (!isModule && interpreter.vm['core-object-type']) interpreter.imageBase = base;
    if (options?.dev) {
      let filesystem;
      try {
        const {DevRepl} = await import('./dev-repl.mjs');
        if (options.dev === true || options.dev.filesystem !== false) {
          const {DevFilesystem} = await import('./dev-filesystem.mjs');
          filesystem = new DevFilesystem(interpreter, options.dev.filesystem ?? true);
        }
        interpreter.dev = new DevRepl(interpreter, options.dev);
        interpreter.dev.filesystem = filesystem;
      } catch (error) {
        filesystem?.close();
        interpreter.close();
        throw error;
      }
    }
    if (payload) {
      try {
        const {output} = interpreter.vm['core-restart']
          ? (interpreter.threads ? await interpreter.threads.evaluate('', {}, true) : interpreter.evaluate('', {}, true))
          : await interpreter.evaluateAsync('(miricl-core::%restart-image)', {eof: true});
        if (output) interpreter.onOutput?.(output);
      } catch (error) {interpreter.close(); throw error;}
    }
    return interpreter;
  }

  saveImage() {
    if (!this.imageBase) throw new Error('Saving a Miricl image requires the original Wasm bytes.');
    if (!this.vm['core-checkpoint-safe']()) throw new Error('Saving a Miricl image requires a single Lisp thread.');
    return captureCore(this.vm, this.imageBase, this.javascript?.weakHashTables);
  }

  failure(cause) {
    const code = this.vm.error.value;
    const output = this.text(this.vm['text-output-length'].value);
    if (!code && this.vm['capture-backtrace']) this.vm['capture-backtrace']();
    this.vm.unwind();
    const message = code ? this.text(this.vm['format-error']()) : cause.message;
    let backtrace = '';
    if (this.vm['format-backtrace']) {
      try { backtrace = this.text(this.vm['format-backtrace']()); }
      catch { this.vm.unwind(); } // A broken printer must not replace the original error.
    }
    return new MiriclError(message, code, output, backtrace, cause);
  }

  hostService(operation, argument) {
    switch (operation) {
      case 0: return Date.now() / 1000 + 2208988800;
      case 1: return performance.now() - this.realOrigin;
      case 2: return this.cpuUsage ? this.cpuUsage() - this.cpuOrigin
        : this.runTime + (this.activeStart === null ? 0 : performance.now() - this.activeStart - this.sleepTime);
      case 3: {
        // A shared WASM memory also works in workers where the JS
        // SharedArrayBuffer constructor is unavailable (for example file:).
        this.sleepBuffer ??= new Int32Array(typeof SharedArrayBuffer === 'function'
          ? new SharedArrayBuffer(4) : new WebAssembly.Memory({initial: 1, maximum: 1, shared: true}).buffer);
        const start = performance.now();
        try { Atomics.wait(this.sleepBuffer, 0, 0, argument); }
        catch { return -1; } // A browser main thread cannot block; use a worker.
        this.sleepTime += performance.now() - start;
        return 0;
      }
      default: return -1;
    }
  }

  text(length) {
    // Always reacquire memory: printing can grow it and detach the old buffer.
    return this.decoder.decode(new Uint8Array(this.vm.memory.buffer, 0, length));
  }

  mountFile(path, source) {
    if (typeof path !== 'string') {
      throw new TypeError('mountFile expects a string path.');
    }
    // CL strings escape quotes and backslashes; newlines remain literal.
    const string = value => `"${value.replace(/["\\]/g, '\\$&')}"`;
    if (source instanceof ArrayBuffer || ArrayBuffer.isView(source)) {
      const bytes = source instanceof ArrayBuffer ? new Uint8Array(source)
        : new Uint8Array(source.buffer, source.byteOffset, source.byteLength);
      // Explicit mounts always create local files, including in dev mode.
      const provider = this.vm['filesystem-services']?.get(0);
      this.vm['filesystem-services']?.set(0, null);
      try {
        if (this.vm['filesystem-mount'] && this.javascript) {
          const name = this.javascript.makeText(path);
          const start = this.vm['javascript-buffer'](bytes.length);
          new Uint8Array(this.vm.memory.buffer, start, bytes.length).set(bytes);
          this.vm['filesystem-mount'](name, bytes.length);
          return {output: '', incomplete: false, quit: false};
        }
        return this.evaluate(`(cl:progn
          (cl:with-open-file (stream ${string(path)} :direction :output
              :element-type '(cl:unsigned-byte 8) :if-exists :supersede)
            (cl:write-sequence '#(${bytes.join(' ')}) stream)) (cl:values))`, {eof: true});
      } catch (cause) { throw cause instanceof MiriclError ? cause : this.failure(cause); }
      finally { this.vm['filesystem-services']?.set(0, provider ?? null); }
    }
    if (typeof source !== 'string') throw new TypeError('mountFile expects UTF-8 text or a byte buffer.');
    return this.evaluate(`(cl::register-source-file ${string(path)} ${string(source)})`, {eof: true});
  }

  async readFile(path) {
    if (typeof path !== 'string') throw new TypeError('readFile expects a string path.');
    if (this.vm['filesystem-local-data'] && this.javascript) {
      try {
        const [start, length] = this.vm['filesystem-local-data'](this.javascript.makeText(path));
        if (start >= 0) return new Uint8Array(this.vm.memory.buffer, start, length).slice();
      } catch (cause) { throw this.failure(cause); }
    }
    const name = `"${path.replace(/["\\]/g, '\\$&')}"`;
    // Use the ordinary file API, including asynchronous dev reads. Transport
    // byte values as character codes through the existing text output channel.
    const {output} = await this.evaluateAsync(`(cl:with-open-file
        (stream ${name} :element-type '(cl:unsigned-byte 8))
      (cl:let ((bytes (cl:make-array (cl:file-length stream)
                                    :element-type '(cl:unsigned-byte 8))))
        (cl:read-sequence bytes stream)
        (cl:write-string (cl:map 'cl:string (cl:function cl:code-char) bytes))
        (cl:values)))`, {eof: true});
    return Uint8Array.from(output, character => character.charCodeAt(0));
  }

  debugStopped(data, reply) {
    const [sessionId, stopId, threadId, threadName, level, parentSessionId, type, message,
      backtrace, descriptions, reports, canStep, hasResult, values, output, error] = data;
    const indices = new Map();
    const frames = backtrace.split('\n').flatMap(line => {
      const match = /^\s*(\d+): (.*)$/.exec(line);
      if (!match) return [];
      const id = Number(match[1]);
      const index = descriptions.findIndex(description => description[0] === id);
      const description = descriptions[index];
      if (description) indices.set(id, index);
      return [{id, name: match[2], inspectable: !!description,
        form: description?.[2] ?? null,
        locals: description?.[3].map(([name, value]) => ({name, value})) ?? []}];
    });
    // A top-level LOCALLY/STEP scope has an environment but no function call
    // in the bounded call ring. Expose that interpreter scope as well.
    descriptions.forEach(([id, name, form, locals], index) => {
      if (frames.some(frame => frame.id === id)) return;
      indices.set(id, index);
      frames.push({id, name, inspectable: true, form, locals: locals.map(([name, value]) => ({name, value}))});
    });
    const result = hasResult ? {values, output, ...(error ? {error} : {})} : null;
    const stop = new DebuggerStop(this, {sessionId, stopId, threadId, threadName, level,
      parentSessionId: parentSessionId || null, condition: {type, message}, backtrace,
      frames, restarts: reports.map(([name, description], id) => ({id, name, description})), canStep, result});
    const previous = this.debugSessions.get(sessionId);
    this.debugSessions.set(sessionId, {stop, reply, indices});
    if (previous?.pending && hasResult) {
      if (error) previous.pending.reject(new Error(error));
      else previous.pending.resolve({values, output});
    }
    queueMicrotask(() => this.onDebugger?.({type: 'stopped', stop}));
  }

  debugClosed(sessionId) {
    const entry = this.debugSessions.get(sessionId);
    if (!entry) return;
    this.debugSessions.delete(sessionId);
    if (entry.pending?.op === 'eval') entry.pending.reject(new Error('Debugger evaluation was interrupted.'));
    else entry.pending?.resolve();
    const {threadId, level, parentSessionId} = entry.stop;
    queueMicrotask(() => this.onDebugger?.({type: 'closed', sessionId, threadId, level, parentSessionId}));
  }

  debuggerSessions() { return [...this.debugSessions.values()].map(entry => entry.stop); }

  assertDebugStop(stop) {
    const entry = this.debugSessions.get(stop.sessionId);
    if (!entry || entry.stop.stopId !== stop.stopId || entry.stop.threadId !== stop.threadId)
      throw new Error('This debugger stop is no longer active.');
    if (!entry.reply || [...this.debugSessions.values()].some(child => child.stop.parentSessionId === stop.sessionId))
      throw new Error('This debugger session is busy or suspended by a nested debugger.');
    return entry;
  }

  async debugCommand(stop, {op, frame = 0, source = ''}) {
    const entry = this.assertDebugStop(stop);
    if (!['eval', 'restart', 'abort', 'into', 'over', 'out', 'resume'].includes(op))
      throw new TypeError('Unknown debugger command.');
    if (!Number.isSafeInteger(frame) || frame < 0 || typeof source !== 'string')
      throw new TypeError('Debugger commands require a nonnegative frame index and a source string.');
    if (['into', 'over', 'out', 'resume'].includes(op) && !stop.canStep)
      throw new Error('This stop cannot resume without invoking an available restart.');
    if (op === 'eval') {
      if (!entry.indices.has(frame)) throw new Error('This frame has no interpreter debugging information.');
      frame = entry.indices.get(frame);
    }
    if (op === 'restart' && !stop.restarts.some(restart => restart.id === frame))
      throw new Error('This restart does not exist at this stop.');
    const reply = entry.reply;
    const quoted = `"${source.replace(/["\\]/g, '\\$&')}"`;
    const result = new Promise((resolve, reject) => {entry.pending = {resolve, reject, op};});
    entry.reply = null;
    try { reply(`(:${op} ${frame} ${quoted})`); this.threads.schedule(); }
    catch (error) {entry.pending.reject(error);}
    return result;
  }

  close() {
    for (const entry of this.debugSessions.values()) entry.pending?.reject(new Error('The interpreter was closed.'));
    this.debugSessions.clear(); this.dev?.close(); this.threads?.close();
  }

  evaluateAsync(source, options) {
    return this.threads ? this.threads.evaluate(source, options) : Promise.resolve(this.evaluate(source, options));
  }

  evaluate(source, {eof = false} = {}, restart = false) {
    if (this.javascript?.depth > 0)
      throw new Error('Use a Lisp callback to reenter an active interpreter.');
    if (this.threads?.main) throw new Error('An asynchronous evaluation is already running.');
    if (this.threads?.initialized) this.vm['thread-enter'](0);
    if (this.vm['thread-active']) this.vm['thread-active'].value = 0;
    const bytes = this.encoder.encode(source);
    const missing = bytes.length - this.vm.memory.buffer.byteLength;
    if (missing > 0) this.vm.memory.grow(Math.ceil(missing / 65536));
    new Uint8Array(this.vm.memory.buffer).set(bytes);
    this.vm.error.value = 0;
    let length;
    this.activeStart = performance.now();
    this.sleepTime = 0;
    try {
      length = this.vm['evaluate-source'](restart ? -1 : bytes.length);
    } catch (cause) {
      throw this.failure(cause);
    } finally {
      this.runTime += performance.now() - this.activeStart - this.sleepTime;
      this.activeStart = null;
      if (this.threads?.initialized) this.vm['thread-save']();
    }
    const status = this.vm['text-status'].value;
    if (eof && status === 1) throw new MiriclError('Incomplete input.', 16);
    return {output: this.text(length), incomplete: status === 1, quit: status === 2};
  }
}
