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
  async evaluate(source, {eof = false} = {}) {
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
      const length = await this.evaluateSource(bytes.length);
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
      this.depth = depth + 1;
      let result;
      try {
        let arguments_ = null;
        for (let i = args.length - 1; i >= 0; i--) arguments_ = this.link(2, this.wrap(args[i]), arguments_);
        result = this.toJS(this.link(3, function_, arguments_));
      } catch (cause) {
        if (nested) throw cause;
        throw this.owner.failure(cause);
      } finally { this.depth = depth; }
      if (!nested && this.owner.onOutput) {
        const output = this.link(8);
        if (output !== null) {
          const text = this.text(output);
          if (text) this.owner.onOutput(text);
        }
      }
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

export class Miricl {
  constructor(instance, {imports = {}, onOutput, onThreadError} = {}) {
    this.vm = instance.exports;
    this.onOutput = onOutput;
    this.onThreadError = onThreadError;
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
    if (this.vm['javascript-services']) this.javascript = new JavaScriptHost(this, imports);
    if (this.vm['thread-services'] && typeof WebAssembly.Suspending === 'function')
      this.threads = new GreenThreads(this);
    if (this.vm['seed-random']) {
      const seed = globalThis.crypto.getRandomValues(new Uint32Array(4));
      this.vm['seed-random'](...seed);
    }
  }

  static async instantiate(bytes, options) {
    const result = await WebAssembly.instantiate(bytes);
    const interpreter = new Miricl(result.instance ?? result, options);
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
    return interpreter;
  }

  failure(cause) {
    const code = this.vm.error.value;
    const output = this.text(this.vm['text-output-length'].value);
    this.vm.unwind();
    const message = code ? this.text(this.vm['format-error']()) : cause.message;
    let backtrace = '';
    if (code && this.vm['format-backtrace']) {
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
        return this.evaluate(`(cl:progn
          (cl:with-open-file (stream ${string(path)} :direction :output
              :element-type '(cl:unsigned-byte 8) :if-exists :supersede)
            (cl:write-sequence '#(${bytes.join(' ')}) stream)) (cl:values))`, {eof: true});
      } finally { this.vm['filesystem-services']?.set(0, provider ?? null); }
    }
    if (typeof source !== 'string') throw new TypeError('mountFile expects UTF-8 text or a byte buffer.');
    return this.evaluate(`(cl::register-source-file ${string(path)} ${string(source)})`, {eof: true});
  }

  async readFile(path) {
    if (typeof path !== 'string') throw new TypeError('readFile expects a string path.');
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

  close() { this.dev?.close(); this.threads?.close(); }

  evaluateAsync(source, options) {
    return this.threads ? this.threads.evaluate(source, options) : Promise.resolve(this.evaluate(source, options));
  }

  evaluate(source, {eof = false} = {}) {
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
      length = this.vm['evaluate-source'](bytes.length);
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
