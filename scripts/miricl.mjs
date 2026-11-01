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

export class MiriclError extends Error {
  constructor(message, code, output = '') {
    super(message);
    this.name = 'MiriclError';
    this.code = code;
    this.output = output;
  }
}

export class Miricl {
  constructor(instance) {
    this.vm = instance.exports;
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
    if (this.vm['seed-random']) {
      const seed = globalThis.crypto.getRandomValues(new Uint32Array(4));
      this.vm['seed-random'](...seed);
    }
  }

  static async instantiate(bytes) {
    const result = await WebAssembly.instantiate(bytes);
    return new Miricl(result.instance ?? result);
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
      return this.evaluate(`(cl:progn
        (cl:with-open-file (stream ${string(path)} :direction :output
            :element-type '(cl:unsigned-byte 8) :if-exists :supersede)
          (cl:write-sequence '#(${bytes.join(' ')}) stream)) (cl:values))`, {eof: true});
    }
    if (typeof source !== 'string') throw new TypeError('mountFile expects UTF-8 text or a byte buffer.');
    return this.evaluate(`(cl::register-source-file ${string(path)} ${string(source)})`, {eof: true});
  }

  evaluate(source, {eof = false} = {}) {
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
      const code = this.vm.error.value;
      const output = this.text(this.vm['text-output-length'].value);
      this.vm.unwind();
      const message = code ? this.text(this.vm['format-error']()) : cause.message;
      throw new MiriclError(message, code, output);
    } finally {
      this.runTime += performance.now() - this.activeStart - this.sleepTime;
      this.activeStart = null;
    }
    const status = this.vm['text-status'].value;
    if (eof && status === 1) throw new MiriclError('Incomplete input.', 16);
    return {output: this.text(length), incomplete: status === 1, quit: status === 2};
  }
}
