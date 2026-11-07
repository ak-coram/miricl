// Read-through development files. Fetching parks the Lisp thread, so the browser
// and other Lisp threads can keep running while ordinary file APIs wait for I/O.
const serviceName = [7, ...new TextEncoder().encode('service')];
const bridgeModule = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,9,1,96,3,127,111,111,2,111,127,
  2,16,1,4,104,111,115,116,...serviceName,0,0,
  7,11,1,...serviceName,0,0,
]));

export class DevFilesystem {
  constructor(interpreter, base = true) {
    this.interpreter = interpreter;
    this.vm = interpreter.vm;
    this.js = interpreter.javascript;
    if (!this.vm['filesystem-services'] || !interpreter.threads)
      throw new Error('Development file access requires an interpreter with asynchronous filesystem support.');
    this.url = new URL(base === true ? '/' : base, globalThis.location?.href ?? 'http://127.0.0.1:8000/');
    if (!['http:', 'https:'].includes(this.url.protocol))
      throw new TypeError('The development filesystem URL must use http: or https:.');
    if (!this.url.pathname.endsWith('/')) this.url.pathname += '/';
    this.requests = new Set();
    this.cache = new Map();
    this.decoder = new TextDecoder('utf-8', {fatal: true});
    const bridge = new WebAssembly.Instance(bridgeModule, {host: {service: (...args) => this.service(...args)}});
    this.serviceFunction = bridge.exports.service;
    this.previous = this.vm['filesystem-services'].get(0);
    this.vm['filesystem-services'].set(0, this.serviceFunction);
  }

  refresh() { this.cache = new Map(); }

  async fetch(operation, name, signal) {
    const encoded = name.split('/').map(encodeURIComponent).join('/');
    const response = await globalThis.fetch(new URL(`__miricl__/fs/${operation}/${encoded}`, this.url),
      {cache: 'no-store', signal, credentials: 'same-origin'});
    if (response.status === 404) return null;
    if (!response.ok) throw new Error(`Development filesystem: HTTP ${response.status} for ${name}`);
    return new Uint8Array(await response.arrayBuffer());
  }

  async stat(name, signal, cache) {
    const key = `stat:${name}`;
    if (cache.has(key)) return cache.get(key);
    const bytes = await this.fetch('stat', name, signal);
    let result = null;
    if (bytes) {
      if (bytes.length !== 24) throw new Error('Invalid filesystem metadata.');
      const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
      result = {kind: view.getUint32(0, true),
        date: view.getUint32(4, true) ? Number(view.getBigInt64(8, true)) + 2208988800 : null};
      if (![2, 5].includes(result.kind)) throw new Error('Invalid filesystem entry type.');
    }
    cache.set(key, result);
    return result;
  }

  async list(name, signal, cache) {
    const key = `list:${name}`;
    if (cache.has(key)) return cache.get(key);
    const bytes = await this.fetch('list', name, signal);
    if (!bytes) { cache.set(key, null); return null; }
    const entries = [];
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    let offset = 0, ended = false;
    while (offset + 8 <= bytes.length) {
      const kind = view.getUint32(offset, true), size = view.getUint32(offset + 4, true);
      offset += 8;
      if (kind === 0 && size === 0) { ended = true; break; }
      if (offset + size > bytes.length) break;
      const entry = this.decoder.decode(bytes.subarray(offset, offset + size));
      if (!entry || entry === '.' || entry === '..' || /[\\/\u0000-\u001f\u007f]/u.test(entry))
        throw new Error('Invalid filesystem directory entry.');
      if (kind === 2 || kind === 5) entries.push([entry, kind === 2]);
      offset += size;
    }
    if (!ended || offset !== bytes.length) throw new Error('Incomplete filesystem directory listing.');
    cache.set(key, entries);
    return entries;
  }

  async directory(pattern, signal, cache) {
    if (!pattern.includes('*')) {
      const name = pattern.replace(/\/$/u, '');
      const stat = await this.stat(name, signal, cache);
      return stat ? [[name, stat.kind === 2]] : [];
    }
    const parts = pattern.split('/').slice(0, -1);
    const first = parts.findIndex(part => part.includes('*'));
    const base = (first < 0 ? parts : parts.slice(0, first)).join('/');
    const recursive = parts.includes('**');
    const depth = first < 0 ? 0 : parts.length - first - Number(pattern.endsWith('/'));
    const result = new Map();
    const visit = async (name, level) => {
      const entries = await this.list(name, signal, cache);
      if (!entries) return;
      result.set(name, true);
      for (const [entry, directory] of entries) {
        const child = name ? `${name}/${entry}` : entry;
        result.set(child, directory);
        if (directory && (recursive || level > 0)) await visit(child, level - 1);
      }
    };
    await visit(base, depth);
    return [...result];
  }

  async operation(op, name, signal, cache) {
    if (op === 0) return this.stat(name, signal, cache);
    if (op === 2) return this.directory(name, signal, cache);
    if (op !== 1) throw new Error('Unknown filesystem operation.');
    const stat = await this.stat(name, signal, cache);
    if (!stat || stat.kind !== 5) return null;
    const bytes = await this.fetch('read', name, signal);
    return bytes ? {bytes, date: stat.date} : null;
  }

  convert(op, path, result) {
    if (result === null) return null;
    if (op === 0)
      return this.js.link(2, this.js.makeInteger(BigInt(result.kind)),
        result.date === null ? null : this.js.makeInteger(BigInt(result.date)));
    if (op === 1) {
      const start = this.vm['javascript-buffer'](result.bytes.length);
      new Uint8Array(this.vm.memory.buffer, start, result.bytes.length).set(result.bytes);
      return this.vm['filesystem-file'](path, result.bytes.length, result.date ?? -1);
    }
    let list = null;
    for (let i = result.length - 1; i >= 0; i--) {
      const [name, directory] = result[i];
      list = this.js.link(2, this.js.link(2, this.js.makeText(name), this.js.bool(directory)), list);
    }
    return list;
  }

  wake(request) {
    this.vm['filesystem-wake'](request.thread);
    this.interpreter.threads.schedule();
  }

  service(op, path, value) {
    if (op === -2) {
      value.cancelled = true;
      value.controller.abort();
      this.requests.delete(value);
      if (this.closed && this.requests.size === 0) this.detach();
      return [null, 0];
    }
    if (op === -1) {
      if (!value.ready) return [value, 1];
      if (value.error) return [null, 2];
      try {
        this.currentCache();
        if (value.op === 0) this.cache.set(`stat:${value.name}`, value.result);
        if (value.op === 1 && value.result)
          this.cache.set(`stat:${value.name}`, {kind: 5, date: value.result.date});
        return [this.convert(value.op, path, value.result), 0];
      }
      catch { return [null, 2]; }
    }
    if (this.closed) return [null, 0];
    const request = {op, thread: value, controller: new AbortController()};
    this.requests.add(request);
    try {
      const name = this.js.text(path);
      this.currentCache();
      if (op === 0 && this.cache.has(`stat:${name}`)) {
        this.requests.delete(request);
        return [this.convert(op, path, this.cache.get(`stat:${name}`)), 0];
      }
      request.name = name;
      this.operation(op, name, request.controller.signal, this.cache).then(result => {
        if (request.cancelled || request.ready) return;
        request.result = result; request.ready = true; this.wake(request);
      }, error => {
        if (request.cancelled || request.ready) return;
        request.error = error; request.ready = true; this.wake(request);
      });
    } catch (error) {
      this.requests.delete(request);
      return [null, 2];
    }
    return [request, 1];
  }

  currentCache() {
    // Reuse metadata only while Lisp runs uninterrupted. A yield or I/O wait
    // starts a new segment, allowing background file watchers to see host edits.
    const segment = this.interpreter.threads.segment;
    if (this.segment !== segment) { this.refresh(); this.segment = segment; }
    return this.cache;
  }

  detach() {
    if (this.vm['filesystem-services'].get(0) === this.serviceFunction)
      this.vm['filesystem-services'].set(0, this.previous);
  }

  close() {
    this.closed = true;
    for (const request of this.requests) {
      request.error = new Error('Development filesystem closed.'); request.ready = true;
      request.controller.abort(); this.wake(request);
    }
    if (this.requests.size === 0) this.detach();
    this.cache.clear();
  }
}
