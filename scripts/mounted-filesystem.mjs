// A downloaded archive can supply files without copying every payload into
// WasmGC at startup. Ordinary asynchronous Lisp file APIs read the local bytes;
// the runtime's existing overlay handles writes, renames and deletions.
const bridgeModule = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,9,1,96,3,127,111,111,2,111,127,
  2,16,1,4,104,111,115,116,7,115,101,114,118,105,99,101,0,0,
  7,11,1,7,115,101,114,118,105,99,101,0,0,
]));

export class MountedFilesystem {
  constructor(interpreter, files) {
    this.vm = interpreter.vm;
    this.js = interpreter.javascript;
    if (!interpreter.threads || !this.vm['filesystem-services'] || !this.js)
      throw new Error('Archive files require asynchronous filesystem support.');
    if (this.vm['filesystem-services'].get(0))
      throw new Error('An interpreter can only have one read-through filesystem.');
    this.files = new Map();
    this.directories = new Set(['']);
    this.loaded = new Map();
    this.date = Math.floor(Date.now() / 1000) + 2208988800;
    for (const [name, source] of files) {
      if (typeof name !== 'string' || !name.startsWith('/') || /(?:\/\.?\.?\/|\/\.?\.?$)/u.test(name))
        throw new TypeError('Archive file names must be canonical absolute paths.');
      if (!(source instanceof ArrayBuffer) && !ArrayBuffer.isView(source))
        throw new TypeError('Archive contents must be byte buffers.');
      const bytes = source instanceof ArrayBuffer ? new Uint8Array(source)
        : new Uint8Array(source.buffer, source.byteOffset, source.byteLength);
      // Guest source-file keys are relative to the virtual root, including
      // when the Lisp caller used an absolute pathname.
      const key = name.slice(1);
      this.files.set(key, bytes);
      for (let end = key.lastIndexOf('/'); end > 0; end = key.lastIndexOf('/', end - 1))
        this.directories.add(key.slice(0, end));
    }
    const bridge = new WebAssembly.Instance(bridgeModule, {host: {service: (op, file) => this.service(op, file)}});
    this.serviceFunction = bridge.exports.service;
    this.vm['filesystem-services'].set(0, this.serviceFunction);
  }

  service(op, path) {
    try {
      const name = this.js.text(path).replace(/\/$/u, '');
      if (op === 0) {
        const kind = this.files.has(name) ? 5 : this.directories.has(name) ? 2 : 0;
        return [kind ? this.js.link(2, this.js.makeInteger(BigInt(kind)), this.js.makeInteger(BigInt(this.date))) : null, 0];
      }
      if (op === 1) {
        const bytes = this.files.get(name);
        if (!bytes) return [null, 0];
        if (!this.loaded.has(name)) {
          const start = this.vm['javascript-buffer'](bytes.length);
          new Uint8Array(this.vm.memory.buffer, start, bytes.length).set(bytes);
          this.loaded.set(name, this.vm['filesystem-file'](path, bytes.length, this.date));
        }
        return [this.loaded.get(name), 0];
      }
      if (op === 2) {
        // DIRECTORY applies its pathname matcher and overlay deduplication to
        // these candidates, including recursive wildcards and directory names.
        let entries = null;
        for (const [names, directory] of [[this.files.keys(), false], [this.directories, true]])
          for (const name of names)
            entries = this.js.link(2, this.js.link(2, this.js.makeText(name), this.js.bool(directory)), entries);
        return [entries, 0];
      }
      return [null, 2];
    } catch {return [null, 2];}
  }

  close() {
    if (this.vm['filesystem-services'].get(0) === this.serviceFunction)
      this.vm['filesystem-services'].set(0, null);
    this.files.clear(); this.loaded.clear(); this.directories.clear();
  }
}
