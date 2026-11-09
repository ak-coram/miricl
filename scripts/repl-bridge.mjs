// Node socket services for the relay embedded in miricl.wasm.
// WebSocket handshakes and framing run in the same Wasm core as the WASI host.
import net from 'node:net';
import fs from 'node:fs/promises';
import path from 'node:path';

// The REPL deliverable also carries a precompiled core without WASI imports or
// stack-switching instructions, so Node can use it without experimental flags.
export async function replCompilerImage(interpreterPath) {
  let bytes;
  try { bytes = await fs.readFile(path.join(path.dirname(interpreterPath), 'miricl-repl.wasm')); }
  catch (error) { if (error.code === 'ENOENT') return undefined; throw error; }
  let position = 8;
  const u32 = () => {
    let value = 0, shift = 0, byte;
    do {
      if (position >= bytes.length || shift > 28) throw new Error('Invalid REPL component section.');
      byte = bytes[position++]; value += (byte & 127) * 2 ** shift; shift += 7;
    } while (byte & 128);
    return value;
  };
  while (position < bytes.length) {
    const id = bytes[position++], size = u32(), end = position + size;
    if (end > bytes.length) throw new Error('Truncated REPL component section.');
    if (id === 0) {
      const length = u32(), name = new TextDecoder().decode(bytes.subarray(position, position + length));
      position += length;
      if (name === 'miricl.node-compiler') return bytes.subarray(position, end);
    }
    position = end;
  }
  return undefined;
}

export async function startReplBridge(image, {devServer = false, tcpBridge = false, directory = '.',
  tcpPort = 4005, websocketPort = 4006, httpPort = 8000} = {}) {
  for (const port of [tcpPort, websocketPort, httpPort])
    if (!Number.isInteger(port) || port < 1 || port > 65535) throw new RangeError('Ports must be integers from 1 to 65535.');
  const ports = [websocketPort, ...(tcpBridge ? [tcpPort] : []), ...(devServer ? [httpPort] : [])];
  if (new Set(ports).size !== ports.length) throw new Error('Enabled listeners need distinct ports.');
  const module = image instanceof WebAssembly.Module ? image : await WebAssembly.compile(image);
  const [core] = WebAssembly.Module.customSections(module, 'miricl.repl-bridge');
  if (!core) throw new Error('This image has no REPL bridge. Rebuild miricl.wasm.');
  const memory = new WebAssembly.Memory({initial: 324});
  const handles = new Map();
  const events = new Map();
  const listeners = [];
  const sockets = new Set();
  const cleanup = [];
  const root = devServer ? await fs.realpath(directory) : undefined;
  if (devServer && !(await fs.stat(root)).isDirectory()) throw new Error('The served path must be a directory.');
  let nextHandle = 1;
  let instance;
  let scheduled = false;
  let closed = false;
  const exited = new Error('REPL host exited.');
  const allocate = value => { const handle = nextHandle++; handles.set(handle, value); return handle; };
  const view = () => new DataView(memory.buffer);
  const store = (p, value) => view().setUint32(p, value, true);
  const result = (p, value = 0) => { store(p, 0); store(p + 4, value); };
  function schedule() {
    if (scheduled || closed) return;
    scheduled = true;
    setImmediate(() => {
      scheduled = false;
      if (closed) return;
      let budget = 256;
      while (events.size && budget--) {
        const [handle, {event, result}] = events.entries().next().value;
        events.delete(handle);
        try { instance.exports.callback(event, handle, result); }
        catch (error) { if (error === exited) return; throw error; }
      }
      if (events.size) schedule();
    });
  }
  function complete(stream, event, value) {
    if (closed) return;
    stream.pending = null;
    events.set(stream.handle, {event, result: value});
    schedule();
  }
  function readAvailable(stream, p, n) {
    if (stream.kind === 'file-status') {
      if (!stream.done) return -1;
      new Uint8Array(memory.buffer, p, 20).fill(0);
      if (stream.error) { store(p, 1); store(p + 4, 12); }
      return 0;
    }
    if (stream.kind === 'directory-read') {
      if (!stream.entries) return -1;
      if (stream.index === stream.entries.length) return 1;
      const entry = stream.entries[stream.index++];
      const name = new TextEncoder().encode(entry.name);
      new Uint8Array(memory.buffer, p, 24).fill(0);
      store(p, entry.isFile() ? 5 : entry.isDirectory() ? 2 : entry.isSymbolicLink() ? 4 : 7);
      store(p + 16, p + 64); store(p + 20, name.length);
      new Uint8Array(memory.buffer, p + 64, name.length).set(name);
      return 16;
    }
    if (stream.kind === 'accept') {
      if (!stream.queue.length) return -1;
      store(p, stream.queue.shift());
      return 16;
    }
    const socket = stream.socket;
    const available = Math.min(n, socket.readableLength);
    if (available) {
      const bytes = socket.read(available);
      new Uint8Array(memory.buffer, p, bytes.length).set(bytes);
      return bytes.length << 4;
    }
    socket.read(0); // Let Node deliver 'end' after its last buffered chunk.
    return socket.readableEnded || socket.destroyed ? 1 : -1;
  }
  function readable(stream) {
    if (!stream.pending) return;
    const {p, n} = stream.pending;
    const count = readAvailable(stream, p, n);
    if (count !== -1) complete(stream, stream.kind === 'file-status' ? 4 : 2, count);
  }
  function read(handle, p, n) {
    const stream = handles.get(handle);
    const count = readAvailable(stream, p, n);
    if (count === -1) stream.pending = {p, n};
    return count;
  }
  function cancel(handle) {
    const stream = handles.get(handle);
    stream.pending = null;
    const event = events.get(handle);
    events.delete(handle);
    return event?.result ?? 2;
  }
  function registerReader(socket, kind = 'read') {
    const stream = {kind, socket};
    stream.handle = allocate(stream);
    const ready = () => readable(stream);
    for (const event of ['readable', 'end', 'close']) socket.on(event, ready);
    if (kind === 'console-read') cleanup.push(() => {
      for (const event of ['readable', 'end', 'close']) socket.off(event, ready);
      socket.pause();
      socket.unref?.();
    });
    return stream;
  }
  function task(operation) {
    const handle = allocate({kind: 'task'});
    Promise.resolve().then(operation).then(() => {
      if (closed) return;
      events.set(handle, {event: 1, result: 2});
      schedule();
    });
    return handle << 4 | 1;
  }
  function fileStatus() {
    const status = {kind: 'file-status'};
    status.handle = allocate(status);
    return status;
  }
  function finishFileStatus(status, error = false) {
    if (status.done) return;
    status.done = true; status.error = error; readable(status);
  }
  const services = {
    memory,
    exit() { close(); throw exited; },
    create(_family, p) { result(p, allocate({kind: 'socket'})); },
    bind(...args) {
      const resource = handles.get(args[0]);
      resource.port = args[2];
      resource.host = args.slice(3, 7).join('.');
      result(args.at(-1));
    },
    listen(handle, p) {
      const resource = handles.get(handle);
      const stream = {kind: 'accept', queue: []};
      stream.handle = allocate(stream);
      const server = net.createServer(socket => {
        if (stream.queue.length >= 8) { socket.destroy(); return; }
        sockets.add(socket);
        const peer = {kind: 'socket', socket};
        socket.on('error', () => {});
        socket.on('close', () => sockets.delete(socket));
        stream.queue.push(allocate(peer));
        readable(stream);
      });
      resource.server = server;
      const ready = new Promise((resolve, reject) => {
        server.once('error', reject);
        server.listen(resource.port, resource.host, () => resolve(server.address()));
      });
      // Install the rejection handler before starting the second listener.
      ready.catch(() => {});
      listeners.push({server, ready});
      result(p, stream.handle);
    },
    receive(handle, p) {
      const {socket} = handles.get(handle);
      const stream = registerReader(socket);
      store(p, stream.handle);
      store(p + 4, allocate({kind: 'status'}));
    },
    'new-stream'() {
      const stream = {kind: 'write'};
      const reader = allocate({kind: 'write-reader', stream});
      stream.handle = allocate(stream);
      return BigInt(reader) | BigInt(stream.handle) << 32n;
    },
    send(handle, reader) {
      const {stream} = handles.get(reader);
      handles.delete(reader);
      stream.socket = handles.get(handle).socket;
      return allocate({kind: 'status'});
    },
    stdin(p) {
      const stream = registerReader(process.stdin, 'console-read');
      store(p, stream.handle); store(p + 4, allocate({kind: 'status'}));
    },
    stdout(reader) {
      const {stream} = handles.get(reader);
      handles.delete(reader);
      stream.socket = process.stdout;
      return allocate({kind: 'status'});
    },
    read, accept: read,
    write(handle, p, n) {
      const stream = handles.get(handle);
      if (stream.socket.destroyed) return 1;
      // Node owns its copy until the write callback runs.
      const bytes = Buffer.from(new Uint8Array(memory.buffer, p, n));
      const pending = {};
      stream.pending = pending;
      stream.socket.write(bytes, error => {
        if (!closed && stream.pending === pending) complete(stream, 3, error ? 1 : n << 4);
      });
      return -1;
    },
    'cancel-read': cancel, 'cancel-write': cancel,
    'drop-reader'(handle) {
      if (handles.get(handle)?.kind === 'file-read') handles.get(handle).socket.destroy();
      handles.delete(handle); events.delete(handle);
    },
    'drop-writer'(handle) { handles.delete(handle); events.delete(handle); },
    'drop-status'(handle) { handles.delete(handle); },
    directories(p) {
      store(32768, allocate({kind: 'directory', path: root}));
      store(32772, 32784); store(32776, 1);
      new Uint8Array(memory.buffer)[32784] = 46;
      store(p, 32768); store(p + 4, 1);
    },
    'open-file'(args, out) {
      const handle = view().getUint32(args, true);
      const p = view().getUint32(args + 8, true);
      const n = view().getUint32(args + 12, true);
      const relative = new TextDecoder().decode(new Uint8Array(memory.buffer, p, n));
      const directory = handles.get(handle).path;
      return task(async () => {
        try {
          const file = await fs.realpath(path.resolve(directory, relative));
          const descriptor = await fs.open(file, 'r');
          if (closed) { await descriptor.close(); return; }
          result(out, allocate({kind: 'file', descriptor, path: file}));
        } catch (error) {
          store(out, 1);
          store(out + 4, error.code === 'ENOENT' ? 19 : error.code === 'ENOTDIR' ? 23 : 0);
        }
      });
    },
    'file-type'(handle, out) {
      return task(async () => {
        try {
          const stat = await handles.get(handle).descriptor.stat();
          result(out, stat.isFile() ? 5 : stat.isDirectory() ? 2 : 7);
        } catch { store(out, 1); store(out + 4, 0); }
      });
    },
    'file-stat'(handle, out) {
      return task(async () => {
        try {
          const stat = await handles.get(handle).descriptor.stat();
          new Uint8Array(memory.buffer, out, 112).fill(0);
          store(out + 8, stat.isFile() ? 5 : stat.isDirectory() ? 2 : 7);
          view().setBigUint64(out + 24, BigInt(stat.nlink), true);
          view().setBigUint64(out + 32, BigInt(stat.size), true);
          if (Number.isFinite(stat.mtimeMs)) {
            store(out + 64, 1);
            view().setBigInt64(out + 72, BigInt(Math.floor(stat.mtimeMs / 1000)), true);
          }
        } catch { store(out, 1); store(out + 8, 0); }
      });
    },
    'file-directory'(handle, out) {
      const stream = {kind: 'directory-read', index: 0};
      const status = fileStatus();
      stream.handle = allocate(stream);
      store(out, stream.handle); store(out + 4, status.handle);
      fs.readdir(handles.get(handle).path, {withFileTypes: true}).then(entries => {
        stream.entries = entries; finishFileStatus(status); readable(stream);
      }, () => { stream.entries = []; finishFileStatus(status, true); readable(stream); });
    },
    'read-directory': read,
    'cancel-directory': cancel,
    'read-file-status': read,
    'cancel-file-status': cancel,
    'drop-directory'(handle) { handles.delete(handle); events.delete(handle); },
    realloc() { return 0; }, // Node's directory names use the request's fixed buffer.
    'file-read'(handle, offset, p) {
      const socket = handles.get(handle).descriptor.createReadStream({start: Number(offset), autoClose: false});
      const stream = {kind: 'file-read', socket};
      const status = fileStatus();
      stream.handle = allocate(stream);
      socket.on('readable', () => readable(stream));
      socket.on('end', () => { finishFileStatus(status); readable(stream); });
      socket.on('close', () => { finishFileStatus(status, true); readable(stream); });
      socket.on('error', () => { finishFileStatus(status, true); readable(stream); });
      store(p, stream.handle); store(p + 4, status.handle);
    },
    'drop-file'(handle) {
      handles.get(handle)?.descriptor.close().catch(() => {});
      handles.delete(handle);
    },
    'drop-file-status'(handle) { handles.delete(handle); },
    'drop-task'(handle) { handles.delete(handle); },
    'drop-socket'(handle) {
      handles.get(handle)?.socket?.destroy();
      handles.delete(handle);
    },
    'new-set': () => allocate({kind: 'set'}),
    join() {},
  };
  const close = () => {
    if (closed) return;
    closed = true;
    events.clear();
    for (const remove of cleanup) remove();
    for (const socket of sockets) socket.destroy();
    for (const {server} of listeners) server.close();
    for (const resource of handles.values()) {
      if (resource.kind === 'file-read') resource.socket.destroy();
      if (resource.kind === 'file') resource.descriptor.close().catch(() => {});
    }
    handles.clear();
  };
  try {
    ({instance} = await WebAssembly.instantiate(core, {'': services}));
    instance.exports.configure(0, Number(devServer) | Number(tcpBridge) << 1);
    instance.exports.configure(1, tcpPort); instance.exports.configure(2, websocketPort); instance.exports.configure(3, httpPort);
    instance.exports.run();
    const addresses = await Promise.all(listeners.map(listener => listener.ready));
    const tcp = tcpBridge && addresses.find(address => address.port === tcpPort);
    const websocket = addresses.find(address => address.port === websocketPort);
    const http = devServer && addresses.find(address => address.port === httpPort);
    return {tcp, websocket, http, close};
  } catch (error) { close(); throw error; }
}
