// Opt-in development adapter for an application's existing interpreter.
// Binary WebSocket messages carry the terminal stream. Text messages carry
// compiler packets; application objects and execution stay in the browser.
const compilerServiceName = [7, ...new TextEncoder().encode('service')];
const compilerBridgeModule = new WebAssembly.Module(new Uint8Array([
  0,97,115,109,1,0,0,0,
  1,9,1,96,3,127,111,111,2,111,127,
  2,16,1,4,104,111,115,116,...compilerServiceName,0,0,
  7,11,1,...compilerServiceName,0,0,
]));
export class DevRepl {
  constructor(interpreter, options = true) {
    if (options === true) options = {};
    if (!options || typeof options !== 'object')
      throw new TypeError('dev must be true or a configuration object.');
    this.url = new URL(options.url ?? 'ws://127.0.0.1:4006/repl');
    if (!['ws:', 'wss:'].includes(this.url.protocol))
      throw new TypeError('The development REPL URL must use ws: or wss:.');
    this.delay = options.reconnectDelay ?? 500;
    if (!Number.isFinite(this.delay) || this.delay < 0)
      throw new TypeError('reconnectDelay must be a nonnegative number.');
    this.onStatus = options.onStatus;
    this.interpreter = interpreter;
    this.encoder = new TextEncoder();
    this.closed = false;
    this.compilations = [];
    this.compilerRequests = new Set();
    this.compilerTable = interpreter.vm['compiler-services'];
    if (this.compilerTable && interpreter.threads) {
      this.compilerPrevious = this.compilerTable.get(0);
      this.compilerService = new WebAssembly.Instance(compilerBridgeModule,
        {host: {service: (...args) => this.compileService(...args)}}).exports.service;
    }
    // Serialize remote evaluations with the application's asynchronous calls.
    // JS callbacks still reenter the evaluator through its existing callback API.
    this.evaluate = interpreter.evaluateAsync;
    this.evaluations = Promise.resolve();
    this.queuedEvaluate = (...args) => {
      const result = this.evaluations.then(() => {
        this.filesystem?.refresh();
        return this.evaluate.call(interpreter, ...args);
      });
      this.evaluations = result.catch(() => {});
      return result;
    };
    this.output = interpreter.onOutput;
    this.threadError = interpreter.onThreadError;
    this.forwardOutput = output => {
      this.send(output);
      this.output?.(output);
    };
    this.forwardThreadError = error => {
      this.send(this.errorText(error));
      this.threadError?.(error);
    };
    interpreter.evaluateAsync = this.queuedEvaluate;
    interpreter.onOutput = this.forwardOutput;
    interpreter.onThreadError = this.forwardThreadError;
    this.connect();
  }

  status(state) { this.state = state; this.onStatus?.(state); }

  connect() {
    if (this.closed) return;
    this.status('connecting');
    let socket;
    try { socket = new WebSocket(this.url); }
    catch { this.retry(); return; }
    this.socket = socket;
    socket.binaryType = 'arraybuffer';
    const session = {socket, decoder: new TextDecoder('utf-8', {fatal: true}),
      lines: '', source: '', queuedBytes: 0, queue: Promise.resolve()};
    socket.onopen = () => {
      if (this.socket !== socket) return;
      if (this.compilerService) this.compilerTable.set(0, this.compilerService);
      this.status('connected');
    };
    socket.onmessage = ({data}) => {
      if (typeof data === 'string') {
        const request = this.compilations.shift();
        if (!request) { socket.close(1002, 'Unexpected compiler response'); return; }
        if (this.encoder.encode(data).length > 1048562) { socket.close(1009, 'Compiler response too large'); return; }
        request.result = data; request.ready = true;
        if (!request.cancelled) this.wakeCompiler(request);
        this.sendCompilation();
        return;
      }
      session.queuedBytes += data.byteLength;
      if (session.queuedBytes > 1024 * 1024) { socket.close(1009, 'Input queue full'); return; }
      session.queue = session.queue.then(async () => {
        if (this.closed || this.socket !== socket || socket.readyState !== WebSocket.OPEN) return;
        session.lines += session.decoder.decode(data, {stream: true});
        session.queuedBytes -= data.byteLength;
        let end;
        while ((end = session.lines.indexOf('\n')) !== -1) {
          session.source += session.lines.slice(0, end + 1);
          session.lines = session.lines.slice(end + 1);
          if (session.source.length > 1024 * 1024) throw new Error('REPL input exceeds 1 MiB.');
          try {
            const result = await this.interpreter.evaluateAsync(session.source);
            this.send(result.output, socket);
            // Application close code 4000 asks the host to exit, rather than
            // treating :quit as a transient browser disconnection.
            if (result.quit) { this.close(4000); return; }
            if (!result.incomplete) session.source = '';
            this.send(result.incomplete ? '...> ' : 'miricl> ', socket);
          } catch (error) {
            session.source = '';
            this.send(this.errorText(error) + 'miricl> ', socket);
          }
          if (this.closed || this.socket !== socket || socket.readyState !== WebSocket.OPEN) return;
        }
        if (session.lines.length + session.source.length > 1024 * 1024)
          throw new Error('REPL input exceeds 1 MiB.');
      }).catch(error => {
        this.send(this.errorText(error), socket);
        socket.close(1007, 'Invalid input');
      });
    };
    // Error is followed by close, including a refused local connection.
    socket.onerror = () => {};
    socket.onclose = () => {
      if (this.socket !== socket) return;
      this.stopCompiler(); this.retry();
    };
  }

  compileService(op, packet, value) {
    if (op === -2) {
      value.cancelled = true; this.compilerRequests.delete(value);
      if (this.state !== 'connected' && this.compilerRequests.size === 0) this.detachCompiler();
      return [null, 0];
    }
    if (op === -1) {
      if (!value.ready) return [value, 1];
      return value.error ? [null, 2] : [this.interpreter.javascript.makeText(value.result), 0];
    }
    const text = this.interpreter.javascript.text(packet);
    if (this.closed || this.socket?.readyState !== WebSocket.OPEN ||
        this.encoder.encode(text).length > 1048562) return [null, 2];
    const request = {text, thread: value};
    this.compilerRequests.add(request);
    this.compilations.push(request);
    if (this.compilations.length === 1) this.sendCompilation();
    return [request, 1];
  }

  sendCompilation() {
    const request = this.compilations[0];
    if (request && !request.sent && this.socket?.readyState === WebSocket.OPEN) {
      request.sent = true; this.socket.send(request.text);
    }
  }

  wakeCompiler(request) {
    this.interpreter.vm['filesystem-wake'](request.thread);
    this.interpreter.threads.schedule();
  }

  stopCompiler() {
    for (const request of this.compilations.splice(0)) {
      request.error = true; request.ready = true;
      if (!request.cancelled) this.wakeCompiler(request);
    }
    if (this.compilerRequests.size === 0) this.detachCompiler();
  }

  detachCompiler() {
    if (this.compilerService && this.compilerTable.get(0) === this.compilerService)
      this.compilerTable.set(0, this.compilerPrevious);
  }

  retry() {
    if (this.closed) return;
    this.status('disconnected');
    this.timer = setTimeout(() => this.connect(), this.delay);
  }

  send(text, socket = this.socket) {
    if (!text || this.closed || socket !== this.socket || socket?.readyState !== WebSocket.OPEN) return;
    const bytes = this.encoder.encode(text);
    // Small binary frames allow arbitrary UTF-8 sequences to cross TCP chunks.
    for (let offset = 0; offset < bytes.length; offset += 16384) {
      if (socket.bufferedAmount > 1024 * 1024) { socket.close(1009, 'Output queue full'); return; }
      socket.send(bytes.subarray(offset, offset + 16384));
    }
  }

  errorText(error) {
    return `${error.output ?? ''}Error: ${error.message}\n${error.backtrace ? error.backtrace + '\n' : ''}`;
  }

  close(code = 1000) {
    if (this.closed) return;
    this.closed = true;
    this.stopCompiler();
    this.filesystem?.close();
    clearTimeout(this.timer);
    this.socket?.close(code);
    const interpreter = this.interpreter;
    if (interpreter.evaluateAsync === this.queuedEvaluate) interpreter.evaluateAsync = this.evaluate;
    if (interpreter.onOutput === this.forwardOutput) interpreter.onOutput = this.output;
    if (interpreter.onThreadError === this.forwardThreadError) interpreter.onThreadError = this.threadError;
    this.status('closed');
  }
}
