// Opt-in development adapter for an application's existing interpreter.
// The TCP/WebSocket relay transports a UTF-8 terminal stream, without RPC.
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
    // Serialize remote evaluations with the application's asynchronous calls.
    // JS callbacks still reenter the evaluator through its existing callback API.
    this.evaluate = interpreter.evaluateAsync;
    this.evaluations = Promise.resolve();
    this.queuedEvaluate = (...args) => {
      const result = this.evaluations.then(() => this.evaluate.call(interpreter, ...args));
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
    socket.onopen = () => { if (this.socket === socket) this.status('connected'); };
    socket.onmessage = ({data}) => {
      if (typeof data === 'string') { socket.close(1003, 'Expected a byte stream'); return; }
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
    socket.onclose = () => { if (this.socket === socket) this.retry(); };
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
    clearTimeout(this.timer);
    this.socket?.close(code);
    const interpreter = this.interpreter;
    if (interpreter.evaluateAsync === this.queuedEvaluate) interpreter.evaluateAsync = this.evaluate;
    if (interpreter.onOutput === this.forwardOutput) interpreter.onOutput = this.output;
    if (interpreter.onThreadError === this.forwardThreadError) interpreter.onThreadError = this.threadError;
    this.status('closed');
  }
}
