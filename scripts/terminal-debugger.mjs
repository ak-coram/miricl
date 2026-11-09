// Text frontend for the same stop handles used by browser applications.
export class TerminalDebugger {
  constructor({write = text => process.stdout.write(text), onReady = () => {}} = {}) {
    this.write = write;
    this.onReady = onReady;
    this.sessions = new Map();
    this.waiting = new Set();
    this.frameIds = new Map();
  }

  get active() { return this.sessions.get(this.selected); }
  get ready() { return !!this.active && this.waiting.has(this.selected); }
  prompt() {
    const stop = this.active;
    return stop ? `debug[${stop.threadId}:${stop.level}]> ` : '';
  }

  handle(event) {
    if (event.type === 'closed') {
      this.write(`[Debugger session ${event.sessionId} exited]\n`);
      this.sessions.delete(event.sessionId);
      this.waiting.delete(event.sessionId);
      this.frameIds.delete(event.sessionId);
      if (this.selected === event.sessionId)
        this.selected = this.sessions.has(event.parentSessionId) ? event.parentSessionId : this.sessions.keys().next().value;
      this.onReady();
      return;
    }
    const stop = event.stop;
    const first = !this.sessions.has(stop.sessionId);
    this.sessions.set(stop.sessionId, stop);
    this.waiting.add(stop.sessionId);
    if (this.selected === undefined || stop.parentSessionId === this.selected) this.selected = stop.sessionId;
    if (!this.frameIds.has(stop.sessionId)) this.frameIds.set(stop.sessionId, stop.frames.find(frame => frame.inspectable)?.id);
    if (first) {
      this.write(`\nDebugger: ${stop.threadName} (#${stop.threadId}), level ${stop.level}, session ${stop.sessionId}\n`);
      this.write(`${stop.condition.type}: ${stop.condition.message}\n${stop.backtrace}`);
      this.write(this.restarts(stop));
      this.write('Commands: bt, frame N, locals, eval FORM, restart N [LIST-FORM], into, next, out, resume, abort, sessions, session N, help\n');
    } else if (stop.result) {
      this.write(stop.result.output);
      this.write(stop.result.error ? `Error: ${stop.result.error}\n` : stop.result.values.map(value => `${value}\n`).join(''));
    }
    if (this.selected === stop.sessionId) this.showFrame(stop);
    else this.write(`Session ${stop.sessionId} is waiting; use "session ${stop.sessionId}" to select it.\n`);
    this.onReady();
  }

  restarts(stop) {
    return 'Restarts:\n' + (stop.restarts.length ? stop.restarts.map(restart =>
      `  ${String(restart.id).padStart(2)}: ${restart.name} — ${restart.description}\n`).join('') : '      No restarts are available.\n');
  }

  showFrame(stop = this.active) {
    const frame = stop?.frames.find(frame => frame.id === this.frameIds.get(stop.sessionId));
    if (frame) this.write(`  ${String(frame.id).padStart(2)}: ${frame.name}\n      ${frame.form}\n`);
  }

  async command(line) {
    const stop = this.active;
    if (!stop) throw new Error('No debugger session is selected.');
    const text = line.trim();
    const match = /^(\S+)(?:\s+([\s\S]*))?$/.exec(text);
    if (!match) return;
    const [, command, argument = ''] = match;
    const frameId = this.frameIds.get(stop.sessionId);
    const send = action => { this.waiting.delete(stop.sessionId); return action(); };
    try {
      switch (command.toLowerCase()) {
        case 'help': case '?':
          this.write('bt: stack trace; frame N: select a call; locals: bindings; eval FORM: evaluate in that frame\n' +
            'restart N [LIST-FORM]: invoke a listed restart; a number alone also invokes it\n' +
            'into / next / out / resume: stepping controls; abort: unwind this debugger entry\n' +
            'sessions: list paused threads; session N: select a debugger session\n'); return;
        case 'bt': case 'backtrace': this.write(stop.backtrace); return;
        case 'restarts': this.write(this.restarts(stop)); return;
        case 'sessions': case 'threads':
          for (const session of this.sessions.values()) this.write(
            `${session.sessionId === this.selected ? '*' : ' '} ${String(session.sessionId).padStart(2)}: ${session.threadName} (#${session.threadId}), level ${session.level}\n`);
          return;
        case 'session': {
          const id = Number(argument);
          if (!/^\d+$/.test(argument) || !this.sessions.has(id)) throw new Error('Unknown debugger session.');
          this.selected = id; this.showFrame(); return;
        }
        case 'frame': {
          const id = Number(argument);
          if (!/^\d+$/.test(argument) || !stop.frames.some(frame => frame.id === id && frame.inspectable))
            throw new Error('Choose an inspectable interpreter frame from the backtrace.');
          this.frameIds.set(stop.sessionId, id); this.showFrame(); return;
        }
        case 'locals':
          for (const {name, value} of await stop.locals(frameId)) this.write(`      ${name} = ${value}\n`);
          return;
        case 'eval': return await send(() => stop.evaluate(frameId, argument));
        case 'restart': {
          const restart = /^(\d+)(?:\s+([\s\S]*))?$/.exec(argument);
          if (!restart) throw new Error('Use restart N followed by an optional expression returning an argument list.');
          return await send(() => stop.invokeRestart(Number(restart[1]), {argumentsSource: restart[2] ?? ''}));
        }
        case 'step': case 'into': return await send(() => stop.step('into'));
        case 'next': case 'over': return await send(() => stop.step('over'));
        case 'out': return await send(() => stop.step('out'));
        case 'resume': return await send(() => stop.resume());
        case 'abort': return await send(() => stop.abort());
        default:
          if (/^\d+$/.test(text)) return await send(() => stop.invokeRestart(Number(text)));
          return await send(() => stop.evaluate(frameId, text));
      }
    } catch (error) {
      this.write(`Error: ${error.message}\n`);
      // A command rejected before transport leaves the same stop usable.
      if (this.sessions.get(stop.sessionId)?.stopId === stop.stopId) this.waiting.add(stop.sessionId);
    }
  }
}
