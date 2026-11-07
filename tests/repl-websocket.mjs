// Exercise the shared Wasm WebSocket implementation with raw TCP frames.
import assert from 'node:assert/strict';
import net from 'node:net';
import path from 'node:path';
import {connectTerminal, startHost, waitFor} from './repl-bridge-fixture.mjs';

const binary = path.resolve(process.argv[2] ?? 'interpreter.wasm');
const wasi = process.argv[3] && path.resolve(process.argv[3]);
const request = 'GET /repl HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\n' +
  'Connection: keep-alive, Upgrade\r\nSec-WebSocket-Version: 13\r\n' +
  'Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nOrigin: http://localhost:8000\r\n\r\n';

function frame(opcode, payload = Buffer.alloc(0), final = true) {
  payload = Buffer.from(payload);
  const extended = payload.length < 126 ? 0 : payload.length < 65536 ? 2 : 8;
  const header = Buffer.alloc(6 + extended);
  header[0] = (final ? 128 : 0) | opcode;
  header[1] = 128 | (extended === 0 ? payload.length : extended === 2 ? 126 : 127);
  if (extended === 2) header.writeUInt16BE(payload.length, 2);
  if (extended === 8) header.writeBigUInt64BE(BigInt(payload.length), 2);
  const mask = Buffer.from([19, 37, 53, 71]); mask.copy(header, 2 + extended);
  return Buffer.concat([header, payload.map((byte, i) => byte ^ mask[i % 4])]);
}

async function connect(headers = request) {
  const socket = net.connect(5706, '127.0.0.1');
  let buffer = Buffer.alloc(0), ended = false, failure;
  socket.on('data', bytes => { buffer = Buffer.concat([buffer, bytes]); });
  socket.on('close', () => { ended = true; }); socket.on('error', error => { failure = error; });
  socket.write(headers);
  const ready = predicate => waitFor(() => {
    if (failure) throw failure;
    if (predicate()) return true;
    if (ended) throw new Error('WebSocket closed before the expected response.');
  }, 'WebSocket response');
  await ready(() => buffer.includes('\r\n\r\n'));
  const end = buffer.indexOf('\r\n\r\n') + 4;
  const response = buffer.subarray(0, end).toString(); buffer = buffer.subarray(end);
  return {socket, response, async readFrame() {
    await ready(() => buffer.length >= 2);
    assert.equal(buffer[1] & 128, 0, 'Server frames must not be masked.');
    const opcode = buffer[0] & 15;
    let size = buffer[1] & 127, offset = 2;
    if (size === 126) { await ready(() => buffer.length >= 4); size = buffer.readUInt16BE(2); offset = 4; }
    if (size === 127) { await ready(() => buffer.length >= 10); size = Number(buffer.readBigUInt64BE(2)); offset = 10; }
    await ready(() => buffer.length >= offset + size);
    const payload = buffer.subarray(offset, offset + size); buffer = buffer.subarray(offset + size);
    return {opcode, payload};
  }};
}

for (const [runtime, image] of [['node', binary], ...(wasi ? [['wasmtime', wasi]] : [])]) {
  const host = await startHost(runtime, image, 'repl-bridge', '.', {tcp: 5705, websocket: 5706});
  let browser, terminal;
  try {
    for (const headers of [
      request.replace('http://localhost:8000', 'https://example.com'),
      request.replace('dGhlIHNhbXBsZSBub25jZQ==', 'dGhlIHNhbXBsZSBub25jZS=='),
      request.replace('Sec-WebSocket-Version: 13', 'Sec-WebSocket-Version: 12'),
      request.replace('Sec-WebSocket-Key:', 'Sec-WebSocket-Key: bad\r\nSec-WebSocket-Key:')
    ]) {
      const connection = await connect(headers);
      try { assert.match(connection.response, /^HTTP\/1.1 400/); } finally { connection.socket.destroy(); }
    }
    browser = await connect();
    assert.match(browser.response, /^HTTP\/1.1 101/);
    assert.match(browser.response, /Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK\+xOo=/);
    terminal = await connectTerminal(5705); await terminal.ready();
    terminal.socket.write('(+ 1 2)\n');
    let received = await browser.readFrame();
    assert.equal(received.opcode, 2); assert.equal(received.payload.toString(), '(+ 1 2)\n');
    const start = terminal.output.length;
    const frames = Buffer.concat([frame(2, 'λ', false), frame(9, 'ping'), frame(0, '🙂\nmiricl> ')]);
    // Split header, mask, payload, and UTF-8 sequences over writes.
    for (let i = 0; i < frames.length; i++) browser.socket.write(frames.subarray(i, i + 1));
    received = await browser.readFrame();
    assert.equal(received.opcode, 10); assert.equal(received.payload.toString(), 'ping');
    await waitFor(() => terminal.output.slice(start) === 'λ🙂\nmiricl> ', 'fragmented binary message');
    for (const length of [126, 65536]) {
      const start = terminal.output.length;
      browser.socket.write(frame(2, Buffer.alloc(length, 97)));
      await waitFor(() => terminal.output.length === start + length, `binary frame length ${length}`);
      assert.equal(terminal.output.slice(start), 'a'.repeat(length));
    }
    browser.socket.write(frame(8, Buffer.from([3, 232])));
    received = await browser.readFrame(); assert.equal(received.opcode, 8); assert.equal(received.payload.readUInt16BE(), 1000);
    await waitFor(() => terminal.output.endsWith('[Browser disconnected; waiting for a browser]\n'), 'normal close status');
    assert.equal(terminal.ended, false);
    browser.socket.destroy();
    for (const [bytes, code] of [
      [Buffer.from([130, 0]), 1002], // Unmasked input.
      [frame(1, 'text'), 1003],
      [frame(0, 'unexpected continuation'), 1002],
      [frame(8, Buffer.from([3, 237])), 1002], // Reserved close code 1005.
      [frame(8, Buffer.from([3, 232, 255])), 1007],
      [Buffer.from([130, 254, 0, 1, 0, 0, 0, 0, 97]), 1002], // Nonminimal length.
      [Buffer.from([130, 255, 128, 0, 0, 0, 0, 0, 0, 0]), 1002],
      [Buffer.from([130, 255, 0, 0, 0, 1, 0, 0, 0, 0]), 1009]
    ]) {
      browser = await connect(); assert.match(browser.response, /^HTTP\/1.1 101/);
      browser.socket.write(bytes);
      const closed = await browser.readFrame(); assert.equal(closed.opcode, 8); assert.equal(closed.payload.readUInt16BE(), code);
      browser.socket.destroy();
      // Wait for the previous browser record to be released before reconnecting.
      await waitFor(() => host.output.endsWith('[Browser disconnected; waiting for a browser]\n'), `WebSocket cleanup (${code})`)
        .catch(error => { throw new Error(`${error.message}; terminal tail: ${JSON.stringify(host.output.slice(-200))}`); });
    }
    console.log(`PASS ${runtime} WebSocket handshake, masking, fragments, ping/pong, lengths, and protocol errors`);
  } finally { browser?.socket.destroy(); terminal?.socket.destroy(); await host.close(); }
}
