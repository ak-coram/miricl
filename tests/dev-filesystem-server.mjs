// Both embedded servers expose the same bounded development filesystem.
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {startHost, testPorts} from './repl-bridge-fixture.mjs';

const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-host-files-'));
const root = path.join(directory, 'root');
await fs.mkdir(path.join(root, 'empty'), {recursive: true});
const bytes = Uint8Array.from({length: 1200000}, (_, i) => i % 256);
await fs.writeFile(path.join(root, 'binary λ.bin'), bytes);
await fs.utimes(path.join(root, 'binary λ.bin'), 1700000000, 1700000000);
await fs.writeFile(path.join(directory, 'outside'), 'outside');
await fs.symlink(path.join(directory, 'outside'), path.join(root, 'escape'));
for (let i = 0; i < 80; i++) await fs.writeFile(path.join(root, `long-${i}-${'x'.repeat(210)}`), '');

function request(target, headers = {}) {
  return new Promise((resolve, reject) => {
    const req = http.get({hostname: '127.0.0.1', port: testPorts.http, path: target, headers}, response => {
      const chunks = [];
      response.on('data', chunk => chunks.push(chunk));
      response.on('end', () => resolve({status: response.statusCode, headers: response.headers, bytes: Buffer.concat(chunks)}));
      response.on('error', reject);
    });
    req.on('error', reject);
    req.setTimeout(10000, () => req.destroy(new Error('Filesystem HTTP timeout')));
  });
}

try {
  for (const [runtime, binary] of [['node', process.argv[2] ?? 'miricl-interpreter.wasm'],
    ...(process.argv[3] ? [['wasmtime', process.argv[3]]] : [])]) {
    const host = await startHost(runtime, path.resolve(binary), 'dev-server', root);
    try {
      const stat = await request('/__miricl__/fs/stat/binary%20%CE%BB.bin');
      assert.equal(stat.status, 200);
      assert.equal(stat.bytes.length, 24);
      assert.equal(stat.bytes.readUInt32LE(0), 5);
      assert.equal(stat.bytes.readUInt32LE(4), 1);
      assert.equal(stat.bytes.readBigInt64LE(8), 1700000000n);
      assert.equal(stat.bytes.readBigUInt64LE(16), BigInt(bytes.length));
      const read = await request('/__miricl__/fs/read/binary%20%CE%BB.bin');
      assert.equal(read.status, 200);
      assert.equal(read.headers['content-length'], String(bytes.length));
      assert.deepEqual(read.bytes, Buffer.from(bytes));
      const empty = await request('/__miricl__/fs/list/empty');
      assert.equal(empty.status, 200);
      assert.deepEqual(empty.bytes, Buffer.alloc(8));
      // Repeated long names would exhaust a bump allocator after two scans.
      for (let i = 0; i < 12; i++) {
        const list = await request('/__miricl__/fs/list/');
        assert.equal(list.status, 200);
        assert.ok(list.bytes.length > 17000);
        assert.ok(list.bytes.subarray(-8).equals(Buffer.alloc(8)));
      }
      for (const target of ['/__miricl__/fs/stat/missing', '/__miricl__/fs/list/missing',
        '/__miricl__/fs/read/empty', '/__miricl__/fs/stat/%2e%2e/outside'])
        assert.equal((await request(target)).status, 404, target);
      assert.equal((await request('/__miricl__/fs/read/escape')).status, runtime === 'node' ? 200 : 403);
      for (const headers of [{Origin: 'http://evil.invalid'}, {Origin: 'null'}, {Host: 'evil.invalid'}]) {
        const result = await request('/__miricl__/fs/stat/', headers);
        assert.equal(result.status, 403);
        assert.equal(result.headers['access-control-allow-origin'], undefined);
      }
      const allowed = await request('/__miricl__/fs/stat/', {Origin: 'http://localhost:9000'});
      assert.equal(allowed.status, 200);
      assert.equal(allowed.headers['access-control-allow-origin'], '*');
      const missing = await request('/__miricl__/fs/stat/missing', {Origin: 'http://localhost:9000'});
      assert.equal(missing.headers['access-control-allow-origin'], '*');
      for (let i = 0; i < 12; i++) {
        const controller = new AbortController();
        const response = await fetch(`http://127.0.0.1:${testPorts.http}/__miricl__/fs/list/`, {signal: controller.signal});
        assert.equal(response.status, 200); controller.abort();
      }
      assert.equal((await request('/__miricl__/fs/stat/')).status, 200);
      console.log(`PASS ${runtime} filesystem HTTP: metadata, binary streaming, directory allocation, path boundaries, origins, and aborted clients`);
    } finally { await host.close(); }
  }
} finally {
  await fs.rm(directory, {recursive: true, force: true});
}
