import http from 'node:http';
import {readFile} from 'node:fs/promises';
import path from 'node:path';

const [directory = 'build/ironclad/site', port = '8000'] = process.argv.slice(2);
const root = path.resolve(directory);
const types = new Map([['.html', 'text/html; charset=utf-8'], ['.mjs', 'text/javascript; charset=utf-8'],
  ['.json', 'application/json'], ['.gz', 'application/gzip'], ['.lisp', 'text/plain; charset=utf-8']]);
await readFile(path.join(root, 'index.html'));
const server = http.createServer(async (request, response) => {
  const name = request.url.split('?', 1)[0].slice(1) || 'index.html';
  if (!/^[a-z0-9][a-z0-9._-]*$/iu.test(name)) return response.writeHead(404).end();
  try {
    const bytes = await readFile(path.join(root, name));
    response.writeHead(200, {'Content-Type': types.get(path.extname(name)) ?? 'text/plain; charset=utf-8'}).end(bytes);
  } catch (error) {
    response.writeHead(error.code === 'ENOENT' ? 404 : 500).end();
  }
});
server.listen(Number(port), '127.0.0.1', () => console.log(`http://127.0.0.1:${server.address().port}/`));
