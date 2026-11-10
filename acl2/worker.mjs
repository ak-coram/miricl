import {decodeFilesystem} from './filesystem-archive.mjs';
import {createAcl2} from './acl2.mjs';

let session, starting = false;
const resource = async name => {
  const response = await fetch(new URL(name, import.meta.url));
  if (!response.ok) throw new Error(`Unable to load ${name}: HTTP ${response.status}.`);
  return response;
};
const bytes = async name => (await resource(name)).arrayBuffer();
const image = async () => {
  const response = await resource('miricl.wasm');
  return WebAssembly.compileStreaming && response.headers.get('Content-Type')?.split(';')[0].trim() === 'application/wasm'
    ? WebAssembly.compileStreaming(response) : WebAssembly.compile(await response.arrayBuffer());
};
const decompress = async name => new Response((await resource(name)).body
  .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
const filesystem = async () => decodeFilesystem(await decompress('filesystem.bin.gz'));
onmessage = async ({data}) => {
  try {
    if (data.type === 'start') {
      if (session || starting) throw new Error('ACL2 has already been started.');
      starting = true;
      postMessage({type: 'status', message: 'Loading ACL2 assets'});
      const manifest = await fetch(new URL('build.json', import.meta.url), {cache: 'no-store'});
      if (!manifest.ok) throw new Error('The ACL2 build manifest is unavailable.');
      const build = await manifest.json();
      if (!build.coreAvailable)
        throw new Error('The ACL2 core is not included in this frontend preview.');
      const [module, core, files] = build.initializedWorld
        ? [await decompress(`acl2-image.wasm.gz?v=${build.imageSha256}`), null, []]
        : await Promise.all([image(), bytes('acl2.wasm'), filesystem()]);
      session = await createAcl2(module, core, files, {
        booksURL: new URL(`books/filesystem.bin.gz?v=${build.books.sha256}`, import.meta.url),
        progress: message => postMessage({type: 'status', message}),
        onOutput: output => postMessage({type: 'output', output}),
      });
      postMessage({type: 'ready', output: session.startup});
    } else if (data.type === 'evaluate') {
      if (!session) throw new Error('ACL2 is not ready.');
      postMessage({type: 'result', ...await session.evaluate(data.source)});
    }
  } catch (error) {
    postMessage({type: 'error', message: error.message, output: error.output ?? '',
      backtrace: error.backtrace ?? '', fatal: !session || Boolean(error.fatal)});
  } finally {if (data.type === 'start') starting = false;}
};
