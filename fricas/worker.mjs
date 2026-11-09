import {decodeFilesystem} from './filesystem-archive.mjs';
import {createFriCAS} from './fricas.mjs';

let session;
const resource = async name => {
  const response = await fetch(new URL(name, import.meta.url));
  if (!response.ok) throw new Error(`Unable to load ${name}: ${response.status}`);
  return response;
};
const bytes = async name => (await resource(name)).arrayBuffer();
const image = async () => {
  const response = await resource('miricl.wasm');
  return WebAssembly.compileStreaming && response.headers.get('Content-Type')?.split(';')[0].trim() === 'application/wasm'
    ? WebAssembly.compileStreaming(response) : WebAssembly.compile(await response.arrayBuffer());
};
const filesystem = async () => {
  const encoded = await bytes('filesystem.bin.gz');
  const decoded = await new Response(new Blob([encoded]).stream()
    .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
  return decodeFilesystem(decoded);
};
onmessage = async ({data}) => {
  try {
    if (data.type === 'start') {
      const [module, core, files] = await Promise.all([
        image(), bytes('fricas.wasm'), filesystem(),
      ]);
      session = await createFriCAS(module, core, files, {
        lazyFiles: true,
        progress: message => postMessage({type: 'status', message}),
        onPrompt: question => postMessage({type: 'prompt', question}),
        onOutput: output => postMessage({type: 'output', output}),
      });
      postMessage({type: 'ready', output: session.startup});
    } else if (data.type === 'evaluate') {
      postMessage({type: 'result', ...await session.evaluate(data.source, {answers: data.answers})});
    } else if (data.type === 'answer') {
      session.answer(data.text);
    }
  } catch (error) {
    postMessage({type: 'error', message: error.message, output: error.output ?? '', backtrace: error.backtrace ?? ''});
  }
};
