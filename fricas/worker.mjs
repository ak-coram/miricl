import {decodeFilesystem} from './filesystem-archive.mjs';
import {createFriCAS} from './fricas.mjs';

let session;
const bytes = async name => {
  const response = await fetch(new URL(name, import.meta.url));
  if (!response.ok) throw new Error(`Unable to load ${name}: ${response.status}`);
  return response.arrayBuffer();
};
onmessage = async ({data}) => {
  try {
    if (data.type === 'start') {
      const [image, core, encoded] = await Promise.all([
        bytes('miricl.wasm'), bytes('fricas.wasm'), bytes('filesystem.bin.gz'),
      ]);
      const decoded = await new Response(new Blob([encoded]).stream()
        .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
      const files = decodeFilesystem(decoded);
      session = await createFriCAS(image, core, files, {
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
