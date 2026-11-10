import {createCoalton} from './coalton.mjs';
let session, busy = false;
const bytes = async name => {
  const response = await fetch(new URL(name, import.meta.url));
  if (!response.ok) throw new Error(`Unable to load ${name}: ${response.status}`);
  return response.arrayBuffer();
};
onmessage = async ({data}) => {
  if (busy) return;
  busy = true;
  try {
    if (data.type === 'start') {
      const [image, core] = await Promise.all([bytes('miricl.wasm'), bytes('coalton.wasm')]);
      session = await createCoalton(image, core, {
        progress: message => postMessage({type: 'status', message}),
        onOutput: output => postMessage({type: 'output', output})});
      postMessage({type: 'ready', output: session.startup});
    } else if (data.type === 'evaluate') {
      if (!session) throw new Error('Coalton has not finished loading.');
      const start = performance.now();
      const result = await session.evaluate(data.source, {mode: data.mode});
      postMessage({type: 'result', id: data.id, ...result, milliseconds: performance.now() - start});
    }
  } catch (error) {
    postMessage({type: 'error', id: data.id, message: error.message,
      output: error.output ?? '', backtrace: error.backtrace ?? ''});
  } finally {busy = false;}
};
