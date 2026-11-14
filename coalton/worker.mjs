import {Miricl} from './miricl.mjs';
import {createCoalton} from './coalton.mjs';
let session, busy = false;
const debuggerStops = new Map();
const resource = async (name, options) => {
  const response = await fetch(new URL(name, import.meta.url), options);
  if (!response.ok) throw new Error(`Unable to load ${name}: ${response.status}`);
  return response;
};
const bytes = async name => Miricl.decompress(await (await resource(name)).arrayBuffer());
onmessage = async ({data}) => {
  // Evaluation is suspended while a stop is open. Debugger commands must pass
  // through even though the ordinary evaluation request is still busy.
  if (data.type === 'debug-command') {
    try {
      const stop = debuggerStops.get(data.sessionId);
      if (!stop || stop.stopId !== data.stopId) throw new Error('This debugger stop has expired.');
      await session.debugCommand(stop, data.command);
    } catch (error) {
      postMessage({type: 'debug-command-error', sessionId: data.sessionId, stopId: data.stopId, message: error.message});
    }
    return;
  }
  if (busy) return;
  busy = true;
  try {
    if (data.type === 'start') {
      const build = await (await resource('build.json', {cache: 'no-store'})).json();
      let image, core = null;
      if (build.initializedImage) {
        postMessage({type: 'status', message: 'Loading Coalton snapshot'});
        image = await bytes(`coalton-image.wasm.gz?v=${build.snapshotSha256}`);
      } else {
        [image, core] = await Promise.all([bytes('miricl.wasm.gz'), bytes('coalton.wasm.gz')]);
      }
      session = await createCoalton(image, core, {
        replPackage: 'COMMON-LISP-USER',
        onDebugger: event => {
          if (event.type === 'stopped') {
            debuggerStops.set(event.stop.sessionId, event.stop);
            postMessage({type: 'debugger', event: {...event, stop: event.stop.snapshot}});
          } else {
            debuggerStops.delete(event.sessionId); postMessage({type: 'debugger', event});
          }
        },
        progress: message => postMessage({type: 'status', message}),
        onOutput: output => postMessage({type: 'output', output})});
      postMessage({type: 'ready', output: session.startup});
    } else if (data.type === 'evaluate') {
      if (!session) throw new Error('Coalton has not finished loading.');
      const start = performance.now();
      const result = await session.evaluate(data.source, {mode: data.mode});
      postMessage({type: 'result', id: data.id, ...result, milliseconds: Math.round(performance.now() - start)});
    }
  } catch (error) {
    postMessage({type: 'error', id: data.id, message: error.message,
      output: error.output ?? '', backtrace: error.backtrace ?? ''});
  } finally {busy = false;}
};
