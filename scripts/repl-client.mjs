// The HTML builder embeds this module and its dependencies in the page.
import {config} from './repl-examples.mjs';
import {createReplDebugger} from './repl-debugger.mjs';

config.createDebugger = createReplDebugger;
config.createWorker = () => {
  const url = URL.createObjectURL(new Blob([document.getElementById('miricl-worker').textContent], {type: 'text/javascript'}));
  const running = new Worker(url);
  const release = () => URL.revokeObjectURL(url);
  running.addEventListener('message', event => {if (event.data.type === 'ready') release();});
  running.addEventListener('error', release);
  const terminate = running.terminate.bind(running), post = running.postMessage.bind(running);
  running.terminate = () => {release(); terminate();};
  running.postMessage = data => {
    if (data.type === 'start') {
      const encoded = document.getElementById('miricl-wasm').textContent.trim();
      const bytes = Uint8Array.from(atob(encoded), char => char.charCodeAt(0));
      post({type: 'initialize', bytes: bytes.buffer}, [bytes.buffer]);
    } else post(data);
  };
  return running;
};
