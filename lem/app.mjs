import {loadMiricl} from './miricl-loader.mjs';
const status = document.getElementById('status');
const launchButton = document.getElementById('launch');
const container = document.getElementById('lem-desktop');
const fail = error => {
  globalThis.lemReady = false;
  console.error(error);
  status.textContent = error.message ?? String(error);
  status.className = 'error';
  globalThis.lemFailure = {message: status.textContent, backtrace: error.backtrace};
};
new MutationObserver(() => {
  const state = container.dataset.lemState;
  if (state === 'failed') fail(new Error('LEM frontend failed; inspect the reported Lisp condition.'));
  if (state === 'stopped') {globalThis.lemReady = false; status.textContent = 'Stopped';}
  if (state === 'closing') {globalThis.lemReady = false; status.textContent = 'Stopping LEM…';}
  if (state === 'starting') {
    globalThis.lemReady = false;
    globalThis.lemFailure = undefined;
    status.textContent = 'Starting LEM…';
  }
  if (state === 'ready') {
    globalThis.lemReady = true;
    status.textContent = 'Ready — physical keyboard input';
    status.className = '';
  }
}).observe(container, {attributes: true, attributeFilter: ['data-lem-state']});
document.getElementById('restart').addEventListener('click', () => location.reload());
try {
  const response = await fetch('./miricl-lem.wasm.gz');
  if (!response.ok) throw new Error(`Cannot load LEM (${response.status}).`);
  const app = await loadMiricl(response, {
    onOutput: text => console.log(text), onThreadError: fail,
  });
  globalThis.lem = app;
  if (!app.threads) throw new Error('LEM needs asynchronous Wasm stack switching in this browser.');
  app.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  const launch = async () => {
    launchButton.disabled = true;
    try {
      await app.evaluateAsync('(lem-browser:start-editor) (values)', {eof: true});
      const deadline = performance.now() + 30000;
      while (container.dataset.lemState !== 'ready') {
        if (globalThis.lemFailure) throw new Error(globalThis.lemFailure.message);
        if (container.dataset.lemState === 'failed') throw new Error('LEM startup failed; inspect the Lisp diagnostic.');
        if (performance.now() > deadline) throw new Error('LEM did not render its first frame.');
        await new Promise(resolve => setTimeout(resolve, 25));
      }
      status.textContent = 'Ready — physical keyboard input';
      status.className = '';
      globalThis.lemReady = true;
    } catch (error) {fail(error);}
    finally {launchButton.disabled = false;}
  };
  launchButton.addEventListener('click', launch);
  await launch();
} catch (error) {fail(error);}
