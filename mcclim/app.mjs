import {Miricl} from './miricl.js';

const status = document.getElementById('status');
const examples = document.getElementById('examples');
const fail = error => {
  console.error(error);
  status.textContent = error.message ?? String(error);
  status.className = 'error';
};
document.getElementById('restart').addEventListener('click', () => location.reload());
try {
  const response = await fetch('./miricl-mcclim.wasm.gz');
  if (!response.ok) throw new Error(`Cannot load McCLIM (${response.status}).`);
  const app = await Miricl.instantiate(await response.arrayBuffer(), {
    onOutput: text => console.log(text), onThreadError: fail,
  });
  globalThis.mcclim = app;
  app.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  const source = await fetch('./startup.lisp', {cache:'no-cache'});
  if (!source.ok) throw new Error(`Cannot load the launcher (${source.status}).`);
  app.mountFile('/startup.lisp', await source.text());
  await app.evaluateAsync('(load "/startup.lisp" :verbose nil) (values)', {eof:true});
  const launch = async () => {
    examples.disabled = true;
    try {
      await app.evaluateAsync('(mcclim-browser::start-demo)', {eof:true});
      status.textContent = 'Ready';
      status.className = '';
    } catch (error) {fail(error);}
    finally {examples.disabled = false;}
  };
  examples.addEventListener('click', launch);
  await launch();
} catch (error) {fail(error);}
