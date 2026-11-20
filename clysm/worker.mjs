// The HTML packager supplies these three local, self-contained modules.
import {loadMiricl} from '@@MIRICL_LOADER_URL@@';
import {decodeFilesystem} from '@@FILESYSTEM_URL@@';
import {loadClysm} from '@@CLYSM_HOST_URL@@';

let lisp;
let busy = false;
const send = (type, data = {}) => postMessage({type, ...data});
const decode = text => Uint8Array.from(atob(text), character => character.charCodeAt(0));
const decompress = async bytes => new Uint8Array(await new Response(new Blob([bytes]).stream()
  .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer());
const prompt = () => globalThis.clysmRepl.packageName() + '> ';

async function bootstrap({image, sources}) {
  const start = performance.now();
  send('status', {message: 'Starting MIRICL…'});
  let vm;
  try {
    vm = await loadMiricl(decode(image), {onOutput: text => send('output', {text, fd: 2})});
    if (!(sources instanceof ArrayBuffer)) throw new TypeError('Expected Clysm source filesystem bytes.');
    const files = decodeFilesystem(await decompress(new Uint8Array(sources)));
    for (const [name, bytes] of files) {
      if (!/^\/(?:clysm\/(?:[a-z0-9-]+\/)*[a-z0-9-]+\.lisp|clysm-bootstrap\.lisp)$/u.test(name))
        throw new Error(`Invalid Clysm source path: ${name}`);
      vm.mountFile(name, bytes);
    }
    send('status', {message: 'Bootstrapping Clysm from Common Lisp sources…'});
    await vm.evaluateAsync('(load "/clysm-bootstrap.lisp")', {eof: true});
    let step = await vm.evaluateAsync('(miricl-clysm:start)', {eof: true});
    for (;;) {
      const lastLine = step.output.trim().split('\n').at(-1);
      if (lastLine === 'NIL') break;
      send('status', {message: lastLine.replace(/^"|"$/gu, '')});
      step = await vm.evaluateAsync('(miricl-clysm:step)', {eof: true});
    }
    const bytes = await vm.readFile('/clysm.wasm');
    vm.close();
    vm = null;
    send('status', {message: 'Starting Clysm…'});
    lisp = await loadClysm(bytes, (text, fd) => send('output', {text, fd}));
    send('ready', {prompt: prompt(), wasmBytes: bytes.length,
      bootstrapMilliseconds: Math.round(performance.now() - start)});
  } finally {vm?.close();}
}

onmessage = async ({data}) => {
  try {
    if (!data || typeof data !== 'object') throw new TypeError('Expected a REPL request.');
    if (data.type === 'start') {
      if (busy || lisp) throw new Error('Clysm has already started.');
      busy = true;
      try {await bootstrap(data);} finally {busy = false;}
    } else if (data.type === 'evaluate') {
      if (!lisp || busy) throw new Error('Clysm is not ready.');
      if (typeof data.source !== 'string') throw new TypeError('Expected Lisp source text.');
      busy = true;
      const bsp = lisp.instance.exports.get_bsp();
      try {send('result', {result: lisp.eval(data.source), prompt: prompt()});}
      catch (error) {
        // JavaScript exceptions bypass Lisp's dynamic binding cleanup.
        globalThis.clysmRepl.unbindTo(bsp);
        send('error', {message: error instanceof RangeError ? 'Control stack exhausted.' : error.message,
          fatal: false, prompt: prompt()});
      } finally {busy = false;}
    } else throw new Error('Unknown REPL request.');
  } catch (error) {send('error', {message: error.message ?? String(error), fatal: true});}
};
