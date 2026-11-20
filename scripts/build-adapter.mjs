// Assemble the distributed adapter from our own modules. This does not embed
// the interpreter, ASDF, application files or any other assets in JavaScript.
import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import {gzipSync} from 'node:zlib';
const source = path.dirname(fileURLToPath(import.meta.url));
const read = name => fs.readFile(path.join(source, name), 'utf8');
const localModule = async (name, exported) => {
  const text = (await read(name)).replace(/^import .*;\n/gm, '')
    .replace(/^export \{[^\n]*\};?\s*$/gm, '').replace(/^export /gm, '');
  return `const{${exported}}=(()=>{${text}${text.endsWith('\n') ? '' : '\n'}return{${exported}}})();\n`;
};
export async function adapterSource({terminal = true} = {}) {
let adapter = await read('miricl.mjs');
for (const statement of ["const {DevRepl} = await import('./dev-repl.mjs');",
  "const {DevFilesystem} = await import('./dev-filesystem.mjs');"]) {
  if (adapter.split(statement).length !== 2) throw new Error('Expected one adapter dependency: '+statement);
  adapter = adapter.replace(statement, '');
}
const browser = await localModule('dev-repl.mjs', 'DevRepl') + await localModule('dev-filesystem.mjs', 'DevFilesystem');
const core = browser + adapter;
// The embedded adapter has the complete runtime/development API. Terminal
// argument parsing and host filesystem mounting belong to the external launcher.
if (!terminal) {
  if (/\bimport\s*(?:\(\s*['"]\.{1,2}\/|[^;\n]*\bfrom\s*['"]\.{1,2}\/|['"]\.{1,2}\/)/u.test(core))
    throw new Error('The embedded adapter has an unbundled relative import.');
  return core;
}
let cli = (await read('repl.mjs')).replace(/^#!.*\n/, '').replace(/^import .*;\n/gm, '')
  .replace("const {startReplBridge} = await import('./repl-bridge.mjs');", '')
  .replace("const {replCompilerImage} = await import('./repl-bridge.mjs');", '')
  .replace("const {HostFilesystem} = await import('./host-filesystem.mjs');", '')
  .replace("const {TerminalDebugger} = await import('./terminal-debugger.mjs');", '')
  .replace('node scripts/repl.mjs', 'node miricl.js');
const terminalSupport = await localModule('host-filesystem.mjs', 'HostFilesystem') +
  await localModule('terminal-debugger.mjs', 'TerminalDebugger') +
  await localModule('repl-bridge.mjs', 'startReplBridge, replCompilerImage') +
  await localModule('compilation-statistics.mjs', 'collectCompilationStatistics, writeCompilationStatistics') +
  await localModule('miricl-loader.mjs', 'loadMiricl, getMiriclAdapter');
return `${core}
// The same file is also the Node terminal command; importing it has no CLI effects.
if (typeof process !== 'undefined' && process.versions?.node && process.argv[1]) {
  const {fileURLToPath} = await import('node:url');
  const {realpathSync} = await import('node:fs');
  if (realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)) {
    const net = (await import('node:net')).default;
    const fs = (await import('node:fs/promises')).default;
    const path = (await import('node:path')).default;
    const readline = (await import('node:readline')).default;
    ${terminalSupport}
    ${cli}
  }
}
`;
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const embedded = process.argv[2] === '--embedded';
  const output = path.resolve(process.argv[embedded ? 3 : 2] ?? (embedded ? 'miricl-adapter.gz' : 'miricl.js'));
  const text = await adapterSource({terminal: !embedded});
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, embedded ? gzipSync(text, {level: 9}) : text);
  console.log(`Wrote ${output}`);
}
