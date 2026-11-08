// Assemble the distributed adapter from our own modules. This does not embed
// the interpreter, ASDF, application files or any other assets in JavaScript.
import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
const source = path.dirname(fileURLToPath(import.meta.url));
const output = path.resolve(process.argv[2] ?? 'miricl.js');
const read = name => fs.readFile(path.join(source, name), 'utf8');
const localModule = async (name, exported, imports = '') => {
  const text = (await read(name)).replace(/^import .*;\n/gm, '').replace(/^export /gm, '');
  return `const {${exported}} = (() => {\n${imports}\n${text}\nreturn {${exported}};\n})();\n`;
};
let adapter = await read('miricl.mjs');
adapter = adapter.replace("const {DevRepl} = await import('./dev-repl.mjs');", '')
  .replace("const {DevFilesystem} = await import('./dev-filesystem.mjs');", '');
const browser = await localModule('dev-repl.mjs', 'DevRepl') + await localModule('dev-filesystem.mjs', 'DevFilesystem');
let cli = (await read('repl.mjs')).replace(/^#!.*\n/, '').replace(/^import .*;\n/gm, '')
  .replace("const {startReplBridge, replCompilerImage} = await import('./repl-bridge.mjs');", '')
  .replace("const {replCompilerImage} = await import('./repl-bridge.mjs');", '')
  .replace("const {HostFilesystem} = await import('./host-filesystem.mjs');", '')
  .replace('node scripts/repl.mjs', 'node miricl.js');
const terminal = await localModule('host-filesystem.mjs', 'HostFilesystem') + await localModule('repl-bridge.mjs', 'startReplBridge, replCompilerImage');
const text = `${browser}\n${adapter}\n
// The same file is also the Node terminal command; importing it has no CLI effects.
if (typeof process !== 'undefined' && process.versions?.node && process.argv[1]) {
  const {fileURLToPath} = await import('node:url');
  const {realpathSync} = await import('node:fs');
  if (realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)) {
    const net = (await import('node:net')).default;
    const fs = (await import('node:fs/promises')).default;
    const path = (await import('node:path')).default;
    const readline = (await import('node:readline')).default;
    ${terminal}
    ${cli}
  }
}
`;
await fs.mkdir(path.dirname(output), {recursive: true});
await fs.writeFile(output, text);
console.log(`Wrote ${output}`);
