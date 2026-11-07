// Link the built runtime and Quicklisp sources; never overwrite application files.
import fs from 'node:fs/promises';
import path from 'node:path';

const [directory, system, root, output, ...systems] = process.argv.slice(2);
async function link(target, name) {
  target = await fs.realpath(target);
  const destination = path.join(directory, name);
  await fs.mkdir(path.dirname(destination), {recursive: true});
  const old = await fs.lstat(destination).catch(error => {
    if (error.code !== 'ENOENT') throw error;
  });
  if (old && !old.isSymbolicLink()) throw new Error(`Refusing to replace ${destination}.`);
  if (old) await fs.unlink(destination);
  await fs.symlink(path.relative(path.dirname(destination), target), destination,
    (await fs.stat(target)).isDirectory() ? 'dir' : 'file');
}
await link(path.join(output, 'runtime'), 'runtime');
await link(output, 'images');
await link(path.join(output, 'miricl-asdf.wasm'), 'miricl-asdf.wasm');
await link(path.join(output, 'miricl-interpreter.wasm'), 'miricl-interpreter.wasm');
await link(path.join(output, 'miricl-repl.wasm'), 'miricl-repl.wasm');

const string = value => `"${value.replace(/["\\]/g, '\\$&')}"`;
const entries = [];
for (let i = 0; i < systems.length; i += 2) {
  const [name, source] = systems.slice(i, i + 2);
  if (!/^[a-z0-9-]+$/.test(name)) throw new Error(`Unexpected dependency: ${name}`);
  await link(source, `quicklisp/${name}`);
  entries.push(`(:directory ${string(`/quicklisp/${name}/`)})`);
}
await fs.writeFile(path.join(directory, 'systems.lisp'),
  `(asdf:initialize-source-registry '(:source-registry\n  ${entries.join('\n  ')}\n  :ignore-inherited-configuration))\n` +
  `(asdf:initialize-output-translations '(:output-translations (t "/build/cache/") :ignore-inherited-configuration))\n`);
if (systems.includes('named-readtables')) {
  await fs.appendFile(path.join(directory, 'systems.lisp'), '\n' +
    await fs.readFile(path.join(root, 'scripts/ports/named-readtables-system.lisp'), 'utf8'));
}
for (const [system, file] of [['fset', 'Code/port.lisp'], ['named-readtables', 'src/cruft.lisp']]) {
  if (!systems.includes(system)) continue;
  await fs.appendFile(path.join(directory, 'systems.lisp'), `
;; Compatibility ports modify the guest overlay, never the source checkout.
(let* ((name "/quicklisp/${system}/${file}")
       (source (uiop:read-file-string name)))
  (unless (search "; MIRICL ${system} portability" source)
    (with-open-file (out name :direction :output :if-exists :supersede)
      (write-string source out)
      (write-line "; MIRICL ${system} portability" out)
      (write-string ${string(await fs.readFile(path.join(root, `scripts/ports/${system}.lisp`), 'utf8'))} out))))
`);
}
console.log(`Prepared ${system} in ${directory}.`);
