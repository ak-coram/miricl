import fs from 'node:fs/promises';
import path from 'node:path';
import {portCoaltonSource, closerMopPort, setNamedReadtablesPort, setFloatFeaturesPort} from '../coalton/ports.mjs';
const root = path.resolve(import.meta.dirname, '..');
export const coaltonDependencies = JSON.parse(await fs.readFile(path.join(root, 'coalton/dependencies.json'), 'utf8'));

export async function coaltonFiles() {
  setNamedReadtablesPort(await fs.readFile(path.join(root, 'scripts/ports/named-readtables.lisp'), 'utf8'));
  setFloatFeaturesPort(await fs.readFile(path.join(root, 'coalton/float-features.lisp'), 'utf8'));
  const files = [];
  const visit = async (directory, guest) => {
    for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
      if (entry.name === '.git' || ['node_modules', 'target', 'test', 'tests', '.github'].includes(entry.name)) continue;
      const source = path.join(directory, entry.name), name = `${guest}/${entry.name}`;
      if (entry.isDirectory()) await visit(source, name);
      else if (entry.isFile() && /\.(?:lisp|asd|ct|txt|text|sexp)$/iu.test(name)) files.push([name, await fs.readFile(source)]);
    }
  };
  for (const dependency of coaltonDependencies)
    await visit(path.join(root, 'build/coalton/deps', dependency.name), `/quicklisp/${dependency.name}`);
  await visit(path.join(root, 'vendor/coalton'), '/coalton');
  return [...new Map([...files.map(([name, bytes]) => [name, portCoaltonSource(name, bytes)]),
    ['/quicklisp/float-features/float-features.asd', Buffer.from(`(asdf:defsystem "float-features"
      :serial t :depends-on ("trivial-features" "documentation-utils")
      :components ((:file "package") (:file "float-features") (:file "infinity") (:file "nan") (:file "documentation")))`)],
    // Miricl supplies the MOP directly. Both guest files are local shims;
    // none of the upstream implementation-specific Closer-MOP files are used.
    ['/quicklisp/closer-mop/closer-mop.asd', Buffer.from('(asdf:defsystem "closer-mop" :components ((:file "closer-miricl")))')],
    ['/quicklisp/closer-mop/closer-miricl.lisp', Buffer.from(closerMopPort)]])];
}
