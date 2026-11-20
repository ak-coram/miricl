// Source-only Clysm bootstrap inputs. Read the upstream manifests as data;
// packaging never runs a Lisp compiler or imports executable manifest forms.
import fs from 'node:fs/promises';
import path from 'node:path';

export function stringList(source, label) {
  const text = source.replace(/;[^\n]*/gu, '').trim();
  if (!/^\(\s*(?:"[a-z0-9/-]+"\s*)+\)$/u.test(text))
    throw new Error(`Expected a nonempty list of source names in ${label}.`);
  const values = [...text.matchAll(/"([a-z0-9/-]+)"/gu)].map(match => match[1]);
  if (new Set(values).size !== values.length)
    throw new Error(`Duplicate source name in ${label}.`);
  return values;
}

export async function clysmFiles(root) {
  const upstream = path.join(root, 'clysm/upstream');
  const [loader, runtimeManifest] = await Promise.all([
    fs.readFile(path.join(upstream, 'load.lisp'), 'utf8'),
    fs.readFile(path.join(upstream, 'tools/runtime-files.lisp'), 'utf8'),
  ]);
  const definitions = [...loader.matchAll(/\(defparameter\s+\*files\*\s+'(\([^)]*\))\s*\)/giu)];
  if (definitions.length !== 1) throw new Error('Expected one Clysm compiler source manifest in load.lisp.');
  const compiler = stringList(definitions[0][1], 'load.lisp *files*');
  const runtime = stringList(runtimeManifest, 'tools/runtime-files.lisp');
  if (compiler.some(name => !/^src\/[a-z0-9-]+$/u.test(name))
      || runtime.some(name => !/^[a-z0-9-]+$/u.test(name)))
    throw new Error('Clysm source manifest contains an invalid path.');
  const names = [...compiler.map(name => `${name}.lisp`),
    ...runtime.map(name => `runtime/${name}.lisp`),
    'lib/live-image.lisp', 'lib/web-main.lisp', 'tools/runtime-files.lisp'];
  if (new Set(names).size !== names.length) throw new Error('Duplicate Clysm source path.');
  const files = await Promise.all(names.map(async name => [`/clysm/${name}`,
    await fs.readFile(path.join(upstream, name))]));
  files.push(['/clysm/compiler-files.lisp', Buffer.from(`(${compiler.map(name => `"${name}"`).join(' ')})\n`)]);
  files.push(['/clysm-bootstrap.lisp', await fs.readFile(path.join(root, 'clysm/bootstrap.lisp'))]);
  return {files, compiler, runtime};
}
