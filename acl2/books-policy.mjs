// Explicit mathematical subject areas; shared dependencies are added separately.
export const maxBookBytes = 2_000_000;
export const mathDirectories = [
  'arithmetic', 'arithmetic-2', 'arithmetic-3', 'arithmetic-5', 'cowles',
  'finite-set-theory', 'ihs', 'ordinals', 'powerlists',
  'kestrel/algebra', 'kestrel/arithmetic-light', 'kestrel/number-theory',
  'kestrel/prime-fields', 'kestrel/sets',
  'projects/cholesky', 'projects/fields', 'projects/gaussian-elim-solvers',
  'projects/groups', 'projects/linear', 'projects/numbers',
  'projects/schroeder-bernstein', 'projects/set-theory',
];

// cert.pl emits flat lists of quoted paths in its dependency map. Parse only
// that data section, never evaluate the generated Lisp or Makefile.
export function parseBookDependencies(text) {
  const start = text.indexOf('( :DEPENDENCY-MAP');
  if (start < 0) throw new Error('cert.pl did not produce a dependency map.');
  const graph = new Map();
  for (const match of text.slice(start).matchAll(/\(\s*((?:"[^"\\]*"\s*)+)\)/gu)) {
    const names = [...match[1].matchAll(/"([^"\\]*)"/gu)].map(item => item[1]);
    graph.set(names[0], names.slice(1));
  }
  return graph;
}

export function selectBooks(candidates, graph, sizes, blocked = new Set()) {
  const included = new Set(), books = [], excluded = [];
  for (const source of candidates) {
    const pending = [source.replace(/\.lisp$/u, '.cert')], closure = new Set(), visited = new Set();
    let reason;
    while (pending.length && !reason) {
      const name = pending.pop();
      if (visited.has(name)) continue;
      visited.add(name);
      if (blocked.has(name)) {reason = `Unsupported host/tool dependency: ${name}`; break;}
      if (graph.has(name)) {pending.push(...graph.get(name)); continue;}
      // These record dependencies on ACL2 system definitions, not guest files.
      if (name.endsWith('.certdep')) continue;
      if (!sizes.has(name)) {reason = `Missing source dependency: ${name}`; break;}
      if (sizes.get(name) > maxBookBytes) {reason = `File exceeds 2 MB: ${name}`; break;}
      closure.add(name);
    }
    if (!reason && !closure.has(source)) reason = 'No source dependency information';
    if (reason) excluded.push({path: source, reason});
    else {books.push({path: source.slice(0, -5), bytes: sizes.get(source)}); for (const name of closure) included.add(name);}
  }
  return {books, excluded, files: [...included].sort()};
}
