import {DevFilesystem} from './dev-filesystem.mjs';
import {decodeFilesystem} from './filesystem-archive.mjs';

// Download the separate source artifact only when Lisp first accesses a book.
// Local writes (certificates and compiled Wasm) use the runtime's overlay.
export class Acl2Books extends DevFilesystem {
  constructor(vm, url) {
    super(vm, new URL('.', url).href);
    this.archiveURL = new URL(url);
    this.date = Math.floor(Date.now() / 1000) + 2208988800;
  }

  async load() {
    if (!this.loading) this.loading = (async () => {
      const response = await fetch(this.archiveURL);
      if (!response.ok) throw new Error(`Unable to load math books: HTTP ${response.status}.`);
      const source = await new Response(response.body.pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
      const files = new Map(), directories = new Set(['acl2/books']);
      for (const [absolute, bytes] of decodeFilesystem(source)) {
        if (!absolute.startsWith('/acl2/books/') || /(?:^|\/)\.{1,2}(?:\/|$)/u.test(absolute))
          throw new Error('Invalid book archive path.');
        const name = absolute.slice(1);
        files.set(name, bytes);
        for (let end = name.lastIndexOf('/'); end > 0; end = name.lastIndexOf('/', end - 1))
          directories.add(name.slice(0, end));
      }
      this.files = files; this.directories = directories;
    })().catch(error => {this.loading = null; throw error;});
    await this.loading;
  }

  async stat(name) {
    name = name.replace(/\/$/u, '');
    if (name === 'acl2/books') return {kind: 2, date: this.date};
    if (!name.startsWith('acl2/books/')) return null;
    await this.load();
    return this.files.has(name) ? {kind: 5, date: this.date}
      : this.directories.has(name) ? {kind: 2, date: this.date} : null;
  }

  async list(name) {
    name = name.replace(/\/$/u, '');
    if (name !== 'acl2/books' && !name.startsWith('acl2/books/')) return null;
    await this.load();
    if (!this.directories.has(name)) return null;
    const entries = new Map(), prefix = name + '/';
    for (const [names, directory] of [[this.files.keys(), false], [this.directories, true]])
      for (const child of names) if (child.startsWith(prefix) && !child.slice(prefix.length).includes('/'))
        entries.set(child.slice(prefix.length), directory);
    return [...entries];
  }

  async operation(op, name, signal, cache) {
    if (op !== 1) return super.operation(op, name, signal, cache);
    if (!(await this.stat(name)) || !this.files?.has(name)) return null;
    return {bytes: this.files.get(name), date: this.date};
  }
}
