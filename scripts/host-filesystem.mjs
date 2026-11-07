// Terminal host I/O. Ordinary Lisp writes stay in the interpreter's overlay;
// MIRICL-BUNDLE explicitly publishes its requested output with operation 3.
import fs from 'node:fs/promises';
import path from 'node:path';
import {DevFilesystem} from './dev-filesystem.mjs';

export class HostFilesystem extends DevFilesystem {
  constructor(interpreter, directory = '.') {
    super(interpreter);
    this.root = path.resolve(directory);
  }
  resolve(name) {
    const filename = path.resolve(this.root, name.replace(/^\//, ''));
    if (filename !== this.root && !filename.startsWith(this.root + path.sep))
      throw new Error('File is outside the selected directory.');
    return filename;
  }
  async stat(name) {
    try {
      const info = await fs.stat(this.resolve(name));
      return info.isDirectory() || info.isFile()
        ? {kind: info.isDirectory() ? 2 : 5, date: Math.floor(info.mtimeMs / 1000) + 2208988800} : null;
    } catch (error) { if (['ENOENT', 'ENOTDIR'].includes(error.code)) return null; throw error; }
  }
  async list(name) {
    try {
      const entries = await fs.readdir(this.resolve(name), {withFileTypes: true});
      const result = [];
      for (const entry of entries) {
        const info = entry.isSymbolicLink() ? await this.stat(`${name}/${entry.name}`) :
          {kind: entry.isDirectory() ? 2 : entry.isFile() ? 5 : 0};
        if (info?.kind === 2 || info?.kind === 5) result.push([entry.name, info.kind === 2]);
      }
      return result;
    } catch (error) { if (['ENOENT', 'ENOTDIR'].includes(error.code)) return null; throw error; }
  }
  async operation(op, name, signal, cache) {
    if (op === 3) throw new Error('Publishing is handled at the synchronous service boundary.');
    if (op !== 1) return super.operation(op, name, signal, cache);
    const stat = await this.stat(name);
    return stat?.kind === 5 ? {bytes: await fs.readFile(this.resolve(name)), date: stat.date} : null;
  }
  // Publishing cannot reenter the evaluator to read a file while Lisp is active.
  service(op, name, value) {
    if (op !== 3) return super.service(op, name, value);
    const filename = this.js.text(name);
    this.published ??= new Set();
    this.published.add(filename);
    return [this.js.bool(true), 0];
  }
  async publish() {
    for (const name of this.published ?? []) {
      const filename = this.resolve(name);
      const bytes = await this.interpreter.readFile(name);
      await fs.mkdir(path.dirname(filename), {recursive: true});
      await fs.writeFile(filename, bytes);
    }
    this.published?.clear();
  }
}
