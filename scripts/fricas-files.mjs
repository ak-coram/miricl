// Canonical source/database layout shared by bootstrap, Node tests and packaging.
import fs from 'node:fs';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
const root = path.resolve(import.meta.dirname, '..');
export function fricasRevision() {
  return execFileSync('git', ['-C', path.join(root, 'vendor/fricas'), 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
}
export function fricasFiles(native) {
  const files = new Map();
  const mount = (guest, host) => files.set(guest, fs.readFileSync(host));
  for (const [dir, guest] of [[path.join(root, 'vendor/fricas/src/lisp'), '/fricas/lisp'],
    [path.join(root, 'vendor/fricas/src/interp'), '/fricas/interp'], [path.join(native, 'src/interp'), '/fricas/interp'],
    [path.join(root, 'vendor/fricas/src/boot'), '/fricas/boot'], [path.join(native, 'src/boot'), '/fricas/boot']]) {
    for (const name of fs.readdirSync(dir)) if (/\.(lisp|clisp)$/.test(name)) mount(guest + '/' + name, path.join(dir, name));
  }
  const platforms = fs.readdirSync(path.join(native, 'target')).filter(name => fs.existsSync(path.join(native, 'target', name, 'algebra/interp.daase')));
  if (platforms.length !== 1) throw new Error('Expected one complete native FriCAS bootstrap.');
  const target = path.join(native, 'target', platforms[0]);
  for (const name of ['interp', 'category', 'operation', 'browse'])
    mount(`/fricas/algebra/${name}.daase`, path.join(target, 'algebra', name + '.daase'));
  // Upstream LOADs this extensionless path; MIRICL accepts exact guest paths.
  mount('/fricas/algebra/exposed', path.join(root, 'vendor/fricas/src/algebra/exposed.lsp'));
  mount('/fricas/algebra/exposed.lisp', path.join(root, 'vendor/fricas/src/algebra/exposed.lsp'));
  for (const dir of fs.readdirSync(path.join(native, 'src/algebra'))) {
    if (!dir.endsWith('.NRLIB')) continue;
    const name = dir.slice(0, -6) + '.lsp', file = path.join(native, 'src/algebra', dir, name);
    if (fs.existsSync(file)) mount('/fricas/algebra/' + name, file);
  }
  const help = path.join(target, 'share/spadhelp');
  for (const name of fs.readdirSync(help)) mount('/fricas/share/spadhelp/' + name, path.join(help, name));
  mount('/fricas-port.lisp', path.join(root, 'scripts/ports/fricas.lisp'));
  const boot = fs.readFileSync(path.join(native, 'src/boot/lobj_lst'), 'utf8');
  const source = (dir, name) => `/fricas/${dir}/${name}${files.has(`/fricas/${dir}/${name}.clisp`) ? '.clisp' : '.lisp'}`;
  const order = ['/fricas/lisp/fricas-package.lisp', '/fricas-port.lisp', '/fricas/lisp/fricas-lisp.lisp',
    '/fricas-host.lisp', '/fricas/lisp/primitives.lisp', '/fricas/interp/util.lisp', '/fricas/algebra/exposed.lisp',
    ...[...boot.matchAll(/"\.\.\/boot\/([\w-]+)\.fasl"/g)].map(([, name]) => source('boot', name)),
    ...[...fs.readFileSync(path.join(native, 'src/interp/makeint.lisp'), 'utf8').matchAll(/"([\w-]+)\.fasl"/g)]
      .map(([, name]) => source('interp', name))];
  files.set('/fricas-host.lisp', Buffer.from('(in-package "CL-USER")\n(fricas-lisp::|miricl-host-services|)\n(setq boot::|$lisp_bin_filetype| "wasm")\n'));
  for (const name of order) if (!files.has(name)) throw new Error(`Missing generated FriCAS source: ${name}`);
  return {files: [...files], order};
}
