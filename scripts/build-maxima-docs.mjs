// Build the upstream manual without generating files in the submodule.
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';

const root = fileURLToPath(new URL('../', import.meta.url));
const source = path.join(root, 'vendor/maxima/doc/info');
const target = path.resolve(process.argv[2] ?? 'build/maxima/doc/info');
const version = fs.readFileSync(path.join(root, 'vendor/maxima/configure.ac'), 'utf8')
  .match(/AC_INIT\(\[maxima\], \[([^\]]+)\]/)[1];
fs.mkdirSync(target, {recursive: true});
fs.cpSync(source, target, {recursive: true});
for (const name of ['include-maxima.texi', 'category-macros.texi', 'math.m4']) {
  fs.writeFileSync(path.join(target, name), fs.readFileSync(path.join(source, name + '.in'), 'utf8')
    .replaceAll('@manual_version@', version).replaceAll('@abs_srcdir@', target)
    .replaceAll('@NO_DISPLAYMATH@', ''));
}
const run = (command, args) => execFileSync(command, args, {cwd: target, maxBuffer: 32 * 1024 * 1024});
for (const name of fs.readdirSync(target).filter(name => name.endsWith('.texi.m4')))
  fs.writeFileSync(path.join(target, name.slice(0, -3)), run('m4', ['--prefix-builtins', 'math.m4', name]));
run('makeinfo', ['-D', 'textmanual', '--split-size=1000000', 'maxima.texi']);
fs.writeFileSync(path.join(target, 'maxima-index.lisp'), run('perl', ['build_index.pl', 'maxima.info']));
// Use upstream's category and anchor generation as well as its HTML layout.
// An empty init file keeps the manual self-contained without CDN assets.
fs.writeFileSync(path.join(target, 'texi2html.init'), '1;\n');
const substitutions = {abs_srcdir: target, DEFAULTLISP: 'sbcl', SBCL_NAME: 'sbcl',
  AWK: 'awk', EGREP: 'grep -E', GREP: 'grep', SED: 'sed', MAKEINFO: 'makeinfo',
  TEXI2HTML: 'texi2html.init', NEED_MATHJAX: 'true'};
fs.writeFileSync(path.join(target, 'build_html.sh'), fs.readFileSync(path.join(source, 'build_html.sh.in'), 'utf8')
  .replace(/@([A-Za-z_0-9]+)@/g, (_, name) => substitutions[name] ?? ''));
run('sh', ['build_html.sh']);
// The upstream HTML indexer is a standalone Lisp utility. Its DEFMFUN entry
// point needs no Maxima evaluation or keyword processing during this build.
const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
fs.writeFileSync(path.join(target, 'build-browser-index.lisp'), `
  (load ${lispString(path.join(root, 'vendor/maxima/src/maxima-package.lisp'))})
  (load ${lispString(path.join(root, 'vendor/maxima/src/pregexp.lisp'))})
  (defmacro maxima::defmfun (name args &body body) (list* 'defun name args body))
  (load ${lispString(path.join(root, 'vendor/maxima/src/verify-html-index.lisp'))})
  (load "build-html-index.lisp")
  (maxima::$build_and_dump_html_index "*.html" :output_file "maxima-index-html.lisp")
`);
run('sbcl', ['--noinform', '--script', 'build-browser-index.lisp']);
console.log(`Built Maxima ${version} text and HTML manuals in ${target}`);
