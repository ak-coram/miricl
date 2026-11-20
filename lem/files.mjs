import fs from 'node:fs/promises';
import path from 'node:path';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {patchLispMode} from './lisp-mode-sources.mjs';

export const root = path.resolve(import.meta.dirname, '..');
const run = promisify(execFile);
const MAX_FILES = 10000, MAX_BYTES = 64 * 1024 * 1024, MAX_FILE_BYTES = 16 * 1024 * 1024;
const bufferSystem = await fs.readFile(new URL('./buffer-system.asd', import.meta.url), 'utf8');
const fail = message => { throw new Error(`LEM sources: ${message}`); };
export function relativeName(name) {
  if (typeof name !== 'string' || !name || name.length > 1024 || /[\\\x00-\x1f\x7f]/u.test(name) ||
      name.split('/').some(part => !part || part === '.' || part === '..' || part === '.git') || path.posix.isAbsolute(name))
    fail(`unsafe relative path ${JSON.stringify(name)}`);
  return name;
}
export function guestName(name) {
  if (typeof name !== 'string' || !name.startsWith('/')) fail(`invalid guest path ${JSON.stringify(name)}`);
  relativeName(name.slice(1));
  return name;
}
async function checkedPath(workspace, name) {
  relativeName(name);
  let current = workspace;
  const parts = name.split('/');
  for (let i = 0; i < parts.length; i++) {
    current = path.join(current, parts[i]);
    const stat = await fs.lstat(current);
    if (stat.isSymbolicLink()) fail(`symlink is not allowed: ${name}`);
    if (i < parts.length - 1 && !stat.isDirectory()) fail(`non-directory ancestor: ${name}`);
  }
  return current;
}
// The index Gitlink and its owning .gitmodules are the source authority.
// Nested dependencies use their own superproject, such as ASDF for PPCRE.
export async function inspectCheckout(directory) {
  const git = async (cwd, args) => (await run('git', ['-C', cwd, ...args], {maxBuffer: 4 * 1024 * 1024})).stdout.trim();
  const parent = await git(path.dirname(directory), ['rev-parse', '--show-toplevel']);
  const relative = path.relative(parent, directory).split(path.sep).join('/');
  const [top, revision, repository, status, indexed, registrations] = await Promise.all([
    git(directory, ['rev-parse', '--show-toplevel']), git(directory, ['rev-parse', 'HEAD']),
    git(directory, ['config', '--get', 'remote.origin.url']),
    git(directory, ['status', '--porcelain', '--untracked-files=all']),
    git(parent, ['ls-files', '--stage', '--', relative]),
    git(parent, ['config', '-f', '.gitmodules', '--get-regexp', '^submodule\..*\.path$'])
  ]);
  if (path.resolve(top) !== directory) fail(`not a separate source checkout: ${directory}`);
  const link = indexed.match(/^160000 ([0-9a-f]{40}) 0\t([^\n]+)$/u);
  if (!link || link[2] !== relative) fail(`missing submodule Gitlink: ${relative}`);
  const registration = registrations.split('\n').map(line => line.match(/^(submodule\..*)\.path (.*)$/u))
    .find(match => match?.[2] === relative);
  if (!registration) fail(`missing submodule registration: ${relative}`);
  const registeredRepository = await git(parent, ['config', '-f', '.gitmodules', '--get', `${registration[1]}.url`]);
  return {revision, gitlink: link[1], repository, registeredRepository, status};
}
async function verifyCheckout(directory, inspect) {
  const state = await inspect(directory);
  if (!/^[0-9a-f]{40}$/u.test(state.gitlink) || state.revision !== state.gitlink) fail(`wrong submodule revision: ${directory}`);
  if (!state.repository || state.repository !== state.registeredRepository) fail(`wrong submodule origin: ${directory}`);
  if (state.status) fail(`dirty checkout: ${directory}: ${state.status}`);
  return state;
}
export function replaceChecked(text, before, after, context) {
  if (typeof before !== 'string' || !before || typeof after !== 'string' || text.split(before).length !== 2)
    fail(`overlay anchor must occur exactly once: ${context}`);
  return text.replace(before, after);
}
const coreEdits = [
  [
    "(defsystem \"lem/core\"",
    "(defsystem \"lem-browser/core\"\n  :version \"2.3.0\""
  ],
  [
    "               \"dexador\"\n",
    "               \"quri\"\n"
  ],
  [
    "               \"cl-mustache\"\n",
    ""
  ],
  [
    "               #+sbcl\n               sb-concurrency\n",
    ""
  ],
  [
    "               ;; Disabled for Nix build due to QL-DIST dependency\n               #-nix-build\n               \"lem-extension-manager\"\n",
    ""
  ],
  [
    "               #+sbcl\n               \"sb-sprof\")",
    ")"
  ],
  [
    "                             (:file \"socket\")\n",
    ""
  ],
  [
    "                             (:file \"deepl\")\n",
    ""
  ],
  [
    "                             (:file \"image-buffer\")",
    ""
  ],
  [
    "                             (:file \"extension-commands\" :if-feature :quicklisp)\n",
    ""
  ]
];
const versionEdits = [
  [
    "(asdf:component-version (asdf:find-system :lem))",
    "(asdf:component-version (asdf:find-system :lem-browser))"
  ],
  [
    "(defun get-git-hash ()\n  \"Return lem's git hash.\"\n  ;; Skip git operations during Nix build\n  #-nix-build\n  (let ((path (asdf:system-relative-pathname :lem \".git/\")))\n    (when (uiop:directory-exists-p path)\n      (let ((repo-root (uiop:pathname-directory-pathname path)))\n        (uiop:with-current-directory (repo-root)\n          (handler-case\n              (string-trim\n               (list #\\Newline #\\Space)\n               #+sbcl\n               (with-output-to-string (stream)\n                 (uiop:run-program \"git rev-parse --short HEAD\"\n                                   :output stream\n                                   :ignore-error-status nil))\n               #-sbcl\n               \"\")\n            (error () nil)))))))\n\n",
    "(defun get-git-hash ()\n  \"Return the pinned browser source revision without running Git.\"\n  \"@REVISION@\")\n\n"
  ]
];
function patchSources(files, revision) {
  const read = name => {
    const bytes = files.get(name);
    if (!bytes) fail(`missing overlay source: ${name}`);
    return bytes.toString('utf8');
  };
  const patch = (name, edits) => {
    let text = read(name);
    for (const [before, after] of edits) text = replaceChecked(text, before, after, name);
    files.set(name, Buffer.from(text));
  };
  patch('/deps/trivial-features/trivial-features.asd', [
    ['#-(or sbcl clisp allegro openmcl mcl mkcl lispworks ecl cmu scl cormanlisp abcl xcl mocl clasp mezzano genera dotcl cl-amiga bliss)',
     '#-(or miricl sbcl clisp allegro openmcl mcl mkcl lispworks ecl cmu scl cormanlisp abcl xcl mocl clasp mezzano genera dotcl cl-amiga bliss)'],
    ['(:file "tf-mezzano" :if-feature :mezzano)',
     '(:file "tf-mezzano" :if-feature :mezzano)\n     (:file "tf-miricl" :if-feature :miricl)']
  ]);
  patch('/deps/bordeaux-threads/bordeaux-threads.asd', [
    ['        mezzano\n        mkcl', '        mezzano\n        miricl-threads\n        mkcl'],
    [':trivial-features :trivial-garbage', ':trivial-features #-miricl :trivial-garbage'],
    ['(:file "bordeaux-threads")\n                         (:file "impl-abcl"',
     '(:file "bordeaux-threads")\n                         (:file "impl-miricl" :if-feature :miricl-threads)\n                         (:file "impl-abcl"'],
    ['(:file "timeout-interrupt")',
     '(:file "timeout-interrupt")\n                         (:file "impl-miricl" :if-feature :miricl-threads)'],
    ['(:file "atomics" :if-feature (:not :abcl))',
     '(:file "atomics" :if-feature (:and (:not :abcl) (:not :miricl-threads)))\n                         (:file "atomics-miricl" :if-feature :miricl-threads)']
  ]);
  patch('/deps/bordeaux-threads/apiv1/bordeaux-threads.lisp', [
    ['#-(or ccl sbcl)', '#-(or ccl sbcl miricl)']
  ]);
  patch('/deps/bordeaux-threads/apiv2/api-threads.lisp', [
    ['(trivial-garbage:make-weak-hash-table #-(or genera amilisp) :weakness #-(or genera amilisp) :key)',
     '#+miricl (js:make-weak-hash-table :weakness :key)\n  #-miricl (trivial-garbage:make-weak-hash-table #-(or genera amilisp) :weakness #-(or genera amilisp) :key)']
  ]);
  patch('/deps/bordeaux-threads/apiv2/api-semaphores.lisp',
    ['(defstruct (%semaphore', '(deftype semaphore', '(defun %signal-semaphore', '(defun %wait-on-semaphore'].map(form => [
      `#-(or abcl allegro ccl ecl lispworks mezzano sbcl)\n${form}`,
      `#-(or abcl allegro ccl ecl lispworks mezzano sbcl miricl)\n${form}`
    ]));
  patch('/deps/trivial-gray-streams/package.lisp', [
    ['#+sbcl :sb-gray', '#+miricl :miricl-gray\n               #+sbcl :sb-gray'],
    ['#-(or sbcl allegro cmu clisp openmcl lispworks ecl clasp mkcl abcl mocl genera mezzano dotcl clamiga) ...',
     '#-(or miricl sbcl allegro cmu clisp openmcl lispworks ecl clasp mkcl abcl mocl genera mezzano dotcl clamiga) ...']
  ]);
  patch('/deps/trivial-gray-streams/trivial-gray-streams.asd', [
    [':serial t\n  :components ((:file "package") (:file "streams"))',
     ':depends-on ("miricl-gray")\n  :serial t\n  :components ((:file "package") (:file "streams") (:file "miricl" :if-feature :miricl))']
  ]);
  patch('/deps/micros/micros.asd', [
    [':depends-on ()', ':depends-on ("miricl-gray" "miricl-ide-mop")'],
    [':pathname "backend"\n                :components', ':pathname "backend"\n                :serial t\n                :components'],
    ['                             (:file "gray")',
     '                             (:file "miricl" :if-feature :miricl)\n                             (:file "gray")']
  ]);
  patch('/deps/trivial-cltl2/trivial-cltl2.asd', [
    [':components ((:file "cltl2")', ':depends-on ("miricl-environment")\n  :components ((:file "cltl2")']
  ]);
  patch('/deps/trivial-cltl2/cltl2.lisp', [
    ['  #+allegro (:import-from #:excl #:compiler-let)',
     '  #+miricl (:import-from #:miricl-environment #:define-declaration #:declaration-information #:augment-environment)\n  #+miricl (:export #:define-declaration #:declaration-information #:augment-environment)\n  #+allegro (:import-from #:excl #:compiler-let)'],
    ['  (:export #:compiler-let', '  #-miricl (:export #:compiler-let']
  ]);
  patch('/deps/closer-mop/closer-mop.asd', [
    ['((:file "closer-mop-packages")',
     '((:file "closer-mop-packages" :if-feature (:not :miricl))\n   (:file "packages-miricl" :if-feature :miricl)']
  ]);
  patch('/deps/closer-mop/closer-mop-shared.lisp', [
    ['#+(or clisp scl mezzano)\n(progn\n  (declaim (inline classp))',
     '#+(or clisp scl mezzano miricl)\n(progn\n  (declaim (inline classp))'],
    ['#+(or abcl allegro clisp clozure ecl clasp lispworks mcl mezzano sbcl)\n  initargs',
     '#+(or abcl allegro clisp clozure ecl clasp lispworks mcl mezzano sbcl miricl)\n  initargs']
  ]);
  // MIRICL uses Unicode character codes without these optional reader aliases.
  patch('/deps/str/str.lisp', [
    ['#\\Ideographic_space', '(code-char 12288)'],
    ['#\\No-break_space', '(code-char 160)']
  ]);
  // LEM uses optional short character names; retain its read-time CHAR-CODE forms.
  const encodingsName = '/lem/src/buffer/encodings.lisp';
  let encodings = read(encodingsName);
  for (const [token, replacement, count] of [
    [/#\\cr(?=[\s()]|$)/gu, '#\\Return', 6],
    [/#\\lf(?=[\s()]|$)/gu, '#\\Linefeed', 10]
  ]) {
    if ((encodings.match(token)?.length ?? 0) !== count)
      fail(`overlay character token must occur exactly ${count} times: ${encodingsName} ${token}`);
    encodings = encodings.replace(token, replacement);
  }
  files.set(encodingsName, Buffer.from(encodings));
  // The package-local LINE nickname qualifies the class; it does not import it.
  patch('/lem/src/buffer/internal/point.lisp', [
    ['   (line\n    :accessor point-line\n    :type line\n',
     '   (line\n    :accessor point-line\n    :type line:line\n']
  ]);
  // The structure's proper type name is private to its defining package.
  patch('/lem/src/buffer/internal/buffer.lisp', [
    ['   (syntax-table\n    :initform (fundamental-syntax-table)\n    :initarg :syntax-table\n    :accessor buffer-syntax-table\n    :type syntax-table)',
     '   (syntax-table\n    :initform (fundamental-syntax-table)\n    :initarg :syntax-table\n    :accessor buffer-syntax-table\n    :type lem/buffer/syntax-table::syntax-table)']
  ]);
  // Resolve designators before checked slot initialization or assignment.
  // Public UNMAP-FRAME returns the old frame and removes only its display key.
  patch('/lem/src/frame.lisp', [
    ['(defun unmap-frame (display)\n  (let ((frame (gethash display *display-frame-map*)))\n    (remhash display frame)\n    frame))',
     '(defun unmap-frame (display)\n  (let ((frame (gethash display *display-frame-map*)))\n    (remhash display *display-frame-map*)\n    frame))']
  ]);
  // Resolve designators before checked slot initialization or assignment.
  patch('/lem/src/overlay.lisp', [
    [String.raw`(defmethod initialize-instance ((overlay overlay) &key &allow-other-keys)
  (let ((overlay (call-next-method)))
    (with-slots (start end attribute) overlay
      (when (point< end start) (rotatef start end))
      (setf attribute (ensure-attribute attribute t)))`,
     String.raw`(defun normalize-overlay-attribute (value)
  (let ((attribute (etypecase value
                     (attribute value)
                     (symbol (ensure-attribute value t)))))
    (check-type attribute (or null attribute))
    attribute))

;; Replace the generated primary after DEFCLASS, retaining generic dispatch on
;; the original new value. NIL is already a valid direct writer value.
(defmethod set-overlay-attribute (value (overlay overlay))
  (setf (slot-value overlay 'attribute)
        (if (null value) nil (normalize-overlay-attribute value))))

(defmethod initialize-instance ((overlay overlay) &rest initargs
                                &key (attribute nil attribute-supplied-p)
                                &allow-other-keys)
  (let ((overlay
          (if attribute-supplied-p
              (let ((normalized-initargs (copy-list initargs)))
                (setf (getf normalized-initargs :attribute)
                      (normalize-overlay-attribute attribute))
                (apply #'call-next-method overlay normalized-initargs))
              (call-next-method))))
    (with-slots (start end attribute) overlay
      (when (point< end start) (rotatef start end))
      ;; An absent initarg retains required-slot and subclass initform behavior.
      (unless attribute-supplied-p
        (setf attribute (normalize-overlay-attribute attribute))))`],
    [String.raw`  (make-instance 'overlay
                 :start (copy-point start start-point-kind)
                 :end (copy-point end end-point-kind)
                 :attribute attribute
                 :buffer (point-buffer start)
                 :temporary temporary))`,
     String.raw`  (let (start-copy end-copy completed)
    (unwind-protect
         (progn
           (setf start-copy (copy-point start start-point-kind))
           (setf end-copy (copy-point end end-point-kind))
           (prog1 (make-instance 'overlay
                                 :start start-copy
                                 :end end-copy
                                 :attribute attribute
                                 :buffer (point-buffer start)
                                 :temporary temporary)
             (setf completed t)))
      (unless completed
        (when start-copy (delete-point start-copy))
        (when end-copy (delete-point end-copy))))))`],
    [String.raw`  (make-instance 'line-endings-overlay
                 :start (copy-point start start-point-kind)
                 :end (copy-point end end-point-kind)
                 :attribute attribute
                 :buffer (point-buffer start)
                 :text text
                 :offset offset
                 :temporary temporary))`,
     String.raw`  (let (start-copy end-copy completed)
    (unwind-protect
         (progn
           (setf start-copy (copy-point start start-point-kind))
           (setf end-copy (copy-point end end-point-kind))
           (prog1 (make-instance 'line-endings-overlay
                                 :start start-copy
                                 :end end-copy
                                 :attribute attribute
                                 :buffer (point-buffer start)
                                 :text text
                                 :offset offset
                                 :temporary temporary)
             (setf completed t)))
      (unless completed
        (when start-copy (delete-point start-copy))
        (when end-copy (delete-point end-copy))))))`]
  ]);
  const original = read('/lem/lem.asd');
  const start = '(defsystem "lem/core"', end = '(defsystem "lem/extensions"';
  for (const anchor of [start, end]) if (original.split(anchor).length !== 2) fail('core system boundary changed');
  if (original.indexOf(start) >= original.indexOf(end)) fail('invalid core system boundaries');
  let core = original.slice(original.indexOf(start), original.indexOf(end));
  for (const [before, after] of coreEdits) core = replaceChecked(core, before, after, 'LEM core');
  let syntax = read('/lem/extensions/lisp-syntax/lem-lisp-syntax.asd');
  syntax = replaceChecked(syntax, '(defsystem "lem-lisp-syntax"', '(defsystem "lem-browser/lisp-syntax"\n  :pathname "extensions/lisp-syntax"', 'Lisp syntax');
  syntax = replaceChecked(syntax, '"lem/core"', '"lem-browser/core"', 'Lisp syntax dependency');
  let version = read('/lem/src/version.lisp');
  for (const [before, after] of versionEdits) version = replaceChecked(version, before, after.replace('@REVISION@', revision), 'LEM version');
  files.set('/lem/src/version.lisp', Buffer.from(version));
  patch('/lem/src/modeline.lisp', [
    ['(asdf:component-version (asdf:find-system :lem))',
     '(asdf:component-version (asdf:find-system :lem-browser))']
  ]);
  // Unused TCP probing and optional online/desktop extension facilities.
  for (const name of ['src/common/socket.lisp', 'src/ext/deepl.lisp', 'src/ext/image-buffer.lisp', 'src/ext/extension-commands.lisp',
                      'lem.asd', 'extensions/lisp-syntax/lem-lisp-syntax.asd'])
    if (!files.delete(`/lem/${name}`)) fail(`missing excluded source: ${name}`);
  files.set('/lem/lem-browser.asd', Buffer.from(`;;; Browser graph from checked upstream patches and buffer-system.asd.\n(in-package #:asdf-user)\n\n(defsystem "lem-browser"\n  :description "LEM browser editor core"\n  :version "2.3.0"\n  :depends-on ("lem-browser/core"))\n\n${core}\n${syntax}\n${bufferSystem}`));
}
/** Offline source mapping. ASDF owns dependency resolution. */
export async function prepareLemSources({workspace = root, inspect = inspectCheckout} = {}) {
  workspace = await fs.realpath(workspace);
  const files = new Map(), checkouts = [];
  let total = 0;
  async function source(checkout, mount, trees = null, selected = []) {
    guestName(mount);
    const directory = await checkedPath(workspace, checkout);
    const state = await verifyCheckout(directory, inspect);
    checkouts.push({checkout, gitlink: state.gitlink, repository: state.registeredRepository});
    async function add(name, depth = 0) {
      if (depth > 32 || files.size >= MAX_FILES) fail('source traversal limit');
      const filename = await checkedPath(directory, name), stat = await fs.lstat(filename);
      if (stat.isDirectory()) {
        // Recursive checkouts also have Git metadata below the checkout root.
        for (const entry of await fs.readdir(filename))
          if (entry !== '.git') await add(`${name}/${entry}`, depth + 1);
      } else {
        if (!stat.isFile() || stat.size > MAX_FILE_BYTES || (total += stat.size) > MAX_BYTES) fail(`source size/type limit: ${name}`);
        const guest = guestName(`${mount}/${name}`);
        if (files.has(guest)) fail(`duplicate guest path: ${guest}`);
        files.set(guest, await fs.readFile(filename));
      }
    }
    const names = trees ? [...trees, ...selected] : (await fs.readdir(directory)).filter(name => name !== '.git');
    for (const name of names) await add(name);
    return state;
  }
  const modeSources = ['errors', 'ui-mode', 'connections', 'message-dispatcher', 'grammar',
    'internal-package', 'message', 'file-conversion', 'ext/detective', 'ext/completion',
    'lisp-mode', 'message-definitions', 'repl', 'ext/inspector', 'ext/eval', 'ext/sldb',
    'ext/paren-coloring', 'ext/macroexpand', 'package'];
  const upstream = await source('vendor/lem', '/lem', ['src', 'resources', 'extensions/lisp-syntax'],
    ['lem.asd', 'LICENCE', ...modeSources.map(name => `extensions/lisp-mode/${name}.lisp`)]);
  await source('mcclim/deps/alexandria', '/deps/alexandria', ['alexandria-1', 'alexandria-2'], ['alexandria.asd', 'LICENCE']);
  await source('vendor/asdf/ext/cl-ppcre', '/deps/cl-ppcre', ['cl-ppcre-unicode'],
    ['cl-ppcre.asd', 'cl-ppcre-unicode.asd', 'packages.lisp', 'specials.lisp', 'util.lisp', 'errors.lisp', 'charset.lisp',
     'charmap.lisp', 'chartest.lisp', 'lexer.lisp', 'parser.lisp', 'regex-class.lisp', 'regex-class-util.lisp', 'convert.lisp',
     'optimize.lisp', 'closures.lisp', 'repetition-closures.lisp', 'scanner.lisp', 'api.lisp', 'docs/index.html']);
  await source('lem/deps/micros', '/deps/micros', ['backend', 'contrib'],
    ['micros.asd', 'packages.lisp', 'micros.lisp', 'lsp-api.lisp', 'lsp-api-load-systems.lisp', 'LICENSE']);
  await source('vendor/asdf/ext/closer-closer-mop', '/deps/closer-mop');
  for (const name of await fs.readdir(await checkedPath(workspace, 'lem/deps'))) {
    if (name !== 'micros') await source(`lem/deps/${relativeName(name)}`, `/deps/${name}`);
  }
  // QURI reads its public-suffix data while compiling the real ETLD component.
  for (const name of ['/deps/quri/quri.asd', '/deps/idna/idna.asd', '/deps/cl-utilities/cl-utilities.asd',
                      '/deps/quri/data/effective_tld_names.dat'])
    if (!files.has(name)) fail(`missing required URI source: ${name}`);
  // Reusable implementation ports live outside the editor and upstream trees.
  for (const [owned, guest] of [
    ['trivial-features/tf-miricl.lisp', '/deps/trivial-features/src/tf-miricl.lisp'],
    ['bordeaux-threads/apiv1.lisp', '/deps/bordeaux-threads/apiv1/impl-miricl.lisp'],
    ['bordeaux-threads/apiv2.lisp', '/deps/bordeaux-threads/apiv2/impl-miricl.lisp'],
    ['bordeaux-threads/atomics.lisp', '/deps/bordeaux-threads/apiv2/atomics-miricl.lisp'],
    ['closer-mop/packages.lisp', '/deps/closer-mop/packages-miricl.lisp'],
    ['gray-streams/miricl-gray.asd', '/deps/miricl-gray/miricl-gray.asd'],
    ['gray-streams/miricl-gray.lisp', '/deps/miricl-gray/miricl-gray.lisp'],
    ['gray-streams/trivial-gray-streams.lisp', '/deps/trivial-gray-streams/miricl.lisp'],
    ['ide-backends/micros.lisp', '/deps/micros/backend/miricl.lisp'],
    ['ide-backends/miricl-ide-mop.asd', '/deps/miricl-ide-mop/miricl-ide-mop.asd'],
    ['ide-backends/mop.lisp', '/deps/miricl-ide-mop/mop.lisp'],
    ['environments/miricl-environment.asd', '/deps/miricl-environment/miricl-environment.asd'],
    ['environments/environments.lisp', '/deps/miricl-environment/environments.lisp'],
    ['ide-services/miricl-ide-services.asd', '/deps/miricl-ide-services/miricl-ide-services.asd'],
    ['ide-services/services.lisp', '/deps/miricl-ide-services/services.lisp']
  ]) {
    const filename = await checkedPath(workspace, `compatibility/${owned}`), stat = await fs.lstat(filename);
    if (!stat.isFile() || stat.size > MAX_FILE_BYTES || (total += stat.size) > MAX_BYTES || files.size >= MAX_FILES)
      fail(`adapter size/type limit: ${owned}`);
    if (files.has(guest)) fail(`duplicate guest path: ${guest}`);
    files.set(guest, await fs.readFile(filename));
  }
  // Independent primary frontend system: a saved development core can load
  // these definitions without redefining or recompiling its retained graph.
  for (const [owned, guest] of [
    ['frontend-system.asd', 'lem-browser-frontend.asd'],
    ...['package', 'display', 'drawing', 'input'].map(name => [`backend/${name}.lisp`, `backend/${name}.lisp`]),
    ['startup.lisp', 'startup.lisp']
  ]) {
    const filename = await checkedPath(workspace, `lem/${owned}`), stat = await fs.lstat(filename);
    if (!stat.isFile() || stat.size > MAX_FILE_BYTES || (total += stat.size) > MAX_BYTES || files.size >= MAX_FILES)
      fail(`frontend source size/type limit: ${owned}`);
    const name = guestName(`/lem-browser-frontend/${guest}`);
    if (files.has(name)) fail(`duplicate guest path: ${name}`);
    files.set(name, await fs.readFile(filename));
  }
  for (const [owned, guest] of [
    ['lisp-mode-system.asd', 'lem-browser-lisp-mode.asd'],
    ...['connection', 'implementation', 'local-ui'].map(name => [`lisp/${name}.lisp`, `${name}.lisp`])
  ]) {
    const filename = await checkedPath(workspace, `lem/${owned}`), stat = await fs.lstat(filename);
    if (!stat.isFile() || stat.size > MAX_FILE_BYTES || (total += stat.size) > MAX_BYTES || files.size >= MAX_FILES)
      fail(`local Lisp source size/type limit: ${owned}`);
    files.set(guestName(`/lem-browser-lisp-mode/${guest}`), await fs.readFile(filename));
  }
  patchSources(files, upstream.revision);
  patchLispMode(files, replaceChecked);
  return {files: new Map([...files].sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0)), checkouts, blockers: []};
}
/** Checked application sources; ASDF owns the selected dependency graph. */
export async function lemFiles(options) {
  return (await prepareLemSources(options)).files;
}
