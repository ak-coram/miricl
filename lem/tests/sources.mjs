import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {root, prepareLemSources, lemFiles, inspectCheckout, relativeName, guestName, replaceChecked} from '../files.mjs';

test('browser source preparation preserves real sources and rejects unsafe inputs', async () => {
  const live = await prepareLemSources();
  const asd = live.files.get('/lem/lem-browser.asd').toString();
  for (const system of ['core', 'lisp-syntax', 'buffer']) assert(asd.includes(`(defsystem "lem-browser/${system}"`));
  assert(asd.includes('               "quri"\n'));
  assert(!asd.includes('"dexador"'));
  const uriSources = ['quri/quri.asd', 'idna/idna.asd', 'cl-utilities/cl-utilities.asd',
                      'quri/data/effective_tld_names.dat'];
  for (const name of uriSources)
    assert.deepEqual(live.files.get(`/deps/${name}`), await fs.readFile(path.join(root, 'lem/deps', name)));
  assert.deepEqual(live.files.get('/lem/src/ext/link.lisp'), await fs.readFile(path.join(root, 'vendor/lem/src/ext/link.lisp')));
  assert(!live.files.has('/lem/lem.asd'));
  const frontend = live.files.get('/lem-browser-frontend/lem-browser-frontend.asd').toString();
  assert(frontend.includes('(defsystem "lem-browser-frontend"'));
  assert(frontend.includes(':depends-on ("lem-browser/core")'));
  for (const name of ['package', 'display', 'drawing', 'input'])
    assert.deepEqual(live.files.get(`/lem-browser-frontend/backend/${name}.lisp`),
      await fs.readFile(path.join(root, `lem/backend/${name}.lisp`)));
  assert.deepEqual(live.files.get('/lem-browser-frontend/startup.lisp'), await fs.readFile(path.join(root, 'lem/startup.lisp')));
  assert(live.files.get('/lem-browser-lisp-mode/lem-browser-lisp-mode.asd').toString().includes('(asdf:defsystem "lem-browser-lisp-mode"'));
  assert(live.files.has('/deps/miricl-ide-services/services.lisp'));
  for (const name of ['lisp-mode', 'repl', 'ext/eval'])
    assert(live.files.has(`/lem/extensions/lisp-mode/${name}.lisp`));
  const localMode = live.files.get('/lem/extensions/lisp-mode/lisp-mode.lisp').toString();
  assert(localMode.includes('(defun self-connect () (ensure-local-connection))'));
  assert(!localMode.includes('lem-process:'));
  assert(!localMode.includes('lem/common/socket:'));
  assert(live.files.has('/lem/src/buffer/internal/basic.lisp'));
  assert(live.files.has('/deps/closer-mop/packages-miricl.lisp'));
  const strSource = await fs.readFile(path.join(root, 'lem/deps/str/str.lisp'), 'utf8');
  assert.equal(live.files.get('/deps/str/str.lisp').toString(), strSource
    .replace('#\\Ideographic_space', '(code-char 12288)')
    .replace('#\\No-break_space', '(code-char 160)'));
  const encodingsSource = await fs.readFile(path.join(root, 'vendor/lem/src/buffer/encodings.lisp'), 'utf8');
  const canonicalEncodings = encodingsSource
    .replaceAll('#\\cr', '#\\Return')
    .replaceAll('#\\lf', '#\\Linefeed');
  assert.equal(live.files.get('/lem/src/buffer/encodings.lisp').toString(), canonicalEncodings);
  const pointSource = await fs.readFile(path.join(root, 'vendor/lem/src/buffer/internal/point.lisp'), 'utf8');
  const pointLineSlot = '   (line\n    :accessor point-line\n    :type line\n';
  const qualifiedPointLineSlot = '   (line\n    :accessor point-line\n    :type line:line\n';
  assert.equal(live.files.get('/lem/src/buffer/internal/point.lisp').toString(),
    pointSource.replace(pointLineSlot, qualifiedPointLineSlot));
  const bufferSource = await fs.readFile(path.join(root, 'vendor/lem/src/buffer/internal/buffer.lisp'), 'utf8');
  const bufferSyntaxSlot = '   (syntax-table\n    :initform (fundamental-syntax-table)\n    :initarg :syntax-table\n    :accessor buffer-syntax-table\n    :type syntax-table)';
  const qualifiedBufferSyntaxSlot = '   (syntax-table\n    :initform (fundamental-syntax-table)\n    :initarg :syntax-table\n    :accessor buffer-syntax-table\n    :type lem/buffer/syntax-table::syntax-table)';
  assert.equal(live.files.get('/lem/src/buffer/internal/buffer.lisp').toString(),
    bufferSource.replace(bufferSyntaxSlot, qualifiedBufferSyntaxSlot));
  const frameSource = await fs.readFile(path.join(root, 'vendor/lem/src/frame.lisp'), 'utf8');
  const oldUnmap = '(defun unmap-frame (display)\n  (let ((frame (gethash display *display-frame-map*)))\n    (remhash display frame)\n    frame))';
  const newUnmap = oldUnmap.replace('(remhash display frame)', '(remhash display *display-frame-map*)');
  assert.equal(live.files.get('/lem/src/frame.lisp').toString(), frameSource.replace(oldUnmap, newUnmap));
  const overlaySource = await fs.readFile(path.join(root, 'vendor/lem/src/overlay.lisp'), 'utf8');
  const oldInitializer = String.raw`(defmethod initialize-instance ((overlay overlay) &key &allow-other-keys)
  (let ((overlay (call-next-method)))
    (with-slots (start end attribute) overlay
      (when (point< end start) (rotatef start end))
      (setf attribute (ensure-attribute attribute t)))`;
  const newInitializer = String.raw`(defun normalize-overlay-attribute (value)
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
        (setf attribute (normalize-overlay-attribute attribute))))`;
  const overlayChanges = [[oldInitializer, newInitializer]];
  for (const [className, extraBefore, extraAfter] of [
    ['overlay', '', ''],
    ['line-endings-overlay', '\n                 :text text\n                 :offset offset',
                             '\n                                 :text text\n                                 :offset offset']
  ]) {
    overlayChanges.push([
      `  (make-instance '${className}\n                 :start (copy-point start start-point-kind)\n                 :end (copy-point end end-point-kind)\n                 :attribute attribute\n                 :buffer (point-buffer start)${extraBefore}\n                 :temporary temporary))`,
      `  (let (start-copy end-copy completed)\n    (unwind-protect\n         (progn\n           (setf start-copy (copy-point start start-point-kind))\n           (setf end-copy (copy-point end end-point-kind))\n           (prog1 (make-instance '${className}\n                                 :start start-copy\n                                 :end end-copy\n                                 :attribute attribute\n                                 :buffer (point-buffer start)${extraAfter}\n                                 :temporary temporary)\n             (setf completed t)))\n      (unless completed\n        (when start-copy (delete-point start-copy))\n        (when end-copy (delete-point end-copy))))))`
    ]);
  }
  assert.equal(live.files.get('/lem/src/overlay.lisp').toString(),
    overlayChanges.reduce((source, [before, after]) => source.replace(before, after), overlaySource));
  const versionQuery = '(asdf:component-version (asdf:find-system :lem))';
  const browserVersionQuery = '(asdf:component-version (asdf:find-system :lem-browser))';
  const modelineSource = await fs.readFile(path.join(root, 'vendor/lem/src/modeline.lisp'), 'utf8');
  assert.equal(live.files.get('/lem/src/modeline.lisp').toString(), modelineSource.replace(versionQuery, browserVersionQuery));
  const versionSource = await fs.readFile(path.join(root, 'vendor/lem/src/version.lisp'), 'utf8');
  const upstream = await inspectCheckout(path.join(root, 'vendor/lem'));
  const gitHelper = versionSource.slice(versionSource.indexOf('(defun get-git-hash ()'), versionSource.indexOf('(defvar *git-revision*'));
  assert.equal(live.files.get('/lem/src/version.lisp').toString(), versionSource
    .replace(versionQuery, browserVersionQuery)
    .replace(gitHelper, `(defun get-git-hash ()\n  "Return the pinned browser source revision without running Git."\n  "${upstream.revision}")\n\n`));
  assert(!live.files.has('/lem/src/common/socket.lisp'));
  assert.deepEqual(await lemFiles(), live.files);
  for (const value of ['../escape', '/absolute', 'a//b', 'a\\b', 'a/./b', 'a\0b', 'a/.git/config'])
    assert.throws(() => relativeName(value), /unsafe relative path/u);
  for (const value of ['relative', '/../escape']) assert.throws(() => guestName(value), /path/u);
  for (const value of ['none', 'anchor anchor']) assert.throws(() => replaceChecked(value, 'anchor', '', 'fixture'), /exactly once/u);

  const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-lem-sources-'));
  try {
    // Source-only copies; no Git repository creation or mutation in these tests.
    for (const checkout of ['vendor/lem', 'mcclim/deps/alexandria', 'vendor/asdf/ext/cl-ppcre', 'vendor/asdf/ext/closer-closer-mop', 'lem/deps', 'compatibility'])
      await fs.cp(path.join(root, checkout), path.join(workspace, checkout), {recursive: true,
        filter: filename => !filename.split(path.sep).includes('.git')});
    for (const owned of ['backend', 'frontend-system.asd', 'startup.lisp', 'lisp-mode-system.asd', 'lisp'])
      await fs.cp(path.join(root, 'lem', owned), path.join(workspace, 'lem', owned), {recursive: true});
    const valid = await inspectCheckout(path.join(root, 'vendor/lem'));
    const prepare = state => prepareLemSources({workspace, inspect: async () => ({...valid, ...state})});
    // Recursive CI checkouts initialize nested submodules, whose Git metadata
    // is never a mounted application source. Cover gitdir files and directories.
    const nestedCheckout = path.join(workspace, 'lem/deps/bordeaux-threads/docs/themes/ci-source-fixture');
    await fs.mkdir(nestedCheckout, {recursive: true});
    const themeSource = Buffer.from('/* nested checkout source */\n');
    await fs.writeFile(path.join(nestedCheckout, 'theme.css'), themeSource);
    for (const metadata of ['file', 'directory']) {
      const git = path.join(nestedCheckout, '.git');
      if (metadata === 'file') await fs.writeFile(git, 'gitdir: unused-metadata\n');
      else {
        await fs.mkdir(git);
        await fs.writeFile(path.join(git, 'HEAD'), 'unused-metadata\n');
      }
      const nested = await prepare();
      assert.deepEqual(nested.files.get('/deps/bordeaux-threads/docs/themes/ci-source-fixture/theme.css'), themeSource);
      assert([...nested.files.keys()].every(name => !name.split('/').includes('.git')));
      await fs.rm(git, {recursive: true});
    }
    await fs.rm(nestedCheckout, {recursive: true});
    for (const state of [{revision: '0'.repeat(40)}, {repository: 'https://example.invalid/other'}, {status: '?? unexpected.lisp'}])
      await assert.rejects(prepare(state), /wrong submodule revision|wrong submodule origin|dirty checkout/u);
    const target = path.join(workspace, 'vendor/lem/src/common/ring.lisp');
    const original = await fs.readFile(target);
    await fs.unlink(target);
    await fs.symlink(path.join(root, 'vendor/lem/src/common/ring.lisp'), target);
    await assert.rejects(prepare(), /symlink/u);
    await fs.unlink(target);
    await fs.writeFile(target, original);
    const asdPath = path.join(workspace, 'vendor/lem/lem.asd'), originalAsd = await fs.readFile(asdPath);
    await fs.writeFile(asdPath, originalAsd.toString().replace('(defsystem "lem/core"', '(defsystem "changed"'));
    await assert.rejects(prepare(), /core system boundary changed/u);
    await fs.writeFile(asdPath, originalAsd);
    const uriDependency = '               "dexador"\n';
    for (const malformed of [originalAsd.toString().replace(uriDependency, ''),
                             originalAsd.toString().replace(uriDependency, uriDependency.repeat(2))]) {
      await fs.writeFile(asdPath, malformed);
      await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
    }
    await fs.writeFile(asdPath, originalAsd);
    for (const name of uriSources) {
      const filename = path.join(workspace, 'lem/deps', name), original = await fs.readFile(filename);
      await fs.unlink(filename);
      await assert.rejects(prepare(), /missing required URI source/u);
      await fs.writeFile(filename, original);
    }
    const strPath = path.join(workspace, 'lem/deps/str/str.lisp');
    for (const anchor of ['#\\Ideographic_space', '#\\No-break_space']) {
      for (const malformed of [strSource.replace(anchor, '(code-char 0)'), `${strSource}\n${anchor}\n`]) {
        await fs.writeFile(strPath, malformed);
        await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
      }
      await fs.writeFile(strPath, strSource);
    }
    const encodingsPath = path.join(workspace, 'vendor/lem/src/buffer/encodings.lisp');
    for (const anchor of ['#\\cr', '#\\lf']) {
      for (const malformed of [encodingsSource.replace(anchor, '#\\Space'), `${encodingsSource}\n${anchor}\n`]) {
        await fs.writeFile(encodingsPath, malformed);
        await assert.rejects(prepare(), /overlay character token must occur exactly/u);
      }
      await fs.writeFile(encodingsPath, encodingsSource);
    }
    const prefixedNames = '\n;; Longer character names: #\\crystal #\\lf-extra\n';
    await fs.writeFile(encodingsPath, encodingsSource + prefixedNames);
    assert.equal((await prepare()).files.get('/lem/src/buffer/encodings.lisp').toString(), canonicalEncodings + prefixedNames);
    await fs.writeFile(encodingsPath, encodingsSource);
    const pointPath = path.join(workspace, 'vendor/lem/src/buffer/internal/point.lisp');
    for (const malformed of [pointSource.replace(pointLineSlot, qualifiedPointLineSlot),
                             `${pointSource}\n${pointLineSlot}`]) {
      await fs.writeFile(pointPath, malformed);
      await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
    }
    await fs.writeFile(pointPath, pointSource);
    const bufferPath = path.join(workspace, 'vendor/lem/src/buffer/internal/buffer.lisp');
    for (const malformed of [bufferSource.replace(bufferSyntaxSlot, qualifiedBufferSyntaxSlot),
                             `${bufferSource}\n${bufferSyntaxSlot}`]) {
      await fs.writeFile(bufferPath, malformed);
      await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
    }
    await fs.writeFile(bufferPath, bufferSource);
    const framePath = path.join(workspace, 'vendor/lem/src/frame.lisp');
    for (const malformed of [frameSource.replace(oldUnmap, ''), `${frameSource}\n${oldUnmap}\n`]) {
      await fs.writeFile(framePath, malformed);
      await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
    }
    await fs.writeFile(framePath, frameSource);
    const overlayPath = path.join(workspace, 'vendor/lem/src/overlay.lisp');
    for (const [anchor] of overlayChanges) {
      for (const malformed of [overlaySource.replace(anchor, ''), `${overlaySource}\n${anchor}\n`]) {
        await fs.writeFile(overlayPath, malformed);
        await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
      }
      await fs.writeFile(overlayPath, overlaySource);
    }
    for (const [name, source] of [['modeline', modelineSource], ['version', versionSource]]) {
      const metadataPath = path.join(workspace, `vendor/lem/src/${name}.lisp`);
      for (const malformed of [source.replace(versionQuery, 'nil'), `${source}\n${versionQuery}\n`]) {
        await fs.writeFile(metadataPath, malformed);
        await assert.rejects(prepare(), /overlay anchor must occur exactly once/u);
      }
      await fs.writeFile(metadataPath, source);
    }
    assert.deepEqual([...((await prepare()).files)], [...live.files]);
  } finally {
    await fs.rm(workspace, {recursive: true, force: true});
  }
});
