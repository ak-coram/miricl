// Test the session transport with real MIRICL and a small LD protocol fixture.
// This fixture does not implement ACL2 or validate theorem proving.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {createAcl2} from '../scripts/acl2.mjs';

const image = fs.readFileSync(process.argv[2] ?? 'miricl-precompiled.wasm');
const init = Buffer.from(`(defpackage :acl2 (:use :cl)) (in-package :acl2)
  (defvar *acl2-readtable* (copy-readtable nil))
  (defvar *the-live-state* :fixture-state)
  (defvar *acl2-status-file* "/acl2/acl2-status.txt")
  (defvar *initializations* 0)
  (defvar *miricl-environment* nil)
  (defun set-initial-cbd () nil)
  (defun proclaim-optimize () nil)`);
const source = Buffer.from(`(in-package :acl2)
  (defun initialize-acl2 ()
    (unless (= (incf *initializations*) 1) (error "ACL2 world was initialized twice."))
    (with-open-file (s *acl2-status-file* :direction :output :if-exists :supersede)
      (write :initialized :stream s)) t)
  (defun fixture-ld (input &key (ld-skip-proofsp :unset) &allow-other-keys)
    (unless (null ld-skip-proofsp) (error "Proof checking was not explicitly enabled."))
    (when (listp input) (return-from fixture-ld (values nil :eof *the-live-state*)))
    (with-open-file (s input)
      (let ((form (read s)))
        (case form
          (:reject (format t "Input rejected.~%") (values nil :error *the-live-state*))
          (:erp (values t nil *the-live-state*))
          (:stop (values nil '(:stop-ld 0) *the-live-state*))
          (otherwise (format t "~S~%" (eval form)) (values nil :eof *the-live-state*))))))
  (defmacro ld (input &rest options) (cons 'fixture-ld (cons input options)))`);
let vm = await Miricl.instantiate(image), session;
try {
  vm.mountFile('/acl2/init.lisp', init); vm.mountFile('/core.lisp', source);
  vm.evaluate('(load "/acl2/init.lisp") (compile-file "/core.lisp" :output-file "/core.wasm")', {eof: true});
  const core = await vm.readFile('/core.wasm');
  vm.close();
  const progress = [];
  session = await createAcl2(image, core, [['/acl2/init.lisp', init]], {
    booksURL: new URL('https://example.invalid/books/filesystem.bin.gz'),
    progress: message => progress.push(message)});
  assert.equal(progress.at(-1), 'Ready');
  assert.match((await session.evaluate('(cdr (assoc "ACL2_SYSTEM_BOOKS" acl2::*miricl-environment* :test #\'equal))')).output, /\/acl2\/books\//);
  assert.match((await session.evaluate('(+ 20 22)')).output, /42/);
  if (session.vm.vm['core-object-type']) {
    const restored = await createAcl2(session.vm.saveImage(), null, []);
    try {
      assert.match((await restored.evaluate('(+ 20 22)')).output, /42/);
      assert.match((await restored.evaluate('acl2::*initializations*')).output, /\b1\b/);
    } finally {restored.close();}
  }
  assert.match((await session.evaluate(String.raw`"λ🙂 \"quoted\" \\ end"`)).output, /λ🙂/);
  for (const input of [':reject', ':erp', ':stop']) assert.equal((await session.evaluate(input)).failed, true, input);
  assert.equal((await session.evaluate('(+ 1 2)')).failed, false);
  const running = session.evaluate('(sleep 0.02)');
  await assert.rejects(session.evaluate('(+ 1 2)'), /already evaluating/); await running;
  await assert.rejects(session.evaluate('(car 1)'));
  assert.equal((await session.evaluate('(+ 1 2)')).failed, false);
  await assert.rejects(session.evaluate(`(labels ((deep (n)
    (if (zerop n) 0 (+ 1 (deep (1- n)))))) (deep 100000))`), error => error.fatal === true);
  await assert.rejects(session.evaluate('(+ 1 2)'), /closed/);
  session.close(); session = null;
  const broken = Buffer.from(`(defpackage :acl2 (:use :cl)) (in-package :acl2)
    (defun proclaim-optimize () (error "Fixture startup failure"))`);
  await assert.rejects(createAcl2(image, core, [['/acl2/init.lisp', broken]]), /Fixture startup failure/);
  console.log('ACL2 session transport: verified startup, input streams, Unicode, LD rejection, proof-check flag, busy/closed sessions and fatal host traps pass (protocol fixture).');
} finally {session?.close(); vm.close();}
