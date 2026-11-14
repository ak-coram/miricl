// Exercise the guest cache-constant adaptation through actual source/FASL loads.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';

const image = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const source = portAcl2Source('axioms.lisp', fs.readFileSync('vendor/acl2/axioms.lisp'));
const start = source.indexOf('(defconstant *ev-fncall-w-guard1-cache-init*');
const end = source.indexOf('(defvar *ev-fncall-w-guard1-cache*', start);
const definition = `(defconstant *ev-fncall-w-guard1-cache-size* 10)
${source.slice(start, end)}`;
const ioSource = portAcl2Source('interface-raw.lisp', fs.readFileSync('vendor/acl2/interface-raw.lisp'));
const ioStart = ioSource.indexOf('(#+allegro\n\n; Builds with Allegro CL warn');
const ioDefinition = ioSource.slice(ioStart, ioSource.indexOf('(defun ld-acl2-customization', ioStart));
const check = (vm, form) => assert.equal(vm.evaluate(form, {eof: true}).output, 'T\n');
let vm = await Miricl.instantiate(image);
try {
  vm.mountFile('/cache.lisp', Buffer.from(definition));
  check(vm, `(progn (load "/cache.lisp")
    (defparameter *saved-cache* *ev-fncall-w-guard1-cache-init*)
    (setf (car *saved-cache*) 7)
    (multiple-value-bind (file warnings failure)
        (compile-file "/cache.lisp" :output-file "/cache.wasm")
      (declare (ignore file warnings))
      (and (not failure) (eq *saved-cache* *ev-fncall-w-guard1-cache-init*)
           (= (car *saved-cache*) 7))))`);
  check(vm, `(progn (load "/cache.wasm")
    (and (eq *saved-cache* *ev-fncall-w-guard1-cache-init*) (= (car *saved-cache*) 7)))`);
  const fasl = await vm.readFile('/cache.wasm');
  vm.close();
  vm = await Miricl.instantiate(image);
  vm.mountFile('/cache.wasm', fasl);
  check(vm, `(progn (load "/cache.wasm")
    (and (constantp '*ev-fncall-w-guard1-cache-init*)
         (equal *ev-fncall-w-guard1-cache-init* '(0 nil nil nil nil nil nil nil nil nil nil))))`);
  vm.mountFile('/io.lisp', Buffer.from(ioDefinition));
  check(vm, `(progn (load "/io.lisp") (defparameter *saved-io* *our-standard-io*)
    (multiple-value-bind (file warnings failure) (compile-file "/io.lisp" :output-file "/io.wasm")
      (declare (ignore file warnings)) (not failure)))`);
  check(vm, `(progn (load "/io.wasm")
    (and (eq *saved-io* *our-standard-io*) (input-stream-p *our-standard-io*) (output-stream-p *our-standard-io*)))`);
  const ioFasl = await vm.readFile('/io.wasm');
  vm.close(); vm = await Miricl.instantiate(image); vm.mountFile('/io.wasm', ioFasl);
  check(vm, `(progn (load "/io.wasm")
    (and (constantp '*our-standard-io*) (input-stream-p *our-standard-io*) (output-stream-p *our-standard-io*)))`);
  const printerStart = source.lastIndexOf('#+(and miricl (not acl2-loop-only))',
    source.indexOf('(defun sharp-atsign-alist ('));
  vm.mountFile('/printer.lisp', source.slice(printerStart,
    source.indexOf('\n; Essay on the Implementation of Time$', printerStart)));
  check(vm, `(progn
    (defun zp (n) (or (not (integerp n)) (<= n 0)))
    (defun make-sharp-atsign (n) (format nil "#@~D#" n))
    (load "/printer.lisp")
    (let* ((seed (list :sentinel)) (result (sharp-atsign-alist 10000 seed)))
      (and (eq (sharp-atsign-alist 0 seed) seed)
           (eq (nthcdr 10000 result) seed)
           (loop for entry in result for i from 1 to 10000
             always (equal entry (cons i (make-sharp-atsign i)))))))`);
  console.log('ACL2 cache and I/O constants: source, compilation, repeated load identity and fresh portable FASL loads pass.');
} finally {vm.close();}
