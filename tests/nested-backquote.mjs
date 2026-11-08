// Nested comma splicing must expand the inner template before its enclosure.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const bytes = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const vm = await Miricl.instantiate(bytes);
const fresh = await Miricl.instantiate(bytes);
const source = `
(defmacro once-simple (specs &body forms)
  (let ((gs (mapcar (lambda (x) (declare (ignore x)) (gensym)) specs)))
    \`(let ,(mapcar (lambda (g) (list g '(gensym))) gs)
       \`(let (,,@(mapcar (lambda (g n) \`\`(,,g ,,n)) gs specs))
          ,(let ,(mapcar #'list specs gs) ,@forms)))))
(defmacro cons-once (x) (once-simple (x) \`(cons ,x ,x)))
(defun nested-result () (let ((n 0)) (list (cons-once (incf n)) n)))
`;
const check = (machine, code, expected) => assert.equal(machine.evaluate(code, {eof: true}).output, expected + '\n');
try {
  vm.mountFile('/nested.lisp', source);
  check(vm, '(load "/nested.lisp") (nested-result)', 'T\n((1 . 1) 1)');
  check(vm, '(let ((x \'(10 20))) (eval (eval ``(list ,,@x))))', '(10 20)');
  check(vm, '(pathnamep (compile-file "/nested.lisp"))', 'T');
  fresh.mountFile('/nested.wasm', await vm.readFile('/nested.wasm'));
  check(fresh, '(load "/nested.wasm") (nested-result)', 'T\n((1 . 1) 1)');
  console.log('Nested backquote: comma splicing, macro generators, single evaluation, and portable FASLs pass.');
} finally { vm.close(); fresh.close(); }
