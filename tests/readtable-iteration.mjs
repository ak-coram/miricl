import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  check(`(let* ((table (copy-readtable nil))
                (characters (miricl:readtable-macro-characters table)))
    (and (= (length characters) 8)
      (every (lambda (c) (member c characters)) (mapcar #'code-char '(34 35 39 40 41 44 59 96)))
      (not (eq characters (miricl:readtable-macro-characters table)))
      (every (lambda (c) (get-dispatch-macro-character #\\# c table))
        (miricl:readtable-macro-characters table #\\#))))`);
  check(`(let ((table (copy-readtable nil)) (macro (lambda (s c) (declare (ignore s c)) :macro)))
    (set-macro-character #\\λ macro nil table)
    (make-dispatch-macro-character #\\☃ nil table)
    (set-dispatch-macro-character #\\☃ #\\λ macro table)
    (and (member #\\λ (miricl:readtable-macro-characters table))
      (member #\\☃ (miricl:readtable-macro-characters table))
      (equal (miricl:readtable-macro-characters table #\\☃) '(#\\Λ))
      (progn (set-syntax-from-char #\\λ #\\A table nil)
        (not (member #\\λ (miricl:readtable-macro-characters table))))
      (handler-case (progn (miricl:readtable-macro-characters table #\\A) nil)
        (error () t))))`);
  console.log('Readtable iteration: standard macros, sparse Unicode entries, dispatch case folding and cleared syntax pass.');
} finally { vm.close(); }
