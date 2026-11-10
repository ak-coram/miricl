import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
try {
  const result = await vm.evaluateAsync(`
    (let ((*readtable* (copy-readtable nil)) (*features* nil) (observed nil))
      (set-syntax-from-char #\\] #\\))
      (set-macro-character #\\[
        (lambda (stream char) (declare (ignore char))
          (push *read-suppress* observed)
          (read-delimited-list #\\] stream t)))
      (assert (= (read-from-string "#+absent [missing-package:foo] 42") 42))
      (assert (equal observed '(t)))
      (assert (null *read-suppress*))
      (setq observed nil)
      (assert (equal (read-from-string "[ok]") '(ok)))
      (assert (equal observed '(nil)))
      (setq observed nil)
      (let ((*read-suppress* :outer))
        (read-from-string "[ignored]")
        (assert (eq *read-suppress* :outer)))
      (assert (equal observed '(:outer)))
      (set-macro-character #\\! (lambda (stream char) (declare (ignore stream char))
        (assert *read-suppress*) (error "reader failure")))
      (assert (handler-case (read-from-string "#+absent !") (error () t)))
      (assert (null *read-suppress*)))
    (values)`, {eof: true});
  assert.equal(result.output, 'NIL\n');
  console.log('Reader suppression: custom macros observe feature suppression, recursive reads skip unknown packages, generalized booleans and error unwinding pass.');
} finally {vm.close();}
