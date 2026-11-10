// Integer representation must not limit object identities or FORMAT comparisons.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const values = [-(1n << 130n), -(1n << 63n), -2147483648n, -(1n << 30n), -1n,
  0n, 1n, (1n << 30n) - 1n, 1n << 30n, (1n << 63n) - 1n, 1n << 63n, 1n << 130n];
const pairs = values.flatMap(a => values.map(b => `(${a} ${b} ${a === b ? 't' : 'nil'})`));
const triples = values.flatMap((a, i) => [
  [a, a, a], [a, values[(i + 1) % values.length], values[(i + 2) % values.length]],
  [-a, a, 0n],
]).map(([a, b, c]) => `(${a} ${b} ${c} ${a <= b && b <= c ? 't' : 'nil'})`);
const body = `(let ((*print-readably* nil))
  (dolist (object (list nil t #\\A "text" '(a b) #(1 2) (make-hash-table) #'car
                         ${values.join(' ')}))
    (let* ((count 0)
           (first (with-output-to-string (s)
                    (assert (equal '(nil) (multiple-value-list
                      (print-unreadable-object (object s :identity t)
                        (incf count) (values 1 2 3)))))))
           (second (with-output-to-string (s)
                     (print-unreadable-object (object s :identity t)))))
      (assert (= count 1))
      (assert (string= first second))
      (assert (string= first "#< " :end1 3))
      (assert (char= (char first (1- (length first))) #\\>))
      (assert (handler-case (let ((*print-readably* t))
                (with-output-to-string (s) (print-unreadable-object (object s :identity t))) nil)
                (print-not-readable () t)))))
  (dolist (case '(${pairs.join(' ')}))
    (let* ((arguments (list (first case) (second case) 'unused))
           (expected (if (third case) "" "ok"))
           (tail nil))
      (assert (string= (apply #'format nil "~v,v^ok" arguments) expected))
      (assert (string= (with-output-to-string (s)
                        (setq tail (apply (formatter "~v,v^ok") s arguments))) expected))
      (assert (eq tail (cddr arguments)))))
  (dolist (case '(${triples.join(' ')}))
    (let* ((arguments (list (first case) (second case) (third case) 'unused))
           (expected (if (fourth case) "" "ok"))
           (tail nil))
      (assert (string= (apply #'format nil "~v,v,v^ok" arguments) expected))
      (assert (string= (with-output-to-string (s)
                        (setq tail (apply (formatter "~v,v,v^ok") s arguments))) expected))
      (assert (eq tail (cdddr arguments)))))
  (dolist (value '(${values.join(' ')}))
    (assert (string= (format nil "~v^ok" value) (if (zerop value) "" "ok"))))
  (assert (string= (format nil "~-2147483648^ok") "ok"))
  (assert (string= (format nil "~18446744073709551616,18446744073709551616^ok") ""))
  (assert (string= (format nil "~v,v^ok" #\\A #\\A) ""))
  (assert (string= (format nil "~v^ok" nil) ""))
  t)`;
try {
  for (const native of [false, true]) {
    const source = native ? `(funcall (compile nil '(lambda () ${body})))` : body;
    assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n');
    console.log(`PASS ${native ? 'native' : 'interpreted'} identities and arbitrary integer FORMAT/FORMATTER comparisons`);
  }
} finally {vm.close();}
