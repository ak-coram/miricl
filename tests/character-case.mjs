import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(loop for code in '(453 456 459 498) for c = (code-char code) always
  (and (eql c (char-upcase c)) (eql c (char-downcase c))
    (not (upper-case-p c)) (not (lower-case-p c)) (not (both-case-p c))
    (= (char-code (miricl:unicode-char-upcase c)) (1- code))
    (= (char-code (miricl:unicode-char-downcase c)) (1+ code))
    (char-equal c (code-char (1+ code)))))`);
check(`(loop for code below 65536 for c = (code-char code) always
  (or (null c) (let ((up (char-upcase c)) (down (char-downcase c)))
    (and (eql down (char-downcase up)) (eql up (char-upcase down))
      (or (lower-case-p c) (eql up c)) (or (upper-case-p c) (eql down c))
      (eq (not (null (both-case-p c))) (not (null (or (upper-case-p c) (lower-case-p c)))))))))`);
check(`(let* ((source "aǅ ǈz") (copy (copy-seq source)))
  (and (equal (string-upcase source) "Aǅ ǈZ")
    (equal (miricl:unicode-string-upcase source) "AǄ ǇZ")
    (equal (miricl:unicode-string-downcase source) "aǆ ǉz")
    (equal (miricl:unicode-string-capitalize source) "Aǆ Ǉz")
    (eq copy (miricl:unicode-nstring-upcase copy :start 1 :end 2))
    (equal copy "aǄ ǈz") (equal source "aǅ ǈz")
    (equal (miricl:unicode-nstring-downcase copy :end 2) "aǆ ǈz")
    (equal (miricl:unicode-nstring-capitalize copy) "Aǆ Ǉz")))`);
check(`(and (handler-case (miricl:unicode-char-upcase 17) (type-error () t))
  (handler-case (miricl:unicode-char-downcase) (program-error () t))
  (= 1 (length (multiple-value-list (miricl:unicode-char-upcase (code-char 453)))))
  (string-equal "ǅ" "ǆ"))`);
console.log('Character case: reciprocal CL pairs across the BMP, titlecase traits, Unicode conversions, bounds, mutation, and folding pass.');
