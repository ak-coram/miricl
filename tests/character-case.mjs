import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(and (eql (name-char "u+fEff") (code-char 65279))
  (eql #\\U+FEFF (name-char 'ufeff))
  (eql (name-char "U+01F642") (code-char 128578))
  (eql #\\U10FFFF (code-char 1114111))
  (every #'null (mapcar #'name-char '("U+123" "U+110000" "U+0000000" "U+FEFG" "U+")))
  (handler-case (name-char 65) (type-error () t)))`);
// MIRICL uses one identity per character, including values reconstructed by
// readers and FASLs. EQ hash tables, property indicators and control tags must
// observe that same choice across character construction paths.
check(`(loop for code in '(0 61 945 128578 1114111) for c = (code-char code) always
  (let ((table (make-hash-table :test 'eq)) (text (string c)))
    (setf (gethash c table) :found)
    (and (eq c (code-char code)) (eq c (char text 0))
      (eq c (read-char (make-string-input-stream text)))
      (eq c (read-from-string (write-to-string c)))
      (eq (gethash (char text 0) table) :found)
      (eq (getf (list c :found) (code-char code)) :found)
      (eq (catch c (throw (char text 0) :found)) :found))))`);
check(`(let ((function (miricl-compiler:compile-native '(lambda (value) (eq value #\\=)))))
  (and (funcall function (code-char 61)) (not (funcall function #\\:))))`);
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
