import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  check(`(list (string-upcase "abλz") (string-downcase "AZΛ")
               (string-capitalize "hELLO 13A, wORLD!"))`, '("ABΛZ" "azλ" "Hello 13a, World!")');
  check(`(let* ((source (copy-seq "aBc")) (result (string-upcase source)))
            (and (equal source "aBc") (equal result "ABC") (not (eq result source))))`, 'T');
  check(`(let ((source (copy-seq "ABC"))) (eq source (string-upcase source)))`, 'T');
  check(`(let* ((backing (copy-seq "_aB-cD_"))
                  (source (make-array 5 :element-type 'character :displaced-to backing
                                     :displaced-index-offset 1 :fill-pointer 4)))
            (and (eq source (nstring-downcase source)) (equal backing "_ab-cD_")))`, 'T');
  check(`(equal (string-capitalize "xABC-DEF" :start 2 :end 7) "xABc-DeF")`, 'T');
  check(`(every (lambda (c)
                  (equal (string (miricl::unicode-char-upcase c))
                         (miricl::unicode-string-upcase (string c))))
                (list #\\a (code-char 181) (code-char 962) (code-char 955)))`, 'T');
  check(`(let ((up (fdefinition 'char-upcase)) (alpha (fdefinition 'alphanumericp))
                (copy (fdefinition 'subseq)))
            (unwind-protect
              (progn (setf (fdefinition 'char-upcase) (lambda (x) (declare (ignore x)) #\\!))
                     (setf (fdefinition 'alphanumericp) (lambda (x) (declare (ignore x)) nil))
                     (setf (fdefinition 'subseq) (lambda (&rest x) (declare (ignore x)) "!"))
                     (equal (string-capitalize "aBC-dEF") "Abc-Def"))
              (setf (fdefinition 'char-upcase) up) (setf (fdefinition 'alphanumericp) alpha)
              (setf (fdefinition 'subseq) copy)))`, 'T');
  check(`(and (handler-case (progn (nstring-upcase 'abc) nil) (type-error () t))
               (handler-case (progn (string-downcase "abc" :end 4) nil) (type-error () t))
               (equal (multiple-value-list (string-capitalize "aBC")) '("Abc")))`, 'T');
  console.log('String case traversal, sharing, canonical operations and recovery pass.');
} finally {vm.close();}
