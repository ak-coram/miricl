import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  check(`(list (char-code (char-upcase #\\a)) (char-code (char-downcase #\\Z))
               (char-code (char-upcase (code-char 955)))
               (char-code (char-downcase (code-char 923))))`, '(65 122 923 955)');
  check(`(let ((micro (code-char 181)) (final-sigma (code-char 962)))
            (and (eql micro (char-upcase micro))
                 (eql final-sigma (char-upcase final-sigma))))`, 'T');
  check(`(list (string-upcase "abλz") (string-downcase "AZΛ")
               (string-equal "λa" "ΛA"))`, '("ABΛZ" "azλ" T)');
  check(`(and (eql (char-upcase #\\a) #\\A)
               (= 1 (length (multiple-value-list (char-upcase #\\a))))
               (= 1 (length (multiple-value-list (char-downcase #\\A)))))`, 'T');
  check(`(and (handler-case (progn (char-upcase 42) nil) (type-error () t))
               (handler-case (progn (char-downcase :bad) nil) (type-error () t))
               (eql (char-upcase #\\z) #\\Z))`, 'T');
  console.log('String case kernel public behavior and recovery pass.');
} finally {vm.close();}
