// Public character behavior after porting table searches and Unicode names.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => {
  const result = vm.evaluate(source, {eof: true});
  assert.equal(result.output, `${expected}\n`, source);
};
try {
  check('(mapcar (lambda (s) (let ((c (name-char s))) (and c (char-code c))))\n' +
    '\'( "U0041" "u+0041" "U000041" "U+1F642" "u10ffff" "U0000"))',
    '(65 65 65 128578 1114111 0)');
  check('(mapcar #\'name-char \'("U110000" "U+FFFFFF" "U041" "U+041" "U0000041" "U+004G" "U-0041" "U+ 041" "U+٠٠٤١" "" "U" "U+"))',
    '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)');
  check('(list (char-code (name-char \'space)) (char-code (name-char :newline))\n' +
    '      (char-code (name-char "Linefeed")) (char-code (name-char "Nul")))', '(32 10 10 0)');
  check('(list (digit-char-p #\\7) (digit-char-p #\\Z 36) (digit-char-p #\\z 36)\n' +
    '      (digit-char-p #\\٥) (digit-char-p #\\🙂 36) (digit-char-p #\\Z 10))',
    '(7 35 35 5 NIL NIL)');
  check('(mapcar (lambda (c) (not (null (alpha-char-p c)))) \'(#\\A #\\z #\\λ #\\Ж #\\٥ #\\🙂))',
    '(T T T T NIL NIL)');
  check('(list (length (multiple-value-list (digit-char-p #\\7)))\n' +
    '      (length (multiple-value-list (alpha-char-p #\\λ)))\n' +
    '      (length (multiple-value-list (name-char "U0041"))))', '(1 1 1)');
  check('(handler-case (name-char 42) (type-error () :type))', ':TYPE');
  check('(handler-case (alpha-char-p 42) (type-error () :type))', ':TYPE');
  check('(handler-case (digit-char-p #\\A 37) (type-error () :type))', ':TYPE');
  check('(char-code (name-char "U0041"))', '65');
  console.log('Character kernel public checks passed.');
} finally {
  vm.close?.();
}
