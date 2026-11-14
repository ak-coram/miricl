import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (source, expected) => assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
try {
  check(`(mapcar #'read-from-string '("0" "-0" "+123" "123." "2147483647" "-2147483648" "2147483648" "-2147483649"))`,
        '(0 0 123 123 2147483647 -2147483648 2147483648 -2147483649)');
  check(`(let ((*read-base* 16)) (mapcar #'read-from-string '("a" "-ff" "123." "10000000000000000")))`,
        '(10 -255 123 18446744073709551616)');
  check(`(list (let ((*read-default-float-format* 'single-float)) (typep (read-from-string "1.5") 'single-float))
               (let ((*read-default-float-format* 'double-float)) (typep (read-from-string "1.5") 'double-float)))`, '(T T)');
  check(`(eq '|123| (read-from-string "|123|"))`, 'T');
  check(`(handler-case (let ((*read-base* 1)) (read-from-string "123")) (type-error () :type))`, ':TYPE');
  check(`(read-from-string "42")`, '42\n2');
  console.log('Reader integer and float-format kernel checks passed.');
} finally { vm.close(); }
