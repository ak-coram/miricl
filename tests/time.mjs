import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(and (equal (multiple-value-list (decode-universal-time 0 0)) '(0 0 0 1 1 1900 0 nil 0))
  (equal (multiple-value-list (decode-universal-time 0 -1)) '(0 0 1 1 1 1900 0 nil -1))
  (= 2414296800 (encode-universal-time 0 0 1 4 7 1976 5)))`);
check(`(loop for year in '(1900 1999 2000 2001 2100 2400 1000000000000000000000000000000)
  always (loop for month from 1 to 12 always
    (let* ((stamp (encode-universal-time 59 58 23 1 month year 1/3600))
           (decoded (multiple-value-list (decode-universal-time stamp 1/3600))))
      (and (equal (subseq decoded 0 6) (list 59 58 23 1 month year))
        (= (nth 8 decoded) 1/3600) (null (nth 7 decoded))))))`);
const before = Date.now()/1000 + 2208988800;
const stamp = Number(vm.evaluate('(get-universal-time)').output.trim());
assert.ok(stamp >= Math.floor(before) && stamp <= Math.floor(Date.now()/1000 + 2208988800));
check(`(let* ((year (nth-value 5 (get-decoded-time))) (start (- year 50)))
  (loop for short below 100 always
    (= (+ start (mod (- short start) 100))
       (nth-value 5 (decode-universal-time (encode-universal-time 0 0 0 1 1 short 0) 0)))))`);
check(`(let ((real (get-internal-real-time)))
  (and (null (sleep 1/50)) (>= (- (get-internal-real-time) real) 15)))`);
check(`(let ((run (get-internal-run-time)))
  (dotimes (i 10000) (* i i)) (>= (get-internal-run-time) run))`);
check(`(let ((result nil))
  (and (plusp (length (with-output-to-string (*trace-output*)
      (setq result (multiple-value-list (time (values 1 2 3)))))))
    (equal result '(1 2 3))
    (plusp (length (with-output-to-string (*trace-output*)
      (assert (null (multiple-value-list (time (values))))))))))`);
check(`(and (handler-case (sleep -1) (type-error () t))
  (handler-case (decode-universal-time -1) (type-error () t))
  (handler-case (decode-universal-time 0 1/7) (type-error () t))
  (handler-case (encode-universal-time 0 0 0 29 2 1900) (error () t))
  (typep (encode-universal-time 0 0 0 29 2 2000) 'unsigned-byte))`);
check(`(and (= 0 (encode-universal-time 0 0 0 1 1 1900 0))
  (= 0 (encode-universal-time 0 0 14 1 1 1900 -14))
  (handler-case (encode-universal-time 31 23 10 1 1 1900 -14) (error () t)))`);
console.log('Time: real clocks, sleeping, leap years, rational zones, bignum years, two-digit years, and TIME values pass.');
