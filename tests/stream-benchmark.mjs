#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
let workloads = [
  ['read-characters', 'λ🙂a'.repeat(10000), '(dotimes (i 30000) (read-char s)) (not (listen s))', 'T'],
  ['read-integers', '17 '.repeat(10000), '(let ((n 0)) (dotimes (i 10000) (incf n (read s))) n)', '170000'],
  ['read-lines', 'aλ🙂1234567890\n'.repeat(10000), '(let ((n 0)) (dotimes (i 10000) (incf n (length (read-line s)))) n)', '130000'],
  ['read-bulk', 'λ🙂a'.repeat(10000), '(let ((data (make-string 30000 :initial-element #\\x))) (and (= 30000 (read-sequence data s)) (char= (char data 0) #\\λ) (char= (char data 29999) #\\a)))', 'T'],
  ['write-bulk', '', `(write-sequence "${'λ🙂a'.repeat(10000)}" s) (= 30000 (length (get-output-stream-string s)))`, 'T'],
  ['write-characters', '', '(dotimes (i 10000) (write-char #\\λ s)) (= 10000 (length (get-output-stream-string s)))', 'T'],
];
if (args.includes('--broadcast')) {
  const text = 'λ🙂a'.repeat(10000);
  workloads = [
    ['write-broadcast-characters', '', `(let* ((a (make-string-output-stream)) (b (make-string-output-stream))
        (s (make-broadcast-stream a b)))
       (dotimes (i 10000) (write-char #\\λ s))
       (and (= 10000 (length (get-output-stream-string a)))
            (= 10000 (length (get-output-stream-string b)))))`, 'T'],
    ['write-broadcast-bulk', '', `(let* ((a (make-string-output-stream)) (b (make-string-output-stream))
        (c (make-string-output-stream)) (d (make-string-output-stream))
        (s (make-broadcast-stream a (make-broadcast-stream b c) d)))
       (write-string "${text}" s)
       (every (lambda (stream) (= 30000 (length (get-output-stream-string stream)))) (list a b c d)))`, 'T'],
    ['write-broadcast-lines', '', `(let* ((a (make-string-output-stream)) (b (make-string-output-stream))
        (s (make-broadcast-stream a b)))
       (dotimes (i 10000) (write-char #\\a s) (fresh-line s))
       (and (= 20000 (length (get-output-stream-string a)))
            (= 20000 (length (get-output-stream-string b)))))`, 'T'],
  ];
}
const quote = text => `"${text.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
const form = (name, source, body) => name.includes('broadcast') ? body :
  `(let ((s (${name.startsWith('write') ? 'make-string-output-stream' : `make-string-input-stream ${quote(source)}`}))) ${body})`;
if (args.includes('--sbcl-source')) {
  for (const [name, source, body, expected] of workloads) {
    console.log(`(let ((fn (compile nil '(lambda () ${form(name, source, body)}))))
      (assert (equal (funcall fn) ${expected}))
      (format t "${name}: ~,3F ms~%" (loop repeat 3 minimize
        (let ((start (get-internal-real-time))) (dotimes (i 50) (funcall fn))
          (* 20.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  const results = {};
  for (const [name, source, body, expected] of workloads) {
    vm.evaluate(`(defun stream-run () ${form(name, source, body)})`);
    const run = () => assert.equal(vm.evaluate('(stream-run)').output, `${expected}\n`, name);
    for (let i = 0; i < 5; i++) run();
    const times = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); times.push(performance.now() - start);}
    results[name] = {milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
