#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  try {assert.equal(evaluate.runInContext(context, {timeout: 2000}), 'T\n', source);}
  catch (error) {vm.vm.unwind(); throw error;}
};
for (const source of [
  `(with-input-from-string (s "aλ🙂")
    (and (char= (read-char s) #\\a) (char= (peek-char nil s) #\\λ)
      (char= (read-char s) #\\λ) (char= (read-char-no-hang s) #\\🙂)
      (eq (read-char s nil :eof) :eof) (not (listen s))))`,
  `(with-input-from-string (s "a\nb")
    (and (equal (multiple-value-list (read-line s)) '("a" nil))
      (equal (multiple-value-list (read-line s)) '("b" t))
      (equal (multiple-value-list (read-line s nil :eof)) '(:eof t))))`,
  `(with-input-from-string (s "  λ")
    (let ((c (peek-char t s))) (read-char s) (unread-char c s) (char= (read-char s) c)))`,
  `(let ((index nil))
    (and (= 123 (with-input-from-string (s "xx123 456" :start 2 :index index) (read s))) (= index 6)))`,
  `(let ((index 17))
    (and (= 1 (catch 'done (with-input-from-string (s "123" :index index) (read s) (throw 'done 1)))) (= index 17)))`,
  `(with-input-from-string (s "#1=(1 . #1#) #1#")
    (let ((x (read s))) (and (eq x (cdr x)) (handler-case (read s) (reader-error (c) (eq s (stream-error-stream c)))))))`,
  `(let ((*standard-input* (make-string-input-stream "#.(read *standard-input*) 42 17")))
    (and (= 42 (read)) (= 17 (read))))`,
  `(and (= 7 (catch 'done (read-from-string "#.(throw 'done 7)"))) (= 42 (read-from-string "42")))`,
  `(with-input-from-string (s "123" :end 2) (= 12 (read s)))`,
  `(let ((data (make-string 5 :initial-element #\\x)))
    (and (= 3 (read-sequence data (make-string-input-stream "aλ🙂"))) (string= data "aλ🙂xx")))`,
  `(let ((data (copy-seq "abcde")))
    (and (= 5 (read-sequence data (make-string-input-stream data) :start 1)) (string= data "aaaaa")))`,
  `(let* ((storage (copy-seq "abcde"))
          (target (make-array 5 :element-type 'character :displaced-to storage :fill-pointer 1)))
    (with-output-to-string (s target) (write-sequence storage s :end 4))
    (and (string= storage "aaaaa") (string= target "aaaaa")))`,
  `(string= (with-output-to-string (*standard-output*) (write-string "abcde" nil :start 1 :end 4)) "bcd")`,
  `(string= (with-output-to-string (*terminal-io*) (write-line "abcde" t :start 1 :end 4)) "bcd\n")`,
  `(string= (with-output-to-string (s) (write-string (make-array 0 :element-type nil) s)) "")`,
  `(let* ((i (make-string-input-stream "123 λ")) (o (make-string-output-stream))
          (s (make-two-way-stream i o)))
    (and (typep s 'two-way-stream) (typep s 'stream) (input-stream-p s) (output-stream-p s)
      (eq i (two-way-stream-input-stream s)) (eq o (two-way-stream-output-stream s))
      (= 123 (read s)) (char= (read-char s) #\\λ)
      (progn (write-string "🙂" s) (string= (get-output-stream-string o) "🙂"))
      (close s) (not (open-stream-p s)) (open-stream-p i) (open-stream-p o)))`,
  `(let* ((o (make-string-output-stream)) (s (make-two-way-stream (make-string-input-stream "abc") o)) (same nil))
    (format s (lambda (stream) (setq same (eq stream s)) (write-string "x" stream)))
    (format s "~&~A" 42)
    (let ((data (make-string 3)))
      (and same (= 3 (read-sequence data s)) (string= data "abc")
        (eq data (write-sequence data s)) (string= (get-output-stream-string o) "x\n42abc"))))`,
  `(let ((target (make-string-input-stream "a"))) (declare (special target))
    (let ((s (make-synonym-stream 'target)))
      (and (typep s 'synonym-stream) (eq 'target (synonym-stream-symbol s)) (char= (read-char s) #\\a)
        (let ((target (make-string-input-stream "λ"))) (declare (special target)) (char= (read-char s) #\\λ))
        (let ((target (make-string-output-stream))) (declare (special target))
          (and (output-stream-p s) (not (input-stream-p s)) (eq (stream-element-type s) 'character)
            (progn (write-sequence "🙂" s) (string= (get-output-stream-string target) "🙂")))))))`,
  `(let* ((s (make-two-way-stream (make-string-input-stream "a") (make-string-output-stream))))
    (close s) (handler-case (read-char s) (stream-error (c) (eq s (stream-error-stream c)))))`,
  `(let* ((s (make-synonym-stream '*standard-output*)))
    (close s) (handler-case (write-char #\\a s) (stream-error (c) (eq s (stream-error-stream c)))))`,
]) check(source);
for (const source of ['(', '"', '#|comment']) {
  const quoted = `"${source.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
  check(`(let ((s (make-string-input-stream ${quoted})))
    (handler-case (read s nil :eof) (end-of-file (c) (eq s (stream-error-stream c)))))`);
}
check('(= 42 (read-from-string "42"))');
console.log('Guest string input: Unicode, bounds, lookahead, EOF, nested readers, labels, bulk transfers, aliasing, output defaults, two-way and synonym streams, and recovery pass.');
