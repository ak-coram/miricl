import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(let* ((a (make-string-input-stream "1")) (b (make-string-input-stream "2 λ"))
              (s (make-concatenated-stream a (make-string-input-stream "") b)))
  (and (typep s 'concatenated-stream) (input-stream-p s) (not (output-stream-p s))
    (= 12 (read s)) (char= #\\λ (read-char s))
    (progn (unread-char #\\λ s) (char= #\\λ (read-char s)))
    (eq :eof (read s nil :eof)) (null (concatenated-stream-streams s))
    (close s) (not (open-stream-p s)) (open-stream-p a) (open-stream-p b)))`);
check(`(let* ((s (make-concatenated-stream (make-string-input-stream "(a .")
                    (make-string-input-stream " b) #|com") (make-string-input-stream "ment|# 42"))))
  (and (equal (read s) '(a . b)) (= 42 (read s)) (not (listen s))))`);
check(`(let* ((out (make-string-output-stream)) (input (make-string-input-stream " abλ🙂"))
              (s (make-echo-stream input out)))
  (and (typep s 'echo-stream) (eq input (echo-stream-input-stream s))
    (eq out (echo-stream-output-stream s))
    (char= #\\a (peek-char t s)) (string= " " (get-output-stream-string out))
    (char= #\\a (read-char s)) (progn (unread-char #\\a s) (char= #\\a (read-char s)))
    (string= "a" (get-output-stream-string out))
    (progn (write-sequence "!" s) (string= "!" (get-output-stream-string out)))
    (string= "bλ🙂" (read-line s)) (string= "bλ🙂" (get-output-stream-string out))
    (close s) (open-stream-p input) (open-stream-p out)))`);
check(`(let* ((out (make-string-output-stream))
              (s (make-echo-stream (make-concatenated-stream (make-string-input-stream "1")
                  (make-string-input-stream "23 ")) out)))
  (and (= 123 (read s)) (string= "123 " (get-output-stream-string out))))`);
check(`(let* ((s (make-concatenated-stream (make-string-input-stream "aλ") (make-string-input-stream "🙂")))
              (data (make-array 5 :initial-element :empty)))
  (and (= 3 (read-sequence data s)) (equalp data #(#\\a #\\λ #\\🙂 :empty :empty))))`);
check(`(progn
  (with-open-file (s "first.bin" :direction :output :element-type '(unsigned-byte 16)) (write-byte 1234 s))
  (with-open-file (s "last.bin" :direction :output :element-type '(unsigned-byte 16)) (write-byte 5678 s))
  (with-open-file (out "echo.bin" :direction :output :element-type '(unsigned-byte 16))
    (with-open-file (a "first.bin" :element-type '(unsigned-byte 16))
      (with-open-file (b "last.bin" :element-type '(unsigned-byte 16))
        (let ((s (make-echo-stream (make-concatenated-stream a b) out)))
          (and (= 1234 (read-byte s)) (= 5678 (read-byte s)) (eq :eof (read-byte s nil :eof)))))))
  (with-open-file (s "echo.bin" :element-type '(unsigned-byte 16))
    (and (= 1234 (read-byte s)) (= 5678 (read-byte s)) (= 2 (file-length s)))))`);
console.log('Concatenated and echo streams: cross-component reading, Unicode, unread, echo suppression, binary I/O, and component lifetime pass.');
