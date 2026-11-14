import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  vm.mountFile('/stream-sequence-bytes.bin', Uint8Array.from([0, 1, 127, 128, 255]));
  vm.mountFile('/stream-sequence-text.txt', 'aλ🙂bc');
  check(`(let ((data (make-array 7 :initial-element :kept)))
    (with-open-file (in "/stream-sequence-bytes.bin" :element-type '(unsigned-byte 8))
      (and (= (read-sequence data in :start 1 :end 7) 6)
           (equalp data #(:kept 0 1 127 128 255 :kept))
           (= (read-sequence data in :start 2) 2))))`);
  check(`(let ((data (make-list 7 :initial-element :kept)))
    (with-open-file (in "/stream-sequence-text.txt")
      (and (= (read-sequence data in :start 1 :end 7) 6)
           (equal data '(:kept #\\a #\\λ #\\🙂 #\\b #\\c :kept))
           (eq (read-char in nil :eof) :eof))))`);
  check(`(let ((data #(0 1 127 128 255)))
    (with-open-file (out "/stream-sequence-output.bin" :direction :output
                        :if-exists :supersede :element-type '(unsigned-byte 8))
      (and (eq data (write-sequence data out :start 1 :end 4))
           (equalp (multiple-value-list (write-sequence #() out)) '(#())))))`);
  assert.deepEqual(await vm.readFile('/stream-sequence-output.bin'), Uint8Array.from([1, 127, 128]));
  check(`(let ((data (copy-seq "abcde")))
    (let ((in (make-string-input-stream data)))
      (and (= 5 (read-sequence data in :start 1)) (string= data "aaaaa")
           (char= (read-char in) #\\a))))`);
  check(`(let* ((storage (copy-seq "xabcdez"))
               (data (make-array 5 :element-type 'character :displaced-to storage :displaced-index-offset 1)))
    (let ((in (make-string-input-stream data)))
      (and (= 5 (read-sequence data in :start 1)) (string= storage "xaaaaaz"))))`);
  check(`(let* ((data (make-array 6 :element-type 'character :initial-contents "abcxxx" :fill-pointer 3))
               (in (make-string-input-stream "λ🙂")))
    (and (= 3 (read-sequence data in :start 1)) (= (fill-pointer data) 3)
         (string= data "aλ🙂")))`);
  check(`(let ((a (make-string-output-stream)) (b (make-string-output-stream)) (data (list #\\a #\\λ #\\🙂)))
    (let ((nested (make-broadcast-stream (make-broadcast-stream a b) a)))
      (and (eq data (write-sequence data nested))
           (string= (get-output-stream-string a) "aλ🙂aλ🙂")
           (string= (get-output-stream-string b) "aλ🙂"))))`);
  check(`(let ((a (make-string-output-stream)) (b (make-string-output-stream)))
    (let ((nested (make-broadcast-stream a b)))
      (and (equal (multiple-value-list (write-line "abc" nested :start 1 :end 3)) '("abc"))
           (string= (get-output-stream-string a) (format nil "bc~%"))
           (string= (get-output-stream-string b) (format nil "bc~%")))))`);
  check(`(let* ((a (make-string-output-stream)) (name (make-symbol "STREAM-ALIAS")) (data (copy-seq "ok")))
    (set name a)
    (let* ((alias (make-synonym-stream name)) (out (make-broadcast-stream alias a)))
      (set name out)
      (and (handler-case (progn (write-sequence "x" out) nil) (stream-error (e) (streamp (stream-error-stream e))))
           (set name a)
           (string= (get-output-stream-string a) "")
           (eq data (write-sequence data out))
           (string= (get-output-stream-string a) "okok"))))`);
  check(`(let ((out (make-string-output-stream)))
    (and (handler-case (progn (write-sequence '(#\\a 42 #\\b) out) nil) (type-error () t))
         (string= (get-output-stream-string out) "")))`);
  check(`(let ((out (make-string-output-stream)) (cycle (list #\\a #\\b)))
    (setf (cddr cycle) cycle)
    (and (handler-case (progn (write-sequence cycle out) nil) (type-error () t))
         (string= (get-output-stream-string out) "")))`);
  for (const bounds of [':start -1', ':start nil', ':start 3 :end 2', ':end 9', ':end 100000000000000000000'])
    check(`(let ((out (make-string-output-stream)))
      (and (handler-case (progn (write-sequence "abc" out ${bounds}) nil) (type-error () t))
           (string= (get-output-stream-string out) "")))`);
  for (const tail of [':start', ':unknown 1', ':unknown 1 :allow-other-keys nil :allow-other-keys t'])
    check(`(let ((out (make-string-output-stream)))
      (and (handler-case (progn (write-sequence "abc" out ${tail}) nil) (program-error () t))
           (string= (get-output-stream-string out) "")))`);
  check(`(let ((out (make-string-output-stream)))
    (and (string= "abc" (write-string "abc" out :start 1 :start -1 :end 2 :unknown t :allow-other-keys t))
         (string= (get-output-stream-string out) "b")))`);
  check(`(let ((out (make-string-output-stream)) (events nil))
    (and (equal (multiple-value-list
             (write-sequence (progn (push :sequence events) (values "abc" :ignored))
                             (progn (push :stream events) out)
                             :start (progn (push :start events) 1)
                             :end (progn (push :end events) 2))) '("abc"))
         (equal (reverse events) '(:sequence :stream :start :end))
         (string= (get-output-stream-string out) "b")))`);
  check(`(let ((out (make-string-output-stream)))
    (equal (multiple-value-list (multiple-value-prog1 (values :kept :second)
           (write-line "abc" (make-broadcast-stream out out)))) '(:kept :second)))`);
  // All primitive dependencies are canonical, including the graph's EQ hash.
  check(`(let ((saved-length (symbol-function 'length)) (saved-make (symbol-function 'make-hash-table))
              (saved-get (symbol-function 'gethash)) (saved-put (fdefinition '(setf gethash)))
              (saved-remove (symbol-function 'remhash)) (saved-eq (symbol-function 'eq))
              (saved-char (symbol-function 'write-char)) (out (make-string-output-stream)))
    (unwind-protect
      (progn
        (setf (symbol-function 'length) (lambda (&rest args) (declare (ignore args)) (values :replaced :extra)))
        (setf (symbol-function 'make-hash-table) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
        (setf (symbol-function 'gethash) (lambda (&rest args) (declare (ignore args)) (values t :extra)))
        (setf (fdefinition '(setf gethash)) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
        (setf (symbol-function 'remhash) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
        (setf (symbol-function 'eq) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
        (setf (symbol-function 'write-char) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
        (string= (progn (write-sequence '(#\\a #\\λ) (make-broadcast-stream out out))
                        (get-output-stream-string out)) "aλaλ"))
      (setf (symbol-function 'length) saved-length) (setf (symbol-function 'make-hash-table) saved-make)
      (setf (symbol-function 'gethash) saved-get) (setf (fdefinition '(setf gethash)) saved-put)
      (setf (symbol-function 'remhash) saved-remove) (setf (symbol-function 'eq) saved-eq)
      (setf (symbol-function 'write-char) saved-char)))`);
  check(`(let ((saved (symbol-function 'error)) (out (make-string-output-stream)))
    (unwind-protect
      (progn
        (setf (symbol-function 'error) (lambda (&rest args) (declare (ignore args)) :replaced))
        (and (handler-case (progn (write-sequence "abc" out :start -1) nil)
               (type-error () t))
             (handler-case (progn (write-sequence '(#\\a 42) out) nil)
               (type-error () t))))
      (setf (symbol-function 'error) saved)))`);
  check('(string= (with-output-to-string (s) (write-string "recovered" s)) "recovered")');
  console.log('Stream sequence kernels: characters/bytes, overlap, graph sharing/cycles, checked bounds, values, effects and recovery pass.');
} finally { vm.close(); }
