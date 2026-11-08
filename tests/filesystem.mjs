// Portable memory files work in the import-free browser image as well as WASI.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const module = await WebAssembly.compile(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
assert.deepEqual(WebAssembly.Module.imports(module), []);
const vm = new Miricl(await WebAssembly.instantiate(module));
const check = (source, expected = 'T') => {
  const result = vm.evaluate(source, {eof: true});
  assert.equal(result.output, `${expected}\n`, source);
};
check(`(progn
  (with-open-file (s "test.txt" :direction :output)
    (write-string "aλ🙂\n123 (a . b)" s))
  (with-open-file (s "test.txt")
    (and (typep s 'file-stream) (input-stream-p s) (not (output-stream-p s))
      (= 19 (file-length s)) (char= (read-char s) #\\a)
      (char= (peek-char nil s) #\\λ) (char= (read-char s) #\\λ)
      (progn (unread-char #\\λ s) (char= (read-char s) #\\λ))
      (char= (read-char s) #\\🙂)
      (equal (multiple-value-list (read-line s)) '("" nil))
      (= (read s) 123) (equal (read s) '(a . b))
      (eq (read s nil :eof) :eof) (not (listen s)))))`);
check(`(and (null (open "missing" :if-does-not-exist nil))
  (handler-case (open "missing") (file-error () t))
  (handler-case (open "test.txt" :direction :output) (file-error () t))
  (null (open "test.txt" :direction :output :if-exists nil))
  (with-open-file (s "test.txt" :direction :probe) (not (open-stream-p s))))`);
check(`(let ((s (open "test.txt")))
  (and (= (file-string-length s "λ🙂") 6) (file-position s :end)
    (= (file-position s) (file-length s)) (file-position s :start)
    (= (file-position s) 0) (char= (read-char s) #\\a)
    (close s) (pathnamep (pathname s))
    (handler-case (read-char s) (stream-error () t))))`);
check(`(progn
  (with-open-file (s "test.txt" :direction :output :if-exists :overwrite) (write-string "z" s))
  (with-open-file (s "test.txt") (char= (read-char s) #\\z)))`);
check(`(progn
  (with-open-file (s "test.txt" :direction :output :if-exists :append) (write-char #\\! s))
  (with-open-file (s "test.txt") (file-position s (- (file-length s) 1)) (char= (read-char s) #\\!)))`);
check(`(and (= 7 (catch 'exit
    (with-open-file (s "test.txt" :direction :output :if-exists :supersede)
      (write-string "replacement" s) (throw 'exit 7))))
  (with-open-file (s "test.txt") (char= (read-char s) #\\z))
  (= 9 (catch 'exit (with-open-file (s "aborted" :direction :output) (throw 'exit 9))))
  (not (probe-file "aborted")))`);
check(`(with-open-file (s "character-widths" :direction :io :if-exists :supersede)
  (let ((codes '(0 127 128 2047 2048 65535 65536 1114111 10)))
    (dolist (code codes) (write-char (code-char code) s))
    (and (= (file-length s) 21) (not (fresh-line s))
      (file-position s 0)
      (equal (loop repeat (length codes) collect (char-code (read-char s))) codes))))`);
check(`(equal (multiple-value-list
  (with-open-file (s "values" :direction :output) (declare (ignorable s)) (values 1 2 3))) '(1 2 3))`);
check(`(progn
  (with-open-file (s "reader.lisp" :direction :output)
    (write-string "#|comment|# #1=(42 . #1#) #.(+ 2 3) (a . b)" s))
  (with-open-file (s "reader.lisp")
    (let ((x (read s))) (and (= 42 (car x)) (eq x (cdr x))
      (= 5 (read s)) (equal '(a . b) (read s)) (eq :eof (read s nil :eof))))))`);
check(`(let ((*readtable* (copy-readtable)))
  (set-macro-character #\\@ (lambda (s c) (declare (ignore c)) (list (read-char s) (read s))))
  (with-open-file (s "macro.txt" :direction :output) (write-string "@λ42 17" s))
  (with-open-file (s "macro.txt") (and (equal (read s) '(#\\λ 42)) (= (read s) 17))))`);
for (const signed of [false, true]) for (const bits of [8, 16, 32, 64, 128]) {
  const kind = signed ? 'signed-byte' : 'unsigned-byte';
  const values = signed ? `0 -1 1 (- (ash 1 ${bits-1})) (1- (ash 1 ${bits-1}))` : `0 1 (1- (ash 1 ${bits}))`;
  check(`(let ((data (list ${values})))
    (with-open-file (s "binary" :direction :output :if-exists :supersede :element-type '(${kind} ${bits}))
      (write-sequence data s))
    (with-open-file (s "binary" :element-type '(${kind} ${bits}))
      (let ((target (make-array (1+ (length data)) :initial-element :untouched)))
        (and (equal (stream-element-type s) '(${kind} ${bits}))
          (= (file-length s) (length data)) (= (read-sequence target s) (length data))
          (equalp (subseq target 0 (length data)) (coerce data 'vector))
          (eq (aref target (length data)) :untouched) (eq (read-byte s nil :eof) :eof)
          (file-position s 1) (= (read-byte s) (second data))))))`);
  const lower = signed ? `(- (ash 1 ${bits - 1}))` : '0';
  const upper = `(1- (ash 1 ${signed ? bits - 1 : bits}))`;
  check(`(with-open-file (s "invalid-byte" :direction :output :if-exists :supersede
      :element-type '(${kind} ${bits}))
    (and (every (lambda (bad)
      (handler-case (progn (write-byte bad s) nil)
        (type-error (condition)
          (and (eql bad (type-error-datum condition))
               (equal (type-error-expected-type condition)
                 (if (integerp bad) '(${kind} ${bits}) 'integer))))))
      (list (1- ${lower}) (1+ ${upper}) 1.0 'invalid))
      (= (file-position s) 0) (= (file-length s) 0)))`);
}
check(`(progn
  (with-open-file (s "sequence.txt" :direction :output)
    (write-sequence '(#\\a #\\λ #\\🙂) s) (write-line "tail" s :end 2))
  (with-open-file (s "sequence.txt")
    (let ((data (make-list 8 :initial-element #\\x)))
      (and (= 7 (read-sequence data s :start 1)) (equal data '(#\\x #\\a #\\λ #\\🙂 #\\t #\\a #\\Newline #\\x))))))`);
check(`(progn
  (with-open-file (s "load.lisp" :direction :output) (write-string "(defparameter *file-value* 123)" s))
  (load "load.lisp") (= *file-value* 123))`);
check(`(multiple-value-bind (new old true) (rename-file "values" "renamed")
  (and (pathnamep new) (pathnamep old) (equal (namestring new) (namestring true)) (not (probe-file "values"))
    (probe-file "renamed") (delete-file "renamed") (not (probe-file "renamed"))))`);
check(`(with-open-file (s "columns" :direction :io :if-exists :supersede)
  (write-string "λ🙂" s)
  (and (file-position s :start) (not (fresh-line s))
    (file-position s :end) (fresh-line s) (not (fresh-line s))))`);
check(`(progn
  (with-open-file (s "backup" :direction :output) (write-string "old" s))
  (with-open-file (s "backup" :direction :output :if-exists :rename) (write-string "new" s))
  (and (with-open-file (s "backup") (string= "new" (read-line s)))
    (with-open-file (s "backup.bak") (string= "old" (read-line s)))
    (progn
      (catch 'abort (with-open-file (s "backup" :direction :output :if-exists :rename)
        (write-string "discard" s) (throw 'abort nil)))
      (with-open-file (s "backup") (string= "new" (read-line s))))))`);
check(`(with-open-file (out "broadcast" :direction :output :if-exists :supersede)
  (write-line "Hello world!" out)
  (let ((s (make-broadcast-stream (make-string-output-stream) out)))
    (and (= 13 (file-length s)) (= 13 (file-position s))
      (= 6 (file-string-length s "λ🙂")))))`);
for (const [type, operation] of [["character", "(read-byte s)"], ["(unsigned-byte 8)", "(read-char s)"]]) {
  check(`(with-open-file (s "wrong-element-type" :direction :io :if-exists :supersede :element-type '${type})
    (handler-case (progn ${operation} nil)
      (type-error (c) (and (eq s (type-error-datum c)) (not (typep s (type-error-expected-type c)))))))`);
}
check(`(progn
  (with-open-file (s "nested-inner.lisp" :direction :output) (write-string "(defparameter *nested-value* 42)" s))
  (with-open-file (s "nested-outer.lisp" :direction :output)
    (write-string "#.(load \\\"nested-inner.lisp\\\") #.(read *standard-input*) 17 99" s))
  (with-open-file (*standard-input* "nested-outer.lisp")
    (and (read) (= 17 (read)) (= 99 (read)) (= 42 *nested-value*))))`);
check(`(let ((path "directories/a/b/file.txt"))
  (multiple-value-bind (original created) (ensure-directories-exist path)
    (and (eq original path) created (probe-file "directories/a/b/")
      (not (second (multiple-value-list (ensure-directories-exist path))))
      (handler-case (delete-file "directories/a/") (file-error () t))
      (delete-file "directories/a/b/") (not (probe-file "directories/a/b/"))
      (delete-file "directories/a/") (delete-file "directories/"))))`);
check(`(and (equal (truename "/") #P"/") (pathnamep (user-homedir-pathname))
  (not (pathname-name (user-homedir-pathname)))
  (null (file-author "test.txt")) (integerp (file-write-date "test.txt"))
  (handler-case (file-author "not-present") (file-error () t))
  (handler-case (file-write-date "*") (file-error () t))
  (handler-case (ensure-directories-exist "*/file") (file-error () t)))`);
check(`(let* ((original *readtable*)
              (stream (make-concatenated-stream
                (make-string-input-stream "(defparameter *stream-load-value* 41)")
                (make-string-input-stream "(incf *stream-load-value*) (setq *readtable* (copy-readtable))"))))
  (and (load stream) (= *stream-load-value* 42) (open-stream-p stream)
    (eq original *readtable*) (null *load-pathname*) (null *load-truename*)))`);
check(`(progn
  (with-open-file (s "load-bindings.lisp" :direction :output)
    (write-string "(defparameter *stream-paths* (list *load-pathname* *load-truename*))" s))
  (with-open-file (s "load-bindings.lisp")
    (and (load s) (open-stream-p s)
      (equal (first *stream-paths*) (merge-pathnames s))
      (equal (second *stream-paths*) (truename s)))))`);
check(`(let* ((*default-pathname-defaults* #P"/relative-load/")
              (s (open "source.lisp" :direction :io :if-exists :supersede)))
  (unwind-protect
    (progn (write-string "(defparameter *relative-stream-value* 42)" s) (file-position s 0)
      (let ((*default-pathname-defaults* #P"/elsewhere/"))
        (and (equal (truename s) #P"/relative-load/source.lisp") (load s)
          (= *relative-stream-value* 42) (close s) (delete-file s))))
    (close s)))`);
check(`(let ((table *readtable*))
  (and (handler-case (load (make-string-input-stream "(setq *readtable* (copy-readtable)) (error 'simple-error)"))
         (simple-error () t))
    (eq table *readtable*)))`);
console.log('Memory files: directories, metadata, Unicode, binary I/O, positions, aborts, rename, delete, and file/stream loading pass.');

// Directory discovery includes both explicitly created and implied parents.
vm.mountFile('registry/alpha/one.lisp', '');
vm.mountFile('registry/alpha/two.lisp', '');
vm.mountFile('registry/alpha/deep/three.lisp', '');
vm.mountFile('registry/λ🙂/four.lisp', '');
check('(progn (ensure-directories-exist "registry/empty/child/") t)');
check(`(sort (mapcar #'namestring (directory "registry/*/")) #'string<)`,
  '("/registry/alpha/" "/registry/empty/" "/registry/λ🙂/")');
check(`(sort (mapcar #'namestring (directory "registry/**/")) #'string<)`,
  '("/registry/" "/registry/alpha/" "/registry/alpha/deep/" "/registry/empty/" "/registry/empty/child/" "/registry/λ🙂/")');
check('(mapcar #\'namestring (directory "registry/alpha"))', '("/registry/alpha/")');
check('(mapcar #\'namestring (directory "/"))', '("/")');
check('(directory "registry/missing/")', 'NIL');
check(`(let* ((first (directory "registry/*/")) (second (directory "registry/*/")))
  (and (not (eq first second))
    (every (lambda (path) (equal path (truename path))) first)
    (= (length (remove-duplicates first :test #'equal)) 3)))`);
console.log('Memory directories: implicit parents, empty directories, recursive wildcards, root, Unicode, unique truenames, and fresh lists pass.');

// A controlled clock exercises real modification dates without sleeping.
const service = vm.hostService.bind(vm);
let date = 4000000000;
vm.hostService = (operation, argument) => operation === 0 ? date : service(operation, argument);
vm.mountFile('dated', 'original');
check('(file-write-date "dated")', '4000000000');
date++;
check(`(with-open-file (s "dated" :direction :output :if-exists :overwrite)
  (write-string "changed" s)
  (and (null (file-write-date s))
    (progn (finish-output s) (= (file-write-date s) 4000000001))))`);
date++;
check(`(progn (rename-file "dated" "renamed-date")
  (= (file-write-date "renamed-date") 4000000001))`);
check(`(with-open-file (s "renamed-date" :direction :io :if-exists :overwrite :element-type '(unsigned-byte 8))
  (write-byte 65 s) (and (null (file-write-date s))
    (progn (force-output s) (= (file-write-date s) 4000000002))))`);
date++;
check(`(progn
  (catch 'abort (with-open-file (s "renamed-date" :direction :output :if-exists :supersede)
    (write-string "discarded" s) (throw 'abort nil)))
  (= (file-write-date "renamed-date") 4000000002))`);
vm.vm['host-services'].set(0, null);
check('(= (file-write-date "renamed-date") 4000000002)');
vm.mountFile('undated', 'no clock');
check('(null (file-write-date "undated"))');
console.log('Memory files: mounted, character, binary, renamed, and rolled-back modification dates pass with optional clock support.');

const large = Uint8Array.from({length: 1024 * 1024}, (_, i) => (i * 17) & 255);
vm.mountFile('/bulk/../bytes λ.bin', large);
const exported = await vm.readFile('/bytes λ.bin');
assert.deepEqual(exported, large);
exported[0] = 99;
assert.equal((await vm.readFile('/bytes λ.bin'))[0], 0, 'Exported bytes are independent copies.');
assert.throws(() => vm.mountFile('../outside.bin', large));
assert.equal(vm.evaluate('(+ 20 22)').output, '42\n', 'An invalid mount leaves the evaluator usable.');
console.log('Memory files: bulk byte transfers, canonical Unicode paths, independent copies and error cleanup pass.');
