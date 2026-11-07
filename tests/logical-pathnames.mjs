import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(progn
  (setf (logical-pathname-translations "TEST") '(("**;*.*.*" "/workspace/**/*.*")))
  (let ((p (logical-pathname "test:dir;file.lisp.newest")))
    (and (typep p 'logical-pathname) (typep p 'pathname)
      (eq (class-of p) (find-class 'logical-pathname))
      (eq p (logical-pathname p)) (eq p (pathname p))
      (string= (namestring p) "TEST:DIR;FILE.LISP.NEWEST")
      (equal (pathname-directory p) '(:absolute "DIR"))
      (eq (pathname-device p) :unspecific) (eq (pathname-version p) :newest)
      (string= (pathname-name p :case :common) "FILE")
      (equal p (read-from-string (write-to-string p :readably t)))
      (string= (namestring (translate-logical-pathname p)) "/workspace/dir/file.lisp"))))`);
check(`(and (null (load-logical-pathname-translations "test"))
  (handler-case (logical-pathname "file.lisp") (type-error () t))
  (handler-case (logical-pathname "MISSING:file") (type-error () t))
  (handler-case (logical-pathname "TEST:%") (type-error () t))
  (handler-case (logical-pathname (make-string-input-stream "")) (type-error () t)))`);
check(`(let* ((p (make-pathname :host "TEST" :name "foo" :type "lisp" :version :newest))
              (copy (read-from-string (write-to-string p :readably t))))
  (and (typep p 'logical-pathname) (equal p copy)
    (string= (pathname-name p) "FOO")
    (string= (namestring (translate-logical-pathname p)) "/workspace/foo.lisp")))`);
check(`(progn
  (with-open-file (s "TEST:FILE.TXT" :direction :output :if-exists :supersede) (write-line "λ🙂" s))
  (let ((s (open "TEST:FILE.TXT")))
    (and (typep (pathname s) 'logical-pathname) (string= (read-line s) "λ🙂")
      (close s) (typep (logical-pathname s) 'logical-pathname)
      (probe-file "TEST:FILE.TXT") (probe-file "/workspace/file.txt")
      (delete-file "TEST:FILE.TXT") (not (probe-file "/workspace/file.txt")))))`);
check(`(progn
  (setf (logical-pathname-translations "CHAIN") '(("**;*.*.*" "TEST:**;*.*.*")))
  (string= (namestring (translate-logical-pathname "CHAIN:A;B.LISP")) "/workspace/a/b.lisp"))`);
check(`(progn
  (setf (logical-pathname-translations "CYCLE") '(("**;*.*.*" "CYCLE:**;*.*.*")))
  (handler-case (translate-logical-pathname "CYCLE:A.LISP") (file-error () t)))`);
check(`(and (string= (namestring (translate-pathname "foobar.lisp" "foo*.lisp" "baz*.lsp")) "bazbar.lsp")
  (string= (namestring (translate-pathname "a/b/c.lisp" "**/*.lisp" "out/**/*.lsp")) "out/a/b/c.lsp")
  (handler-case (translate-pathname "a" "b" "c") (error () t)))`);
check(`(let ((p (pathname "physical"))) (eq p (translate-logical-pathname p :allow-other-keys t :extra t)))`);
console.log('Logical pathnames: parsing, host mappings, wildcard captures, chained translation, cycles, file streams, and readable metadata pass.');
