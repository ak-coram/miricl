import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(let* ((p (make-pathname :directory '(:relative "foo" "BAR" "Mixed")
    :name "source" :type "lisp" :version :newest)))
  (and (equal (pathname-directory p) '(:relative "foo" "BAR" "Mixed"))
    (equal (pathname-directory p :case :common) '(:relative "FOO" "bar" "Mixed"))
    (equal (pathname-name p :case :common) "SOURCE") (eq (pathname-version p) :newest)
    (equal (pathname-type p :allow-other-keys t :unused 1) "lisp")
    (equal p (make-pathname :defaults p))
    (equal p (read-from-string (write-to-string p :readably t)))
    (not (equal p (make-pathname :defaults p :version 1)))
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash p table) :found)
      (eq :found (gethash (read-from-string (write-to-string p :readably t)) table)))))`);
check(`(let ((p (make-pathname :type "lsp" :name nil)))
  (and (null (pathname-name p)) (equal "lsp" (pathname-type p))
    (equal p (read-from-string (write-to-string p :readably t)))))`);
check(`(let* ((base (make-pathname :directory '(:absolute "src" "old") :name "base" :type "lisp" :version 2))
              (p (merge-pathnames (make-pathname :directory '(:relative :back "new") :name "code") base)))
  (and (equal (pathname-directory p) '(:absolute "src" "new"))
    (equal (pathname-name p) "code") (equal (pathname-type p) "lisp")
    (eq (pathname-version p) :newest)
    (= (pathname-version (merge-pathnames (make-pathname) base)) 2)))`);
check(`(and (pathname-match-p "src/x.lisp" "src/*.lisp")
  (pathname-match-p "src/a/b/x.lisp" "src/**/*.lisp")
  (pathname-match-p "src/x.lisp" "src/**/*.lisp")
  (not (pathname-match-p "src/a/x.lisp" "src/*.lisp"))
  (not (pathname-match-p "src/x.dat" "src/*.lisp"))
  (not (pathname-match-p "src/*.lisp" "src/x.lisp"))
  (wild-pathname-p "src/**/*.lisp" :directory)
  (not (wild-pathname-p "src/**/*.lisp" :type))
  (wild-pathname-p (make-pathname :version :wild) :version)
  (not (wild-pathname-p (make-pathname :name "x" :version :newest))))`);
check(`(let ((name (copy-seq "name")) (directory (list :relative (copy-seq "dir"))))
  (let ((p (make-pathname :name name :directory directory)))
    (setf (char name 0) #\\x (char (second directory) 0) #\\x)
    (and (equal (pathname-name p) "name") (equal (pathname-directory p) '(:relative "dir")))))`);
for (const form of ['(pathname-name "x" :case :bad)', '(pathname-host 42)',
  '(pathname-type "x" :unknown t)', '(pathname-directory "x" :case :local :unknown t)',
  '(make-pathname :directory 42)', '(make-pathname :version -1)']) {
  check(`(handler-case (progn ${form} nil) (error () t))`);
}
console.log('Physical pathnames: components, customary case, defaults, wildcards, readable printing, copying, and EQUAL hashes pass.');
