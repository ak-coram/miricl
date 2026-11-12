// Development facilities must work without host Lisp or WebAssembly imports.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const module = await WebAssembly.compile(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
assert.deepEqual(WebAssembly.Module.imports(module), []);
const vm = new Miricl(await WebAssembly.instantiate(module));
const output = source => {
  const result = vm.evaluate(source, {eof: true});
  assert.equal(result.incomplete, false);
  assert.equal(result.quit, false);
  return result.output;
};
const check = (source, expected) => assert.equal(output(source), `${expected}\n`, source);
const fails = source => assert.throws(() => output(source), MiriclError);

// A primitive call returns its own values, not extra values from arguments.
check("(multiple-value-list (car (values '(17 19) :discard)))", '(17)');
check('(multiple-value-list (+ (values 17 :discard) (values 25 :discard)))', '(42)');
check('(multiple-value-list (identity (values)))', '(NIL)');
check("(multiple-value-list (floor 17 5))", '(3 2)');
check(`(let ((table (make-hash-table))) (setf (gethash :key table) 17)
  (multiple-value-list (gethash :key table)))`, '(17 T)');
check(`(let ((original (symbol-function 'identity)))
  (unwind-protect
    (progn (setf (symbol-function 'identity) (lambda (x) (values x :replacement)))
      (multiple-value-list (identity 17)))
    (setf (symbol-function 'identity) original)))`, '(17 :REPLACEMENT)');
check('(handler-case (car 1 2) (program-error () :arity))', ':ARITY');

check('(list (string-equal "ÉΛЖ𐐀🙂" "éλж𐐨🙂") (string-equal \'abc "ABC") (string-equal #\\a "A"))',
  '(T T T)');
check('(list (string-equal "prefixABCtail" "abc" :start1 6 :end1 9) (string-equal "a" "ab"))', '(T NIL)');
check('(length (multiple-value-list (string-equal "a" "A")))', '1');
check('(handler-case (string-equal "a" "abc" :end1 2) (type-error () :bounds))', ':BOUNDS');
check('(list (string< "abc" "abd") (string<= "abc" "abc") (string>= "abc" "ab") (string/= "ab" "abc"))',
  '(2 3 2 2)');
check('(list (string-not-equal "ÉΛ𐐀" "éλ𐐨") (string-not-greaterp "ÉΛ𐐀" "éλ𐐨") (string-greaterp "aZ" "Ay"))',
  '(NIL 3 1)');
check('(string= "prefixabctail" "abc" :start1 6 :end1 9)', 'T');
check(`(let ((original (symbol-function 'string<)))
  (unwind-protect
    (progn (setf (symbol-function 'string<) (lambda (a b) :replacement))
      (list (string< "a" "b") (funcall original "a" "b")))
    (setf (symbol-function 'string<) original)))`, '(:REPLACEMENT 0)');

check("(list (boundp 'cl:*compile-file-pathname*) (boundp 'cl:*compile-file-truename*) cl:*compile-file-pathname* cl:*compile-file-truename*)", '(T T NIL NIL)');
vm.mountFile('compile-paths.lisp', `(defparameter *compile-path-observation*
  (list *compile-file-pathname* *compile-file-truename*
        (namestring (or *compile-file-truename* *load-truename*))))`);
check('(load "compile-paths.lisp") (list *compile-path-observation* *compile-file-pathname* *compile-file-truename*)',
  'T\n((NIL NIL "/compile-paths.lisp") NIL NIL)');
check('(namestring (compile-file-pathname "src/code.lisp"))', '"src/code.wasm"');
check('(namestring (compile-file-pathname #P"src/code.lisp" :output-file #P"/build/"))',
  '"/build/code.wasm"');
check('(namestring (compile-file-pathname "src/code.lisp" :output-file "build/" :verbose t))',
  '"src/build/code.wasm"');
check('(let* ((fn (compile-file-pathname "init.lsp")) (type (pathname-type fn))) (list (pathnamep fn) type))',
  '(T "wasm")');
vm.mountFile('directory-check/a.lisp', '');
vm.mountFile('directory-check/sub/b.lisp', '');
check('(mapcar #\'namestring (directory #P"directory-check/*/*.lisp"))',
  '("/directory-check/sub/b.lisp")');
check(`(let ((*default-pathname-defaults* #P"directory-check/"))
  (list (length (directory (make-pathname :name :wild :type "lisp")))
        (length (directory (make-pathname :directory '(:relative :wild-inferiors)
                                          :name :wild :type "lisp")))
        (every (lambda (p) (equal p (truename p))) (directory "**/*.*"))))`, '(1 2 T)');

check('(progn (defmacro twice (x) `(list ,x ,x)) (twice (+ 1 2)))', '(3 3)');
check("(multiple-value-list (macroexpand-1 '(twice 7)))", '((LIST 7 7) T)');
check("(multiple-value-list (macroexpand '42))", '(42 NIL)');
check('(progn (defmacro nothing () nil) (multiple-value-list (macroexpand-1 \'(nothing))))', '(NIL T)');
check('(let ((value 9)) (defmacro captured () value)) (captured)', 'CAPTURED\n9');
check('(macrolet ((m (x) `(+ ,x 1))) (m 5))', '6');
check('(progn (defmacro m () 10) (flet ((m () 20)) (m)))', '20');
check('(macrolet ((m () 10)) (flet ((m () 20)) (m)))', '20');
check('(labels ((fact (n) (if (zerop n) 1 (* n (fact (1- n)))))) (fact 8))', '40320');
check('(flet (((setf local) (new cell) (rplaca cell new))) (let ((x (list 1))) (setf (local x) 8) x))', '(8)');
check(`(progn
  (defmacro in-env (form &environment env) (macroexpand form env))
  (macrolet ((m () 42)) (in-env (m))))`, '42');
check(`(progn
  (defmacro options ((a &optional (b 2 bp)) &key ((:value (c d)) '(3 4)))
    (list 'quote (list a b bp c d)))
  (options (1) :value (5 6)))`, '(1 2 NIL 5 6)');
check(`(progn
  (defmacro whole (&whole form &optional (value (length form))) (list 'quote value))
  (whole))`, '1');
check('(destructuring-bind (a (b &rest c) . tail) \'(1 (2 3 4) 5 6) (list a b c tail))', '(1 2 (3 4) (5 6))');
check('(destructuring-bind (() x) \'(() 7) x)', '7');
check(`(macrolet ((m ((&key a) &key (b a supplied))
  (list 'quote (list a b supplied))))
  (list (m (:a 3)) (m (:a 3) :b nil) (m (:a 3) :b 4 :b 5)))`,
  '((3 3 NIL) (3 NIL T) (3 4 T))');
check(`(let ((calls 0))
  (destructuring-bind ((&optional (a (incf calls))) &optional (b (+ a (incf calls)) bp)
                      &key (c (+ b (incf calls)) cp) &aux (d (+ a b c)))
      '(())
    (list a b bp c cp d calls)))`, '(1 3 NIL 6 NIL 10 3)');
check(`(let ((calls 0))
  (destructuring-bind ((a) &optional (b (incf calls) bp) &key (c (+ a b) cp))
      '((2) nil :c nil) (list a b bp c cp calls)))`, '(2 NIL T NIL T 0)');
check(`(let ((calls 0)) (list (loop repeat (progn (incf calls) -1.5) collect :bad) calls))`, '(NIL 1)');
check(`(handler-case (loop repeat 1.5 collect :bad) (type-error () :count))`, ':COUNT');
check(`(list (loop for x from 1 to 5 do (progn) finally (return x))
  (loop for x from 1 below 5 do (progn) finally (return x))
  (loop for x from 10 downto 0 do (progn) finally (return x))
  (loop for x from 10 above 0 do (progn) finally (return x)))`, '(5 4 0 1)');
check(`(loop for x from 1d0 to 2d0 by 1d300 do (progn) finally (return x))`, '1.0d0');
check(`(loop for x of-type fixnum from most-positive-fixnum to most-positive-fixnum
  do (progn) finally (return (= x most-positive-fixnum)))`, 'T');
check(`(let ((calls 0))
  (list (loop for x from 1 to (progn (incf calls) 5) by (progn (incf calls) 2)
    do (progn) finally (return x)) calls))`, '(5 2)');
fails('(destructuring-bind (a b) \'(1) (list a b))');
check(`(progn
  (setf (macro-function 'installed) (lambda (form env) (list 'quote (cdr form))))
  (installed a b))`, '(A B)');
check('(progn (fmakunbound \'installed) (list (fboundp \'installed) (macro-function \'installed)))', '(NIL NIL)');
check('(progn (defmacro replaced () 1) (defun replaced () 2) (replaced))', '2');
check('(progn (defmacro changed () 1) (setf (symbol-function \'changed) (lambda () 2)) (changed))', '2');
check('(let ((calls 0) (x (list 1))) (list (incf (car (progn (incf calls) x)) 2) calls x))', '(3 1 (3))');
check('(let ((calls 0) (x (list (list 1 2)))) (list (pop (car (progn (incf calls) x))) calls x))', '(1 1 ((2)))');
check('(let ((x 2) (ys \'(3 4))) `(1 ,x ,@ys . 5))', '(1 2 3 4 . 5)');
check('#+miricl 1 #-miricl unknown-package:ignored #+(and miricl (not sbcl)) 2', '1\n2');
check('(list #+missing ignored #-missing 42)', '(42)');
check('#.(+ 20 22)', '42');
fails('(let ((*read-eval* nil)) (read-from-string "#.(+ 20 22)"))');
check('(read-from-string "(+ 1 2)")', '(+ 1 2)\n7');
check('(read-from-string "" nil :eof)', ':EOF\n0');
fails(',outside');
check('(let ((table (make-hash-table :test \'equal))) (setf (gethash "a" table) nil) (list (consp table) (typep table \'hash-table) (multiple-value-list (gethash "a" table :missing)) (gethash "b" table :missing) (hash-table-count table) (remhash "a" table) (hash-table-count table)))',
  '(NIL T (NIL T) :MISSING 1 T 0)');
check('(list (pathnamep #P"source.lisp") (typep #P"source.lisp" \'standard-object) (equal #P"source.lisp" (pathname "source.lisp")))', '(T NIL T)');
check('(eval-when (:load-toplevel) :bad)', 'NIL');
check('(eval-when (:execute) (values 3 4))', '3\n4');
check('(progn (declaim (special *declared*)) (let ((*declared* 42)) (eval \'*declared*)))', '42');

vm.mountFile('library/inner.lisp', '(defparameter *loaded* (namestring *load-pathname*))');
check('(list (namestring (truename "library/inner.lisp")) (probe-file "missing.lisp"))', '("/library/inner.lisp" NIL)');
fails('(truename "missing.lisp")');
vm.mountFile('library/outer.lisp', `
  (defpackage :loaded-package (:use :cl))
  (in-package :loaded-package)
  (defmacro loaded-macro (x) (list '+ x 1))
  (defparameter answer (loaded-macro 41))
  (load "inner.lisp")
  (defparameter outer-path (namestring *load-truename*))`);
check('(let ((*default-pathname-defaults* "library/")) (load "outer.lisp"))', 'T');
check('(list loaded-package::answer loaded-package::*loaded* loaded-package::outer-path (package-name *package*) *load-pathname*)',
  '(42 "library/inner.lisp" "/library/outer.lisp" "COMMON-LISP-USER" NIL)');
vm.mountFile('broken.lisp', '(defparameter *partial-load* 7) (error "load failed")');
check('(handler-case (load "broken.lisp") (error () :caught)) (+ 1 2)', ':CAUGHT\n3');
check('(list *partial-load* *load-pathname* *load-truename*)', '(7 NIL NIL)');
vm.mountFile('incomplete.lisp', '(defparameter *partial-load* 8) (');
fails('(load "incomplete.lisp")');
check('*partial-load*', '8');
check('(load "missing.lisp" :if-does-not-exist nil)', 'NIL');
check('(handler-case (load "missing.lisp") (file-error (e) (namestring (file-error-pathname e))))', '"missing.lisp"');
const literal = 'quotes: " and slash: \\';
vm.mountFile('literal.lisp', `(defparameter *literal* ${JSON.stringify(literal)})`);
check('(load "literal.lisp") *literal*', `T\n${JSON.stringify(literal)}`);
check('(load #P"library/../missing.lisp" :if-does-not-exist nil)', 'NIL');
// List operations: identity and sharing are observable even when contents match.
check("(list (butlast '(1 2 3 . 4)) (butlast '(1 2 3 . 4) 0))", '((1 2) (1 2 3))');
// Counts cross immediate, boxed i64 and bignum representations without
// changing saturation or the oversized NBUTLAST no-mutation behavior.
check(`(every (lambda (n)
          (let ((x (list 1 2 3)))
            (and (null (butlast x n)) (null (nbutlast x n))
                 (equal x '(1 2 3)))))
        '(3 1073741823 1073741824 2147483647 2147483648
          9223372036854775807 9223372036854775808))`, 'T');
for (const count of ['-1', '-1073741824', '-1073741825', '-9223372036854775809', '1.0', '1/2', 'nil'])
  check(`(handler-case (butlast '(1 2 3) ${count})
    (type-error (c) (and (eql (type-error-datum c) ${count})
      (not (typep (type-error-datum c) (type-error-expected-type c))))))`, 'T');
check("(butlast '(1 2 3) 1)", '(1 2)');
check(`(let* ((x (list 1 2 3)) (last (cddr x)) (tail (list 4 5))
               (result (nreconc x tail)))
         (list (eq result last) (eq (cdddr result) tail) result))`, '(T T (3 2 1 4 5))');
check(`(let* ((x (list 1 2 3)) (tail (list 4 5)) (result (revappend x tail)))
         (list (eq (cdddr result) tail) result x))`, '(T (3 2 1 4 5) (1 2 3))');
check("(list (nthcdr 1000000000000000000000000000000 '(1 2 3)) (nthcdr 3 '(1 2 3 . 4)))", '(NIL 4)');
check(`(let* ((key (list 1)) (value (list 2)) (x (list (cons key value) nil))
               (result (copy-alist x)))
         (list (not (eq x result)) (not (eq (car x) (car result)))
               (eq (caar result) key) (eq (cdar result) value) (cadr result)))`, '(T T T T NIL)');
check("(list (make-list 0) (make-list 3) (make-list 3 :initial-element 7 :initial-element 8))", '(NIL (NIL NIL NIL) (7 7 7))');
check(`(let* ((value (list 1)) (x (make-list 3 :initial-element value)))
         (list (eq (first x) value) (eq (second x) value) (eq (third x) value)))`, '(T T T)');
check(`(let* ((key (list 1)) (value (list 2)) (tail (list (cons 3 4)))
               (result (pairlis (list key) (list value) tail)))
         (list (eq (caar result) key) (eq (cdar result) value) (eq (cdr result) tail) tail))`, '(T T T ((3 . 4)))');
check(`(let* ((x (list* 1 2 3 4)) (prefix (ldiff x (cddr x))) (copy (ldiff x nil)))
         (list prefix copy x (eq copy x) (eq (cdr copy) (cdr x))))`, '((1 2) (1 2 3 . 4) (1 2 3 . 4) NIL NIL)');
check("(ldiff (list* 1 2 (1- 18000000000001)) 18000000000000)", '(1 2)');
check("(maplist (lambda (x y) (list (car x) (car y))) '(1 2 3) '(4 5))", '((1 4) (2 5))');
check(`(let* ((x (list 1 2 3)) (tails (maplist #'identity x)))
         (list (eq (first tails) x) (eq (second tails) (cdr x))
               (maplist (lambda (tail) (values (car tail) 99)) x)))`, '(T T (1 2 3))');
check(`(let ((x (list 1 2 3)) (seen nil))
         (list (eq (mapl (lambda (xt yt) (push (list (car xt) (car yt)) seen) (values 99 100)) x '(4 5)) x)
               seen))`, '(T ((2 5) (1 4)))');
check("(let ((x (list 1))) (eq (mapl (lambda (&rest tails) (error \"called\")) x nil) x))", 'T');
for (const form of [
  "(butlast '(1 2) -1)", "(butlast 1)", "(nthcdr -1 '(1 2))", "(nthcdr 2 '(1 . 2))",
  "(nreconc (cons 1 2) nil)", "(revappend '(1 . 2) nil)", "(copy-alist '((1 . 2) . 3))",
  "(make-list -1)", "(make-list 1.0)", "(pairlis '(1 . 2) '(3 . 4))", "(ldiff 1 1)",
  "(maplist #'identity '(1 . 2))", "(mapl #'identity '(1 . 2))",
]) {
  check(`(handler-case ${form} (type-error (c)
    (not (typep (type-error-datum c) (type-error-expected-type c)))))`, 'T');
}
for (const form of [
  '(nreconc nil)', '(revappend nil nil nil)', '(make-list 2 :bad t)', '(make-list 2 :initial-element)',
  '(pairlis nil)', '(ldiff nil)', '(maplist)', "(maplist #'identity)", "(mapl #'identity)",
]) {
  check(`(handler-case ${form} (program-error () t))`, 'T');
}
// HyperSpec regressions from the sequence runtime review.
check('(list (reverse "λ🙂") (nreverse (copy-seq "abc")))', '("🙂λ" "cba")');
check('(let* ((s (copy-seq "abcd")) (r (reverse s))) (list s r (eq s r)))', '("abcd" "dcba" NIL)');
check('(let* ((s (copy-seq "abc")) (r (nreverse s))) (list s r (eq s r)))', '("cba" "cba" T)');
check("(list (count 2 '(1 2 2) :key nil) (count 2 '(1 2 3 2 4) :start 1 :end 4 :test-not #'eql))", '(2 1)');
check("(let ((seen nil)) (count-if (lambda (x) (push x seen) t) '(1 2 3) :from-end t) seen)", '(1 2 3)');
check("(let ((seen nil)) (count 2 '(1 2 3 4 5) :start 1 :end 4 :from-end t :key (lambda (x) (push x seen) x)) seen)", '(2 3 4)');
check('(let ((seen nil)) (count-if-not (lambda (x) (push x seen) nil) "abcd" :start 1 :end 3 :from-end t) seen)', '(#\\b #\\c)');
check("(multiple-value-list (count-if (lambda (x) (values x 99)) '(1 nil 2)))", '(2)');
for (const operator of ['mapcar', 'mapc', 'mapcan', 'maplist', 'mapl', 'every', 'some']) {
  const value = operator === 'some' ? 'nil' : operator === 'mapcan' ? '(list arg)' : 't';
  check(`(progn
    (defparameter *mapping-seen* nil)
    (defun mapping-changing (arg)
      (setf (symbol-function 'mapping-changing) (lambda (arg) (push 99 *mapping-seen*) t))
      (push (if (consp arg) (car arg) arg) *mapping-seen*) ${value})
    (${operator} 'mapping-changing '(1 2 3))
    (reverse *mapping-seen*))`, '(1 2 3)');
}
check('(list (fresh-line) (fresh-line))', '(NIL NIL)');
check('(progn (write-string "x") (list (fresh-line) (fresh-line)))', 'x\n(T NIL)');
check(`(format nil (lambda (s) (let ((a (fresh-line s)))
  (write-string "x" s)
  (let ((b (fresh-line s)) (c (fresh-line s))) (format s "~S" (list a b c))))))`, '"x\n(NIL T NIL)"');
for (const form of ['(car 1)', '(cdr 1)', "(cadr '(1 . 2))", '(fresh-line 1)',
  "(count 1 '(1 2) :start -1)", "(count 1 '(1 2) :end 3)", "(count 1 '(1 2) :start 2 :end 1)"]) {
  check(`(handler-case ${form} (type-error (c)
    (not (typep (type-error-datum c) (type-error-expected-type c)))))`, 'T');
}
console.log('Standalone development, native list/sequence, and review regression checks pass.');
