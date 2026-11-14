import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const parseContracts = process.argv.includes('--parse-contracts');
const sourceCompiler = process.argv.includes('--source') && !parseContracts;
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const quote = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
const check = source => assert.equal(vm.evaluate(parseContracts
  ? `(with-input-from-string (in ${quote(source)})
       (let ((end (gensym))) (loop for form = (read in nil end) until (eq form end) finally (return t))))`
  : source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('hir-types.lisp', 'utf8'));
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (value)
        (declare (optimize (safety ${safety})) (inline characterp)) (characterp value)))))
      (every (lambda (value) (equal (multiple-value-list (funcall f value)) (list (characterp value))))
        '(nil t 0 1267650600228229401496703205376 #\\a "a" #(a) (a))))`);
    check(`(let ((f (miricl-compiler:compile-native '(lambda (value)
        (declare (optimize (safety ${safety})) (inline zerop)) (zerop value)))))
      (and (every (lambda (value) (equal (multiple-value-list (funcall f value)) (list (zerop value))))
              '(0 1 -1 0.0f0 0.0d0 1/2 #c(0 1) #c(1 2) 1267650600228229401496703205376))
           (handler-case (progn (funcall f :bad) nil) (type-error () t))))`);
    for (const operator of ['<', '>', '<=', '>=']) {
      check(`(let* ((cases '((0 0) (-1 1) (1 -1) (1/2 0.5d0)
                   (1267650600228229401496703205376 1267650600228229401496703205375)
                   (-1267650600228229401496703205376 -1267650600228229401496703205375)))
              (expected (mapcar (lambda (args) (apply #'${operator} args)) cases))
              (f (miricl-compiler:compile-native '(lambda (x y)
                   (declare (optimize (safety ${safety})) (inline ${operator})) (${operator} x y)))))
        (and (every (lambda (args result) (equal (multiple-value-list (apply f args)) (list result))) cases expected)
             (handler-case (progn (funcall f 0 :bad) nil) (type-error () t))
             (handler-case (progn (funcall f #c(1 2) 1) nil) (type-error () t))))`);
    }
    for (const operator of ['mod', 'truncate']) {
      for (const primary of [false, true]) {
        check(`(let* ((cases '((17 5) (-17 5) (17 -5) (-17 -5) (0 1)
               (-1073741824 -1) (1073741823 -1073741824)
               (1267650600228229401496703205376 7)
               (-1267650600228229401496703205376 -7)
               (17/2 3/2) (-17/2 3/2) (17.0f0 5.0d0)))
              (expected (mapcar (lambda (args) (multiple-value-list
                ${primary ? `(values (apply #'${operator} args))` : `(apply #'${operator} args)`})) cases))
              (f (miricl-compiler:compile-native '(lambda (x y)
                   (declare (optimize (safety ${safety})) (inline ${operator}))
                   ${primary ? `(values (${operator} x y))` : `(${operator} x y)`}))))
          (every (lambda (args wanted) (equalp wanted (multiple-value-list (apply f args)))) cases expected))`);
      }
      check(`(let ((f (miricl-compiler:compile-native '(lambda (x y)
          (declare (optimize (safety ${safety})) (inline ${operator}))
          (values (${operator} x y))))))
        (and (handler-case (progn (funcall f 1 0) nil) (division-by-zero () t))
             (handler-case (progn (funcall f :bad 1) nil) (type-error () t))
             (handler-case (progn (funcall f 1 :bad) nil) (type-error () t))
             (handler-case (progn (funcall f #c(1 2) 1) nil) (type-error () t))
             (= (funcall f 17 5) ${operator === 'mod' ? 2 : 3})))`);
    }
    check(`(let ((f (miricl-compiler:compile-native '(lambda (code)
        (declare (optimize (safety ${safety})) (inline code-char)) (code-char code)))))
      (and (char= #\\A (funcall f 65)) (null (funcall f 55296))
           (= 1114111 (char-code (funcall f 1114111)))
           (every (lambda (code) (handler-case (progn (funcall f code) nil) (type-error () t)))
                  '(-1 1114112 :bad 1/2)) (char= #\\B (funcall f 66))))`);
    check(`(let* ((f (miricl-compiler:compile-native '(lambda (value string index)
          (declare (optimize (safety ${safety})) (inline (setf char)))
          (funcall #'(setf char) value string index))))
         (flat (make-array 4 :element-type 'character :initial-element #\\a))
         (view (make-array 2 :element-type 'character :displaced-to flat :displaced-index-offset 1))
         (fill (make-array 4 :element-type 'character :fill-pointer 1 :initial-element #\\a)))
      (and (equal (multiple-value-list (funcall f #\\z flat 0)) '(#\\z))
           (char= #\\x (funcall f #\\x view 1)) (char= #\\x (char flat 2))
           (char= #\\y (funcall f #\\y fill 3)) (= 1 (length fill)) (char= #\\y (char fill 3))
           (handler-case (progn (funcall f :bad flat 0) nil) (type-error () t))
           (handler-case (progn (funcall f #\\x flat 4) nil) (type-error () t))
           (handler-case (progn (funcall f #\\x '(a) 0) nil) (type-error () t))
           (char= #\\q (funcall f #\\q flat 1))))`);
    check(`(let ((f (miricl-compiler:compile-native '(lambda (size)
          (declare (type integer size) (optimize (safety ${safety})) (inline make-array))
          (make-array size :element-type 'character)))))
      (and (simple-string-p (funcall f 3)) (= 3 (length (funcall f 3)))
           (not (eq (funcall f 0) (funcall f 0)))
           (handler-case (progn (funcall f -1) nil) (type-error () t))
           (string= (funcall f 0) "")))`);
  }
  for (const operator of ['mod', 'truncate']) {
    for (const primary of [false, true]) {
      check(`(let ((f (miricl-compiler:compile-native '(lambda (x y)
          (declare (type (integer 0 9223372036854775807) x)
                   (type (integer 1 9223372036854775807) y)
                   (optimize (safety 0)) (inline ${operator}))
          ${primary ? `(values (${operator} x y))` : `(${operator} x y)`}))))
        (every (lambda (args) (equal (multiple-value-list (apply f args))
          (multiple-value-list ${primary ? `(values (apply #'${operator} args))` : `(apply #'${operator} args)`})))
          '((0 1) (17 5) (9223372036854775807 1) (9223372036854775807 1073741824))))`);
    }
  }
  check(`(let* ((f (miricl-compiler:compile-native '(lambda (value string index)
       (declare (type character value) (type simple-string string) (type (signed-byte 64) index)
                (optimize (safety 0)) (inline (setf char)))
       (funcall #'(setf char) value string index))))
      (string (make-array 3 :element-type 'character :initial-element #\\a)))
    (and (char= #\\z (funcall f #\\z string 1)) (string= string "aza")
         (every (lambda (index) (handler-case (progn (funcall f #\\x string index) nil) (type-error () t)))
                '(-1 3 9223372036854775807))
         (handler-case (progn (funcall f #\\x (make-array 0 :element-type nil) 0) nil) (type-error () t))
         (char= #\\q (funcall f #\\q string 0))))`);
  check(`(let ((events nil)
        (f (miricl-compiler:compile-native '(lambda (callback flag)
          (declare (inline mod truncate))
          (if flag (values (mod (funcall callback :left -17) (funcall callback :right 5))
                           (truncate (funcall callback :numerator -17) (funcall callback :divisor 5))) :skipped)))))
    (and (eq :skipped (funcall f (lambda (&rest args) (declare (ignore args)) (error "unreachable")) nil))
         (equal (multiple-value-list (funcall f (lambda (event value) (push event events) (values value :extra)) t)) '(3 -3))
         (equal (reverse events) '(:left :right :numerator :divisor))))`);
  // INLINE must reach private callbacks, including compiler byte-to-string
  // conversion, so sealed callers retain the canonical dependency.
  check(`(let ((f (miricl-compiler:compile-native '(lambda (codes)
        (declare (inline code-char)) (map 'string (lambda (code) (code-char code)) codes))))
        (original (symbol-function 'code-char)))
    (unwind-protect
      (progn (setf (symbol-function 'code-char) (lambda (code) (declare (ignore code)) :replacement))
             (string= (funcall f '(65 66)) "AB"))
      (setf (symbol-function 'code-char) original)))`);
  if (!sourceCompiler) {
    for (const [operator, args] of [['mod', '17 5'], ['truncate', '17 5'], ['code-char', '65'], ['<', '17 5'], ['>', '17 5'], ['<=', '17 5'], ['>=', '17 5']]) {
      const parameters = operator === 'code-char' ? 'x' : 'x y';
      check(`(let* ((expected (values (${operator} ${args})))
            (ordinary (miricl-compiler:compile-native '(lambda (${parameters}) (${operator} ${parameters}))))
            (blocked (miricl-compiler:compile-native '(lambda (${parameters}) (declare (notinline ${operator})) (${operator} ${parameters}))))
            (captured (miricl-compiler:compile-native '(lambda (${parameters}) (declare (inline ${operator})) (values (${operator} ${parameters})))))
            (original (symbol-function '${operator})))
        (unwind-protect
          (progn (setf (symbol-function '${operator}) (lambda (&rest args) (declare (ignore args)) (values :replacement :extra)))
            (and (equal (multiple-value-list (funcall ordinary ${args})) '(:replacement :extra))
                 (equal (multiple-value-list (funcall blocked ${args})) '(:replacement :extra))
                 (eql expected (funcall captured ${args}))
                 (let ((fresh (miricl-compiler:compile-native '(lambda (${parameters}) (declare (inline ${operator})) (${operator} ${parameters})))))
                   (equal (multiple-value-list (funcall fresh ${args})) '(:replacement :extra)))))
          (setf (symbol-function '${operator}) original)))`);
    }
  }
} finally { vm.close(); }
console.log(parseContracts ? 'Symbol primitive test forms parse.'
  : `Symbol primitives: numeric values, multiple values, errors, allocation, character stores and effects pass (${sourceCompiler ? 'source' : 'sealed'} compiler).`);
