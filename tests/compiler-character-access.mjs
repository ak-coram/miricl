import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

// Run with a sealed precompiled image: redefining CHAR/CHAR-CODE must not
// replace live dependencies of a source compiler while it emits these tests.
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const sourceCompiler = process.argv.includes('--source');
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
  for (const [safety, inline] of [[0, ''], [3, ''], [0, '(inline char char-code)'], [3, '(inline char char-code)']]) {
    check(`(progn
      (defparameter *character-read* (miricl-compiler:compile-native
        '(lambda (s i) (declare (optimize (safety ${safety})) ${inline}) (values (char s i)))))
      (defparameter *character-code* (miricl-compiler:compile-native
        '(lambda (c) (declare (optimize (safety ${safety})) ${inline}) (values (char-code c)))))
      (and (eql (funcall *character-read* "abc" 1) #\\b)
           (= (funcall *character-code* #\\a) 97)
           (= (funcall *character-code* (code-char 955)) 955)
           (equal (multiple-value-list (funcall *character-code* #\\a)) '(97))))`);
    check(`(let* ((backing (copy-seq "abλdef"))
                  (view (make-array 4 :element-type 'character :displaced-to backing
                           :displaced-index-offset 1 :fill-pointer 1 :adjustable t)))
      (and (eql (funcall *character-read* view 3) #\\e)
           (progn (setf (char backing 2) #\\z)
                  (eql (funcall *character-read* view 1) #\\z))))`);
    check(`(and (handler-case (progn (funcall *character-code* 42) nil) (type-error () t))
       (handler-case (progn (funcall *character-read* "a" -1) nil) (type-error () t))
       (handler-case (progn (funcall *character-read* "a" 1) nil) (type-error () t))
       (handler-case (progn (funcall *character-read* "a" (ash 1 100)) nil) (type-error () t))
       (handler-case (progn (funcall *character-read* 42 0) nil) (type-error () t))
       (= (funcall *character-code* (funcall *character-read* "ab" 1)) 98))`);
  }
  check(`(let ((events nil)
        (f (miricl-compiler:compile-native '(lambda (s i callback)
             (values (char (funcall callback :string s) (funcall callback :index i)))))))
    (and (eql (funcall f "ab" 1 (lambda (event value) (push event events) (values value :extra))) #\\b)
         (equal (reverse events) '(:string :index))))`);
  // The optimized primary calls and ordinary full-value calls must each
  // honor replacement definitions, with no stale type or value-count facts.
  for (const [operator, parameters, args] of sourceCompiler ? [] : [
    ['char', '(s i)', '"ab" 1'], ['char-code', '(c)', '#\\a'],
  ]) {
    check(`(progn
      (defparameter *character-primary* (miricl-compiler:compile-native
        '(lambda ${parameters} (values (${operator} ${parameters.slice(1, -1)})))))
      (defparameter *character-full* (miricl-compiler:compile-native
        '(lambda ${parameters} (${operator} ${parameters.slice(1, -1)}))))
      (let ((original (symbol-function '${operator})))
        (unwind-protect
          (progn (setf (symbol-function '${operator})
                   (lambda (&rest arguments) (declare (ignore arguments)) (values :replacement :extra)))
             (and (equal (multiple-value-list (funcall *character-primary* ${args})) '(:replacement))
                  (equal (multiple-value-list (funcall *character-full* ${args})) '(:replacement :extra))))
          (setf (symbol-function '${operator}) original))))`);
  }
} finally {vm.close();}
console.log(sourceCompiler
  ? 'Character source compiler: Unicode, array views, checked errors/recovery and evaluation order pass; sealed redefinition suite runs separately.'
  : 'Character access: Unicode, array views, checked errors/recovery, evaluation order and redefinition pass.');
