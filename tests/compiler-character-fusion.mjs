import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const sourceCompiler = process.argv.includes('--source');
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = form => assert.equal(vm.evaluate(form).output, 'T\n', form);
const quoteLisp = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    check(`(progn
      (defparameter *fused-read* (miricl-compiler:compile-native
        '(lambda (s i) (declare (optimize (safety ${safety}) (debug 0)) (inline char char-code))
           (char-code (char s i)))))
      (and (= 98 (funcall *fused-read* "abc" 1))
           (= 955 (funcall *fused-read* "aλ🙂" 1))
           (= 128578 (funcall *fused-read* "aλ🙂" 2))
           (equal (multiple-value-list (funcall *fused-read* "a" 0)) '(97))))`);
    check(`(let* ((backing (copy-seq "aλ🙂def"))
                  (view (make-array 4 :element-type 'character :displaced-to backing
                           :displaced-index-offset 1 :fill-pointer 0 :adjustable t)))
      (and (= 101 (funcall *fused-read* view 3))
           (progn (setf (char backing 2) #\\z)
                  (= 122 (funcall *fused-read* view 1)))))`);
    check(`(and (handler-case (progn (funcall *fused-read* 42 0) nil) (type-error () t))
       (handler-case (progn (funcall *fused-read* "a" -1) nil) (type-error () t))
       (handler-case (progn (funcall *fused-read* "a" 1) nil) (type-error () t))
       (handler-case (progn (funcall *fused-read* "a" (ash 1 100)) nil) (type-error () t))
       (handler-case (progn (funcall *fused-read* "a" :bad) nil) (type-error () t))
       (= 98 (funcall *fused-read* "ab" 1)))`);
  }
  check(`(let ((f (miricl-compiler:compile-native
      '(lambda (s n)
         (declare (type (integer -1073741824 1073741824) n)
                  (optimize (safety 0) (debug 0)) (inline + char char-code))
         (char-code (char s (+ n 1)))))))
    (and (= 98 (funcall f "ab" 0))
         (handler-case (progn (funcall f "ab" -2) nil)
           (type-error (c) (= -1 (type-error-datum c))))
         (handler-case (progn (funcall f "ab" 1073741824) nil)
           (type-error (c) (= 1073741825 (type-error-datum c))))
         (= 97 (funcall f "ab" -1))))`);
  check(`(let ((f (miricl-compiler:compile-native
      '(lambda (s i callback)
         (declare (optimize (debug 0)) (inline char char-code))
         (char-code (char (funcall callback :string s) (funcall callback :index i))))))
        (events nil))
    (and (= 98 (funcall f "ab" 1 (lambda (event value) (push event events) (values value :extra))))
         (equal (reverse events) '(:string :index))))`);
  // A fused successful code is a bounded integer even though its original
  // producer's HIR value denoted a character. Exercise raw arithmetic, shared
  // boxed uses, control-flow joins and escaping integer captures together.
  for (const safety of [0, 3]) {
    check(`(let ((f (miricl-compiler:compile-native
       '(lambda (s i flag)
          (declare (optimize (safety ${safety}) (debug 0))
                   (inline char char-code + - <))
          (let ((code (char-code (char s i))))
            (values (+ code 1) (if flag code (- code 1))
                    (list code code) (lambda () code)))))))
      (multiple-value-bind (next branch pair closure) (funcall f "aλ🙂" 2 t)
        (and (= next 128579) (= branch 128578)
             (equal pair '(128578 128578)) (= (funcall closure) 128578)
             (equal (subseq (multiple-value-list (funcall f "λ" 0 nil)) 0 3)
                    '(956 954 (955 955))))))`);
    check(`(let ((f (miricl-compiler:compile-native
       '(lambda (s flag)
          (declare (optimize (safety ${safety}) (debug 0)) (inline char char-code +))
          (+ (if flag (char-code (char s 0)) (char-code (char s 1))) 7)))))
      (and (= 962 (funcall f "λ🙂" t)) (= 128585 (funcall f "λ🙂" nil))))`);
    check(`(let ((f (miricl-compiler:compile-native
       '(lambda (s i)
          (declare (optimize (safety ${safety}) (debug 0)) (inline char char-code))
          (char-code (char s i)) :done))))
      (and (handler-case (progn (funcall f "a" -1) nil) (type-error () t))
           (eq :done (funcall f "a" 0))))`);
  }
  // Read and error timing must remain at CHAR, before an intervening effect.
  check(`(let ((f (miricl-compiler:compile-native
      '(lambda (s i callback)
         (declare (optimize (debug 0)) (inline char char-code))
         (let ((character (char s i)))
           (funcall callback s)
           (char-code character)))))
        (called nil))
    (and (= 97 (funcall f (copy-seq "ab") 0 (lambda (s) (setf (char s 0) #\\z))))
         (handler-case (progn (funcall f "ab" 3 (lambda (s) (declare (ignore s)) (setq called t))) nil)
           (type-error () (null called)))))`);
  // A separately observed/captured character retains its character value.
  for (const debug of [0, 3]) {
    check(`(let ((f (miricl-compiler:compile-native
       '(lambda (s) (declare (optimize (debug ${debug})) (inline char char-code))
          (let ((character (char s 0)))
            (values character (char-code character) (lambda () character)))))))
      (multiple-value-bind (character code closure) (funcall f "λ")
        (and (characterp character) (eql character #\\λ) (= code 955)
             (eql character (funcall closure)))))`);
  }
  const redefinitions = [
    `(progn
      (defparameter *fusion-ordinary* (miricl-compiler:compile-native
        '(lambda (s i) (declare (optimize (debug 0)) (inline char-code)) (char-code (char s i)))))
      (let ((original (symbol-function 'char)))
        (unwind-protect
          (progn (setf (symbol-function 'char)
                   (lambda (&rest args) (declare (ignore args)) (values #\\z :extra)))
             (= 122 (funcall *fusion-ordinary* "a" 0)))
          (setf (symbol-function 'char) original))))`,
    `(progn
      (defparameter *fusion-ordinary-code* (miricl-compiler:compile-native
        '(lambda (s i) (declare (optimize (debug 0)) (inline char)) (char-code (char s i)))))
      (let ((original (symbol-function 'char-code)))
        (unwind-protect
          (progn (setf (symbol-function 'char-code)
                   (lambda (&rest args) (declare (ignore args)) (values :replacement :extra)))
             (equal (multiple-value-list (funcall *fusion-ordinary-code* "a" 0)) '(:replacement :extra)))
          (setf (symbol-function 'char-code) original))))`,
    `(let ((original-char (symbol-function 'char)) (original-code (symbol-function 'char-code))
           (f (miricl-compiler:compile-native
             '(lambda (s i) (declare (optimize (debug 0)) (inline char char-code)) (char-code (char s i))))))
      (unwind-protect
        (progn (setf (symbol-function 'char) (lambda (&rest args) (declare (ignore args)) #\\z)
                     (symbol-function 'char-code) (lambda (&rest args) (declare (ignore args)) :replacement))
               (= 97 (funcall f "a" 0)))
        (setf (symbol-function 'char) original-char (symbol-function 'char-code) original-code)))`,
    `(let ((original-char (symbol-function 'char)) (original-code (symbol-function 'char-code))
           (f (miricl-compiler:compile-native
             '(lambda (s i) (declare (optimize (debug 0)) (notinline char char-code))
                (char-code (char s i))))))
      (unwind-protect
        (progn (setf (symbol-function 'char)
                       (lambda (&rest args) (declare (ignore args)) (values #\\z :ignored))
                     (symbol-function 'char-code)
                       (lambda (character) (values (eql character #\\z) :replacement)))
               (equal (multiple-value-list (funcall f "a" 0)) '(t :replacement)))
        (setf (symbol-function 'char) original-char (symbol-function 'char-code) original-code)))`,
  ];
  for (const form of redefinitions) {
    // Always parse sealed-only branches too, even in a source-compiler run.
    check(`(with-input-from-string (in ${quoteLisp(form)})
      (read in) (eq :eof (read in nil :eof)))`);
    if (!sourceCompiler) check(form);
  }
} finally { vm.close(); }
console.log('Character load fusion: checked views, Unicode, effects/read timing, observed characters and recovery pass.');
