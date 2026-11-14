import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const parseContracts = process.argv.includes('--parse-contracts');
const sourceCompiler = process.argv.includes('--source') && !parseContracts;
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const quoteLisp = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
const check = source => assert.equal(vm.evaluate(parseContracts
  ? `(with-input-from-string (in ${quoteLisp(source)})
       (let ((end (gensym))) (loop for form = (read in nil end) until (eq form end) finally (return t))))`
  : source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    check(`(let* ((f (miricl-compiler:compile-native '(lambda (value array index)
                (declare (optimize (safety ${safety})) (inline (setf row-major-aref)))
                (funcall #'(setf row-major-aref) value array index))))
           (item (list :value)) (matrix (make-array '(2 3) :initial-element nil))
           (scalar (make-array nil :initial-element nil))
           (backing (make-array 10 :initial-element :old))
           (view (make-array '(2 3) :displaced-to backing :displaced-index-offset 2))
           (text (make-array 4 :element-type 'character :fill-pointer 0 :initial-element #\\a))
           (bits (make-array 67 :element-type 'bit :initial-element 0)))
      (and (equal (multiple-value-list (funcall f item matrix 5)) (list item))
        (eq item (row-major-aref matrix 5)) (null (row-major-aref matrix 4))
        (eq item (funcall f item scalar 0)) (eq item (row-major-aref scalar 0))
        (eq :changed (funcall f :changed view 5)) (eq :changed (aref backing 7))
        (char= #\\λ (funcall f #\\λ text 3)) (char= #\\λ (char text 3)) (= 0 (length text))
        (every (lambda (i) (and (= 1 (funcall f 1 bits i)) (= 1 (sbit bits i)))) '(0 31 32 63 64 66))
        (every (lambda (index) (handler-case (progn (funcall f :bad #(0 0) index) nil) (type-error () t)))
          (list -1 2 (ash 1 100) 1/2 :bad))
        (handler-case (progn (funcall f :bad :not-an-array 0) nil) (type-error () t))
        (handler-case (progn (funcall f :bad (make-array 0 :element-type nil) 0) nil) (type-error () t))
        (handler-case (progn (funcall f :bad text 0) nil) (type-error () t))
        (char= #\\a (char text 0))
        (handler-case (progn (funcall f 2 bits 1) nil) (type-error () t))
        (= 0 (sbit bits 1)) (= 1 (funcall f 1 bits 1))
        (eq :recovered (funcall f :recovered matrix 0))))`);
    check(`(let ((f (miricl-compiler:compile-native '(lambda (value array index)
               (declare (optimize (safety ${safety})) (inline (setf row-major-aref)))
               (funcall #'(setf row-major-aref) value array index) :done))))
      (and (handler-case (progn (funcall f :bad #(0) 1) nil) (type-error () t))
           (handler-case (progn (funcall f 2 #*0 0) nil) (type-error () t))
           (let ((array (vector nil)))
             (and (eq :done (funcall f :stored array 0)) (eq :stored (svref array 0))))))`);
  }
  for (const [expression, order] of [
    ["(funcall #'(setf row-major-aref) (funcall callback :value :stored) (funcall callback :array array) (funcall callback :index 1))", '(:value :array :index)'],
    ['(setf (row-major-aref (funcall callback :array array) (funcall callback :index 1)) (funcall callback :value :stored))', '(:array :index :value)'],
  ]) {
    check(`(let ((events nil) (array (vector nil nil))
          (f (miricl-compiler:compile-native '(lambda (callback array)
               (declare (inline (setf row-major-aref))) ${expression}))))
      (and (equal (multiple-value-list (funcall f (lambda (event value)
                     (push event events) (values value :ignored)) array)) '(:stored))
           (equal (reverse events) '${order}) (eq :stored (svref array 1))))`);
  }
  check(`(let* ((first (vector :old :old)) (second (vector :old :old))
        (view (make-array 2 :adjustable t :displaced-to first))
        (f (miricl-compiler:compile-native '(lambda (array callback)
             (declare (inline (setf row-major-aref)))
             (setf (row-major-aref array 0) :first)
             (funcall callback array)
             (setf (row-major-aref array 0) :second)))))
    (and (eq :second (funcall f view (lambda (array) (adjust-array array 2 :displaced-to second))))
         (eq :first (svref first 0)) (eq :second (svref second 0))))`);
  if (!sourceCompiler) {
    // Replacement probes run with the sealed compiler, whose own byte-buffer
    // resizing must keep using canonical runtime stores during compilation.
    check(`(let ((ordinary (miricl-compiler:compile-native '(lambda (value array index)
                (funcall #'(setf row-major-aref) value array index))))
            (blocked (miricl-compiler:compile-native '(lambda (value array index)
                (declare (notinline (setf row-major-aref)))
                (funcall #'(setf row-major-aref) value array index))))
            (captured (miricl-compiler:compile-native '(lambda (value array index)
                (declare (inline (setf row-major-aref)))
                (funcall #'(setf row-major-aref) value array index))))
            (original (fdefinition '(setf row-major-aref))) (array (vector nil)))
      (unwind-protect
        (progn (setf (fdefinition '(setf row-major-aref))
                 (lambda (value array index) (declare (ignore value array index)) (values :replacement :extra)))
          (and (equalp (map 'vector #'identity '(1 2 3)) #(1 2 3))
               (equalp (remove 2 #(1 2 3)) #(1 3))
               (equalp (substitute :changed 2 #(1 2 3)) #(1 :changed 3))
               (eq :filled (row-major-aref (make-array '(2 2) :initial-element :filled) 3))
               (equal (multiple-value-list (funcall ordinary :unused array 0)) '(:replacement :extra))
               (equal (multiple-value-list (funcall blocked :unused array 0)) '(:replacement :extra))
               (null (svref array 0))
               (equal (multiple-value-list (funcall captured :stored array 0)) '(:stored))
               (eq :stored (svref array 0))
               (let ((fresh (miricl-compiler:compile-native '(lambda (value array index)
                     (declare (inline (setf row-major-aref)))
                     (funcall #'(setf row-major-aref) value array index)))))
                 (and (equal (multiple-value-list (funcall fresh :unused array 0)) '(:replacement :extra))
                      (eq :stored (svref array 0))))))
        (setf (fdefinition '(setf row-major-aref)) original)))`);
  }
} finally {vm.close();}
console.log(parseContracts ? 'ROW-MAJOR-AREF store test forms parse.'
  : `ROW-MAJOR-AREF stores: checked physical bounds, values, displacement, effects, and recovery pass (${sourceCompiler ? 'source' : 'sealed'} compiler).`);
