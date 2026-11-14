import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  // Upstream ANSI array/stack tests cover ordinary shapes, contents, fill
  // pointers and return values. These checks exercise migration boundaries.
  check(`(let* ((token (list :shared)) (old (make-array '(2 3) :initial-element token))
                  (new (adjust-array old '(3 2) :initial-element :new)))
    (and (not (eq old new)) (equal (array-dimensions old) '(2 3))
      (eq token (aref new 1 1)) (eq :new (aref new 2 0))))`);
  check(`(let* ((storage (vector 0 1 2 3 4 5 6 7 8))
                  (old (make-array '(2 2) :adjustable t :displaced-to storage :displaced-index-offset 1))
                  (alias (make-array 2 :displaced-to old :displaced-index-offset 1)))
    (and (eq old (adjust-array old '(3 3) :initial-element :new))
         (= 1 (aref old 0 0)) (= 4 (aref old 1 1))
         (= 2 (aref alias 0)) (eq :new (aref alias 1))
         (progn (setf (aref old 0 0) 99) (= 1 (aref storage 1)))
         (null (array-displacement old))))`);
  check(`(let* ((storage (vector 0 1 2 3 4 5))
                  (old (make-array 2 :adjustable t :initial-element :old))
                  (alias (make-array 1 :displaced-to old)))
    (and (eq old (adjust-array old 3 :displaced-to storage :displaced-index-offset 2))
         (= 2 (aref alias 0))
         (progn (setf (aref alias 0) 99) (= 99 (aref storage 2)))
         (eq storage (array-displacement old))))`);
  for (const extension of ['nil', '0', '-1', '1/2', ':bad', '1.0', ...(baseline ? [] : ['100000000000000000000000000000000000000'])]) {
    check(`(let ((v (make-array 1 :adjustable t :fill-pointer t :initial-element :old)))
      (and (handler-case (progn (vector-push-extend :new v ${extension}) nil) (error () t))
           (= 1 (array-total-size v)) (= 1 (fill-pointer v)) (eq :old (aref v 0))))`);
  }
  check(`(let ((v (make-array 2 :adjustable t :fill-pointer 0)))
    (and (handler-case (progn (vector-push-extend :new v 0) nil) (type-error () t))
         (= 0 (fill-pointer v)) (= 2 (array-total-size v))))`);
  if (!baseline) check(`(let ((v (make-array 2 :adjustable t :fill-pointer 0 :initial-element :old))
               (extension (ash 1 200)))
    (and (= 0 (vector-push-extend :new v extension))
         (= 2 (array-total-size v)) (= 1 (fill-pointer v)) (eq :new (aref v 0))
         (progn (setf (fill-pointer v) 2)
           (handler-case (progn (vector-push-extend :failed v extension) nil)
             (type-error (condition) (eql extension (type-error-datum condition)))))
         (= 2 (array-total-size v)) (= 2 (fill-pointer v))
         (eq :new (aref v 0)) (eq :old (vector-pop v))))`);
  for (const [type, bad, initial] of [['bit', ':bad', '0'], ['character', '42', '#\\a'], ['nil', ':bad', 'nil']]) {
    check(`(let ((v (make-array ${type === 'nil' ? 0 : 1} :element-type '${type} :adjustable t :fill-pointer t ${type === 'nil' ? '' : `:initial-element ${initial}`})))
      (let ((size (array-total-size v)))
        (and (handler-case (progn (vector-push-extend ${bad} v 3) nil) (type-error () t))
             (= size (array-total-size v)) (= size (fill-pointer v)))))`);
  }
  check(`(let ((v (make-array 1 :element-type 'bit :fill-pointer 0 :initial-element 0)))
    (and (handler-case (progn (vector-push :bad v) nil) (type-error () t))
         (= 0 (fill-pointer v)) (= 0 (aref v 0))))`);
  check(`(let ((v (make-array 1 :fill-pointer t :initial-element :old)))
    (and (handler-case (progn (vector-push-extend :new v) nil) (error () t))
         (= 1 (fill-pointer v)) (eq :old (aref v 0))))`);
  for (const dimensions of ["'(1 . bad)", "'(1 -1)", "'(1 1/2)", ...(baseline ? [] : ["'(536870910 2)", "(make-list 256 :initial-element 0)"]), "(let ((x (list 1))) (setf (cdr x) x) x)"]) {
    check(`(let ((old (make-array 1 :adjustable t :initial-element :old)))
      (and (handler-case (progn (adjust-array old ${dimensions}) nil) (error () t))
           (= 1 (array-total-size old)) (eq :old (aref old 0))))`);
  }
  check(`(let ((shape '(536870910 2 0)))
    (let ((v (make-array '(0 0 0) :adjustable t)))
      (and (eq v (adjust-array v shape)) (= 0 (array-total-size v)) (equal shape (array-dimensions v)))))`);
  for (const keys of [':fill-pointer 0', ':initial-element :x :initial-contents #(a)', ':displaced-index-offset 0', ':unexpected 1', ':initial-contents #()']) {
    check(`(let ((v (make-array 1 :adjustable t :initial-element :old)))
      (and (handler-case (progn (adjust-array v 1 ${keys}) nil) (error () t))
           (eq :old (aref v 0))))`);
  }
  check(`(let ((v (make-array 2 :adjustable t :initial-element :old)))
    (and (eq v (adjust-array v 3 :initial-element :left :initial-element :right :strange 4 :allow-other-keys t))
         (eq :left (aref v 2))))`);
  check(`(progn
    (defparameter *array-adjust-expansion-array* nil)
    (defparameter *array-adjust-expansions* 0)
    (deftype array-adjust-effectful-element ()
      (incf *array-adjust-expansions*)
      (setf (fill-pointer *array-adjust-expansion-array*) 0) t)
    (let ((v (make-array 2 :adjustable t :fill-pointer 2)))
      (setq *array-adjust-expansion-array* v)
      (and (eq v (adjust-array v 3 :element-type 'array-adjust-effectful-element))
           (= 0 (fill-pointer v)) (= 1 *array-adjust-expansions*))))`);
  check(`(let* ((a (make-array 2 :adjustable t :initial-element :old))
                  (b (make-array 2 :adjustable t :displaced-to a)))
    (and (handler-case (progn (adjust-array a 2 :displaced-to b) nil) (error () t))
         (handler-case (progn (adjust-array a 2 :displaced-to a) nil) (error () t))
         (eq :old (aref a 0)) (eq a (adjust-array a 3 :initial-element :new))
         (eq :old (aref b 0))))`);
  if (!baseline) {
    check(`(let ((old (make-array 1 :adjustable t)) (dimensions '(1 1)))
      (and (handler-case (progn (adjust-array old dimensions) nil)
             (type-error (condition) (eq dimensions (type-error-datum condition))))
           (= 1 (array-total-size old))))`);
    check(`(let* ((old (make-array 1 :adjustable t))
                 (target (make-array 1 :adjustable t :displaced-to old)))
      (and (handler-case (progn (adjust-array old 1 :displaced-to target) nil)
             (type-error (condition) (eq target (type-error-datum condition))))
           (= 1 (array-total-size old))))`);
  }
  check(`(let* ((shape (make-list 255 :initial-element 1))
                  (old (make-array shape :adjustable t :initial-element :old)))
    (and (eq old (adjust-array old shape :initial-element :new))
         (= 255 (array-rank old)) (eq :old (row-major-aref old 0))))`);
  check(`(let ((original (fdefinition 'array-rank))
                  (v (make-array '(2 2) :adjustable t :initial-element :old)))
    (unwind-protect
      (progn
        (setf (fdefinition 'array-rank) (lambda (x) (declare (ignore x)) 77))
        (and (= 77 (funcall (fdefinition 'array-rank) v))
             (eq v (adjust-array v '(3 2)))
             (eq :old (aref v 1 1))))
      (setf (fdefinition 'array-rank) original)))`);
  check(`(let ((original (fdefinition 'error)) (bad '(integer 0 1 extra)))
    (unwind-protect
      (progn
        (setf (fdefinition 'error) (lambda (&rest args) (declare (ignore args)) :returned))
        (handler-case (progn (adjust-array (make-array 1) 2 :element-type bad) nil)
          (type-error (condition) (eq bad (type-error-datum condition)))))
      (setf (fdefinition 'error) original)))`);
  check(`(let ((v (make-array 2 :adjustable t :fill-pointer 1)))
    (equal '(:first :second) (multiple-value-list (multiple-value-prog1 (values :first :second)
      (adjust-array v 3) (vector-push-extend :y v)))))`);
  check(`(= (+ 20 22) 42)`);
  console.log('Array adjustment migration: sharing, views, rejection order, limits, callbacks, recovery and pending values pass.');
} finally { vm.close(); }
