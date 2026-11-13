import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const check = form => assert.equal(vm.evaluate(form, {eof: true}).output, 'T\n', form);
async function compile(source, name = '*guarded*') {
  vm.evaluate(`(in-package :cl-user)
    (multiple-value-bind (bytes literals)
        (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (defparameter ${name} (miricl-compiler::compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/generic-call.wasm" :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (write-sequence bytes out))) (values)`, {eof: true});
  return WebAssembly.Module.imports(new WebAssembly.Module(await vm.readFile('/generic-call.wasm')))
    .some(entry => entry.name === 'native-generic-call');
}
try {
  vm.evaluate(`
    (defclass proof-root () ())
    (defclass proof-child (proof-root) ())
    (defparameter *proof-root* (make-instance 'proof-root))
    (defparameter *proof-child* (make-instance 'proof-child))
    (defparameter *proof-log* nil)
    (defgeneric proof-call (object))
    (defmethod proof-call ((object t)) (declare (ignore object)) (values :base 17))
    (defmethod proof-call ((object proof-root))
      (declare (ignore object)) (push :root *proof-log*) (values :root 42))
    (defmethod proof-call :before ((object proof-root))
      (declare (ignore object)) (push :before *proof-log*))
    (defmethod proof-call :after ((object proof-root))
      (declare (ignore object)) (push :after *proof-log*))
    (defmethod proof-call :around ((object proof-root))
      (declare (ignore object))
      (push :around *proof-log*)
      (multiple-value-prog1 (call-next-method) (push :returned *proof-log*)))
    (defmethod proof-call ((object proof-child))
      (declare (ignore object)) (push :child *proof-log*) (values :child 73))
    (values)`, {eof: true});
  assert.ok(await compile(`(lambda (object)
    (declare (type proof-root object))
    (cons :done (multiple-value-list
      (proof-call (progn (push :argument *proof-log*) object)))))`));
  check(`(and
    (progn (setq *proof-log* nil)
      (equal (funcall *guarded* *proof-root*) '(:done :root 42)))
    (equal (reverse *proof-log*) '(:argument :around :before :root :after :returned))
    (progn (setq *proof-log* nil)
      (equal (funcall *guarded* *proof-child*) '(:done :child 73)))
    (equal (reverse *proof-log*) '(:argument :around :before :child :after :returned)))`);
  check(`(let ((method (defmethod proof-call ((object proof-root))
                        (declare (ignore object)) (values :replacement 99))))
    (and (equal (funcall *guarded* *proof-root*) '(:done :replacement 99))
      (progn (remove-method #'proof-call method)
        (equal (funcall *guarded* *proof-root*) '(:done :base 17)))))`);
  check(`(let ((original #'proof-call))
    (unwind-protect
        (progn (setf (fdefinition 'proof-call)
                 (lambda (object) (declare (ignore object)) (values :rebound 11)))
               (equal (funcall *guarded* *proof-root*) '(:done :rebound 11)))
      (setf (fdefinition 'proof-call) original)))`);

  vm.evaluate(`(defclass proof-parent () ())
    (defclass proof-layout (proof-root) ())
    (defparameter *proof-layout* (make-instance 'proof-layout))
    (defgeneric proof-hierarchy (object))
    (defmethod proof-hierarchy ((object t)) (declare (ignore object)) '(:base))
    (defmethod proof-hierarchy ((object proof-parent)) (declare (ignore object)) '(:parent))
    (defmethod proof-hierarchy ((object proof-layout)) (declare (ignore object))
      (cons :layout (call-next-method))) (values)`);
  assert.ok(await compile(`(lambda (object) (declare (type proof-layout object))
    (list (proof-hierarchy object)))`, '*hierarchy*'));
  check(`(and (equal (funcall *hierarchy* *proof-layout*) '((:layout :base)))
    (progn (defclass proof-layout (proof-parent) ())
      (equal (funcall *hierarchy* *proof-layout*) '((:layout :parent)))))`);

  // A declared superclass need not have a primary method of its own. The
  // actual argument can be a subclass that does. Planning must not turn a
  // possible invocation error into an error while compiling the caller.
  vm.evaluate(`(defgeneric proof-partial (object))
    (defmethod proof-partial :before ((object proof-root)) (declare (ignore object)))
    (defmethod proof-partial ((object proof-child)) (declare (ignore object)) :child)
    (values)`);
  assert.equal(await compile(`(lambda (object) (declare (type proof-root object))
    (list (proof-partial object)))`, '*partial*'), false);
  check(`(and (equal (funcall *partial* *proof-child*) '(:child))
    (handler-case (progn (funcall *partial* *proof-root*) nil) (error () t)))`);
  // The same fallback must apply while preparing a portable native bundle.
  vm.mountFile('/partial-dispatch.lisp', `(in-package :cl-user)
    (defun proof-partial-file (object) (declare (type proof-root object))
      (list (proof-partial object)))`);
  vm.evaluate(`(compile-file "/partial-dispatch.lisp")
    (miricl-compiler:compile-native-bundle "/partial-dispatch.wasm" "/partial-native.wasm")
    (load "/partial-native.wasm") (values)`, {eof: true});
  check("(equal (proof-partial-file *proof-child*) '(:child))");

  vm.evaluate(`(defgeneric proof-eql (value))
    (defmethod proof-eql ((value t)) (declare (ignore value)) :other)
    (defmethod proof-eql ((value (eql 7))) (declare (ignore value)) (values :seven 7))
    (values)`);
  assert.ok(await compile('(lambda () (list (multiple-value-list (proof-eql 7))))', '*singleton*'));
  check("(equal (funcall *singleton*) '((:seven 7)))");
  check(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir
        '(lambda () (list (multiple-value-list (proof-eql 7))))))
    (let ((function (miricl-compiler::compiler-primitive 17 bytes literals nil)))
      ;; The caller owns the returned vector; installation must retain its
      ;; own container while sharing the literal objects it references.
      (fill literals nil)
      (equal (funcall function) '((:seven 7)))))`);
  assert.ok(await compile(`(lambda (x) (declare (type (integer 7 7) x))
    (list (multiple-value-list (proof-eql x))))`, '*singleton-type*'), 'Singleton integer types should select a guarded effective method');
  check("(equal (funcall *singleton-type* 7) '((:seven 7)))");
  assert.equal(await compile('(lambda (x) (list (proof-eql x)))', '*unknown*'), false);
  assert.equal(await compile(`(lambda (x) (declare (type proof-root x) (notinline proof-call))
    (list (proof-call x)))`), false);

  // A new MOP applicability method must invalidate already compiled plans.
  vm.evaluate(`(defparameter *proof-protocol-calls* 0)
    (defparameter *proof-protocol-method*
      (defmethod compute-applicable-methods :around ((gf (eql #'proof-hierarchy)) arguments)
        (incf *proof-protocol-calls*)
        (last (call-next-method)))) (values)`);
  check(`(and (equal (funcall *hierarchy* *proof-layout*) '((:base)))
              (plusp *proof-protocol-calls*))`);
  assert.equal(await compile(`(lambda (object) (declare (type proof-layout object))
    (list (proof-hierarchy object)))`), false);
  check(`(progn (remove-method #'compute-applicable-methods *proof-protocol-method*)
    (equal (funcall *hierarchy* *proof-layout*) '((:layout :parent))))`);

  vm.evaluate(`(defparameter *proof-predicate-calls* 0)
    (defun opaque-proof-type (value) (incf *proof-predicate-calls*) (typep value 'proof-root))
    (values)`);
  assert.equal(await compile(`(lambda (x) (declare (type (satisfies opaque-proof-type) x))
    (list (proof-call x)))`), false);
  check('(zerop *proof-predicate-calls*)');
  vm.evaluate(`(deftype opaque-proof-alias ()
    (incf *proof-predicate-calls*) 'proof-root) (values)`);
  assert.equal(await compile(`(lambda (x) (declare (type opaque-proof-alias x))
    (list (proof-call x)))`), false);
  check('(zerop *proof-predicate-calls*)');

  vm.evaluate(`(defclass proof-other () ())
    (defparameter *proof-identity-object* (make-instance 'proof-root))
    (defgeneric proof-identity (object))
    (defmethod proof-identity ((object proof-root)) (declare (ignore object)) '(:root))
    (defmethod proof-identity ((object proof-other)) (declare (ignore object)) '(:other))
    (defmethod proof-identity ((object (eql *proof-identity-object*)))
      (declare (ignore object)) (cons :same (call-next-method))) (values)`);
  assert.ok(await compile(`(lambda () (list (proof-identity '#.*proof-identity-object*)))`, '*identity*'));
  check(`(and (equal (funcall *identity*) '((:same :root)))
    (progn (change-class *proof-identity-object* 'proof-other)
      (equal (funcall *identity*) '((:same :other)))))`);
  check(`(let ((constructor (symbol-function
                 (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-CALL-PLAN"))))
    (and (handler-case (progn (funcall constructor nil nil nil nil) nil) (error () t))
         (handler-case (progn (funcall constructor #'identity nil nil nil) nil) (error () t))
         (equal (funcall *identity*) '((:same :other)))))`);

  vm.evaluate(`(defgeneric proof-saved (x))
    (defmethod proof-saved ((x fixnum)) (+ x 2)) (values)`);
  assert.ok(await compile(`(lambda (x) (declare (fixnum x)) (list (proof-saved x)))`, '*saved*'));
  const restored = await createCompilerTestVM(vm.saveImage());
  try {
    assert.equal(restored.evaluate('(funcall *saved* 40)').output, '(42)\n');
    assert.equal(restored.evaluate(`(progn (defmethod proof-saved ((x fixnum)) (+ x 3))
      (funcall *saved* 40))`).output, '(43)\n');
  } finally {restored.close();}

  vm.mountFile('/guarded-file.lisp', `
    (defpackage :guarded-file (:use :cl)) (in-package :guarded-file)
    (defgeneric compute-value (x))
    (defmethod compute-value ((x fixnum)) (+ x 2))
    (defun entry (x) (declare (fixnum x)) (list (compute-value x)))`);
  vm.evaluate(`(compile-file "/guarded-file.lisp") (load "/guarded-file.wasm")
    (miricl-compiler:compile-native-bundle "/guarded-file.wasm" "/guarded-native.wasm")
    (values)`, {eof: true});
  const fresh = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
  try {
    fresh.mountFile('/guarded-native.wasm', await vm.readFile('/guarded-native.wasm'));
    fresh.evaluate(`(setq miricl-compiler::*compiler-state* :cold)
      (load "/guarded-native.wasm") (values)`, {eof: true});
    assert.equal(fresh.evaluate(`(and (eq miricl-compiler::*compiler-state* :cold)
      (equal (guarded-file::entry 40) '(42)))`).output, 'T\n');
  } finally {fresh.close();}
  console.log('Compiler generic calls: guarded classes and singletons, method combination, values, effects, subclasses, redefinition and MOP fallback pass.');
} finally {vm.close();}
