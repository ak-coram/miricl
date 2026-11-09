import assert from 'node:assert/strict';
import fs from 'node:fs';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  vm.evaluate(`
    (defgeneric eql-dispatch (x))
    (defmethod eql-dispatch ((x t)) (values :other x))
    (defmethod eql-dispatch ((x (eql 7))) (values :integer x))
    (defmethod eql-dispatch ((x (eql 7f0))) (values :single x))
    (defmethod eql-dispatch ((x (eql nil))) (values :nil x))
    (defun check-eql-dispatch ()
      (and (loop for i below 1000 always
        (equal (multiple-value-list (eql-dispatch i)) (list (if (= i 7) :integer :other) i)))
        (equal (multiple-value-list (eql-dispatch 7f0)) '(:single 7f0))
        (equal (multiple-value-list (eql-dispatch 7d0)) '(:other 7d0))
        (equal (multiple-value-list (eql-dispatch nil)) '(:nil nil))))`);
  check('(check-eql-dispatch)');
  vm.evaluate(`
    (defgeneric context-dispatch (x))
    (defmethod context-dispatch ((x t)) (values :base x))
    (defmethod context-dispatch ((x integer))
      (let ((next (next-method-p)))
        (values (lambda (&rest args) (apply #'call-next-method args)) next)))
    (defun check-method-context ()
      (multiple-value-bind (first present) (context-dispatch 7)
        (let ((second (context-dispatch 19)))
          (and present
            (equal (multiple-value-list (funcall first)) '(:base 7))
            (equal (multiple-value-list (funcall second)) '(:base 19))
            (equal (multiple-value-list (funcall first 11)) '(:base 11))))))`);
  check('(check-method-context)');
  vm.evaluate(`(setf (symbol-function 'check-method-context)
    (miricl-compiler:compile-native (function-lambda-expression #'check-method-context)))`);
  check('(check-method-context)');
  // Distinct arguments outside the EQL specializers share a dispatch class.
  // Bound retained cache state independently of the number of objects passed.
  check(`(<= (length (funcall (symbol-function
    (miricl-compiler::compiler-primitive 10 1 "%GET")) #'eql-dispatch :cache)) 5)`);
  vm.evaluate('(defmethod eql-dispatch ((x (eql 19))) :new)');
  check('(and (eq (eql-dispatch 19) :new) (eq (eql-dispatch 20) :other))');
  vm.evaluate("(remove-method #'eql-dispatch (find-method #'eql-dispatch nil '((eql 19))))");
  check('(check-eql-dispatch)');
  vm.evaluate("(setf (symbol-function 'check-eql-dispatch) (miricl-compiler:compile-native (function-lambda-expression #'check-eql-dispatch)))");
  check('(check-eql-dispatch)');
  console.log('Generic dispatch: bounded EQL cache, types, multiple values, method edits, escaped next-method contexts and native callers pass.');
} finally {vm.close();}
