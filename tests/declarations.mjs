import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(block found
  (handler-bind ((warning (lambda (c) (declare (ignore c)) (return-from found t))))
    (eval (list 'let nil (list 'declare (list (gensym))) nil)) nil))`);
check(`(let ((name (gensym)))
  (proclaim (list 'declaration name))
  (handler-bind ((warning (lambda (c) (declare (ignore c)) (error "Unexpected warning."))))
    (eq :ok (eval (list 'let nil (list 'declare (list name :arbitrary :data)) :ok)))))`);
for (const definition of [name => `(deftype ${name} () 'integer)`, name => `(defstruct ${name} slot)`,
  name => `(defclass ${name} () (slot))`, name => `(define-condition ${name} (condition) ())`]) {
  check(`(let ((name (gensym)))
    (proclaim (list 'declaration name))
    (handler-case (eval (subst name 'placeholder '${definition('placeholder')})) (error () t)))`);
  check(`(let ((name (gensym)))
    (eval (subst name 'placeholder '${definition('placeholder')}))
    (handler-case (proclaim (list 'declaration name)) (error () t)))`);
}
check(`(and (handler-case (proclaim '(declaration integer)) (error () t))
  (handler-case (proclaim '(declaration 17)) (type-error () t))
  (let ((x 42)) (declare (integer x)) (= x 42)))`);
check(`(and (let ((x 42)) (declare ((integer 0 100) x)) (= x 42))
  (eval (list 'let '((x 42)) (list 'declare (list (find-class 'integer) 'x)) '(= x 42))))`);
console.log('Declarations: unknown-name warnings, registered extensions, and bidirectional type/class/structure/condition conflicts pass.');
