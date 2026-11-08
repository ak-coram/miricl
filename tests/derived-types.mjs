#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  assert.equal(evaluate.runInContext(context, {timeout: 2000}), 'T\n', source);
};
check(`(let ((bound 7)) (deftype captured () (list 'integer 0 bound))
  (and (typep 7 'captured) (not (typep 8 'captured))))`);
check(`(progn (deftype optional (&optional (n)) (list 'integer 0 n))
  (and (typep 100 'optional) (typep 3 '(optional 3)) (not (typep 4 '(optional 3)))))`);
check(`(progn (deftype keywords (&key low high) (list 'member low high))
  (and (typep '* 'keywords) (not (typep nil 'keywords)) (typep nil '(keywords :high nil))))`);
check(`(progn (deftype nested ((a &optional b)) (list 'member a b))
  (and (typep 1 '(nested (1))) (typep '* '(nested (1))) (typep nil '(nested (1 nil)))))`);
check(`(progn (deftype documented () (declare (optimize (safety 3))) "doc" 'integer)
  (and (string= (documentation 'documented 'type) "doc")
    (string= (setf (documentation 'documented 'type) "new") "new")
    (string= (documentation 'documented 'type) "new")))`);
check(`(progn (deftype first-type () 'integer) (deftype alias-type () 'first-type)
  (and (typep 1 'alias-type) (progn (deftype first-type () 'string) (typep "x" 'alias-type))))`);
check(`(progn (deftype char-type () 'character) (deftype double-type () 'double-float)
  (and (eq (array-element-type (make-array 3 :element-type 'char-type)) 'character)
    (typep (coerce 1/3 'double-type) 'double-float)))`);
check(`(progn (deftype bounded (&optional (n 7)) (list 'integer 0 n))
  (equal (multiple-value-list (subtypep '(bounded 9) '(integer 0 9))) '(t t)))`);
check(`(progn
  (deftype contextual (&environment e)
    (if (nth-value 1 (macroexpand '(local-type-macro) e)) 'integer 'null))
  (defmacro check-context (&environment e)
    (list 'quote (and (typep 1 'contextual e) (typep nil 'contextual nil) (not (typep nil 'contextual e)))))
  (macrolet ((local-type-macro () t)) (check-context)))`);
check(`(progn (deftype contextual (&environment e) (declare (ignore e)) (throw 'done :done))
  (defmacro throw-context (&environment e) (list 'quote (catch 'done (typep 1 'contextual e))))
  (and (eq :done (macrolet ((local-type-macro () t)) (throw-context)))
    (progn (deftype contextual (&environment e) (list 'eql (not (null e)))) (null (the contextual nil)))))`);
check(`(handler-case (typep 1 '(bounded 2 3)) (program-error () t))`);
check('(= 42 (+ 20 22))');
console.log('Guest derived types: defaults, destructuring, lexical captures, redefinition, documentation, arrays, coercion, expansion environments, and recovery pass.');
