import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate(`(setq miricl-compiler:*compile-evaluations* nil)
    (defparameter *saved-string-call1* (miricl-compiler:compile-native
      '(lambda (function value) (values (funcall function value)))))
    (defparameter *saved-string-call2* (miricl-compiler:compile-native
      '(lambda (function left right) (values (funcall function left right)))))`);
  check(`(let* ((simple (copy-seq "abc"))
               (base (copy-seq "-abc-"))
               (view (make-array 3 :element-type 'character :displaced-to base :displaced-index-offset 1))
               (filled (make-array 5 :element-type 'character :initial-contents "abc--" :fill-pointer 3)))
    (and (eq simple (funcall *saved-string-call1* #'string simple))
         (eq view (funcall *saved-string-call1* #'string view))
         (eq filled (funcall *saved-string-call1* #'string filled))
         (funcall *saved-string-call2* #'string= view simple)
         (funcall *saved-string-call2* #'string= filled simple)
         (not (funcall *saved-string-call2* #'string= simple "abC"))
         (not (funcall *saved-string-call2* #'string= simple "abcd"))
         (funcall *saved-string-call2* #'string= "" "")))`);
  check(`(and (equal (funcall *saved-string-call1* #'string #\\λ) "λ")
    (eq (funcall *saved-string-call1* #'string :keyword) (symbol-name :keyword))
    (eq (funcall *saved-string-call1* #'symbol-name nil) (symbol-name nil))
    (eq (funcall *saved-string-call1* #'symbol-name :keyword) (symbol-name :keyword))
    (funcall *saved-string-call2* #'string= nil "NIL")
    (funcall *saved-string-call2* #'string= #\\λ "λ")
    (not (funcall *saved-string-call2* #'string= "λ" "Λ"))
    (funcall *saved-string-call2* #'string= :keyword "KEYWORD"))`);
  check(`(and
    (handler-case (progn (funcall *saved-string-call1* #'string 42) nil) (type-error () t))
    (handler-case (progn (funcall *saved-string-call1* #'symbol-name "x") nil) (type-error () t))
    (handler-case (progn (funcall *saved-string-call2* #'string= "a" 42) nil) (type-error () t))
    (handler-case (progn (funcall *saved-string-call2* #'string= 42 "a") nil) (type-error () t))
    (funcall *saved-string-call2* #'string= "recovery" "recovery"))`);
  check(`(let ((events nil)
    (f (miricl-compiler:compile-native '(lambda (function left right)
       (values (funcall (funcall function) (funcall left) (funcall right)))))))
    (and (equal (multiple-value-list (funcall f
           (lambda () (push :function events) #'string=)
           (lambda () (push :left events) (values "same" :ignored))
           (lambda () (push :right events) "same"))) '(t))
         (equal (reverse events) '(:function :left :right))))`);
  for (const name of ['string', 'symbol-name', 'string=']) {
    const binary = name === 'string=';
    check(`(let ((saved (symbol-function '${name}))
                 (ordinary (miricl-compiler:compile-native
                    '(lambda (x ${binary ? 'y' : ''}) (values (${name} x ${binary ? 'y' : ''}))))))
      (unwind-protect
        (progn (setf (symbol-function '${name})
          (lambda (&rest arguments) (declare (ignore arguments)) (values :replacement :extra)))
          (and (equal (multiple-value-list (funcall ordinary ${binary ? '"x" "x"' : ':x'})) '(:replacement))
               (equal (multiple-value-list (funcall *saved-string-call${binary ? 2 : 1}*
                        (symbol-function '${name}) ${binary ? '"x" "x"' : ':x'})) '(:replacement))
               ${binary
                 ? '(equal (multiple-value-list (funcall *saved-string-call2* saved "x" "x")) \'(t))'
                 : '(string= (funcall *saved-string-call1* saved :keyword) "KEYWORD")'}))
        (setf (symbol-function '${name}) saved)))`);
  }
  check(`(let ((f (miricl-compiler:compile-native '(lambda (function)
     (values (funcall function "abc" "bc" :start1 1))))))
    (funcall f #'string=))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (function)
     (values (funcall function))))))
    (handler-case (progn (funcall f #'string) nil) (program-error () t)))`);
} finally {vm.close();}
console.log('Saved string builtins: identity/views, designators, Unicode, checked errors/recovery, effects, primary values, replacement and keyword fallback pass.');
