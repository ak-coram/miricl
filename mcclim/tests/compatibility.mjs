import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? new URL('../../miricl-precompiled.wasm', import.meta.url)));
async function check(name, form) {
  const {output} = await vm.evaluateAsync(form, {eof: true});
  assert.equal(output.trim(), 'T', `${name}: ${output}`);
  console.log(`PASS ${name}`);
}
try {
  for (const name of ['platform', 'mop', 'gray-streams', 'garbage']) {
    vm.mountFile(`/${name}.lisp`, await fs.readFile(new URL(`../compatibility/${name}.lisp`, import.meta.url)));
    await vm.evaluateAsync(`(load "/${name}.lisp") (values)`, {eof: true});
  }
  await check('MOP class definitions preserve custom slot options', `(progn
    (defclass port-direct-slot (c2mop:standard-direct-slot-definition)
      ((kind :initarg :kind :reader port-slot-kind)))
    (defclass port-metaclass (standard-class) ())
    (defmethod c2mop:direct-slot-definition-class ((class port-metaclass) &rest initargs)
      (declare (ignore initargs)) (find-class 'port-direct-slot))
    (defclass port-slot-test ()
      ((value :kind :answer :initform 42 :reader port-slot-value))
      (:metaclass port-metaclass))
    (and (= (port-slot-value (make-instance 'port-slot-test)) 42)
         (eq (port-slot-kind (first (c2mop:class-direct-slots (find-class 'port-slot-test)))) :answer)
         (eq (class-of (c2mop:class-prototype (find-class 'port-slot-test))) (find-class 'port-slot-test))
         (eq (c2mop:class-prototype (find-class 'port-slot-test))
             (c2mop:class-prototype (find-class 'port-slot-test)))
         (eq (find-package "C2MOP") (find-package "CLOSER-MOP"))))`);
  await check('MOP specialized lambda lists preserve optional defaults', `(and
    (equal (c2mop:extract-lambda-list '(x (y integer) &optional (z 42 zp) &rest tail))
           '(x y &optional (z 42 zp) &rest tail))
    (equal (c2mop:extract-specializer-names '(x (y (eql :key)) &key option)) '(t (eql :key)))
    (handler-case (progn (c2mop:extract-lambda-list '((x integer extra))) nil)
      (program-error () t)))`);
  await check('MOP default initargs respect class precedence and redefinition', `(progn
    (defclass default-parent () () (:default-initargs :a 17 :b 19))
    (defclass default-child (default-parent) () (:default-initargs :a 42))
    (let ((defaults (c2mop:class-default-initargs (find-class 'default-child))))
      (and (= (funcall (third (assoc :a defaults))) 42)
           (= (funcall (third (assoc :b defaults))) 19)
           (= (length defaults) 2)
           (progn (defclass default-parent () ())
             (c2mop:ensure-finalized (find-class 'default-child))
             (null (assoc :b (c2mop:class-default-initargs (find-class 'default-child))))))))`);
  await vm.evaluateAsync(`
    (defclass capture-stream (trivial-gray-streams:fundamental-character-output-stream)
      ((text :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0) :reader capture-text)))
    (defmethod trivial-gray-streams:stream-write-char ((s capture-stream) char)
      (vector-push-extend char (capture-text s)))
    (defmethod trivial-gray-streams:stream-line-column ((s capture-stream))
      (let* ((text (capture-text s)) (last (position #\\Newline text :from-end t)))
        (- (length text) (if last (1+ last) 0))))
    (defclass input-test-stream (trivial-gray-streams:fundamental-character-input-stream)
      ((input :initarg :input :reader test-input)))
    (defmethod trivial-gray-streams:stream-read-char ((s input-test-stream))
      (cl:read-char (test-input s) nil :eof))
    (defmethod trivial-gray-streams:stream-unread-char ((s input-test-stream) char)
      (cl:unread-char char (test-input s)))
    (values)`, {eof: true});
  await check('unicode output, bounds, return identity and stream designators', `
    (let* ((s (make-instance 'capture-stream)) (text "xλ😀y") (*standard-output* s))
      (and (eq text (mcclim-streams:write-string text nil :start 1 :end 3))
           (char= #\\! (mcclim-streams:write-char #\\!))
           (string= (capture-text s) "λ😀!") (mcclim-streams:streamp s)
           (mcclim-streams:output-stream-p s) (not (mcclim-streams:input-stream-p s))))`);
  await check('malformed stream bounds reject and allow recovery', `
    (let ((s (make-instance 'capture-stream)))
      (and (handler-case (mcclim-streams:write-string "abc" s :start -1) (error () t))
           (handler-case (mcclim-streams:write-string "abc" s :start 2 :end 1) (error () t))
           (handler-case (mcclim-streams:write-string "abc" s :end 4) (error () t))
           (progn (mcclim-streams:write-string "ok" s) (string= "ok" (capture-text s)))))`);
  await check('read-line multiple values and end of file', `
    (let ((s (make-instance 'input-test-stream :input (make-string-input-stream (format nil "λ~%last")))))
      (and (equal (multiple-value-list (mcclim-streams:read-line s)) '("λ" nil))
           (equal (multiple-value-list (mcclim-streams:read-line s)) '("last" t))
           (equal (multiple-value-list (mcclim-streams:read-line s nil :end)) '(:end t))
           (handler-case (mcclim-streams:read-char s) (end-of-file () t))))`);
  await check('Bordeaux API v1 lock names and condition wakeups', `
    (let ((lock (bt:make-lock "test")) (cv (bt:make-condition-variable)) (ready nil))
      (let ((worker (bt:make-thread (lambda ()
                     (bt:with-lock-held (lock)
                       (loop until ready do (bt:condition-wait cv lock))) 42))))
        (bt:with-lock-held (lock) (setf ready t) (bt:condition-notify cv))
        (= (bt:join-thread worker) 42)))`);
  await check('native streams remain usable', `
    (let ((s (make-string-output-stream)))
      (mcclim-streams:write-string "native" s)
      (and (mcclim-streams:streamp s) (string= "native" (get-output-stream-string s))))`);
} finally {vm.close();}
