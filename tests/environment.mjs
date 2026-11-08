import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(let* ((output (make-string-output-stream))
  (*query-io* (make-two-way-stream (make-string-input-stream (format nil "maybe~%Yes~%no~%")) output)))
  (and (yes-or-no-p "Accept ~A?" :test) (not (y-or-n-p))
    (not (null (search "Accept TEST?" (get-output-stream-string output))))))`);
check(`(let* ((output (make-string-output-stream))
  (*query-io* (make-two-way-stream (make-string-input-stream (format nil "Y~%no~%")) output)))
  (and (y-or-n-p (lambda (stream argument) (write-string argument stream)) "Question")
    (not (yes-or-no-p))))`);
check(`(let ((x 2)) (equal (multiple-value-list (step (values x (incf x)))) '(2 3)))`);
check(`(let* ((input (make-string-input-stream (format nil "hello~%")))
  (output (make-string-output-stream)) (*standard-input* input) (*standard-output* output))
  (dribble "transcript.txt") (assert (equal (read-line) "hello")) (write-line "world") (dribble)
  (and (eq *standard-input* input) (eq *standard-output* output)
    (equal (get-output-stream-string output) (format nil "world~%"))
    (with-open-file (stream "transcript.txt")
      (and (equal (read-line stream) "hello") (equal (read-line stream) "world")
        (null (read-line stream nil nil))))))`);
check(`(let* ((object (list 1)) (output (make-string-output-stream))
  (*query-io* (make-two-way-stream
    (make-string-input-stream "(:set :car 9) (:get :car) :up :quit") output)))
  (and (null (multiple-value-list (inspect object))) (= (car object) 9)
    (not (null (search "Inspect" (get-output-stream-string output))))))`);
check(`(let ((miricl:*editor-function* (lambda (object) (list :edit object))))
  (and (equal (ed 'car) '(:edit car)) (equal (ed "source.lisp") '(:edit "source.lisp"))
    (handler-case (ed 37) (type-error () t))))`);
check(`(and (eq (symbol-package 'register-source-file) (find-package :miricl))
  (eq (find-symbol "REGISTER-SOURCE-FILE" :cl) 'miricl:register-source-file)
  (eq (nth-value 1 (find-symbol "REGISTER-SOURCE-FILE" :cl)) :internal))`);
console.log('Environment: query streams, STEP values, transcript recording, interactive inspection, editor callbacks, and extension namespaces pass.');
