// Run against a freshly built interpreter or the FORMAT diagnostic image.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'build/kernel-migration/format-bench.wasm'), {directory: process.cwd()});
try {
  const cases = [
    ['(format nil "~,2F" 1/8)', '"0.13"'],
    ['(format nil "~,2F" -1/8)', '"-0.13"'],
    ['(format nil "~,2,-3F" 125)', '"0.13"'],
    ['(format nil "~,0F" 123456789012345678901/2)', '"61728394506172839451."'],
    ['(format nil "~3,2F" 1/8)', '".13"'],
    ['(format nil "~4,2F" -1/8)', '"-.13"'],
    ['(format nil "~,2F" -0.0d0)', '"-0.00"'],
    ['(format nil "~F" 0.0d0)', '"0.0"'],
    ['(format nil "~F" 1.25d0)', '"1.25"'],
    ['(format nil "~,2E" 12.5d0)', '"1.25d+1"'],
    ['(handler-case (progn (format nil "~/missing" 1) nil) (error () t))', 'T'],
    ['(handler-case (progn (format nil "~,,,2001E" 1) nil) (error () t))', 'T'],
    ['(format nil "~A" :recovered)', '"RECOVERED"'],
  ];
  const parserCases = [
    `(progn
       (defun format-parser-capture (stream object colon at &rest parameters)
         (declare (ignore object))
         (prin1 (list colon at (mapcar (lambda (value) (if (characterp value) (char-code value) value)) parameters)) stream))
       (equal (format nil "~v,#,'🙂,-12345678901234567890:@/format-parser-capture/" 17 :object :extra)
              "(T T (17 2 128578 -12345678901234567890))"))`,
    `(equal (format nil "~1,2,3,4,5,6,7,8,9,10/format-parser-capture/" :object)
              "(NIL NIL (1 2 3 4 5 6 7 8 9 10))")`,
    `(equal (format nil "~[~vA~;~A~] ~A" 1 :chosen :tail) "CHOSEN TAIL")`,
    `(equal (format nil "~[~';A~;yes~]" 1) "yes")`,
    `(equal (format nil "~[~/FORMAT~]HELPER/~;ok~]" 1 :unused) "ok")`,
    `(equal (format nil "~{~[zero~;~A~]~^, ~}" '(1 :one 0 1 :two)) "ONE, zero, TWO")`,
    `(equal (format nil "~(~[UPPER~;LOWER~]~)" 0) "upper")`,
    `(equal (format nil "~vA" nil :object) "OBJECT")`,
    `(= 97 (length (format nil "~vA" #\\a "x")))`,
    `(handler-case (progn (format nil "~vA" (ash 1 50) "x") nil)
       (program-error () t))`,
    `(handler-case (progn (format nil "~vA" :bad-width "x") nil)
       (type-error (condition)
         (and (eq (type-error-datum condition) :bad-width)
              (eq (type-error-expected-type condition) 'integer))))`,
    `(equal (multiple-value-list (format nil "~A ~A" (values :first :discarded) :second)) '("FIRST SECOND"))`,
    `(equal (multiple-value-list (multiple-value-prog1 (values :kept :second)
              (format nil "~[x~;~A~]" 1 :nested))) '(:kept :second))`,
    `(let ((old-char (symbol-function 'char)) (old-code (symbol-function 'char-code))
             (old-array (symbol-function 'make-array)) (old-length (symbol-function 'list-length)))
       (unwind-protect
           (progn
             (setf (symbol-function 'char) (lambda (&rest args) (declare (ignore args)) (values #\\Z :extra)))
             (setf (symbol-function 'char-code) (lambda (&rest args) (declare (ignore args)) (values 0 :extra)))
             (setf (symbol-function 'make-array) (lambda (&rest args) (declare (ignore args)) (values nil :extra)))
             (setf (symbol-function 'list-length) (lambda (&rest args) (declare (ignore args)) (values :replaced :extra)))
             (equal (format nil "~[bad~;~vA/~#D~]" 1 nil :ok 7) "OK/7"))
         (setf (symbol-function 'char) old-char) (setf (symbol-function 'char-code) old-code)
         (setf (symbol-function 'make-array) old-array) (setf (symbol-function 'list-length) old-length)))`,
    `(handler-case (progn (format nil "~vA") nil) (program-error () t))`,
    `(let ((failed t))
       (dolist (text '("~" "~+" "~-" "~'" "~1" "~::A" "~@@A" "~[missing" "~{missing"))
         (unless (handler-case (progn (format nil text) nil) (error () t)) (setq failed nil)))
       (and failed (equal (format nil "~A" :recovered) "RECOVERED")))`,
  ];
  cases.push(...parserCases.map(source => [source, 'T']));
  for (const [source, expected] of cases) {
    const result = await vm.evaluateAsync(source);
    assert.equal(result.output.trim(), expected, source);
    assert.equal(result.incomplete, false, source);
  }
  console.log(`FORMAT runtime checks passed (${cases.length} cases).`);
} finally { vm.close(); }
