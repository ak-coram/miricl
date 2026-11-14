import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const binary = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const builder = await createCompilerTestVM(binary);
const fresh = await createCompilerTestVM(binary);
assert.equal(builder.evaluate("(fboundp 'miricl-compiler:optimize-system)").output, 'T\n');
assert.equal(builder.evaluate(`(and (miricl-compiler::compile-linker) (miricl-compiler::compile-linker)
  (null (function-lambda-expression #'miricl-compiler::compact-unit)))`).output, 'T\n');
const check = (vm, form) => assert.equal(vm.evaluate(form, {eof: true}).output, 'T\n', form);
try {
  check(builder, `(let* ((records '#((0) (1 "1") (1 "1") (3 "text") (3 "text")
                                  (4 "FRESH" nil 0) (4 "FRESH" nil 0)
                                  (4 "SAME" "KEYWORD" 0) (4 "SAME" "KEYWORD" 0)
                                  (5 1 0 0) (5 2 0 0) (5 1 0 1)
                                  (5 7 12 0) (5 8 13 0)
                                  (10 9) (10 9) (9 9 0) (9 9 0)
                                  (5 14 0 0) (5 15 0 0)
                                  (5 21 0 0) (5 20 0 0)
                                  (5 22 0 0) (5 1 1 0)))
                       (live (make-array (length records) :initial-element t))
                       (native (miricl-compiler::coalesce-records records live))
                       (primitive (symbol-function 'miricl-compiler::compiler-primitive))
                       (portable nil))
    (unwind-protect
      (progn
        (setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (operation &rest arguments)
            (if (= operation 25) (values nil nil) (apply primitive operation arguments))))
        (setf portable (miricl-compiler::coalesce-records records live)))
      (setf (symbol-function 'miricl-compiler::compiler-primitive) primitive))
    (and (equalp native portable)
         (= (aref native 1) (aref native 2)) (= (aref native 3) (aref native 4))
         (/= (aref native 5) (aref native 6)) (= (aref native 7) (aref native 8))
         (= (aref native 9) (aref native 10)) (/= (aref native 9) (aref native 11))
         (/= (aref native 12) (aref native 13))
         (/= (aref native 14) (aref native 15)) (/= (aref native 16) (aref native 17))
         (/= (aref native 18) (aref native 19))
         (= (aref native 20) 20) (= (aref native 21) 21) (= (aref native 22) 22)))`);
  check(builder, `(flet ((references (form) (miricl-compiler::form-references form)))
    (and
      (not (member '(:function ("LOCAL" "COMMON-LISP-USER"))
             (references '(labels ((local () (local))) (funcall #'local))) :test #'equal))
      (member '(:function ("LOCAL" "COMMON-LISP-USER"))
             (references '(flet ((local () (local))) (local))) :test #'equal)
      (not (nth-value 1 (miricl-compiler::form-references
             '(flet ((eval (x) x)) (eval 1)))))
      (not (nth-value 1 (miricl-compiler::form-references
             '(if nil (eval form) (if 'yes (values) (symbol-function name))))))
      (not (member '(:literal ("UNUSED" "COMMON-LISP-USER"))
             (references '(proclaim '(inline unused))) :test #'equal))
      (member '(:literal ("UNUSED" "COMMON-LISP-USER"))
             (references '(flet ((proclaim (x) x)) (proclaim '(inline unused)))) :test #'equal)
      (member '(:function ("EFFECT" "COMMON-LISP-USER"))
             (references '(proclaim '(inline unused) (effect))) :test #'equal) t))`);
  check(builder, `(flet ((barriers (form) (nth-value 1 (miricl-compiler::form-references form))))
    (and
      (null (barriers '(let ((function (lambda (x) (known-target x))))
                         (lambda (arguments) (apply function arguments)))))
      (barriers '(let ((function (lambda () :known)))
                   (setq function callback) (funcall function)))
      (barriers '(let ((function (lambda () :known)))
                   (let ((function callback)) (funcall function))))
      (barriers '(let ((function (lambda () :known)))
                   (lambda (function) (funcall function))))
      (barriers '(let ((function (lambda () :known)))
                   (flet ((invoke (function) (funcall function))) (invoke callback))))
      (barriers '(let ((function (lambda () :known)))
                   (declare (special function)) (funcall function))) t))`);
  builder.mountFile('/definitions.lisp', `
    (defpackage :whole-program-test (:use :cl) (:export :start :unused))
    (in-package :whole-program-test)
    (defmacro twice (x) (list '+ x x))
    (defun helper (x) (+ x 2))
    (declaim (inline unused-inline))
    (defun unused-inline () :unused)
    (defun callback (x) (+ x 10))
    (defun constants ()
      (list '(a b c) '(a b c) '#1=(:cycle . #1#) '#2=(:cycle . #2#)
        '#:distinct '#:distinct
        (load-time-value (list 1)) (load-time-value (list 1))
        '(list 1) (list 1)))
    (defun (setf accessor) (value object) (setf (car object) value))
    (defun parallel-accessor (object) (car object))
    (defun (setf parallel-accessor) (value object) (setf (car object) value))
    (defgeneric classify (x))
    (defmethod classify ((x integer)) :integer)
    (defmethod classify ((x string)) :string)
    (defstruct printable (value 7))
    (defmethod print-object ((object printable) stream)
      (format stream "printable/~D" (printable-value object)))
  `);
  builder.evaluate('(compile-file "/definitions.lisp") (load "/definitions.wasm")');
  builder.mountFile('/application.lisp', `
    (in-package :whole-program-test)
    (defparameter *events* nil)
    (defparameter *cycle* '#1=("é𝄞" . #1#))
    (defparameter *unused-pure* 99)
    (defun record-event () (push :loaded *events*))
    (defun unused-load-time-value () (load-time-value (push :value-loaded *events*)))
    (defparameter *unused-effect* (record-event))
    (defun dead-only-helper () :unreachable)
    (defun local-load-event () (push :local-load *events*))
    (defun escaped-helper () 23)
    (defun local-tree ()
      (labels ((unused-lookup (name) (symbol-function name))
               (unused-helper () (dead-only-helper))
               (unused-load-value () (load-time-value (local-load-event)))
               (left (n) (if (zerop n) 17 (right (1- n))))
               (right (n) (if (zerop n) 19 (left (1- n)))))
        (left 2)))
    (defun escaping-local ()
      (flet ((callback () (escaped-helper))) #'callback))
    (push :twice *events*)
    (push :twice *events*)
    (defun explicitly-kept () 17)
    (defun unused () "${'unnecessary application data '.repeat(2500)}")
    (defun start ()
      (let ((object (list 0)))
        (funcall (fdefinition '(setf accessor)) 5 object)
        (psetf (parallel-accessor object) 5)
        (list (helper (twice 20)) (classify 1) (classify "a")
              (funcall #'callback 2) (eq *cycle* (cdr *cycle*))
              (car *cycle*) *events* (car object)
              (format nil "~A" (make-printable)) (constants))))
  `);
  // A nested COMPILE-FILE must collect its own dependencies even when its
  // caller is externalizing native output without dependency metadata.
  builder.evaluate(`(progv (list (miricl-compiler::compiler-primitive 10 1 "%FILE-COLLECT-OBJECT-REFERENCES"))
      '(nil) (compile-file "/application.lisp"))`);
  check(builder, '(not (boundp (quote whole-program-test::*events*)))');
  // Bundle sections exactly as ASDF does; no runtime image is consulted.
  const sections = await Promise.all(['/definitions.wasm', '/application.wasm'].map(name => builder.readFile(name)));
  const bundle = Buffer.concat([sections[0], sections[1].subarray(8)]);
  builder.mountFile('/all.wasm', bundle);
  check(builder, `(pathnamep (nth-value 0 (miricl-compiler:shake-file "/all.wasm" "/small.wasm"
    :entry-points '("WHOLE-PROGRAM-TEST:START")
    :keep '("WHOLE-PROGRAM-TEST:EXPLICITLY-KEPT" "WHOLE-PROGRAM-TEST:ESCAPING-LOCAL"
            "WHOLE-PROGRAM-TEST:LOCAL-TREE")
    :report "/retention.txt")))`);
  check(builder, '(not (boundp (quote whole-program-test::*events*)))');
  const small = await builder.readFile('/small.wasm');
  const definitionChunks = async bytes => WebAssembly.Module.customSections(await WebAssembly.compile(bytes), 'miricl.fasl')
    .flatMap(payload => {
      const view = new DataView(payload);
      return Array.from({length: view.getUint32(12, true)}, (_, i) => view.getUint32(20 + 16 * i, true));
    }).filter(kind => kind === 4).length;
  assert.equal(await definitionChunks(small), 0, 'Production omits the optional source/name index.');
  check(builder, `(pathnamep (miricl-compiler:shake-file "/all.wasm" "/indexed.wasm"
    :entry-points '("WHOLE-PROGRAM-TEST:START") :index-definitions t))`);
  const indexed = await builder.readFile('/indexed.wasm');
  assert.ok(await definitionChunks(indexed) > 0);
  const indexedVM = await createCompilerTestVM(binary);
  try {
    indexedVM.mountFile('/indexed.wasm', indexed);
    check(indexedVM, '(load "/indexed.wasm")');
    check(indexedVM, '(= (first (whole-program-test:start)) 42)');
  } finally {indexedVM.close();}
  assert.ok(small.length < bundle.length / 2, `${small.length} / ${bundle.length}`);
  assert.ok(WebAssembly.validate(small));
  const report = new TextDecoder().decode(await builder.readFile('/retention.txt'));
  assert.match(report, /DROP :FUNCTION \("UNUSED"/);
  assert.match(report, /DROP :MACRO \("TWICE"/);
  assert.match(report, /KEEP :FUNCTION \("EXPLICITLY-KEPT"/);
  assert.match(report, /:RUNTIME-METHOD/);
  fresh.mountFile('/small.wasm', small);
  check(fresh, '(load "/small.wasm")');
  check(fresh, `(and (equal (butlast (whole-program-test:start))
                      '(42 :integer :string 12 t "é𝄞" (:twice :twice :local-load :loaded :value-loaded) 5 "printable/7"))
    (= (whole-program-test::explicitly-kept) 17)
    (not (fboundp 'whole-program-test:unused))
    (not (fboundp 'whole-program-test::unused-inline))
    (null (macro-function 'whole-program-test::twice))
    (not (boundp 'whole-program-test::*unused-pure*))
    (compiled-function-p #'whole-program-test:start))`);
  check(fresh, `(and (= (whole-program-test::local-tree) 17)
    (= (funcall (whole-program-test::escaping-local)) 23)
    (not (fboundp 'whole-program-test::dead-only-helper)))`);
  check(fresh, `(let* ((a (whole-program-test::constants))
                       (b (whole-program-test::constants)))
    (and (eq (first a) (second a))
         (eq (third a) (cdr (third a))) (eq (fourth a) (cdr (fourth a)))
         (not (eq (third a) (fourth a)))
         (not (eq (fifth a) (sixth a)))
         (eq (fifth a) (fifth b))
         (not (eq (seventh a) (eighth a)))
         (eq (seventh a) (seventh b))
         (progn (setf (car (seventh a)) 19)
                (and (= (car (seventh b)) 19) (= (car (eighth a)) 1)))
         (equal (ninth a) '(list 1)) (equal (tenth a) '(1))))`);
  check(builder, `(handler-case
    (progn (miricl-compiler:shake-file "/all.wasm" "/missing.wasm"
             :entry-points '("WHOLE-PROGRAM-TEST:MISSING")) nil)
    (error () (not (probe-file "/missing.wasm"))))`);
  check(builder, `(handler-case
    (progn (miricl-compiler:shake-file "/all.wasm" "/no-entry.wasm") nil)
    (error () (not (probe-file "/no-entry.wasm"))))`);

  // Computed runtime lookup prevents an unsound reduction. Quoted code is
  // inert and must not accidentally turn into a reflection barrier.
  builder.mountFile('/reflection.lisp', `
    (defpackage :reflection-test (:use :cl)) (in-package :reflection-test)
    (defun hidden () 37)
    (defun lookup (name) (funcall (symbol-function name)))
    (defun quoted-data () '(eval (+ 1 2)))
  `);
  builder.evaluate('(compile-file "/reflection.lisp")');
  check(builder, `(pathnamep (nth-value 0 (miricl-compiler:shake-file "/reflection.wasm" "/reflection-small.wasm"
    :entry-points '("REFLECTION-TEST:LOOKUP") :report "/reflection.txt")))`);
  assert.match(new TextDecoder().decode(await builder.readFile('/reflection.txt')), /:REFLECTION \("SYMBOL-FUNCTION"\)/);
  fresh.mountFile('/reflection.wasm', await builder.readFile('/reflection-small.wasm'));
  check(fresh, '(load "/reflection.wasm")');
  check(fresh, `(= (reflection-test::lookup 'reflection-test::hidden) 37)`);
  check(builder, `(pathnamep (nth-value 0 (miricl-compiler:shake-file "/reflection.wasm" "/quoted.wasm"
    :entry-points '("REFLECTION-TEST:QUOTED-DATA") :report "/quoted.txt")))`);
  assert.doesNotMatch(new TextDecoder().decode(await builder.readFile('/quoted.txt')), /:REFLECTION/);
  builder.mountFile('/symbol-input.lisp', `
    (defpackage :symbol-input-test (:use :cl)) (in-package :symbol-input-test)
    (defun hidden () 81)
    (defun lookup (name) (funcall (intern name "SYMBOL-INPUT-TEST")))
  `);
  builder.evaluate('(compile-file "/symbol-input.lisp")');
  check(builder, `(pathnamep (nth-value 0 (miricl-compiler:shake-file "/symbol-input.wasm" "/symbol-small.wasm"
    :entry-points '("SYMBOL-INPUT-TEST:LOOKUP") :report "/symbol-input.txt")))`);
  assert.match(new TextDecoder().decode(await builder.readFile('/symbol-input.txt')), /:SYMBOL-INPUT "INTERN"/);
  fresh.mountFile('/symbol-input.wasm', await builder.readFile('/symbol-small.wasm'));
  check(fresh, '(load "/symbol-input.wasm")');
  check(fresh, '(= (symbol-input-test::lookup "HIDDEN") 81)');
  // The metadata pass and unit-at-a-time decoding must produce the same
  // bytes as eager analysis, including when replacing the input pathname.
  builder.evaluate(`(miricl-compiler::write-program
    (miricl-compiler::mark-program (miricl-compiler:analyze-file "/all.wasm") nil nil nil)
    "/eager.wasm")
    (miricl-compiler:shake-file "/all.wasm" "/streamed.wasm" :strip nil) (values)`);
  const eager = await builder.readFile('/eager.wasm');
  assert.deepEqual(await builder.readFile('/streamed.wasm'), eager);
  builder.mountFile('/in-place.wasm', bundle);
  builder.evaluate('(miricl-compiler:shake-file "/in-place.wasm" "/in-place.wasm" :strip nil) (values)');
  assert.deepEqual(await builder.readFile('/in-place.wasm'), eager);
  builder.evaluate('(miricl-compiler:shake-file "/all.wasm" "relative/streamed.wasm" :strip nil) (values)');
  assert.deepEqual(await builder.readFile('/relative/streamed.wasm'), eager);
  // Replacing a relative output also exercises the publisher's backup rename.
  builder.evaluate('(miricl-compiler:shake-file "/all.wasm" "relative/streamed.wasm" :strip nil) (values)');
  assert.deepEqual(await builder.readFile('/relative/streamed.wasm'), eager);
  // A malformed record in a later unit is discovered after the first unit
  // was processed. Neither compaction nor native emission may publish it.
  const good = await builder.readFile('/reflection.wasm');
  const bad = Buffer.from(good);
  bad[bad.length - 1] = 255;
  builder.mountFile('/late-invalid.wasm', Buffer.concat([good, bad.subarray(8)]));
  const sentinel = Buffer.from('keep the previous output');
  for (const [name, operation] of [
    ['compact', '(miricl-compiler:shake-file "/late-invalid.wasm" "/preserved.wasm" :strip nil)'],
    ['native', '(miricl-compiler:compile-native-bundle "/late-invalid.wasm" "/preserved.wasm")'],
  ]) {
    builder.mountFile('/preserved.wasm', sentinel);
    check(builder, `(handler-case (progn ${operation} nil) (error () t))`);
    assert.deepEqual(await builder.readFile('/preserved.wasm'), new Uint8Array(sentinel), name);
    check(builder, '(null (directory "/.preserved-*.wasm"))');
  }
  console.log(`Whole-program IR: ${bundle.length} -> ${small.length} bytes; fresh LOAD preserves effects, methods, callbacks, SETF functions, Unicode/cycles and explicit roots; reflection remains conservative.`);
} finally { builder.close(); fresh.close(); }
