import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl.wasm';
const vm = await Miricl.instantiate(await fs.readFile(image));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (miricl-compiler::ensure-compiler) (values)');
  check(`(and (not (fboundp 'miricl-compiler::read-hir-code-instruction-lisp))
              (not (fboundp 'miricl-compiler::unpack-hir-blocks-lisp)))`);
  // Reference readers belong to the host bootstrap, not the shipped runtime.
  const readerSource = await fs.readFile(new URL('../hir-code.lisp', import.meta.url), 'utf8');
  const reference = name => {
    const start = readerSource.indexOf(`(defun ${name} `);
    const next = readerSource.indexOf('\n(defun ', start + 1);
    assert.ok(start >= 0 && next > start);
    return readerSource.slice(start, next);
  };
  vm.mountFile('/hir-reader-reference.lisp', `(in-package :miricl-compiler)\n${reference('read-hir-code-instruction-lisp')}\n${reference('unpack-hir-blocks-lisp')}`);
  vm.evaluate('(load "/hir-reader-reference.lisp" :verbose nil) (values)');
  const word = value => {
    const bytes = [];
    do { const low = value % 128; value = Math.floor(value / 128); bytes.push(low + (value ? 128 : 0)); }
    while (value);
    return bytes;
  };
  for (const value of [0, 1, 127, 128, 16383, 16384, 0x7fffffff]) {
    const bytes = [0, 0, ...word(value)]; // CONSTANT, output R0, literal index.
    check(`(let ((code (map 'string #'code-char '( ${bytes.join(' ')} )))
                  (native (make-array 8 :initial-element 99))
                  (host (make-array 8 :initial-element 99)))
      (and (= (miricl-compiler::read-hir-code-instruction code 0 native) ${bytes.length})
           (= (miricl-compiler::read-hir-code-instruction-lisp code 0 host) ${bytes.length})
           (= (svref native 4) ${value}) (equalp native host)))`);
  }
  for (const bytes of [[0, 128], [0, 0, 128], [0, 0, 256], [0, 0, 255, 255, 255, 255, 8],
    [16, 0, 128, 1], [16, 0, 0, 128], [16, 0, 0, 256]]) {
    check(`(handler-case
      (progn (miricl-compiler::read-hir-code-instruction
        (map 'string #'code-char '(${bytes.join(' ')})) 0 (make-array 8)) nil)
      (program-error () t))`);
  }
  // Variable-arity operations retain their minimum and maximum checks.
  for (const bytes of [[18, 0, 1, 0], [30, 2, 0, 1, 0]]) {
    for (const reader of ['read-hir-code-instruction', 'read-hir-code-instruction-lisp']) {
      check(`(handler-case (progn (miricl-compiler::${reader}
          (map 'string #'code-char '(${bytes.join(' ')})) 0 (make-array 16)) nil)
        (program-error () t))`);
    }
  }
  // Metadata handoff retains unsigned destination+1 and clears absent fields.
  for (const destination of [127, 128, 0x7fffffff]) {
    const bytes = [0, ...word(destination), 0];
    check(`(let ((code (map 'string #'code-char '(${bytes.join(' ')})))
                 (native (make-array 8 :initial-element 99)) (host (make-array 8 :initial-element 99)))
      (and (= (miricl-compiler::read-hir-code-instruction code 0 native)
              (miricl-compiler::read-hir-code-instruction-lisp code 0 host))
           (= (svref native 2) ${destination + 1}) (equalp native host)))`);
  }
  // A previously enlarged internal buffer must not hide a small caller buffer.
  for (const [bytes, size] of [...Array.from({length: 6}, (_, size) => [[0, 0, 0], size]),
    [[10, 0, 2, 0, 1], 7]]) {
    check(`(let ((scratch (make-array ${size} :initial-element 99)))
      (miricl-compiler::read-hir-code-instruction (map 'string #'code-char '(0 0 7)) 0 (make-array 64))
      (and (handler-case (progn (miricl-compiler::read-hir-code-instruction
              (map 'string #'code-char '(${bytes.join(' ')})) 0 scratch) nil)
             (program-error () t))
           (every (lambda (value) (= value 99)) scratch)
           (= (miricl-compiler::read-hir-code-instruction
                (map 'string #'code-char '(0 0 7)) 0 (make-array 8)) 3)))`);
  }
  check(`(let ((scratch (make-array 8)))
    (= (miricl-compiler::read-hir-code-instruction (map 'string #'code-char '(0 0 7)) 0 scratch) 3))`);
  console.log('HIR checks: native test compiler ready.');
  vm.evaluate(`(in-package :miricl-compiler)
    (defun same-hir-readers-p (source)
      (let ((p (validate-hir-code (emit-hir-code (compile-to-hir source)))))
        (loop for f across (svref p 1) always
          (and
          (let* ((bytes (pack-hir-blocks (svref f 5)))
                 (native (unpack-hir-blocks bytes)) (host (unpack-hir-blocks-lisp bytes)))
            (and (= (length native) (length host))
              (loop for a across native for b across host always
                (and (= (svref a 0) (svref b 0)) (= (svref a 1) (svref b 1))
                     (string= (svref a 2) (svref b 2))))))
          (loop for b across (svref f 5) always
            (let ((code (svref b 2)) (pc 0)
                  (native (make-array (+ 6 (svref f 7)) :initial-element 0))
                  (host (make-array (+ 6 (svref f 7)) :initial-element 0)))
              (loop while (< pc (length code)) do
                (let ((next (read-hir-code-instruction code pc native))
                      (expected (read-hir-code-instruction-lisp code pc host)))
                  (unless (and (= next expected) (equalp native host)) (return-from same-hir-readers-p nil))
                  (setq pc next))) t))))))
    (defun hir-wire-operations (source &optional raw)
      (let ((p (validate-hir-code (emit-hir-code (compile-to-hir source)))) (result nil))
        (loop for f across (svref p 1) do
          (loop for b across (svref f 5) do
            (let ((code (svref b 2)) (pc 0) (scratch (make-array (+ 6 (svref f 7)))))
              (loop while (< pc (length code)) do
                (setq pc (read-hir-code-instruction code pc scratch))
                (let ((spec (svref *hir-code-operations* (svref scratch 0))))
                  (push (if raw spec (first spec)) result))))))
        result)) (values)
    (in-package :cl-user) (values)`);
  const cases = [
    ['(lambda (x) (+ x 3))', '4'],
    // DEBUG 3 must remain consumable by both backends while debugger HIR
    // markers are being integrated. Coalton also emits this policy.
    ['(lambda (x) (declare (optimize (debug 3))) (let ((y (+ x 2))) (values x y)))', '4', false],
    ['(lambda (x) (let ((y (locally (declare (optimize (debug 3))) (+ x 2)))) (values x y)))', '4', false],
    ['(lambda (&optional (x 7 xp) &rest rest) (values x xp rest))', ''],
    ['(lambda (&optional (x 7 xp) &rest rest) (values x xp rest))', '4 5 6'],
    ['(lambda (x &optional (y (+ x 2)) &aux (z (+ y 3))) (values x y z))', '4'],
    ['(lambda (&key (x 2) (y 3 yp)) (values x y yp))', ':y 9'],
    ['(lambda () (values))', ''],
    ["(lambda (x) (if (member x '(1 2)) (values :match x) (values :missing x)))", '2'],
    ["(lambda (x) (if (member (progn (incf x) x) '()) :bad (values :empty x)))", '2'],
    ['(lambda (x) (case x (nil :unreachable) ((nil) (values :nil 2)) (1 (values)) ((2 3) :group) (otherwise (values :default 4))))', 'nil'],
    ['(lambda (x) (case x (1 (values)) ((2 3) (values :group 9))))', '1'],
    ['(lambda (x) (case x (1 :one) ((2 3) (values :group 9))))', '3'],
    ['(lambda (x) (case x (1 :one) ((2 3) :group)))', '9'],
    ['(lambda (fn) (+ 1 (funcall fn)))', "#'(lambda () (values 7 :extra))"],
    ...Array.from({length: 6}, (_, arity) => {
      const names = Array.from({length: arity}, (_, i) => `x${i}`);
      const source = `(lambda (fn) (list (funcall fn ${names.map((_, i) => i + 1).join(' ')})))`;
      const callee = `(lambda (${names.join(' ')}) (values (+ ${names.join(' ')}) :extra))`;
      return [[source, `#'${callee}`], [source, `(miricl-compiler:compile-native '${callee})`],
        [source, `(miricl-compiler:compile-hir '${callee})`]];
    }).flat(),
    ['(lambda (fn) (funcall fn))', "#'(lambda () (values 7 :extra))"],
    ['(lambda (fn) (funcall fn 4 5))', "(miricl-compiler:compile-hir '(lambda (a &optional b &rest rest) (values a b rest)))"],
    ['(lambda (fn) (funcall fn :x 7))', "(miricl-compiler:compile-hir '(lambda (&key x) (values x :extra)))"],
    ['(lambda (fn) (funcall fn 4 5))', "(miricl-compiler:compile-hir '(lambda (&rest args) (values args)))"],
    ['(lambda (fn) (let ((x (funcall fn))) (values x x)))', "#'(lambda () (values))"],
    ['(lambda (fn) (let ((x (funcall fn 4))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (&rest args) (values args :ignored)))"],
    ['(lambda (fn) (let ((x (funcall fn 4))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (a &optional (b (+ a 2) supplied)) (values (list a b supplied) :ignored)))"],
    ['(lambda (fn) (let ((x (funcall fn 4 5))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (a &optional (b 7)) (values (list a b) :ignored)))"],
    ['(lambda (fn) (let ((x (funcall fn 4 5 6))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (a &rest args) (values (list a args) :ignored)))"],
    ['(lambda (fn) (let ((x (funcall fn 4 5 :z 7))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (a b &key z) (values (list a b z) :ignored)))"],
    ['(lambda (fn) (let ((x (funcall fn 4 5))) (values x :outer)))',
      "(miricl-compiler:compile-hir '(lambda (a b) (declare (ignore a b)) (values)))"],
    ['(lambda (fn) (funcall fn) (let ((x 5)) (if x (values x 6) (values))))', "#'(lambda () (values 1 2 3))"],
    ['(lambda (fn) (funcall fn) (values))', "#'(lambda () (values 1 2 3))"],
    ['(lambda (fn) (multiple-value-call (function list) (funcall fn)))', "#'(lambda () (values 7 :extra))"],
    ['(lambda (x) (multiple-value-call (function list) (values x 2) (values) (values 3 4)))', '1'],
    ['(lambda (x) (let ((get (lambda () x)) (put (lambda (v) (setq x v)))) (funcall put 19) (values (funcall get) x)))', '7'],
    ['(lambda (x) (labels ((walk (n) (if (zerop n) x (walk (1- n))))) (walk 25)))', '8'],
    ['(lambda (x) (block done (funcall (lambda () (return-from done (values x 9)))) :bad))', '8'],
    ['(lambda () (let ((x 0)) (block done (tagbody again (incf x) (when (< x 5) (funcall (lambda () (go again))))) x)))', ''],
    ['(lambda (x) (catch (quote stop) (throw (quote stop) (values x 3))))', '7'],
    ['(lambda () (let ((x 0)) (values (unwind-protect (values 3 4) (setq x 9)) x)))', ''],
    ['(lambda () (unwind-protect (values 3 4) (values 8 9)))', ''],
    ['(lambda () (handler-case (error "test") (error (c) (declare (ignore c)) (values :caught 2))))', ''],
    ['(lambda () (restart-case (invoke-restart (quote use) 19) (use (x) (values x :restart))))', ''],
    ['(lambda () (restart-bind ((pick (lambda () (values 5 7)))) (invoke-restart (quote pick))))', ''],
    [`(lambda () (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'use-value 9))))
       (restart-case (error "fixture") (use-value (x) (values x 7)))))`, ''],
    ['(lambda () (progv (quote (*hir-special*)) (quote (91)) *hir-special*))', ''],
    ['(lambda (x) (let ((*hir-special* x)) (values *hir-special* (locally (declare (special *hir-special*)) *hir-special*))))', '19'],
  ];
  vm.evaluate('(defparameter *hir-special* 6) (values)');
  for (const [source, args, executableHIR = true] of cases) {
    console.log(`HIR parity: ${source}`);
    const ordinary = vm.evaluate(`(multiple-value-list (funcall (function ${source}) ${args}))`).output;
    vm.evaluate(`(defparameter *hir-test* (miricl-compiler:compile-hir '${source})) (values)`);
    const actual = vm.evaluate(`(multiple-value-list (funcall *hir-test* ${args}))`).output;
    assert.equal(actual, ordinary, source);
    check(executableHIR ? '(compiled-function-p *hir-test*)' : '(not (compiled-function-p *hir-test*))');
    check(`(miricl-compiler::same-hir-readers-p '${source})`);
    vm.evaluate(`(multiple-value-bind (bytes literals)
        (miricl-compiler:emit-native-module
          (miricl-compiler::hir-code-to-unit
            (miricl-compiler:emit-hir-code (miricl-compiler:compile-to-hir '${source}))))
      (setf *hir-test* (miricl-compiler::compiler-primitive 17 bytes literals nil))) (values)`);
    assert.equal(vm.evaluate(`(multiple-value-list (funcall *hir-test* ${args}))`).output, ordinary,
      `native HIR round trip: ${source}`);
  }
  check(`(let ((ops (miricl-compiler::hir-wire-operations
                    '(lambda (x) (let ((y x)) (values y y))))))
    (and (not (member :cell-new ops)) (not (member :cell-read ops))))`);
  check(`(handler-case (progn
    (funcall (miricl-compiler:compile-hir '(lambda (fn) (let ((x (funcall fn 1 2))) x)))
             (miricl-compiler:compile-hir '(lambda (x) x))) nil)
    (program-error () t))`);
  check(`(= (funcall (miricl-compiler:compile-hir '(lambda (x) (1+ x))) 4) 5)`);
  check(`(equal (multiple-value-list
    (funcall (miricl-compiler:compile-hir '(lambda (fn)
      (multiple-value-bind (a b) (funcall fn) (values (+ a 10) (+ b 20)))))
      (lambda ()
        (miricl-compiler::read-hir-code-instruction
          (map 'string #'code-char '(10 0 2 127 128 1)) 0 (make-array 8))
        (values 7 9)))) '(17 29))`);
  vm.evaluate(`(defparameter *hir-thread-worker* (miricl-compiler:compile-hir
    '(lambda (bias)
       (let ((total 0) (step (lambda (a b) (values (+ a b) :extra))))
         (dotimes (i 40 total)
           (incf total (funcall step bias i))
           (miricl-threads:thread-yield)))))) (values)`);
  assert.equal((await vm.evaluateAsync(`(let
    ((a (miricl-threads:make-thread (lambda () (funcall *hir-thread-worker* 10))))
     (b (miricl-threads:make-thread (lambda () (funcall *hir-thread-worker* 100)))))
    (and (= (miricl-threads:join-thread a) 1180) (= (miricl-threads:join-thread b) 4780)))`,
    {eof: true})).output, 'T\n', 'Nested HIR calls retain operands across thread switches');
  check(`(not (null (member :call-primary (miricl-compiler::hir-wire-operations
      '(lambda (fn) (+ 1 (funcall fn)))))))`);
  check(`(not (member :call-primary (miricl-compiler::hir-wire-operations
      '(lambda (fn) (funcall fn)))))`);
  check(`(let ((ops (miricl-compiler::hir-wire-operations
                    '(lambda (x) (let ((f (lambda () x))) (setq x 9) (funcall f))))))
    (and (not (null (member :cell-new ops))) (not (null (member :cell-write ops)))))`);
  // Wide register tables retain operands, scoped types and debug bindings
  // through compact-code execution and native reconstruction.
  const wide = `(lambda (x) (declare (type (integer 0 1000) x))
    (block wide-outer (block wide-inner
    (let (${Array.from({length: 140}, (_, i) => `(v${i} (+ x ${i}))`).join(' ')})
      (list ${Array.from({length: 140}, (_, i) => `v${i}`).join(' ')}
        ${Array.from({length: 20}, () => 'v139').join(' ')})))))`;
  const wideExpected = vm.evaluate(`(funcall (function ${wide}) 3)`).output;
  vm.evaluate(`(defparameter *hir-wide-payload*
    (miricl-compiler:emit-hir-code (miricl-compiler:compile-to-hir '${wide}))) (values)`);
  check('(> (svref (svref (svref *hir-wide-payload* 1) 0) 1) 128)');
  assert.equal(vm.evaluate('(funcall (miricl-compiler:load-hir-code *hir-wide-payload*) 3)').output, wideExpected);
  assert.equal(vm.evaluate('(funcall (miricl-compiler::compile-native-hir *hir-wide-payload*) 3)').output, wideExpected);
  check(`(let ((ops (miricl-compiler::hir-wire-operations
      '(lambda () (block outer (block inner (values 7 9)))))))
    (and (not (member :block-enter ops)) (not (member :block-environment ops))
         (member :checked-jump ops) t))`);
  check(`(let ((ops (miricl-compiler::hir-wire-operations
      '(lambda () (declare (optimize (debug 2))) (block outer (values 7 9))))))
    (and (member :block-enter ops) (not (member :checked-jump ops)) t))`);
  check(`(let ((unit (miricl-compiler:compile-to-hir
      '(lambda () (block done (funcall (lambda () (return-from done (values 7 9)))))))))
    (dolist (function (miricl-compiler::hir-unit-functions unit))
      (dolist (identity (miricl-compiler::hir-function-captures function))
        (when (miricl-compiler::%hir-target-p identity)
          (setf (miricl-compiler::hir-target-captured-p identity) nil))))
    (equal (multiple-value-list (funcall (miricl-compiler:load-hir-code
      (miricl-compiler:emit-hir-code unit)))) '(7 9)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir '(lambda (x) (lambda () x))))
                (function (first (miricl-compiler:hir-unit-functions unit))))
    (dolist (binding (miricl-compiler::hir-function-bindings function))
      (setf (miricl-compiler::hir-binding-captured-p binding) nil))
    (= (funcall (funcall (miricl-compiler:load-hir-code (miricl-compiler:emit-hir-code unit)) 17)) 17))`);
  // These fixtures inspect or corrupt cells before optimization removes them.
  check(`(let* ((unit (miricl-compiler::lower-source-to-hir '(lambda (x) x)))
                (function (first (miricl-compiler:hir-unit-functions unit)))
                (instructions (loop for block in (miricl-compiler::hir-function-blocks function)
                                    append (miricl-compiler::hir-block-instructions block)))
                (cell (find :cell-new instructions :key (lambda (i) (miricl-compiler::hir-instruction-opcode i))))
                (read (find :cell-read instructions :key (lambda (i) (miricl-compiler::hir-instruction-opcode i)))))
    (setf (miricl-compiler::hir-instruction-inputs cell)
          (list (miricl-compiler::hir-instruction-output read)))
    (handler-case (progn (miricl-compiler:emit-hir-code unit) nil)
      (program-error (condition) (not (null (search "circular" (princ-to-string condition)))))))`);
  check(`(let* ((unit (miricl-compiler::lower-source-to-hir
                    '(lambda (x) (let ((y x))
                       (list (locally (declare (type (unsigned-byte 8) y)) y) y)))))
                (function (first (miricl-compiler:hir-unit-functions unit)))
                (ops (loop for block in (miricl-compiler::hir-function-blocks function) append
                       (mapcar (lambda (instruction) (miricl-compiler::hir-instruction-opcode instruction))
                         (miricl-compiler::hir-block-instructions block)))))
    (and (member :cell-new ops) (not (null (member :cell-read ops)))))`);
  check(`(let ((ops (miricl-compiler::hir-wire-operations
                    '(lambda (x) (declare (optimize (debug 3))) (let ((y x)) (values y y))))))
    (and (not (null (member :cell-new ops))) (not (null (member :cell-read ops)))))`);
  vm.evaluate('(defparameter *hir-effects* nil) (values)');
  vm.evaluate('(defun hir-many-arguments (&rest args) (values args :discard)) (values)');
  check(`(loop for n from 0 to 8 always
    (let* ((expected (loop for i below n collect i))
           (arguments (loop for i below n collect
             (list 'progn (list 'push i '*hir-effects*) i)))
           (source (list 'lambda nil (list 'list (cons 'hir-many-arguments arguments))))
           (f (miricl-compiler:compile-hir source)))
      (setq *hir-effects* nil)
      (and (equal (multiple-value-list (funcall f)) (list (list expected)))
           (equal *hir-effects* (reverse expected))
           (some (lambda (spec) (and (eq (first spec) :call-primary)
             (if (< n 5) (and (eql (second spec) (+ n 2)) (eql (third spec) (+ n 2)))
               (null (third spec))))) (miricl-compiler::hir-wire-operations source t)))))`);
  vm.evaluate('(setq *hir-effects* nil) (values)');
  check(`(let* ((wire (miricl-compiler:emit-hir-code
                 (miricl-compiler:compile-to-hir '(lambda (x) (values (+ x 1) (list x))))))
                (f (miricl-compiler:load-hir-code wire))
                (code (svref (svref (svref wire 1) 0) 5)))
    (and (loop repeat 3 always (equal (multiple-value-list (funcall f 40)) '(41 (40))))
         (progn (setf (char code (1- (length code))) (code-char 255))
           (handler-case (progn (miricl-compiler:load-hir-code wire) nil)
             (program-error (c) (plusp (length (princ-to-string c))))))
         (equal (multiple-value-list (funcall f 40)) '(41 (40)))))`);
  vm.evaluate('(defun hir-redefined-call (x) (values (+ x 1) :old)) (values)');
  check(`(let ((f (miricl-compiler:compile-hir '(lambda (x) (list (hir-redefined-call x)))))
               (original (symbol-function 'hir-redefined-call)))
    (unwind-protect
      (and (equal (funcall f 40) '(41))
        (progn (setf (symbol-function 'hir-redefined-call) (lambda (x) (values (+ x 2) :extra)))
          (equal (funcall f 40) '(42)))
        (progn (fmakunbound 'hir-redefined-call)
          (handler-case (progn (funcall f 40) nil)
            (undefined-function (c) (eq (cell-error-name c) 'hir-redefined-call)))))
      (setf (symbol-function 'hir-redefined-call) original)))`);
  for (const backend of ['compile-hir', 'compile-native']) {
    check(`(let* ((name (gensym "FUNCTION-REFERENCE-"))
                   (f (miricl-compiler:${backend} (list 'lambda nil
                     (list 'funcall (list 'function name) 40)))))
      (unwind-protect
        (progn (setf (symbol-function name) (lambda (x) (+ x 2)))
          (= (funcall f) 42))
        (fmakunbound name)))`);
    check(`(handler-case
      (progn (funcall (miricl-compiler:${backend} '(lambda () #'nil))) nil)
      (undefined-function (c) (null (cell-error-name c))))`);
    check(`(let* ((name '(setf hir-function-reference-place))
                   (f (miricl-compiler:${backend}
                     '(lambda (x) (funcall #'(setf hir-function-reference-place) x)))))
      (unwind-protect
        (and (progn (setf (fdefinition name) (lambda (x) (+ x 1))) (= (funcall f 40) 41))
             (progn (setf (fdefinition name) (lambda (x) (+ x 2))) (= (funcall f 40) 42))
             (progn (fmakunbound name)
               (handler-case (progn (funcall f 40) nil)
                 (undefined-function (c) (equal (cell-error-name c) name)))))
        (fmakunbound name)))`);
  }
  check(`(let ((f (miricl-compiler:compile-hir
                  '(lambda ()
                     (let ((x (progn (push :init *hir-effects*) (values 7 :discard))))
                       (values x x *hir-effects*))))))
    (and (equal (multiple-value-list (funcall f)) '(7 7 (:init)))
         (equal *hir-effects* '(:init))))`);
  check(`(let* ((source '(lambda (x) (+ x 1))) (f (miricl-compiler:compile-hir source)))
    (multiple-value-bind (expression closure name) (function-lambda-expression f)
      (declare (ignore name)) (and (null expression) (not closure))))`);
  check(`(let* ((source '(lambda (x &optional (y (+ x 2)))
                          (labels ((inner (z) (+ z y))) (inner x))))
               (original (copy-tree source)) (unit (miricl-compiler:compile-to-hir source)))
    (and (equal source original)
      (loop for function in (miricl-compiler:hir-unit-functions unit) always
        (and (every #'atom (miricl-compiler::hir-function-parameters function))
          (loop for binding in (miricl-compiler::hir-function-bindings function) always
            (let ((local (miricl-compiler::hir-binding-local binding)))
              (or (null local) (null (miricl-compiler::hir-local-definition local)))))
          (loop for block in (miricl-compiler::hir-function-blocks function) always
            (loop for instruction in (cons (miricl-compiler::hir-block-terminator block)
                                          (miricl-compiler::hir-block-instructions block)) always
              (atom (miricl-compiler::hir-instruction-origin instruction))))))))`);
  // Keep the source interpreter's existing debugging contract at DEBUG 3.
  check(`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (+ x 1)))))
    (and (= (funcall f 4) 5) (not (null (function-lambda-expression f)))))`);
  check(`(let* ((f (miricl-compiler:compile-hir
                   '(lambda (x) (declare (integer x)) (let ((y (+ x 1))) (if y (values y x) (values))))
                   :name 'hir-listing))
                (result :unset)
                (text (with-output-to-string (*standard-output*) (setq result (disassemble f)))))
    (and (null result) (search "HIR function" text) (search "B0:" text)
      (search "[Y]" text) (search "then B" text) (search "values" text)
      (null (function-lambda-expression f))))`);

  vm.evaluate(`(in-package :miricl-compiler)
    (defparameter *hir-payload* (emit-hir-code (compile-to-hir '(lambda (x) (+ x 1)))))
    (defun rejects-hir (payload)
      (handler-case (progn (load-hir-code payload) nil) (program-error (c) (plusp (length (princ-to-string c))))))
    (values)`);
  for (const name of ['BINDING', 'DEFINITIONS', 'PRIMARY', 'VARIABLES', 'INITIALIZATION']) {
    check(`(let ((symbol (compiler-primitive 10 1 "${name}")))
      (and (null (symbol-package symbol)) (= (compiler-primitive 9 symbol) 1)))`);
  }
  check('(rejects-hir nil)');
  check('(rejects-hir #())');
  check('(rejects-hir (vector #() #()))');
  // Structurally valid bytecode still must not cast a data value to a private
  // context, leak a context into Lisp data, or traverse a circular value group.
  vm.evaluate(`(defun hir-runtime-rejects (literals bytes)
    (let ((p (vector literals (vector (vector :malformed 3 0 0 0
                 (pack-hir-blocks (vector (vector 0 1 (map 'string #'code-char bytes)))) nil #() nil)))))
      (handler-case (progn (funcall (load-hir-code p)) nil) (error () t)))) (values)`);
  check('(hir-runtime-rejects #() (quote (10 1 1 0 31 1)))');
  check('(hir-runtime-rejects (vector nil) (quote (0 1 0 9 1 10 2 0 31 2)))');
  check(`(let ((cycle (list 1))) (setf (cdr cycle) cycle)
    (hir-runtime-rejects (vector cycle) '(0 1 0 31 1)))`);
  check(`(let ((cycle (list 1))) (setf (cdr cycle) cycle)
    (hir-runtime-rejects (vector cycle) '(0 1 0 38 1 1 0)))`);
  for (const mutation of [
    '(setf (svref f 1) 2147483647)',
    '(setf (svref f 2) 999)',
    '(setf (svref f 3) 999)',
    '(setf (svref f 4) 1)',
    '(setf (svref b 0) 999)',
    '(setf (svref b 2) (string (code-char 255)))',
    '(setf (svref b 2) (string (code-char 128)))',
    '(setf (svref b 2) "")',
    '(setf (svref b 2) (concatenate (quote string) (svref b 2) (string (code-char 0))))',
  ]) {
    check(`(let* ((p (emit-hir-code (compile-to-hir '(lambda (x) (+ x 1)))))
                 (f (svref (svref p 1) 0)) (blocks (unpack-hir-blocks (svref f 5))) (b (svref blocks 0)))
      ${mutation} (setf (svref f 5) (pack-hir-blocks blocks)) (rejects-hir p))`);
  }
  // Mutating the caller's payload cannot change installed executable code.
  check(`(let* ((p (emit-hir-code (compile-to-hir '(lambda (x) (+ x 1))))) (f (load-hir-code p)))
    (setf (svref (svref (svref p 1) 0) 5) "invalid")
    (= (funcall f 9) 10))`);
  check('(= (funcall (compile-hir (quote (lambda (x) (+ x 1)))) 10) 11)');
  vm.evaluate('(in-package :cl-user) (values)');

  const source = `(in-package :cl-user)
    (defparameter *hir-order* nil)
    (defun hir-fasl-values (x) (values (+ x 1) x))
    (push :first *hir-order*)
    (defparameter *hir-literal* (load-time-value (list :shared)))
    (defun hir-fasl-literal () *hir-literal*)
    (defun hir-fasl-counter (n) (lambda () (incf n)))
    (defparameter *hir-counter* (hir-fasl-counter 40))
    (push :second *hir-order*)`;
  vm.mountFile('/hir-source.lisp', source);
  vm.evaluate(`(let ((miricl-compiler::*fasl-code-format* :hir))
    (compile-file "/hir-source.lisp" :output-file "/hir-test.wasm" :verbose nil)) (values)`);
  const fasl = await vm.readFile('/hir-test.wasm');
  const fresh = await Miricl.instantiate(await fs.readFile(process.argv[3] ?? image));
  let snapshot;
  try {
    fresh.mountFile('/hir-test.wasm', fasl);
    fresh.evaluate('(load "/hir-test.wasm" :verbose nil) (values)');
    assert.equal(fresh.evaluate('(multiple-value-list (hir-fasl-values 8))').output, '(9 8)\n');
    assert.equal(fresh.evaluate('(equal *hir-order* (quote (:second :first)))').output, 'T\n');
    assert.equal(fresh.evaluate('(eq (hir-fasl-literal) *hir-literal*)').output, 'T\n');
    assert.equal(fresh.evaluate(`(not (null (search "HIR function"
      (with-output-to-string (*standard-output*) (disassemble 'hir-fasl-values)))))`).output, 'T\n');
    assert.equal(fresh.evaluate('(funcall *hir-counter*)').output, '41\n');
    fresh.evaluate('(miricl-core:save-image "/hir-snapshot.wasm") (values)');
    snapshot = await fresh.readFile('/hir-snapshot.wasm');
  } finally {fresh.close();}
  const restored = await Miricl.instantiate(snapshot);
  try {
    assert.equal(restored.evaluate('(funcall *hir-counter*)').output, '42\n');
    assert.equal(restored.evaluate('(eq (hir-fasl-literal) *hir-literal*)').output, 'T\n');
  } finally {restored.close();}
  vm.evaluate(`(miricl-compiler:compile-native-bundle "/hir-test.wasm" "/hir-native.wasm" :load t) (values)`);
  const native = await vm.readFile('/hir-native.wasm');
  const nativeVM = await Miricl.instantiate(await fs.readFile(process.argv[3] ?? image));
  try {
    nativeVM.mountFile('/hir-native.wasm', native);
    nativeVM.evaluate('(load "/hir-native.wasm" :verbose nil) (values)');
    assert.equal(nativeVM.evaluate('(multiple-value-list (hir-fasl-values 8))').output, '(9 8)\n');
    assert.equal(nativeVM.evaluate('(eq (hir-fasl-literal) *hir-literal*)').output, 'T\n');
  } finally {nativeVM.close();}
  console.log('Compact HIR: interpreter parity, closures, control transfers, validation/recovery and fresh FASL load pass.');
} finally {vm.close();}
