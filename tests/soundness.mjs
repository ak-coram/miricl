// Implementation contracts absent from ANSI: untrusted compiler boundaries,
// conservative optimization proofs, redefinition guards, and the FASL codec.
// node tests/soundness.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);
const recover = () => assert.equal(evaluate("(funcall (compile-native '(lambda (x) (+ x 1))) 41)"), '42\n');
try {
  evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  // The private native ABI accepts raw mutable eqref arrays. These standalone
  // stable WasmGC types canonicalize to that packet type across modules; the
  // fixture constructs packets without depending on runtime layout indices.
  const section = (id, bytes) => [id, bytes.length, ...bytes];
  const body = bytes => [bytes.length + 1, 0, ...bytes];
  const packets = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, [3, 0x5e, 0x6d, 1,
      0x60, 2, 0x6f, 0x6f, 1, 0x6f, 0x60, 0, 1, 0x6f]),
    ...section(3, [2, 1, 2]),
    ...section(7, [2, 4, 109, 97, 107, 101, 0, 0,
      5, 101, 109, 112, 116, 121, 0, 1]),
    ...section(10, [2,
      ...body([0x20, 0, 0xfb, 0x1a, 0xfb, 0x17, 0x6d,
        0x20, 1, 0xfb, 0x1a, 0xfb, 0x17, 0x6d,
        0xfb, 8, 0, 2, 0xfb, 0x1b, 0x0b]),
      ...body([0x41, 0, 0xfb, 7, 0, 0xfb, 0x1b, 0x0b])]),
  ]))).exports;
  const elements = [vm.javascript.makeText('packet element'), null];
  const rejectArgument = (arguments_, index) => {
    assert.throws(() => {
      try {vm.vm['native-argument'](arguments_, index);}
      catch (cause) {
        assert.ok(cause instanceof WebAssembly.Exception, 'Reject through a Lisp condition, without a Wasm trap.');
        throw vm.failure(cause);
      }
    }, error => error.code > 0 && error.message.length > 0);
    recover();
  };
  for (const [arguments_, expected] of [[packets.make(...elements), elements],
    [packets.empty(), []], [vm.vm['native-list2'](...elements), elements]]) {
    expected.forEach((value, index) => assert.equal(vm.vm['native-argument'](arguments_, index), value));
    for (const index of [expected.length, 0x7fffffff])
      assert.equal(vm.vm['native-argument'](arguments_, index), null, 'Absent arguments return NIL.');
    for (const index of [-1, -0x80000000]) rejectArgument(arguments_, index);
  }
  rejectArgument(vm.vm['native-cons'](elements[0], elements[0]), 1);
  console.log('Soundness: raw native packets preserve elements and reject invalid reads with recovery.');

  // The image factory keeps its packet ABI while its children may use fixed
  // workers. Exercise that build policy without installing another compiler.
  check(`(let ((compiler (symbol-function 'compile-native))
      (policy *wasm-bootstrap-direct-workers*))
    (multiple-value-bind (entries bytes)
        (let ((*compiler-bootstrapping* t) (*wasm-bootstrap-direct-workers* t))
          (compile-native-module
            '((defun image-worker-check (x)
                (declare (optimize (debug 0))) (values (1+ x) :second))
              (defun image-worker-tail (x)
                (declare (optimize (debug 0))) (image-worker-check x))
              (defun image-worker-captures (initial)
                (declare (optimize (debug 0)))
                (let ((counter initial))
                  (values (lambda (delta) (incf counter delta))
                          (lambda () counter))))
              (defun image-worker-wide (a b c d e)
                (declare (optimize (debug 0))) (values a b c d e)))
            :inline nil))
      (let ((fixed (cdr (assoc 'image-worker-check entries)))
            (tail (cdr (assoc 'image-worker-tail entries)))
            (captures (cdr (assoc 'image-worker-captures entries)))
            (wide (cdr (assoc 'image-worker-wide entries))))
        (and (plusp (length bytes))
          (every (lambda (entry) (compiled-function-p (cdr entry))) entries)
          (equal (multiple-value-list (funcall fixed 41)) '(42 :second))
          (equal (multiple-value-list (funcall tail 41)) '(42 :second))
          (handler-case (progn (funcall fixed) nil) (program-error () t))
          (handler-case (progn (funcall fixed 1 2) nil) (program-error () t))
          (equal (multiple-value-list (funcall fixed 42)) '(43 :second))
          (multiple-value-bind (advance observe) (funcall captures 10)
            (multiple-value-bind (other-advance other-observe) (funcall captures 100)
              (and (= (funcall advance 2) 12) (= (funcall observe) 12)
                (= (funcall other-observe) 100) (= (funcall other-advance 3) 103)
                (= (funcall observe) 12))))
          (equal (multiple-value-list (funcall wide 1 nil 3 4 5)) '(1 nil 3 4 5))
          (eq compiler (symbol-function 'compile-native))
          (eq policy *wasm-bootstrap-direct-workers*)))))`);
  console.log('Soundness: image worker factories preserve arity, values, captures and compiler bindings.');

  const compilerEntries = vm.vm['core-compiler-functions'];
  const workerIndex = Array.from({length: compilerEntries.length}, (_, index) => index)
    .find(index => vm.vm['native-entry-valid'](compilerEntries.get(index), 1));
  for (const index of [compilerEntries.length, ...(workerIndex === undefined ? [] : [workerIndex])]) {
    check(`(handler-case (progn (compiler-primitive 20 ${index} nil nil) nil)
      (program-error () t))`);
    recover();
  }
  if (workerIndex !== undefined) {
    // Saved images reconstruct native objects through these checked readers
    // and constructors. An existing entry index must also have the right type.
    const native = vm.vm['core-native-function'](compilerEntries.get(0), null, null, 0, null);
    const type = vm.vm['core-object-type'](native);
    const reader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${type}:`));
    const signature = reader.split(':')[2], fields = vm.vm[reader](native);
    const args = fields.filter((_, index) => signature[index] === signature[index].toLowerCase());
    const entry = Array.from(signature.slice(0, signature.indexOf('e')))
      .filter(kind => kind === kind.toLowerCase()).length;
    const invalid = args.slice();
    invalid[entry] = compilerEntries.get(workerIndex);
    assert.throws(() => {
      try {vm.vm[`core-new:${type}`](...invalid);}
      catch (cause) {
        assert.ok(cause instanceof WebAssembly.Exception, 'Reject the wrong entry type through a Lisp condition.');
        throw vm.failure(cause);
      }
    }, error => error.code > 0);
    assert.ok(vm.vm[`core-new:${type}`](...args), 'A rejected entry must not poison subsequent restoration.');
    recover();
  }

  const reject = (source, reason) => {
    const output = evaluate(`(handler-case (progn ${source} :unexpected-acceptance)
      (compiler-input-error (condition) (princ-to-string condition)))`);
    assert.doesNotMatch(output, /:UNEXPECTED-ACCEPTANCE/, source);
    assert.match(output, reason, source);
    assert.match(output, /Input:/, 'The condition identifies the offending input.');
    assert.ok(output.length < 4096, 'Malformed graphs must have bounded diagnostics.');
    recover();
  };
  // Malformed source spines must fail before unchecked evaluator traversal;
  // repairing the same source must leave all subsequent values intact.
  for (const tail of ['arguments', ':dotted']) {
    check(`(let* ((arguments (list 1 nil 3 :fourth :fifth))
        (last (last arguments)) (form (cons 'values arguments)) (rejected nil))
      (setf (cdr last) ${tail})
      (handler-case (eval form)
        (type-error (condition)
          (setq rejected (and (eq (type-error-datum condition) arguments)
            (type-error-expected-type condition)))))
      (setf (cdr last) nil)
      (and rejected
        (equal (multiple-value-list (eval form)) '(1 nil 3 :fourth :fifth))))`);
  }
  for (const tail of ['arguments', ':dotted']) {
    check(`(let* ((arguments (list 1 2 3 4 5))
        (last (last arguments)) (form (cons '+ arguments)) (rejected nil))
      (setf (cdr last) ${tail})
      (handler-case (eval form)
        (type-error (condition)
          (setq rejected (and (consp (type-error-datum condition))
            (type-error-expected-type condition)))))
      (setf (cdr last) nil)
      (and rejected (equal (multiple-value-list (eval form)) '(15))))`);
  }
  for (const [source, reason] of [
    ["(compile-native '(lambda (x . tail) x))", /dotted tail/],
    ["(compile-native '(lambda (x x) x))", /duplicate parameter/],
    ["(compile-native '(lambda (&rest &key) nil))", /lambda-list keyword/],
    ["(compile-native '(lambda (&optional (x nil &rest)) x))", /lambda-list keyword/],
    ["(compile-native '(lambda () (declare (optimize (safety 4))) nil))", /level from 0 through 3/],
    ["(compile-native '(lambda (x) (declare (type (array t (-1)) x)) x))", /array dimension/],
    ["(compile-native '(lambda (x) (declare ((12) x)) x))", /symbol naming the compound type/],
    ["(compile-native '(lambda (x) (declare ((integer 0 2) 12)) x))", /variable name/],
    ["(compile-native '(lambda (x) (declare ((integer . 2) x)) x))", /dotted tail/],
    [`(let ((form (list 'progn nil))) (setf (second form) form)
       (compile-to-hir (list 'lambda nil form)))`, /circular executable/],
    [`(let ((parameters (list 'x))) (setf (cdr parameters) parameters)
       (compile-to-hir (list 'lambda parameters 'x)))`, /circular list/],
    [`(let ((type (list 'array nil '(5)))) (setf (second type) type)
       (compile-to-hir (list 'lambda '(x) (list 'declare (list 'type type 'x)) 'x)))`, /circular type/],
    [`(let ((type (list 'and nil))) (setf (second type) type)
       (compile-to-hir (list 'lambda '(x) (list 'declare (list type 'x)) 'x)))`, /circular type/],
    ['(compile-native-module 4)', /module definitions/],
    ['(emit-native-module #())', /HIR unit/],
    ['(infer-hir-types nil)', /HIR unit/],
    ['(native-read-uleb #(128) 0)', /truncated/],
    ['(native-read-sleb #(128 128 128 128 128 128 128 128 128 128 0) 0)', /exceeds 64 bits/],
  ]) reject(source, reason);
  // Reachability analysis consumes source without expanding opaque macro
  // operands. Their dotted or cyclic data must not become executable code.
  evaluate(`(defmacro soundness-scanner-syntax (&rest syntax)
    (declare (ignore syntax)) (error "Reachability must not expand macros.")) (values)`);
  check(`(let ((operand (cons 'scanner-first 'scanner-rest)))
    (setf (cdr operand) operand)
    (multiple-value-bind (references barriers)
        (form-references (list 'soundness-scanner-syntax operand))
      (and barriers
        (member (list :literal (symbol-key 'scanner-first)) references :test #'equal) t)))`);
  check(`(and
    (nth-value 1 (form-references
      '(lambda (node) (loop :for (a . b) :in node :collect a))))
    (nth-value 1 (form-references
      '(macrolet ((pattern ((a . b)) (list 'quote a))) (pattern (1 . 2))))) t)`);
  for (const source of [
    "(form-references '(progn (+ 1 . 2)))",
    `(form-references '(flet ((soundness-scanner-syntax (x) x))
       (soundness-scanner-syntax (+ 1 . 2))))`,
    `(let ((form (list '+ 1))) (setf (cddr form) form) (form-references form))`,
  ]) reject(source, /dotted tail|circular list/);
  evaluate("(fmakunbound 'soundness-scanner-syntax) (values)");
  evaluate("(defmacro soundness-bad-expansion () '(let ((x 1 2)) x)) (values)");
  reject("(compile-native '(lambda () (soundness-bad-expansion)))", /binding/);
  for (const [mutation, reason] of [
    ['(setf (hir-function-parent function) function)', /parent chain/],
    ['(setf (hir-block-id block) -1)', /block record or ID/],
    ['(setf (hir-instruction-opcode instruction) :unknown)', /unknown opcode/],
    ['(setf (hir-instruction-safety-level instruction) -1)', /safety level/],
  ]) reject(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 1))))
      (function (first (hir-unit-functions unit)))
      (block (first (hir-function-blocks function)))
      (instruction (first (hir-block-instructions block))))
    ${mutation} (emit-native-module unit))`, reason);
  // Sharing and literal cycles are valid inputs, unlike cyclic executable code.
  check(`(let ((object (list :cycle)) (shared '(+ x 1)))
    (setf (cdr object) object)
    (and (eq object (funcall (compile-native (list 'lambda nil (list 'quote object)))))
      (equal (funcall (compile-native (list 'lambda '(x) (list 'list shared shared))) 1) '(2 2))))`);
  console.log('Soundness: compiler boundaries reject malformed graphs and recover.');

  // Group kinds determine the executable calling convention. They cannot be
  // forged into single-value certificates through the public HIR boundary.
  for (const [form, opcode] of [
    ['(lambda () (values nil nil))', ':values'],
    ['(lambda (f) (funcall f))', ':call'],
  ]) reject(`(let* ((unit (compile-to-hir '${form}))
      (function (first (hir-unit-functions unit)))
      (instruction (find ${opcode} (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
        (hir-function-blocks function)) :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (setf (hir-value-kind (hir-instruction-output instruction)) :single)
    (emit-native-module unit))`, /multiple-value group/);
  reject(`(let* ((unit (compile-to-hir '(lambda (x) (if x (values 1 2) (values 3 4)))))
      (function (first (hir-unit-functions unit)))
      (join (find-if (lambda (block) (hir-block-parameters block)) (hir-function-blocks function))))
    (setf (hir-value-kind (first (hir-block-parameters join))) :single)
    (emit-native-module unit))`, /multiple-value group|block arguments/);

  // The prover distinguishes zero values from one NIL and does not infer a
  // fixed result count from a union of different return counts.
  check(`(and (loop for count from 0 to 5 always
      (let* ((unit (compile-to-hir (list 'lambda nil (cons 'values (make-list count)))))
             (facts (hir-result-arities-checked unit)))
        (eql count (gethash (first (hir-unit-functions unit)) facts))))
    (let* ((unit (compile-to-hir '(lambda (x) (if x (values nil nil) (values)))))
           (facts (hir-result-arities-checked unit)))
      (null (gethash (first (hir-unit-functions unit)) facts))))`);
  // A closed consumer and a later dynamic caller share the same function.
  // Its fixed native results preserve padding, complete returns and identity.
  for (const count of [2, 3, 4]) {
    const values = Array(count).fill('y').join(' ');
    check(`(let* ((factory (compile-native '(lambda (escape x)
          (declare (optimize (debug 0)))
          (flet ((worker (y) (declare (optimize (debug 0))) (values ${values})))
            (declare (notinline worker))
            (if escape #'worker
              (multiple-value-bind (a b c d e) (worker x) (list a b c d e)))))))
        (worker (funcall factory t nil)))
      (and (equal (multiple-value-list (funcall worker 17)) (make-list ${count} :initial-element 17))
        (equal (funcall factory nil 19)
          (append (make-list ${count} :initial-element 19) (make-list (- 5 ${count}))))))`);
  }
  // Discarding native secondary results must retain operand order and errors.
  check(`(let ((effects nil) (function (compile-native '(lambda (x y effect)
        (declare (optimize (debug 0)))
        (flet ((pair (a b) (declare (inline car) (optimize (debug 0) (safety 1)))
                 (values a (car b))))
          (declare (notinline pair))
          (values (pair (progn (funcall effect :first) x)
                        (progn (funcall effect :second) y))))))))
    (and (= 17 (funcall function 17 '(18) (lambda (x) (push x effects))))
      (equal effects '(:second :first))
      (progn (setq effects nil)
        (handler-case (progn (funcall function 17 18 (lambda (x) (push x effects))) nil)
          (type-error () (equal effects '(:second :first)))))))`);

  // Macro headers are untrusted even when their bodies will be interpreted.
  // Conditions retain the input for inspection without printing cyclic data.
  for (const [prepare, reason] of [
    ["(setf (cddr binding) binding)", 'circular binding'],
    ["(setf binding (append binding '(:unknown 1)))", 'unrecognized keyword'],
  ]) {
    check(`(let ((binding (list 'stream "abc")))
      ${prepare}
      (handler-case (progn (macroexpand-1 (list 'with-input-from-string binding)) nil)
        (program-error (condition)
          (and (typep condition 'simple-condition)
            (eq binding (first (simple-condition-format-arguments condition)))
            (let ((message (princ-to-string condition)))
              (and (< (length message) 256) (search "${reason}" message) t))))))`);
    check('(eq (with-input-from-string (stream "ok") (read-char stream)) #\\o)');
  }

  // Native DO expanders reject every spine they traverse before entering the
  // unchecked CL worker, retaining the offending object without printing it.
  for (const [mutation, reason] of [
    ['(setf (cdddr form) :dotted offending form)', 'proper list'],
    ['(setf (cdddr form) form offending form)', 'circular list'],
    ["(setf form (list 'do) offending form)", 'variable and termination'],
    ['(setf (cdr bindings) :dotted offending bindings)', 'proper list'],
    ['(setf (cdr bindings) bindings offending bindings)', 'circular list'],
    ['(setf (cdddr binding) :dotted offending binding)', 'proper list'],
    ['(setf (cdddr binding) binding offending binding)', 'circular list'],
    ['(setf (cdddr binding) (list :extra) offending binding)', 'one to three'],
    ['(setf (car binding) 17 offending 17)', 'variable name'],
    ['(setf (cdr termination) :dotted offending termination)', 'proper list'],
    ['(setf (cdr termination) termination offending termination)', 'circular list'],
    ['(setf (third form) nil offending nil)', 'end test'],
  ]) {
    check(`(let* ((binding (list 'x 0 '(1+ x))) (bindings (list binding))
        (termination (list t :ok)) (form (list 'do bindings termination)) (offending nil))
      ${mutation}
      (handler-case (progn (macroexpand-1 form) nil)
        (program-error (condition)
          (and (typep condition 'simple-condition)
            (eq offending (first (simple-condition-format-arguments condition)))
            (let ((message (princ-to-string condition)))
              (and (< (length message) 256) (search "DO/DO*" message)
                (search "${reason}" message) t))))))`);
    check(`(equal (multiple-value-list
      (do ((x 0 (1+ x))) ((= x 3) (values x :ok)))) '(3 :ok))`);
  }

  // Resolve the current private bootstrap identity directly. Macro functions
  // may omit their source through FUNCTION-LAMBDA-EXPRESSION.
  evaluate(`(defparameter *soundness-iteration-helper*
    (compiler-primitive 10 1 "%ITERATION-EXPANSION")) (values)`);
  for (const mutation of [
    '(setf body :dotted)',
    '(setf (cdr body) :dotted)',
    '(setf (cdr body) body)',
  ]) {
    check(`(let ((body (list '(identity x))))
      ${mutation}
      (handler-case (progn (funcall *soundness-iteration-helper* 'x 'source nil body nil) nil)
        (program-error (condition)
          (and (typep condition 'simple-condition)
            (eq body (first (simple-condition-format-arguments condition)))
            (< (length (princ-to-string condition)) 256)))))`);
    check(`(equal (multiple-value-list (dolist (x '(1 2) (values x :recovered))))
      '(nil :recovered))`);
  }
  for (const variable of ['17', 'nil', ':constant']) {
    check(`(handler-case
      (progn (funcall *soundness-iteration-helper* ${variable} 'source nil nil nil) nil)
      (program-error (condition)
        (and (typep condition 'simple-condition)
          (eql ${variable} (first (simple-condition-format-arguments condition))))))`);
    recover();
  }

  evaluate(`(defun soundness-proven-result (source)
    (let* ((unit (compile-to-hir source))
           (facts (infer-hir-types-checked unit nil nil nil t)))
      (gethash (first (hir-unit-functions unit)) facts t))) (values)`);
  for (const source of [
    '(lambda (x) (declare (type (eql 7) x)) x)',
    '(lambda (x) (the (eql 7) x))',
    `(lambda () (declare (ftype (function () (eql 7)) soundness-external)) (soundness-external))`,
    '(lambda () (let ((x 7)) (funcall (lambda () (setq x 8))) x))',
    '(lambda (flag) (let ((x 7)) (when flag (setq x 8)) x))',
  ]) check(`(not (hir-type-proves-p (soundness-proven-result '${source}) '(eql 7)))`);
  check("(hir-type-proves-p (soundness-proven-result '(lambda () 7)) '(eql 7))");
  evaluate(`(deftype soundness-opaque () (error "Proof expanded an opaque DEFTYPE"))
    (defun soundness-predicate (value) (declare (ignore value))
      (error "Proof executed a SATISFIES predicate")) (values)`);
  check(`(and (not (hir-type-proves-p 'soundness-opaque 'integer))
    (not (hir-type-proves-p '(satisfies soundness-predicate) 'integer))
    (hir-type-proves-p '(and integer (satisfies soundness-predicate)) 'integer))`);
  // An exhausted proof budget supplies no proof, even for a true relationship.
  check(`(and (not (hir-type-proves-p 'integer 'integer 0))
    (not (hir-type-proves-p 'integer 'integer 32 (list 0))))`);
  // Negative facts use target disjointness, including the overlap of NIL's
  // symbol and list types. Unknown alternatives cannot prove a complement.
  check(`(and
    (every (lambda (case) (hir-type-proves-p (first case) (second case)))
      '((cons (not vector)) (symbol (not number)) (null (not cons))
        (list (not number)) ((or cons string) (not number))
        ((and cons soundness-opaque) (not vector))
        ((member nil :key "text") (not number))
        (cons (not (or number character)))
        (symbol (not (and number integer)))))
    (every (lambda (case) (not (hir-type-proves-p (first case) (second case))))
      '((null (not symbol)) (list (not symbol)) (sequence (not symbol))
        ((or cons number) (not number)) (soundness-opaque (not number))
        ((or cons soundness-opaque) (not vector))
        ((satisfies soundness-predicate) (not number))))
    (not (hir-type-proves-p 'cons '(not vector) 0))
    (not (hir-type-proves-p 'cons '(not vector) 32 (list 0))))`);
  for (const [type, value, predicate] of [
    ['cons', '(cons 1 2)', 'vectorp'],
    ['symbol', ':value', 'integerp'],
    ['simple-vector', '#(1)', 'consp'],
    ['character', '#\\a', 'symbolp'],
  ]) {
    check(`(let ((effects 0)
        (function (compile-native '(lambda (value effect)
          (declare (type ${type} value) (inline ${predicate})
            (optimize (safety 0) (debug 0)))
          (if (${predicate} (progn (funcall effect) value))
            :wrong (values :disjoint nil :extra))))))
      (and (equal (multiple-value-list
          (funcall function ${value} (lambda () (incf effects))))
        '(:disjoint nil :extra)) (= effects 1)))`);
  }
  // A declaration alone cannot remove a checked predicate's other outcome.
  for (const safety of [1, 2, 3]) {
    check(`(let ((function (compile-native '(lambda (value)
      (declare (type cons value) (inline integerp)
        (optimize (safety ${safety}) (debug 0)))
      (if (integerp value) :integer :other)))))
      (and (eq (funcall function 7) :integer)
        (eq (funcall function '(7)) :other)))`);
  }
  // A nonnumeric operand permits identity comparison; a union containing
  // numbers must retain value comparison. Both operands keep their effects.
  for (const [type, left, right, expected] of [
    ['cons', 'object', 'object', 't'],
    ['cons', 'object', '(copy-list object)', 'nil'],
    ['(or cons number)', '(parse-integer text)', '(parse-integer text)', 't'],
  ]) {
    check(`(let* ((object (list :value)) (text "18446744073709551617") (effects nil)
        (function (compile-native '(lambda (left right effect)
          (declare (type ${type} left) (inline eql)
            (optimize (safety 0) (debug 0)))
          (values (eql (progn (funcall effect :left) left)
                       (progn (funcall effect :right) right)) :extra)))))
      (and (equal (multiple-value-list
          (funcall function ${left} ${right} (lambda (tag) (push tag effects))))
        '(${expected} :extra)) (equal effects '(:right :left))))`);
  }
  // Variable shift proofs must enclose every result, including negative
  // operands and counts. Leaving signed i64 supplies no scalar proof.
  check(`(and
    (every (lambda (case)
      (let* ((operand (first case)) (count (second case))
             (result (hir-inline-result-type 'ash (list operand count))))
        (and (hir-type-proves-p result '(signed-byte 64))
          (loop for value from (second operand) to (third operand) always
            (loop for shift from (second count) to (third count) always
              (typep (ash value shift) result))))))
      '(((integer -7 -1) (integer -3 3))
        ((integer -7 7) (integer -3 3))
        ((integer 0 7) (integer -3 3))))
    (hir-type-proves-p
      (hir-inline-result-type 'ash '((integer -1 0) (integer 0 63))) '(signed-byte 64))
    (not (hir-type-proves-p
      (hir-inline-result-type 'ash '((integer 1 1) (integer 0 63))) '(signed-byte 64))))`);
  for (const [countType, count, expected] of [
    ['(integer 0 5)', 3, -136], ['(integer -5 0)', -2, -5],
  ]) {
    check(`(let ((effects nil)
        (function (compile-native '(lambda (value count effect)
          (declare (type (integer -100 100) value) (type ${countType} count)
            (inline ash) (optimize (safety 0) (debug 0)))
          (values (ash (progn (funcall effect :left) value)
                       (progn (funcall effect :right) count)) :extra)))))
      (and (equal (multiple-value-list
          (funcall function -17 ${count} (lambda (value) (push value effects))))
        '(${expected} :extra))
        (equal effects '(:right :left))))`);
  }
  // Non-NIL proofs share the target's numeric/array rules. General sequences
  // and symbols can contain NIL; an opaque restriction supplies no new fact.
  check(`(and
    (every (lambda (type) (hir-type-proves-p type '(not null)))
      '(bignum ratio bit-vector (simple-bit-vector 8) base-string
        (simple-array bit (8)) (and bit-vector (satisfies soundness-predicate))))
    (every (lambda (type) (not (hir-type-proves-p type '(not null))))
      '(null list symbol sequence (or null bit-vector) soundness-opaque
        (satisfies soundness-predicate))))`);
  check(`(let ((effects 0)
      (function (compile-native '(lambda (value effect)
        (declare (type bit-vector value) (inline null)
          (optimize (safety 0) (debug 0)))
        (values (null (progn (funcall effect) value)) :kept)))))
    (and (equal (multiple-value-list
        (funcall function #* (lambda () (incf effects)))) '(nil :kept))
      (= effects 1)))`);
  // A direct truth proof removes only the test, preserving its effects and
  // the selected arm's entire value group. Mutable cells keep both outcomes.
  for (const [type, value, expected] of [
    ['bit-vector', '#*', '(:present nil :extra)'],
    ['ratio', '1/2', '(:present nil :extra)'],
    ['null', 'nil', '(:missing :extra)'],
    ['(or null bit-vector)', 'nil', '(:missing :extra)'],
    ['(or null bit-vector)', '#*', '(:present nil :extra)'],
  ]) {
    check(`(let ((effects 0)
        (function (compile-native '(lambda (value effect)
          (declare (type ${type} value) (optimize (safety 0) (debug 0)))
          (if (progn (funcall effect) value)
            (values :present nil :extra) (values :missing :extra))))))
      (and (equal (multiple-value-list
          (funcall function ${value} (lambda () (incf effects)))) '${expected})
        (= effects 1)))`);
  }
  check(`(let ((function (compile-native '(lambda (replace)
      (declare (optimize (safety 0) (debug 0)))
      (let ((value #*))
        (funcall (lambda () (when replace (setq value nil))))
        (if value (values :present :extra) (values :missing)))))))
    (and (equal (multiple-value-list (funcall function nil)) '(:present :extra))
      (equal (multiple-value-list (funcall function t)) '(:missing))))`);
  // Executed truth tests establish facts at checked safeties too. Their
  // extent is limited by real writes, capture and control-flow joins, rather
  // than by declared parameter types or the result of an ordinary callee.
  for (const safety of [1, 2, 3]) {
    check(`(let ((effects 0)
        (function (compile-native '(lambda (value effect)
          (declare (optimize (safety ${safety}) (debug 0)))
          (if value
            (progn (funcall effect)
              (if value (values :present nil :extra) :wrong))
            (progn (funcall effect)
              (if value :wrong (values :missing :extra))))))))
      (and (equal (multiple-value-list
          (funcall function :value (lambda () (incf effects)))) '(:present nil :extra))
        (equal (multiple-value-list
          (funcall function nil (lambda () (incf effects)))) '(:missing :extra))
        (= effects 2)))`);
    check(`(let ((function (compile-native '(lambda (value)
        (declare (optimize (safety ${safety}) (debug 0)))
        (if value
          (progn (setq value nil) (if value :wrong (values :cleared :extra)))
          (progn (setq value :new) (if value (values :set :extra) :wrong)))))))
      (and (equal (multiple-value-list (funcall function t)) '(:cleared :extra))
        (equal (multiple-value-list (funcall function nil)) '(:set :extra))))`);
    check(`(let ((function (compile-native '(lambda (value)
        (declare (optimize (safety ${safety}) (debug 0)))
        (let ((change (lambda () (setq value (not value)))))
          (if value
            (progn (funcall change) (if value :wrong :cleared))
            (progn (funcall change) (if value :set :wrong))))))))
      (and (eq (funcall function t) :cleared)
        (eq (funcall function nil) :set)))`);
    check(`(let ((function (compile-native '(lambda (value observe)
        (declare (optimize (safety ${safety}) (debug 0)))
        (when observe (if value :present :missing))
        (if value (values :present :extra) (values :missing :extra))))))
      (and (equal (multiple-value-list (funcall function t nil)) '(:present :extra))
        (equal (multiple-value-list (funcall function nil nil)) '(:missing :extra))
        (equal (multiple-value-list (funcall function t t)) '(:present :extra))
        (equal (multiple-value-list (funcall function nil t)) '(:missing :extra))))`);
  }
  // A false canonical predicate establishes a complement for later reads.
  // Writes, captured aliases and paths which skipped the test end that fact.
  for (const safety of [0, 3]) {
    for (const change of [
      '(setq value (list :new))',
      '(funcall (lambda () (setq value (list :new))))',
    ]) {
      check(`(let ((effects 0)
          (function (compile-native '(lambda (value effect)
            (declare (inline consp) (optimize (safety ${safety}) (debug 0)))
            (if (consp value) :wrong
              (progn (funcall effect)
                (unless (consp value) ${change})
                (if (consp value) (values :changed nil :extra) :wrong)))))))
        (and (equal (multiple-value-list
            (funcall function :old (lambda () (incf effects))))
          '(:changed nil :extra)) (= effects 1)))`);
    }
    check(`(let ((function (compile-native '(lambda (value observe)
      (declare (inline consp) (optimize (safety ${safety}) (debug 0)))
      (when observe (if (consp value) :cons :atom))
      (if (consp value) (values :cons nil :extra) (values :atom :extra))))))
      (every (lambda (observe)
        (and (equal (multiple-value-list (funcall function '(1) observe)) '(:cons nil :extra))
          (equal (multiple-value-list (funcall function :atom observe)) '(:atom :extra))))
        '(nil t)))`);
  }
  // Proper-spine proofs describe current objects, not the CL LIST type.
  // Canonical constructors retain fresh spines, operand order and all values.
  for (const safety of [0, 1, 2, 3]) {
    for (const constructor of [
      '(list (funcall effect :first) (funcall effect :second))',
      '(cons (funcall effect :first) (cons (funcall effect :second) nil))',
    ]) {
      check(`(let ((effects nil)
          (function (compile-native '(lambda (effect)
            (declare (inline list cons) (optimize (safety ${safety}) (debug 0)))
            (apply (lambda (&rest arguments) (values arguments :extra)) ${constructor})))))
        (flet ((effect (tag) (push tag effects)
                 (values (if (eq tag :first) 11 22) :discarded)))
          (let* ((first (multiple-value-list (funcall function #'effect)))
                 (second (multiple-value-list (funcall function #'effect))))
            (and (equal first '((11 22) :extra)) (equal second first)
              (not (eq (car first) (car second)))
              (equal effects '(:second :first :second :first))))))`);
    }
    // A successful traversal cannot justify another after aliased mutation,
    // whether the mutator is visible, inline, or an unknown callback.
    for (const mutation of [
      '(rplacd alias :dotted)',
      '(rplacd alias arguments)',
      '(funcall mutate alias)',
    ]) {
      check(`(let* ((arguments (list 1 2))
          (function (compile-native '(lambda (arguments mutate)
            (declare (inline list cons rplacd)
              (optimize (safety ${safety}) (debug 0)))
            (apply (lambda (&rest checked) (declare (ignore checked)) nil) arguments)
            (let ((alias arguments)) ${mutation})
            (apply (lambda (&rest result) result) arguments)))))
        (and (handler-case
               (progn (funcall function arguments
                 (lambda (alias) (rplacd alias :dotted))) nil)
               (type-error (condition)
                 (eq (type-error-datum condition)
                   ${mutation === '(rplacd alias arguments)' ? 'arguments' : ':dotted'})))
          (progn (rplacd arguments nil)
            (equal (apply (lambda (&rest result) result) arguments) '(1)))))`);
    }
    check(`(let ((function (compile-native '(lambda (arguments)
        (declare (type list arguments) (optimize (safety ${safety}) (debug 0)))
        (apply (lambda (&rest result) result) arguments)))))
      (let ((arguments (cons 1 :dotted)))
        (handler-case (progn (funcall function arguments) nil)
          (type-error (condition) (eq (type-error-datum condition) :dotted)))))`);
    recover();
  }
  // The primary value's proof cannot flow to another value or across a join.
  for (const body of [
    `(multiple-value-bind (proper arguments) (values (list 1 2) supplied)
       (declare (ignore proper)) (apply (lambda (&rest result) result) arguments))`,
    `(let ((arguments (list 1 2)))
       (when supplied (rplacd arguments :dotted))
       (apply (lambda (&rest result) result) arguments))`,
    `(let ((arguments (list 1 2)))
       (let ((mutate (lambda () (rplacd arguments :dotted))))
         (funcall mutate))
       (apply (lambda (&rest result) result) arguments))`,
  ]) {
    check(`(let ((function (compile-native '(lambda (supplied)
        (declare (inline list cons rplacd) (optimize (safety 0) (debug 0))) ${body}))))
      (handler-case (progn (funcall function :dotted) nil) (type-error () t)))`);
  }

  // ANSI leaves redefining CL primitives undefined. MIRICL explicitly supports
  // it unless an INLINE/sealed contract permits capturing the old definition.
  check(`(let ((function (compile-native '(lambda (v i)
      (declare (optimize (safety 0)) (type simple-vector v) (fixnum i))
      (list (svref v i))))) (original (symbol-function 'svref)))
    (and (equal (funcall function #(42) 0) '(42))
      (unwind-protect
        (progn (setf (symbol-function 'svref)
          (lambda (v i) (declare (ignore v i)) (values :replacement :extra)))
          (equal (funcall function #(42) 0) '(:replacement)))
        (setf (symbol-function 'svref) original))
      (equal (funcall function #(42) 0) '(42))))`);
  check(`(let* ((ordinary (compile-native '(lambda (x) (list x))))
      (indirect (compile-native '(lambda (x) (declare (notinline list)) (list x))))
      (captured (compile-native '(lambda (x) (declare (inline list)) (list x))))
      (module (compile-native-module
        '((defun module-list-default (x) (list x))
          (defun module-list-notinline (x) (declare (notinline list)) (list x))
          (defun module-list-inline (x) (declare (inline list)) (list x)))
        :inline nil))
      (module-ordinary (cdr (assoc 'module-list-default module)))
      (module-indirect (cdr (assoc 'module-list-notinline module)))
      (module-captured (cdr (assoc 'module-list-inline module)))
      (ordinary-apply (compile-native '(lambda (x)
        (apply (lambda (&rest result) result) (list x)))))
      (captured-apply (compile-native '(lambda (x) (declare (inline list))
        (apply (lambda (&rest result) result) (list x)))))
      (replacement (compile-native '(lambda (x) (declare (ignore x)) (values :replacement :extra))))
      (*wasm-builtin-codes* (make-hash-table :test 'eq))
      (canonical (native-canonical-builtin-code 'list))
      (original (symbol-function 'list)))
    (unwind-protect
      (progn (setf (symbol-function 'list) replacement)
        (and (equal (multiple-value-list (funcall ordinary 1)) '(:replacement :extra))
          (equal (multiple-value-list (funcall indirect 2)) '(:replacement :extra))
          (equal (multiple-value-list (funcall captured 3)) '((3)))
          (equal (multiple-value-list (funcall module-ordinary 1)) '(:replacement :extra))
          (equal (multiple-value-list (funcall module-indirect 2)) '(:replacement :extra))
          (equal (multiple-value-list (funcall module-captured 3)) '((3)))
          (handler-case (progn (funcall ordinary-apply 4) nil)
            (type-error (condition) (eq (type-error-datum condition) :replacement)))
          (equal (funcall captured-apply 5) '(5))
          ;; Query the per-emission eligibility check directly: interpreted
          ;; compiler macros and MEMBER themselves depend on live LIST.
          (not (= (native-builtin-code 'list) canonical))))
      (setf (symbol-function 'list) original)))`);
  for (const count of [9, 129]) {
    const parameters = Array.from({length: count}, (_, index) => `argument${index}`).join(' ');
    const operands = Array.from({length: count}, () => 'x').join(' ');
    check(`(let ((ordinary (compile-native '(lambda (x) (list ${operands}))))
        (indirect (compile-native '(lambda (x) (declare (notinline list)) (list ${operands}))))
        (captured (compile-native '(lambda (x)
          (declare (inline list car length) (optimize (safety 0)))
          (let ((result (list ${operands}))) (values (car result) (length result))))))
        (replacement (compile-native '(lambda (${parameters})
          (declare (ignore ${parameters})) (values :replacement :extra))))
        (original (symbol-function 'list)))
      (unwind-protect
        (progn (setf (symbol-function 'list) replacement)
          (and (equal (multiple-value-list (funcall ordinary 1)) '(:replacement :extra))
            (equal (multiple-value-list (funcall indirect 2)) '(:replacement :extra))
            (equal (multiple-value-list (funcall captured 3)) '(3 ${count}))))
        (setf (symbol-function 'list) original)))`);
  }
  recover();
  console.log('Soundness: proofs remain conservative across declarations, mutation and opaque types; primitive guards honor redefinition.');

  // Constants used after recursion must not create large retained frames.
  // This case overflowed the ordinary browser-compatible stack when every
  // repeated function name was cached for the entire native invocation.
  const names = Array.from({length: 80}, (_, index) => `soundness-effect-${index}`);
  const repetitions = 8, depth = 1024;
  const calls = Array.from({length: repetitions}, () => names.map(name => `(${name} n)`).join(' ')).join('\n');
  evaluate(`(defparameter *soundness-effects* 0)
    (setf (fdefinition 'soundness-effect) (compile-native
      '(lambda (x) (declare (ignore x)) (incf *soundness-effects*) nil)))
    (dolist (name '(${names.join(' ')})) (setf (symbol-function name) #'soundness-effect))
    (setf (fdefinition 'soundness-recur) (compile-native
      '(lambda (n) (declare (type (integer 0 4096) n) (inline zerop 1- 1+)
                           (optimize (safety 0) (debug 0)))
         (if (zerop n) 0
           (let ((answer (soundness-recur (1- n)))) ${calls} (1+ answer)))))) (values)`);
  assert.equal((await vm.evaluateAsync(`(let ((answer (soundness-recur ${depth})))
    (and (= answer ${depth}) (= *soundness-effects* ${depth * names.length * repetitions})))`,
    {eof: true})).output, 'T\n');
  // Signaling can also reenter a function while its failing frame is retained.
  evaluate(`(defparameter *soundness-reentry* (compile-native
    '(lambda (fail) (declare (special *soundness-unbound*))
       (if fail *soundness-unbound* (values :kept :kept :kept :kept :kept :kept))))) (values)`);
  check(`(equal (block done
    (handler-bind ((unbound-variable (lambda (condition) (declare (ignore condition))
      (return-from done (multiple-value-list (funcall *soundness-reentry* nil))))))
      (funcall *soundness-reentry* t))) '(:kept :kept :kept :kept :kept :kept))`);
  console.log('Soundness: native recursion preserves stack depth and effects; condition handlers can reenter with multiple values.');

  vm.mountFile('/soundness.lisp', `(in-package :cl-user)
    (defparameter *soundness-graph* '#1=("λ𝄞" #2=(:shared) #2# . #1#))
    (defparameter *soundness-functions*
      '#.(funcall (miricl-compiler:compile-hir
          '(lambda (initial)
            (let ((n initial) (object (list :kept)))
              (labels ((sum (x) (if (zerop x) n (+ x (sum (1- x))))))
                (vector #'sum #'sum (lambda (delta) (incf n delta))
                        object (constantly object)))))) 40))`);
  check('(pathnamep (compile-file "/soundness.lisp" :verbose nil))');
  const bytes = await vm.readFile('/soundness.wasm');
  assert.ok(WebAssembly.validate(bytes), 'Current FASLs are valid Wasm containers.');
  const graphCheck = `(and (load "/soundness.wasm")
    (let ((graph cl-user::*soundness-graph*))
      (and (string= (first graph) "λ𝄞") (eq (second graph) (third graph))
        (eq graph (cdddr graph))))
    (let ((graph cl-user::*soundness-functions*))
      (and (eq (svref graph 0) (svref graph 1))
           (= (funcall (svref graph 0) 3) 46)
           (= (funcall (svref graph 2) 2) 42)
           (= (funcall (svref graph 0) 0) 42)
           (eq (svref graph 3) (funcall (svref graph 4) :arguments)))))`;
  check(graphCheck);
  for (const invalid of [
    "(vector '(5 0 9 0))", "(vector '(12 0 0 0 7 0))",
    "(vector '(12 1 9 0 -1 0))", "(let ((record (list 0))) (setf (cdr record) record) (vector record))",
  ]) {
    check(`(and (handler-case (progn (validate-fasl-graph '(0) ${invalid}) nil)
                  (program-error (condition) (not (null (princ-to-string condition)))))
                (= (+ 20 22) 42))`);
  }
  check(`(let ((shell (compiler-primitive 50 0 (vector 0 0) nil)))
    (and (handler-case (progn (funcall shell) nil) (program-error () t))
         (handler-case (progn (compiler-primitive 50 1 shell (vector 0 (vector) 0 (vector) nil)) nil)
           (program-error (condition) (not (null (princ-to-string condition)))))
         (= (+ 20 22) 42)))`);
  // Derive Wasm section offsets from the produced file; no fixed section
  // inventory, object count, or encoded-size expectation is needed.
  let cursor = 8, payloadStart, payloadEnd;
  const readULEB = () => {
    let value = 0, shift = 0, octet;
    do {octet = bytes[cursor++]; value += (octet & 127) * 2 ** shift; shift += 7;} while (octet & 128);
    return value;
  };
  while (cursor < bytes.length) {
    const kind = bytes[cursor++], length = readULEB(), end = cursor + length;
    if (kind === 0) {
      const nameLength = readULEB();
      const name = new TextDecoder().decode(bytes.subarray(cursor, cursor + nameLength));
      cursor += nameLength;
      if (name === 'miricl.fasl') {payloadStart = cursor; payloadEnd = end; break;}
    }
    cursor = end;
  }
  assert.ok(payloadStart !== undefined, 'The compiled file contains its current FASL payload.');
  const corruptions = [
    ['magic', copy => {copy[payloadStart] ^= 255;}],
    ['directory offset', copy => {
      // The current FASL header is five u32s, then four-u32 directory entries:
      // kind, key, payload-relative offset and size. Corrupt the first offset.
      const headerBytes = 5 * Uint32Array.BYTES_PER_ELEMENT;
      const offsetField = 2 * Uint32Array.BYTES_PER_ELEMENT;
      new DataView(copy.buffer, copy.byteOffset + payloadStart)
        .setUint32(headerBytes + offsetField, 0xffffffff, true);
    }],
    ['truncation', copy => copy.subarray(0, payloadEnd - 1)],
  ];
  for (const [name, mutate] of corruptions) {
    const copy = bytes.slice(), broken = mutate(copy) ?? copy;
    vm.mountFile('/soundness-bad.wasm', broken);
    const output = evaluate(`(handler-case (progn (load "/soundness-bad.wasm") :unexpected-acceptance)
      (error (condition) (princ-to-string condition)))`);
    assert.doesNotMatch(output, /:UNEXPECTED-ACCEPTANCE/, name);
    assert.ok(output.trim().length > 2, `${name}: Lisp condition has a diagnostic.`);
    check(graphCheck);
    recover();
  }
  console.log('Soundness: current FASLs preserve shared/cyclic literals and reject corruption without poisoning later loads.');
} finally {vm.close();}
