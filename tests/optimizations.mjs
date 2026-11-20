// Observable compiler capabilities beyond ANSI conformance. WASI cannot load
// generated code at runtime and is exempt from this entire optimization suite.
// node tests/optimizations.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const [image = 'miricl.wasm', ...extra] = process.argv.slice(2);
if (extra.length) throw new Error('Expected [native IMAGE.wasm]; WASI optimization tests are excluded.');
const vm = await Miricl.instantiate(await fs.readFile(image));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = (name, source) => assert.equal(evaluate(source), 'T\n', name);
const define = (name, source) => evaluate(`(setf (symbol-function '${name})
  (miricl-compiler:compile-native '${source} :name '${name})) (values)`);
// This depth exceeds ordinary host stacks. Success, not a timing or code-shape
// threshold, establishes the tail-call capability.
const depth = 100000;
const policy = '(declare (optimize (speed 1) (space 3) (debug 0) (safety 1)))';
try {
  evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  define('optimization-tail-count', `(lambda (n acc) ${policy}
    (if (zerop n) (values acc nil :done)
      (optimization-tail-count (1- n) (1+ acc))))`);
  check('deep self tail calls preserve multiple values',
    `(equal (multiple-value-list (optimization-tail-count ${depth} 0)) '(${depth} nil :done))`);
  define('optimization-tail-even', `(lambda (n) ${policy}
    (if (zerop n) t (optimization-tail-odd (1- n))))`);
  define('optimization-tail-odd', `(lambda (n) ${policy}
    (if (zerop n) nil (optimization-tail-even (1- n))))`);
  check('mutual tail calls', `(and (optimization-tail-even ${depth})
    (not (optimization-tail-odd ${depth})))`);
  define('optimization-tail-four', `(lambda (n acc token empty) ${policy}
    (if (zerop n)
        (if empty (values) (values acc nil token :four 5 6 7 8 9))
        (optimization-tail-four (1- n) (1+ acc) token empty)))`);
  check('deep four-argument calls preserve zero and overflow values', `(and
    (equal (multiple-value-list (optimization-tail-four ${depth} 0 :retained nil))
           '(${depth} nil :retained :four 5 6 7 8 9))
    (null (multiple-value-list (optimization-tail-four ${depth} 0 :retained t))))`);
  define('optimization-tail-indirect', `(lambda (n function) ${policy}
    (if (zerop n) (values 42 :indirect) (funcall function (1- n) function)))`);
  check('indirect tail calls through function and symbol designators', `(and
    (equal (multiple-value-list (optimization-tail-indirect ${depth} #'optimization-tail-indirect)) '(42 :indirect))
    (equal (multiple-value-list (optimization-tail-indirect ${depth} 'optimization-tail-indirect)) '(42 :indirect)))`);
  // File preparation must retain indirect tail calls and the ordinary SETF
  // calling convention when its resolved forms become native Wasm.
  evaluate('(defpackage :optimization-shadow (:use :cl) (:shadow :funcall)) (values)');
  vm.mountFile('/optimization-prepared.lisp', `
    (in-package :cl-user)
    (defun optimization-prepared-tail (n function) ${policy}
      (if (zerop n) (values 42 nil :prepared)
        (funcall function (1- n) function)))
    (defun optimization-prepared-call (function) ${policy}
      (let ((effects nil))
        (values (multiple-value-list
          (funcall (progn (push :function effects) (values function :ignored))
            (progn (push :argument effects) (values 40 :ignored))))
          (reverse effects))))
    (defun optimization-prepared-store (bits) ${policy}
      (let ((effects nil))
        (let ((value (setf (sbit (progn (push :array effects) bits)
                           (progn (push :index effects) 0))
                      (progn (push :value effects) 1))))
          (values value (reverse effects)))))
    (defun optimization-prepared-shadow () ${policy}
      (flet ((optimization-shadow::funcall (x) (values (+ x 2) :shadow)))
        (optimization-shadow::funcall 40)))`);
  evaluate(`(compile-file "/optimization-prepared.lisp" :verbose nil :block-compile nil)
    (miricl-compiler:compile-native-bundle "/optimization-prepared.wasm" "/optimization-prepared-native.wasm")
    (load "/optimization-prepared-native.wasm") (values)`);
  check('prepared indirect tail calls preserve depth and multiple values', `(and
    (equal (multiple-value-list (optimization-prepared-tail ${depth} #'optimization-prepared-tail)) '(42 nil :prepared))
    (equal (multiple-value-list (optimization-prepared-tail ${depth} 'optimization-prepared-tail)) '(42 nil :prepared)))`);
  check('prepared indirect calls preserve operands, values and lexical shadowing', `(and
    (equal (multiple-value-list (optimization-prepared-call (lambda (x) (values (+ x 2) nil :tail))))
      '((42 nil :tail) (:function :argument)))
    (equal (multiple-value-list (optimization-prepared-call (lambda (x) (declare (ignore x)) (values))))
      '(nil (:function :argument)))
    (equal (multiple-value-list (optimization-prepared-shadow)) '(42 :shadow))
    (let ((bits (make-array 1 :element-type 'bit :initial-element 0)))
      (and (equal (multiple-value-list (optimization-prepared-store bits)) '(1 (:array :index :value)))
           (= (sbit bits 0) 1))))`);
  // ANSI covers the individual binding/value rules. Exercise their composition
  // through file preparation, canonicalization and native bundle conversion.
  evaluate(`(shadow '(values multiple-value-bind) :optimization-shadow)
    (defvar *optimization-prepared-binding* :outside) (values)`);
  vm.mountFile('/optimization-prepared-values.lisp', `
    (in-package :cl-user)
    (defmacro optimization-prepared-expand (form) form)
    (defun optimization-prepared-components (flag effect) ${policy}
      (optimization-prepared-expand
        (multiple-value-bind (a b c d e f)
            (block parts
              (if flag (values (funcall effect :first) nil 3 4 5 6)
                (return-from parts (values (funcall effect :second) nil 30 40 50 60))))
          (values f e d c b a))))
    (defun optimization-prepared-receive (producer cleanup) ${policy}
      (multiple-value-bind (a b c d e f)
          (unwind-protect (funcall producer) (funcall cleanup))
        (values a b c d e f)))
    (defun optimization-prepared-mixed (mode) ${policy}
      (multiple-value-bind (a b c d e f)
          (if mode (if (eq mode :few) (values nil 2) (values 1 nil 3 4 5 6 7))
            (values))
        (if (eq mode :empty) (values) (values a b c d e f))))
    (defun optimization-prepared-scope () ${policy}
      (symbol-macrolet ((a :outer))
        (multiple-value-bind (a b) (values :bound a)
          (flet ((defaults (&optional (x a xp) &key (y b yp))
                   (values x xp y yp)))
            (values (multiple-value-list (defaults))
              (lambda () (values a b)) (setq a :changed))))))
    (defun optimization-prepared-special (callback) ${policy}
      (multiple-value-bind (*optimization-prepared-binding* ignored)
          (values :inside 10)
        (declare (ignore ignored))
        (funcall callback)))
    (defun optimization-prepared-value-shadow () ${policy}
      (flet ((optimization-shadow::values (x) (values x :local)))
        (macrolet ((optimization-shadow::multiple-value-bind (x)
                     (list 'optimization-shadow::values x)))
          (optimization-shadow::multiple-value-bind 42))))`);
  for (const blockCompile of ['nil', 't']) {
    evaluate(`(compile-file "/optimization-prepared-values.lisp" :verbose nil
        :block-compile ${blockCompile})
      (defmacro optimization-prepared-expand (form)
        (declare (ignore form)) (error "Prepared code must retain its expansion."))
      (miricl-compiler:compile-native-bundle "/optimization-prepared-values.wasm"
        "/optimization-prepared-values-native.wasm")
      (load "/optimization-prepared-values-native.wasm") (values)`);
    check('prepared branch values retain components and operand effects', `(every
      (lambda (flag)
        (let ((effects nil))
          (and (equal (multiple-value-list
                  (optimization-prepared-components flag
                    (lambda (event) (push event effects) (values event :discarded))))
                (if flag '(6 5 4 3 nil :first) '(60 50 40 30 nil :second)))
            (equal effects (if flag '(:first) '(:second)))))) '(nil t))`);
    check('prepared binding survives cleanup with zero, few and overflow values', `(every
      (lambda (returned)
        (let ((effects nil))
          (and (equal (multiple-value-list
                  (optimization-prepared-receive
                    (lambda () (push :producer effects) (values-list returned))
                    (lambda () (push :cleanup effects) (values :discarded :extra))))
                (subseq (append returned (make-list 6)) 0 6))
            (equal (reverse effects) '(:producer :cleanup)))))
      '(nil (nil) (1 nil 3 4 5 6 7)))`);
    check('prepared branches retain differing counts and zero-value results', `(every
      (lambda (case)
        (equal (multiple-value-list (optimization-prepared-mixed (car case))) (cdr case)))
      '((nil nil nil nil nil nil nil) (:few nil 2 nil nil nil nil)
        (:many 1 nil 3 4 5 6) (:empty)))`);
    check('prepared cleanup retains nonlocal values and recovers after errors', `(let ((effects nil))
      (and (equal (multiple-value-list
              (catch 'optimization-prepared-exit
                (optimization-prepared-receive
                  (lambda () (throw 'optimization-prepared-exit (values nil :escaped 3 4 5 6)))
                  (lambda () (push :throw-cleanup effects))))) '(nil :escaped 3 4 5 6))
        (handler-case
            (progn (optimization-prepared-receive (lambda () (error "Expected failure"))
                     (lambda () (push :error-cleanup effects))) nil)
          (error () t))
        (equal effects '(:error-cleanup :throw-cleanup))
        (equal (multiple-value-list
                (optimization-prepared-receive (lambda () (values 42)) (lambda () (values))))
          '(42 nil nil nil nil nil))))`);
    check('prepared binding scopes retain defaults, captures and special restoration', `(and
      (multiple-value-bind (defaults closure changed) (optimization-prepared-scope)
        (and (equal defaults '(:bound nil :outer nil)) (eq changed :changed)
          (equal (multiple-value-list (funcall closure)) '(:changed :outer))))
      (let ((*optimization-prepared-binding* :outside))
        (and (equal (multiple-value-list
                (optimization-prepared-special
                  (lambda () (values *optimization-prepared-binding* nil :seen))))
              '(:inside nil :seen))
          (eq *optimization-prepared-binding* :outside)))
      (equal (multiple-value-list (optimization-prepared-value-shadow)) '(42 :local)))`);
  }
  define('optimization-tail-factory', `(lambda (offset) ${policy}
    (labels ((walk (n acc) (if (zerop n) (+ offset acc) (walk (1- n) (1+ acc))))) #'walk))`);
  check('tail-recursive escaped closure',
    `(= (funcall (optimization-tail-factory 7) ${depth} 0) (+ ${depth} 7))`);
  console.log('Optimizations: deep self, mutual, indirect and closure tail calls pass.');

  evaluate(`(defparameter *optimization-generic-remaining* 0)
    (defparameter *optimization-generic-empty* nil)
    (defgeneric optimization-generic-zero ())
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-zero () ${policy}
        (if (zerop *optimization-generic-remaining*)
            (if *optimization-generic-empty* (values) (values 17 nil :generic))
            (progn (decf *optimization-generic-remaining*)
                   (optimization-generic-zero-step))))))) (values)`);
  define('optimization-generic-zero-step', `(lambda () ${policy}
    (optimization-generic-zero))`);
  check('deep zero-argument generic calls preserve zero and multiple values', `(and
    (let ((*optimization-generic-remaining* ${depth}) (*optimization-generic-empty* nil))
      (and (equal (multiple-value-list (optimization-generic-zero-step)) '(17 nil :generic))
           (zerop *optimization-generic-remaining*)))
    (let ((*optimization-generic-remaining* ${depth}) (*optimization-generic-empty* t))
      (and (null (multiple-value-list (optimization-generic-zero-step)))
           (zerop *optimization-generic-remaining*))))`);

  evaluate(`(defgeneric optimization-generic-context (n token))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-context ((n number) token) ${policy}
        (values n token :next))
      (defmethod optimization-generic-context ((n integer) token) ${policy}
        (if (zerop n)
            (values (lambda () (call-next-method)) :context)
            (optimization-generic-context-step (1- n) token)))))) (values)`);
  define('optimization-generic-context-step', `(lambda (n token) ${policy}
    (funcall #'optimization-generic-context n token))`);
  check('deep indirect generic calls retain escaping next-method context',
    `(multiple-value-bind (next marker) (optimization-generic-context-step ${depth} :retained)
       (and (eq marker :context)
            (equal (multiple-value-list (funcall next)) '(0 :retained :next))))`);
  console.log('Optimizations: deep generic calls preserve values and escaping next-method context.');

  // Changed CALL-NEXT-METHOD arguments retain applicability checks and live
  // protocol extensions after the native chain and its metadata are warm.
  evaluate(`(defclass optimization-cnm-parent () ())
    (defclass optimization-cnm-child (optimization-cnm-parent) ())
    (defgeneric optimization-cnm-args (object amount))
    (defparameter *optimization-cnm-replacement* nil)
    (defparameter *optimization-cnm-protocol-calls* 0)
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-cnm-args ((object optimization-cnm-parent) amount) ${policy}
        (cond ((eq amount :empty) (values))
              ((eq amount :throw) (throw 'optimization-cnm-transfer :transferred))
              (t (values amount object :cnm))))
      (defmethod optimization-cnm-args ((object optimization-cnm-child) amount) ${policy}
        (cond ((eq amount :escape)
               (values (lambda (new-amount) (call-next-method object new-amount)) :captured))
              ((eq amount :arity) (call-next-method object))
              (t (call-next-method (or *optimization-cnm-replacement* object)
                   (if (numberp amount) (1+ amount) amount))))))))
    (defparameter *optimization-cnm-object* (make-instance 'optimization-cnm-child))
    (defparameter *optimization-cnm-next* (optimization-cnm-args *optimization-cnm-object* :escape))
    (values)`);
  define('optimization-cnm-driver', `(lambda (object amount) ${policy}
    (optimization-cnm-args object amount))`);
  check('native replacement chains retain values, arity, applicability and cleanup', `(let
      ((object *optimization-cnm-object*))
    (dotimes (i 8) (optimization-cnm-driver object i))
    (and (equal (multiple-value-list (optimization-cnm-driver object 10)) (list 11 object :cnm))
      (null (multiple-value-list (optimization-cnm-driver object :empty)))
      (eq (catch 'optimization-cnm-transfer (optimization-cnm-driver object :throw)) :transferred)
      (handler-case (progn (optimization-cnm-driver object :arity) nil) (error () t))
      (let ((*optimization-cnm-replacement* (make-instance 'optimization-cnm-parent)))
        (handler-case (progn (optimization-cnm-driver object 10) nil) (error () t)))
      (let* ((other (make-instance 'optimization-cnm-child)) (*optimization-cnm-replacement* other))
        (equal (multiple-value-list (optimization-cnm-driver object 10)) (list 11 other :cnm)))
      (equal (multiple-value-list (optimization-cnm-driver object 12)) (list 13 object :cnm))))`);
  evaluate(`(defmethod optimization-cnm-args ((object optimization-cnm-parent) (amount (eql 11)))
      (values amount object :eql)) (values)`);
  check('new EQL applicability invalidates a warmed replacement path', `(unwind-protect
      (handler-case (progn (optimization-cnm-driver *optimization-cnm-object* 10) nil) (error () t))
    (remove-method #'optimization-cnm-args
      (find-method #'optimization-cnm-args nil
        (list (find-class 'optimization-cnm-parent) (miricl-mop:intern-eql-specializer 11)))))`);
  evaluate(`(defmethod compute-applicable-methods :around
        ((gf (eql #'optimization-cnm-args)) arguments)
      (declare (ignore gf arguments))
      (incf *optimization-cnm-protocol-calls*) (call-next-method)) (values)`);
  check('escaped native replacement calls observe custom applicable-method protocol', `(unwind-protect
      (progn (setq *optimization-cnm-protocol-calls* 0)
        (and (equal (multiple-value-list (funcall *optimization-cnm-next* 9))
               (list 9 *optimization-cnm-object* :cnm))
          (= *optimization-cnm-protocol-calls* 2)))
    (remove-method #'compute-applicable-methods
      (find-method #'compute-applicable-methods '(:around)
        (list (miricl-mop:intern-eql-specializer #'optimization-cnm-args) (find-class t)))))`);
  check('replacement path recovers after protocol removal', `(equal
    (multiple-value-list (optimization-cnm-driver *optimization-cnm-object* 20))
    (list 21 *optimization-cnm-object* :cnm))`);
  // Saved-image constructors expose canonical layouts. Distinct cached cells
  // with copied values still cannot authorize skipping the live-property proof.
  // Keep values equivalent here so the ordinary fallback remains meaningful.
  const capturedCallback = vm.javascript.callback;
  let cnmGeneric;
  vm.javascript.callback = function (value) {
    cnmGeneric = value;
    return capturedCallback.call(this, value);
  };
  try {evaluate("(js:callback #'optimization-cnm-args) (values)");}
  finally {vm.javascript.callback = capturedCallback;}
  const cnmFields = object => {
    const type = vm.vm['core-object-type'](object);
    const reader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${type}:`));
    assert.ok(reader, 'Canonical CLOS metadata has a saved-image reader.');
    return {type, signature: reader.split(':')[2], values: vm.vm[reader](object)};
  };
  const cnmSetFields = (object, fields, values) => vm.vm[`core-set:${fields.type}`](object,
    ...values.filter((_, index) => fields.signature[index] !== fields.signature[index].toLowerCase()));
  const cnmInfo = cnmFields(cnmGeneric).values[4], cnmInfoFields = cnmFields(cnmInfo);
  const cnmCache = cnmInfoFields.values[6];
  assert.ok(cnmCache, 'The preceding native calls warmed generic metadata.');
  const cnmCacheType = vm.vm['core-object-type'](cnmCache);
  const cnmCacheLengthExport = Object.keys(vm.vm).find(name => name.startsWith(`core-array:${cnmCacheType}:`));
  const cnmCacheLength = vm.vm[cnmCacheLengthExport](cnmCache);
  const cnmCopiedCache = vm.vm[`core-new:${cnmCacheType}`](cnmCacheLength);
  for (let index = 0; index < cnmCacheLength; index++) {
    let value = vm.vm[`core-read:${cnmCacheType}`](cnmCache, index);
    // Cache entries 1..8 are value cells; the remaining entries are derived
    // dispatch rows and validation state and retain their original identities.
    if (index > 0 && index <= 8 && value !== null) {
      const fields = cnmFields(value);
      const copy = vm.vm[`core-new:${fields.type}`](...fields.values.filter((_, position) =>
        fields.signature[position] === fields.signature[position].toLowerCase()));
      cnmSetFields(copy, fields, fields.values);
      value = copy;
    }
    vm.vm[`core-set:${cnmCacheType}`](cnmCopiedCache, index, value);
  }
  try {
    for (const cache of [cnmCopiedCache, null]) {
      cnmSetFields(cnmInfo, cnmInfoFields,
        cnmInfoFields.values.map((value, index) => index === 6 ? cache : value));
      check('replacement calls recover with copied or borrowed cache cells', `(and
        (equal (multiple-value-list (optimization-cnm-driver *optimization-cnm-object* 22))
          (list 23 *optimization-cnm-object* :cnm))
        (handler-case (progn (optimization-cnm-driver *optimization-cnm-object* :arity) nil)
          (error () t)))`);
    }
  } finally {cnmSetFields(cnmInfo, cnmInfoFields, cnmInfoFields.values);}
  check('replacement calls recover after restoring warmed metadata', `(equal
    (multiple-value-list (optimization-cnm-driver *optimization-cnm-object* 24))
    (list 25 *optimization-cnm-object* :cnm))`);
  console.log('Optimizations: native next-method replacements retain applicability, live protocols and complete values.');

  evaluate(`(defgeneric optimization-generic-three (n acc token))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-three ((n integer) acc token) ${policy}
        (if (zerop n) (values acc nil token :generic :three)
          (optimization-generic-three-step (1- n) (1+ acc) token)))))) (values)`);
  define('optimization-generic-three-step', `(lambda (n acc token) ${policy}
    (apply #'optimization-generic-three n (list acc token)))`);
  check('deep three-argument generic calls through APPLY preserve overflow values',
    `(equal (multiple-value-list (optimization-generic-three-step ${depth} 0 :retained))
            '(${depth} nil :retained :generic :three))`);

  evaluate(`(defgeneric optimization-generic-one (n))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod optimization-generic-one ((n integer)) ${policy}
        (if (zerop n) :original (optimization-generic-one-step (1- n))))))) (values)`);
  define('optimization-generic-one-step', `(lambda (n) ${policy}
    (optimization-generic-one n))`);
  check('deep unary generic calls', `(eq (optimization-generic-one-step ${depth}) :original)`);
  evaluate(`(funcall (miricl-compiler:compile-native '(lambda ()
    (defmethod optimization-generic-one ((n integer)) ${policy}
      (if (zerop n) (values :replacement nil :current)
        (optimization-generic-one-step (1- n))))))) (values)`);
  check('warmed generic tail calls observe method replacement', `(equal
    (multiple-value-list (optimization-generic-one-step ${depth})) '(:replacement nil :current))`);
  console.log('Optimizations: generic tail APPLY, overflow values and live method replacement pass.');

  define('optimization-tail-apply', `(lambda (n &rest results) ${policy}
    (if (zerop n) (values-list results) (apply #'optimization-tail-apply (1- n) results)))`);
  check('tail APPLY preserves value groups', `(equal
    (multiple-value-list (optimization-tail-apply ${depth} 17 nil 23)) '(17 nil 23))`);
  define('optimization-tail-values', `(lambda (n) ${policy}
    (if (zerop n) (values)
      (multiple-value-call #'optimization-tail-values (values (1- n)) (values))))`);
  check('tail MULTIPLE-VALUE-CALL preserves zero values',
    `(null (multiple-value-list (optimization-tail-values ${depth})))`);
  define('optimization-tail-exit', `(lambda (n) ${policy}
    (if (zerop n) (throw :optimization-exit (values 17 :exit))
      (optimization-tail-exit (1- n))))`);
  check('outer dynamic extent survives deep tail calls', `(let ((cleaned nil))
    (and (equal (multiple-value-list
      (catch :optimization-exit
        (unwind-protect (optimization-tail-exit ${depth}) (setf cleaned t)))) '(17 :exit))
      cleaned))`);
  console.log('Optimizations: tail dispatch preserves value groups, nonlocal exits and outer cleanup.');

  // Retaining many cheap function references must not inflate every suspended
  // recursive frame. Exercise actual recursive completion and effects through
  // the ordinary async adapter, without prescribing caches, locals or opcodes.
  const names = Array.from({length: 80}, (_, i) => `optimization-literal-effect-${i}`);
  const repetitions = 8, recursiveDepth = 2048;
  const calls = Array.from({length: repetitions}, () => names.map(name => `(${name} n)`).join(' ')).join('\n');
  evaluate('(defparameter *optimization-literal-effects* 0) (values)');
  define('optimization-literal-effect', `(lambda (x) (declare (ignore x))
    (incf *optimization-literal-effects*) nil)`);
  evaluate(`(dolist (name '(${names.join(' ')}))
    (setf (symbol-function name) #'optimization-literal-effect)) (values)`);
  define('optimization-literal-recur', `(lambda (n)
    (declare (type (integer 0 4096) n) (inline zerop 1- 1+)
      (optimize (safety 0) (debug 0)))
    (if (zerop n) 0
      (let ((answer (optimization-literal-recur (1- n)))) ${calls} (1+ answer))))`);
  const pressure = await vm.evaluateAsync(`(let ((answer (optimization-literal-recur ${recursiveDepth})))
    (and (= answer ${recursiveDepth})
      (= *optimization-literal-effects* ${recursiveDepth * names.length * repetitions})))`, {eof: true});
  assert.equal(pressure.output, 'T\n', 'Literal-heavy retained frames preserve recursion capacity and effects.');
  console.log('Optimizations: literal-heavy non-tail recursion completes without excessive retained-frame pressure.');

  // Repeated substantial regions should pay for their private helpers while
  // retaining callback effects, ordinary primary values and the final values.
  check('module outlining shares large regions without changing effects or values', `(let*
    ((definitions (loop for i below 10 collect
       (list 'defun (intern (format nil "OPTIMIZATION-OUTLINE-~D" i)) '(x callback)
         '(declare (optimize (space 3) (speed 1) (debug 0)))
         (list 'let* '((a (funcall callback x x x x x x))
                       (b (funcall callback a a a a a a))
                       (c (funcall callback b b b b b b)))
           (list 'values 'c i)))))
     (plain (let ((miricl-compiler::*native-outlining-budget* 0))
       (multiple-value-list (miricl-compiler:compile-native-module definitions :inline nil))))
     (shared (multiple-value-list (miricl-compiler:compile-native-module definitions :inline nil))))
    (and (< (length (second shared)) (length (second plain)))
      (loop for variant in (list plain shared) always
        (loop for entry in (first variant) for i from 0 always
          (let ((effects nil))
            (and (equal (multiple-value-list
                   (funcall (cdr entry) 40
                     (lambda (&rest arguments)
                       (push (first arguments) effects)
                       (values (apply #'+ arguments) :discarded))))
                 (list 8640 i))
                 (equal effects '(1440 240 40))))))))`);
  console.log('Optimizations: whole-module outlining reduces repeated code and preserves callback effects and multiple values.');

  // ANSI covers IF and values separately. This composition gives the branches
  // different prefixes followed by the same effectful operation.
  define('optimization-branch-tail', `(lambda (flag callback) ${policy}
    (if flag
      (progn (funcall callback :left) (funcall callback :tail))
      (progn (funcall callback :right) (funcall callback :tail))))`);
  check('common branch tails retain selected effects and complete value groups', `(loop
    for flag in '(t nil) always
      (loop for returned in '(nil (nil) (17 nil :third :four :five)) always
        (let ((events nil))
          (and (equal (multiple-value-list
                 (optimization-branch-tail flag (lambda (event)
                   (push event events)
                   (if (eq event :tail) (values-list returned) (values :ignored :extra)))))
                returned)
            (equal (reverse events) (list (if flag :left :right) :tail))))))`);
  check('common branch tails retain errors, ordering and recovery', `(loop for flag in '(t nil) always
    (let ((prefix (if flag :left :right)))
      (and (loop for failure in (list prefix :tail) always
        (let ((events nil))
          (handler-case
            (progn (optimization-branch-tail flag (lambda (event)
              (push event events) (when (eq event failure) (error "Branch callback")))) nil)
            (simple-error ()
              (equal (reverse events) (if (eq failure prefix) (list prefix) (list prefix :tail)))))))
        (equal (multiple-value-list
                 (optimization-branch-tail flag (lambda (event) (values event nil :recovered))))
          '(:tail nil :recovered)))))`);
  check('branch tails consume the selected branch-local value', `(let ((function
    (miricl-compiler:compile-native '(lambda (flag callback) ${policy}
      (if flag
        (funcall callback (cons :left nil))
        (funcall callback (cons :right nil)))))))
    (loop for flag in '(t nil) always
      (equal (multiple-value-list (funcall function flag
               (lambda (object) (values object nil :extra))))
        (list (list (if flag :left :right)) nil :extra))))`);

  evaluate('(defvar *optimization-branch-scope* :outside) (values)');
  define('optimization-branch-environment', `(lambda (flag callback) ${policy}
    (if flag
      (let ((*optimization-branch-scope* :left))
        (unwind-protect (progn (funcall callback :left) (funcall callback :tail))
          (funcall callback :cleanup)))
      (let ((*optimization-branch-scope* :right))
        (unwind-protect (progn (funcall callback :right) (funcall callback :tail))
          (funcall callback :cleanup)))))`);
  check('branch tails retain dynamic bindings and cleanup on returns and exits', `(loop
    for flag in '(t nil) always (loop for escape in '(nil t) always
      (let ((events nil) (scope (if flag :left :right)))
        (and (equal (multiple-value-list
               (catch 'optimization-branch-exit
                 (optimization-branch-environment flag (lambda (event)
                   (push (list event *optimization-branch-scope*) events)
                   (if (and escape (eq event :tail))
                     (throw 'optimization-branch-exit (values 17 nil :extra))
                     (values 17 nil :extra)))))) '(17 nil :extra))
          (equal (reverse events) (list (list scope scope) (list :tail scope) (list :cleanup scope)))
          (eq *optimization-branch-scope* :outside)))))`);
  console.log('Optimizations: common branch tails preserve effects, value groups, errors and dynamic cleanup.');

  // ANSI covers comparisons and LOGAND separately. Exercise executable range
  // proofs with retained checked producers, effectful operands and full values.
  const checkedComparisonPolicy = '(declare (optimize (space 3) (speed 1) (safety 1) (debug 1)))';
  for (const [operator, suffix] of [['<', 'less'], ['>', 'greater'], ['<=', 'le'], ['>=', 'ge']]) {
    for (const mask of ['1073741823', '9223372036854775807']) {
      const name = `optimization-checked-${suffix}-${mask}`;
      define(name, `(lambda (left right callback) ${checkedComparisonPolicy}
        (values (multiple-value-list (${operator}
          (logand (funcall callback :left left) ${mask})
          (logand (funcall callback :right right) ${mask}))) :comparison))`);
      check('checked scalar comparison preserves values, effects and integer boundaries', `(loop
        for pair in '((0 0) (0 1) (1 0) (-1 0)
          (1073741823 1073741824) (1073741824 1073741823)
          (-1073741825 -1073741824)
          (-9223372036854775808 9223372036854775807)
          (9223372036854775807 -9223372036854775808)
          (18446744073709551617 18446744073709551618)) always
        (let ((events nil) (left (first pair)) (right (second pair)))
          (and (equal (multiple-value-list
                  (${name} left right (lambda (event value)
                    (push event events) (values value :discarded :extra))))
                (list (list (${operator} (logand left ${mask}) (logand right ${mask}))) :comparison))
            (equal (reverse events) '(:left :right)))))`);
      check('checked scalar producers retain errors, evaluation order and recovery', `(loop
        for pair in '((:bad 5) (5 :bad) (1.5 5) (5 #c(1 2))) always
        (let ((events nil))
          (and (handler-case
              (progn (${name} (first pair) (second pair) (lambda (event value)
                (push event events) (values value :discarded))) nil)
              (type-error (condition)
                (and (eql (type-error-datum condition)
                       (if (integerp (first pair)) (second pair) (first pair)))
                  (equal (reverse events)
                    (if (integerp (first pair)) '(:left :right) '(:left))))))
            (equal (multiple-value-list
                (${name} 2 3 (lambda (event value) (declare (ignore event)) (values value :extra))))
              (list (list (${operator} 2 3)) :comparison)))))`);
    }
    const declared = `optimization-declared-${suffix}`;
    define(declared, `(lambda (left right) ${checkedComparisonPolicy}
      (declare (type (signed-byte 64) left right)) (${operator} left right))`);
    check('declaration-only comparison retains target signed64 boundary semantics', `(loop
      for left in '(-9223372036854775808 -1073741825 0 1073741824 9223372036854775807)
      always (loop for right in '(-9223372036854775808 -1073741825 0 1073741824 9223372036854775807)
        always (eql (${declared} left right) (${operator} left right))))`);
    const general = `optimization-general-${suffix}`;
    define(general, `(lambda (left right) ${checkedComparisonPolicy} (${operator} left right))`);
    check('unproved comparisons retain real-number semantics and checked errors', `(and
      (eql (${general} 1/2 0.75) (${operator} 1/2 0.75))
      (eql (${general} 18446744073709551616 18446744073709551617)
        (${operator} 18446744073709551616 18446744073709551617))
      (handler-case (progn (${general} :bad 1) nil) (type-error () t))
      (eql (${general} 2 3) (${operator} 2 3)))`);
    const guarded = `optimization-guarded-${suffix}`;
    define(guarded, `(lambda (left right) ${checkedComparisonPolicy}
      (if (and (typep left '(signed-byte 64)) (typep right '(signed-byte 64)))
        (${operator} (logand left -1) (logand right -1)) :outside))`);
    check('executably guarded comparisons preserve signed64 limits and outside inputs', `(and
      (loop for left in '(-9223372036854775808 -1073741825 -1073741824 0 1073741823 1073741824 9223372036854775807)
        always (loop for right in '(-9223372036854775808 -1073741825 -1073741824 0 1073741823 1073741824 9223372036854775807)
          always (eql (${guarded} left right) (${operator} left right))))
      (eq (${guarded} 9223372036854775808 0) :outside)
      (eq (${guarded} 0 -9223372036854775809) :outside)
      (eq (${guarded} :bad 0) :outside))`);
  }
  console.log('Optimizations: executable scalar comparison proofs retain checked producers, operand effects and multiple values.');

  define('optimization-counter-snapshots', `(lambda (initial count)
    (declare (type (integer 0 32) count)
      (optimize (speed 1) (space 3) (safety 1) (debug 1)))
    (let ((value initial) (previous initial))
      (dotimes (i count)
        (setq previous value)
        (setq value (1+ value)))
      (values initial previous value :fourth nil :sixth :seventh :eighth :ninth)))`);
  check('private counter snapshots survive writes and full-value returns', `(and
    (loop for initial in '(0 1073741823 -1073741825 1099511627776
                           9223372036854775807 -9223372036854775809 3/2 2.5 #C(1 2))
      always (loop for count in '(0 1 2 32) always
        (equal (multiple-value-list (optimization-counter-snapshots initial count))
          (list initial (+ initial (max 0 (1- count))) (+ initial count)
            :fourth nil :sixth :seventh :eighth :ninth))))
    (let ((object (list :original)))
      (eq (nth-value 2 (optimization-counter-snapshots object 0)) object))
    (handler-case (progn (optimization-counter-snapshots :bad 1) nil)
      (type-error () t))
    (= (nth-value 2 (optimization-counter-snapshots 7 2)) 9))`);
  define('optimization-counter-capture', `(lambda (value)
    (declare (optimize (speed 1) (space 3) (safety 1) (debug 1)))
    (let ((reader (lambda () value)))
      (setq value (1+ value))
      (values (funcall reader) reader)))`);
  check('evolving counters retain shared captured state', `(multiple-value-bind (value reader)
    (optimization-counter-capture 1099511627776)
    (and (= value 1099511627777) (= (funcall reader) value)))`);
  for (const [index, operator] of ['<', '>', '<=', '>=', '=', '/='].entries()) {
    const name = `optimization-counter-predicate-${index}`;
    define(name, `(lambda (value limit effects)
      (declare (optimize (speed 1) (space 3) (safety 1) (debug 1)))
      (push :body (car effects))
      (setq value (1+ value))
      (values (if (not (null (not (${operator} value limit)))) :yes :no) value))`);
    check('counter predicate polarity preserves promotion, checked errors and recovery', `(and
      (loop for pair in '((0 1) (1073741823 1073741824)
                         (1099511627776 1099511627778)
                         (9223372036854775807 9223372036854775808) (3/2 2) (2.5 4))
        always (let* ((value (first pair)) (limit (second pair)) (effects (list nil)))
          (and (equal (multiple-value-list (${name} value limit effects))
                 (list (if (not (${operator} (1+ value) limit)) :yes :no) (1+ value)))
            (equal (car effects) '(:body)))))
      (let ((effects (list nil)))
        (and (handler-case (progn (${name} 0 :bad effects) nil) (type-error () t))
          (equal (car effects) '(:body))))
      (equal (multiple-value-list (${name} 0 2 (list nil)))
        (list (if (not (${operator} 1 2)) :yes :no) 1)))`);
  }

  // Declared hot paths deliberately exercise inlining and scalar lowering.
  // The assertions concern legal input, effects and values, not a prescribed
  // lowering strategy or the optimizer's private representation.
  define('optimization-inlined-capture', `(lambda (initial)
    (declare (optimize (speed 3) (space 1)))
    (let ((state initial))
      (flet ((advance (amount) (incf state amount)))
        (declare (inline advance))
        (let ((observe (lambda () state)))
          (values (advance 2) (funcall observe) (advance 3) (funcall observe))))))`);
  check('inlined writes retain shared closure cells', `(equal
    (multiple-value-list (optimization-inlined-capture 7)) '(9 9 12 12))`);
  define('optimization-inlined-check', `(lambda (v i)
    (declare (optimize (speed 3) (space 1) (safety 0)))
    (flet ((checked (v i)
      (declare (optimize (safety 3)) (type simple-vector v) (fixnum i)) (svref v i)))
      (declare (inline checked)) (checked v i)))`);
  check('inlining preserves the callee safety contract', `(and
    (= (optimization-inlined-check #(42) 0) 42)
    (handler-case (progn (optimization-inlined-check #(42) 1) nil) (type-error () t))
    (= (optimization-inlined-check #(43) 0) 43))`);

  // A literal-selected small arm can make an otherwise large callee cheap.
  // Its unused operands still run before checked callee code, under the
  // caller's scope; the body retains its definition's lexical/dynamic scope.
  evaluate('(defvar *optimization-inline-scope* :outside) (values)');
  for (const [speed, space] of [[1, 3], [3, 1]]) {
    for (const effort of [0, 3]) for (const directive of ['inline', 'notinline']) {
      define('optimization-inline-literal', `(lambda (vector effect)
        (declare (optimize (speed ${speed}) (space ${space})
          (compilation-speed ${effort}) (safety 0) (debug 0)))
        (let ((token :definition))
          (flet ((worker (selected vector *optimization-inline-scope* unused)
                   (declare (ignore unused) (optimize (safety 3) (debug 0)))
                   (if selected
                     (values token (svref vector 0) *optimization-inline-scope*)
                     (progn ${Array(16).fill('(funcall effect :unselected)').join(' ')}
                       (values :unselected nil)))))
            (declare (${directive} worker))
            (let ((token :caller))
              (declare (ignore token))
              (worker t (progn (funcall effect :vector) vector)
                (progn (funcall effect :scope) :callee)
                (funcall effect :unused))))))`);
      check('literal inlining keeps operand order, scope, callee checks and recovery', `(let ((events nil))
        (flet ((effect (event) (push event events)))
          (and (equal (multiple-value-list
                 (optimization-inline-literal #(42) #'effect)) '(:definition 42 :callee))
            (equal (reverse events) '(:vector :scope :unused))
            (eq *optimization-inline-scope* :outside)
            (progn (setq events nil)
              (handler-case (progn (optimization-inline-literal :bad #'effect) nil)
                (type-error () (equal (reverse events) '(:vector :scope :unused)))))
            (eq *optimization-inline-scope* :outside)
            (progn (setq events nil)
              (equal (multiple-value-list
                (optimization-inline-literal #(43) #'effect)) '(:definition 43 :callee))))))`);
    }
  }

  // Lifetime hints never imply absence of aliases or mutation. Exercise the
  // private REST path alongside uses that must retain an ordinary Lisp list.
  for (const safety of [0, 1, 3]) {
    const restPolicy = `(declare (dynamic-extent xs)
      (optimize (space 3) (speed 1) (debug 1) (safety ${safety})))`;
    define('optimization-dx-count', `(lambda (prefix &rest xs) ${restPolicy}
      (values prefix (length xs) (length xs)))`);
    define('optimization-dx-nested', `(lambda (hook &rest xs) ${restPolicy}
      (funcall hook) (length xs))`);
    define('optimization-dx-mutate', `(lambda (hook &rest xs) ${restPolicy}
      (funcall hook xs) (length xs))`);
    define('optimization-dx-assign', `(lambda (&rest xs) ${restPolicy}
      (setq xs (list :replacement :tail)) (length xs))`);
    define('optimization-dx-alias', `(lambda (&rest xs) ${restPolicy}
      (let ((alias xs)) (rplacd alias nil) (values (eq alias xs) (length xs))))`);
    define('optimization-dx-free', `(lambda (&rest xs)
      (locally (declare (dynamic-extent xs)) (length xs)))`);
    define('optimization-dx-shadow', `(lambda (&rest xs) ${restPolicy}
      (let ((xs (list :inner :binding))) (length xs)))`);
    check('dynamic-extent REST preserves counts, nested calls, mutation and binding identity', `(and
      (equal (multiple-value-list (optimization-dx-count :prefix)) '(:prefix 0 0))
      (equal (multiple-value-list
        (apply #'optimization-dx-count :prefix (make-list 64 :initial-element :value))) '(:prefix 64 64))
      (= (optimization-dx-nested (lambda () (list :nested :call :packet)) :a :b :c) 3)
      (= (optimization-dx-mutate (lambda (list) (rplacd list nil)) :a :b :c) 1)
      (= (optimization-dx-assign :a :b :c) 2)
      (equal (multiple-value-list (optimization-dx-alias :a :b :c)) '(t 1))
      (= (optimization-dx-free :a :b) 2)
      (= (optimization-dx-shadow :a :b :c) 2))`);
  }
  vm.mountFile('/optimization-dx-rest.lisp', `(in-package :cl-user)
    (defun optimization-dx-loaded (&optional prefix &rest xs)
      (declare (dynamic-extent xs))
      (values prefix (length xs)))
    (defun optimization-dx-out-of-line (&rest xs)
      (declare (dynamic-extent xs) (notinline length)) (length xs))`);
  evaluate(`(compile-file "/optimization-dx-rest.lisp" :verbose nil :block-compile nil)
    (miricl-compiler:compile-native-bundle "/optimization-dx-rest.wasm" "/optimization-dx-rest-native.wasm")
    (load "/optimization-dx-rest-native.wasm") (values)`);
  check('dynamic-extent REST survives file preparation and native conversion', `(and
    (equal (multiple-value-list (optimization-dx-loaded)) '(nil 0))
    (equal (multiple-value-list (optimization-dx-loaded :prefix :a :b)) '(:prefix 2))
    (= (optimization-dx-out-of-line :a :b :c) 3))`);

  define('optimization-mutable-loop', `(lambda (cell n)
    (declare (optimize (speed 3) (space 1) (safety 0))
      (type cons cell) (type (integer 0 20) n) (inline car not consp))
    (let ((sum 0) (effects 0))
      (dotimes (i n)
        (when (car (progn (incf effects) cell)) (incf sum))
        (setf (car cell) (not (car cell))))
      (values sum effects (car cell))))`);
  check('loop motion and reuse respect heap mutation and operand effects', `(and
    (equal (multiple-value-list (optimization-mutable-loop (list t) 0)) '(0 0 t))
    (equal (multiple-value-list (optimization-mutable-loop (list t) 5)) '(3 5 nil))
    (equal (multiple-value-list (optimization-mutable-loop (list nil) 6)) '(3 6 nil)))`);
  console.log('Optimizations: inlined captures and safety, and mutation/effect-sensitive loops pass.');
} finally {vm.close();}
