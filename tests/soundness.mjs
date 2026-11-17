// Implementation contracts absent from ANSI: untrusted compiler boundaries,
// conservative optimization proofs, redefinition guards, and the FASL codec.
// node tests/soundness.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);
const recover = () => assert.equal(evaluate("(funcall (compile-native '(lambda (x) (+ x 1))) 41)"), '42\n');
try {
  evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  // These private method bodies are callable independently of their public
  // MOP generic functions. Keep their checked boundary and live callbacks.
  evaluate(`(defclass soundness-mop-meta (standard-class) ())
    (defmethod miricl-mop:validate-superclass
        ((class soundness-mop-meta) (super standard-class)) t)
    (defvar *soundness-mop-events* nil)
    (defmethod miricl-mop:class-finalized-p :around ((class soundness-mop-meta))
      (push :finalized *soundness-mop-events*)
      (values (call-next-method) :discarded))
    (defmethod miricl-mop:finalize-inheritance :around ((class soundness-mop-meta))
      (push :finalize *soundness-mop-events*)
      (values (call-next-method) :discarded))
    (defmethod class-name :around ((class soundness-mop-meta))
      (push :name *soundness-mop-events*) (call-next-method))
    (defclass soundness-mop-record () ((value :initarg :value))
      (:metaclass soundness-mop-meta) (:default-initargs :value 42)) (values)`);
  check(`(let* ((class (find-class 'soundness-mop-record))
      (slots (symbol-function (compiler-primitive 10 1 "%%STANDARD-CLASS-SLOTS")))
      (defaults (symbol-function (compiler-primitive 10 1 "%%STANDARD-CLASS-DEFAULT-INITARGS"))))
    (labels ((recover ()
               (and (consp (funcall slots class))
                 (= (slot-value (make-instance class) 'value) 42)))
             (reject (function arguments &optional arityp)
               (and (handler-case (progn (apply function arguments) nil)
                      (program-error (condition) (plusp (length (princ-to-string condition))))
                      (error (condition)
                        (and (not arityp) (plusp (length (princ-to-string condition))))))
                    (recover))))
      (and
        (every (lambda (function)
          (and (reject function nil t) (reject function (list class class) t)
            (every (lambda (object) (reject function (list object)))
                   (list nil 3 'not-a-class #()))))
          (list slots defaults))
        ;; MIRICL retains lazy CLASS-SLOTS finalization; defaults still reject
        ;; an unfinalized class and call the extensible CLASS-NAME diagnostic.
        (progn (make-instances-obsolete class)
          (let ((*soundness-mop-events* nil))
            (and (handler-case (progn (funcall defaults class) nil) (error () t))
              (member :name *soundness-mop-events*)
              (not (member :finalize *soundness-mop-events*))
              (let* ((slot-values (multiple-value-list (funcall slots class)))
                     (default-values (multiple-value-list (funcall defaults class))))
                (and (member :finalized *soundness-mop-events*)
                  (member :finalize *soundness-mop-events*)
                  (equal slot-values (list (funcall slots class)))
                  (equal default-values (list (funcall defaults class)))
                  (eq (car slot-values) (miricl-mop:class-slots class))
                  (eq (car default-values) (miricl-mop:class-default-initargs class))
                  (recover)))))))))`);
  console.log('Soundness: native MOP readers reject invalid calls with recovery and retain finalization callbacks, sharing and values.');
  // Unary runtime callbacks preserve complete method values and cleanup. Keep
  // dispatch class-only: EQL profiles deliberately retain ordinary dispatch.
  evaluate(`(defgeneric soundness-unary-callback (object))
    (defvar *soundness-unary-scope* :outside)
    (funcall (compile-native '(lambda ()
      (defmethod soundness-unary-callback ((object t))
        (declare (optimize (debug 0))) (values t :parent))
      (defmethod soundness-unary-callback ((object symbol))
        (declare (optimize (debug 0)))
        (case object
          (:zero (values))
          (:many (values t :second :third :fourth :fifth))
          (:escape (let ((*soundness-unary-scope* :inside))
                     (throw 'soundness-unary-exit (values :escaped :extra))))
          (:failure (let ((*soundness-unary-scope* :inside)) (error "Callback failure")))
          (otherwise (multiple-value-bind (value extra) (call-next-method)
                       (values value :child extra)))))))) (values)`);
  check(`(let* ((metadata (symbol-function (compiler-primitive 10 1 "%%CLOS-METHOD-METADATA")))
      (original (fdefinition 'miricl-mop:class-finalized-p))
      (replacement (compile-native '(lambda (object) (values t :replacement object))))
      (class (find-class 'standard-object)) (result nil))
    (labels ((probe (object) (funcall metadata 0 object nil))
             (normal () (equal (multiple-value-list (probe :normal)) '(t :child :parent))))
      (unwind-protect
        (progn
          (setf (fdefinition 'miricl-mop:class-finalized-p) #'soundness-unary-callback)
          (dotimes (i 8) (normal))
          (setq result
            (and (normal)
              (null (multiple-value-list (probe :zero)))
              (equal (multiple-value-list (probe :many)) '(t :second :third :fourth :fifth))
              (equal (multiple-value-list (catch 'soundness-unary-exit (probe :escape)))
                     '(:escaped :extra))
              (eq *soundness-unary-scope* :outside)
              (handler-case (progn (probe :failure) nil) (error () t))
              (eq *soundness-unary-scope* :outside) (normal)
              (equal (multiple-value-list
                (probe (progn (setf (fdefinition 'miricl-mop:class-finalized-p) replacement)
                              :changed))) '(t :replacement :changed)))))
        (setf (fdefinition 'miricl-mop:class-finalized-p) original))
      (and result
        (equal (multiple-value-list (probe class))
               (multiple-value-list (miricl-mop:class-finalized-p class))))))`);
  console.log('Soundness: unary MOP callbacks retain next methods, full values, live bindings and nonlocal cleanup.');
  // The private fixed-call ABI must recognize saved function objects, including
  // after rebinding their names, and retain the ordinary checked boundary.
  check(`(let* ((tail-name (compiler-primitive 10 1 "%%NTHCDR"))
      (copy-name (compiler-primitive 10 1 "%%LIST-COPY"))
      (slot-name (compiler-primitive 10 1 "%%SLOT-OPERATION"))
      (tail (symbol-function tail-name)) (copy (symbol-function copy-name))
      (slot (symbol-function slot-name))
      (one (compile-native '(lambda (f a) (funcall f a))))
      (two (compile-native '(lambda (f a b) (funcall f a b))))
      (three (compile-native '(lambda (f a b c) (funcall f a b c))))
      (four (compile-native '(lambda (f a b c d) (funcall f a b c d))))
      (five (compile-native '(lambda (f a b c d e) (funcall f a b c d e))))
      (replacement (compile-native '(lambda (&rest arguments)
        (declare (ignore arguments)) (values :replacement :extra))))
      (items (list :first :second))
      (object (make-instance 'soundness-mop-record)))
    (labels ((recover ()
               (and (equal (multiple-value-list (funcall two tail 1 items))
                           (list (cdr items)))
                    (equal (multiple-value-list (funcall four slot 0 object 'value nil)) '(42))))
             (reject (thunk &optional arityp)
               (and (handler-case (progn (funcall thunk) nil)
                      (program-error (condition) (plusp (length (princ-to-string condition))))
                      (error (condition)
                        (and (not arityp) (plusp (length (princ-to-string condition))))))
                    (recover))))
      (and
        (every (lambda (function)
          (and (reject (lambda () (funcall one function nil)) t)
               (reject (lambda () (funcall three function nil nil nil)) t)))
          (list tail copy))
        (reject (lambda () (funcall three slot 0 object 'value)) t)
        (reject (lambda () (funcall five slot 0 object 'value nil nil)) t)
        (every (lambda (selector)
          (and (reject (lambda () (funcall two copy items selector)))
               (reject (lambda () (funcall four slot selector object 'value nil)))))
          (list -1 (expt 2 40) :bad-selector))
        (reject (lambda () (funcall two tail 1 :not-a-list)))
        (reject (lambda () (funcall two tail -1 items)))
        (reject (lambda () (funcall two copy :not-a-list 0)))
        (reject (lambda () (funcall two copy (list :not-a-pair) 1)))
        (reject (lambda () (funcall four slot 0 17 'value nil)))
        (every (lambda (entry)
          (let ((name (car entry)) (original (cdr entry)))
            (unwind-protect
              (progn (setf (symbol-function name) replacement)
                (and (if (eq name slot-name)
                       (equal (multiple-value-list (funcall four original 0 object 'value nil)) '(42))
                       (equal (multiple-value-list (funcall two original
                                  (if (eq name tail-name) 1 items)
                                  (if (eq name tail-name) items 0)))
                              (list (if (eq name tail-name) (cdr items) items))))
                     (equal (multiple-value-list
                       (if (eq name slot-name)
                         (funcall four (symbol-function name) 0 object 'value nil)
                         (funcall two (symbol-function name) nil nil))) '(:replacement :extra))))
              (setf (symbol-function name) original))))
          (list (cons tail-name tail) (cons copy-name copy) (cons slot-name slot)))
        (recover))))`);
  evaluate(`(defvar *soundness-slot-calls* 0)
    (defmethod miricl-mop:slot-value-using-class :around
        ((class soundness-mop-meta) (object soundness-mop-record) slot)
      (incf *soundness-slot-calls*) (values (call-next-method) :discarded)) (values)`);
  // An inherited generated accessor can use a slot proof even when another
  // primary follows it. Its private effective-method boundary still validates
  // the complete next-method spine before publishing that proof.
  evaluate(`(defclass soundness-inherited-slot-base ()
      ((value :initform 11 :reader soundness-inherited-slot)))
    (defclass soundness-inherited-slot-child (soundness-inherited-slot-base)
      ((value :initform 42 :reader soundness-inherited-slot)))
    (defparameter *soundness-inherited-object* (make-instance 'soundness-inherited-slot-child))
    (defparameter *soundness-inherited-reader*
      (compile-native '(lambda (object) (soundness-inherited-slot object)))) (values)`);
  check(`(let* ((gf (fdefinition 'soundness-inherited-slot))
       (methods (compute-applicable-methods gf (list *soundness-inherited-object*)))
       (primitive (symbol-function (compiler-primitive 10 1 "%%PROPERTY-EFFECTIVE"))))
    (labels ((recover () (= (funcall *soundness-inherited-reader* *soundness-inherited-object*) 42))
             (reject (next)
               (and (handler-case
                      (progn (funcall primitive nil (list 'call-method (car methods) next)
                               gf (list *soundness-inherited-object*)) nil)
                      (type-error (condition)
                        (let ((*print-circle* t)) (plusp (length (princ-to-string condition))))))
                    (recover))))
      (let ((cycle (list (cadr methods))))
        (setf (cdr cycle) cycle)
        (and (cdr methods) (recover) (reject (cons (car methods) :tail))
             (reject cycle) (recover)))))`);
  check(`(let ((function (symbol-function (compiler-primitive 10 1 "%%SLOT-OPERATION")))
      (object (make-instance 'soundness-mop-record))
      (driver (compile-native '(lambda (function object)
        (let ((events nil))
          (values (multiple-value-list (funcall function
            (progn (push :mode events) 0) (progn (push :object events) object)
            (progn (push :name events) 'value) (progn (push :value events) nil)))
            (reverse events)))))))
    (let ((*soundness-slot-calls* 0))
      (and (equal (multiple-value-list (funcall driver function object))
                  '((42) (:mode :object :name :value)))
           (= *soundness-slot-calls* 1))))`);
  console.log('Soundness: fixed primitive aliases retain identity, validation, recovery, operand order and custom MOP callbacks.');
  // Public bootstrap functions also escape as saved objects. Rebinding their
  // public names must not replace those objects or bypass their arity checks.
  check(`(let* ((items (list :first :second))
      (object (make-instance 'soundness-mop-record))
      (one (compile-native '(lambda (function a) (funcall function a))))
      (two (compile-native '(lambda (function a b) (funcall function a b))))
      (replacement (compile-native '(lambda (&rest arguments)
        (declare (ignore arguments)) (values :replacement :extra)))))
    (every (lambda (entry)
      (destructuring-bind (name arguments expected) entry
        (let ((saved (fdefinition name)))
          (labels ((invoke ()
                     (if (cdr arguments)
                       (funcall two saved (car arguments) (cadr arguments))
                       (funcall one saved (car arguments))))
                   (recover ()
                     (and (equal (multiple-value-list (invoke)) (list expected))
                       (if (eq name 'copy-list) (not (eq (invoke) items)) t))))
            (and
              (handler-case
                (progn (if (cdr arguments) (funcall one saved nil)
                           (funcall two saved nil nil)) nil)
                (program-error (condition) (plusp (length (princ-to-string condition)))))
              (recover)
              (unwind-protect
                (progn (setf (fdefinition name) replacement)
                  (and (recover)
                    (equal (multiple-value-list (apply (fdefinition name) arguments))
                           '(:replacement :extra))))
                (setf (fdefinition name) saved))
              (recover))))))
      (list (list 'nth (list 1 items) :second)
            (list 'nthcdr (list 1 items) (cdr items))
            (list 'list-length (list items) 2)
            (list 'copy-list (list items) items)
            (list 'slot-value (list object 'value) 42))))`);
  check(`(let* ((saved (fdefinition 'nth))
      (original-car (fdefinition 'car)) (original-tail (fdefinition 'nthcdr))
      (driver (compile-native '(lambda (function count items)
        (let ((events nil))
          (values (multiple-value-list
                    (funcall (progn (push :function events) function)
                      (progn (push :count events) count)
                      (progn (push :items events) items)))
                  (reverse events))))))
      ;; Expand PUSH and SETF before changing primitives used by source macros.
      (factory (compile-native '(lambda ()
        (let ((events nil))
          (values
            (lambda (tail)
              (push (list :car tail) events) (values :current-car :discarded))
            (lambda (count items)
              (push (list :nthcdr count items) events)
              (values '(:current-tail) :discarded))
            (lambda () (reverse events)))))))
      (runner (compile-native '(lambda (driver saved items car-callback tail-callback
                                       original-car original-tail)
        (unwind-protect
          (progn (setf (fdefinition 'car) car-callback
                       (fdefinition 'nthcdr) tail-callback)
            (multiple-value-list (funcall driver saved 1 items)))
          (setf (fdefinition 'car) original-car
                (fdefinition 'nthcdr) original-tail)))))
      (items (list :first :second)))
    (multiple-value-bind (car-callback tail-callback read-events) (funcall factory)
      (let ((result (funcall runner driver saved items car-callback tail-callback
                             original-car original-tail)))
        (and (equal result '((:current-car) (:function :count :items)))
             (equal (funcall read-events)
                    (list (list :nthcdr 1 items) '(:car (:current-tail))))
             (equal (multiple-value-list (funcall saved 1 items)) '(:second))))))`);
  console.log('Soundness: public saved aliases survive rebinding and arity errors; NTH retains live callbacks, operand order and one value.');
  // Initarg validation uses intrinsic method-selection caches. A warmed
  // constructor must observe method keyword changes without repeating hooks.
  check(`(progn
    (defclass soundness-initarg-cache () ())
    (defparameter *soundness-initarg-cache-calls* 0)
    (dotimes (i 3) (make-instance 'soundness-initarg-cache))
    (labels ((reject ()
               (handler-case
                   (progn (make-instance 'soundness-initarg-cache :added 42) nil)
                 (error () t))))
      (and (reject)
        (progn
          (defmethod initialize-instance :after
              ((object soundness-initarg-cache) &key added)
            (when (eql added 42) (incf *soundness-initarg-cache-calls*)))
          (let ((method (find-method #'initialize-instance '(:after)
                          (list (find-class 'soundness-initarg-cache)))))
            (unwind-protect
              (and (typep (make-instance 'soundness-initarg-cache :added 42) 'soundness-initarg-cache)
                   (= *soundness-initarg-cache-calls* 1)
                   (typep (make-instance 'soundness-initarg-cache :added 42) 'soundness-initarg-cache)
                   (= *soundness-initarg-cache-calls* 2))
              (remove-method #'initialize-instance method))))
        (reject) (= *soundness-initarg-cache-calls* 2)
        (typep (make-instance 'soundness-initarg-cache) 'soundness-initarg-cache))))`);
  console.log('Soundness: warmed initarg checks observe method keyword changes and invoke hooks once.');
  // A warmed generic must observe new applicability hooks and borrowed metadata.
  evaluate(`(defgeneric soundness-dispatch-protocol (x))
    (defvar *soundness-dispatch-protocol-calls* 0)
    (funcall (compile-native '(lambda ()
      (defmethod soundness-dispatch-protocol ((x integer))
        (declare (optimize (space 3) (speed 1) (debug 0) (safety 1))) x)))) (values)`);
  check(`(let ((driver (compile-native '(lambda (x)
      (declare (optimize (space 3) (speed 1) (debug 0) (safety 1)))
      (soundness-dispatch-protocol x))))
    (replacement (compile-native '(lambda (arguments)
      (declare (ignore arguments)) :borrowed))))
    (dotimes (i 8) (funcall driver i))
    (and
      (let ((method (eval '(defmethod compute-applicable-methods :around
          ((generic (eql #'soundness-dispatch-protocol)) arguments)
          (incf *soundness-dispatch-protocol-calls*) (call-next-method)))))
        (unwind-protect
          (let ((*soundness-dispatch-protocol-calls* 0))
            (and (= (funcall driver 41) 41)
                 (= *soundness-dispatch-protocol-calls* 1)
                 (= (funcall driver 42) 42)
                 (= *soundness-dispatch-protocol-calls* 2)))
          (remove-method #'compute-applicable-methods method)))
      (progn
        (dotimes (i 8) (funcall driver i))
        (let* ((info (symbol-function (compiler-primitive 10 1 "%%GENERIC-INFO")))
          (raw (symbol-function (compiler-primitive 10 1 "%%RAW-SLOTS")))
          (slots (funcall raw (funcall info #'soundness-dispatch-protocol)))
          (old (getf slots :discriminating-function)))
          (and (member :discriminating-function slots)
            (unwind-protect
              (progn (setf (getf slots :discriminating-function) replacement)
                (eq (funcall driver 31) :borrowed))
              (setf (getf slots :discriminating-function) old))
            (= (funcall driver 32) 32))))))`);
  console.log('Soundness: warmed generic dispatch observes applicability hooks and borrowed canonical metadata.');
  // The private native ABI accepts raw mutable eqref arrays. These standalone
  // stable WasmGC types canonicalize to that packet type across modules; the
  // fixture constructs and mutates packets without runtime layout indices.
  const section = (id, bytes) => [id, bytes.length, ...bytes];
  const body = bytes => [bytes.length + 1, 0, ...bytes];
  const packets = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, [5, 0x5e, 0x6d, 1,
      0x60, 2, 0x6f, 0x6f, 1, 0x6f, 0x60, 0, 1, 0x6f,
      0x60, 3, 0x6f, 0x7f, 0x6f, 0,
      0x60, 7, ...Array(7).fill(0x6f), 1, 0x6f]),
    ...section(3, [4, 1, 2, 3, 4]),
    ...section(7, [4, 4, 109, 97, 107, 101, 0, 0,
      5, 101, 109, 112, 116, 121, 0, 1,
      3, 115, 101, 116, 0, 2, 4, 119, 105, 100, 101, 0, 3]),
    ...section(10, [4,
      ...body([0x20, 0, 0xfb, 0x1a, 0xfb, 0x17, 0x6d,
        0x20, 1, 0xfb, 0x1a, 0xfb, 0x17, 0x6d,
        0xfb, 8, 0, 2, 0xfb, 0x1b, 0x0b]),
      ...body([0x41, 0, 0xfb, 7, 0, 0xfb, 0x1b, 0x0b]),
      ...body([0x20, 0, 0xfb, 0x1a, 0xfb, 0x16, 0,
        0x20, 1, 0x20, 2, 0xfb, 0x1a, 0xfb, 0x17, 0x6d,
        0xfb, 0x0e, 0, 0x0b]),
      ...body([...Array.from({length: 7}, (_, index) =>
        [0x20, index, 0xfb, 0x1a, 0xfb, 0x17, 0x6d]).flat(),
        0xfb, 8, 0, 7, 0xfb, 0x1b, 0x0b])]),
  ]))).exports;
  const elements = [vm.javascript.makeText('packet element'), null];
  const rejectPacketOperation = operation => {
    assert.throws(() => {
      try {operation();}
      catch (cause) {
        assert.ok(cause instanceof WebAssembly.Exception, 'Reject through a Lisp condition, without a Wasm trap.');
        throw vm.failure(cause);
      }
    }, error => error.code > 0 && error.message.length > 0);
    recover();
  };
  const rejectArgument = (arguments_, index) =>
    rejectPacketOperation(() => vm.vm['native-argument'](arguments_, index));
  for (const [arguments_, expected] of [[packets.make(...elements), elements],
    [packets.empty(), []], [vm.vm['native-list2'](...elements), elements]]) {
    expected.forEach((value, index) => assert.equal(vm.vm['native-argument'](arguments_, index), value));
    for (const index of [expected.length, 0x7fffffff])
      assert.equal(vm.vm['native-argument'](arguments_, index), null, 'Absent arguments return NIL.');
    for (const index of [-1, -0x80000000]) rejectArgument(arguments_, index);
  }
  rejectArgument(vm.vm['native-cons'](elements[0], elements[0]), 1);
  const owned = vm.vm['native-arguments'](elements.length);
  rejectArgument(owned, 0); // Incomplete construction must not expose a private sentinel.
  for (const [index, element] of elements.entries())
    assert.equal(vm.vm['native-argument-set'](owned, index, element), owned);
  assert.ok(vm.vm['native-argument-present'](owned, 1), 'Present NIL differs from an absent argument.');
  assert.equal(vm.vm['native-argument-present'](owned, elements.length), null);
  assert.equal(vm.vm['native-argument'](owned, 1), null);
  const foreign = {invalidPacket: true};
  for (const operation of [
    () => vm.vm['native-arguments'](-1),
    () => vm.vm['native-argument-set'](owned, -1, null),
    () => vm.vm['native-argument-set'](owned, elements.length, null),
    () => vm.vm['native-argument-set'](owned, 1, elements[0]),
    () => vm.vm['native-argument-set'](vm.vm['native-arguments'](1), 0, foreign),
    () => vm.vm['native-argument-set'](foreign, 0, null),
    () => vm.vm['native-argument'](foreign, 0),
    () => vm.vm['native-argument-present'](owned, -1),
    () => vm.vm['native-argument-present'](foreign, 0),
    () => vm.vm['native-check-arguments'](owned, -1, -1),
    () => vm.vm['native-check-arguments'](owned, elements.length, 0),
  ]) {
    rejectPacketOperation(operation);
    assert.equal(vm.vm['native-argument'](owned, 0), elements[0]);
    vm.vm['native-check-arguments'](owned, elements.length, elements.length);
  }
  console.log('Soundness: raw native packets preserve elements and reject invalid reads with recovery.');

  // Capture a Lisp function through the adapter's ordinary callback bridge,
  // then exercise the checked native boundary directly with foreign storage.
  const packetFunction = expression => {
    const callback = vm.javascript.callback;
    let function_;
    vm.javascript.callback = function (value) {
      function_ = value;
      return callback.call(this, value);
    };
    try {evaluate(`(js:callback ${expression}) (values)`);}
    finally {vm.javascript.callback = callback;}
    assert.ok(function_, 'The Lisp callback bridge supplied its function.');
    return function_;
  };
  // Private binder packets admit finite dotted spines, but cycles must fail
  // even when the requested slot occurs before the cycle. Repair the same
  // object to check that rejection leaves its storage and the runtime usable.
  for (const [length, entry] of [[1, 0], [3, 1], [4, 0]]) {
    const fixture = vm.javascript.link(0, vm.vm['native-call'](null,
      packetFunction(`(lambda ()
        (let* ((items (make-list ${length})) (end (last items)))
          (setf (cdr end) (nthcdr ${entry} items))
          (cons items (lambda () (setf (cdr end) nil) items))))`), packets.empty()));
    const cyclic = vm.javascript.link(0, fixture), repair = vm.javascript.link(1, fixture);
    for (const operation of [
      () => vm.vm['native-argument'](cyclic, 0),
      () => vm.vm['native-argument-present'](cyclic, 0),
      () => vm.vm['native-rest-argument'](cyclic, 0),
      () => vm.vm['native-check-arguments'](cyclic, 0, -1),
    ]) rejectPacketOperation(operation);
    assert.equal(vm.javascript.link(0, vm.vm['native-call'](null, repair, packets.empty())), cyclic);
    vm.vm['native-check-arguments'](cyclic, length, length);
    assert.deepEqual(vm.javascript.list(vm.vm['native-rest-argument'](cyclic, 0), value => value),
      Array(length).fill(null), 'Repair preserves every original argument slot.');
  }
  console.log('Soundness: private binder helpers reject circular spines and recover after in-place repair.');
  evaluate(`(defgeneric soundness-owned-next (a &optional b c d e &key marker))
    (funcall (compile-native '(lambda ()
      (defmethod soundness-owned-next ((a t) &optional b c d e &key marker)
        (declare (optimize (debug 0))) (values a b c d e marker))
      (defmethod soundness-owned-next ((a integer) &optional b c d e &key marker)
        (declare (optimize (debug 0)) (ignore b c d e marker))
        (setq a 999) (lambda () (call-next-method)))))) (values)`);
  const nextGeneric = packetFunction("#'soundness-owned-next");
  // JavaScript numbers become Lisp floats; dispatch must select the INTEGER
  // method that returns the escaped next-method closure.
  const originalFirst = vm.javascript.fromJS(1n), changedFirst = vm.javascript.fromJS(99n);
  const marker = vm.javascript.link(0,
    vm.vm['native-call'](null, packetFunction("(lambda () :marker)"), packets.empty()));
  const originalOperands = [originalFirst, null, vm.javascript.fromJS(3),
    vm.javascript.fromJS(4), vm.javascript.fromJS(5), marker, null];
  for (const call of [
    (function_, args) => vm.vm['native-primary-call'](null, function_, args),
    (function_, args) => vm.javascript.link(0, vm.vm['native-call'](null, function_, args)),
    (function_, args) => vm.vm['native-tail-call'](function_, args),
    (function_, args) => vm.vm['native-generic-call'](null, function_, args, 1),
    (function_, args) => vm.javascript.link(9, function_, args),
  ]) {
    // Repeat the same call so the escaped context also comes from a warm
    // mode19-eligible cache path (one required dispatch position, wide call).
    let escaped, arguments_;
    for (let warm = 0; warm < 3; warm++) {
      arguments_ = packets.wide(...originalOperands);
      escaped = call(nextGeneric, arguments_);
    }
    packets.set(arguments_, 0, changedFirst);
    packets.set(arguments_, 1, elements[0]);
    const values = vm.vm['native-call'](null, escaped, packets.empty());
    assert.deepEqual(vm.javascript.list(values, value => value),
      [originalFirst, null, ...originalOperands.slice(2, 5), null],
      'Escaped CALL-NEXT-METHOD retains original argument slots and all values after foreign array mutation.');
    // Completion through the fill-once builder also supplies no ownership
    // authority: the canonical foreign mutator can still alias that storage.
    const built = vm.vm['native-arguments'](originalOperands.length);
    originalOperands.forEach((value, index) => vm.vm['native-argument-set'](built, index, value));
    const builtEscaped = call(nextGeneric, built);
    packets.set(built, 0, changedFirst);
    assert.deepEqual(vm.javascript.list(vm.vm['native-call'](null, builtEscaped, packets.empty()), value => value),
      [originalFirst, null, ...originalOperands.slice(2, 5), null]);
    rejectPacketOperation(() => call(nextGeneric, vm.vm['native-arguments'](1)));
  }
  const sharedOperand = vm.vm['native-cons'](originalFirst, null);
  const sharedPacket = packets.wide(originalFirst, sharedOperand, ...originalOperands.slice(2));
  const sharedEscaped = vm.vm['native-primary-call'](null, nextGeneric, sharedPacket);
  vm.vm['native-cell-write'](sharedOperand, changedFirst);
  const sharedValues = vm.javascript.list(vm.vm['native-call'](null, sharedEscaped, packets.empty()), value => value);
  assert.equal(sharedValues[1], sharedOperand, 'Snapshotting slots preserves referenced Lisp object identity.');
  assert.equal(vm.vm['native-cell-read'](sharedValues[1]), changedFirst,
    'Changes to an argument object remain visible through the original argument.');
  console.log('Soundness: native ingress snapshots foreign raw slots before retaining escaped next-method calls.');

  let initializerPacket;
  vm.javascript.imports['soundness-packet'] = {mutate() {
    packets.set(initializerPacket, 2, changedFirst);
    packets.set(initializerPacket, 4, elements[0]);
  }};
  evaluate(`(js:define-import soundness-packet-mutate ("soundness-packet" "mutate") () :void)
    (values)`);
  const keywordList = vm.vm['native-primary-call'](null,
    packetFunction("(lambda () (list :later :third :allow-other-keys))"), packets.empty());
  const [later, third, allowOtherKeys] = vm.javascript.list(keywordList, value => value);
  const initialized = packetFunction(`(compile-native '(lambda (a &key
      (first (soundness-packet-mutate)) later third)
    (declare (optimize (debug 0)) (ignore first)) (list a later third)))`);
  const initializedType = vm.vm['core-object-type'](initialized);
  const initializedReader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${initializedType}:`));
  const initializedEntry = vm.vm[initializedReader](initialized)[initializedReader.split(':')[2].indexOf('e')];
  for (const call of [
    args => vm.javascript.link(0, vm.vm['native-call'](null, initialized, args)),
    args => vm.vm['native-primary-call'](null, initialized, args),
    args => vm.vm['native-tail-call'](initialized, args),
    args => vm.vm['native-generic-call'](null, initialized, args, 1),
    args => vm.javascript.link(9, initialized, args),
    args => initializedEntry(initialized, args),
  ]) {
    initializerPacket = packets.wide(originalFirst, later, originalOperands[2],
      third, originalOperands[3], allowOtherKeys, vm.javascript.true);
    assert.deepEqual(vm.javascript.list(call(initializerPacket), value => value),
      [originalFirst, originalOperands[2], originalOperands[3]],
      'A callback in an earlier default cannot mutate later supplied argument slots.');
    assert.equal(vm.vm['native-argument'](initializerPacket, 2), changedFirst,
      'The initializer actually mutated the foreign source packet.');
  }
  delete vm.javascript.imports['soundness-packet'];

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
                          (lambda () counter)
                          (lambda (effect)
                            (let ((before counter) (child (lambda () counter)))
                              (funcall effect)
                              (values before counter child))))))
              (defun image-worker-function-captures (initial)
                (declare (optimize (debug 0)))
                (let ((counter initial))
                  (labels ((entry (n) (funcall (target) n))
                           (target () #'walk)
                           (walk (n)
                             (if (zerop n) (values counter #'entry #'walk)
                               (progn (incf counter) (again (1- n)))))
                           (again (n) (walk n))
                           (observe () (lambda () (values counter #'walk))))
                    (declare (notinline entry target walk again observe))
                    (values #'entry #'observe (lambda (delta) (incf counter delta))))))
              (defun image-worker-wide (a b c d e)
                (declare (optimize (debug 0))) (values a b c d e)))
            :inline nil))
      (let ((fixed (cdr (assoc 'image-worker-check entries)))
            (tail (cdr (assoc 'image-worker-tail entries)))
            (captures (cdr (assoc 'image-worker-captures entries)))
            (function-captures (cdr (assoc 'image-worker-function-captures entries)))
            (wide (cdr (assoc 'image-worker-wide entries))))
        (and (plusp (length bytes))
          (every (lambda (entry) (compiled-function-p (cdr entry))) entries)
          (equal (multiple-value-list (funcall fixed 41)) '(42 :second))
          (equal (multiple-value-list (funcall tail 41)) '(42 :second))
          (handler-case (progn (funcall fixed) nil) (program-error () t))
          (handler-case (progn (funcall fixed 1 2) nil) (program-error () t))
          (equal (multiple-value-list (funcall fixed 42)) '(43 :second))
          (multiple-value-bind (advance observe probe) (funcall captures 10)
            (multiple-value-bind (other-advance other-observe) (funcall captures 100)
              (and (= (funcall advance 2) 12) (= (funcall observe) 12)
                (= (funcall other-observe) 100) (= (funcall other-advance 3) 103)
                (= (funcall observe) 12)
                (multiple-value-bind (before after child)
                    (funcall probe (lambda () (funcall advance 5) (values :ignored :extra)))
                  (and (= before 12) (= after 17) (= (funcall child) after)
                    (= (funcall advance 1) 18) (= (funcall child) 18)
                    (= (funcall other-observe) 103))))))
          (multiple-value-bind (entry observe advance) (funcall function-captures 10)
            (multiple-value-bind (other-entry other-observe) (funcall function-captures 100)
              (let ((read (funcall observe)) (other-read (funcall other-observe)))
                (multiple-value-bind (answer same-entry walk) (funcall entry 3)
                  (multiple-value-bind (observed same-walk) (funcall read)
                    (and (= answer 13) (= observed answer) (eq entry same-entry)
                      (eq walk same-walk) (= (funcall advance 2) 15)
                      (= (funcall read) 15) (= (funcall other-read) 100)
                      (= (funcall other-entry 1) 101) (= (funcall read) 15)))))))
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

  const reject = (source, reason, conditionType = 'compiler-input-error') => {
    const output = evaluate(`(handler-case (progn ${source} :unexpected-acceptance)
      (${conditionType} (condition) (princ-to-string condition)))`);
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
  // APPLY's public boundary requires a proper final list even though the
  // private destructuring binder also accepts finite dotted spines.
  for (const tail of ['arguments', ':dotted']) {
    check(`(let* ((arguments (list 1 nil 3)) (end (last arguments))
        (native (compile-native '(lambda (arguments) (apply #'list arguments)))))
      (every (lambda (function)
        (setf (cdr end) ${tail})
        (let ((rejected
            (handler-case (progn (funcall function arguments) nil)
              (type-error (condition)
                (and (type-error-expected-type condition)
                  (plusp (length (princ-to-string condition))))))))
          (setf (cdr end) nil)
          (and rejected (equal (funcall function arguments) '(1 nil 3)))))
        (list (lambda (arguments) (apply #'list arguments)) native)))`);
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
  ]) for (const consumer of ['emit-native-module', 'emit-hir-code', 'optimize-hir', 'infer-hir-types'])
    reject(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 1))))
      (function (first (hir-unit-functions unit)))
      (block (first (hir-function-blocks function)))
      (instruction (first (hir-block-instructions block))))
    ${mutation} (${consumer} unit))`, reason);
  // Public CALL metadata is validated before compiler internals or retained-code
  // serialization; malformed policy data must leave the compiler usable.
  for (const attributes of [':speed', '17', "'(:unknown)", "'(:speed :speed)",
    "(cons :speed :dotted)", "(let ((cycle (list :speed))) (setf (cdr cycle) cycle) cycle)"]) {
    reject(`(let* ((unit (lower-source-to-hir '(lambda (x) (soundness-unknown x))))
        (instructions (mapcan (lambda (function)
          (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
            (hir-function-blocks function))) (hir-unit-functions unit)))
        (call (find :call instructions :key (lambda (instruction) (hir-instruction-opcode instruction)))))
      (setf (hir-instruction-attributes call) ${attributes})
      (emit-native-module unit))`, /attribute|policy|dotted|circular/);
  }
  for (const selectEntry of [
    '(emit-native-module unit :entries (list function))',
    '(setf (hir-unit-functions unit) (cons function (remove function (hir-unit-functions unit)))) (emit-native-module unit)',
  ]) reject(`(let* ((unit (compile-to-hir '(lambda (x) (lambda () x))))
      (function (find-if (lambda (function) (hir-function-captures function)) (hir-unit-functions unit))))
    ${selectEntry})`, /entry requires a closure environment/);
  // Private activation reads carry checked attributes through public HIR and
  // the current wire codec. Ordinary lambda-list semantics live in ANSI tests.
  for (const [opcode, attributes, reason] of [
    [':argument-present', "'(-1)", /index/],
    [':keyword-present', "'(0 17)", /symbol/],
    [':keyword-argument', "'(-1 :sample)", /index/],
    [':check-keywords', "'(0 (:sample . :tail) nil)", /dotted/],
    [':check-keywords', "'(0 (17) nil)", /symbol/],
    [':check-keywords', "'(0 (:sample) :yes)", /boolean/],
    [':check-keywords', "(let ((keys (list :sample))) (setf (cdr keys) keys) (list 0 keys nil))", /circular/],
  ]) reject(`(let* ((unit (lower-source-to-hir
        '(lambda (&optional (x nil xp) &key (sample nil sp)) (values x xp sample sp))))
      (instructions (mapcan (lambda (function)
        (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
          (hir-function-blocks function))) (hir-unit-functions unit)))
      (instruction (find ${opcode} instructions
        :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (setf (hir-instruction-attributes instruction) ${attributes})
    (emit-hir-code unit))`, reason);

  evaluate(`(defun soundness-keyword-wire ()
      (emit-hir-code (compile-to-hir
        '(lambda (&optional (x :default xp) &key ((soundness-key y) :other yp))
           (values x xp y yp)))))
    (defun soundness-keyword-descriptor-slot (wire)
      (let* ((payload (validate-hir-code wire))
             (functions (svref payload 1)))
        (loop for function across functions do
          (let ((scratch (make-array (+ 6 (svref function 7)) :initial-element 0)))
            (loop for block across (svref function 5) do
              (let ((pc 0) (code (svref block 2)))
                (loop while (< pc (length code)) do
                  (setq pc (read-hir-code-instruction code pc scratch))
                  (when (eq (first (svref *hir-code-operations* (svref scratch 0))) :check-keywords)
                    (return-from soundness-keyword-descriptor-slot (svref scratch 5))))))))
        (error "Missing keyword descriptor"))) (values)`);
  for (const descriptor of ["'(nil)", "'((17) nil)", "'((soundness-key . :tail) nil)",
    "'((soundness-key) :yes)",
    "(let ((keys (list 'soundness-key))) (setf (cdr keys) keys) (list keys nil))"]) {
    reject(`(let* ((wire (soundness-keyword-wire))
        (slot (soundness-keyword-descriptor-slot wire)))
      (setf (svref (svref wire 0) slot) ${descriptor})
      (load-hir-code wire))`, /keyword|symbol|circular|dotted/, 'hir-code-input-error');
  }
  check(`(let* ((wire (soundness-keyword-wire))
      (decoded (validate-hir-code wire))
      (slot (soundness-keyword-descriptor-slot wire))
      (interpreted (load-hir-code wire))
      (native (compile-native-hir (copy-hir-code decoded))))
    ;; Both loading and exporting privately copy executable keyword metadata.
    (setf (first (svref (svref wire 0) slot)) nil
          (second (svref (svref wire 0) slot)) t)
    (let ((exported (copy-hir-code decoded)))
      (setf (first (svref (svref exported 0) slot)) nil
            (second (svref (svref exported 0) slot)) t))
    (every (lambda (function)
      (and (equal (multiple-value-list (funcall function)) '(:default nil :other nil))
        (equal (multiple-value-list (funcall function nil 'soundness-key nil)) '(nil t nil t))
        (handler-case (progn (funcall function nil :unrecognized 1) nil) (program-error () t))
        (equal (multiple-value-list (funcall function 7 'soundness-key 9)) '(7 t 9 t))))
      (list interpreted native (load-hir-code (copy-hir-code decoded)))))`);
  evaluate("(fmakunbound 'soundness-keyword-wire) (fmakunbound 'soundness-keyword-descriptor-slot) (values)");
  // Capture metadata describes the actual closure environment. Public HIR
  // must not name an absent slot or mistake the private child-cache slot for
  // a binding, even when the individual records and operands are well formed.
  for (const [opcode, mutation, reason] of [
    [':capture', '(setf (hir-instruction-output instruction) nil)', /capture must produce/],
    [':capture', '(setf (first (hir-instruction-attributes instruction)) (length (hir-function-captures function)))', /index and identity/],
    [':capture', '(setf (second (hir-instruction-attributes instruction)) (second (hir-function-captures function)))', /index and identity/],
    [':closure', '(setf (hir-instruction-inputs instruction) (cdr (hir-instruction-inputs instruction)))', /operand count/],
    [':closure', '(push (first (hir-instruction-inputs instruction)) (hir-instruction-inputs instruction))', /operand count/],
  ]) reject(`(let* ((unit (compile-to-hir '(lambda (x y) (lambda () (values x y)))))
      (function (find-if (lambda (function)
        (find ${opcode} (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
          (hir-function-blocks function)) :key (lambda (instruction) (hir-instruction-opcode instruction)))) (hir-unit-functions unit)))
      (instruction (find ${opcode} (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
        (hir-function-blocks function)) :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    ${mutation} (emit-native-module unit))`, reason);
  // Sharing and literal cycles are valid inputs, unlike cyclic executable code.
  check(`(let ((object (list :cycle)) (shared '(+ x 1)))
    (setf (cdr object) object)
    (and (eq object (funcall (compile-native (list 'lambda nil (list 'quote object)))))
      (equal (funcall (compile-native (list 'lambda '(x) (list 'list shared shared))) 1) '(2 2))))`);
  // Optimization declarations survive retained-code round trips without
  // changing values or turning advisory metadata into an unchecked boundary.
  check(`(loop for space from 0 to 3 for speed in '(3 2 1 0)
      for effort from 0 to 3 always
      (let* ((source (list 'lambda '(x)
              (list 'declare (list 'optimize (list 'space space) (list 'speed speed)
                (list 'compilation-speed effort) '(debug 0))) '(values x nil)))
             (unit (compile-to-hir source))
             (wire (emit-hir-code-checked unit))
             (decoded (validate-hir-code wire))
             (exported (copy-hir-code decoded))
             (rebuilt (hir-code-to-unit-checked decoded))
             (expected (+ speed (* 4 space) (* 16 effort))))
        (and (= expected (hir-function-optimization-policy (first (hir-unit-functions unit))))
          (= expected (hir-function-optimization-policy (first (hir-unit-functions rebuilt))))
          (equalp wire exported)
          (every (lambda (function)
            (equal (multiple-value-list (funcall function 42)) '(42 nil)))
            (list (load-hir-code exported) (compile-native-hir exported))))))`);
  // OPTIMIZE is a free declaration: lambda initializers inherit their outer
  // scope. Whole-function sharing must conservatively retain both policies.
  check(`(let ((outer (compiler-primitive 52 nil)))
    (loop for space from 0 to 3 for speed in '(3 2 1 0)
      for effort from 0 to 3 always
      (let* ((source (list 'lambda '(x &optional (y x) &aux (z y))
              (list 'declare (list 'optimize (list 'space space) (list 'speed speed)
                (list 'compilation-speed effort) '(debug 0))) '(values z nil)))
             (unit (compile-to-hir source)) (wire (emit-hir-code-checked unit))
             (expected (+ (max speed (logand outer 3))
               (* 4 (min space (logand 3 (ash outer -2))))
               (* 16 (max effort (ash outer -4))))))
        (and (= expected (hir-function-optimization-policy (first (hir-unit-functions unit))))
          (every (lambda (function)
            (and (equal (multiple-value-list (funcall function 42)) '(42 nil))
                 (equal (multiple-value-list (funcall function 42 43)) '(43 nil))))
            (list (load-hir-code wire) (compile-native-hir wire)))))))`);
  for (const policy of ['-1', '64', 'nil'])
    reject(`(let* ((unit (compile-to-hir '(lambda () 42)))
        (function (first (hir-unit-functions unit))))
      (setf (hir-function-optimization-policy function) ${policy})
      (emit-native-module unit))`, /optimization policy/);
  for (const policy of ['-1', '256', 'nil'])
    reject(`(let* ((wire (emit-hir-code-checked (compile-to-hir '(lambda () 42))))
        (descriptor (svref (svref wire 1) 0)))
      (setf (svref descriptor (1- (length descriptor))) ${policy})
      (load-hir-code wire))`, /function descriptor/, 'hir-code-input-error');
  console.log('Soundness: compiler boundaries reject malformed graphs and recover.');

  for (const consumer of ['emit-native-module', 'emit-hir-code'])
    reject(`(let* ((unit (lower-source-to-hir
          '(lambda (flag) (if flag (values 1 2) (values 3 4)))))
        (function (first (hir-unit-functions unit)))
        (join (find-if (lambda (block) (hir-block-parameters block))
                       (hir-function-blocks function)))
        (parameters (hir-block-parameters join)))
      (setf (cdr parameters) parameters)
      (${consumer} unit))`, /circular/, 'compiler-input-error');

  for (const consumer of ['emit-native-module', 'emit-hir-code'])
    reject(`(let* ((unit (lower-source-to-hir
          '(lambda (flag) (if flag (values 1 2) (values 3 4)))))
        (function (first (hir-unit-functions unit)))
        (join (find-if (lambda (block) (hir-block-parameters block))
                       (hir-function-blocks function)))
        (instruction (first (hir-block-instructions (hir-function-entry function)))))
      (setf (hir-instruction-output instruction) (first (hir-block-parameters join)))
      (${consumer} unit))`, /output|producer/, 'compiler-input-error');

  for (const consumer of ['emit-native-module', 'emit-hir-code']) {
    for (const mutation of [
      '(setf (hir-value-owner output) (second (hir-unit-functions unit)))',
      '(setf (hir-value-producer output) (hir-function-entry function))',
    ]) reject(`(let* ((unit (lower-source-to-hir '(lambda () (lambda () 42))))
        (function (first (hir-unit-functions unit)))
        (instruction (find-if (lambda (instruction) (hir-instruction-output instruction))
                       (hir-block-instructions (hir-function-entry function))))
        (output (hir-instruction-output instruction)))
      ${mutation}
      (${consumer} unit))`, /output|owner|producer/, 'compiler-input-error');
  }

  // Block-group metadata is untrusted current-format input. Check both
  // callable loading and native reconstruction, with recovery after each
  // malformed descriptor. The scalar parameter field encodes register+1.
  for (const consumer of ['load-hir-code', 'compile-native-hir']) {
    reject(`(let* ((wire (emit-hir-code-checked (compile-to-hir
          '(lambda (flag) (declare (optimize (debug 0)))
             (if flag (values 1 2) (values 3 4))))))
        (descriptor (svref (svref wire 1) 0))
        (blocks (unpack-hir-blocks (svref descriptor 5)))
        (join (find-if (lambda (block) (plusp (svref block 0))) blocks)))
      (setf (svref join 0) (1+ (svref descriptor 1)))
      (setf (svref descriptor 5) (pack-hir-blocks blocks))
      (${consumer} wire))`, /block descriptor|parameter|register/, 'hir-code-input-error');
    reject(`(let* ((wire (emit-hir-code-checked (compile-to-hir '(lambda () 42))))
        (descriptor (svref (svref wire 1) 0)))
      (setf (svref descriptor 1) #x7fffffff)
      (${consumer} wire))`, /register count/, 'hir-code-input-error');
  }

  // Exercise the complete retained-code transport contract. Parallel DO stepping
  // permutes live locals across backedges; both branches assign a live value.
  check(`(let* ((wire (emit-hir-code (compile-to-hir
        '(lambda (n choose empty effect)
           (declare (optimize (debug 0)))
           (let ((state :seed))
             (do ((i n (1- i)) (a :a b) (b :b c) (c :c a))
                 ((zerop i) (if empty (values) (values a b c state)))
               (funcall effect i)
               (if choose (setq state (multiple-value-list (values a b)))
                          (setq state (multiple-value-list (values b c))))))))))
      (decoded (validate-hir-code wire))
      (exported (copy-hir-code decoded))
      (rebuilt (hir-code-to-unit exported)))
    (verify-hir rebuilt)
    (every (lambda (function)
      (let ((effects nil))
        (labels ((effect (i) (push i effects) (values :ignored :extra)))
          (and (equal (multiple-value-list (funcall function 0 t nil #'effect))
                      '(:a :b :c :seed))
               (null effects)
               (equal (multiple-value-list (funcall function 1 t nil #'effect))
                      '(:b :c :a (:a :b)))
               (equal effects '(1))
               (progn (setq effects nil)
                 (equal (multiple-value-list (funcall function 2 nil nil #'effect))
                        '(:c :a :b (:c :a))))
               (equal effects '(1 2))
               (null (multiple-value-list (funcall function 3 t t #'effect)))))))
      (list (load-hir-code exported) (compile-native-hir exported)
            (compile-native-hir (emit-hir-code rebuilt)))))`);
  console.log('Soundness: retained code preserves parallel loop transfers, effects and values, and rejects malformed metadata with recovery.');

  // Construct corrupt wire using the private emitter only as a fixture. A
  // well-formed byte stream is not proof of lexical or control-flow semantics
  // when stored code is admitted to the unchecked native compiler.
  for (const [source, mutation, reason] of [
    ["(lambda () (lambda () 42))", `
      (let ((closure (find :closure (hir-block-instructions (first (hir-function-blocks function)))
                      :key (lambda (instruction) (hir-instruction-opcode instruction)))))
        (setf (hir-instruction-attributes closure) (list function)))`, /parent|lexical|circular|cycle/],
    ["(lambda (x) (if x (values 1 2) (values 3 4)))", `
      (let* ((join (find-if (lambda (block) (hir-block-parameters block)) (hir-function-blocks function)))
             (branch (find :branch (mapcar (lambda (block) (hir-block-terminator block))
                                    (hir-function-blocks function))
                       :key (lambda (instruction) (hir-instruction-opcode instruction)))))
        (setf (hir-instruction-attributes branch) (list join join)))`, /conditional|branch|parameter|block arguments/],
    ["(lambda (x) (if x (values 1 2) (values 3 4)))", `
      (let ((jump (find-if (lambda (instruction)
                    (and (eq (hir-instruction-opcode instruction) :jump)
                         (hir-block-parameters (first (hir-instruction-attributes instruction)))))
                   (mapcar (lambda (block) (hir-block-terminator block))
                     (hir-function-blocks function)))))
        (setf (hir-instruction-inputs jump) nil))`, /group|parameter|block arguments/],
  ]) reject(`(let* ((unit (compile-to-hir '${source}))
      (function (first (hir-unit-functions unit))))
    ${mutation}
    (compile-native-hir (emit-hir-code-checked unit)))`, reason, 'hir-code-input-error');

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
  // Closed-call facts belong to one emission. The default first entry remains
  // open even when valid public HIR also creates and calls its closure.
  check(`(let* ((unit (compile-to-hir '(lambda ()
        (declare (optimize (debug 0)))
        (flet ((worker (flag) (if flag :true :false)))
          (declare (notinline worker)) (worker nil)))))
      (worker (hir-local-function (hir-binding-local
        (find 'worker (hir-function-bindings (first (hir-unit-functions unit)))
          :key (lambda (binding) (hir-binding-name binding)))))))
    (setf (hir-unit-functions unit) (cons worker (remove worker (hir-unit-functions unit))))
    (verify-hir unit)
    (multiple-value-bind (bytes literals) (emit-native-module unit)
      (let ((native (compiler-primitive 17 bytes literals nil)))
        (and (eq :true (funcall native t)) (eq :false (funcall native nil))))))`);

  check(`(let* ((unit (compile-to-hir '(lambda ()
        (declare (optimize (debug 0)))
        (flet ((worker (flag) (if flag :true :false)))
          (declare (notinline worker)) (worker nil)))))
      (worker (hir-local-function (hir-binding-local
        (find 'worker (hir-function-bindings (first (hir-unit-functions unit)))
          :key (lambda (binding) (hir-binding-name binding))))))
      (before (emit-hir-code unit)))
    (and (verify-hir unit)
      (multiple-value-bind (bytes literals) (emit-native-module unit)
        (eq :false (funcall (compiler-primitive 17 bytes literals nil))))
      (equalp before (emit-hir-code unit))
      (multiple-value-bind (bytes literals) (emit-native-module unit :entries (list worker))
        (let ((native (compiler-primitive 17 (cons bytes 0) (first literals) nil)))
          (and (eq :true (funcall native t)) (eq :false (funcall native nil)))))
      (equalp before (emit-hir-code unit))
      (progn
        (setf (hir-unit-functions unit) (cons worker (remove worker (hir-unit-functions unit))))
        (verify-hir unit)
        (multiple-value-bind (bytes literals) (emit-native-module unit)
          (let ((native (compiler-primitive 17 bytes literals nil)))
            (and (eq :true (funcall native t)) (eq :false (funcall native nil))))))))`);

  // Public value-list order is independent of IDs. A long forwarded closure
  // alias returned to outside callers must keep unrestricted arguments.
  check(`(let* ((unit (compile-to-hir '(lambda () (declare (optimize (debug 0)))
                 (flet ((worker (flag) (if flag :true :false)))
                   (declare (notinline worker)) (worker nil)))))
       (root (first (hir-unit-functions unit)))
       (closure (loop for block in (hir-function-blocks root)
                      thereis (find :closure (hir-block-instructions block)
                                :key (lambda (i) (hir-instruction-opcode i)))))
       (exit (find-if (lambda (b) (eq :return (hir-instruction-opcode (hir-block-terminator b))))
                     (hir-function-blocks root)))
       (value (hir-instruction-output closure)) (chain nil))
  (dotimes (index 120)
    (let* ((out (make-hir-value-record (hir-function-value-count root) :single root nil t))
           (copy (make-hir-instruction-record :copy (list value) out nil nil :pure)))
      (incf (hir-function-value-count root)) (push out (hir-function-values root))
      (setf (hir-value-producer out) copy) (push copy chain) (setq value out)))
  (let* ((out (make-hir-value-record (hir-function-value-count root) :multiple root nil t))
         (group (make-hir-instruction-record :values (list value) out nil nil :pure)))
    (incf (hir-function-value-count root)) (push out (hir-function-values root))
    (setf (hir-value-producer out) group
          (hir-block-instructions exit) (append (hir-block-instructions exit) (nreverse chain) (list group))
          (hir-instruction-inputs (hir-block-terminator exit)) (list out)
          (hir-function-values root) (reverse (hir-function-values root))))
  (verify-hir unit)
  (multiple-value-bind (bytes literals) (emit-native-module unit)
    (let ((callback (funcall (compiler-primitive 17 bytes literals nil))))
      (and (eq :true (funcall callback t)) (eq :false (funcall callback nil))))))`);

  // Explicit sibling exports share their lexical literal bank in every entry
  // order, including an exported parent beside its no-capture children.
  check(`(let* ((unit (compile-to-hir '(lambda () (declare (optimize (debug 0)))
                  (flet ((one (flag) (if flag :one-true :one-false))
                         (two (flag) (if flag :two-true :two-false)))
                    (declare (notinline one two)) (values (one nil) (two nil))))))
       (root (first (hir-unit-functions unit)))
       (one (hir-local-function (hir-binding-local (find 'one (hir-function-bindings root)
                           :key (lambda (b) (hir-binding-name b))))))
       (two (hir-local-function (hir-binding-local (find 'two (hir-function-bindings root)
                           :key (lambda (b) (hir-binding-name b)))))))
  (verify-hir unit)
  (loop for entries in (list (list one two) (list two one) (list root one two) (list two one root)) always
    (multiple-value-bind (bytes literals) (emit-native-module unit :entries entries)
      (loop for entry in entries for bank in literals for index from 0 always
        (let ((function (compiler-primitive 17 (cons bytes index) bank nil)))
          (cond ((eq entry root) (equal (multiple-value-list (funcall function)) '(:one-false :two-false)))
                ((eq entry one) (and (eq (funcall function t) :one-true) (eq (funcall function nil) :one-false)))
                (t (and (eq (funcall function t) :two-true) (eq (funcall function nil) :two-false)))))))))`);

  // Factory entries escape through ordinary list construction, including the
  // default-all entry selection. Only explicitly private helpers are closed.
  for (const entries of ['', ":entry-points '(soundness-ipcp-root)", ':entry-points nil']) {
    check(`(let ((module (compile-native-module
        '((defun soundness-ipcp-worker (flag)
            (declare (optimize (debug 0))) (if flag :true :false))
          (defun soundness-ipcp-root ()
            (declare (notinline soundness-ipcp-worker) (optimize (debug 0)))
            (soundness-ipcp-worker nil))) :inline nil ${entries})))
      ${entries === ':entry-points nil' ? '(null module)' : `(and
        (eq :false (funcall (cdr (assoc 'soundness-ipcp-root module))))
        ${entries === '' ? `(let ((worker (cdr (assoc 'soundness-ipcp-worker module))))
          (and (eq :true (funcall worker t)) (eq :false (funcall worker nil))))` : 't'})`})`);
  }

  // Primary truth cannot discard an ordinary worker call, its argument
  // effects, a secondary VALUES error, or the distinction between groups.
  for (const returned of ['(values)', '(values nil)', '(values nil :second :third)']) {
    check(`(let ((effects nil) (function (compile-native '(lambda (effect full)
          (declare (optimize (debug 0)))
          (flet ((worker (left right)
                   (funcall effect :body) ${returned}))
            (declare (notinline worker))
            (if full
              (multiple-value-list (worker (funcall effect :left) (funcall effect :right)))
              (if (worker (funcall effect :left) (funcall effect :right)) :true :false)))))))
      (and (eq :false (funcall function (lambda (event) (push event effects)) nil))
        (equal effects '(:body :right :left))
        (progn (setq effects nil)
          (and (equal (funcall function (lambda (event) (push event effects)) t)
                      '${returned === '(values)' ? 'nil' : returned === '(values nil)' ? '(nil)' : '(nil :second :third)'})
            (equal effects '(:body :right :left))))))`);
  }
  check(`(let ((events nil) (function (compile-native '(lambda (effect bad)
        (declare (optimize (debug 0)))
        (flet ((worker (x)
                 (declare (inline car) (optimize (safety 1)))
                 (funcall effect :body) (values nil (car x))))
          (declare (notinline worker))
          (if (worker (progn (funcall effect :argument) bad)) :true :false))))))
    (and (handler-case
        (progn (funcall function (lambda (event) (push event events)) :invalid) nil)
        (type-error () (equal events '(:body :argument))))
      (eq :false (funcall function (lambda (event) (push event events)) '(17)))))`);
  // Unsupported argument protocols keep their ordinary default/supplied-p
  // effects, and one malformed call must not seal the other call sites.
  check(`(let ((events nil) (function (compile-native '(lambda (effect)
        (declare (optimize (debug 0)))
        (flet ((worker (flag &optional (extra (progn (funcall effect :default) :default) supplied))
                 (values flag extra supplied)))
          (declare (notinline worker))
          (list (if (worker nil) :true :false)
                (multiple-value-list (worker nil :explicit))))))))
    (and (equal (funcall function (lambda (event) (push event events)))
                '(:false (nil :explicit t)))
      (equal events '(:default))))`);
  check(`(let ((function (compile-native '(lambda (bad effect)
        (declare (optimize (debug 0)))
        (flet ((worker (flag) (declare (ignore flag)) nil))
          (declare (notinline worker))
          (if bad (worker (funcall effect :first) (funcall effect :second))
            (if (worker nil) :true :false)))))) (events nil))
    (and (eq :false (funcall function nil (lambda (event) (push event events))))
      (handler-case
        (progn (funcall function t (lambda (event) (push event events))) nil)
        (program-error () (equal events '(:second :first))))
      (eq :false (funcall function nil (lambda (event) (push event events))))))`);
  console.log('Soundness: closed-call facts retain open entries, fresh emission ownership, effects, arity and complete value groups.');
  // A primary return fact describes successful returns; it cannot replace a
  // call that diverges. Interrupt it using the same VM boundary as ANSI, then
  // prove that ordinary compilation still works in the unwound VM.
  evaluate(`(setf (symbol-function 'soundness-ipcp-diverge)
    (compile-native '(lambda (flag)
      (declare (optimize (debug 0)))
      (labels ((worker (flag) (if flag nil (worker flag))))
        (declare (notinline worker))
        (if (worker flag) :true :false))))) (values)`);
  try {
    const context = createContext({run: () => evaluate('(soundness-ipcp-diverge nil)')});
    assert.throws(() => new Script('run()').runInContext(context, {timeout: 250}),
      error => error.code === 'ERR_SCRIPT_EXECUTION_TIMEOUT');
  } finally {vm.vm.unwind();}
  recover();

  // Wide, valid nonalias operands still consume provenance work. Keep this
  // graph proof-only: it deliberately exceeds the bounded audit's work cap.
  check(`(let* ((unit (lower-source-to-hir '(lambda (x effect)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (values y y)))
          (declare (notinline worker))
          (funcall #'worker (progn (funcall effect :operand) x))))))
      (root (first (hir-unit-functions unit)))
      (binding (find 'worker (hir-function-bindings root)
        :key (lambda (binding) (hir-binding-name binding))))
      (worker (hir-local-function (hir-binding-local binding)))
      (barriers (hir-optimization-barriers unit))
      (facts (hir-result-callees unit barriers))
      (call (find-if (lambda (instruction)
        (and (eq (hir-instruction-opcode instruction) :call)
          (eq worker (hir-result-callee (second (hir-instruction-inputs instruction)) facts))))
        (mapcan (lambda (block) (copy-list (hir-block-instructions block))) (hir-function-blocks root))))
      (callee (and call (second (hir-instruction-inputs call))))
      (group (find :values
        (mapcan (lambda (block) (copy-list (hir-block-instructions block))) (hir-function-blocks worker))
        :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (and (verify-hir unit) callee (null (hir-result-callee callee))
      (let ((sources (hir-primary-flow unit barriers)))
        (and (null (gethash callee sources))
          (progn (hir-local-callee-flow unit barriers sources) (gethash callee sources))))
      (progn
        (setf (hir-instruction-inputs group)
          (make-list 100000 :initial-element (first (hir-instruction-inputs group))))
        (verify-hir unit)
        (and (null (hir-result-callee callee (hir-result-callees unit barriers)))
          (let ((sources (make-hash-table :test 'eq)))
            (setf (gethash :retained sources) :retained)
            (hir-local-callee-flow unit barriers sources)
            (and (null (gethash callee sources))
              (eq :retained (gethash :retained sources))))))))`);
  // A small unproved call retains selected closure context, operand order
  // and the complete replacement return group after a conservative fallback.
  check(`(let* ((unit (compile-to-hir '(lambda (flag replacement capture x effect)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (values capture y)))
          (declare (notinline worker))
          (funcall (progn (funcall effect :selected) (if flag #'worker replacement))
            (progn (funcall effect :operand) x))))))
      (root (first (hir-unit-functions unit)))
      (return (find :return (mapcar (lambda (block) (hir-block-terminator block)) (hir-function-blocks root))
        :key (lambda (instruction) (hir-instruction-opcode instruction))))
      (call (hir-value-producer (first (hir-instruction-inputs return))))
      (effects nil)
      (replacement (lambda (y) (values :replacement y :extra))))
    (and (eq (hir-instruction-opcode call) :call)
      (null (hir-result-callee (second (hir-instruction-inputs call))
                 (hir-result-callees unit (hir-optimization-barriers unit))))
      (let ((native (compile-native-hir (emit-hir-code-checked unit))))
        (and (equal (multiple-value-list
                 (funcall native t replacement 17 19 (lambda (event) (push event effects)))) '(17 19))
          (equal effects '(:operand :selected))
          (progn (setq effects nil)
            (and (equal (multiple-value-list
                   (funcall native nil replacement 23 29 (lambda (event) (push event effects))))
                   '(:replacement 29 :extra))
              (equal effects '(:operand :selected))))))))`);
  // Executable same-code joins retain the selected closure's context and
  // operand effects. NOTINLINE keeps the worker out of the caller's body.
  for (const count of [2, 4]) for (const full of [false, true]) {
    const returned = ['capture', ...Array(count - 1).fill('y')].join(' ');
    const call = '(funcall selected (progn (funcall effect :operand) x))';
    check(`(let ((effects nil) (function (compile-native '(lambda (flag capture x effect)
          (declare (optimize (debug 0)))
          (flet ((worker (y) (declare (optimize (debug 0))) (values ${returned})))
            (declare (notinline worker))
            (let ((selected (if flag #'worker (values #'worker (funcall effect :branch)))))
              ${full ? `(multiple-value-list ${call})` : `(values ${call})`}))))))
      (and (equal (funcall function t 17 19 (lambda (x) (push x effects)))
                  ${full ? `'(17 ${Array(count - 1).fill('19').join(' ')})` : '17'})
        (equal effects '(:operand))
        (progn (setq effects nil)
          (and (equal (funcall function nil 23 29 (lambda (x) (push x effects)))
                      ${full ? `'(23 ${Array(count - 1).fill('29').join(' ')})` : '23'})
            (equal effects '(:operand :branch))))))`);
  }
  // Mixed targets, unknown ingress and a captured mutable function variable
  // must retain general dispatch and the replacement's complete return group.
  for (const selection of [
    "(if flag #'worker #'other)",
    "(if flag #'worker replacement)",
    `(let ((selected #'worker))
       (flet ((replace () (setq selected replacement)))
         (declare (notinline replace))
         (unless flag (replace)) selected))`,
    `(block chosen
       (flet ((escape () (return-from chosen replacement)))
         (declare (notinline escape))
         (if flag #'worker (escape))))`,
  ]) check(`(let* ((function (compile-native '(lambda (flag replacement x)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (values y y))
               (other (y) (declare (ignore y)) (values :replacement :second :third)))
          (declare (notinline worker other))
          (funcall ${selection} x)))))
      (replacement (lambda (y) (declare (ignore y)) (values :replacement :second :third))))
    (and (equal (multiple-value-list (funcall function t replacement 29)) '(29 29))
      (equal (multiple-value-list (funcall function nil replacement 29)) '(:replacement :second :third))))`);
  check(`(let* ((factory (compile-native '(lambda (flag)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (declare (inline car) (optimize (safety 1))) (car y)))
          (declare (notinline worker))
          (let ((selected (if flag #'worker (values #'worker :ignored))))
            (funcall selected '(17)) selected)))))
      (worker (funcall factory nil)))
    (and (handler-case (progn (funcall worker :invalid) nil)
           (type-error (condition) (eq (type-error-datum condition) :invalid)))
      (= 23 (funcall worker '(23)))))`);
  check(`(let* ((unit (compile-to-hir '(lambda (flag replacement x)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (values y y)))
          (declare (notinline worker))
          (let ((selected #'worker))
            (when flag (setq selected replacement))
            (funcall selected x))))))
      (function (first (hir-unit-functions unit)))
      (bindings (hir-function-bindings function))
      (selected (find 'selected bindings :key (lambda (binding) (hir-binding-name binding))))
      (worker (find 'worker bindings :key (lambda (binding) (hir-binding-name binding))))
      (call (find :call (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
                          (hir-function-blocks function))
                 :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (setf (hir-binding-local selected) (hir-binding-local worker))
    (verify-hir unit)
    (and (null (hir-result-callee (second (hir-instruction-inputs call))
                 (hir-result-callees unit (hir-optimization-barriers unit))))
      (multiple-value-bind (bytes literals) (emit-native-module unit)
        (let* ((native (compiler-primitive 17 bytes literals nil))
               (replacement (lambda (y) (declare (ignore y)) (values :replacement :second :third))))
          (and (equal (multiple-value-list (funcall native nil replacement 29)) '(29 29))
            (equal (multiple-value-list (funcall native t replacement 29)) '(:replacement :second :third)))))))`);
  // Capture identities describe a slot, not the executable cell supplied by
  // every closure creation. Re-emission must audit a changed context operand.
  check(`(let* ((unit (compile-to-hir '(lambda (stop replacement x)
        (declare (optimize (debug 0)))
        (labels ((worker (stop y) (if stop y (worker t y))))
          (declare (notinline worker))
          (worker stop x)))))
      (root (first (hir-unit-functions unit)))
      (replacement-binding (find 'replacement (hir-function-bindings root)
        :key (lambda (binding) (hir-binding-name binding))))
      (worker-binding (find 'worker (hir-function-bindings root)
        :key (lambda (binding) (hir-binding-name binding))))
      (worker (hir-local-function (hir-binding-local worker-binding)))
      (closure (find-if (lambda (instruction)
        (and (eq (hir-instruction-opcode instruction) :closure)
             (eq (first (hir-instruction-attributes instruction)) worker)))
        (mapcan (lambda (block) (copy-list (hir-block-instructions block))) (hir-function-blocks root))))
      (recursive (find :call
        (mapcan (lambda (block) (copy-list (hir-block-instructions block))) (hir-function-blocks worker))
        :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (emit-native-module unit)
    (setf (first (hir-instruction-inputs closure)) (hir-binding-cell replacement-binding)
          (hir-binding-captured-p replacement-binding) t (hir-binding-assigned-p replacement-binding) t)
    (verify-hir unit)
    (and (null (hir-result-callee (second (hir-instruction-inputs recursive))
                 (hir-result-callees unit (hir-optimization-barriers unit))))
      (multiple-value-bind (bytes literals) (emit-native-module unit)
        (let* ((native (compiler-primitive 17 bytes literals nil))
               (replacement (lambda (stop y) (declare (ignore stop)) (values :replacement y :extra))))
          (and (= 29 (funcall native t replacement 29))
            (equal (multiple-value-list (funcall native nil replacement 29)) '(:replacement 29 :extra)))))))`);
  // A valid client mutation after emission invalidates the earlier join proof.
  // Neither a retained certificate nor descriptive local metadata may seal it.
  check(`(let* ((unit (compile-to-hir '(lambda (flag replacement x)
        (declare (optimize (debug 0)))
        (flet ((worker (y) (values y y)))
          (declare (notinline worker))
          (funcall (if flag #'worker (values #'worker :branch)) x)))))
      (function (first (hir-unit-functions unit)))
      (call (find :call (mapcan (lambda (block) (copy-list (hir-block-instructions block)))
                      (hir-function-blocks function))
                    :key (lambda (instruction) (hir-instruction-opcode instruction))))
      (callee (second (hir-instruction-inputs call)))
      (join (find-if (lambda (block) (hir-block-parameters block)) (hir-function-blocks function)))
      (edge (hir-block-terminator (first (hir-block-predecessors join))))
      (group (hir-value-producer (first (hir-instruction-inputs edge))))
      (replacement-argument (find-if (lambda (value)
        (let ((producer (hir-value-producer value)))
          (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :argument)
               (= 1 (first (hir-instruction-attributes producer)))))) (hir-function-values function))))
    (and (hir-result-callee callee (hir-result-callees unit (hir-optimization-barriers unit)))
      (progn
        (emit-native-module unit)
        (setf (first (hir-instruction-inputs group)) replacement-argument)
        (verify-hir unit)
        (and (null (hir-result-callee callee (hir-result-callees unit (hir-optimization-barriers unit))))
          (let* ((native (compile-native-hir (emit-hir-code-checked unit)))
                 (replacement (lambda (y) (declare (ignore y)) (values :replacement :second :third)))
                 (answers (list (multiple-value-list (funcall native t replacement 29))
                                (multiple-value-list (funcall native nil replacement 29)))))
            (and (member '(29 29) answers :test #'equal)
                 (not (null (member '(:replacement :second :third) answers :test #'equal)))))))))`);
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

  // Open wide calls keep MIRICL's lookup-before-operands contract even when
  // their result is primary-only or discarded. A full consumer of the same
  // dynamically replaced callee still receives its complete value group.
  check(`(let* ((name 'soundness-wide-boundary)
      (original (compile-native '(lambda (group b c d e f fail)
        (declare (ignore b c d e f))
        (when fail (error "Wide boundary failure"))
        (values-list group))))
      (replacement (compile-native '(lambda (&rest arguments)
        (declare (ignore arguments)) (values :replacement nil :new))))
      (primary (compile-native '(lambda (effect fail)
        (let ((answer (soundness-wide-boundary (funcall effect) nil 3 4 5 6 fail)))
          (values answer :caller)))))
      (discard (compile-native '(lambda (effect fail)
        (soundness-wide-boundary (funcall effect) nil 3 4 5 6 fail)
        (values :discarded :caller))))
      (full (compile-native '(lambda (effect fail)
        (multiple-value-list
          (soundness-wide-boundary (funcall effect) nil 3 4 5 6 fail))))))
    (unwind-protect
      (and
        (loop for group in '(nil (nil) (:old nil :third 4 5 6 7 8 :last)) always
          (loop for caller in (list primary discard full)
                for expected in (list (list (car group) :caller)
                                      '(:discarded :caller) (list group)) always
            (let ((effects nil))
              (setf (symbol-function name) original)
              (and (equal (multiple-value-list
                  (funcall caller (lambda ()
                    (push :argument effects)
                    (setf (symbol-function name) replacement) group) nil)) expected)
                (equal effects '(:argument))
                (equal (multiple-value-list
                  (funcall primary (lambda () group) nil)) '(:replacement :caller))))))
        (let ((effects nil))
          (setf (symbol-function name) original)
          (and (handler-case
              (unwind-protect
                (progn (funcall primary (lambda ()
                  (push :argument effects)
                  (setf (symbol-function name) replacement) nil) t) nil)
                (push :cleanup effects))
              (error () (equal effects '(:cleanup :argument))))
            (equal (multiple-value-list
              (funcall primary (lambda () nil) nil)) '(:replacement :caller)))))
      (fmakunbound name)))`);

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
  // A false n-ary comparison can fail after its first pair. Relational
  // proofs must not turn that arm into the inverse of the first comparison.
  for (const [operator, arguments_, expected] of [
    ['<', '1 2 1', 't'], ['<=', '1 2 1', 't'],
    ['>', '2 1 2', 't'], ['>=', '2 1 2', 't'],
    ['=', '1 1 2', 't'], ['/=', '1 2 2', 't'],
  ]) check(`(let ((function (compile-native
      '(lambda (a b c)
         (declare (type (integer -10 10) a b c) (inline ${operator})
                  (optimize (debug 0) (safety 0)))
         (if (${operator} a b c) :all (${operator} a b))))))
    (eq (funcall function ${arguments_}) ${expected}))`);
  // Integer storage proofs must cover every incoming value. An assertion in
  // the arithmetic scope ends before a later, unrestricted assignment.
  check(`(let* ((function (compile-native
        '(lambda (n replacement)
           (declare (type (integer 0 8) n) (inline + <)
                    (optimize (debug 0) (safety 0)))
           (let ((x 1099511627776))
             (dotimes (i n)
               (setq x (+ (the (integer 0 4503599627370495) x) 1))
               (setq x replacement))
             x))))
      (token (list :retained)) (large (expt 2 90)))
    (and (= (funcall function 0 token) 1099511627776)
         (eq (funcall function 1 token) token)
         (eql (funcall function 1 1/2) 1/2)
         (eql (funcall function 1 0.5d0) 0.5d0)
         (eql (funcall function 1 large) large)
         (= (funcall function 2 42) 42)))`);
  // Earlier boxed observations remain snapshots when integer loop state is
  // stored without boxes. The same edges also carry the reference history.
  check(`(let* ((base 1099511627776)
      (function (compile-native
        '(lambda (n)
           (declare (type (integer 0 8) n) (inline + <)
                    (optimize (debug 0) (safety 0)))
           (let ((x 1099511627776) (y 1099511627786) (history nil))
             (declare (type (integer 0 4503599627370495) x y))
             (dotimes (i n)
               (let ((old x)) (setq x (+ y 1) y (+ old 2)))
               (push (list x y) history))
             (values x y (nreverse history)))))))
    (and (equal (multiple-value-list (funcall function 0))
                (list base (+ base 10) nil))
         (equal (multiple-value-list (funcall function 2))
                (list (+ base 3) (+ base 13)
                      (list (list (+ base 11) (+ base 2))
                            (list (+ base 3) (+ base 13)))))))`);
  // Widen a recurrence's storage without losing the finite range supplied by
  // its declared operand reads. Executable-only proofs still ignore promises.
  evaluate(`(defparameter *soundness-bounded-recurrence*
    '(lambda (count)
       (declare (type (integer 0 16) count) (inline + eql)
                (optimize (safety 0) (debug 0)))
       (let ((index 0))
         (declare (type (integer 0 16) index))
         (tagbody next (if (eql index count) (go done))
           (setq index (+ index 1)) (go next) done)
         (+ index 1)))) (values)`);
  check(`(let* ((unit (compile-to-hir *soundness-bounded-recurrence*))
                (facts (infer-hir-types unit)))
    (and (hir-type-proves-p (gethash (first (hir-unit-functions unit)) facts t)
                            '(integer 1 17))
      (not (hir-type-proves-p (soundness-proven-result *soundness-bounded-recurrence*)
                              '(signed-byte 64)))))`);
  check(`(let ((function (compile-native *soundness-bounded-recurrence*)))
    (loop for count from 0 to 16 always (= (funcall function count) (1+ count))))`);
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
  // Both size preferences preserve predicate operands, branch polarity and
  // value groups, including the debugger's retained-code configuration.
  for (const [space, speed] of [[3, 1], [1, 3]]) {
    for (const safety of [0, 3]) {
      for (const debug of [0, 2]) {
        check(`(let ((function (compile-native '(lambda (x y effect)
            (declare (inline consp eq)
              (optimize (space ${space}) (speed ${speed}) (safety ${safety}) (debug ${debug})))
            (values (consp (funcall effect :object x))
              (eq (funcall effect :left x) (funcall effect :right y))
              (if (consp x) :cons :atom) (if (eq x y) :same :different)))))
            (objects (list nil 17 (ash 1 80) :symbol #\\a #(1) '(1))))
          (every (lambda (object)
            (every (lambda (other)
              (let ((effects nil))
                (and (equal (multiple-value-list
                    (funcall function object other
                      (lambda (tag value) (push tag effects) (values value :discarded))))
                  (list (consp object) (eq object other)
                    (if (consp object) :cons :atom) (if (eq object other) :same :different)))
                  (equal effects '(:right :left :object)))))
              objects)) objects))`);
      }
    }
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

  // Field and bit facts must meet all paths and end at observations. ANSI
  // covers the individual operators; these exercise their optimized composition.
  for (const safety of [0, 3]) {
    check(`(let ((function (compile-native '(lambda (change)
        (declare (inline cons car cdr rplaca) (optimize (safety ${safety}) (debug 0)))
        (let ((object (cons :old :tail)))
          (when change (rplaca object :new))
          (values (car object) (cdr object)))))))
      (and (equal (multiple-value-list (funcall function nil)) '(:old :tail))
        (equal (multiple-value-list (funcall function t)) '(:new :tail))))`);
    check(`(let ((function (compile-native '(lambda (value)
        (declare (inline cons car rplaca eq) (optimize (safety ${safety}) (debug 0)))
        (let* ((object (cons :old nil)) (first (rplaca object :intermediate))
               (last (rplaca object value)))
          (values (eq first object) (eq last object) (car object) :extra))))))
      (equal (multiple-value-list (funcall function :final)) '(t t :final :extra)))`);
    check(`(let ((function (compile-native '(lambda ()
        (declare (inline cons car rplaca) (optimize (safety ${safety}) (debug 0)))
        (let ((object (cons :old nil)))
          (rplaca object :observed)
          (let ((observed (car object)))
            (rplaca object :final) (values observed (car object))))))))
      (equal (multiple-value-list (funcall function)) '(:observed :final)))`);
    check(`(let ((function (compile-native '(lambda (observe)
        (declare (inline cons car rplaca) (optimize (safety ${safety}) (debug 0)))
        (let ((object (cons :old nil)))
          (handler-case
            (progn (rplaca object :observed) (funcall observe object)
              (rplaca object :final))
            (error () (values (car object) :recovered))))))))
      (equal (multiple-value-list
        (funcall function (lambda (object)
          (unless (eq (car object) :observed) (throw 'soundness-wrong-store nil))
          (error "Observe the preceding store")))) '(:observed :recovered)))`);
    check(`(let ((function (compile-native '(lambda (value choose)
        (declare (type integer value) (inline logand logior evenp)
          (optimize (safety ${safety}) (debug 0)))
        (evenp (if choose (logand value -2) (logior value 1)))))))
      (and (funcall function 37 t) (not (funcall function 37 nil))))`);
    check(`(let ((function (compile-native '(lambda (input)
        (declare (type integer input) (inline logand logior evenp)
          (optimize (safety ${safety}) (debug 0)))
        (let ((value (logand input -2)))
          (funcall (lambda () (setq value (logior value 1))))
          (evenp value))))))
      (not (funcall function 37)))`);
    check(`(let ((effects nil)
        (function (compile-native '(lambda (effect)
          (declare (inline logand evenp) (optimize (safety ${safety}) (debug 0)))
          (values (evenp (logand (funcall effect) -2)) :extra)))))
      (and (equal (multiple-value-list
          (funcall function (lambda () (push :operand effects) (values 37 :discarded))))
        '(t :extra)) (equal effects '(:operand))))`);
  }
  check(`(let ((function (compile-native '(lambda (value)
      (declare (integer value) (inline evenp) (optimize (safety 3) (debug 0)))
      (evenp value)))))
    (and (handler-case (progn (funcall function 2.0) nil) (type-error () t))
      (funcall function 2) (not (funcall function 3))))`);
  // Run redefinition against the sealed compiler: its implementation can use
  // the primitive whose application binding this test temporarily replaces.
  check(`(let* ((source '(lambda (value)
        (declare (inline evenp logand) (optimize (safety 3) (debug 0)))
        (evenp (logand value -2))))
      (captured (compile-native source))
      (ordinary (compile-native '(lambda (value) (evenp value))))
      (replacement (compile-native '(lambda (value)
        (declare (ignore value)) (values :replacement :extra))))
      (original (symbol-function 'evenp)))
    (unwind-protect
      (progn (setf (symbol-function 'evenp) replacement)
        (and (funcall captured 37)
          (equal (multiple-value-list (funcall ordinary 37)) '(:replacement :extra))
          (equal (multiple-value-list (funcall (compile-native source) 37))
            '(:replacement :extra))))
      (setf (symbol-function 'evenp) original)))`);

  // ANSI leaves redefining CL primitives undefined. MIRICL explicitly supports
  // it unless an INLINE/sealed contract permits capturing the old definition.
  for (const [predicate, arguments_] of [['consp', 'object'], ['eq', 'object object']]) {
    check(`(let* ((object (list :identity))
        (ordinary (compile-native '(lambda (object) (${predicate} ${arguments_}))))
        (indirect (compile-native '(lambda (object) (declare (notinline ${predicate}))
          (${predicate} ${arguments_}))))
        (captured (compile-native '(lambda (object) (declare (inline ${predicate}))
          (${predicate} ${arguments_}))))
        (replacement (compile-native '(lambda (&rest arguments) (declare (ignore arguments))
          (values :replacement :extra))))
        (original (symbol-function '${predicate})))
      (and (equal (multiple-value-list (funcall ordinary object)) '(t))
        (unwind-protect
          (progn (setf (symbol-function '${predicate}) replacement)
            (and (equal (multiple-value-list (funcall ordinary object)) '(:replacement :extra))
              (equal (multiple-value-list (funcall indirect object)) '(:replacement :extra))
              (equal (multiple-value-list (funcall captured object)) '(t))
              (equal (multiple-value-list
                (funcall (compile-native '(lambda (object) (declare (inline ${predicate}))
                  (${predicate} ${arguments_}))) object)) '(:replacement :extra))))
          (setf (symbol-function '${predicate}) original))
        (equal (multiple-value-list (funcall ordinary object)) '(t))))`);
  }
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

  // Policy metadata is a retained-code capability: delayed definitions keep
  // each default initializer's original context after later proclamations.
  evaluate(`(defun soundness-policy-flags (unit)
  (labels ((source (value)
             (dotimes (fuel 16 value)
               (let ((producer (hir-value-producer value)))
                 (unless (and (%hir-instruction-p producer)
                              (member (hir-instruction-opcode producer) '(:primary :copy)))
                   (return value))
                 (setq value (first (hir-instruction-inputs producer))))))
           (constant (value)
             (let ((producer (hir-value-producer (source value))))
               (when (and (%hir-instruction-p producer)
                          (eq (hir-instruction-opcode producer) :constant))
                 (first (hir-instruction-attributes producer))))))
    (let ((flags nil))
      (dolist (function (hir-unit-functions unit))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (hir-block-instructions block))
            (when (eq (hir-instruction-opcode instruction) :call)
              (let* ((inputs (hir-instruction-inputs instruction))
                     (reference (hir-value-producer (second inputs))))
                (when (and (%hir-instruction-p reference)
                           (eq (hir-instruction-opcode reference) :function-reference)
                           (eq (first (hir-instruction-attributes reference)) 'consp))
                  (let ((probe (hir-value-producer (source (third inputs)))))
                    (when (and (%hir-instruction-p probe)
                               (eq (hir-instruction-opcode probe) :call))
                      (let ((tag (constant (third (hir-instruction-inputs probe)))))
                        (when (keywordp tag)
                          (let ((policy (hir-function-optimization-policy function)))
                            (push (cons tag (> (logand 3 (ash policy -2))
                                               (logand policy 3))) flags))))))))))))
      flags)))

(defun soundness-policy-probe (tag object) (declare (ignore tag)) object) (values)`);
  const savedSize = Number(evaluate('(values (compiler-primitive 52 nil))').trim());
  const savedDebug = Number(evaluate('(values (compiler-primitive 33 nil))').trim());
  if (!Number.isInteger(savedSize) || savedSize < 0 || savedSize > 63
      || !Number.isInteger(savedDebug) || savedDebug < 0 || savedDebug > 3)
    throw Error('Invalid initial optimization policy');
  try {
    for (const debug of [0, 3]) {
      vm.mountFile('/soundness-policy.lisp', `(in-package :cl-user)
        (declaim (optimize (debug 1) (space 3) (speed 1)))
        (defun soundness-policy-early (&optional (object
            (progn (consp (miricl-compiler::soundness-policy-probe :optional nil)) nil))
            &key ((:sample sample) (consp (miricl-compiler::soundness-policy-probe :key object)) supplied)
            &aux (aux (consp (miricl-compiler::soundness-policy-probe :aux object))))
          (declare (inline consp) (optimize (debug ${debug})))
          (values (consp (miricl-compiler::soundness-policy-probe :early object)) sample supplied aux))
        (declaim (optimize (space 1) (speed 3)))
        (defun soundness-policy-late (object)
          (declare (inline consp) (optimize (debug ${debug})))
          (consp (miricl-compiler::soundness-policy-probe :late object)))
        (declaim (optimize (space 0) (speed 0)))`);
      check(`(let ((result (multiple-value-list (compile-file "/soundness-policy.lisp"
          :verbose nil :block-compile t))))
        (and (pathnamep (first result)) (null (third result))
          (load "/soundness-policy.wasm")))`);
      check(`(let* ((recipe (compiler-primitive 44 #'cl-user::soundness-policy-early))
          (code (car recipe)) (flags (soundness-policy-flags (hir-code-to-unit code))))
        (and (every (lambda (tag) (let ((entry (assoc tag flags))) (and entry (cdr entry))))
               '(:optional :key :aux :early))
          (let ((entry (assoc :late flags))) (and entry (null (cdr entry))))
          (every (lambda (tag)
            (let* ((copy (copy-hir-code (validate-hir-code code)))
                   (entry (assoc tag (soundness-policy-flags (hir-code-to-unit copy)))))
              (and entry (cdr entry)))) '(:optional :key :aux :early))
          (equal (multiple-value-list (cl-user::soundness-policy-early)) '(nil nil nil nil))
          (equal (multiple-value-list (cl-user::soundness-policy-early '(x))) '(t t nil t))
          (equal (multiple-value-list (cl-user::soundness-policy-early '(x) :sample 99)) '(t 99 t t))
          (cl-user::soundness-policy-late '(x)) (not (cl-user::soundness-policy-late 17))))`);
    }
  } finally {
    evaluate(`(proclaim '(optimize (space ${(savedSize >> 2) & 3}) (speed ${savedSize & 3})
      (compilation-speed ${savedSize >> 4})
      (debug ${savedDebug}))) (values)`);
  }

// Ordinary COMPILE must inherit the current global preference even when the
// source function has a live lexical environment and retains no debug source.
check(`(let ((saved-policy (compiler-primitive 52 nil))
    (saved-debug (compiler-primitive 33 nil)))
  (labels ((retained-policy (function)
             (let* ((recipe (compiler-primitive 44 function))
                    (payload (validate-hir-code (car recipe)))
                    (functions (svref payload 1)) (index (cdr recipe)))
               ;; A retained factory may capture a live external cell through
               ;; its argument. Read its validated metadata without requiring
               ;; a closed lexical graph for another native compilation.
               (and (typep index '(unsigned-byte 31)) (< index (length functions))
                    (svref (svref functions index) 13)))))
    (unwind-protect
        (every (lambda (policy)
          (proclaim (list 'optimize (list 'space (first policy))
                                   (list 'speed (second policy)) '(debug 0)))
          (multiple-value-bind (raw captured)
              (eval '(let ((captured (list :captured)))
                       (values (lambda (object effect)
                                 (declare (inline consp) (optimize (debug 0)))
                                 (values captured (consp (funcall effect object))))
                               captured)))
            (and (not (compiled-function-p raw))
              (nth-value 1 (function-lambda-expression raw))
              (let* ((compiled (compile nil raw))
                     (preference (retained-policy compiled))
                     (expected (+ (* 4 (first policy)) (second policy)
                                  (* 16 (ash saved-policy -4))))
                     (effects nil))
                (setf (car captured) :changed)
                (and (= preference expected)
                  (let ((results (multiple-value-list
                          (funcall compiled '(x)
                            (lambda (object) (push :effect effects)
                              (values object :discarded))))))
                    (and (eq (first results) captured)
                      (equal results '((:changed) t))
                      (equal effects '(:effect)))))))))
          '((0 3) (3 1)))
      (proclaim (list 'optimize (list 'space (logand 3 (ash saved-policy -2)))
                               (list 'compilation-speed (ash saved-policy -4))
                               (list 'speed (logand saved-policy 3))
                               (list 'debug saved-debug))))))`);

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
  // The image loader feeds current binary record descriptions directly from
  // a byte-valued string; decoding must preserve records and the input string.
  check(`(let* ((text (map 'string #'code-char
                         (loop for code below 256 collect code)))
      (records (vector '(0) (list 3 text) '(5 1 0 0) '(5 1 2 0)
                       '(4 "KEYWORD" "COMMON-LISP" 0) '(11 0 0 (-1 1))
                       '(6 (1) 0 (1))))
      (octets (compiler-primitive 15 records 0))
      (packed (map 'string #'code-char octets))
      (saved (copy-seq packed)) (size (length packed)))
    (labels ((decode (source start end) (compiler-primitive 16 source start end))
             (reject (source start end)
               (and (handler-case (progn (decode source start end) nil)
                      (error (condition) (not (null (princ-to-string condition)))))
                    (equalp (decode packed 0 size) records))))
      (and (equalp (decode packed 0 size) records)
        ;; General strings use their actual view, including displacement.
        (equalp (decode (make-array size :element-type 'character
                         :displaced-to (concatenate 'string "pad" packed)
                         :displaced-index-offset 3) 0 size) records)
        (equalp (decode (concatenate 'string (string (code-char 256)) packed
                                    (string (code-char 256))) 1 (1+ size)) records)
        (reject packed 0 (1- size)) (reject packed -1 size)
        (reject packed 1 0) (reject packed 0 (1+ size))
        (reject "" 0 0) (reject packed size size)
        (reject (make-array 0 :element-type 'character :displaced-to packed) 0 0)
        ;; A nonbyte in an otherwise valid byte-string payload must not be
        ;; copied through as a wider character by the binary decoder.
        (let ((raw (map 'string #'code-char
                        (compiler-primitive 15 (string (code-char 255))))))
          (setf (char raw (1- (length raw))) (code-char 256))
          (reject raw 0 (length raw)))
        (equal packed saved) (= (length packed) size))))`);
  recover();
  // Compact arrays reconstruct the public record graph, including self and
  // mutual references. Other dimensions and reference orders stay valid.
  check(`(let* ((records #((6 (1) 0 (0)) (6 (2) 0 (0 1))
                           (6 (0) 0 ()) (6 (2) 0 (1 1)) (6 (1 1) 0 (0))))
                (bytes (compiler-primitive 15 records 0))
                (packed (map 'string #'code-char bytes)))
    (labels ((decode (text) (compiler-primitive 16 text 0 (length text)))
             (reject (octets)
               (and (handler-case (progn (decode (map 'string #'code-char octets)) nil)
                      (error (condition) (not (null (princ-to-string condition)))))
                    (equalp (decode packed) records))))
      (and (equalp (decode packed) records)
           (every #'reject
             '((10 0 1 14 0 0 0) (10 0 1 14 2 0 0)
               (10 2 1 14 1 0 1) (10 0 1 14 1 0 1)
               (10 0 1 14 1 1 0) (10 0 1 14 1 0)
               (10 0 2 14 2 0 0 14 1 0 0)
               (10 0 1 14 128 128 128 128 8 0 0))))))`);
  recover();
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
    ['missing required grammar feature', copy => {copy[payloadStart + 4] ^= 16;}],
    ['unknown grammar feature', copy => {copy[payloadStart + 4] |= 32;}],
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
