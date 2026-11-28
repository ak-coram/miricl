// Implementation contracts absent from ANSI: untrusted compiler boundaries,
// conservative optimization proofs, redefinition guards, and the FASL codec.
// node tests/soundness.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
import {checkCountedResults} from './soundness-counted-results.mjs';
import {checkPollImports} from './soundness-poll-imports.mjs';
import {checkScalarTransport} from './soundness-scalar-transport.mjs';

const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const vm = await Miricl.instantiate(image);
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);
const recover = () => assert.equal(evaluate("(funcall (compile-native '(lambda (x) (+ x 1))) 41)"), '42\n');
try {
  evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  await checkPollImports(image);
  await checkScalarTransport(image);
  // Lazy builtin condition classes can invoke Lisp during a cache lookup.
  // Isolate the fixture so earlier tests cannot materialize every candidate.
  {
    const dispatch = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
    try {
      await dispatch.mountFile('/soundness-discriminator-callback.lisp',
        await fs.readFile(new URL('./soundness-discriminator-callback.lisp', import.meta.url), 'utf8'));
      assert.equal(dispatch.evaluate('(load "/soundness-discriminator-callback.lisp")',
        {eof: true}).output, 'T\n');
    } finally {dispatch.close();}
  }
  console.log('Soundness: an in-flight lazy-class callback selects the live discriminator and retains all values.');
  // Shared generated functions retain checked descriptor boundaries and
  // captured state across an actual image restore.
  {
    let generated = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
    try {
      generated.evaluate(`(setq miricl-compiler:*compile-evaluations* nil)
        (let ((counter 0))
          (defstruct (soundness-shared-boa (:constructor make-soundness-shared-boa (value)))
            value (serial (incf counter))))
        (defparameter *soundness-shared-constructor* #'make-soundness-shared-boa)
        (values)`, {eof: true});
      assert.equal(generated.evaluate(`(let* ((class (find-class 'soundness-shared-boa))
          (maker (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%STRUCTURE-FUNCTION")))
          (allocate (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%ALLOCATE")))
          (slots (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%GET")) class :slots)))
        (labels ((recover () (= (soundness-shared-boa-value
                                (funcall *soundness-shared-constructor* 42)) 42))
                 (reject (owner plan)
                   (handler-case (progn (funcall (funcall maker (lambda () nil) owner plan 20) 1) nil)
                     (error () (recover)))))
          (and (reject class #())
            (reject class (vector 1 slots #(:value :serial) #(-1 nil)))
            (reject class (vector 1 slots #(:value) #(0)))
            (let* ((bad-slots (list 3))
                   (forged (funcall allocate (class-of class)
                     (list :slots bad-slots :struct-initargs '(:value)))))
              (reject forged (vector 1 bad-slots #(:value) #(0))))
            (recover))))`, {eof: true}).output, 'T\n');
      generated.evaluate(`(defgeneric soundness-shared-keyword (value &key marker))
        (defmethod soundness-shared-keyword ((value t) &key marker) (values value marker))
        (defmethod soundness-shared-keyword ((value integer) &key marker)
          (declare (ignore marker)) (lambda () (call-next-method)))
        (defparameter *soundness-shared-next* (soundness-shared-keyword 42 :marker :retained))
        (defparameter *soundness-shared-serial*
          (soundness-shared-boa-serial (funcall *soundness-shared-constructor* 1)))
        (values)`, {eof: true});
      const saved = generated.saveImage(); generated.close(); generated = await Miricl.instantiate(saved);
      assert.equal(generated.evaluate(`(and
        (= (soundness-shared-boa-serial (funcall *soundness-shared-constructor* 2))
           (1+ *soundness-shared-serial*))
        (equal (multiple-value-list (funcall *soundness-shared-next*)) '(42 :retained)))`,
      {eof: true}).output, 'T\n');
    } finally { generated?.close(); }
  }
  // This target specialization exceeds the host's FIXNUM range. Keep its
  // numeric values and shared displaced backing through an actual image save.
  {
    const numeric = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
    const numericCheck = source => assert.equal(numeric.evaluate(source, {eof: true}).output, 'T\n', source);
    try {
      numeric.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)', {eof: true});
      numericCheck(`(and
        (every (lambda (type) (eq (upgraded-array-element-type type) 'fixnum))
          '(fixnum (signed-byte 8) (signed-byte 64) (unsigned-byte 63)
            (member 0 2) (or bit (signed-byte 8)) (and fixnum (integer 2 5))))
        (eq (upgraded-array-element-type '(unsigned-byte 1)) 'bit)
        (eq (upgraded-array-element-type '(unsigned-byte 64)) t)
        (eq (upgraded-array-element-type '(and (member 0 :non-integer) integer)) 'bit)
        (eq (upgraded-array-element-type '(and (or bit character) integer)) 'bit)
        (eq (upgraded-array-element-type (cons 'or (make-list 9000 :initial-element 'fixnum))) 'fixnum)
        (eq (upgraded-array-element-type
              (cons 'or (make-list 9000 :initial-element '(signed-byte 8)))) 'fixnum)
        (every (lambda (type)
          (eq (upgraded-array-element-type (list 'or type type))
              (upgraded-array-element-type type)))
          '((signed-byte 64) (unsigned-byte 63) (unsigned-byte 64)
            (unsigned-byte 1) (integer 0 1) (integer (0) (3))))
        (eq (upgraded-array-element-type
              '(and (signed-byte 8) (integer 0 1))) 'bit)
        (eq (upgraded-array-element-type
              '(and (member 0 #\\a) integer)) 'bit)
        (eq (upgraded-array-element-type
              '(or character (unsigned-byte 1))) t)
        (eq (upgraded-array-element-type
              (list 'and (cons 'or (make-list 9000 :initial-element '(eql 2))) 'fixnum)) 'fixnum)
        (let ((array (make-array 1 :element-type '(signed-byte 8) :initial-element 127)))
          (and (eq (array-element-type array) 'fixnum)
            (= (setf (aref array 0) 1000) 1000)
            (= (aref array 0) 1000)
            (handler-case
              (progn (make-array 1 :element-type '(signed-byte 8) :initial-element 128) nil)
              (type-error () t)))))`);
      for (const safety of [0, 1, 3]) {
        numericCheck(`(let* ((values '(0 -1 -9223372036854775808 9223372036854775807 9007199254740993))
            (array (make-array (length values) :element-type 'fixnum :initial-contents values))
            (writer (miricl-compiler:compile-native '(lambda (array index value)
              (declare (type (simple-array fixnum (*)) array)
                (optimize (space 3) (speed 1) (debug 1) (safety ${safety})))
              (values (setf (aref array index) value) (aref array index))))))
          (and (typep array '(simple-array fixnum (*))) (not (simple-vector-p array))
            (loop for value in values for index from 0 always
              (equal (multiple-value-list (funcall writer array index value)) (list value value)))
            (handler-case (progn (funcall writer array 0 9223372036854775808) nil)
              (type-error () (= (aref array 0) 0)))
            (handler-case (progn (funcall writer array 0 1/2) nil)
              (type-error () (= (aref array 0) 0)))
            (equalp array (coerce values 'vector))))`);
      }
      numeric.evaluate(`(defparameter *soundness-fixnum-base*
        (make-array 3 :element-type 'fixnum :adjustable t
          :initial-contents '(0 -9223372036854775808 9223372036854775807)))
        (defparameter *soundness-fixnum-view*
          (make-array 2 :element-type '(signed-byte 64)
            :displaced-to *soundness-fixnum-base* :displaced-index-offset 1))
        (adjust-array *soundness-fixnum-base* 5 :initial-element 0) (values)`, {eof: true});
      const restored = await Miricl.instantiate(numeric.saveImage());
      try {
        assert.equal(restored.evaluate(`(and
          (eq (array-element-type *soundness-fixnum-base*) 'fixnum)
          (eq (array-displacement *soundness-fixnum-view*) *soundness-fixnum-base*)
          (= (aref *soundness-fixnum-view* 0) -9223372036854775808)
          (= (aref *soundness-fixnum-view* 1) 9223372036854775807)
          (progn (setf (aref *soundness-fixnum-base* 1) 9007199254740993)
            (= (aref *soundness-fixnum-view* 0) 9007199254740993))
          (progn (setf (aref *soundness-fixnum-view* 1) -9223372036854775808)
            (= (row-major-aref *soundness-fixnum-base* 2) -9223372036854775808)))`, {eof: true}).output, 'T\n');
      } finally { restored.close(); }
    } finally { numeric.close(); }
  }
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
  // MIRICL retains direct slot documentation; effective slots initially have
  // no merged documentation but support explicit assignments through the GF.
  evaluate(`(defclass soundness-slot-documentation ()
    ((documented :documentation "Retained slot documentation") (undocumented))) (values)`);
  check(`(let* ((class (find-class 'soundness-slot-documentation))
      (direct (miricl-mop:class-direct-slots class))
      (documented (find 'documented direct :key #'miricl-mop:slot-definition-name))
      (undocumented (find 'undocumented direct :key #'miricl-mop:slot-definition-name))
      (effective (find 'documented (miricl-mop:class-slots class)
                      :key #'miricl-mop:slot-definition-name))
      (reader (compile-native '(lambda (slot) (documentation slot t)))))
    (and (equal (funcall reader documented) "Retained slot documentation")
      (null (funcall reader undocumented)) (null (funcall reader effective))
      (unwind-protect
        (progn (setf (documentation effective t) "Assigned effective documentation")
          (equal (funcall reader effective) "Assigned effective documentation"))
        (setf (documentation effective t) nil))))`);
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
  // A native dynamic call with five operands exercises the general argument
  // transport. A late scalar miss must leave each operand for ordinary dispatch.
  check(`(let ((invoke (compile-native '(lambda (function a b c d e)
                   (declare (optimize (safety 1) (debug 0)))
                   (funcall function a b c d e))))
               (wide (ash 1 80)))
    (and (equal (multiple-value-list (funcall invoke #'logeqv 1 2 4 8 wide))
                (list (+ wide 15)))
         (handler-case (progn (funcall invoke #'logand 0 2 4 8 :last) nil)
           (type-error (condition)
             (and (eq (type-error-datum condition) :last)
                  (eq (type-error-expected-type condition) 'integer))))
         (equal (multiple-value-list (funcall invoke #'logeqv 1 2 4 8 16)) '(31))))`);
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
  // A warmed symbol designator must select its live binding at each fixed
  // call, including changes that leave the native fast path entirely.
  check(`(let* ((name (gensym "SOUNDNESS-FIXED-DESIGNATOR-"))
      (native (compile-native '(lambda (x)
        (declare (optimize (debug 1) (safety 1)))
        (case x (0 (values)) (1 (values nil)) (otherwise (values x nil :native))))))
      (full (compile-native '(lambda (function x effect)
        (declare (optimize (debug 1) (safety 1)))
        (multiple-value-list
          (funcall (progn (funcall effect :function) function)
            (progn (funcall effect :argument) (values x :ignored)))))))
      (primary (compile-native '(lambda (function x)
        (declare (optimize (debug 1) (safety 1)))
        (list (funcall function x) :done))))
      (discard (compile-native '(lambda (function x)
        (declare (optimize (debug 1) (safety 1)))
        (funcall function x) :done)))
      (events nil))
    (labels ((observe (event) (push event events) (values :noise :extra))
             (recover ()
               (setf (symbol-function name) native)
               (equal (funcall full name 2 #'observe) '(2 nil :native))))
      (unwind-protect
        (and (recover)
          (progn (dotimes (i 8) (funcall full name 2 #'observe)) t)
          (progn (setq events nil)
            (and (null (funcall full name 0 #'observe))
              (equal (reverse events) '(:function :argument))))
          (equal (funcall full name 1 #'observe) '(nil))
          (equal (funcall primary name 0) '(nil :done))
          (equal (funcall primary name 2) '(2 :done))
          (eq (funcall discard name 2) :done)
          (progn (setf (symbol-function name) (lambda (x) (values x :interpreted)))
            (equal (funcall full name 2 #'observe) '(2 :interpreted)))
          (progn (setf (symbol-function name) #'identity)
            (equal (funcall full name 2 #'observe) '(2)))
          (progn (setf (symbol-function name) (compile-native '(lambda () :wrong-arity)))
            (and (handler-case (progn (funcall full name 2 #'observe) nil)
                   (program-error (condition) (plusp (length (princ-to-string condition)))))
              (recover)))
          (progn (fmakunbound name)
            (and (handler-case (progn (funcall full name 2 #'observe) nil)
                   (undefined-function (condition) (eq (cell-error-name condition) name)))
              (recover)))
          (equal (funcall full native 2 #'observe) '(2 nil :native)))
        (fmakunbound name))))`);
  console.log('Soundness: warmed symbol calls preserve live binding changes, value demand, effects and rejection recovery.');
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
  // Public function objects also escape through ordinary user aliases.
  // Rebinding an alias must preserve the saved object and its arity checks.
  check(`(let* ((items (list :first :second))
      (object (make-instance 'soundness-mop-record))
      (one (compile-native '(lambda (function a) (funcall function a))))
      (two (compile-native '(lambda (function a b) (funcall function a b))))
      (replacement (compile-native '(lambda (&rest arguments)
        (declare (ignore arguments)) (values :replacement :extra)))))
    (every (lambda (entry)
      (destructuring-bind (public-name arguments expected) entry
        (let ((name (gensym "SOUNDNESS-ALIAS-")) (saved (fdefinition public-name)))
          (setf (fdefinition name) saved)
          (labels ((invoke ()
                     (if (cdr arguments)
                       (funcall two saved (car arguments) (cadr arguments))
                       (funcall one saved (car arguments))))
                   (recover ()
                     (and (equal (multiple-value-list (invoke)) (list expected))
                       (if (eq public-name 'copy-list) (not (eq (invoke) items)) t))))
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
              (prog1 (recover) (fmakunbound name)))))))
      (list (list 'nth (list 1 items) :second)
            (list 'nthcdr (list 1 items) (cdr items))
            (list 'list-length (list items) 2)
            (list 'copy-list (list items) items)
            (list 'slot-value (list object 'value) 42))))`);
  evaluate(`(defun soundness-alias-car (items) (car items))
    (defun soundness-alias-nthcdr (count items) (nthcdr count items))
    (setf (fdefinition 'soundness-alias-nth) (compile-native
      '(lambda (count items) (values (soundness-alias-car (soundness-alias-nthcdr count items))))))
    (values)`);
  check(`(let* ((saved (fdefinition 'soundness-alias-nth))
      (original-car (fdefinition 'soundness-alias-car))
      (original-tail (fdefinition 'soundness-alias-nthcdr))
      (driver (compile-native '(lambda (function count items)
        (let ((events nil))
          (values (multiple-value-list
                    (funcall (progn (push :function events) function)
                      (progn (push :count events) count)
                      (progn (push :items events) items)))
                  (reverse events))))))
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
          (progn (setf (fdefinition 'soundness-alias-car) car-callback
                       (fdefinition 'soundness-alias-nthcdr) tail-callback)
            (multiple-value-list (funcall driver saved 1 items)))
          (setf (fdefinition 'soundness-alias-car) original-car
                (fdefinition 'soundness-alias-nthcdr) original-tail)))))
      (items (list :first :second)))
    (multiple-value-bind (car-callback tail-callback read-events) (funcall factory)
      (let ((result (funcall runner driver saved items car-callback tail-callback
                             original-car original-tail)))
        (and (equal result '((:current-car) (:function :count :items)))
             (equal (funcall read-events)
                    (list (list :nthcdr 1 items) '(:car (:current-tail))))
             (equal (multiple-value-list (funcall saved 1 items)) '(:second))))))`);
  console.log('Soundness: saved function objects retain identity and arity checks; user callbacks preserve operand order and one value.');
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
  check(`(progn
    (defclass soundness-constructor-plan ()
      ((value :initarg :value) (alias :initarg :value)))
    (defparameter *soundness-constructor-hooks* 0)
    (let* ((class (find-class 'soundness-constructor-plan))
           (first (make-instance class :value 1))
           (second (make-instance 'soundness-constructor-plan :value 2)))
      (dotimes (i 12) (make-instance class :value i))
      (setf (slot-value first 'value) 7)
      (and (= (slot-value first 'alias) 1)
           (= (slot-value second 'value) 2)
           (= (slot-value second 'alias) 2)
           (progn
             (defmethod shared-initialize :after
                 ((object soundness-constructor-plan) slots &key value)
               (declare (ignore object slots value))
               (incf *soundness-constructor-hooks*))
             (let ((method (find-method #'shared-initialize '(:after)
                             (list class (find-class t)))))
               (unwind-protect
                   (and (= (slot-value (make-instance class :value 3) 'value) 3)
                        (= *soundness-constructor-hooks* 1))
                 (remove-method #'shared-initialize method))))
           (progn
             (dotimes (i 12) (make-instance class :value i))
             (defclass soundness-constructor-plan ()
               ((value :initarg :value) (added :initform 41)))
             (let ((object (make-instance class :value 9)))
               (and (= (slot-value object 'value) 9)
                    (= (slot-value object 'added) 41)))))))`);
  console.log('Soundness: warmed constructors preserve fresh slot storage, user initialization and class redefinition.');
  evaluate(`(defclass soundness-initarg-owned-union () ())
    (defmethod initialize-instance :after ((object soundness-initarg-owned-union) &key one two)
      (declare (ignore object one two))) (values)`);
  check(`(let* ((object (make-instance 'soundness-initarg-owned-union))
      (checker (symbol-function (compiler-primitive 10 1 "%%STANDARD-CHECK-INITARGS")))
      (name (compiler-primitive 10 1 "%CHECK-KEYS"))
      (original (symbol-function name)) (retained nil))
    (unwind-protect
        (progn
          (setf (symbol-function name)
            (lambda (arguments allowed allow)
              (funcall original arguments allowed allow)
              (push allowed retained)))
          (funcall checker object '(:one 1 :two 2) '(initialize-instance) t nil)
          ;; The ordinary final checker may retain its argument. Mutating it
          ;; after return must not alter method metadata or the next union.
          (when (car retained) (rplaca (car retained) :changed) (rplacd (car retained) nil))
          (funcall checker object '(:one 3 :two 4) '(initialize-instance) t nil)
          (let ((union (car retained))
                (keys (function-keywords (find-method #'initialize-instance '(:after)
                        (list (find-class 'soundness-initarg-owned-union))))))
            (and (member :one keys) (member :two keys)
              (member :one union) (member :two union) t)))
      (setf (symbol-function name) original)))`);
  // Custom MOP readers own these lists. Their mutable results are untrusted
  // boundary data, distinct from the standard readers' immutable metadata.
  evaluate(`(defclass soundness-initarg-reader () ())
    (defmethod initialize-instance :after ((object soundness-initarg-reader) &key original)
      (declare (ignore object original)))
    (defparameter *soundness-initarg-reader-target*
      (find-method #'initialize-instance '(:after) (list (find-class 'soundness-initarg-reader))))
    (defvar *soundness-initarg-reader-list* nil)
    (defvar *soundness-initarg-reader-calls* 0) (values)`);
  check(`(let* ((object (make-instance 'soundness-initarg-reader))
      (checker (symbol-function (compiler-primitive 10 1 "%%STANDARD-CHECK-INITARGS")))
      (pair (list :original 'value))
      (*soundness-initarg-reader-list* (list 'object '&key (list pair nil)))
      (*soundness-initarg-reader-calls* 0)
      (reader nil))
    (labels ((accept (arguments)
               (funcall checker object arguments '(initialize-instance) t nil) t)
             (reject (arguments)
               (handler-case (progn (accept arguments) nil)
                 (error (condition) (plusp (length (princ-to-string condition)))))))
      ;; Warm the standard metadata path before changing its reader protocol.
      (dotimes (i 8) (accept '(:original 1)))
      (unwind-protect
        (progn
          (setq reader (eval '(defmethod miricl-mop:method-lambda-list :around
              ((method (eql *soundness-initarg-reader-target*)))
            (incf *soundness-initarg-reader-calls*) *soundness-initarg-reader-list*)))
          (dotimes (i 8) (accept '(:original 1)))
          (and
            (let ((before *soundness-initarg-reader-calls*))
              (and (accept '(:original 2)) (> *soundness-initarg-reader-calls* before)))
            ;; Rename inside the same outer spine and nested key pair.
            (progn (setf (car pair) :renamed)
              (and (accept '(:renamed 3)) (reject '(:original 4))))
            (progn (setf (cdr (last *soundness-initarg-reader-list*))
                         (list '&allow-other-keys))
              (accept '(:unlisted 5)))
            (progn (setf (cdddr *soundness-initarg-reader-list*) nil)
              (reject '(:unlisted 5)))
            (every (lambda (bad)
              (let ((*soundness-initarg-reader-list* bad))
                (and (reject nil)
                  (let ((*soundness-initarg-reader-list* (list 'object '&key (list pair nil))))
                    (accept '(:renamed 6))))))
              (list #() "object"
                (cons 'object :tail)
                (let ((cycle (list 'object))) (setf (cdr cycle) cycle) cycle)
                (list 'object '&key (list (list :bad 'value 'extra) nil))
                (list 'object '&key (list (cons :bad :tail) nil))))))
        (when reader (remove-method #'miricl-mop:method-lambda-list reader)))
      (and (accept '(:original 7)) (reject '(:renamed 8))
        (let ((original (fdefinition 'miricl-mop:method-lambda-list)))
          (unwind-protect
            (progn
              (setf (fdefinition 'miricl-mop:method-lambda-list)
                (lambda (method)
                  (if (eq method *soundness-initarg-reader-target*)
                    *soundness-initarg-reader-list* (funcall original method))))
              (and (accept '(:renamed 9)) (reject '(:original 10))))
            (setf (fdefinition 'miricl-mop:method-lambda-list) original)))
        (accept '(:original 11)))))`);
  console.log('Soundness: warmed initarg checks observe mutable custom method metadata and recover after rejection.');
  evaluate(`(defclass soundness-initarg-slot-reader ()
      ((first :initarg :first) (second :initarg :second)))
    (defvar *soundness-initarg-slot-class* nil)
    (defvar *soundness-initarg-slot-target* nil)
    (defvar *soundness-initarg-slot-list* nil)
    (defvar *soundness-initarg-slot-keys* nil) (values)`);
  check(`(let* ((object (make-instance 'soundness-initarg-slot-reader))
      (checker (symbol-function (compiler-primitive 10 1 "%%STANDARD-CHECK-INITARGS")))
      (*soundness-initarg-slot-class* (class-of object))
      (slots (miricl-mop:class-slots *soundness-initarg-slot-class*))
      (*soundness-initarg-slot-target* (find 'first slots :key #'miricl-mop:slot-definition-name))
      (second (find 'second slots :key #'miricl-mop:slot-definition-name))
      (*soundness-initarg-slot-list* (list *soundness-initarg-slot-target*))
      (*soundness-initarg-slot-keys* (list :borrowed))
      (slot-reader nil) (key-reader nil))
    (labels ((accept (arguments) (funcall checker object arguments nil t nil) t)
             (reject (arguments)
               (handler-case (progn (accept arguments) nil)
                 (error (condition) (plusp (length (princ-to-string condition)))))))
      (dotimes (i 8) (accept '(:first 1)))
      (unwind-protect
        (progn
          (setq slot-reader (eval '(defmethod miricl-mop:class-slots :around
              ((class (eql *soundness-initarg-slot-class*))) *soundness-initarg-slot-list*)))
          (setq key-reader (eval '(defmethod miricl-mop:slot-definition-initargs :around
              ((slot (eql *soundness-initarg-slot-target*))) *soundness-initarg-slot-keys*)))
          (dotimes (i 8) (accept '(:borrowed 1)))
          (and (reject '(:first 1))
            (progn (setf (car *soundness-initarg-slot-keys*) :changed)
              (and (accept '(:changed 2)) (reject '(:borrowed 2))))
            (every (lambda (bad)
              (let ((*soundness-initarg-slot-keys* bad))
                (and (reject nil)
                  (let ((*soundness-initarg-slot-keys* (list :changed)))
                    (accept '(:changed 3))))))
              (list #() "keys" (list 17) (cons :bad :tail)
                (let ((cycle (list :bad))) (setf (cdr cycle) cycle) cycle)))
            (progn (setf (car *soundness-initarg-slot-list*) second)
              (and (accept '(:second 4)) (reject '(:changed 4))))))
        (when key-reader (remove-method #'miricl-mop:slot-definition-initargs key-reader))
        (when slot-reader (remove-method #'miricl-mop:class-slots slot-reader)))
      (and (accept '(:first 5 :second 6)) (reject '(:borrowed 7)))))`);
  console.log('Soundness: warmed initarg checks observe custom slot metadata changes and recover after rejection.');
  check(`(progn
    (defclass soundness-initarg-redefined () ((value :initarg :old)))
    (let ((driver (compile-native '(lambda (key value)
        (slot-value (make-instance 'soundness-initarg-redefined key value) 'value)))))
      (dotimes (i 8) (funcall driver :old i))
      (defclass soundness-initarg-redefined () ((value :initarg :new)))
      (and (= (funcall driver :new 42) 42)
        (handler-case (progn (funcall driver :old 43) nil)
          (error (condition) (plusp (length (princ-to-string condition)))))
        (= (funcall driver :new 44) 44))))`);
  console.log('Soundness: warmed constructors observe redefined class initargs and recover after rejection.');
  // An implicit method keyword has the same identity as its explicit form.
  // Its EQL specializer runs after file preparation has lowered the body.
  evaluate(`(defclass soundness-keyword-timing () ())
    (defparameter *soundness-keyword-timing-object* (make-instance 'soundness-keyword-timing))
    (defparameter *soundness-keyword-timing-old* (intern "SOUNDNESS-DEFINITION-KEY" "KEYWORD"))
    (defparameter *soundness-keyword-timing-new* nil)
    (defparameter *soundness-keyword-timing-events* nil) (values)`);
  vm.mountFile('/soundness-keyword-timing.lisp', `(in-package :miricl-compiler)
    (defmethod initialize-instance :after
        ((object (eql (progn
           (push :specializer *soundness-keyword-timing-events*)
           (unintern *soundness-keyword-timing-old* "KEYWORD")
           (setq *soundness-keyword-timing-new* (intern "SOUNDNESS-DEFINITION-KEY" "KEYWORD"))
           *soundness-keyword-timing-object*)))
         &key soundness-definition-key)
      (declare (ignore object))
      (push soundness-definition-key *soundness-keyword-timing-events*)
      soundness-definition-key)`);
  evaluate(`(compile-file "/soundness-keyword-timing.lisp" :verbose nil :block-compile nil)
    (compile-native-bundle "/soundness-keyword-timing.wasm" "/soundness-keyword-timing-native.wasm")
    (load "/soundness-keyword-timing-native.wasm") (values)`);
  check(`(let* ((object *soundness-keyword-timing-object*)
      (old *soundness-keyword-timing-old*) (new *soundness-keyword-timing-new*)
      (checker (symbol-function (compiler-primitive 10 1 "%%STANDARD-CHECK-INITARGS")))
      (method (find-method #'initialize-instance '(:after)
                (list (miricl-mop:intern-eql-specializer object)))))
    (labels ((accept () (funcall checker object (list old 42) '(initialize-instance) t nil) t)
             (reject ()
               (handler-case
                   (progn (funcall checker object (list new 43) '(initialize-instance) t nil) nil)
                 (error (condition) (plusp (length (princ-to-string condition)))))))
      (unwind-protect
        (and (not (eq old new))
          (equal *soundness-keyword-timing-events* '(:specializer))
          (progn (dotimes (i 8) (accept)) t) (reject) (accept)
          (equal *soundness-keyword-timing-events* '(:specializer))
          (= (funcall (miricl-mop:method-function method) (list object old 42) nil) 42)
          (equal *soundness-keyword-timing-events* '(42 :specializer)))
        (remove-method #'initialize-instance method)
        (unintern new "KEYWORD"))))`);
  console.log('Soundness: file/native method metadata retains the body keyword identity across specializer effects.');
  check(`(let* ((allocate (symbol-function (compiler-primitive 10 1 "%%ALLOCATE-METHOD")))
      (class (find-class 'standard-method))
      (original '(object &key safe))
      (canonical '(object &key ((:safe safe)))))
    (labels ((create (source target)
               (funcall allocate class (list :lambda-list source) target)))
      (and
        (every (lambda (bad)
          (handler-case (progn (create bad canonical) nil)
            (error (condition) (plusp (length (princ-to-string condition))))))
          (list (cons 'object :tail)
            (let ((cycle (list 'object))) (setf (cdr cycle) cycle) cycle)
            '(object &key ((:other different)))))
        (let* ((method (create original canonical))
               (keys (function-keywords method)))
          (setf (car keys) :borrowed)
          (and (equal (function-keywords method) '(:safe))
            (not (nth-value 1 (function-keywords method)))))
        (equal (function-keywords (create original canonical)) '(:safe)))))`);
  console.log('Soundness: owned method profiles reject malformed construction, recover and return independent keyword lists.');
  // A fixed standard name still runs permitted methods. Compile the caller
  // before installing and replacing a hook, and discard the constructor result.
  check(`(progn
    (defclass soundness-standard-constructor () ())
    (defparameter *soundness-standard-constructor-calls* 0)
    (let ((driver (compile-native '(lambda ()
             (declare (optimize (safety 3) (debug 0)))
             (make-instance 'soundness-standard-constructor) :done))))
      (and (eq (funcall driver) :done)
        (zerop *soundness-standard-constructor-calls*)
        (progn
          (defmethod initialize-instance :after
              ((object soundness-standard-constructor) &key)
            (incf *soundness-standard-constructor-calls*))
          (and (eq (funcall driver) :done)
            (= *soundness-standard-constructor-calls* 1)))
        (progn
          (defmethod initialize-instance :after
              ((object soundness-standard-constructor) &key)
            (incf *soundness-standard-constructor-calls* 10))
          (and (eq (funcall driver) :done)
            (= *soundness-standard-constructor-calls* 11))))))`);
  // A warmed generic must observe new applicability hooks and borrowed metadata.
  evaluate(`(defgeneric soundness-dispatch-protocol (x))
    (defvar *soundness-dispatch-protocol-calls* 0)
    (funcall (compile-native '(lambda ()
      (defmethod soundness-dispatch-protocol ((x integer))
        (declare (optimize (space 3) (speed 1) (debug 0) (safety 1))) x)))) (values)`);
  for (const body of [
    `(declare (optimize (space 3) (speed 1) (debug 0) (safety 1)))
      (soundness-dispatch-protocol x)`,
    `(declare (type integer x)
        (optimize (space 3) (speed 1) (debug 0) (safety 1)))
      (values (soundness-dispatch-protocol x) :extra)`,
  ]) check(`(let ((driver (compile-native '(lambda (x) ${body})))
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
  // Typed fixed calls select primary/discard transport independently of full
  // value demand. Warming must not freeze methods or an ordinary user binding.
  evaluate(`(defgeneric soundness-typed-transport (x))
    (funcall (compile-native '(lambda ()
      (defmethod soundness-typed-transport ((x integer))
        (when (minusp x) (error "Rejected transport input ~S" x))
        (case x (0 (values)) (1 (values nil))
          (otherwise (values x :secondary))))))) (values)`);
  check(`(let ((primary (compile-native '(lambda (x)
        (declare (type integer x) (optimize (safety 3) (debug 1)))
        (list (soundness-typed-transport x) :done))))
      (full (compile-native '(lambda (x)
        (declare (type integer x) (optimize (safety 3) (debug 1)))
        (multiple-value-list (soundness-typed-transport x)))))
      (discard (compile-native '(lambda (x)
        (declare (type integer x) (optimize (safety 3) (debug 1)))
        (soundness-typed-transport x) :done)))
      (generic #'soundness-typed-transport))
    (dotimes (i 8) (funcall primary i) (funcall full i) (funcall discard i))
    (and (equal (funcall primary 0) '(nil :done))
      (equal (funcall primary 1) '(nil :done))
      (equal (funcall primary 2) '(2 :done))
      (null (funcall full 0)) (equal (funcall full 1) '(nil))
      (equal (funcall full 2) '(2 :secondary))
      (equal (multiple-value-list (funcall discard 2)) '(:done))
      (handler-case (progn (funcall primary -1) nil)
        (error (condition) (plusp (length (princ-to-string condition)))))
      (equal (funcall primary 3) '(3 :done))
      (progn
        (funcall (compile-native '(lambda ()
          (defmethod soundness-typed-transport ((x integer))
            (declare (ignore x)) (values :new :secondary)))))
        (and (equal (funcall primary 2) '(:new :done))
          (equal (funcall full 2) '(:new :secondary))
          (eq (funcall discard 2) :done)))
      (unwind-protect
        (progn
          (setf (symbol-function 'soundness-typed-transport)
            (compile-native '(lambda (x) (values x :rebound))))
          (and (equal (funcall primary 4) '(4 :done))
            (equal (funcall full 4) '(4 :rebound))
            (eq (funcall discard 4) :done)))
        (setf (symbol-function 'soundness-typed-transport) generic))
      (equal (funcall primary 5) '(:new :done))))`);
  console.log('Soundness: typed generic transports preserve value demand, recovery and live method/function replacement.');
  // Reusing a checked owned spine must not bless borrowed or replaced metadata.
  evaluate(`(defgeneric soundness-metadata-shape (x))
    (defmethod soundness-metadata-shape ((x t)) x) (values)`);
  check(`(let* ((generic #'soundness-metadata-shape)
      (metadata (symbol-function (compiler-primitive 10 1 "%%GENERIC-DISPATCH-METADATA")))
      (put (symbol-function (compiler-primitive 10 1 "%%PUT")))
      (raw (symbol-function (compiler-primitive 10 1 "%%RAW-SLOTS")))
      (replace (symbol-function (compiler-primitive 10 1 "%%SET-RAW-SLOTS")))
      (info (funcall (symbol-function (compiler-primitive 10 1 "%%GENERIC-INFO")) generic))
      (profile (list nil)))
    (labels ((query () (funcall metadata 2 generic nil))
             (reject ()
               (handler-case (progn (query) nil)
                 (error (condition) (plusp (length (princ-to-string condition)))))))
      (funcall put generic :eql-dispatch-profile profile)
      (dotimes (i 8) (query))
      (and (eq (query) profile)
        (progn (funcall put generic :soundness-extra-property :extra)
          (funcall put generic :eql-dispatch-profile (setq profile (list :updated)))
          (eq (query) profile))
        (let* ((slots (funcall raw info)) (tail (last slots)))
          (and
            (unwind-protect
                (progn (rplacd tail :dotted) (reject))
              (rplacd tail nil))
            (eq (query) profile)
            (unwind-protect
                (progn (rplacd tail slots) (reject))
              (rplacd tail nil))
            (eq (query) profile)
            (unwind-protect
                (progn (rplacd tail (list :unpaired)) (reject))
              (rplacd tail nil))
            (eq (query) profile)
            (progn (setf (getf slots :eql-dispatch-profile) (setq profile (list :borrowed)))
              (eq (query) profile))
            (unwind-protect
                (progn (funcall replace info '(:unpaired)) (reject))
              (funcall replace info slots))
            (eq (query) profile))))))`);
  recover();
  console.log('Soundness: owned metadata validation observes value updates, borrowing, malformed spines and replacement recovery.');
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
  const workLimit = vm.vm['native-work-limit'];
  const workComplete = vm.vm['native-work-complete'];
  for (const invalid of [0, -1, 0x7fffffff]) {
    rejectPacketOperation(() => workLimit(invalid));
    rejectPacketOperation(() => workComplete(invalid));
  }
  assert.equal(workLimit(1), 1);
  workComplete(1);
  console.log('Soundness: scheduler work imports reject invalid counts and recover.');
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
  await checkCountedResults(process.argv[2] ?? 'miricl.wasm');

  // Canonical calls and shared list paths are public native boundaries too.
  // Invalid identities, foreign objects and unchecked path codes must signal
  // useful Lisp conditions while leaving subsequent compilation usable.
  const canonicalCode = name => {
    const code = Number(evaluate(`(native-canonical-builtin-code '${name})`).trim());
    assert.ok(Number.isInteger(code) && code > 0, `${name} has a canonical standard identity.`);
    return code;
  };
  const identityCode = canonicalCode('identity');
  const pair = vm.vm['native-cons'](elements[0], null);
  assert.equal(vm.vm['native-list-path'](pair, 2), elements[0]);
  assert.equal(vm.vm['native-standard-call1'](identityCode, pair), pair);
  assert.equal(vm.vm['native-set-list-path'](null, pair, 2), null);
  assert.equal(vm.vm['native-list-path'](pair, 2), null);
  for (const operation of [
    () => vm.vm['native-standard-function-reference'](foreign),
    () => vm.vm['native-standard-call0'](0),
    () => vm.vm['native-standard-call1'](-0x80000000, null),
    () => vm.vm['native-standard-call1'](identityCode, foreign),
    () => vm.vm['native-standard-call2'](identityCode, null, foreign),
    () => vm.vm['native-standard-call3'](identityCode, null, null, foreign),
    () => vm.vm['native-standard-call4'](identityCode, null, null, null, foreign),
    () => vm.vm['native-list-path'](foreign, 2),
    () => vm.vm['native-list-path'](pair, 0),
    () => vm.vm['native-list-path'](pair, 1),
    () => vm.vm['native-list-path'](pair, -0x80000000),
    () => vm.vm['native-set-list-path'](foreign, pair, 2),
    () => vm.vm['native-set-list-path'](null, null, 2),
  ]) rejectPacketOperation(operation);
  const integerOne = vm.vm['native-int64'](1n);
  for (const name of ['1+', '1-']) {
    const code = canonicalCode(name);
    for (const invalid of [foreign, packets.empty()])
      rejectPacketOperation(() => vm.vm['native-standard-call1'](code, invalid));
  }
  for (const name of ['+', '-', '*']) {
    const code = canonicalCode(name);
    for (const invalid of [foreign, packets.empty()]) {
      rejectPacketOperation(() => vm.vm['native-standard-call2'](code, invalid, integerOne));
      rejectPacketOperation(() => vm.vm['native-standard-call2'](code, integerOne, invalid));
    }
  }
  // A canonical cons can carry a private GC array in either bytespec slot.
  const callStandard2 = vm.vm['native-standard-call2'];
  const byteSize = vm.vm['native-int64'](3n), byteInput = vm.vm['native-int64'](14n);
  const byteSpec = callStandard2(canonicalCode('byte'), byteSize, integerOne);
  const ldbCode = canonicalCode('ldb'), dpbCode = canonicalCode('dpb');
  const callStandard3 = vm.vm['native-standard-call3'], privateArray = packets.empty();
  for (const [size, position] of [[privateArray, integerOne], [byteSize, privateArray]]) {
    const malformed = vm.vm['native-cons'](size, position);
    rejectPacketOperation(() => callStandard2(ldbCode, malformed, byteInput));
    assert.equal(vm.javascript.integer(callStandard2(ldbCode, byteSpec, byteInput)), 7n);
    rejectPacketOperation(() => callStandard3(dpbCode, integerOne, malformed, byteInput));
    assert.equal(vm.javascript.integer(callStandard3(dpbCode, integerOne, byteSpec, byteInput)), 2n);
  }
  console.log('Soundness: canonical standard calls and list paths reject malformed ingress with recovery.');

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
  // The fixed bitwise paths must reject malformed values in every raw slot.
  const logeqv = packetFunction("#'logeqv"), logeqvCode = canonicalCode('logeqv');
  const scalarBitInputs = [vm.vm['native-int64'](-(1n << 63n)),
    vm.vm['native-int64']((1n << 63n) - 1n), integerOne];
  for (const [arity, expected] of [[2, 0n], [3, -2n]]) {
    const inputs = scalarBitInputs.slice(0, arity);
    for (const [entry, callee] of [[`native-call${arity}`, logeqv],
      [`native-standard-call${arity}`, logeqvCode]]) {
      const call = vm.vm[entry];
      for (let index = 0; index < inputs.length; index++) {
        for (const invalid of [foreign, privateArray]) {
          const malformed = inputs.map((value, slot) => slot === index ? invalid : value);
          rejectPacketOperation(() => call(callee, ...malformed));
        }
      }
      assert.equal(vm.javascript.integer(call(callee, ...inputs)), expected);
    }
  }
  // Fixed array adapters use live dimensions and retain checked storage access.
  const rank1Read = packetFunction("#'aref");
  const rank1ReadRaw = vm.vm['native-aref-int64'];
  const rank1ReadI32 = vm.vm['native-aref-i31-index'];
  const rank1Write = packetFunction("(fdefinition '(setf aref))");
  const rank1Svref = packetFunction("#'svref");
  const rank1Svset = packetFunction("(fdefinition '(setf svref))");
  const rank1Eql = packetFunction("#'eql");
  const rank1Value = source => vm.vm['native-call0'](packetFunction(`(lambda () ${source})`));
  const rank1Index = rank1Value('1');
  const rank1BoxedIndex = vm.vm['native-int64'](1n);
  const rank1Equal = (actual, expected) => assert.notEqual(vm.vm['native-call2'](rank1Eql, actual, expected), null);
  const rank1Fixtures = [
    ["(make-array 3 :element-type 'fixnum :adjustable t :fill-pointer 0 :initial-contents '(0 -9223372036854775808 2))", '-9223372036854775808', '9223372036854775807'],
    ["(make-array 3 :adjustable t :fill-pointer 0 :initial-contents '(a b c))", "'b", "'(new value)"],
    ["(make-array 3 :element-type 'character :adjustable t :fill-pointer 0 :initial-contents \"abc\")", '#\\b', '#\\z'],
    ["(make-array 3 :element-type 'bit :adjustable t :fill-pointer 0 :initial-contents '(0 1 0))", '1', '0'],
    ['(copy-seq "abc")', '#\\b', '#\\z'],
    ["(make-string 3 :initial-element (code-char 955))", '(code-char 955)', '(code-char 956)'],
    ['(copy-seq #*010)', '1', '0'],
  ];
  let rank1Numeric;
  for (const [source, expectedSource, replacementSource] of rank1Fixtures) {
    const array = rank1Value(source), expected = rank1Value(expectedSource), replacement = rank1Value(replacementSource);
    rank1Equal(vm.vm['native-call2'](rank1Read, array, rank1Index), expected);
    rank1Equal(vm.vm['native-call2'](rank1Read, array, rank1BoxedIndex), expected);
    rank1Equal(rank1ReadRaw(array, 1n), expected);
    rank1Equal(rank1ReadI32(array, 1), expected);
    assert.equal(vm.vm['native-call3'](rank1Write, replacement, array, rank1Index), replacement);
    rank1Equal(vm.vm['native-call2'](rank1Read, array, rank1BoxedIndex), replacement);
    if (!rank1Numeric) rank1Numeric = array;
  }
  evaluate(`(defparameter *soundness-rank1-base*
    (make-array 4 :element-type 'fixnum :adjustable t :initial-contents '(10 11 12 13)))
    (defparameter *soundness-rank1-view*
      (make-array 2 :element-type 'fixnum :displaced-to *soundness-rank1-base* :displaced-index-offset 1))
    (values)`);
  const rank1Base = rank1Value('*soundness-rank1-base*'), rank1View = rank1Value('*soundness-rank1-view*');
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1View, rank1Index), rank1Value('12'));
  rank1Equal(rank1ReadRaw(rank1View, 1n), rank1Value('12'));
  rank1Equal(rank1ReadI32(rank1View, 1), rank1Value('12'));
  evaluate("(adjust-array *soundness-rank1-base* 6 :initial-contents '(100 101 102 103 104 105)) (values)");
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1View, rank1Index), rank1Value('102'));
  rank1Equal(rank1ReadRaw(rank1View, 1n), rank1Value('102'));
  rank1Equal(rank1ReadI32(rank1View, 1), rank1Value('102'));
  const rank1MatrixView = rank1Value("(let ((base (make-array '(2 2) :element-type 'fixnum :initial-contents '((1 2) (3 4))))) (make-array 4 :element-type 'fixnum :displaced-to base))");
  rank1Equal(rank1ReadRaw(rank1MatrixView, 2n), rank1Value('3'));
  const rank1ObjectMatrixView = rank1Value("(let ((base (make-array '(2 2) :initial-contents '((a b) (c d))))) (make-array 2 :displaced-to base :displaced-index-offset 1))");
  rank1Equal(rank1ReadRaw(rank1ObjectMatrixView, 1n), rank1Value("'c"));
  rank1Equal(rank1ReadI32(rank1ObjectMatrixView, 1), rank1Value("'c"));
  const rank1New = rank1Value('9007199254740993');
  assert.equal(vm.vm['native-call3'](rank1Write, rank1New, rank1View, rank1Index), rank1New);
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1Base, rank1Value('2')), rank1New);
  const rank1Empty = rank1Value("(make-array 0 :element-type 'fixnum)");
  const rank1Matrix = rank1Value("(make-array '(1 1) :element-type 'fixnum :initial-element 7)");
  for (const [array, index] of [
    [rank1Numeric, rank1Value('-1')], [rank1Numeric, rank1Value('3')],
    [rank1Numeric, rank1Value('1208925819614629174706176')], [rank1Numeric, rank1Value('1d0')],
    [rank1Empty, rank1Value('0')], [rank1Matrix, rank1Value('0')],
  ]) {
    rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, array, index));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, rank1New, array, index));
  }
  for (const value of [rank1Value('9223372036854775808'), rank1Value('1/2')])
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, value, rank1Numeric, rank1Index));
  for (const index of [-1n, 3n, 4294967297n, -(1n << 63n), (1n << 63n) - 1n])
    rejectPacketOperation(() => rank1ReadRaw(rank1Numeric, index));
  rejectPacketOperation(() => rank1ReadRaw(rank1Empty, 0n));
  rejectPacketOperation(() => rank1ReadRaw(rank1Matrix, 0n));
  for (const index of [-0x80000000, -1, 3, 0x40000000, 0x7fffffff])
    rejectPacketOperation(() => rank1ReadI32(rank1Numeric, index));
  rejectPacketOperation(() => rank1ReadI32(rank1Empty, 0));
  rejectPacketOperation(() => rank1ReadI32(rank1Matrix, 0));
  // Construct malformed flat storage through this image's actual core layout.
  // Immutable fields go to CORE-NEW; mutable fields go to CORE-SET.
  const rank1Clone = (object, change) => {
    const type = vm.vm['core-object-type'](object);
    const reader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${type}:`));
    assert.ok(reader, 'The fixture has a physical struct reader.');
    const signature = reader.split(':')[2], fields = vm.vm[reader](object);
    change(fields);
    const mutable = index => signature[index] === signature[index].toUpperCase();
    const copy = vm.vm[`core-new:${type}`](...fields.filter((_, i) => !mutable(i)));
    vm.vm[`core-set:${type}`](copy, ...fields.filter((_, i) => mutable(i)));
    return copy;
  };
  const rank1FlatString = rank1Value('(copy-seq "abc")'), rank1FlatBits = rank1Value('(copy-seq #*010)');
  const rank1BrokenString = rank1Clone(rank1FlatString, fields => {fields[3] = null;});
  const rank1ShortBits = rank1Clone(rank1FlatBits, fields => {fields[1] = 33;});
  const rank1BrokenBits = rank1Clone(rank1FlatBits, fields => {fields[3] = null;});
  for (const [array, index, value] of [
    [rank1BrokenString, rank1Index, rank1Value('#\\z')],
    [rank1BrokenString, rank1Value('-1'), rank1Value('#\\z')],
    [rank1ShortBits, rank1Value('32'), rank1Value('1')],
    [rank1BrokenBits, rank1Index, rank1Value('1')],
    [rank1Value('""'), rank1Value('0'), rank1Value('#\\a')],
    [rank1Value('#*'), rank1Value('0'), rank1Value('1')],
  ]) {
    rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, array, index));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, value, array, index));
  }
  for (const [array, invalidValue, expected] of [
    [rank1FlatString, rank1Value('1'), rank1Value('#\\b')],
    [rank1FlatBits, rank1Value('2'), rank1Value('1')],
  ]) {
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, invalidValue, array, rank1Index));
    rank1Equal(vm.vm['native-call2'](rank1Read, array, rank1Index), expected);
    rejectPacketOperation(() => vm.vm['native-call2'](rank1Svref, array, rank1Index));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Svset, invalidValue, array, rank1Index));
    for (const index of [rank1Value('-1'), rank1Value('3'), vm.vm['native-int64'](4294967297n), rank1Value('1d0')]) {
      rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, array, index));
      rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, expected, array, index));
    }
  }
  rejectPacketOperation(() => vm.vm['native-call2'](rank1Svref, rank1Numeric, rank1Index));
  rejectPacketOperation(() => vm.vm['native-call3'](rank1Svset, rank1New, rank1Numeric, rank1Index));
  const rank1Type = vm.vm['core-object-type'](rank1Numeric);
  const rank1Reader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${rank1Type}:`));
  const rank1Private = vm.vm[rank1Reader](rank1Numeric).find(value => {
    if (value === null || typeof value !== 'object') return false;
    const type = vm.vm['core-object-type'](value);
    return Object.keys(vm.vm).some(name => name.startsWith(`core-array:${type}:l`));
  });
  assert.ok(rank1Private, 'The fixture includes the actual private i64 array payload.');
  const rank1BackingSlot = vm.vm[rank1Reader](rank1Numeric).indexOf(rank1Private);
  const rank1ShortBacking = vm.vm[rank1Reader](rank1Value("(make-array 1 :element-type 'fixnum)"))[rank1BackingSlot];
  const rank1BrokenNumeric = rank1Clone(rank1Numeric, fields => {fields[rank1BackingSlot] = rank1ShortBacking;});
  const rank1WrongNumericBacking = rank1Clone(rank1Numeric, fields => {fields[rank1BackingSlot] = rank1Base;});
  const rank1FalseHeaders = [34, 35].map(tag => rank1Clone(rank1FlatString, fields => {
    fields[0] = (fields[0] & ~63) | tag;
  }));
  // A zero-offset view exposes index zero of malformed backing unless the
  // resolver checks the target's whole layout, even when this index fits.
  const rank1BadTargets = [rank1BrokenNumeric, rank1WrongNumericBacking].map(target =>
    rank1Clone(rank1MatrixView, fields => {fields[rank1BackingSlot] = target;}));
  // Each replacement has a valid first slot but cannot hold the declared extent.
  for (const kind of ['t', 'character', 'bit']) {
    const base = rank1Value(`(make-array 48 :element-type '${kind} :adjustable t)`);
    const short = rank1Value(`(make-array 1 :element-type '${kind})`);
    const malformed = rank1Clone(base, fields => {fields[rank1BackingSlot] = short;});
    const view = rank1Value(`(make-array 1 :element-type '${kind}
      :displaced-to (make-array 48 :element-type '${kind} :adjustable t))`);
    rank1BadTargets.push(malformed, rank1Clone(view, fields => {fields[rank1BackingSlot] = malformed;}));
  }
  const rank1Cycle = rank1Clone(rank1MatrixView, () => {});
  const rank1CycleFields = vm.vm[rank1Reader](rank1Cycle);
  rank1CycleFields[rank1BackingSlot] = rank1Cycle;
  const rank1Signature = rank1Reader.split(':')[2];
  vm.vm[`core-set:${rank1Type}`](rank1Cycle, ...rank1CycleFields.filter((_, i) =>
    rank1Signature[i] === rank1Signature[i].toUpperCase()));
  rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, rank1BrokenNumeric, rank1Index));
  rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, rank1New, rank1BrokenNumeric, rank1Index));
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1Numeric, rank1Index), rank1Value('9223372036854775807'));
  for (const invalid of [foreign, packets.empty(), rank1Private]) {
    rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, invalid, rank1Index));
    rejectPacketOperation(() => vm.vm['native-call2'](rank1Read, rank1Numeric, invalid));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, invalid, rank1Numeric, rank1Index));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, rank1New, invalid, rank1Index));
    rejectPacketOperation(() => vm.vm['native-call3'](rank1Write, rank1New, rank1Numeric, invalid));
  }
  const rank1Updated = rank1Value('9223372036854775807');
  for (const invalid of [foreign, packets.empty(), rank1Private, pair, ...rank1FalseHeaders,
    rank1BrokenString, rank1BrokenBits, rank1ShortBits, rank1BrokenNumeric, rank1WrongNumericBacking,
    ...rank1BadTargets, rank1Cycle]) {
    rejectPacketOperation(() => rank1ReadRaw(invalid, 0n));
    rank1Equal(rank1ReadRaw(rank1Numeric, 1n), rank1Updated);
    rejectPacketOperation(() => rank1ReadI32(invalid, 0));
    rank1Equal(rank1ReadI32(rank1Numeric, 1), rank1Updated);
  }
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1Numeric, rank1Index), rank1Value('9223372036854775807'));
  console.log('Soundness: fixed rank-one array access preserves live dimensions, backing, element checks and rejection recovery.');
  const rank1Snapshot = vm.vm['native-aref-snapshot'];
  const rank1WideSnapshot = vm.vm['native-aref-snapshot64'];
  const integerFlow = vm.vm['native-integer-flow'];
  const integerFlowBox = vm.vm['native-integer-flow-box'];
  for (const invalid of [foreign, rank1Private, packets.empty()]) {
    rejectPacketOperation(() => integerFlow(0, 0n, 0, invalid, 1n, 1, null));
    rejectPacketOperation(() => integerFlow(0, 0n, 1, invalid, 1n, 1, null));
    rejectPacketOperation(() => integerFlowBox(0n, 0, invalid));
  }
  rejectPacketOperation(() => integerFlow(2, 0n, 1, null, 0n, 1, null));
  rejectPacketOperation(() => integerFlow(0, 0n, 1, null, 0n, 3, null));
  rejectPacketOperation(() => integerFlowBox(0n, -1, null));
  rank1Equal(integerFlowBox(...integerFlow(0, 1073741823n, 1, null, 1n, 1, null)),
    rank1Value('1073741824'));
  rank1Equal(integerFlowBox(...integerFlow(1, 1n, 1, null, 0n, 0, rank1Value('1/2'))),
    rank1Value('1/2'));
  const rank1WideStore = vm.vm['native-set-aref-int64'];
  const boxedNil = rank1Value('(vector nil)');
  const immediateZero = rank1Value('0');
  assert.deepEqual(rank1WideSnapshot(boxedNil, 0n), [0n, 0, null]);
  rank1WideStore(rank1Write, -(1n << 63n), rank1Numeric, rank1Index);
  for (const invalid of [foreign, rank1Private, ...rank1BadTargets, rank1Cycle]) {
    rejectPacketOperation(() => rank1WideSnapshot(invalid, 0n));
    rejectPacketOperation(() => rank1WideStore(rank1Write, 0n, invalid, immediateZero));
    assert.deepEqual(rank1Snapshot(rank1Numeric, rank1Index), [-(1n << 63n), 1, null]);
  }
  rejectPacketOperation(() => rank1Snapshot(rank1Numeric, rank1Private));
  rejectPacketOperation(() => rank1WideStore(rank1Write, 0n, rank1Numeric, rank1Private));
  rejectPacketOperation(() => rank1WideSnapshot(rank1Numeric, 4294967297n));
  const forgedBoxedIndex = rank1Clone(rank1BoxedIndex, fields => {fields[1] = 0;});
  const rawCharacterStore = vm.vm['native-character-set-code-inline'];
  rejectPacketOperation(() => rank1Snapshot(rank1FlatBits, forgedBoxedIndex));
  rejectPacketOperation(() => rank1WideStore(rank1Write, 0n, rank1FlatBits, forgedBoxedIndex));
  rejectPacketOperation(() => rank1Snapshot(rank1ShortBits, rank1BoxedIndex));
  rejectPacketOperation(() => rank1WideStore(rank1Write, 0n, rank1ShortBits, rank1BoxedIndex));
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1FlatBits, rank1Index), rank1Value('1'));
  rank1WideStore(rank1Write, 0n, rank1FlatBits, rank1BoxedIndex);
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1FlatBits, rank1Index), rank1Value('0'));
  rejectPacketOperation(() => rawCharacterStore(122, rank1FlatString, forgedBoxedIndex));
  rejectPacketOperation(() => rawCharacterStore(0xd800, rank1FlatString, rank1BoxedIndex));
  rejectPacketOperation(() => rawCharacterStore(122, rank1Private, rank1BoxedIndex));
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1FlatString, rank1Index), rank1Value('#\\b'));
  rawCharacterStore(122, rank1FlatString, rank1BoxedIndex);
  rank1Equal(vm.vm['native-call2'](rank1Read, rank1FlatString, rank1Index), rank1Value('#\\z'));
  evaluate('(defparameter *scalar-store-arguments* nil) (values)');
  const replacementSetter = packetFunction(`(lambda (value array index)
    (setq *scalar-store-arguments* (list value array index))
    (values :ignored :secondary))`);
  // A replacement can accept arguments that canonical AREF would reject.
  rank1WideStore(replacementSetter, 17n, rank1Value(':array'), rank1Value(':index'));
  check("(equal *scalar-store-arguments* '(17 :array :index))");
  assert.deepEqual(rank1WideSnapshot(rank1Numeric, 1n), [-(1n << 63n), 1, null]);
  console.log('Soundness: scalar snapshots distinguish NIL and i64 values and retain checked store dispatch.');
  const floatPair = vm.vm['native-float-pair'];
  const singleOne = rank1Value('1.0f0');
  const singleHalf = rank1Value('0.5f0');
  assert.deepEqual(floatPair(0, 3, 0, 0n, singleOne, singleHalf), [0x3ff8000000000000n, 1, null]);
  for (const invalid of [foreign, packets.empty(), rank1Private]) {
    rejectPacketOperation(() => floatPair(0, 3, 0, 0n, invalid, singleHalf));
    rejectPacketOperation(() => floatPair(1, 3, 1, 0x3ff8000000000000n, null, invalid));
  }
  rejectPacketOperation(() => floatPair(0, 4, 1, 0x7ff0000000000000n, null, singleOne));
  rejectPacketOperation(() => floatPair(0, 1, 0, 0n, singleOne, singleHalf));
  rank1Equal(floatPair(1, 3, 1, 0x3ff8000000000000n, null, singleHalf)[2], singleOne);
  check(`(let ((worker (compile-native '(lambda (x next)
                 (declare (optimize (debug 1) (space 2) (speed 1)))
                 (values (- (+ x 1.0f0) (funcall next))))))
               (calls 0))
           (and (equal (multiple-value-list
                         (funcall worker 16777216.0f0
                           (lambda () (incf calls) 16777216.0f0))) '(0.0f0))
                (equal (multiple-value-list
                         (funcall worker 6.0f0 (lambda () (incf calls) 2.0d0))) '(5.0d0))
                (handler-case (progn (funcall worker :bad (lambda () (incf calls))) nil)
                  (type-error (condition) (eq (type-error-datum condition) :bad)))
                (= calls 2)))`);
  const scalarReader = rank1Value(`(compile-native '(lambda (array index)
    (declare (optimize (debug 1) (space 2) (speed 1)))
    (logand (aref array index) 255)))`);
  const scalarInput = rank1Value('(make-array 2 :initial-contents (list 512 753))');
  const scalarCall = vm.vm['native-scalar-call2'];
  assert.deepEqual(scalarCall(scalarReader, scalarInput, rank1Index), [241n, 1, null]);
  const copiedReader = rank1Clone(scalarReader, () => {});
  const [, admitted, ordinary] = scalarCall(copiedReader, scalarInput, rank1Index);
  assert.equal(admitted, 0);
  rank1Equal(ordinary, rank1Value('241'));
  rejectPacketOperation(() => scalarCall(scalarReader, rank1Private, rank1Index));
  assert.deepEqual(scalarCall(scalarReader, scalarInput, rank1Index), [241n, 1, null]);
  check(`(let ((caller (compile-native '(lambda (array index)
                   (declare (optimize (debug 1) (space 2) (speed 1)))
                   (logand (soundness-replaceable-scalar array index) 127))))
               (input #(753)))
           (unwind-protect
                (let ((original (compile-native '(lambda (array index)
                                   (declare (optimize (debug 1) (space 2) (speed 1)))
                                   (logand (aref array index) 255)))))
                  (setf (fdefinition 'soundness-replaceable-scalar) original)
                  (and (= (funcall caller input 0) 113)
                       (progn (setf (fdefinition 'soundness-replaceable-scalar)
                                    (lambda (array index)
                                      (declare (ignore array index)) (values 243 :secondary)))
                              (and (equal (multiple-value-list
                                            (soundness-replaceable-scalar input 0)) '(243 :secondary))
                                   (= (funcall caller input 0) 115)))
                       (= (funcall original input 0) 241)
                       (progn (fmakunbound 'soundness-replaceable-scalar)
                              (handler-case (progn (funcall caller input 0) nil)
                                (undefined-function () t)))))
             (fmakunbound 'soundness-replaceable-scalar)))`);
  console.log('Soundness: reusable scalar entries bind to their installed owner and preserve live user calls.');
  check(`(let* ((worker (compile-native '(lambda (array limit)
                  (declare (optimize (safety 1) (debug 1) (space 3) (speed 1)))
                  (do ((index 0 (1+ index)) (sum 0 (+ sum (aref array index))))
                      ((>= index limit) (values sum :done))))))
                (base (make-array 4 :element-type 'fixnum :initial-contents '(9 3 -5 11)))
                (view (make-array 3 :element-type 'fixnum :displaced-to base :displaced-index-offset 1)))
           (and (equal (multiple-value-list (funcall worker base 4)) '(18 :done))
                (equal (multiple-value-list (funcall worker view 3)) '(9 :done))
                (equal (multiple-value-list (funcall worker view 2.5d0)) '(9 :done))
                (equal (multiple-value-list (funcall worker view 0)) '(0 :done))
                (handler-case (progn (funcall worker view 4) nil)
                  (type-error (condition) (eql (type-error-datum condition) 3)))
                (equal (multiple-value-list (funcall worker view 1)) '(3 :done))))`);
  check(`(let* ((worker (compile-native '(lambda (array)
                  (declare (optimize (safety 1) (debug 0) (space 3) (speed 1)))
                  (let ((index 0) (seen nil))
                    (dolist (step '(a b c) (nreverse seen))
                      (incf index)
                      (push (multiple-value-list (aref array index)) seen))))))
                (array (make-array 4 :element-type 'fixnum :fill-pointer 0
                          :initial-contents '(5 7 9223372036854775807 13)))
                (expected '((7) (9223372036854775807) (13))))
           (and (equal (funcall worker array) expected)
                (handler-case (progn (funcall worker (make-array 2)) nil)
                  (type-error (condition)
                    (and (eql (type-error-datum condition) 2)
                         (typep 1 (type-error-expected-type condition))
                         (not (typep 2 (type-error-expected-type condition))))))
                (equal (funcall worker array) expected)))`);
  check(`(let* ((worker (compile-native '(lambda (vector)
                    (loop for value across vector collect value))))
                (cycle (list :cycle)))
      (setf (cdr cycle) cycle)
      (and (every (lambda (input)
                    (and (handler-case (progn (funcall worker input) nil)
                           (type-error (condition)
                             (and (eq (type-error-datum condition) input)
                                  (eq (type-error-expected-type condition) 'vector))))
                         (equal (funcall worker #(1 2 3)) '(1 2 3))))
                  (list nil '(1 2) cycle 42
                        (make-array nil :initial-element 0)
                        (make-array '(0 2)) (make-array '(2 2))))))`);
  const across = rank1Value("(compile-native '(lambda (vector) (loop for value across vector count value)))");
  for (const invalid of [foreign, packets.empty(), rank1Private]) {
    rejectPacketOperation(() => vm.vm['native-call1'](across, invalid));
  }
  console.log('Soundness: ACROSS rejects nonvectors and malformed raw values and recovers after rejection.');

  // Scalar native imports admit every supplied operand before builtin
  // constructors or workers can discard it, just like full-value transport.
  await vm.mountFile('/soundness-scalar-calls.lisp',
    await fs.readFile(new URL('./soundness-scalar-calls.lisp', import.meta.url), 'utf8'));
  evaluate('(load "/soundness-scalar-calls.lisp") (values)');
  const scalarWorkers = Array.from({length: 5}, (_, arity) =>
    packetFunction(`(svref *soundness-scalar-workers* ${arity})`));
  const scalarList = packetFunction("#'list");
  const scalarEql = packetFunction("#'eql");
  const scalarExpected = vm.vm['native-int64'](42n);
  const checkScalarResult = value =>
    assert.notEqual(vm.vm['native-call2'](scalarEql, value, scalarExpected), null);
  for (const [arity, worker] of scalarWorkers.entries()) {
    const call = (function_, args) => vm.vm[`native-call${arity}`](function_, ...args);
    const tail = (function_, args) => vm.vm[`native-tail-call${arity}`](function_, ...args);
    const valid = Array(arity).fill(null);
    checkScalarResult(call(worker, valid));
    const validTail = tail(scalarList, valid);
    vm.vm['native-check-arguments'](validTail, arity, arity);
    valid.forEach((value, index) => assert.equal(vm.vm['native-argument'](validTail, index), value));
    for (const invalid of [foreign, packets.empty()]) {
      rejectPacketOperation(() => call(invalid, valid));
      rejectPacketOperation(() => tail(invalid, valid));
      for (let index = 0; index < arity; index++) {
        const args = valid.with(index, invalid);
        rejectPacketOperation(() => call(worker, args));
        rejectPacketOperation(() => call(scalarList, args));
        rejectPacketOperation(() => tail(worker, args));
        rejectPacketOperation(() => tail(scalarList, args));
      }
    }
    checkScalarResult(call(worker, valid));
  }
  for (const name of ['native-tail-enter', 'native-tail-direct']) {
    for (const invalid of [foreign, packets.empty()])
      rejectPacketOperation(() => vm.vm[name](invalid));
  }
  console.log('Soundness: scalar and fixed tail calls reject malformed functions and ignored operands with recovery.');
  check('(check-wide-native-calls)');
  const wideWorker = packetFunction(`(compile-native
    '(lambda (a b c d e f g h i j k l m n o p)
       (declare (ignore a b c d e f g h i j k l m n o) (optimize (debug 1)))
       p))`);
  const wideCall = (args, flags) => vm.vm['native-wide-call'](wideWorker, ...args, flags);
  const wideArgs = [...Array(15).fill(null), elements[0]];
  for (const flags of [16 | 32, 16 | 64]) {
    assert.equal(wideCall(wideArgs, flags), elements[0]);
    for (const invalid of [foreign, packets.empty()]) {
      for (const index of [0, 14, 15])
        rejectPacketOperation(() => wideCall(wideArgs.with(index, invalid), flags));
    }
    assert.equal(wideCall(wideArgs, flags), elements[0]);
  }
  rejectPacketOperation(() => wideCall(wideArgs, 17));
  console.log('Soundness: wide direct and packet calls preserve tail calls, values, checked operands and arity recovery.');
  // Fixed transport keeps ordinary checking and complete values even when
  // its actual callee misses the native-worker path.
  const knownCall = (function_, args, primary = false) => vm.vm['native-fixed-call'](
    function_, ...Array.from({length: 4}, (_, index) => args[index] ?? null),
    args.length | (primary ? 8 : 0));
  const knownWorker = packetFunction(`(compile-native '(lambda (function value)
    (declare (optimize (debug 1) (safety 1))) (funcall function value)))`);
  const manyValues = packetFunction('(lambda (value) (values value nil :third 4 5 6 7 8 9))');
  const noValues = packetFunction('(lambda (value) (declare (ignore value)) (values))');
  const fallback = packetFunction('(lambda (left right) (values left right))');
  for (const [function_, args, expected] of [
    [knownWorker, [manyValues, elements[0]], [elements[0], null]],
    [knownWorker, [noValues, elements[0]], []],
    [fallback, elements, elements],
  ]) {
    const result = knownCall(function_, args);
    expected.forEach((value, index) => assert.equal(vm.vm['native-argument'](result, index), value));
    if (!expected.length) assert.equal(result, null);
    assert.equal(knownCall(function_, args, true), expected[0] ?? null);
  }
  const completeKnownValues = knownCall(knownWorker, [manyValues, elements[0]]);
  vm.vm['native-check-arguments'](completeKnownValues, 9, 9);
  for (const flags of [-1, 5, 7, 13, 0x7fffffff])
    rejectPacketOperation(() => vm.vm['native-fixed-call'](knownWorker, null, null, null, null, flags));
  for (const invalid of [foreign, packets.empty()]) {
    for (let index = 0; index < 5; index++) {
      const operands = [knownWorker, manyValues, elements[0], null, null];
      operands[index] = invalid;
      rejectPacketOperation(() => vm.vm['native-fixed-call'](...operands, 2));
    }
  }
  rejectPacketOperation(() => knownCall(knownWorker, [manyValues]));
  rejectPacketOperation(() => knownCall(null, []));
  assert.equal(knownCall(knownWorker, [manyValues, elements[0]], true), elements[0]);
  console.log('Soundness: fixed native transport checks ingress, retains complete values and recovers from fallback errors.');
  // A loader proof belongs to one actual code object. Image constructors
  // cannot restore authority from an integer supplied by a saved payload.
  const coreFields = object => {
    const type = vm.vm['core-object-type'](object);
    const reader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${type}:`));
    return {type, signature: reader.split(':')[2], values: vm.vm[reader](object)};
  };
  evaluate(`(defgeneric soundness-fixed-dispatch (function value))
    (defvar *soundness-fixed-dispatch-events* nil)
    (funcall (compile-native '(lambda ()
      (defmethod soundness-fixed-dispatch ((function t) value)
        (declare (optimize (debug 1))) (funcall function value))
      (defmethod soundness-fixed-dispatch :before ((function function) value)
        (declare (ignore function value)) (push :before *soundness-fixed-dispatch-events*))
      (defmethod soundness-fixed-dispatch :after ((function function) value)
        (declare (ignore function value)) (push :after *soundness-fixed-dispatch-events*))))) (values)`);
  const knownGeneric = packetFunction("#'soundness-fixed-dispatch");
  for (const callback of [manyValues, noValues, manyValues]) {
    evaluate('(setq *soundness-fixed-dispatch-events* nil) (values)');
    const values = knownCall(knownGeneric, [callback, elements[0]]);
    if (callback === noValues) assert.equal(values, null);
    else {
      vm.vm['native-check-arguments'](values, 9, 9);
      assert.equal(vm.vm['native-argument'](values, 0), elements[0]);
    }
    check("(equal *soundness-fixed-dispatch-events* '(:after :before))");
  }
  // Serialized metadata remains boundary data even when its function carries
  // a generic dispatch code. Restore the same object after each rejection.
  const genericFields = coreFields(knownGeneric);
  const genericMetadata = genericFields.signature.indexOf('R');
  assert.ok(genericMetadata >= 0);
  const setGenericMetadata = metadata => vm.vm[`core-set:${genericFields.type}`](knownGeneric,
    ...genericFields.values.map((value, index) => index === genericMetadata ? metadata : value)
      .filter((_, index) => genericFields.signature[index] !== genericFields.signature[index].toLowerCase()));
  try {
    for (const invalid of [null, elements[0], integerOne, packets.empty()]) {
      setGenericMetadata(invalid);
      rejectPacketOperation(() => knownCall(knownGeneric, [manyValues, elements[0]]));
      setGenericMetadata(genericFields.values[genericMetadata]);
      assert.equal(knownCall(knownGeneric, [manyValues, elements[0]], true), elements[0]);
    }
  } finally {setGenericMetadata(genericFields.values[genericMetadata]);}
  console.log('Soundness: fixed generic calls retain effective-method values, effects and checked metadata recovery.');
  const provedRoot = packetFunction(`(compile-native '(lambda (x)
    (declare (optimize (debug 1))) x) :name 'soundness-proof-replay)`);
  const checkedRoot = packetFunction(`(compile-native '(lambda (x)
    (declare (inline car) (optimize (debug 1) (safety 1))) (car x)))`);
  const provedFields = coreFields(provedRoot), checkedFields = coreFields(checkedRoot);
  const directSlot = provedFields.signature.indexOf('e') + 2;
  const provedCode = provedFields.values[directSlot];
  const codeFields = coreFields(provedCode);
  const codeEntry = codeFields.signature.indexOf('e'), proofSlot = codeEntry + 1;
  const cloneFields = codeFields.values.slice();
  cloneFields[codeEntry] = coreFields(checkedFields.values[directSlot]).values[codeEntry];
  cloneFields[proofSlot] = 0x7fffffff;
  const clonedCode = vm.vm[`core-new:${codeFields.type}`](...cloneFields.filter((_, index) =>
    codeFields.signature[index] === codeFields.signature[index].toLowerCase()));
  assert.equal(coreFields(provedCode).values[proofSlot], 0, 'Saving strips the process-local proof hint.');
  assert.equal(coreFields(clonedCode).values[proofSlot], 0, 'Restoring ignores a supplied proof hint.');
  const setDirect = direct => vm.vm[`core-set:${provedFields.type}`](provedRoot,
    ...provedFields.values.map((value, index) => index === directSlot ? direct : value)
      .filter((_, index) => provedFields.signature[index] !== provedFields.signature[index].toLowerCase()));
  try {
    setDirect(clonedCode);
    for (const invoke of [() => vm.vm['native-call1'](provedRoot, elements[0]),
      () => knownCall(provedRoot, [elements[0]])]) {
      assert.throws(() => {
        try {invoke();}
        catch (cause) {
          assert.ok(cause instanceof WebAssembly.Exception);
          throw vm.failure(cause);
        }
      }, error => /SOUNDNESS-PROOF-REPLAY/.test(error.backtrace ?? ''));
    }
  } finally {setDirect(provedCode);}
  assert.equal(vm.vm['native-call1'](provedRoot, elements[0]), elements[0]);
  // An ordinary attached worker has no executable result proof. Reject
  // foreign references and private packets within its dynamic/frame owner.
  const rawResult = new WebAssembly.Global({value: 'externref', mutable: true}, null);
  const rawEntries = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, [2, 0x60, 2, 0x6f, 0x6f, 1, 0x6f,
      0x60, 5, ...Array(5).fill(0x6f), 1, 0x6f]),
    ...section(2, [1, 1, 109, 5, 118, 97, 108, 117, 101, 3, 0x6f, 1]),
    ...section(3, [2, 0, 1]),
    ...section(7, [2, 5, 101, 110, 116, 114, 121, 0, 0,
      6, 100, 105, 114, 101, 99, 116, 0, 1]),
    ...section(10, [2, ...body([0xd0, 0x6f, 0x0b]), ...body([0x23, 0, 0x0b])]),
  ])), {m: {value: rawResult}}).exports;
  const uncheckedResult = vm.vm['native-attach-direct-root'](
    vm.vm['core-native-function'](rawEntries.entry, null, null, 0, null), rawEntries.direct, 1);
  rejectPacketOperation(() => vm.vm['native-attach-direct-root'](
    vm.vm['core-native-function'](rawEntries.entry, null, null, 0, null), rawEntries.direct, 16));
  for (const invalid of [foreign, packets.empty()]) {
    for (const invoke of [() => vm.vm['native-call1'](uncheckedResult, null),
      () => vm.vm['native-call-values1'](uncheckedResult, null),
      () => knownCall(uncheckedResult, [null]),
      () => knownCall(uncheckedResult, [null], true)]) {
      rawResult.value = invalid;
      rejectPacketOperation(invoke);
      rawResult.value = elements[0];
      assert.deepEqual(vm.vm['native-call-values1'](uncheckedResult, null),
        [elements[0], null, null, null, 1]);
    }
  }
  for (const primary of elements) for (const stale of [
    () => vm.vm['native-return'](null),
    () => vm.vm['native-return3'](...Array(3).fill(elements[0])),
  ]) {
    stale();
    assert.deepEqual(vm.vm['native-call-values1'](provedRoot, primary),
      [primary, null, null, null, 1], 'An audited leaf normalizes zero or stale extra values to exactly one.');
  }
  rejectPacketOperation(() => vm.vm['core-native-attach-certified-root'](provedRoot,
    codeFields.values[codeEntry], 1));
  console.log('Soundness: executable authority is transient, bound to its owned code object and cannot replace an existing attachment.');

  // The same proved leaf remains an ordinary mutable function binding and
  // explicit object. Stronger caller debugging preserves caller ownership.
  check(`(let* ((saved (compile-native '(lambda (x)
        (declare (optimize (debug 1))) x)))
      (caller (compile-native '(lambda (function x)
        (declare (optimize (debug 2))) (values (funcall function x)))))
      (named (compile-native '(lambda (x)
        (declare (notinline soundness-proof-global) (optimize (debug 1)))
        (soundness-proof-global x))))
      (object (list :identity)))
    (unwind-protect
      (progn (setf (symbol-function 'soundness-proof-global) saved)
        (and (eq (funcall named object) object)
          (equal (multiple-value-list (funcall caller saved nil)) '(nil))
          (progn (setf (symbol-function 'soundness-proof-global)
                   (compile-native '(lambda (x) (values x :replacement))))
            (and (equal (multiple-value-list (funcall named object))
                        (list object :replacement))
              (eq (funcall caller saved object) object)))))
      (fmakunbound 'soundness-proof-global)))`);
  // Native name lookup reads the live binding cell, including
  // unbinding and first installation after an absent lookup. Exercise the
  // public raw boundary directly so compiler activity cannot hide stale hits.
  const lookupName = vm.vm['native-call0'](packetFunction("(lambda () 'soundness-live-lookup)"));
  const lookupAbsent = vm.vm['native-call0'](packetFunction("(lambda () 'soundness-absent-lookup)"));
  const lookupSet = packetFunction('(lambda (name function) (setf (fdefinition name) function))');
  const lookupUnset = packetFunction('(lambda (name) (fmakunbound name))');
  const lookupFunctions = [packetFunction('(lambda (x) (values x :original))'),
    packetFunction('(lambda (x) (values x :replacement))')];
  const lookup = name => vm.vm['native-function-reference'](name);
  const installLookup = (name, function_) => vm.vm['native-call2'](lookupSet, name, function_);
  try {
    installLookup(lookupName, lookupFunctions[0]);
    assert.equal(lookup(lookupName), lookupFunctions[0]);
    assert.equal(lookup(lookupName), lookupFunctions[0]);
    installLookup(lookupName, lookupFunctions[1]);
    assert.equal(lookup(lookupName), lookupFunctions[1], 'A warmed name observes replacement.');
    vm.vm['native-call1'](lookupUnset, lookupName);
    rejectPacketOperation(() => lookup(lookupName));
    installLookup(lookupName, lookupFunctions[0]);
    assert.equal(lookup(lookupName), lookupFunctions[0]);
    rejectPacketOperation(() => lookup(lookupAbsent));
    installLookup(lookupAbsent, lookupFunctions[1]);
    assert.equal(lookup(lookupAbsent), lookupFunctions[1], 'An absent lookup cannot freeze an undefined binding.');
    for (const invalid of [foreign, packets.empty(), packets.make(...elements)]) {
      rejectPacketOperation(() => lookup(invalid));
      assert.equal(lookup(lookupName), lookupFunctions[0]);
    }
  } finally {
    vm.vm['native-call1'](lookupUnset, lookupName);
    vm.vm['native-call1'](lookupUnset, lookupAbsent);
  }
  console.log('Soundness: native name lookup observes warmed cell mutation and rejects malformed names with recovery.');
  // Exercise the scalar imports themselves: a compiled unknown-call driver
  // can choose full-value transport instead and miss these primary entries.
  const primarySymbol = vm.vm['native-call0'](packetFunction("(lambda () 'soundness-primary-designator)"));
  const primaryRebound = vm.vm['native-call0'](packetFunction('(lambda () :rebound)'));
  const primaryIdentity = packetFunction("#'identity");
  try {
    for (const arity of [1, 2, 3, 4]) {
      const parameters = ['a', 'b', 'c', 'd'].slice(0, arity);
      const original = packetFunction(`(compile-native '(lambda (${parameters.join(' ')})
        (declare (optimize (debug 1) (safety 1)) ${arity > 1 ? `(ignore ${parameters.slice(1).join(' ')})` : ''})
        (values a nil :original)))`);
      const replacement = packetFunction(`(compile-native '(lambda (${parameters.join(' ')})
        (declare (ignore ${parameters.join(' ')}) (optimize (debug 1) (safety 1)))
        (values :rebound nil :replacement)))`);
      const arguments_ = Array(arity).fill(elements[0]);
      const invoke = () => vm.vm[`native-call${arity}`](primarySymbol, ...arguments_);
      installLookup(primarySymbol, original);
      assert.equal(invoke(), elements[0]);
      assert.equal(invoke(), elements[0]);
      installLookup(primarySymbol, replacement);
      assert.equal(invoke(), primaryRebound, 'A warmed scalar symbol call selects its current native binding.');
      assert.equal(vm.vm[`native-call${arity}`](original, ...arguments_), elements[0],
        'The saved callable retains its own identity after rebinding.');
    }
    for (const [expression, expected] of [
      ["(compile-native '(lambda (x) (declare (ignore x)) (values)))", null],
      ["(compile-native '(lambda (x) (declare (ignore x)) (values nil)))", null],
      ['(lambda (x) (values x :interpreted))', elements[0]],
    ]) {
      installLookup(primarySymbol, packetFunction(expression));
      assert.equal(vm.vm['native-call1'](primarySymbol, elements[0]), expected);
    }
    installLookup(primarySymbol, primaryIdentity);
    assert.equal(vm.vm['native-call1'](primarySymbol, elements[0]), elements[0]);
    installLookup(primarySymbol, packetFunction("(compile-native '(lambda () :wrong-arity))"));
    rejectPacketOperation(() => vm.vm['native-call1'](primarySymbol, elements[0]));
    installLookup(primarySymbol, primaryIdentity);
    assert.equal(vm.vm['native-call1'](primarySymbol, elements[0]), elements[0]);
    vm.vm['native-call1'](lookupUnset, primarySymbol);
    rejectPacketOperation(() => vm.vm['native-call1'](primarySymbol, elements[0]));
    installLookup(primarySymbol, primaryIdentity);
    assert.equal(vm.vm['native-call1'](primarySymbol, elements[0]), elements[0]);
  } finally {vm.vm['native-call1'](lookupUnset, primarySymbol);}
  console.log('Soundness: scalar symbol entries retain live native bindings, primary demand and fallback recovery.');
  // Canonical cons layouts do not authenticate their children. Function-name
  // admission checks the bounded (SETF symbol) grammar before reading tags.
  const setterName = vm.vm['native-call0'](packetFunction("(lambda () '(setf soundness-shaped-lookup))"));
  const setfSymbol = vm.vm['native-argument'](setterName, 0);
  const targetSymbol = vm.vm['native-argument'](setterName, 1);
  const digits = coreFields(elements[0]).values[3];
  assert.ok(digits);
  const literalPool = vm.vm['native-call0'](packetFunction(
    "(lambda () (vector '(setf soundness-shaped-lookup) 23))"));
  const literalOwner = pool => vm.vm['core-native-function'](rawEntries.entry, pool, null, 0, null);
  const lookupLiteral = vm.vm['native-literal-function'];
  const setterOwner = literalOwner(literalPool);
  try {
    installLookup(setterName, lookupFunctions[0]);
    for (const invalid of [packets.empty(), digits]) {
      for (const malformed of [
        vm.vm['native-cons'](invalid, vm.vm['native-cons'](targetSymbol, null)),
        vm.vm['native-cons'](setfSymbol, invalid),
        vm.vm['native-cons'](setfSymbol, vm.vm['native-cons'](invalid, null)),
        vm.vm['native-cons'](setfSymbol, vm.vm['native-cons'](targetSymbol, invalid)),
      ]) {
        rejectPacketOperation(() => lookup(malformed));
        assert.equal(lookup(setterName), lookupFunctions[0]);
      }
    }
    assert.equal(lookupLiteral(setterOwner, 0), lookupFunctions[0]);
    installLookup(setterName, lookupFunctions[1]);
    assert.equal(lookupLiteral(setterOwner, 0), lookupFunctions[1]);
    vm.vm['native-call1'](lookupUnset, setterName);
    rejectPacketOperation(() => lookupLiteral(setterOwner, 0));
    installLookup(setterName, lookupFunctions[0]);
    for (const invalid of [null, foreign, packets.empty(), digits, literalPool, literalOwner(digits)]) {
      rejectPacketOperation(() => lookupLiteral(invalid, 0));
      assert.equal(lookupLiteral(setterOwner, 0), lookupFunctions[0]);
    }
    for (const index of [-1, 1, 2]) {
      rejectPacketOperation(() => lookupLiteral(setterOwner, index));
      assert.equal(lookupLiteral(setterOwner, 0), lookupFunctions[0]);
    }
    const poolData = coreFields(literalPool).values.at(-1);
    for (const invalid of [packets.empty(), digits]) {
      packets.set(poolData, 0, invalid);
      rejectPacketOperation(() => lookupLiteral(setterOwner, 0));
      rejectPacketOperation(() => vm.vm['native-argument'](literalPool, 0));
      packets.set(poolData, 0, setterName);
      assert.equal(lookupLiteral(setterOwner, 0), lookupFunctions[0]);
      assert.equal(vm.vm['native-argument'](literalPool, 0), setterName);
    }
  } finally {vm.vm['native-call1'](lookupUnset, setterName);}
  console.log('Soundness: checked names and literal lookups preserve live SETF bindings and recover from malformed inputs.');
  const applyArguments = vm.vm['native-cons'](elements[0], null);
  for (const mode of [0, 1, 2, 3]) {
    const result = vm.vm['native-apply-list'](provedRoot, applyArguments, mode);
    if (mode & 1) assert.equal(result, elements[0]);
    else assert.deepEqual(vm.javascript.list(result, value => value), [elements[0]]);
    for (const invalid of [foreign, packets.empty()]) {
      rejectPacketOperation(() => vm.vm['native-apply-list'](invalid, applyArguments, mode));
      rejectPacketOperation(() => vm.vm['native-apply-list'](provedRoot, invalid, mode));
    }
    rejectPacketOperation(() => vm.vm['native-apply-list'](provedRoot,
      vm.vm['native-cons'](elements[0], elements[0]), mode));
    assert.equal(vm.vm['native-apply-list'](provedRoot, applyArguments, mode | 1), elements[0]);
  }
  for (const mode of [-1, 4])
    rejectPacketOperation(() => vm.vm['native-apply-list'](provedRoot, applyArguments, mode));
  for (const invalid of [packets.empty(), digits]) {
    const badTail = vm.vm['native-cons'](elements[0], invalid);
    const badElement = vm.vm['native-cons'](invalid, null);
    for (const mode of [0, 1, 2, 3]) {
      rejectPacketOperation(() => vm.vm['native-apply-list'](provedRoot, badTail, mode));
      rejectPacketOperation(() => vm.vm['native-apply-list'](provedRoot, badElement, mode));
      assert.equal(vm.vm['native-apply-list'](provedRoot, applyArguments, mode | 1), elements[0]);
    }
    rejectPacketOperation(() => vm.vm['native-argument'](badTail, 0));
    rejectPacketOperation(() => vm.vm['native-argument'](badElement, 0));
    assert.equal(vm.vm['native-argument'](applyArguments, 0), elements[0]);
    rejectPacketOperation(() => vm.vm['native-check-arguments'](badTail, 1, 1));
  }
  console.log('Soundness: APPLY list transport validates raw ingress and recovers in full and primary modes.');
  for (const terminal of [integerOne, elements[0]]) {
    const dotted = vm.vm['native-cons'](elements[0], terminal);
    vm.vm['native-check-arguments'](dotted, 1, -1);
    assert.equal(vm.vm['native-rest-argument'](dotted, 1), terminal);
    rejectPacketOperation(() => vm.vm['native-check-arguments'](dotted, 1, 1));
    vm.vm['native-check-arguments'](dotted, 1, -1);
    assert.equal(vm.vm['native-rest-argument'](dotted, 1), terminal);
  }
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
  assert.equal(vm.vm['native-primary'](sharedValues[1]), changedFirst,
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
  const allowedKeyword = vm.vm['native-cons'](later, null);
  for (const invalid of [packets.empty(), digits]) {
    const invalidName = packets.make(invalid, null);
    const invalidValue = packets.make(later, invalid);
    rejectPacketOperation(() => vm.vm['native-keyword-present'](invalidName, 0, later));
    rejectPacketOperation(() => vm.vm['native-keyword-argument'](invalidValue, 0, later));
    rejectPacketOperation(() => vm.vm['native-check-keywords'](invalidName, 0, allowedKeyword, 1));
    rejectPacketOperation(() => vm.vm['native-check-keywords'](invalidValue, 0, allowedKeyword, 0));
  }
  const suppliedNil = packets.make(later, null);
  assert.ok(vm.vm['native-keyword-present'](suppliedNil, 0, later));
  assert.equal(vm.vm['native-keyword-argument'](suppliedNil, 0, later), null);
  assert.equal(vm.vm['native-keyword-present'](suppliedNil, 0x7fffffff, later), null);
  vm.vm['native-check-keywords'](suppliedNil, 0, allowedKeyword, 0);
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

  // Current-format numeric restoration can supply a boxed i64 whose value
  // fits i31. Executed range facts must not turn its extraction into a cast.
  const integerSampleValue = 1n << 40n;
  const integerSample = vm.vm['native-int64'](integerSampleValue);
  const integerType = vm.vm['core-object-type'](integerSample);
  const integerReader = Object.keys(vm.vm).find(name => name.startsWith(`core-read:${integerType}:`));
  const integerSignature = integerReader.split(':')[2];
  const integerFields = vm.vm[integerReader](integerSample);
  const immutableIntegerFields = Array.from(integerSignature, (kind, index) => index)
    .filter(index => integerSignature[index] === integerSignature[index].toLowerCase());
  const mutableIntegerFields = Array.from(integerSignature, (kind, index) => index)
    .filter(index => integerSignature[index] !== integerSignature[index].toLowerCase());
  const integerPayloadArgument = immutableIntegerFields.findIndex(index =>
    integerSignature[index] === 'l' && integerFields[index] === integerSampleValue);
  const integerSignArgument = mutableIntegerFields.findIndex(index =>
    integerSignature[index] === 'I' && integerFields[index] === 1);
  assert.ok(integerPayloadArgument >= 0 && integerSignArgument >= 0);
  const boxedInteger = number => {
    const arguments_ = immutableIntegerFields.map(index => integerFields[index]);
    const mutable = mutableIntegerFields.map(index => integerFields[index]);
    arguments_[integerPayloadArgument] = number;
    mutable[integerSignArgument] = number < 0n ? -1 : number > 0n ? 1 : 0;
    const value = vm.vm[`core-new:${integerType}`](...arguments_);
    vm.vm[`core-set:${integerType}`](value, ...mutable);
    assert.equal(typeof value, 'object', 'Exercise actual boxed storage.');
    assert.equal(vm.javascript.integer(value), number);
    return value;
  };
  const wideInteger = (value, expected) => {
    assert.equal(vm.javascript.integer(value), expected);
    assert.equal(typeof value, 'object', 'A wide result is stored in an immutable scalar box.');
    assert.equal(vm.vm['core-object-type'](value), integerType,
      'Computed scalar i64 results retain boxed storage, including zero.');
    return value;
  };
  const freshInteger = number => vm.vm['native-call0'](
    packetFunction(`(compile-native '(lambda () ${number}))`));
  const freshZero = freshInteger(0n), freshOne = freshInteger(1n);
  const immediateIdentity = rank1Value(`(compile-native '(lambda (value)
    (declare (type (signed-byte 31) value) (optimize (debug 1) (space 2) (speed 1)))
    value))`);
  const boxedOne = boxedInteger(1n);
  assert.deepEqual(vm.vm['native-scalar-call'](immediateIdentity, freshOne, null, null, null, 1, 1),
    [1n, 1, null]);
  assert.deepEqual(vm.vm['native-scalar-call'](immediateIdentity, boxedOne, null, null, null, 1, 1),
    [0n, 0, boxedOne]);
  for (const value of [freshZero, freshOne, boxedInteger(1n), boxedInteger(-(1n << 63n)),
    boxedInteger((1n << 63n) - 1n)]) {
    assert.equal(vm.vm['native-check-int64'](value), value,
      'Checked scalar ingress preserves both immediate and boxed integer identity.');
  }
  for (const invalid of [foreign, packets.empty(), null, freshInteger(1n << 80n)]) {
    rejectPacketOperation(() => vm.vm['native-check-int64'](invalid));
  }
  assert.equal(vm.vm['native-check-int64'](freshOne), freshOne);
  // Keep an actual scalar box while giving it a non-integer object header.
  const malformedIntegerFields = integerFields.slice();
  malformedIntegerFields[0] = coreFields(pair).values[0];
  const malformedInteger = vm.vm[`core-new:${integerType}`](
    ...immutableIntegerFields.map(index => malformedIntegerFields[index]));
  vm.vm[`core-set:${integerType}`](malformedInteger,
    ...mutableIntegerFields.map(index => malformedIntegerFields[index]));
  const lowFieldAdd = vm.vm['native-low-field-add'];
  rejectPacketOperation(() => lowFieldAdd(3, malformedInteger, freshOne));
  rejectPacketOperation(() => lowFieldAdd(3, freshOne, malformedInteger));
  assert.equal(vm.javascript.integer(lowFieldAdd(3, freshOne, byteInput)), 7n);
  for (const [name, expected] of [['add', 2n], ['sub', 0n], ['mul', 1n]]) {
    const operation = vm.vm[`native-${name}-integer`];
    assert.equal(operation(freshOne, freshOne), freshInteger(expected),
      'Scalar object entries preserve actual immediate operands without promotion.');
    wideInteger(operation(boxedInteger(1n), freshOne), expected);
    for (const invalid of [foreign, packets.empty(), null, freshInteger(1n << 80n)]) {
      rejectPacketOperation(() => operation(invalid, freshOne));
      rejectPacketOperation(() => operation(freshOne, invalid));
    }
    assert.equal(operation(freshOne, freshOne), freshInteger(expected));
  }
  for (const [name, left, right, expected] of [
    ['+', 5n, -4n, 1n], ['-', 5n, 4n, 1n], ['*', -1n, -1n, 1n],
    ['*', 5n, 0n, 0n], ['*', 0n, 5n, 0n],
  ]) {
    const code = canonicalCode(name), a = boxedInteger(left), b = boxedInteger(right);
    wideInteger(vm.vm['native-standard-call2'](code, a, b), expected);
    assert.equal(vm.javascript.integer(a), left, 'Arithmetic preserves shared input snapshots.');
    assert.equal(vm.javascript.integer(b), right);
  }
  for (const [left, right] of [
    [-(1n << 30n), -(1n << 30n)], [(1n << 30n) - 1n, (1n << 30n) - 1n],
    [-(1n << 30n), (1n << 30n) - 1n], [-(1n << 30n), -1n],
    [(1n << 30n) - 1n, 1n], [0n, -(1n << 30n)], [-5n, 7n],
  ]) {
    const a = freshInteger(left), b = freshInteger(right);
    assert.equal(vm.javascript.integer(vm.vm['native-standard-call2'](canonicalCode('*'), a, b)), left * right);
    assert.equal(vm.javascript.integer(a), left, 'Immediate products preserve shared input snapshots.');
    assert.equal(vm.javascript.integer(b), right);
  }
  for (const invalid of [foreign, packets.empty(), null]) {
    rejectPacketOperation(() => vm.vm['native-standard-call2'](canonicalCode('*'), freshZero, invalid));
    rejectPacketOperation(() => vm.vm['native-standard-call2'](canonicalCode('*'), invalid, freshZero));
  }
  for (const [name, expected] of [['1+', 1n], ['1-', -1n], ['-', 0n]])
    wideInteger(vm.vm['native-standard-call1'](canonicalCode(name), boxedInteger(0n)), expected);
  // Native unary ingress accepts restored scalar boxes as well as fresh
  // immediates and bignums. Carry/borrow and promotion must leave shared
  // input snapshots intact, including across complete base-2^15 limbs.
  for (const name of ['1+', '1-']) {
    const step = name === '1+' ? 1n : -1n, code = canonicalCode(name);
    for (const bits of [30n, 63n, 75n, 150n]) {
      for (const number of [(1n << bits) - 1n, 1n << bits, -(1n << bits), -(1n << bits) + 1n]) {
        const inputs = [freshInteger(number)];
        if (number >= -(1n << 63n) && number < (1n << 63n)) inputs.push(boxedInteger(number));
        for (const input of inputs) {
          assert.equal(vm.javascript.integer(vm.vm['native-standard-call1'](code, input)), number + step);
          assert.equal(vm.javascript.integer(input), number, 'Unary arithmetic preserves shared integer snapshots.');
        }
      }
    }
  }
  for (const [name, expected] of [['add', 6n], ['sub', 4n], ['mul', 5n]])
    for (const suffix of ['', '-checked'])
      wideInteger(vm.vm[`native-${name}-int64${suffix}`](5n, 1n), expected);
  const large = 1n << 80n, largeValue = freshInteger(large);
  wideInteger(vm.vm['native-standard-call2'](canonicalCode('-'), largeValue,
    freshInteger(large - 7n)), 7n);
  wideInteger(vm.vm['native-standard-call2'](canonicalCode('-'), largeValue, largeValue), 0n);
  wideInteger(vm.vm['native-standard-call2'](canonicalCode('*'), largeValue, freshZero), 0n);
  const promoted = vm.vm['native-add-int64-checked']((1n << 63n) - 1n, 1n);
  wideInteger(vm.vm['native-standard-call2'](canonicalCode('-'), promoted, freshOne), (1n << 63n) - 1n);
  check(`(and (typep (- (expt 2 80) (- (expt 2 80) 7)) 'fixnum)
    (not (typep (- (expt 2 80) (- (expt 2 80) 7)) 'bignum)))`);
  const narrowedInteger = packetFunction(`(compile-native '(lambda (x)
    (declare (optimize (safety 0) (debug 0)))
    (if (integerp x)
        (if (>= x 0) (if (< x 10) (+ x 1) x) x)
        0)))`);
  for (const number of [-1073741825n, -1n, 0n, 5n, 9n, 10n, 1073741823n, 1073741824n]) {
    const expected = number >= 0n && number < 10n ? number + 1n : number;
    for (const value of [vm.vm['native-int64'](number), boxedInteger(number)]) {
      const result = vm.vm['native-call1'](narrowedInteger, value);
      assert.equal(vm.javascript.integer(result), expected);
      if (number >= 0n && number < 10n) wideInteger(result, expected);
    }
  }
  const provenInteger = packetFunction(`(compile-native '(lambda (x)
    (declare (optimize (safety 1) (debug 0))) (+ (logand x 7) 1)))`);
  assert.equal(vm.javascript.integer(vm.vm['native-call1'](provenInteger, boxedInteger(5n))), 6n);
  recover();

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
    ["(compile-native '(lambda () (multiple-value-bind (x))))", /value form/],
    ["(compile-native '(lambda () (multiple-value-bind (x x) (values 1 2) x)))", /duplicate variable/],
    ["(compile-native '(lambda () (multiple-value-bind (x . y) (values 1 2) x)))", /dotted tail/],
    ["(compile-native '(lambda () (multiple-value-bind (:constant) 1 nil)))", /nonconstant variable/],
    [`(let ((variables (list 'x))) (setf (cdr variables) variables)
       (compile-native (list 'lambda nil (list 'multiple-value-bind variables nil nil))))`, /circular list/],
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
  const caseOperator = "(car (macroexpand '(case nil (otherwise nil))))";
  // Private selection syntax keeps operator-looking keys opaque while writes
  // in its executable key form and bodies still invalidate function values.
  check(`(let ((key (list 'multiple-value-bind)))
    (setf (cdr key) key)
    (let ((form (list ${caseOperator} 0 nil
                  (list (list key) '(values 42)))))
      (and (not (nth-value 1 (form-written-variables form)))
        (member (list :literal (symbol-key 'multiple-value-bind))
          (form-references form) :test #'equal) t)))`);
  check(`(every (lambda (selection)
      (member (list :indirect-call (symbol-name 'funcall))
        (nth-value 1 (form-references
          (list 'let '((fn (lambda (x) x)))
            (cons ${caseOperator} selection) '(funcall fn 42))))
        :test #'equal))
    '((0 (setq fn (soundness-scanner-producer))
        (multiple-value-bind (values 42)))
      (2 nil
        ((multiple-value-bind setq) (setq fn (soundness-scanner-producer))))))`);
  check(`(and
    (every (lambda (mode)
      (nth-value 1 (form-written-variables
        (list ${caseOperator} mode 'fn
          '((satisfies soundness-scanner-syntax) (values 42)))))) '(1 3 4 5))
    (nth-value 1 (form-written-variables
      (list ${caseOperator} 0 nil
        '(otherwise (declare (special fn)) fn)))) t)`);
  // Retained standard MVB syntax is executable binding structure. Its body
  // must mask outer function-value facts before reachability removes roots.
  check(`(let* ((plain '(let ((fn (lambda (x) x)))
                         (multiple-value-bind (x) (values 42) (funcall fn x))))
      (plain-barriers (nth-value 1 (form-references plain)))
      (shadowed-barriers (nth-value 1 (form-references
        '(let ((fn (lambda (x) x)))
           (multiple-value-bind (fn) (soundness-scanner-producer) (funcall fn 42)))))))
    (and (not (nth-value 1 (form-written-variables plain)))
      (not (member "UNEXPANDED-MACRO" plain-barriers :test #'equal))
      (not (member (list :indirect-call (symbol-name 'funcall)) plain-barriers :test #'equal))
      (member (list :indirect-call (symbol-name 'funcall)) shadowed-barriers :test #'equal) t))`);
  for (const source of [
    "(form-references '(progn (+ 1 . 2)))",
    `(form-references '(flet ((soundness-scanner-syntax (x) x))
       (soundness-scanner-syntax (+ 1 . 2))))`,
    `(let ((form (list '+ 1))) (setf (cddr form) form) (form-references form))`,
    `(form-references (list ${caseOperator} 0 nil
       '(multiple-value-bind . 42)))`,
    `(let ((clause (list 'multiple-value-bind))) (setf (cdr clause) clause)
       (form-references (list ${caseOperator} 0 nil clause)))`,
  ]) reject(source, /dotted tail|circular list/);
  reject(`(form-references (list ${caseOperator} 0))`, /mode and key form/);
  reject(`(form-references (list ${caseOperator} :invalid nil))`, /mode from 0 through 5/);
  reject(`(form-references (list ${caseOperator} 0 nil nil))`, /expected a key/);
  evaluate("(fmakunbound 'soundness-scanner-syntax) (values)");
  evaluate("(defmacro soundness-bad-expansion () '(let ((x 1 2)) x)) (values)");
  reject("(compile-native '(lambda () (soundness-bad-expansion)))", /binding/);
  // Preserving MVB bypasses its ordinary macro expansion, so both native and
  // file preparation must still validate a macro-produced binding structure.
  evaluate(`(defmacro soundness-bad-mvb ()
    '(multiple-value-bind (x x) (values 1 2) x)) (values)`);
  reject("(compile-native '(lambda () (soundness-bad-mvb)))", /duplicate variable/);
  vm.mountFile('/soundness-mvb.lisp', `(in-package :miricl-compiler)
    (defun soundness-file-mvb () (soundness-bad-mvb))`);
  reject('(compile-file "/soundness-mvb.lisp" :verbose nil)', /duplicate variable/);
  vm.mountFile('/soundness-mvb.lisp', `(in-package :miricl-compiler)
    (defun soundness-file-mvb ()
      (multiple-value-bind (x y) (values 42) (values x y)))`);
  check(`(and (pathnamep (compile-file "/soundness-mvb.lisp" :verbose nil))
    (load "/soundness-mvb.wasm")
    (equal (multiple-value-list (soundness-file-mvb)) '(42 nil)))`);
  vm.mountFile('/soundness-case.lisp', `(in-package :miricl-compiler)
    (defun soundness-file-case (key)
      (let ((events nil))
        (case (progn (setq events (cons :key events)) key)
          (multiple-value-bind
            (setq events (cons :mvb events)) (values 42 events))
          ((setq) (setq events (cons :setq events)) (values 43 events))
          (otherwise (values 44 events)))))`);
  check(`(and (pathnamep (compile-file "/soundness-case.lisp" :verbose nil))
    (load "/soundness-case.wasm")
    (equal (multiple-value-list (soundness-file-case 'multiple-value-bind)) '(42 (:mvb :key)))
    (equal (multiple-value-list (soundness-file-case 'setq)) '(43 (:setq :key)))
    (equal (multiple-value-list (soundness-file-case :other)) '(44 (:key))))`);
  for (const [mutation, reason] of [
    ['(setf (hir-function-parent function) function)', /parent chain/],
    ['(setf (hir-block-id block) -1)', /block record or ID/],
    ['(setf (hir-instruction-opcode instruction) :unknown)', /unknown opcode/],
    ['(setf (hir-instruction-safety-level instruction) -1)', /safety level/],
    ['(push instruction (hir-block-instructions block))', /duplicate instruction placement/],
  ]) for (const consumer of ['emit-native-module', 'emit-hir-code', 'optimize-hir', 'infer-hir-types'])
    reject(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 1))))
      (function (first (hir-unit-functions unit)))
      (block (first (hir-function-blocks function)))
      (instruction (first (hir-block-instructions block))))
    ${mutation} (${consumer} unit))`, reason);
  // Type inference and native scalar admission depend on dominating definitions.
  for (const consumer of ['emit-native-module', 'infer-hir-types'])
    reject(`(let* ((unit (lower-source-to-hir '(lambda () (values 7 9))))
      (function (first (hir-unit-functions unit)))
      (block (hir-function-entry function))
      (instructions (hir-block-instructions block))
      (group (find :values instructions :key (lambda (instruction) (hir-instruction-opcode instruction))))
      (producer (hir-value-producer (first (hir-instruction-inputs group)))))
    (setf (hir-block-instructions block)
      (append (remove producer instructions) (list producer)))
    (${consumer} unit))`, /used before its definition/, 'hir-code-input-error');
  for (const consumer of ['emit-native-module', 'infer-hir-types'])
    reject(`(let* ((unit (lower-source-to-hir '(lambda (flag)
                      (if flag (values 17 29) (values 47 61)))))
      (function (first (hir-unit-functions unit)))
      (groups (loop for block in (hir-function-blocks function)
        for group = (find :values (hir-block-instructions block)
                      :key (lambda (instruction) (hir-instruction-opcode instruction)))
        when (and group (= 2 (length (hir-instruction-inputs group)))) collect group)))
    (setf (first (hir-instruction-inputs (first groups)))
          (first (hir-instruction-inputs (second groups))))
    (${consumer} unit))`, /does not dominate/, 'hir-code-input-error');
  reject(`(let* ((unit (lower-source-to-hir '(lambda (x) (values x *soundness-context*))))
      (function (first (hir-unit-functions unit)))
      (instructions (hir-block-instructions (hir-function-entry function)))
      (access (find :global-read instructions :key (lambda (instruction) (hir-instruction-opcode instruction))))
      (argument (find :argument instructions :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (setf (first (hir-instruction-inputs access)) (hir-instruction-output argument))
    (emit-native-module unit))`, /dynamic environment/, 'hir-code-input-error');
  for (const mutation of [
    '(setf (hir-instruction-attributes enter) (hir-instruction-attributes (block-entry child)))',
    '(setf (hir-target-token (first (hir-instruction-attributes enter))) (hir-function-environment root))',
  ]) reject(`(flet ((block-entry (function)
      (loop for block in (hir-function-blocks function) thereis
        (find :block-enter (hir-block-instructions block)
          :key (lambda (instruction) (hir-instruction-opcode instruction))))))
    (let* ((unit (lower-source-to-hir '(lambda ()
                   (block outer (values 1 (lambda () (block inner (values 2 3))))))))
           (root (first (hir-unit-functions unit)))
           (child (second (hir-unit-functions unit)))
           (enter (block-entry root)))
      ${mutation}
      (emit-native-module unit)))`, /target owner or token/, 'hir-code-input-error');
  check(`(multiple-value-bind (bytes literals)
      (emit-native-module (lower-source-to-hir '(lambda (flag)
                            (if flag (values 17 29) (values 47 61)))))
    (let ((function (compiler-primitive 17 bytes literals nil)))
      (and (equal (multiple-value-list (funcall function t)) '(17 29))
           (equal (multiple-value-list (funcall function nil)) '(47 61)))))`);
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
  // Inline cost analysis must not execute an unknown macro to speculate about
  // its emitted body. The actual lowering gets its single expansion normally.
  evaluate(`(defvar *soundness-inline-expanded* nil)
    (defmacro soundness-inline-expansion (value)
      (when *soundness-inline-expanded* (error "Repeated speculative expansion"))
      (setq *soundness-inline-expanded* t)
      (list 'values value nil :expanded)) (values)`);
  for (const effort of [0, 3]) {
    check(`(let* ((*soundness-inline-expanded* nil)
        (function (compile-native '(lambda (x)
          (declare (optimize (speed 3) (space 1) (debug 0)
            (compilation-speed ${effort})))
          (flet ((worker (selected value)
                   (if selected (soundness-inline-expansion value) (values))))
            (declare (inline worker)) (worker t x))))))
      (and *soundness-inline-expanded*
        (equal (multiple-value-list (funcall function 42)) '(42 nil :expanded))
        (equal (multiple-value-list (funcall function 43)) '(43 nil :expanded))))`);
  }
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

  // Capture slots require shared cells. A client cannot clear the binding's
  // capture flag and select private storage for a cell used by a closure.
  for (const consumer of ['infer-hir-types', 'emit-native-module', 'emit-hir-code'])
    reject(`(let* ((unit (compile-to-hir '(lambda (replacement)
          (declare (optimize (safety 1) (debug 0)))
          (let ((x 0))
            (flet ((change () (setq x replacement))) (change) x)))))
        (root (first (hir-unit-functions unit)))
        (binding (find 'x (hir-function-bindings root)
          :key (lambda (binding) (hir-binding-name binding)))))
      (setf (hir-binding-captured-p binding) nil)
      (${consumer} unit))`, /captured binding|marked captured/);
  check(`(let ((function (compile-native '(lambda (replacement)
        (declare (optimize (safety 1) (debug 0)))
        (let ((x 0))
          (flet ((change () (setq x replacement))) (change) x))))))
    (and (eq (funcall function :replacement) :replacement)
      (= (funcall function 1073741824) 1073741824)))`);

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

  // Shared branch tails must remain valid through public retained-code
  // validation and reconstruction, including definitions no longer executed.
  check(`(let* ((wire (emit-hir-code (compile-to-hir
        '(lambda (flag callback) (declare (optimize (debug 0)))
           (if flag
             (progn (funcall callback :left) (funcall callback :tail))
             (progn (funcall callback :right) (funcall callback :tail)))))))
      (exported (copy-hir-code (validate-hir-code wire)))
      (rebuilt (hir-code-to-unit exported)))
    (verify-hir rebuilt)
    (every (lambda (function)
      (loop for flag in '(t nil) always (let ((events nil))
        (and (equal (multiple-value-list (funcall function flag (lambda (event)
                 (push event events) (values event nil :extra)))) '(:tail nil :extra))
          (equal (reverse events) (list (if flag :left :right) :tail))))))
      (list (load-hir-code exported) (compile-native-hir exported)
        (compile-native-hir (emit-hir-code rebuilt)))))`);

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

  // The retained leaf can feed a raw consumer only while its ingress and
  // successful value count are closed. Boxed fallback retains ordinary CL.
  for (const [input, expected] of [
    ['1073741824', '1073741829'], ['-2', '3'],
    ['9223372036854775807', '9223372036854775812'],
  ]) {
    check(`(let* ((module (compile-native-module
        '((defun soundness-scalar-entry ()
            (declare (optimize (debug 0) (safety 1) (speed 3) (space 1)))
            (locally (declare (notinline soundness-scalar-leaf))
              (+ (soundness-scalar-leaf ${input}) 3)))
          (defun soundness-scalar-leaf (argument)
            (declare (optimize (debug 0) (safety 1))) (+ argument 2)))
        :entry-points '(soundness-scalar-entry) :inline nil)))
      (equal (multiple-value-list (funcall (cdr (assoc 'soundness-scalar-entry module))))
        '(${expected})))`);
  }
  for (const [body, expected] of [
    ['(values)', 'nil'], ['(values (+ argument 2) :extra)', '(1073741826 :extra)'],
  ]) {
    check(`(let* ((module (compile-native-module
        '((defun soundness-scalar-entry (full)
            (declare (optimize (debug 0) (safety 1) (speed 3) (space 1)))
            (if full (soundness-scalar-leaf 1073741824)
                (list (soundness-scalar-leaf 1073741824))))
          (defun soundness-scalar-leaf (argument)
            (declare (ignorable argument) (optimize (debug 0) (safety 1))) ${body}))
        :entry-points '(soundness-scalar-entry) :inline nil))
        (entry (cdr (assoc 'soundness-scalar-entry module))))
      (and (equal (multiple-value-list (funcall entry t)) '${expected})
           (equal (funcall entry nil) (list (first '${expected})))))`);
  }
  for (const debug of [0, 2]) check(`(let ((package (make-package "SOUNDNESS-SCALAR-CALLS" :use nil)))
    (unwind-protect
      (let ((leaf (intern "LEAF" package)))
        (export leaf package)
        (setf (symbol-function leaf) (lambda (x) (values x :before)))
        (let* ((module (compile-native-module
                  \`((defun soundness-scalar-entry (mode)
                      (declare (optimize (debug 0) (safety 1) (speed 3) (space 1)))
                      (case mode (:escape (function ,leaf))
                        (:global (funcall ',leaf 7))
                        (otherwise (+ (,leaf 1073741824) 3))))
                    (defun ,leaf (argument)
                      (declare (optimize (debug ${debug}) (safety 1))) (+ argument 2)))
                  :entry-points '(soundness-scalar-entry) :inline nil))
               (entry (cdr (assoc 'soundness-scalar-entry module)))
               (escaped (funcall entry :escape)))
          (and (equal (multiple-value-list (funcall entry :global)) '(7 :before))
               (progn (setf (symbol-function leaf) (lambda (x) (values x :after))) t)
               (equal (multiple-value-list (funcall entry :global)) '(7 :after))
               (equal (multiple-value-list (funcall entry :direct)) '(1073741829))
               (eql (funcall escaped 0.5d0) 2.5d0)
               (= (funcall escaped 9223372036854775808) 9223372036854775810)
               (handler-case (progn (funcall escaped "invalid") nil) (type-error () t))
               (= (funcall escaped 40) 42))))
      (delete-package package)))`);

  check(await fs.readFile(new URL('./soundness-integer-flow.lisp', import.meta.url), 'utf8'));

  // A character read remains an observation across the fixed call and a
  // callback. The caller uses the old character and the live destination.
  check(`(let* ((module (compile-native-module
      '((defun soundness-character-leaf (text)
          (declare (optimize (debug 0) (safety 1)))
          (char text 0))
        (defun soundness-character-entry (text output transition)
          (declare (notinline soundness-character-leaf)
                   (optimize (debug 0) (safety 1) (speed 3) (space 1)))
          (let ((observed (soundness-character-leaf text)))
            (funcall transition)
            (setf (aref output 0) (logxor (char-code observed) 255)))
          (values nil :stored)))
      :entry-points '(soundness-character-entry) :inline nil))
    (entry (cdr (assoc 'soundness-character-entry module)))
    (text (make-array 1 :element-type 'character :initial-element (code-char #x1f642)))
    (output (make-array 1 :element-type 'fixnum :adjustable t :initial-element 0))
    (backing (make-array 3 :element-type 'fixnum :initial-contents '(11 12 13)))
    (effects 0))
    (and (equal (multiple-value-list
                 (funcall entry text output
                   (lambda ()
                     (incf effects)
                     (setf (char text 0) (code-char 0))
                     (assert (eq output (adjust-array output 1 :displaced-to backing
                                                       :displaced-index-offset 1))))))
                '(nil :stored))
         (= (aref backing 1) (logxor #x1f642 255))
         (eq :invalid (catch 'read-error
                        (handler-bind ((error (lambda (condition)
                                                (declare (ignore condition))
                                                (throw 'read-error :invalid))))
                          (funcall entry 27 output (lambda () (incf effects))))))
         (= effects 1)
         (equal (multiple-value-list (funcall entry text output (lambda () (incf effects))))
                '(nil :stored))
         (= effects 2) (= 255 (aref output 0))))`);

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
  // Closed tuple forwarders retain each escaped closure's context and full
  // value group across calls that replace the ambient multiple-value state.
  for (const count of [2, 3]) {
    const names = ['a', 'b', 'c'].slice(0, count).join(' ');
    check(`(let* ((factory (compile-native '(lambda (capture)
          (declare (optimize (debug 0)))
          (flet ((leaf (x) (values capture x ${count === 3 ? 'nil' : ''})))
            (declare (notinline leaf))
            (flet ((forward (x)
                     (multiple-value-bind (${names}) (leaf x) (values ${names}))))
              (declare (notinline forward))
              (flet ((forward2 (x)
                       (multiple-value-bind (${names}) (forward x) (values ${names}))))
                (declare (notinline forward2))
                (lambda (x effect)
                  (multiple-value-prog1 (forward2 x) (funcall effect)))))))))
        (object (list :identity))
        (left (funcall factory object)) (right (funcall factory nil))
        (events nil) (snapshot nil))
      (and (loop for effect in
            (list (lambda () (push :effect events) (values))
                  (lambda () (push :effect events) (values :noise))
                  (lambda () (push :effect events) (values 1 2 3 4 5))) always
          (loop for entry in (list (list left object nil) (list right nil object)
                                   (list left object object)) always
            (progn (setq events nil)
              (let ((result (multiple-value-list (funcall (first entry) (third entry) effect))))
                (unless snapshot (setq snapshot result))
                (and (= (length result) ${count})
                  (eq (first result) (second entry)) (eq (second result) (third entry))
                  ${count === 3 ? '(null (third result))' : 't'}
                  (equal events '(:effect)))))))
        (eq (first snapshot) object) (null (second snapshot))
        ${count === 3 ? '(null (third snapshot))' : 't'}))`);
  }
  // Ordinary DEBUG 1 entries retain their own frame contract while adapting
  // exact private results. Exercise both lexical call shapes and escaped
  // entries, including live named dispatch after the original entry is warm.
  for (const count of [2, 3]) {
    check(`(let* ((factory (compile-native '(lambda (mode capture x)
          (declare (optimize (debug 1)))
          (flet ((worker (y) (values capture y ${count === 3 ? 'nil' : ''})))
            (declare (notinline worker))
            (case mode
              (:escape #'worker)
              (:list (multiple-value-list (worker x)))
              (otherwise (worker x)))))))
        (unknown (compile-native '(lambda (function x)
          (declare (optimize (debug 1))) (funcall function x))))
        (named (compile-native '(lambda (x)
          (declare (optimize (debug 1))) (soundness-normal-tuple x))))
        (object (list :identity)) (snapshot nil))
      (unwind-protect
        (and
          (loop for entry in (list (list object nil) (list nil object) (list nil nil)) always
            (let* ((capture (first entry)) (x (second entry))
                   (expected (list capture x ${count === 3 ? 'nil' : ''}))
                   (worker (funcall factory :escape capture nil)))
              (setf (symbol-function 'soundness-normal-tuple) worker)
              (dotimes (i 8) (funcall named x))
              (let ((result (multiple-value-list (funcall named x))))
                (unless snapshot (setq snapshot result))
                (and (equal result expected)
                  (equal (multiple-value-list (funcall unknown worker x)) expected)
                  (equal (multiple-value-list (funcall factory :tail capture x)) expected)
                  (equal (funcall factory :list capture x) expected)))))
          (eq (first snapshot) object) (null (second snapshot))
          ${count === 3 ? '(null (third snapshot))' : 't'}
          (loop for replacement in
              (list (compile-native '(lambda (x) (declare (ignore x)) (values)))
                    (compile-native '(lambda (x) (declare (ignore x)) (values nil)))
                    (compile-native '(lambda (x) (values x nil :third :fourth :fifth))))
                for expected in '(nil (nil) (:changed nil :third :fourth :fifth)) always
            (progn (setf (symbol-function 'soundness-normal-tuple) replacement)
              (equal (multiple-value-list (funcall named :changed)) expected))))
        (fmakunbound 'soundness-normal-tuple)))`);
  }
  // A checked secondary result fails inside the retained named leaf, before
  // any result group is committed. Cleanup still sees its dynamic scope and
  // the same entry remains usable after unwinding across the host boundary.
  evaluate(`(defvar *soundness-normal-scope* :outside)
    (defvar *soundness-normal-cleanup* nil)
    (setf (symbol-function 'soundness-normal-checked)
      (compile-native '(lambda (x)
        (declare (inline car) (optimize (debug 1) (safety 1)))
        (values nil (car x) nil)) :name 'soundness-normal-checked))
    (setf (symbol-function 'soundness-normal-cleanup-call)
      (compile-native '(lambda (x)
        (declare (optimize (debug 1)))
        (let ((*soundness-normal-scope* :inside))
          (unwind-protect (soundness-normal-checked x)
            (push *soundness-normal-scope* *soundness-normal-cleanup*)))))) (values)`);
  try {
    assert.throws(() => evaluate('(soundness-normal-cleanup-call :invalid)'), error =>
      /SOUNDNESS-NORMAL-CHECKED/.test(error.backtrace ?? ''));
    check(`(and (eq *soundness-normal-scope* :outside)
      (equal *soundness-normal-cleanup* '(:inside))
      (equal (multiple-value-list (soundness-normal-cleanup-call '(17))) '(nil 17 nil))
      (equal *soundness-normal-cleanup* '(:inside :inside))
      (eq *soundness-normal-scope* :outside))`);
  } finally {
    evaluate("(fmakunbound 'soundness-normal-checked) (fmakunbound 'soundness-normal-cleanup-call) (values)");
  }
  console.log('Soundness: ordinary tuple entries retain complete values, live bindings, checked leaf frames and dynamic cleanup.');

  // Retained-frame recursion preserves distinct closure contexts beyond the
  // backtrace ring, and wrong-arity rejection leaves the same entry usable.
  check(`(let* ((factory (compile-native '(lambda (capture)
        (declare (optimize (debug 1)))
        (labels ((walk (n) (if (< n 1) capture (progn (walk (- n 1)) capture))))
          (declare (notinline walk)) #'walk))))
      (one (list :one)) (two (list :two))
      (left (funcall factory one)) (right (funcall factory two)))
    (and (eq (funcall left 70) one) (eq (funcall right 70) two)
      (handler-case (progn (funcall left) nil) (program-error () t))
      (eq (funcall left 70) one)))`);
  // A primary recursive use is not an exact-one-result certificate. The
  // self call observes only primary truth; every level can return zero or two.
  check(`(let ((function (compile-native '(lambda (choose capture)
        (declare (optimize (debug 1)))
        (labels ((walk (n)
                   (if (< n 1)
                       (if choose (values capture :secondary) (values))
                       (if (walk (- n 1)) (values capture :secondary) (values)))))
          (declare (notinline walk)) (values (walk 70)))))))
    (and (equal (multiple-value-list (funcall function t :one)) '(:one))
      (equal (multiple-value-list (funcall function nil :one)) '(nil))))`);
  evaluate(`(defvar *soundness-recursive-scope* :outside)
    (defvar *soundness-recursive-cleanup* nil)
    (setf (symbol-function 'soundness-framed-recursion)
      (compile-native '(lambda (input)
        (declare (optimize (debug 1) (safety 1)))
        (labels ((walk (n) (if (< n 1) (+ input 0) (+ 1 (walk (- n 1))))))
          (declare (notinline walk)) (values (walk 70))))
        :name 'soundness-framed-recursion)) (values)`);
  try {
    assert.throws(() => evaluate('(soundness-framed-recursion :invalid)'), error =>
      /:\+\n/.test(error.backtrace ?? '') &&
      (error.backtrace?.match(/:LAMBDA\b/g) ?? []).length >= 60);
    check(`(and
      (eq (catch 'recursive-exit
        (let ((*soundness-recursive-scope* :inside))
          (unwind-protect
            (handler-bind ((type-error (lambda (condition)
              (declare (ignore condition))
              (throw 'recursive-exit *soundness-recursive-scope*))))
              (soundness-framed-recursion :invalid))
            (push *soundness-recursive-scope* *soundness-recursive-cleanup*)))) :inside)
      (eq *soundness-recursive-scope* :outside)
      (equal *soundness-recursive-cleanup* '(:inside))
      (= (soundness-framed-recursion 0) 70))`);
  } finally {evaluate("(fmakunbound 'soundness-framed-recursion) (values)");}
  console.log('Soundness: framed recursive calls retain captures, primary boundaries, error frames, transfer cleanup and ring recovery.');

  await vm.mountFile('/soundness-named-recursion.lisp',
    await fs.readFile(new URL('./soundness-named-recursion.lisp', import.meta.url), 'utf8'));
  try {
    check('(load "/soundness-named-recursion.lisp")');
    assert.throws(() => evaluate('(soundness-named-frame 5 :invalid)'), error =>
      (error.backtrace?.match(/\bSOUNDNESS-NAMED-FRAME\b/g) ?? []).length >= 3);
    assert.equal(evaluate('(soundness-named-frame 5 0)'), '5\n');
  } finally {
    evaluate(`(mapc #'fmakunbound
      '(soundness-named-frame soundness-named-values soundness-named-shadow soundness-named-carrier)) (values)`);
  }
  console.log('Soundness: named recursive calls preserve binding scope, multiple values and owned frames across errors and transfers.');

  // A typed result crosses DEBUG 1 frames. An error shows the callee and
  // caller, and a transfer through the callee leaves the frame stack usable.
  evaluate(`(setf (fdefinition 'soundness-framed-scalar)
    (cdr (assoc 'soundness-framed-scalar
      (compile-native-module
        '((defun soundness-framed-leaf (text)
            (declare (optimize (debug 1) (safety 1)))
            (char text 0))
          (defun soundness-framed-scalar (text array)
            (declare (notinline soundness-framed-leaf)
                     (optimize (debug 1) (safety 1) (speed 1) (space 2)))
            (setf (aref array 0) (logxor (char-code (soundness-framed-leaf text)) 255))
            (values nil :stored)))
        :entry-points '(soundness-framed-scalar) :inline nil))))
    (values)`);
  try {
    assert.throws(() => evaluate('(soundness-framed-scalar 77 (vector 0))'), error => {
      const trace = error.backtrace ?? '';
      return /\b(?:LAMBDA|SOUNDNESS-FRAMED-LEAF)\b[^\n]*\n[^\n]*\bSOUNDNESS-FRAMED-SCALAR\b/.test(trace);
    });
    check(`(let ((output (vector :untouched)))
      (and (eq (catch 'read-transfer
                 (handler-bind ((error (lambda (condition)
                                        (declare (ignore condition))
                                        (throw 'read-transfer :transferred))))
                   (soundness-framed-scalar 77 output)))
               :transferred)
           (eq (aref output 0) :untouched)
           (equal (multiple-value-list (soundness-framed-scalar "x" output)) '(nil :stored))
           (= (aref output 0) (logxor (char-code #\\x) 255))))`);
  } finally {evaluate("(fmakunbound 'soundness-framed-scalar) (values)");}

  check(`(let* ((module
          (compile-native-module
            '((defun soundness-index-leaf (index source)
                (declare (optimize (debug 1) (safety 1)))
                (logand (aref source index) #xffffffff))
              (defun soundness-index-entry (indices source destination transition)
                (declare (optimize (debug 1) (safety 1) (speed 1) (space 2))
                         (notinline soundness-index-leaf))
                (setf (aref destination 0)
                  (logxor (soundness-index-leaf (logand (aref indices 0) 255)
                                               (progn (funcall transition) source)) 255))
                (setf (aref destination 1)
                  (logxor (soundness-index-leaf (logand (aref indices 1) 255) source) 255))
                (values :stored nil)))
            :entry-points '(soundness-index-entry) :inline nil))
         (entry (cdr (assoc 'soundness-index-entry module)))
         (indices (make-array 2 :element-type 'fixnum :initial-contents '(0 1)))
         (old (make-array 2 :element-type 'fixnum :initial-contents '(44 55)))
         (next (make-array 2 :element-type 'fixnum :initial-contents '(-1 42)))
         (source (make-array 2 :element-type 'fixnum :adjustable t :displaced-to old))
         (destination (make-array 2 :element-type 'fixnum :initial-element 0))
         (events 0)
         (transition (lambda ()
                       (incf events)
                       (setf (aref indices 0) 1)
                       (adjust-array source 2 :displaced-to next))))
     (and (equal (multiple-value-list (funcall entry indices source destination transition)) '(:stored nil))
          (= events 1)
          (= (aref destination 0) (logxor #xffffffff 255))
          (= (aref destination 1) (logxor 42 255))
          (eq (catch 'bad-index
                (handler-bind ((error (lambda (condition)
                                        (declare (ignore condition))
                                        (throw 'bad-index :transferred))))
                  (funcall entry #(99 0) source destination transition)))
              :transferred)
          (= events 2)
          (equal (multiple-value-list
                    (funcall entry (vector (+ (ash 1 100) 1) 0) source destination transition))
                 '(:stored nil))
          (= events 3)
          (= (aref destination 0) (logxor 42 255))
          (= (aref destination 1) (logxor #xffffffff 255))))`);

  check(`(let* ((module
          (compile-native-module
            '((defun soundness-joined-mask (choice indices other destination)
                (declare (optimize (debug 1) (safety 1) (speed 1) (space 2)))
                (let ((result (logand (if choice (logand (aref indices 0) 255) other) 127)))
                  (setf (aref destination 0) result)
                  (values result :stored))))
            :entry-points '(soundness-joined-mask) :inline nil))
         (entry (cdr (assoc 'soundness-joined-mask module)))
         (indices (make-array 1 :element-type 'fixnum :initial-element -1))
         (destination (make-array 1 :element-type 'fixnum :initial-element 0)))
     (and (equal (multiple-value-list (funcall entry t indices nil destination)) '(127 :stored))
          (= (aref destination 0) 127)
          (handler-case (progn (funcall entry nil indices nil destination) nil)
            (type-error () t))
          (= (aref destination 0) 127)
          (equal (multiple-value-list
                    (funcall entry nil indices (+ (ash 1 100) 42) destination))
                 '(42 :stored))
          (= (aref destination 0) 42)))`);

  // Distinct captured workers may call arbitrary callbacks and own dynamic
  // bindings. Full-value transport must retain their cleanup on every exit.
  evaluate(`(defvar *soundness-known-scope* :outside)
    (defvar *soundness-known-cleanup* nil) (values)`);
  check(`(let* ((factory (compile-native '(lambda (capture)
        (declare (optimize (space 3) (speed 1) (debug 1) (safety 1)))
        (labels ((worker (function input)
                   (let ((*soundness-known-scope* capture))
                     (unwind-protect (funcall function input)
                       (push *soundness-known-scope* *soundness-known-cleanup*)))))
          (declare (notinline worker))
          (lambda (function input) (multiple-value-list (worker function input)))))))
      (object (list :capture)) (left (funcall factory object)) (right (funcall factory nil)))
    (and
      (null (funcall left (lambda (x) (declare (ignore x)) (values)) nil))
      (equal (funcall right #'identity :one) '(:one))
      (equal (funcall left (lambda (x) (values *soundness-known-scope* x nil 4 5 6 7 8 9)) :input)
             (list object :input nil 4 5 6 7 8 9))
      (equal (multiple-value-list
        (catch 'soundness-known-exit
          (funcall left (lambda (x) (throw 'soundness-known-exit (values *soundness-known-scope* x))) :escaped)))
        (list object :escaped))
      (handler-case (progn (funcall right #'car :invalid) nil) (type-error () t))
      (handler-case (progn (funcall left #'identity) nil) (program-error () t))
      (eq *soundness-known-scope* :outside)
      (equal *soundness-known-cleanup* (list nil object object nil object))
      (equal (funcall left #'identity :recovered) '(:recovered))
      (eq *soundness-known-scope* :outside)))`);
  console.log('Soundness: nonrecursive fixed calls retain captured workers, full values and callee-owned dynamic cleanup.');

  // Scalar forwarders keep each closure's capture and the primary-value
  // boundary, including checked effects in a discarded secondary value.
  check(`(let* ((factory (compile-native '(lambda (capture)
        (declare (optimize (debug 1)))
        (labels ((leaf (choose input)
                   (declare (inline car) (optimize (debug 0) (safety 1)))
                   (if choose (values capture (car input)) (values)))
                 (forward (choose input)
                   (declare (optimize (debug 0))) (values (leaf choose input)))
                 (forward2 (choose input)
                   (declare (optimize (debug 0))) (values (forward choose input))))
          (declare (notinline leaf forward forward2))
          (lambda (choose input observe)
            (multiple-value-prog1 (values (forward2 choose input)) (funcall observe)))))))
      (object (list :identity)) (left (funcall factory object)) (right (funcall factory nil))
      (events nil))
    (flet ((observe () (push :effect events) (values :noise :secondary :third)))
      (and
        (let ((result (multiple-value-list (funcall left t '(17) #'observe))))
          (and (null (cdr result)) (eq (car result) object)))
        (equal (multiple-value-list (funcall right t '(19) #'observe)) '(nil))
        (equal (multiple-value-list (funcall left nil :unused #'observe)) '(nil))
        (equal events '(:effect :effect :effect))
        (handler-case (progn (funcall left t :invalid #'observe) nil)
          (type-error (condition) (eq (type-error-datum condition) :invalid)))
        (equal events '(:effect :effect :effect))
        (eq (funcall left t '(23) #'observe) object))))`);
  evaluate(`(defvar *soundness-scalar-scope* :outside)
    (defvar *soundness-scalar-cleanup* nil)
    (setf (symbol-function 'soundness-scalar-root)
      (compile-native '(lambda (input)
        (declare (optimize (debug 1)))
        (labels ((leaf (x) (declare (inline car) (optimize (debug 0) (safety 1))) (car x))
                 (forward (x) (declare (optimize (debug 0))) (values (leaf x)))
                 (forward2 (x) (declare (optimize (debug 0))) (values (forward x))))
          (declare (notinline leaf forward forward2))
          (let ((*soundness-scalar-scope* :inside))
            (unwind-protect (list (forward2 input) :done)
              (push *soundness-scalar-scope* *soundness-scalar-cleanup*)))))
        :name 'soundness-scalar-root)) (values)`);
  try {
    assert.throws(() => evaluate('(soundness-scalar-root :invalid)'), error =>
      /SOUNDNESS-SCALAR-ROOT/.test(error.backtrace ?? ''));
    check(`(and (eq *soundness-scalar-scope* :outside)
      (equal *soundness-scalar-cleanup* '(:inside))
      (equal (soundness-scalar-root '(17)) '(17 :done))
      (equal *soundness-scalar-cleanup* '(:inside :inside))
      (eq *soundness-scalar-scope* :outside))`);
  } finally {
    evaluate("(fmakunbound 'soundness-scalar-root) (values)");
  }
  console.log('Soundness: scalar closed chains retain captures, primary boundaries, checked effects and ancestor cleanup/frame.');
  // A wide valid call to a known small callee keeps its runtime arity error;
  // rejecting its private-call proof must leave other invocations reusable.
  check(`(let ((function (compile-native '(lambda (bad)
      (declare (optimize (debug 0)))
      (labels ((leaf (x) x)
               (wide () (values (leaf ${Array.from({length: 128}, (_, index) => index).join(' ')}))))
        (declare (notinline leaf wide))
        (if bad (wide) (leaf :ok)))))))
    (and (eq (funcall function nil) :ok)
      (handler-case (progn (funcall function t) nil)
        (program-error (condition) (plusp (length (princ-to-string condition)))))
      (eq (funcall function nil) :ok)))`);

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

  // Trimming private secondary results can make a formal newly unused. Its
  // caller operands still run in order, and a checked secondary still signals.
  for (const discard of [false, true]) {
    const call = `(worker (funcall effect :kept)
      (progn (funcall effect :checked) bad) (funcall effect :unused))`;
    check(`(let ((function (compile-native '(lambda (bad effect)
          (declare (optimize (debug 0) (safety 1)))
          (flet ((worker (kept checked unused)
                   (declare (inline car cons) (optimize (debug 0) (safety 1)))
                   (values kept (car checked) (cons unused unused))))
            (declare (notinline worker))
            ${discard ? `(progn ${call} 42)` : `(values ${call})`})))))
      (labels ((run (bad failure)
                 (let ((events nil))
                   (handler-case
                     (values (funcall function bad (lambda (event)
                               (push event events)
                               (when (eq event failure) (error "Private operand failure"))
                               17))
                       (reverse events) nil)
                     (error (condition) (values nil (reverse events) condition)))))
               (recover ()
                 (multiple-value-bind (answer events condition) (run '(18) nil)
                   (and (= answer ${discard ? 42 : 17})
                     (equal events '(:kept :checked :unused)) (null condition)))))
        (and (recover)
          (loop for failure in '(:kept :checked :unused)
                for prefix in '((:kept) (:kept :checked) (:kept :checked :unused)) always
            (multiple-value-bind (answer events condition) (run '(18) failure)
              (and (null answer) (equal events prefix) (typep condition 'simple-error))))
          (multiple-value-bind (answer events condition) (run :invalid nil)
            (and (null answer) (equal events '(:kept :checked :unused))
              (typep condition 'type-error)))
          (recover))))`);
  }

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
  // A private literal cursor retains proper suffixes across control flow and
  // effects. Early exits keep sharing and the complete multiple-value result.
  check(`(let* ((source '(lambda (stop)
        (declare (optimize (safety 1) (debug 0)))
        (let* ((cursor '(a b c)) (original cursor) (seen nil))
          (block done
            (loop while cursor do
              (when (eq stop :zero) (return-from done (values)))
              (when (eq (car cursor) stop)
                (return-from done (values cursor (nreverse seen) original)))
              (push (car cursor) seen)
              (setq cursor (cdr cursor)))
            (values cursor (nreverse seen) original)))))
      (function (compile-native source))
      (early (multiple-value-list (funcall function 'b)))
      (complete (multiple-value-list (funcall function :absent))))
    (and (hir-type-proves-p (soundness-proven-result source) 'list)
      (equal early '((b c) (a) (a b c)))
      (eq (first early) (cdr (third early)))
      (equal complete '(nil (a b c) (a b c)))
      (null (multiple-value-list (funcall function :zero)))))`);
  // One arbitrary replacement invalidates the suffix invariant. Dotted tails
  // remain observable; invalid CAR/CDR arguments keep checked Lisp failures.
  check(`(let* ((source '(lambda (replacement)
        (declare (optimize (safety 1) (debug 0)))
        (let ((cursor '(a b c)))
          (setq cursor replacement) (setq cursor (cdr cursor)) cursor)))
      (function (compile-native source)))
    (and (not (hir-type-proves-p (soundness-proven-result source) 'list))
      (eq (funcall function '(a . :tail)) :tail)
      (handler-case (progn (funcall function :invalid) nil) (type-error () t))
      (equal (funcall function '(a b)) '(b))))`);
  // The bounded proof inspection must accept legal dotted and cyclic quoted
  // data without trying to establish a proper-spine invariant for it.
  check(`(and
    (eq (funcall (compile-native '(lambda ()
      (declare (optimize (safety 1) (debug 0)))
      (let ((cursor '(a . :tail))) (setq cursor (cdr cursor)) cursor)))) :tail)
    (equal (multiple-value-list (funcall (compile-native '(lambda ()
      (declare (optimize (safety 1) (debug 0)))
      (let ((cursor '#1=(a . #1#)))
        (dotimes (i 3) (setq cursor (cdr cursor)))
        (values (eq cursor (cdr cursor)) (car cursor))))))) '(t a)))`);
  // Literal traversals bound integer counters in both ordinary and executable
  // inference. Mixed steps and LOOP's final increment remain observable.
  for (const [body, expected, result] of [
    ["(let ((i 0)) (dolist (x '(a b c) i) (incf i)))", '(integer 0 16)', 3],
    ["(loop for x in '(a b c) for i from 0 finally (return i))", '(integer 0 16)', 3],
    ["(let ((i 5)) (dolist (x '(a b c) i) (decf i 2) (incf i)))", '(integer -16 16)', 2],
    ["(let ((i 5)) (dolist (x '() i) (incf i)))", '(integer 0 16)', 5],
  ]) check(`(let* ((source '(lambda ()
        (declare (optimize (safety 1) (debug 0))) ${body}))
      (unit (compile-to-hir source)) (owner (first (hir-unit-functions unit)))
      (ordinary (infer-hir-types unit)))
    (and (hir-type-proves-p (gethash owner ordinary t) '${expected})
      (hir-type-proves-p (soundness-proven-result source) '${expected})
      (= (funcall (compile-native source)) ${result})))`);
  // Scalar private storage preserves a read observed after a later write,
  // including integer representation boundaries and a bignum fallback.
  for (const initial of ['0', '1073741822', '2147483646', '9223372036854775806']) {
    check(`(equal (multiple-value-list (funcall (compile-native '(lambda ()
        (declare (optimize (safety 1) (debug 0)))
        (let ((i ${initial}) (seen nil))
          (dolist (x '(a b c))
            (let ((before i)) (incf i) (push before seen)))
          (values i (nreverse seen)))))))
      (list (+ ${initial} 3) (list ${initial} (+ ${initial} 1) (+ ${initial} 2))))`);
  }
  // Speed-oriented unit steps can share private tiered storage.
  // Both that path and compact checked calls preserve snapshots, generic
  // numeric fallback and recovery after rejection.
  for (const policy of ['(speed 1) (space 3)', '(speed 3) (space 1)']) {
    for (const operator of ['+', '-', '1+', '1-']) {
      const update = value => `(${operator} ${value}${operator === '+' || operator === '-' ? ' 1' : ''})`;
      check(`(let ((function (compile-native '(lambda (x observe)
        (declare (optimize ${policy} (safety 1) (debug 0)))
        (dotimes (i 3 x)
          (let ((before x)) (setq x ${update('x')}) (funcall observe before x)))))))
      (and
        (loop for initial in '(0 1073741823 -1073741824 9223372036854775807
                              -9223372036854775808 1267650600228229401496703205376
                              -1267650600228229401496703205376 1/2 0.5d0 #c(1 2)) always
          (let ((expected initial) (valid t))
            (and (= (funcall function initial (lambda (before after)
                         (unless (and (= before expected) (= after ${update('expected')}))
                           (setq valid nil))
                         (setq expected ${update('expected')}))) expected)
                 valid)))
        (handler-case (progn (funcall function :invalid (lambda (a b) (declare (ignore a b)))) nil)
          (type-error () t))
        (= (funcall function 10 (lambda (a b) (declare (ignore a b)))) ${update(update(update('10')))})))`);
    }
  }
  // A repeated body GO and an unknown write must not inherit the traversal's
  // counter envelope. Errors still leave the compiled function reusable.
  check(`(let* ((source '(lambda (replacement)
        (declare (optimize (safety 1) (debug 0)))
        (let ((i 0))
          (dolist (x '(a b c) i) (setq i replacement) (incf i)))))
      (function (compile-native source)))
    (and (not (hir-type-proves-p (soundness-proven-result source) 'integer))
      (= (funcall function 0.5d0) 1.5d0)
      (handler-case (progn (funcall function :invalid) nil) (type-error () t))
      (= (funcall function 100) 101)))`);
  check(`(let* ((source '(lambda ()
        (declare (optimize (safety 1) (debug 0)))
        (let ((i 0)) (dolist (x '(a b c) i)
          (tagbody again (incf i) (when (< i 20) (go again)))))))
      (function (compile-native source)))
    (and (not (hir-type-proves-p (soundness-proven-result source) '(integer 0 16)))
      (= (funcall function) 22)))`);
  // Bounded recurrences retain executable finite ranges without declarations.
  // Check useful bounds rather than the widening ladder's exact thresholds.
  for (const [test, update, expected] of [
    ['(< i 100)', '(incf i)', '(integer 0 2147483647)'],
    ['(> i -100)', '(decf i)', '(integer -2147483648 0)'],
  ]) check(`(hir-type-proves-p
    (soundness-proven-result '(lambda ()
      (declare (optimize (safety 1) (debug 0)))
      (let ((i 0)) (loop while ${test} do ${update}) i))) '${expected})`);
  // Every write participates in the recurrence. Unknown replacement values
  // must retain floating-point arithmetic and its ordinary checked failures.
  check(`(let* ((source '(lambda (replacement)
        (declare (optimize (safety 1) (debug 0)))
        (let ((i 0))
          (loop while (< i 100) do
            (when (= i 0) (setq i replacement)) (incf i)) i)))
      (function (compile-native source)))
    (and (not (hir-type-proves-p (soundness-proven-result source) 'integer))
      (= (funcall function 0.5d0) 100.5d0)
      (handler-case (progn (funcall function :invalid) nil) (type-error () t))
      (= (funcall function 1000) 1001)))`);
  // Standard result facts remain valid for out-of-line calls. Result counts
  // are independent of primary types and remain unknown for forwarded values.
  check(`(with-compilation-statistics (statistics)
    (let ((function (compile-native '(lambda (value)
      (declare (optimize (safety 3) (debug 0)))
      (if (typep value '(integer 0 10)) (values (+ value 1) :inside)
          (values 0 :outside))))))
      (and (equal (multiple-value-list (funcall function 7)) '(8 :inside))
        (equal (multiple-value-list (funcall function -1)) '(0 :outside))
        (let ((rows (compilation-statistics-report statistics)))
          (and (assoc :proof/branch-premise rows)
            (every (lambda (row)
              (and (keywordp (first row))
                   (or (symbolp (second row)) (integerp (second row)))
                   (typep (third row) '(integer 0 *)))) rows))))))`);
  check(`(and
    (hir-type-proves-p
      (soundness-proven-result '(lambda (x) (declare (notinline abs)) (abs x))) 'real)
    (hir-type-proves-p
      (soundness-proven-result '(lambda (x) (declare (notinline reverse)) (reverse x))) 'sequence)
    (loop for source in
        '((lambda (x) (declare (notinline abs)) (abs x))
          (lambda (x) (declare (notinline floor)) (floor x))
          (lambda (function x) (funcall function x))
          (lambda (arguments) (values-list arguments)))
      for expected in '(1 2 nil nil) always
        (let* ((unit (compile-to-hir source))
               (facts (hir-result-arities-checked unit)))
          (eql expected (gethash (first (hir-unit-functions unit)) facts)))))`);
  // A private literal bytespec bounds a successful field result. An escaped
  // bytespec and a dynamic width keep the ordinary full-width path.
  check(`(let ((compare (compile-native '(lambda (x)
                    (declare (optimize (safety 1) (debug 1)))
                    (< (ldb (byte 32 0) x) #x80000000))))
               (dynamic (compile-native '(lambda (width x)
                    (declare (optimize (safety 1) (debug 1)))
                    (< (ldb (byte width 0) x) #x80000000)))))
    (and (hir-type-proves-p
           (soundness-proven-result '(lambda (x) (ldb (byte 32 0) x))) '(unsigned-byte 32))
         (not (hir-type-proves-p
           (soundness-proven-result '(lambda (x observe)
                (let ((spec (byte 32 0))) (funcall observe spec) (ldb spec x)))) '(unsigned-byte 32)))
         (equal (multiple-value-list (funcall compare #x7fffffff)) '(t))
         (equal (multiple-value-list (funcall compare #x80000000)) '(nil))
         (equal (multiple-value-list (funcall compare (+ (ash 1 100) 1))) '(t))
         (equal (multiple-value-list (funcall dynamic 100 (ash 1 99))) '(nil))
         (handler-case (progn (funcall compare :invalid) nil) (type-error () t))
         (equal (multiple-value-list (funcall compare -1)) '(nil))))`);
  // Canonical numeric predicates refine unbounded and bignum integer ranges;
  // their truth facts must not depend on a scalar Wasm representation.
  for (const type of ['integer', '(integer -1267650600228229401496703205376 1267650600228229401496703205376)']) {
    for (const [predicate, alternate, expected] of [
      ['plusp', '1', '(integer 1 *)'],
      ['minusp', '-1', '(integer * -1)'],
      ['zerop', '0', '(integer 0 0)'],
    ]) {
      check(`(let* ((unit (compile-to-hir
          '(lambda (x)
             (declare (type ${type} x) (notinline ${predicate})
                      (optimize (safety 0) (debug 0)))
             (if (${predicate} x) x ${alternate}))))
          (facts (infer-hir-types unit)))
        (hir-type-proves-p (gethash (first (hir-unit-functions unit)) facts t) '${expected}))`);
    }
  }
  // Executed integer predicates, rather than declarations, justify rounding
  // ordered bounds. Both arms and operand orientations include bignum limits.
  check(`(and
    (loop for (left right expected) in
        '(((integer * 9) integer (integer * 9))
          ((integer 4 *) integer (integer 4 *))
          ((integer * 9) (integer 4 *) (integer 4 9))) always
      (and (hir-type-proves-p (hir-type-restrict left right) expected)
           (hir-type-proves-p (hir-type-restrict right left) expected)))
    (null (hir-type-restrict '(integer 10 *) '(integer * 9)))
    (null (hir-type-restrict '(integer * 9) '(integer 10 *))))`);
  for (const threshold of [
    10n, -1073741825n, -1073741824n, 1073741823n, 1073741824n,
    9223372036854775807n, -9223372036854775808n,
  ]) {
    for (const [operator, upper, inclusive] of [
      ['<', true, false], ['<=', true, true],
      ['>', false, false], ['>=', false, true],
    ]) {
      for (const positive of [true, false]) {
        const bound = positive
          ? threshold + (inclusive ? 0n : upper ? -1n : 1n)
          : threshold + (inclusive ? upper ? 1n : -1n : 0n);
        const expected = (positive ? upper : !upper)
          ? `(integer * ${bound})` : `(integer ${bound} *)`;
        const reversed = { '<': '>', '<=': '>=', '>': '<', '>=': '<=' }[operator];
        for (const test of [`(${operator} x ${threshold})`, `(${reversed} ${threshold} x)`]) {
          check(`(hir-type-proves-p
            (soundness-proven-result
              '(lambda (x)
                 (declare (notinline ${operator} ${reversed})
                          (optimize (safety 1) (debug 0)))
                 (if (integerp x)
                     (if ${test} ${positive ? 'x' : bound} ${positive ? bound : 'x'})
                     ${bound}))) '${expected})`);
        }
      }
    }
  }
  // A declaration cannot supply executable integer membership to a proof.
  check(`(not (hir-type-proves-p
    (soundness-proven-result
      '(lambda (x)
         (declare (type integer x) (optimize (safety 1) (debug 0)))
         (if (< x 10) x 9))) '(integer * 9)))`);
  // Observed comparisons cannot transfer a range to a later cell value.
  // Keep threshold effects, primary extraction and saved predicate identity.
  for (const body of [
    '(if (< x (progn (setq x 20) (values 10 :extra))) (< x 10) t)',
    '(let ((test (< x 10))) (setq x 20) (if test (< x 10) t))',
    '(if (not (>= x 10)) (progn (setq x 20) (< x 10)) t)',
    '(if (< x 10) (progn (funcall (lambda () (setq x 20))) (< x 10)) t)',
    '(if (< x 10) (progn (if flag (setq x 20) (setq x 30)) (< x 10)) t)',
  ]) {
    check(`(let ((function (compile-native
        '(lambda (flag)
           (declare (optimize (safety 0) (debug 0)))
           (let ((x 1)) ${body})))))
      (and (null (funcall function nil)) (null (funcall function t))))`);
  }
  // Comparisons against integers do not establish integer membership for
  // rational/float inputs, and must retain operand effects and checked errors.
  for (const safety of [0, 1, 3]) {
    check(`(let ((function (compile-native
        '(lambda (x effect)
           (declare (optimize (safety ${safety}) (debug 0)))
           (if (< x (progn (funcall effect) (values 10 :extra)))
               (values (<= x 9) x) (values nil x))))) (effects 0))
      (and (equal (multiple-value-list
          (funcall function 19/2 (lambda () (incf effects)))) '(nil 19/2))
        (equal (multiple-value-list
          (funcall function 9.5d0 (lambda () (incf effects)))) '(nil 9.5d0))
        (handler-case
          (progn (funcall function :invalid (lambda () (incf effects))) nil)
          (type-error () t))
        (equal (multiple-value-list
          (funcall function 7 (lambda () (incf effects)))) '(t 7))
        (= effects 4)))`);
  }
  // Executed bounds can narrow arithmetic while retaining checked entry facts.
  check(`(let ((function (compile-native
      '(lambda (x)
         (declare (optimize (safety 0) (debug 0)))
         (if (integerp x)
             (if (>= x 0)
                 (if (< x 10) (values (+ x 1) :small) (values x :large))
                 (values x :negative))
             (values x :other))))))
    (and (equal (multiple-value-list (funcall function 7)) '(8 :small))
         (equal (multiple-value-list (funcall function 1073741824)) '(1073741824 :large))
         (equal (multiple-value-list (funcall function -1)) '(-1 :negative))
         (equal (multiple-value-list (funcall function 7.5d0)) '(7.5d0 :other))))`);
  // Default-safety scalar chains retain snapshots, multiple values and
  // canonical promotion when an intermediate leaves the immediate range.
  for (const declaration of ['', '(notinline + - 1+ 1-)']) {
    check(`(let ((function (compile-native
        '(lambda (x)
           (declare ${declaration} (optimize (safety 1) (debug 0)))
           (let* ((a (logand x 1023))
                  (b (+ a 1073741823))
                  (c (- b 1073741824)))
             (values b (1+ (1- c)) a))))))
      (and (loop for x in '(0 1 1023 -1 1267650600228229401496703205375) always
          (let ((a (logand x 1023)))
            (equal (multiple-value-list (funcall function x))
              (list (+ a 1073741823) (- a 1) a))))
        (handler-case (progn (funcall function :invalid) nil) (type-error () t))
        (equal (multiple-value-list (funcall function 7)) '(1073741830 6 7))))`);
  }
  // Ordered comparisons consume saved arithmetic results across later writes
  // and callbacks, retaining values when the result crosses the i31 boundary.
  for (const operator of ['<', '<=', '>', '>=']) {
    check(`(let ((function (compile-native
        '(lambda (x effect)
           (declare (optimize (safety 1) (debug 0)))
           (let ((a (+ (logand x 1023) 1073741823)))
             (setq x 0)
             (funcall effect)
             (values (${operator} a 1073741824) (- a 1073741823) x)))))
        (inputs '(0 1 1023 -1 1267650600228229401496703205375)) (effects 0))
      (and (loop for x in inputs always
          (let ((low (logand x 1023)))
            (equal (multiple-value-list
                (funcall function x (lambda () (incf effects) (values :extra :ignored))))
              (list (${operator} (+ low 1073741823) 1073741824) low 0))))
        (= effects (length inputs))
        (handler-case
          (progn (funcall function :invalid (lambda () (incf effects))) nil)
          (type-error () t))
        (= effects (length inputs))
        (equal (multiple-value-list (funcall function 1 (lambda () (incf effects))))
          (list (${operator} 1073741824 1073741824) 1 0))
        (= effects (1+ (length inputs)))))`);
  }
  // Dynamic integer bounds justify the final strict update, while inclusive
  // guards at the scalar endpoints still require exact bignum promotion.
  for (const [operator, initial, limit, step, expected, reversed] of [
    ['<', '9223372036854775806', '9223372036854775807', '1+', '9223372036854775807'],
    ['<=', '9223372036854775806', '9223372036854775807', '1+', '9223372036854775808'],
    ['>', '-9223372036854775807', '-9223372036854775808', '1-', '-9223372036854775808'],
    ['>=', '-9223372036854775807', '-9223372036854775808', '1-', '-9223372036854775809'],
    ['>', '9223372036854775806', '9223372036854775807', '1+', '9223372036854775807', true],
    ['<', '-9223372036854775807', '-9223372036854775808', '1-', '-9223372036854775808', true],
  ]) {
    check(`(let ((function (compile-native
        '(lambda (limit)
           (declare (type fixnum limit)
             (optimize (space 3) (speed 1) (safety 1) (debug 1)))
           (let ((index ${initial}))
             (loop while (${operator} ${reversed ? 'limit index' : 'index limit'})
               do (setq index (${step} index)))
             (values index :done))))))
      (equal (multiple-value-list (funcall function ${limit})) '(${expected} :done)))`);
  }
  check(`(let ((function (compile-native
      '(lambda (count)
         (declare (type fixnum count)
           (optimize (space 3) (speed 1) (safety 1) (debug 1)))
         (dotimes (index count (values index :done)) nil)))))
    (and (equal (multiple-value-list (funcall function -3)) '(0 :done))
      (equal (multiple-value-list (funcall function 0)) '(0 :done))
      (equal (multiple-value-list (funcall function 7)) '(7 :done))))`);
  // Statistics distinguish accepted plans from emitted arithmetic. Checked
  // fixed calls remain available under NOTINLINE; expansion honors its veto.
  check(`(with-compilation-statistics (statistics)
    (let ((function (compile-native
        '(lambda (x)
           (declare (optimize (safety 1) (debug 0)))
           (1+ (logand x 1023))))))
      (and (= (funcall function 7) 8)
        (let ((rows (compilation-statistics-report statistics)))
          (and (assoc :proof/integer-arithmetic-i32 rows)
               (not (null (assoc :optimization/integer-arithmetic-i32 rows))))))))`);
  check(`(with-compilation-statistics (statistics)
    (let ((function (compile-native
        '(lambda (x)
           (declare (notinline + - 1+ 1-) (optimize (safety 1) (debug 0)))
           (let ((a (logand x 1023))) (1+ (- (+ a 17) 3)))))))
      (and (= (funcall function 7) 22)
        (not (assoc :proof/integer-arithmetic-i32
               (compilation-statistics-report statistics))))))`);
  // A proved integer range is independent of storage representation. Outside
  // the range, ordinary arithmetic preserves floating point and large values.
  check(`(let ((function (compile-native
      '(lambda (x)
         (declare (optimize (safety 1) (debug 0)))
         (if (and (integerp x) (>= x -2147483647) (<= x 2147483646))
             (values (+ x 1) :bounded) (values (+ x 1) :general))))))
    (and (loop for x in '(-2147483647 -1073741824 0 1073741823 2147483646) always
        (equal (multiple-value-list (funcall function x)) (list (+ x 1) :bounded)))
      (loop for x in '(-2147483648 2147483647 9223372036854775807 1/2 0.5d0) always
        (equal (multiple-value-list (funcall function x)) (list (+ x 1) :general)))
      (handler-case (progn (funcall function :invalid) nil) (type-error () t))
      (equal (multiple-value-list (funcall function 4)) '(5 :bounded))))`);
  // A bounded operand does not prove a bounded arithmetic result. The last
  // increment can leave signed i32 and must retain the ordinary exact path.
  check(`(let ((function (compile-native
      '(lambda (x)
         (declare (optimize (safety 1) (debug 0)))
         (+ (logand x 2147483647) 1)))))
    (and (= (funcall function 2147483647) 2147483648)
         (= (funcall function 0) 1)))`);
  // Later writes cannot change raw SSA observations or inherit old facts.
  check(`(let ((function (compile-native
      '(lambda (value effect)
         (declare (optimize (safety 1) (debug 0)))
         (let* ((x (logand value 15)) (saved (+ x 1)))
           (setq x (funcall effect))
           (values saved (+ x 1)))))) (effects 0))
    (and (equal (multiple-value-list
        (funcall function 7 (lambda () (incf effects) 0.5d0))) '(8 1.5d0))
      (= effects 1)))`);
  // The final update may leave i64 even when the executed loop guard fits.
  check(`(let ((function (compile-native
      '(lambda (x)
         (declare (type integer x) (inline + <=)
                  (optimize (safety 0) (debug 0)))
         (loop while (<= x 9223372036854775807) do (setq x (+ x 1)))
         (values x :done)))))
    (equal (multiple-value-list (funcall function 9223372036854775806))
      '(9223372036854775808 :done)))`);
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
  await vm.mountFile('/soundness-integer-recurrence.lisp',
    await fs.readFile(new URL('./soundness-integer-recurrence.lisp', import.meta.url), 'utf8'));
  check('(load "/soundness-integer-recurrence.lisp")');
  evaluate(`(deftype soundness-opaque () (error "Proof expanded an opaque DEFTYPE"))
    (defun soundness-predicate (value) (declare (ignore value))
      (error "Proof executed a SATISFIES predicate")) (values)`);
  check(`(and (not (hir-type-proves-p 'soundness-opaque 'integer))
    (not (hir-type-proves-p '(satisfies soundness-predicate) 'integer))
    (hir-type-proves-p '(and integer (satisfies soundness-predicate)) 'integer))`);
  // Query inference must leave opaque runtime work alone. These callbacks
  // deliberately fail if compilation tries to answer the query itself.
  for (const query of [
    "(typep value 'soundness-opaque)",
    "(typep value '(satisfies soundness-predicate))",
    "(subtypep 'soundness-opaque 'integer)",
    "(subtypep '(satisfies soundness-predicate) 'integer)",
  ]) {
    check(`(let ((function (compile-native '(lambda (value)
        (declare (optimize (safety 3) (debug 0))) ${query}))))
      (and (functionp function)
        (let ((type (soundness-proven-result '(lambda (value) ${query}))))
          (and (not (hir-type-proves-p type 'null))
               (not (hir-type-proves-p type '(not null)))))))`);
  }
  // Quoted type operands bypass declaration parsing. A malformed spine or
  // cycle must still finish inference without providing a branch proof.
  check(`(let ((cycle (list 'or 'integer)) (nested 'integer))
    (setf (cddr cycle) cycle)
    (dotimes (i 20) (setq nested (list 'not nested)))
    (and (every (lambda (type)
      (handler-case
        (let ((result (soundness-proven-result
              (list 'lambda '(value) (list 'typep 'value (list 'quote type))))))
          (and (not (hir-type-proves-p result 'null))
               (not (hir-type-proves-p result '(not null)))))
        (error (condition)
          (let ((*print-circle* t)) (plusp (length (princ-to-string condition)))))))
      (list cycle nested '(integer . 3) '(integer (0 1) 5)
            '(not integer integer) '(mod 0)))
      (= (funcall (compile-native '(lambda (value) (+ value 1))) 41) 42)))`);
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
          (declare (type ${type} value)
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
  // The compound shift and field extraction must preserve both operand
  // snapshots and the field across subsequent calls and value transport.
  check(`(let ((calls 0)
        (function (compile-native '(lambda (value count suffix)
          (declare (optimize (safety 1) (debug 0)))
          (let ((field (ldb (byte 63 0) (ash value count))))
            (funcall suffix field value count field))))))
      (flet ((record (field value count again)
               (incf calls)
               (values field value count again)))
        (and (equal (multiple-value-list (funcall function 2147483649 1 #'record))
                    '(4294967298 2147483649 1 4294967298))
             (handler-case (progn (funcall function :value :count #'record) nil)
               (type-error (condition) (eq (type-error-datum condition) :value)))
             (handler-case (progn (funcall function 0 :count #'record) nil)
               (type-error (condition) (eq (type-error-datum condition) :count)))
             (equal (multiple-value-list (funcall function 1 63 #'record))
                    '(0 1 63 0))
             (= calls 2))))`);
  // A private literal BYTE can feed LDB without changing suffix values or
  // the checked behavior of a discarded extraction.
  check(`(let ((field (compile-native '(lambda (value)
                 (declare (optimize (debug 0)))
                 (values (ldb (byte 13 64) value) value))))
               (discard (compile-native '(lambda (value)
                 (declare (optimize (debug 0)))
                 (ldb (byte 0 17) value)
                 :done))))
      (and (equal (multiple-value-list (funcall field -1)) '(8191 -1))
           (equal (multiple-value-list (funcall field 9223372036854775807))
                  '(0 9223372036854775807))
           (equal (multiple-value-list (funcall field 115292150460684697600))
                  '(6 115292150460684697600))
           (handler-case (progn (funcall discard :wrong-type) nil)
             (type-error (condition) (eq (type-error-datum condition) :wrong-type)))
           (eq (funcall discard 17) :done)))`);
  // The shared deposit worker preserves the order of checked errors and
  // leaves the native caller usable after a miss, including at width zero.
  for (const [operation, expected] of [['dpb', 42333], ['deposit-field', 93]]) {
    check(`(let ((deposit (compile-native '(lambda (replacement spec destination)
                   (${operation} (values replacement :extra) spec destination))))
                 (fixed (compile-native '(lambda (replacement spec destination)
                   (values (${operation} replacement spec destination) :kept)))))
      (flet ((reject (replacement spec destination datum expected-type)
               (handler-case (progn (funcall deposit replacement spec destination) nil)
                 (type-error (condition)
                   (and (eq (type-error-datum condition) datum)
                        (eq (type-error-expected-type condition) expected-type))))))
        (and (reject :replacement :spec :destination :destination 'integer)
             (reject :replacement :spec 0 :replacement 'integer)
             (reject 1 :spec 0 :spec 'list)
             (reject 1 nil 0 nil 'unsigned-byte)
             (reject :replacement (byte 0 7) 0 :replacement 'integer)
             (equal (multiple-value-list (funcall deposit 165 (byte 8 8) 93))
                    '(${expected}))
             (equal (multiple-value-list (funcall fixed 165 (byte 8 8) 93))
                    '(${expected} :kept))
             (equal (multiple-value-list (funcall fixed 0 (byte 4 60) -1))
                    '(-17293822569102704641 :kept)))))`);
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
            (declare (optimize (safety ${safety}) (debug 0)))
            (if (consp value) :wrong
              (progn (funcall effect)
                (unless (consp value) ${change})
                (if (consp value) (values :changed nil :extra) :wrong)))))))
        (and (equal (multiple-value-list
            (funcall function :old (lambda () (incf effects))))
          '(:changed nil :extra)) (= effects 1)))`);
    }
    check(`(let ((function (compile-native '(lambda (value observe)
      (declare (optimize (safety ${safety}) (debug 0)))
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
            (declare (optimize (space ${space}) (speed ${speed}) (safety ${safety}) (debug ${debug})))
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
    // A useful TYPE-ERROR may identify the whole bad input or its dotted tail.
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
                 (and (type-error-expected-type condition)
                   (or (eq (type-error-datum condition) arguments)
                     ${mutation === '(rplacd alias arguments)' ? 'nil' : '(eq (type-error-datum condition) :dotted)'}))))
          (progn (rplacd arguments nil)
            (equal (apply (lambda (&rest result) result) arguments) '(1)))))`);
    }
    check(`(let ((function (compile-native '(lambda (arguments)
        (declare (type list arguments) (optimize (safety ${safety}) (debug 0)))
        (apply (lambda (&rest result) result) arguments)))))
      (let ((arguments (cons 1 :dotted)))
        (handler-case (progn (funcall function arguments) nil)
          (type-error (condition)
            (and (type-error-expected-type condition)
              (or (eq (type-error-datum condition) arguments)
                  (eq (type-error-datum condition) :dotted)))))))`);
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
  // Ordinary standard calls keep checked boundaries and operand effects when
  // composed, including the debugger's retained-code configuration.
  for (const debug of [0, 2]) {
    check(`(let ((character (compile-native '(lambda (object index effect)
          (declare (optimize (safety 3) (debug ${debug})))
          (values (char-code (char (progn (funcall effect :string) object)
                                  (progn (funcall effect :index) index))) :extra))))
        (head (compile-native '(lambda (object effect)
          (declare (optimize (safety 3) (debug ${debug})))
          (values (car (progn (funcall effect :list) object)) :extra))))
        (string (make-array 2 :element-type 'character :fill-pointer 0
                   :initial-contents "ab"))
        (events nil))
      (labels ((effect (tag) (push tag events)))
        (and (equal (multiple-value-list (funcall character string 1 #'effect))
                    (list (char-code #\\b) :extra))
          (equal events '(:index :string))
          (progn (setq events nil)
            (handler-case (progn (funcall character :not-a-string 0 #'effect) nil)
              (type-error () (member :string events))))
          (progn (setq events nil)
            (handler-case (progn (funcall head :not-a-list #'effect) nil)
              (type-error () (equal events '(:list)))))
          (equal (multiple-value-list (funcall head (cons :head :tail) #'effect))
                 '(:head :extra))
          (equal (multiple-value-list (funcall head nil #'effect)) '(nil :extra))
          (equal (multiple-value-list (funcall character string 1 #'effect))
                 (list (char-code #\\b) :extra)))))`);
  }
  // Only exact CL symbols have standard result facts. Shadowed names keep
  // live user dispatch, complete values and captured function identities.
  evaluate(`(defpackage :soundness-shadow (:use :cl)
    (:shadow :consp :eq :evenp :svref :list :abs :class-name)) (values)`);
  // Conditional operation facts need explicit domains and independent
  // obligations. An opaque type query must never execute user code.
  evaluate(`(deftype soundness-query-opaque ()
    (error "Proof query expanded an opaque type")) (values)`);
  check(`(let* ((unit (compile-to-hir '(lambda (x y) (< x y))))
        (function (first (hir-unit-functions unit)))
        (call (loop for block in (hir-function-blocks function) thereis
          (find-if (lambda (instruction) (eq (hir-instruction-opcode instruction) :call))
            (hir-block-instructions block))))
        (operands (cddr (hir-instruction-inputs call)))
        (left (first operands)) (right (second operands))
        (facts (make-hash-table :test 'eq)))
    (setf (gethash left facts) 'list (gethash right facts) 'integer)
    (and (hir-standard-property-negative-p 'endp (list left) :may-signal facts)
      (not (hir-standard-property-negative-p 'endp operands :may-signal facts))
      (not (hir-standard-property-negative-p 'endp (list right) :may-signal facts))
      (not (hir-standard-property-negative-p 'endp (list left) :poll-free facts))
      (not (hir-standard-property-negative-p 'identity (list left) :allocation facts))
      (not (hir-standard-property-negative-p 'funcall operands :callbacks facts))
      (not (hir-standard-property-negative-p (make-symbol "ENDP") (list left) :may-signal facts))
      (not (hir-standard-property-negative-p 'endp (list left) :may-signal facts (list 0)))
      (progn (setf (gethash left facts) 'integer)
        (multiple-value-bind (answer dependencies)
            (hir-standard-property-negative-p '< operands :may-signal facts)
          (and answer (equal dependencies operands))))
      (progn (setf (gethash right facts) t)
        (not (hir-standard-property-negative-p '< operands :may-signal facts)))
      (every (lambda (type)
        (setf (gethash left facts) type)
        (not (hir-standard-property-negative-p 'endp (list left) :may-signal facts)))
        '(soundness-query-opaque (satisfies soundness-query-never-run)))))`);
  // A retained helper's return type supplies the domain while its callback
  // and errors remain observable before the discarded operation checkpoint.
  check(`(let ((events nil)
      (function (compile-native '(lambda (effect)
        (declare (optimize (space 3) (speed 1) (safety 1) (debug 0)))
        (labels ((empty (callback) (funcall callback :operand) nil))
          (declare (notinline empty))
          (endp (empty effect)) (values :after nil :tail))))))
    (and (equal (multiple-value-list
        (funcall function (lambda (event) (push event events)))) '(:after nil :tail))
      (equal events '(:operand))
      (handler-case (progn (funcall function (lambda (event)
        (declare (ignore event)) (error "Operand callback"))) nil) (simple-error () t))
      (equal (multiple-value-list (funcall function
        (lambda (event) (declare (ignore event))))) '(:after nil :tail))))`);
  check(`(let ((function (compile-native '(lambda (replacement)
      (declare (optimize (space 3) (speed 1) (safety 1) (debug 0)))
      (let ((object nil))
        (when (listp object)
          (funcall (lambda () (setq object replacement))) (endp object)) :after)))))
    (and (handler-case (progn (funcall function :atom) nil) (type-error () t))
      (eq (funcall function nil) :after)))`);
  // Inventory membership alone supplies no generic-method result proof.
  // Exact standardized function identity excludes other CL symbols and
  // same-name user functions; malformed function-name shapes stay bounded.
  check(`(and
    (every #'standard-function-name-p
      '(cl:abs cl:mapcar cl:compile-file cl:make-instance cl:class-name
        (cl:setf cl:class-name) (cl:setf cl:documentation)))
    (notany #'standard-function-name-p
      '(cl:when cl:if cl:fixnum cl:*package* soundness-shadow::abs
        (cl:setf cl:car) (cl:setf soundness-shadow::class-name)
        (cl:setf cl:class-name :extra) (cl:setf . cl:class-name)))
    (let ((name (list 'cl:setf 'cl:class-name)))
      (setf (cddr name) name)
      (not (standard-function-name-p name)))
    (null (standard-function-result-count 'cl:funcall))
    (null (standard-function-result-count 'cl:make-instance))
    (not (hir-type-proves-p
      (soundness-proven-result
        '(lambda (x) (declare (notinline soundness-shadow::abs))
           (soundness-shadow::abs x))) 'real)))`);
  check(`(let ((effects nil)
        (function (compile-native '(lambda (object effect)
          (declare (optimize (safety 3) (debug 0)))
          (setf (third (progn (funcall effect :object) object))
                (progn (funcall effect :value) (values 42 :discarded)))))))
    (flet ((effect (event) (push event effects)))
      (and
        (handler-case (progn (funcall function '(a b . c) #'effect) nil)
          (type-error () (equal effects '(:value :object))))
        (let ((object (list 'a 'b 'c 'd)))
          (setq effects nil)
          (and (equal (multiple-value-list (funcall function object #'effect)) '(42))
            (equal effects '(:value :object)) (equal object '(a b 42 d)))))))`);
  for (const [name, arguments_, expected] of [
    ['consp', 'object', '(nil)'],
    ['eq', 'object object', '(t)'],
    ['evenp', '2', '(t)'],
    ['svref', '#(42) 0', '(42)'],
    ['list', 'object', '((:identity))'],
  ]) {
    check(`(let* ((name 'soundness-shadow::${name}) (object :identity)
        (initial (compile-native '(lambda (&rest arguments) (declare (ignore arguments))
          (values :initial :second))))
        (replacement (compile-native '(lambda (&rest arguments) (declare (ignore arguments))
          (values :replacement :extra)))))
      (setf (symbol-function name) initial)
      (let* ((ordinary (compile-native '(lambda (object)
                 (declare (optimize (safety 3) (debug 0)))
                 (soundness-shadow::${name} ${arguments_}))))
             (indirect (compile-native '(lambda (object)
                 (declare (notinline soundness-shadow::${name}))
                 (soundness-shadow::${name} ${arguments_}))))
             (standard (compile-native '(lambda (object) (cl:${name} ${arguments_}))))
             (captured (symbol-function name))
             (closure (funcall (compile-native '(lambda (saved object)
               (lambda () (funcall saved object)))) captured object)))
        (and (equal (multiple-value-list (funcall ordinary object)) '(:initial :second))
          (equal (multiple-value-list (funcall standard object)) '${expected})
          (progn (setf (symbol-function name) replacement)
            (and (equal (multiple-value-list (funcall ordinary object)) '(:replacement :extra))
              (equal (multiple-value-list (funcall indirect object)) '(:replacement :extra))
              (equal (multiple-value-list (funcall closure)) '(:initial :second))
              (eq captured initial)
              (equal (multiple-value-list
                (funcall (compile-native '(lambda (object)
                  (declare (inline soundness-shadow::${name}))
                  (soundness-shadow::${name} ${arguments_}))) object)) '(:replacement :extra)))))))`);
  }
  check(`(let* ((name 'soundness-shadow::list) (original #'list)
      (replacement (compile-native '(lambda (x) (declare (ignore x))
        (values :replacement :extra)))))
    (setf (symbol-function name) original)
    (let* ((module (compile-native-module
              '((defun soundness-module-list (x) (soundness-shadow::list x))
                (defun soundness-module-list-notinline (x)
                  (declare (notinline soundness-shadow::list)) (soundness-shadow::list x)))
              :inline nil))
           (callers (mapcar (lambda (name) (cdr (assoc name module)))
             '(soundness-module-list soundness-module-list-notinline)))
           (ordinary-apply (compile-native '(lambda (x)
             (apply (lambda (&rest result) result) (soundness-shadow::list x))))))
      (and (every #'functionp callers)
        (every (lambda (function) (equal (funcall function 1) '(1))) callers)
        (equal (funcall ordinary-apply 2) '(2))
        (progn (setf (symbol-function name) replacement)
          (and (every (lambda (function)
                 (equal (multiple-value-list (funcall function 1)) '(:replacement :extra))) callers)
            (handler-case (progn (funcall ordinary-apply 2) nil)
              (type-error (condition) (eq (type-error-datum condition) :replacement))))))))`);
  // A named call saves its callable before operand effects and still uses
  // the live definition on the next invocation. Exercise primary cleanup
  // for a callee returning secondary values and for a callee returning none.
  check(`(let* ((name 'soundness-shadow::five-primary)
      (original (lambda (a b c d e) (values (list a b c d e) :secondary)))
      (replacement (lambda (a b c d e) (declare (ignore a b c d e)) (values)))
      (events nil))
    (unwind-protect
         (progn
           (setf (fdefinition name) original)
           (let ((caller (compile-native '(lambda (a b c d e observe)
                    (declare (optimize (space 2) (speed 1) (safety 1) (debug 0)))
                    (let ((result (soundness-shadow::five-primary
                                    (progn (funcall observe :a) a)
                                    (progn (funcall observe :b) b)
                                    (progn (funcall observe :c) c)
                                    (progn (funcall observe :d) d)
                                    (progn (funcall observe :e) e))))
                      (values result))))))
             (and (equal (multiple-value-list
                           (funcall caller 1 2 3 4 5
                             (lambda (event)
                               (push event events)
                               (when (eq event :a) (setf (fdefinition name) replacement)))))
                         '((1 2 3 4 5)))
                  (equal (reverse events) '(:a :b :c :d :e))
                  (equal (multiple-value-list (funcall caller 1 2 3 4 5 #'identity))
                         '(nil)))))
      (fmakunbound name)))`);
  // Exercise both fixed and packet argument transport with fresh standard LIST
  // results, primary argument values, and observable left-to-right effects.
  for (const count of [9, 129]) {
    const operands = Array.from({length: count}, (_, index) =>
      `(progn (funcall effect ${index}) (values x :discarded))`).join(' ');
    for (const declaration of ['', '(notinline list)', '(inline list)']) {
      check(`(let ((function (compile-native '(lambda (x effect)
          (declare ${declaration} (optimize (safety 3) (debug 0)))
          (list ${operands}))))
          (events nil) (object (list :identity)))
        (let* ((first (funcall function object (lambda (index) (push index events))))
               (second (funcall function object (lambda (index) (declare (ignore index)))))
               (expected (make-list ${count} :initial-element object)))
          (and (equal (reverse events) (loop for index below ${count} collect index))
            (equal first expected) (equal second expected)
            (every (lambda (item) (eq item object)) first)
            (loop for left on first for right on second always (not (eq left right)))
            (progn (rplaca first :changed) (eq (car second) object)))))`);
    }
  }
  // ANSI covers MEMBER generally; this targets quoted integer selection while
  // retaining the original tail and first duplicate, including a bignum key.
  for (const declaration of ['', '(notinline member)', '(inline member)']) {
    check(`(let ((function (compile-native '(lambda (item)
      (declare ${declaration} (optimize (safety 3) (debug 0)))
      (values (member item '#1=(3 4 5 4 6 7 8 1180591620717411303424)) '#1#)))))
    (every (lambda (key)
      (multiple-value-bind (tail items) (funcall function key)
        (let ((expected (member key items :test #'eql)))
          (and (eq tail expected) (equal tail expected)))))
      '(3 4 5 8 1180591620717411303424 0 9 4.0 #\\a)))`);
  }
  // Standard-operation facts describe executable values, including reads
  // through immutable captures. Writes and mixed joins must retain complete
  // values from an arbitrary replacement rather than inherit CAR's contract.
  for (const [body, changed] of [
    ["(let ((operation #'car)) (funcall operation object))", false],
    ["(let ((operation (if choose #'car #'car))) (funcall operation object))", false],
    ["(let ((operation #'car)) (funcall (lambda () (funcall operation object))))", false],
    ["(let ((operation (if choose #'car replacement))) (funcall operation object))", true],
    ["(let ((operation #'car)) (let ((call (lambda () (funcall operation object)))) (setq operation replacement) (funcall call)))", true],
    ["(let ((operation #'car)) (let ((saved operation)) (funcall (lambda () (setq operation replacement))) (funcall saved object)))", false],
  ]) {
    check(`(let ((function (compile-native '(lambda (object replacement choose)
          (declare (optimize (safety 3) (debug 0))) ${body})))
        (replacement (lambda (object) (values :replacement object :extra)))
        (object (list :head :tail)))
      (and (equal (multiple-value-list (funcall function object replacement nil))
                  ${changed ? '(list :replacement object :extra)' : "'(:head)"})
        (equal (multiple-value-list (funcall function object replacement t))
          ${changed && body.includes('if choose') ? "'(:head)" : changed ? '(list :replacement object :extra)' : "'(:head)"})))`);
  }
  check(`(let ((events nil)
      (function (compile-native '(lambda (object effect)
        (declare (optimize (safety 3) (debug 0)))
        (funcall (progn (funcall effect :designator) #'car)
                 (progn (funcall effect :argument) (values object :discarded)))))))
    (and (equal (multiple-value-list
        (funcall function '(:head) (lambda (event) (push event events)))) '(:head))
      (equal events '(:argument :designator))
      (progn (setq events nil)
        (handler-case
          (progn (funcall function 17 (lambda (event) (push event events))) nil)
          (type-error () (equal events '(:argument :designator)))))
      (eq (funcall function '(:recovered) (lambda (event) (declare (ignore event)))) :recovered)))`);
  // Known test designators may select a checked shared search, while all
  // designator/operand effects and the original tail or pair stay observable.
  for (const name of ['member', 'assoc']) {
    for (const declaration of ['', `(notinline ${name})`]) {
      for (const control of [':test', ':test-not']) {
        check(`(let* ((events nil)
            (first (cons :first :one)) (match (cons :match :two))
            (items ${name === 'assoc' ? '(list nil first match (cons :match :duplicate))' : "(list :first :match :match)"})
            (function (compile-native '(lambda (item items effect)
              (declare ${declaration} (optimize (safety 3) (debug 0)))
              (let ((test (progn (funcall effect :test) #'${control === ':test' ? 'eql' : 'eq'})))
                (${name} (progn (funcall effect :item) (values item :discarded))
                         (progn (funcall effect :items) items)
                         ${control} test :key (progn (funcall effect :key) nil)))))))
          (labels ((effect (event) (push event events))
                   (probe (item)
                     (let ((answer (multiple-value-list (funcall function item items #'effect))))
                       (and (null (cdr answer))
                            (eq (car answer) ${name === 'assoc' ? 'match' : '(cdr items)'})))))
            (and (probe ${control === ':test' ? ':match' : ':first'})
              (equal events '(:key :items :item :test))
              (handler-case (progn (funcall function :match 17 #'effect) nil)
                (type-error (condition) (eq (type-error-datum condition) 17)))
              (probe ${control === ':test' ? ':match' : ':first'}))))`);
      }
      check(`(let ((seen nil)
          (function (compile-native '(lambda (items key)
            (declare ${declaration} (optimize (safety 3) (debug 0)))
            (${name} :match items :test #'eql :key key)))))
        (let* ((first (list :first)) (match (list :match))
            (items ${name === 'assoc' ? '(list nil (cons first :one) (cons match :two))' : '(list first match)'})
            (expected ${name === 'assoc' ? '(third items)' : '(cdr items)'}))
          (and (let ((answer (multiple-value-list
              (funcall function items (lambda (object)
                (push object seen) (values (car object) :discarded))))))
              (and (null (cdr answer)) (eq (car answer) expected)))
            (member first seen :test #'eq) (member match seen :test #'eq)
            (handler-case
              (progn (funcall function items (lambda (object)
                (declare (ignore object)) (error "Search key callback"))) nil)
              (simple-error () t))
            (eq (funcall function items #'car) expected))))`);
    }
  }
  recover();
  console.log('Soundness: proofs remain conservative across declarations, mutation and opaque types; user names retain live dispatch and values.');

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

// Explicit lambda compilation checks input and can recover without starting
// the optimized compiler on an otherwise loader-equipped runtime.
check(`(let ((*compile-evaluations* t) (*compiler-state* :cold)
             (source (list 'lambda nil 42)))
  (setf (cdddr source) source)
  (and (handler-case (progn (compile nil source) nil)
         (compiler-input-error (condition) (eq (compiler-error-datum condition) source)))
       (eq *compiler-state* :cold)
       (= (funcall (compile nil '(lambda () (declare (optimize (debug 1))) 42))) 42)
       (eq *compiler-state* :cold)))`);

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
  // The optional declaration service shares real lexical contours with the
  // evaluator/compiler; opaque data and explicit NIL must survive both paths.
  evaluate(await fs.readFile(new URL('../compatibility/environments/environments.lisp', import.meta.url), 'utf8'));
  evaluate(`(in-package :miricl-compiler)
    (miricl-environment:define-declaration soundness-free (specifier environment)
      (if (eq (second specifier) :invalid)
          (values :declare :invalid-shell)
          (values :declare (cons 'soundness-free-key (second specifier)))))
    (defmacro soundness-free-value (&environment environment)
      (list 'quote (miricl-environment:declaration-information 'soundness-free-key environment)))
    (proclaim '(soundness-free :global)) (values)`);
  check(`(let* ((source '(lambda ()
                  (locally (declare (soundness-free :outer))
                    (list (soundness-free-value)
                      (locally (declare (soundness-free nil)) (soundness-free-value))
                      (soundness-free-value)
                      (let ((soundness-opaque :runtime-value))
                        (declare (ignorable soundness-opaque) (soundness-free soundness-opaque))
                        (soundness-free-value))))))
                (expected '(:outer nil :outer soundness-opaque)))
      (and (equal (funcall (eval source)) expected)
           (equal (funcall (compile-native source)) expected)
           (eq :global (miricl-environment:declaration-information 'soundness-free-key))))`);
  check(`(let* ((payload (list :opaque))
                 (base (miricl-environment:augment-environment nil :declare '((soundness-free :base))))
                 (inner (miricl-environment:augment-environment base :declare '((soundness-free nil)))))
      (setf (cdr payload) payload)
      (and (= 1 (length (multiple-value-list (miricl-environment:augment-environment base))))
           (eq :base (miricl-environment:declaration-information 'soundness-free-key base))
           (null (miricl-environment:declaration-information 'soundness-free-key inner))
           (eq payload (miricl-environment:declaration-information 'soundness-free-key
             (miricl-environment:augment-environment nil :declare (list (list 'soundness-free payload)))))
           (handler-case (progn (proclaim '(soundness-free :invalid)) nil)
             (error (condition) (plusp (length (princ-to-string condition)))))
           (handler-case (progn (miricl-environment:declaration-information 'soundness-free-key '(invalid)) nil)
             (type-error () t))
           (eq :global (miricl-environment:declaration-information 'soundness-free-key))
           (= 4 (funcall (compile-native '(lambda (x) (declare ((integer 0 10) x)) (+ x 1))) 3))))`);
  recover();
  console.log('Soundness: optional free declarations preserve lexical NIL and opaque data, with checked rejection and recovery.');
  // Gray extensions must reach ordinary CL entry points and checked callback
  // boundaries. Native string streams retain their canonical generic methods.
  evaluate(await fs.readFile(new URL('../compatibility/gray-streams/miricl-gray.lisp', import.meta.url), 'utf8'));
  evaluate(`(in-package :miricl-compiler)
    (defclass soundness-gray (miricl-gray:fundamental-input-stream miricl-gray:fundamental-output-stream)
      ((input :initform "" :initarg :input :reader soundness-gray-input)
       (position :initform 0 :accessor soundness-gray-position)
       (output :initform nil :accessor soundness-gray-output)
       (bad :initform nil :initarg :bad :reader soundness-gray-bad)
       (closed :initform nil :accessor soundness-gray-closed)))
    (defmethod miricl-gray:stream-read-char ((stream soundness-gray))
      (if (eq (soundness-gray-bad stream) :character) 42
          (let ((position (soundness-gray-position stream)))
            (if (= position (length (soundness-gray-input stream))) :eof
                (prog1 (char (soundness-gray-input stream) position)
                  (incf (soundness-gray-position stream)))))))
    (defmethod miricl-gray:stream-unread-char ((stream soundness-gray) character)
      (declare (ignore character)) (decf (soundness-gray-position stream)) nil)
    (defmethod miricl-gray:stream-write-char ((stream soundness-gray) character)
      (push character (soundness-gray-output stream)) :ignored)
    (defmethod miricl-gray:stream-line-column ((stream soundness-gray))
      (if (eq (soundness-gray-bad stream) :column) -1
          (loop for character in (soundness-gray-output stream)
                until (char= character #\\Newline) count 1)))
    (defmethod close ((stream soundness-gray) &key abort)
      (declare (ignore abort)) (setf (soundness-gray-closed stream) t) t)
    (defclass soundness-gray-index (soundness-gray) ())
    (defmethod miricl-gray:stream-read-sequence
        ((stream soundness-gray-index) sequence start end &key)
      (declare (ignore sequence start)) (1+ end))
    (defclass soundness-gray-byte (miricl-gray:fundamental-binary-input-stream
                                  miricl-gray:fundamental-binary-output-stream) ())
    (defmethod stream-element-type ((stream soundness-gray-byte)) '(unsigned-byte 4))
    (defmethod miricl-gray:stream-read-byte ((stream soundness-gray-byte)) 42)
    (defmethod miricl-gray:stream-write-byte ((stream soundness-gray-byte) byte)
      (error "Invalid bytes must be rejected before this callback."))
    (defmethod open-stream-p ((stream soundness-gray))
      (unless (soundness-gray-closed stream) (values :open :secondary)))
    (defclass soundness-gray-printable () ())
    (defmethod print-object ((object soundness-gray-printable) stream)
      (write-string "PRINTED" stream) object)
    (defvar *soundness-gray-native-query* nil)
    (defmethod stream-element-type :around ((stream string-stream))
      (when *soundness-gray-native-query* (incf *soundness-gray-native-query*))
      (call-next-method)) (values)`);
  check(`(let* ((stream (make-instance 'soundness-gray :input "#| comment |# (1 2)Q"))
                 (marker (list :eof)))
      (and (streamp stream) (typep stream 'stream)
           (equal (read stream) '(1 2)) (char= #\\Q (read-char stream))
           (null (unread-char #\\Q stream)) (char= #\\Q (read-char stream))
           (eq marker (read-char stream nil marker))
           (null (format stream (lambda (destination &rest arguments)
              (declare (ignore arguments)) (assert (eq destination stream))
              (write-char #\\A destination) (format destination "~D" 42))))
           (string= "A42" (coerce (reverse (soundness-gray-output stream)) 'string))
           (close stream) (soundness-gray-closed stream) (not (open-stream-p stream))
           (handler-case (progn (write-char #\\X stream) nil) (stream-error () t))))`);
  check(`(let ((caller (compile-native '(lambda (stream) (stream-element-type stream))))
               (stream (make-string-output-stream)))
      (let ((*soundness-gray-native-query* 0))
        (and (eq 'character (funcall caller stream)) (plusp *soundness-gray-native-query*))))`);
  check(`(and
      (handler-case (progn (read-char (make-instance 'soundness-gray :bad :character)) nil)
        (type-error (condition) (eql 42 (type-error-datum condition))))
      (handler-case (progn (fresh-line (make-instance 'soundness-gray :bad :column)) nil)
        (type-error (condition) (eql -1 (type-error-datum condition))))
      (handler-case (progn (read-byte (make-instance 'soundness-gray-byte)) nil)
        (type-error (condition) (eql 42 (type-error-datum condition))))
      (handler-case (progn (write-sequence #(:eof) (make-instance 'soundness-gray-byte)) nil)
        (type-error (condition) (eq :eof (type-error-datum condition))))
      (handler-case (progn (read-sequence (make-string 1) (make-instance 'soundness-gray-index)) nil)
        (type-error (condition) (= 2 (type-error-datum condition))))
      (char= #\\R (read-char (make-instance 'soundness-gray :input "R"))))`);
  recover();
  check(`(let* ((stream (make-instance 'soundness-gray))
                 (caller (compile-native '(lambda (stream) (open-stream-p stream))))
                 (expected '(:open :secondary)))
      (and (equal expected (multiple-value-list (open-stream-p stream)))
           (equal expected (multiple-value-list (funcall (fdefinition 'open-stream-p) stream)))
           (equal expected (multiple-value-list (funcall caller stream)))))`);
  check(`(let ((*print-circle* t) (object (make-instance 'soundness-gray-printable))
                (condition (make-condition 'simple-error :format-control "condition ~D"
                                           :format-arguments '(7))))
      (and (every (lambda (control)
                    (let ((stream (make-string-output-stream)))
                      (format stream control object)
                      (string= "pre PRINTED post" (get-output-stream-string stream))))
                  '("pre ~A post" "pre ~S post" "pre ~W post"))
           (let ((stream (make-string-output-stream)))
             (format stream "pre ~A post" condition)
             (string= "pre condition 7 post" (get-output-stream-string stream)))
           (let ((stream (make-instance 'soundness-gray)))
             (format stream "pre ~A post" object)
             (string= "pre PRINTED post"
                      (coerce (reverse (soundness-gray-output stream)) 'string)))
           (let ((stream (make-string-output-stream)) (width 12))
             (format stream "pre ~vA post" width object)
             (string= (concatenate 'string "pre PRINTED"
                         (make-string (- width (length "PRINTED")) :initial-element #\\Space) " post")
                      (get-output-stream-string stream)))))`);
  console.log('Soundness: ordinary Gray I/O preserves generic dispatch and rejects malformed callback results.');
} finally {vm.close();}
