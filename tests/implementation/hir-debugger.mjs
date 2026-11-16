// Live HIR debugger contracts, including public evaluation and minimal COMPILE.
// node tests/implementation/hir-debugger.mjs [matching-image.wasm]
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../../scripts/miricl.mjs';
const stops = [];
let pendingStop;
const nextStop = () => stops.length ? Promise.resolve(stops.shift())
  : new Promise(resolve => {pendingStop = resolve;});
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'), {
  onDebugger(event) {
    if (event.type !== 'stopped') return;
    if (pendingStop) {const resolve = pendingStop; pendingStop = null; resolve(event.stop);}
    else stops.push(event.stop);
  },
});
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const cases = [
['captured cell and escaping frame-created closure',`(progn
 (defparameter *debug-retained* nil)
 (let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (funcall (lambda () (values x :done)))) :name 'captured-debug)) (edited nil))
  (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (null (miricl-debugger:frame-name frame)) (assoc 'x (miricl-debugger:frame-locals frame)))
    (setq edited t) (miricl-debugger:eval-in-frame '(progn (setq *debug-retained* (lambda (delta) (incf x delta))) (setq x 42)) frame)))))
   (list (multiple-value-list (funcall f 40)) (funcall *debug-retained* 8) (funcall *debug-retained* 1) (miricl-debugger:frames)))))`,'((42 :DONE) 50 51 NIL)\n'],
['lexical function exact identity and calls',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (flet ((helper (y) (+ x y))) (values #'helper (helper 1)))) :name 'function-debug)) (seen nil) (edited nil))
  (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values)))
    (setq edited t seen (miricl-debugger:eval-in-frame '(list #'helper (helper 2)) frame))))))
   (let ((result (multiple-value-list (funcall f 40)))) (list (eq (car result) (car seen)) (second result) (second seen)))) )`,'(T 41 42)\n'],
['unused lexical function remains callable in frame',`(let ((f (miricl-compiler:compile-hir
 '(lambda (x) (declare (optimize (debug 3))) (flet ((helper (y) (+ x y))) (values x :done))))) (seen nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame)
   (when (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values))
     (setq seen (miricl-debugger:eval-in-frame '(helper 2) frame))))))
   (list (multiple-value-list (funcall f 40)) seen)))`,'((40 :DONE) 42)\n'],
['RETURN-FROM in frame preserves cleanup and multiple values',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (block outer (unwind-protect (values x :ordinary) (incf *debug-cleanup*)))) :name 'exit-debug)) (edited nil) (*debug-cleanup* 0))
  (declare (special *debug-cleanup*))
  (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values)))
    (setq edited t) (miricl-debugger:eval-in-frame '(return-from outer (values x :escaped)) frame)))))
   (list (multiple-value-list (funcall f 42)) *debug-cleanup* (miricl-debugger:frames))))`,'((42 :ESCAPED) 1 NIL)\n'],
['GO in frame reaches existing tag and runs cleanup',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (tagbody before (unwind-protect (setq x 10) (incf *debug-cleanup*)) after (incf x)) (values x :done)) :name 'go-debug)) (edited nil) (*debug-cleanup* 0))
  (declare (special *debug-cleanup*))
  (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'setq)))
    (setq edited t) (miricl-debugger:eval-in-frame '(go after) frame)))))
   (list (multiple-value-list (funcall f 41)) *debug-cleanup* (miricl-debugger:frames))))`,'((42 :DONE) 1 NIL)\n'],
['special read/write observes dynamic binding',`(progn (defparameter *debug-special* 3)
(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (let ((*debug-special* x)) (declare (special *debug-special*)) (values *debug-special* :done))) :name 'special-debug)) (edited nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values)))
   (setq edited t) (miricl-debugger:eval-in-frame '(setq *debug-special* (+ *debug-special* 2)) frame)))))
  (list (multiple-value-list (funcall f 40)) *debug-special*))))`,'((42 :DONE) 3)\n'],
['malformed frame expression rejects and resumes without repeated effects',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (values x :done)) :name 'malformed-debug)) (rejected nil) (edited nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (unless edited (setq edited t)
   (let ((bad (list 'progn))) (setf (cdr bad) (list bad))
    (handler-case (miricl-debugger:eval-in-frame bad frame) (program-error () (setq rejected t))))))))
  (list (multiple-value-list (funcall f 42)) rejected (miricl-debugger:frames))))`,'((42 :DONE) T NIL)\n'],
['live symbol macro place',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (symbol-macrolet ((alias x)) (values alias :done))) :name 'review-symbol-macro)) (edited nil) (failure nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values))
   (setq edited t) (handler-case (miricl-debugger:eval-in-frame '(incf alias) frame) (error (c) (setq failure (princ-to-string c))))))))
  (list (multiple-value-list (funcall f 40)) failure (miricl-debugger:frames))))`,'((41 :DONE) NIL NIL)\n'],
['live local macro environment',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (symbol-macrolet ((alias x)) (macrolet ((bump (&environment env) (list 'incf (macroexpand 'alias env)))) (values alias :done)))) :name 'review-local-macro)) (edited nil) (failure nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values))
   (setq edited t) (handler-case (miricl-debugger:eval-in-frame '(bump) frame) (error (c) (setq failure (princ-to-string c))))))))
  (list (multiple-value-list (funcall f 40)) failure (miricl-debugger:frames))))`,'((41 :DONE) NIL NIL)\n'],
['typed write rejection and recovery',`(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (type fixnum x) (optimize (debug 3))) (values x :done)) :name 'review-typed)) (edited nil) (rejected nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (assoc 'x (miricl-debugger:frame-locals frame)))
   (setq edited t)
   (handler-case (miricl-debugger:eval-in-frame '(locally (declare (optimize (safety 0))) (setq x :wrong)) frame) (type-error () (setq rejected t)))
   (miricl-debugger:eval-in-frame '(incf x) frame)))))
  (list (multiple-value-list (funcall f 41)) rejected (miricl-debugger:frames))))`,'((42 :DONE) T NIL)\n'],
['expired target and stale frame reject',`(progn (defparameter *review-exit* nil) (defparameter *review-frame* nil)
 (let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (block outer (values x :done))) :name 'review-lifetime)) (edited nil))
  (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values))
    (setq edited t *review-frame* frame)
    (miricl-debugger:eval-in-frame '(setq *review-exit* (lambda () (return-from outer :expired))) frame)))))
   (let ((answer (multiple-value-list (funcall f 42))))
    (list answer (handler-case (funcall *review-exit*) (control-error () t))
      (handler-case (miricl-debugger:eval-in-frame '42 *review-frame*) (control-error () t))
      (miricl-debugger:frames))))))`,'((42 :DONE) T T NIL)\n'],
['quoted cyclic literal and displayed source share identity',`(let* ((object (list :cycle)) (seen nil) (source nil) (f nil))
 (setf (cdr object) object source (list 'lambda nil '(declare (optimize (debug 3))) (list 'values (list 'quote object) :done)))
 (setq f (miricl-compiler:compile-hir source :name 'review-literal))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (let ((form (miricl-debugger:frame-form frame)))
   (when (and (consp form) (eq (car form) 'quote)) (setq seen (eq (second form) object)))))))
  (let ((answer (multiple-value-list (funcall f)))) (list (eq (first answer) object) seen (second answer) (miricl-debugger:frames)))))`,'(T T :DONE NIL)\n'],
['public routing', `(progn (setq miricl-compiler:*compile-evaluations* :interpreted)
 (defun public-hir-debug (x) (declare (optimize (debug 3))) (values x :tail))
 (let ((edited nil)) (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (not edited) (eq (miricl-debugger:frame-name frame) 'public-hir-debug)) (setq edited t) (miricl-debugger:eval-in-frame '(incf x) frame)))))
 (list (multiple-value-list (public-hir-debug 41)) edited (compiled-function-p (compile nil '(lambda (x) (declare (optimize (debug 3))) x)))))))`, '((42 :TAIL) T T)\n'],
['macro scope shadowing', `(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (symbol-macrolet ((alias x)) (let ((alias 7)) (values x alias)))))) (seen nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'values)) (setq seen (miricl-debugger:eval-in-frame '(list alias x) frame))))))
 (list (multiple-value-list (funcall f 42)) seen)))`, '((42 7) (7 42))\n'],
['macros shadow outer lexical bindings', `(let ((f (miricl-compiler:compile-hir '(lambda (alias) (declare (optimize (debug 3))) (let ((x 40)) (symbol-macrolet ((alias x)) (macrolet ((values (&rest ignored) (declare (ignore ignored)) '(list alias))) (values :ignored))))))) (seen nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (when (and (consp (miricl-debugger:frame-form frame)) (eq (car (miricl-debugger:frame-form frame)) 'list)) (setq seen (miricl-debugger:eval-in-frame '(progn (incf alias) (values :ignored)) frame))))))
 (list (funcall f 7) seen)))`, '((41) (41))\n'],
['snapshot isolation', `(let ((f (miricl-compiler:compile-hir '(lambda (x) (declare (type (integer 0 100) x) (optimize (debug 3))) (values x :done)))) (edited nil) (rejected nil))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (unless edited (setq edited t)
   (let* ((live (svref frame 6)) (scope (svref live 0)) (registers (svref live 1)))
    (loop for i below (length registers) do (setf (svref registers i) nil))
    (loop for i from 0 below (length scope) by 4 do (when (consp (svref scope (+ i 3))) (setf (second (svref scope (+ i 3))) 99))))))))
  (funcall f 42))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (unless rejected
   (handler-case (miricl-debugger:eval-in-frame '(setq x 101) frame) (type-error () (setq rejected t)))))))
  (list (multiple-value-list (funcall f 42)) rejected)))`, '((42 :DONE) T)\n'],
['type metadata private after load', `(let* ((f (miricl-compiler:compile-hir '(lambda (x) (declare (type (integer 0 100) x) (optimize (debug 3))) (values x :done))))
 (wire (miricl-compiler::compiler-primitive 43 f)) (loaded (miricl-compiler:load-hir-code wire)) (edited nil) (rejected nil))
 (loop for descriptor across (svref wire 1) do
  (loop for site across (svref (svref descriptor 6) 2) do
    (let ((scope (svref site 4))) (loop for i from 0 below (length scope) by 4 do
      (when (consp (svref scope (+ i 3))) (setf (third (svref scope (+ i 3))) 1000))))))
 (let ((miricl-debugger:*step-hook* (lambda (frame) (unless edited (setq edited t)
  (handler-case (miricl-debugger:eval-in-frame '(setq x 101) frame) (type-error () (setq rejected t)))))))
 (list (multiple-value-list (funcall loaded 42)) rejected)))`, '((42 :DONE) T)\n'],
['malformed default debug eval and recovery', `(progn (setq miricl-compiler:*compile-evaluations* :interpreted) (let ((form (list 'progn nil))) (setf (second form) form)
 (list (handler-case (eval (list 'locally '(declare (optimize (debug 3))) form))
   (program-error (c) (not (null (princ-to-string c))))) (eval '(+ 20 22)))))`, '(T 42)\n'],
['malformed macro syntax and recovery', `(progn (setq miricl-compiler:*compile-evaluations* :interpreted)
 (let ((parameters (list nil)) (definitions (list '(bad () 1))))
  (setf (car parameters) parameters (cdr definitions) definitions)
  (list
   (handler-case (eval (list 'macrolet (list (list 'bad parameters 42)) '(bad)))
     (program-error (c) (not (null (princ-to-string c)))))
   (handler-case (eval (list 'macrolet definitions 42)) (program-error () t))
   (eval '(+ 20 22)))))`, '(T T 42)\n'],
['unregistered lexical name identity',"(let ((variable (gensym)) (local-function (gensym)) (block-name (gensym))) (list (eval (list 'let (list (list variable 40)) (list 'let (list (list 'f (list 'function (list 'lambda nil (list 'incf variable))))) '(let ((g (compile nil f))) (list (funcall g) (funcall f)))))) (eval (list 'flet (list (list local-function nil 42)) (list 'funcall (list 'compile nil (list 'function (list 'lambda nil (list local-function))))))) (eval (list 'block block-name (list 'funcall (list 'compile nil (list 'function (list 'lambda nil (list 'return-from block-name 42)))))))))", '((41 42) 42 42)\n'],
['shared source/HIR cells',`(let ((x 40))
 (let* ((read (lambda () x)) (write (lambda (value) (setq x value)))
        (hir-read (compile nil read)) (hir-write (compile nil write)))
  (setq x 41)
  (list (compiled-function-p hir-read)
    (funcall hir-read) (progn (funcall write 42) (funcall hir-read))
    (progn (funcall hir-write 43) (funcall read)))))`,'(T 41 42 43)\n'],
['source LABELS forward references',`(labels ((first (n) (if (zerop n) :done (second (1- n))))
           (second (n) (first n)))
 (list (funcall (compile nil #'first) 3)
       (eq (funcall (compile nil (lambda () #'second))) #'second)))`,'(:DONE T)\n'],
['source GO cleanup and expiration',`(let ((x 40) (cleanup 0) (saved nil))
 (tagbody before
  (setq saved (compile nil (lambda () (go after))))
  (unwind-protect (funcall saved) (incf cleanup))
  (setq x 999)
  after (incf x))
 (list x cleanup (handler-case (funcall saved) (control-error () t))))`,'(41 1 T)\n'],
['captured declared type check',`(let ((x 41)) (declare (type fixnum x))
 (let ((read (lambda () x)) (write (compile nil (lambda (value) (setq x value)))))
  (list (handler-case (funcall write :bad) (type-error () t)) (funcall read)
        (funcall write 42) (funcall read))))`,'(T 41 42 42)\n'],
];
try {
  // Keep the harness in source mode; individual cases exercise the public route.
  evaluate("(setq miricl-compiler:*compile-evaluations* nil) (when (boundp 'miricl-compiler::*compiler-state*) (unless (eq miricl-compiler::*compiler-state* :ready) (setq miricl-compiler::*compiler-state* :interpreted))) (values)");
  for (const [name, source, expected] of cases) {
    evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
    assert.equal(evaluate(source), expected, name);
    assert.equal(evaluate('(miricl-debugger:frames)'), 'NIL\n', name + ': frame cleanup');
    console.log('HIR debugger: ' + name);
  }

  evaluate(`(setq miricl-compiler:*compile-evaluations* nil)
    (defparameter *debug-wire* (miricl-compiler::compiler-primitive 43
      (miricl-compiler:compile-hir '(lambda (x) (declare (optimize (debug 3))) (values x :done)))))
    (values)`);
  for (const [name, mutation] of [
    ['self parent', '(setf (svref site 3) 1)'],
    ['incorrect root depth', '(setf (svref site 3) 0 (svref site 5) 1)'],
    ['invalid scope kind', '(setf (svref scope 0) 11)'],
    ['register out of bounds', '(setf (svref scope 0) 0 (svref scope 1) (svref descriptor 1))'],
    ['capture out of bounds', '(setf (svref scope 0) 1 (svref scope 1) (svref descriptor 4))'],
    ['constant variable', '(setf (svref scope 2) :keyword)'],
    ['nonfunction macro expander', '(setf (svref scope 0) 10 (svref scope 3) :invalid)'],
  ]) {
    assert.equal(evaluate(`(let* ((wire (miricl-compiler::compiler-primitive 43
        (miricl-compiler:load-hir-code *debug-wire*)))
        (descriptor (svref (svref wire 1) 0))
        (site (svref (svref (svref descriptor 6) 2) 0)) (scope (vector 0 0 'x t)))
      (setf (svref site 4) scope) ${mutation}
      (list (handler-case (progn (miricl-compiler:load-hir-code wire) nil)
               (program-error (c) (not (null (princ-to-string c)))))
            (multiple-value-list (funcall (miricl-compiler:load-hir-code *debug-wire*) 42))))`),
      '(T (42 :DONE))\n', name);
  }
  console.log('HIR debugger: corrupt metadata rejects with recovery.');

  // Public lexical snapshots reject malformed environments before inspecting
  // private records, and a rejection must leave compilation usable.
  for (const setup of [
    '(setq bad :bad)', '(setq bad (list 1))', '(setq bad (cons nil :tail))',
    '(setq bad (list nil)) (setf (cdr bad) bad)',
  ]) {
    assert.equal(evaluate(`(let ((bad nil)) ${setup}
      (list (handler-case (miricl-compiler::compiler-primitive 48 bad)
              (error (condition) (not (null (princ-to-string condition)))))
            (funcall (compile nil '(lambda () 42)))))`), '(T 42)\n');
  }
  console.log('HIR debugger: malformed lexical environments reject with recovery.');

  // File compilation must preserve lexical names and macro environments in
  // executable HIR, including the actual expander functions in its metadata.
  vm.mountFile('/hir-debugger.lisp', `(in-package :cl-user)
    (defun file-hir-debugger-helper () :done)
    (defun file-hir-debugger (x)
      (declare (type fixnum x) (optimize (debug 3)))
      (symbol-macrolet ((alias x))
        (macrolet ((bump (&environment environment)
                     (list 'incf (macroexpand 'alias environment))))
          (values alias (file-hir-debugger-helper)))))
    (symbol-macrolet ((alias x))
      (macrolet ((bump (&environment environment)
                   (list 'incf (macroexpand 'alias environment))))
        (defun file-inherited-hir-debugger (x)
          (declare (type fixnum x) (optimize (debug 3)))
          (values alias :done))))`);
  for (const blockCompile of ['nil', 't']) {
  assert.equal(evaluate(`(and (pathnamep (compile-file "/hir-debugger.lisp" :verbose nil :block-compile ${blockCompile}))
    (load "/hir-debugger.wasm")
    (every (lambda (function) (let ((edited nil) (rejected nil))
      (let ((miricl-debugger:*step-hook*
              (lambda (frame)
                (when (and (not edited) (consp (miricl-debugger:frame-form frame))
                           (eq (car (miricl-debugger:frame-form frame)) 'values))
                  (setq edited t)
                  (handler-case (miricl-debugger:eval-in-frame '(setq alias :bad) frame)
                    (type-error () (setq rejected t)))
                  (miricl-debugger:eval-in-frame '(bump) frame)))))
        (and (equal (multiple-value-list (funcall function 40)) '(41 :done))
             edited rejected (null (miricl-debugger:frames))))))
      (list #'file-hir-debugger #'file-inherited-hir-debugger)))`), 'T\n');
  }
  console.log('HIR debugger: compiled files retain lexical macros and checked frame writes.');

  if (vm.threads) {
    evaluate(`(setq miricl-compiler:*compile-evaluations* :interpreted) (defun interactive-hir-debug (x)
      (declare (optimize (debug 3)))
      (let ((shared (lambda () x))) (break "HIR debugger") (values x (funcall shared) :tail)))
      (compile 'interactive-hir-debug) (values)`);
    const running = vm.evaluateAsync('(multiple-value-list (interactive-hir-debug 40))', {eof: true});
    let stop = await nextStop();
    const frame = stop.frames.find(frame => frame.name.includes('INTERACTIVE-HIR-DEBUG') && frame.inspectable);
    assert.ok(frame, 'BREAK exposes the HIR frame.');
    const inspect = async source => {
      const updated = nextStop();
      const result = await stop.evaluate(frame.id, source);
      stop = await updated;
      return result.values;
    };
    assert.deepEqual(await inspect('(values x (funcall shared))'), ['40', '40']);
    assert.deepEqual(await inspect('(incf x)'), ['41']);
    assert.deepEqual(await inspect('(values x (funcall shared))'), ['41', '41']);
    const restart = stop.restarts.find(restart => restart.name === 'CONTINUE');
    assert.ok(restart, 'BREAK supplies CONTINUE.');
    await stop.invokeRestart(restart.id);
    assert.equal((await running).output, '(41 41 :TAIL)\n');
    assert.equal(evaluate('(miricl-debugger:frames)'), 'NIL\n');
    console.log('HIR debugger: interactive BREAK, frame writes and CONTINUE.');
  }
} finally {vm.close();}
