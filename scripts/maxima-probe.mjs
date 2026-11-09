// Shared by the Node probe and its browser worker. No host Lisp is involved.
import {Miricl} from './miricl.mjs';

export const maximaSmokeCases = [
  ['arithmetic', '2+3*4;', '14;'],
  ['rational arithmetic', '1/3+1/6;', '1/2;'],
  ['factor', 'factor(x^4-1);', '(x-1)*(x+1)*(x^2+1);'],
  ['ratsimp', 'ratsimp((x^3-1)/(x-1));', 'x^2+x+1;'],
  ['differentiate', 'diff(x^2*sin(x),x);', '2*x*sin(x)+x^2*cos(x);'],
  ['integrate', 'integrate(x^3,x);', 'x^4/4;'],
  ['solve', 'solve(x^2-1,x);', '[x=-1,x=1];'],
  ['determinant', 'determinant(matrix([1,2],[3,4]));', '-2;'],
  ['bignum', '100!;', `${Array.from({length: 100}, (_, i) => BigInt(i + 1))
    .reduce((a, b) => a * b, 1n)};`],
];

export async function probeMaxima(image, files, emit = () => {}, mode = 'source', options = {}) {
  const report = {mode, stages: [], loads: [], compilations: [], tests: [], suiteProgress: []};
  const createVM = () => Miricl.instantiate(image, {imports: {probe: {
    event(kind, file) {
      if (kind === 'load' || kind === 'compile')
        report[kind === 'load' ? 'loads' : 'compilations'].push(file);
      else if (kind === 'problem') report.currentProblem = file;
      else report.suiteProgress.push({kind, file});
      emit({kind, file});
    },
  }}});
  let vm = await createVM();
  let compiledBundle;
  let stage;
  const evaluate = async (name, source) => {
    stage = name;
    emit({kind: 'stage', name});
    const start = performance.now();
    try {
      const {output} = await vm.evaluateAsync(source, {eof: true});
      const result = {name, passed: true, milliseconds: Math.round(performance.now() - start), output};
      report.stages.push(result);
      emit({kind: 'stage-result', result});
      return output;
    } catch (error) {
      const result = {name, passed: false, milliseconds: Math.round(performance.now() - start)};
      report.stages.push(result);
      emit({kind: 'stage-result', result});
      throw error;
    }
  };
  try {
    for (const [name, bytes] of files) vm.mountFile(name, bytes);
    report.features = await evaluate('features', '(list *features* most-positive-fixnum)');
    await evaluate('configure', `
      (setq *default-pathname-defaults* #P"/maxima/")
      (load "configure.lisp")
      (configure :interactive nil :maxima-directory "/maxima")
      (values)`);
    if (mode !== 'upstream') await evaluate('port shim', '(load "/maxima-probe-port.lisp")');
    await evaluate('build tooling', `
      (setq *default-pathname-defaults* #P"/maxima/src/")
      (load "maxima-build.lisp")`);
    // Maxima initializes circular assumption databases in init-cl.lisp. A
    // compilation must start in a clean image, rather than reload that file
    // after a complete source bootstrap has already initialized the database.
    if (options.compiled) {
      await evaluate('load compiled core', '(load "/maxima-core.wasm")');
    } else await evaluate(mode === 'compile' ? 'compile core' : 'load core', `
      (js:define-import maxima-probe-event ("probe" "event") (:string :string) :void)
      (let ((original (symbol-function 'mk::load-function))
            (original-compiler (symbol-function 'mk::compile-function)))
        (unwind-protect
            (progn
              (setf (symbol-function 'mk::load-function)
                (lambda (component)
                  (let ((loader (funcall original component)))
                    (lambda (file &rest arguments)
                      (maxima-probe-event "load" (namestring file))
                      (apply loader file arguments)))))
              (setf (symbol-function 'mk::compile-function)
                (lambda (component)
                  (let ((compiler (funcall original-compiler component)))
                    (lambda (file &rest arguments)
                      (maxima-probe-event "compile" (namestring file))
                      (apply compiler file arguments)))))
              ${mode === 'compile' ? '(maxima-compile)' : `
                (let ((mk::*compile-during-load* nil)
                      (mk::*load-source-instead-of-binary* t)
                      (mk::*bother-user-if-no-binary* nil))
                  (maxima-load))`}
              (values))
          (setf (symbol-function 'mk::load-function) original
                (symbol-function 'mk::compile-function) original-compiler)))
      (unless (every #'fboundp '(maxima::meval maxima::mread maxima::mstring cl-user::run))
        (error "Maxima's core did not load."))
      (values)`);
    if (mode === 'compile') {
      const parts = [];
      const order = [...report.loads];
      // Native Maxima dumps the build image, including metadata initialized by
      // maxima.system. Preserve that metadata explicitly in our FASL bundle.
      await evaluate('compile build metadata', `
        (with-open-file (stream "/maxima-build-metadata.lisp" :direction :output :if-exists :supersede)
          (write '(in-package :cl-user) :stream stream) (terpri stream)
          (write (list 'defparameter '*maxima-build-time* (list 'quote *maxima-build-time*)) :stream stream)
          (terpri stream)
          (write '(export '*maxima-build-time*) :stream stream))
        (compile-file "/maxima-build-metadata.lisp" :output-file "/maxima-build-metadata.wasm")
        (values)`);
      parts.push(await vm.readFile('/maxima-build-metadata.wasm'));
      for (let i = 0; i < order.length; i++) {
        let file = order[i];
        if (file.endsWith('.lisp')) {
          const target = `/maxima-load-only-${i}.wasm`;
          await evaluate(`compile load-only ${file}`, `(compile-file ${JSON.stringify(file)} :output-file ${JSON.stringify(target)})`);
          file = target;
        }
        parts.push(await vm.readFile(file));
      }
      const bundle = new Uint8Array(8 + parts.reduce((size, bytes) => size + bytes.length - 8, 0));
      bundle.set([0, 97, 115, 109, 1, 0, 0, 0]);
      let position = 8;
      for (const part of parts) {bundle.set(part.subarray(8), position); position += part.length - 8;}
      const module = await WebAssembly.compile(bundle);
      if (WebAssembly.Module.imports(module).length ||
          WebAssembly.Module.customSections(module, 'miricl.fasl').length !== order.length + 1)
        throw new Error('Invalid Maxima bundle.');
      report.bundleBytes = bundle.length;
      compiledBundle = bundle;
      if (options.buildOnly) {
        await options.saveBundle?.(bundle);
        report.passed = true; report.validation = 'not-run';
        return report;
      }
      vm.close();
      vm = await createVM();
      for (const [name, bytes] of files) vm.mountFile(name, bytes);
      vm.mountFile('/maxima-core.wasm', bundle);
      await evaluate('fresh compiled startup', `
        (load "/maxima-probe-port.lisp")
        (setq *default-pathname-defaults* #P"/maxima/src/")
        (load "/maxima/src/maxima-build.lisp")
        (load "/maxima-core.wasm")
        (unless (every (lambda (name) (compiled-function-p (symbol-function name)))
                       '(maxima::meval maxima::mread maxima::mstring))
          (error "The fresh Maxima core is not compiled."))
        (unless (and (boundp '*maxima-build-time*) (= (length *maxima-build-time*) 9))
          (error "The compiled Maxima core lost its build timestamp."))
        (values)`);
    }
    await evaluate('initialize runtime', `
      (let ((*package* (find-package :maxima)))
        (maxima::initialize-runtime-globals)
        (maxima::setup-search-lists))
      (values)`);
    await evaluate('smoke helpers', `
      (defun maxima-probe-read (text)
        (let ((*package* (find-package :maxima)))
          (with-input-from-string (stream text) (third (maxima::mread stream)))))
      (defun maxima-probe-eval (text)
        (let ((*package* (find-package :maxima)))
          (maxima::meval (maxima-probe-read text))))
      (values)`);
    for (const [name, input, expected] of maximaSmokeCases) {
      try {
        const output = await evaluate(name, `
          (let ((actual (maxima-probe-eval ${JSON.stringify(input)}))
                (expected (maxima-probe-eval ${JSON.stringify(expected)})))
            (unless (maxima::alike1 actual expected)
              (error "Unexpected Maxima result: ~S; expected ~S" actual expected))
            (coerce (maxima::mstring actual) 'string))`);
        const result = {name, input, expected, passed: true, output};
        report.tests.push(result);
        emit({kind: 'test', ...result});
      } catch (error) {
        const result = {name, input, expected, passed: false, message: error.message,
          output: error.output, backtrace: error.backtrace};
        report.tests.push(result);
        emit({kind: 'test', ...result});
      }
    }
    if (mode === 'suite' || mode === 'share') {
      await evaluate('runtime paths', '(maxima::set-pathnames) (maxima::setup-search-lists) (values)');
      if (options.listOnly) {
        report.plan = [];
        await evaluate('test plan', `
          (js:define-import maxima-probe-event ("probe" "event") (:string :string) :void)
          (dolist (entry (cdr maxima::${mode === 'share' ? '$share_testsuite_files' : '$testsuite_files'}))
            (maxima-probe-event "plan"
              (maxima::maxima-string (if (consp entry) (second entry) entry))))
          (values)`);
        report.plan = report.suiteProgress.filter(event => event.kind === 'plan').map(event => event.file);
        if (options.tests?.length) {
          for (const name of options.tests)
            if (!report.plan.includes(name)) throw new Error(`Unknown Maxima ${mode} test: ${name}`);
          report.plan = [...options.tests];
        }
        report.passed = report.tests.every(test => test.passed) && report.plan.length > 0;
        return report;
      }
      const tests = [mode === 'share' ? 'share_tests=only' : '', options.tests?.length
        ? `tests=[${options.tests.map(name => JSON.stringify(name)).join(',')}]` : ''].filter(Boolean).join(',');
      const output = await evaluate('upstream tests', `
        (js:define-import maxima-probe-event ("probe" "event") (:string :string) :void)
        (let ((original (symbol-function 'maxima::test-batch))
              (original-evaluator (symbol-function 'maxima::meval*))
              (problem 0))
          (unwind-protect
            (progn
              (setf (symbol-function 'maxima::test-batch)
                (lambda (&rest arguments)
                  (setq problem 0)
                  (maxima-probe-event "suite" (namestring (car arguments)))
                  (let* ((stream (make-string-output-stream))
                         (results (let ((*standard-output* stream)) (multiple-value-list (apply original arguments))))
                         (text (get-output-stream-string stream)))
                    (maxima-probe-event "suite-result" text)
                    (write-string text)
                    (values-list results))))
              (setf (symbol-function 'maxima::meval*)
                (lambda (expression)
                  (when (and (consp expression) (consp (car expression))
                             (eq (caar expression) 'maxima::$errcatch))
                    (incf problem)
                    (let ((*print-length* 20) (*print-level* 8))
                      (maxima-probe-event "problem" (format nil "~D: ~S" problem (second expression)))))
                  (funcall original-evaluator expression)))
              (maxima-probe-eval ${JSON.stringify(`run_testsuite(${tests});`)}))
            (setf (symbol-function 'maxima::test-batch) original
                  (symbol-function 'maxima::meval*) original-evaluator)))`);
      report.suitePassed = /No unexpected errors found out of [1-9][0-9,]* tests/.test(output)
        && !/Maxima encountered a Lisp error|Caused an error break/.test(output);
      report.unexpectedPasses = output.match(/Tests that were expected to fail but passed:[\s\S]*/)?.[0] ?? '';
    }
    report.passed = report.tests.every(test => test.passed) && report.suitePassed !== false;
    if (report.passed && compiledBundle) await options.saveBundle?.(compiledBundle);
  } catch (error) {
    report.passed = false;
    report.failure = {stage, message: error.message, output: error.output, backtrace: error.backtrace};
  } finally { vm.close(); }
  return report;
}
