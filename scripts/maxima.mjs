// Reusable browser/Node session. Algebra and parsing run entirely in MIRICL.
import {Miricl} from './miricl.mjs';
const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';

export async function createMaxima(image, core, {progress = () => {}, onPrompt, ...host} = {}) {
  const results = [];
  let answer;
  const vm = await Miricl.instantiate(image, {...host, imports: {...host.imports,
    maxima: {
      result(label, text) {results.push({label, text});},
      prompt(text) {answer = undefined; onPrompt(text);},
      hasAnswer() {return answer !== undefined;},
      takeAnswer() {const text = answer; answer = undefined; return text;},
    }}});
  try {
    vm.mountFile('/maxima.core', core);
    const evaluate = source => vm.evaluateAsync(source, {eof: true});
    progress('Loading Maxima');
    await evaluate(`
      (setq miricl:*trap-floating-point-underflow* nil)
      (load "/maxima.core")
      (values)`);
    progress('Initializing Maxima');
    const startup = await evaluate(`
      (setf (symbol-function 'cl-info::load-primary-index) (lambda () nil))
      (let ((help (lambda (&rest topics) (declare (ignore topics))
                    (format t "The manual is not bundled. See https://maxima.sourceforge.io/docs/manual/~%") nil)))
        (setf (symbol-function 'cl-info::info-exact) help
              (symbol-function 'cl-info::info-inexact) help))
      (let ((*package* (find-package :maxima)))
        (maxima::initialize-runtime-globals)
        (maxima::setup-search-lists))
      (values)
      (js:define-import maxima-browser-result ("maxima" "result") (:string :string) :void)
      ${onPrompt && vm.threads ? `
        (js:define-import maxima-browser-prompt ("maxima" "prompt") (:string) :void)
        (js:define-import maxima-browser-answer-p ("maxima" "hasAnswer") () :boolean)
        (js:define-import maxima-browser-answer ("maxima" "takeAnswer") () :string)
        (defun maxima-browser-await-input (question)
          (maxima-browser-prompt question)
          (loop until (maxima-browser-answer-p) do (sleep 0.02))
          (make-string-input-stream (maxima-browser-answer)))
        (let ((original (symbol-function 'maxima::retrieve)))
          (setf (symbol-function 'maxima::retrieve)
            (lambda (message flag)
              (if (peek-char nil *query-io* nil nil)
                (funcall original message flag)
                (progn
                  (let* ((*standard-input* (maxima-browser-await-input
                      (cond ((null message) "Input:")
                          ((atom message) (format nil "~A" message))
                          ((eq flag t) (format nil "~{~A~}" (cdr message)))
                          (t (with-output-to-string (stream)
                               (maxima::format-prompt stream "~M" message))))))
                         (*query-io* (make-two-way-stream *standard-input* *standard-output*)))
                    (funcall original message flag)))))))
        (let ((original (symbol-function 'maxima::$readonly)))
          (setf (symbol-function 'maxima::$readonly)
            (lambda (&rest messages)
              (if (peek-char nil *query-io* nil nil)
                (apply original messages)
                (let* ((*standard-input* (maxima-browser-await-input
                          (if messages (with-output-to-string (stream)
                            (let ((*standard-output* stream)) (apply #'maxima::$print messages))) "Input:")))
                       (*query-io* (make-two-way-stream *standard-input* *standard-output*)))
                  (apply original messages))))))` : ''}
      (defun maxima-browser-evaluate (text answers)
        (let ((*package* (find-package :maxima))
              (*debugger-hook* nil)
              (maxima::*mread-prompt* "")
              (*standard-input* (make-string-input-stream answers)))
          (let ((*query-io* (make-two-way-stream *standard-input* *standard-output*)))
            (catch 'maxima::macsyma-quit
              (with-input-from-string (stream text)
                (loop for form = (maxima::dbm-read stream nil '(nil)) until (equal form '(nil)) do
                  (when (or (not (maxima::checklabel maxima::$inchar))
                            (not (maxima::checklabel maxima::$outchar)))
                    (incf maxima::$linenum))
                  (let ((input (maxima::makelabel maxima::$inchar)))
                    (unless maxima::$nolabels (set input (third form))))
                  (setq maxima::$__ (third form)
                        maxima::$% (maxima::toplevel-macsyma-eval (third form))
                        maxima::$_ maxima::$__)
                  (let ((label (maxima::makelabel maxima::$outchar)))
                    (unless maxima::$nolabels (set label maxima::$%))
                    (when (eq (caar form) 'maxima::displayinput)
                      (maxima-browser-result (subseq (symbol-name label) 1)
                        (coerce (maxima::mstring maxima::$%) 'string))))))))))
      (values)`);
    progress('Ready');
    return {
      vm, startup: startup.output,
      answer(text) {answer = /[;$]\s*$/.test(text) ? text : text + ';';},
      async evaluate(source, {answers = ''} = {}) {
        results.length = 0;
        const start = performance.now();
        const {output} = await evaluate(`(progn (maxima-browser-evaluate ${lispString(source)} ${lispString(answers)}) (values))`);
        return {results: [...results], output, milliseconds: Math.round(performance.now() - start)};
      },
      close() {vm.close();},
    };
  } catch (error) {vm.close(); throw error;}
}
