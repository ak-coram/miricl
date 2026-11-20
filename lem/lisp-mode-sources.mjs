// Checked source adaptations for the genuine local Lisp editing/listener UI.
// The owning submodule Gitlink is checked by files.mjs before these edits.
export function patchLispMode(files, replaceChecked) {
  const root = '/lem/extensions/lisp-mode/';
  function patch(name, transform) {
    const key = root + name + '.lisp', source = files.get(key);
    if (!source) throw new Error(`Missing selected Lisp-mode source: ${key}`);
    files.set(key, Buffer.from(transform(source.toString())));
  }
  // This scanner only locates a pinned source form; it never reads user source
  // or evaluates Lisp. Exact unique anchors and balanced boundaries fail closed.
  function form(source, anchor, replacement) {
    const start = source.indexOf(anchor);
    if (start < 0 || source.indexOf(anchor, start + 1) >= 0)
      throw new Error(`Lisp-mode overlay anchor must occur exactly once: ${anchor}`);
    let depth = 0, string = false, line = false, block = 0;
    for (let i = start; i < source.length; i++) {
      const c = source[i], next = source[i + 1];
      if (line) {if (c === '\n') line = false; continue;}
      if (block) {
        if (c === '#' && next === '|') {block++; i++;}
        else if (c === '|' && next === '#') {block--; i++;}
        continue;
      }
      if (string) {if (c === '\\') i++; else if (c === '"') string = false; continue;}
      if (c === ';') {line = true; continue;}
      if (c === '#' && next === '|') {block = 1; i++; continue;}
      if (c === '#' && next === '\\') {
        i += 2;
        if (/[A-Za-z]/u.test(source[i] ?? '')) while (i + 1 < source.length && !/[\s()]/u.test(source[i + 1])) i++;
        continue;
      }
      if (c === '"') string = true;
      else if (c === '(') depth++;
      else if (c === ')' && --depth === 0)
        return replacement === undefined ? source.slice(start, i + 1)
          : replaceChecked(source, source.slice(start, i + 1), replacement, anchor);
    }
    throw new Error(`Unbalanced Lisp-mode overlay form: ${anchor}`);
  }
  patch('lisp-mode', source => {
    for (const [anchor, replacement] of [
      ['(defun self-connected-p ()', '(defun self-connected-p ()\n  (and (current-connection) (self-connection-p (current-connection))\n       (connection-live-p (current-connection))))'],
      ['(defun self-connected-port ()', '(defun self-connected-port () (unsupported-operation "TCP ports on a local connection"))'],
      ['(defun self-connect ()', '(defun self-connect () (ensure-local-connection))'],
      ['(defun notify-change-connection-to-wait-message-thread ()', '(defun notify-change-connection-to-wait-message-thread ()\n  ;; Local workers deliver events directly; no transport poller exists.\n  (values))'],
      ['(defun start-thread ()', '(defun start-thread ()\n  ;; Local request submission starts the genuine worker lazily.\n  (values))'],
      ['(defun connect-to-micros (hostname port)', '(defun connect-to-micros (hostname port)\n  (declare (ignore hostname port)) (unsupported-operation "remote Micros servers"))'],
      ['(defun run-lisp (&key command port directory)', '(defun run-lisp (&key command port directory)\n  (declare (ignore command port directory)) (unsupported-operation "desktop Lisp processes"))'],
      ['(defun send-micros-create-server (process port)', '(defun send-micros-create-server (process port)\n  (declare (ignore process port)) (unsupported-operation "socket servers"))'],
      ['(defun run-slime (command &key (directory (buffer-directory)))', '(defun run-slime (command &key (directory (buffer-directory)))\n  (declare (ignore command directory)) (unsupported-operation "desktop Lisp processes"))'],
      ['(defun delete-lisp-connection (connection)', '(defun delete-lisp-connection (connection)\n  (close-local-connection connection) (remove-and-change-connection connection))'],
      ['(defun compute-context-menu-items ()', '(defun compute-context-menu-items ()\n  (list (lem/context-menu:make-item :label "Clear eval results"\n         :callback (lambda (&rest args) (declare (ignore args))\n                     (uiop:symbol-call :lem-lisp-mode/eval :lisp-eval-clear)))))'],
      ['(defun compilation-finished (result)', '(defun compilation-finished (result) (local-compilation-finished result))'],
      ['(define-command lisp-compile-and-load-file ()', '(define-command lisp-compile-and-load-file () ()\n  (unsupported-operation "file compilation; use compile-region or compile-defun"))'],
      ['(defun lisp-eval-async (form &optional cont (package (current-package)))', '(defun lisp-eval-async (form &optional cont (package (current-package)))\n  (local-lisp-eval-async form cont package))'],
      ['(defun read-package-name ()', `(defun read-package-name ()
  (check-connection)
  (let ((names (mapcar #'package-name (list-all-packages))))
    (prompt-for-string "Package: " :initial-value (connection-package (current-connection))
     :completion-function (lambda (string) (completion string names))
     :test-function (lambda (string) (find-package string)) :history-symbol 'mh-lisp-package)))`],
      ['(define-command lisp-interrupt ()', '(define-command lisp-interrupt () () (local-interrupt))']
    ]) source = form(source, anchor, replacement);
    source = replaceChecked(source,
      "  (setf (variable-value 'completion-spec)\n        (make-completion-spec 'completion-symbol-async :async t))",
      "  ;; Local lexical completion is a separate service capability.\n  (setf (variable-value 'completion-spec) nil)", 'local Lisp completion');
    for (const [variable, value] of [['find-definitions-function', 'lisp-find-definitions'], ['find-references-function', 'lisp-find-references']])
      source = replaceChecked(source, `(setf (variable-value '${variable}) '${value})`,
                              `(setf (variable-value '${variable}) nil)`, variable);
    source = replaceChecked(source,
      '(send-message-string (current-connection) (format nil "(:emacs-interrupt ~D)" thread-id))',
      '(local-interrupt)', 'synchronous local interrupt');
    source = form(source, '(defun buffer-package (buffer &optional default)', `(defun buffer-package (buffer &optional default)
  (let ((name (buffer-value buffer "package")))
    (cond ((stringp name) name)
          ((symbolp name) (if name (symbol-name name)
                              (or (guess-current-position-package (buffer-point buffer)) default)))
          ((and (consp name) (null (cdr name)))
           (etypecase (car name) (string (car name)) (symbol (symbol-name (car name)))))
          (t (editor-error "Malformed Lisp buffer package selection.")))))`);
    source = form(source, '(defun guess-current-position-package (point)', `(defun guess-current-position-package (point)
  ;; Infer only a complete single-line IN-PACKAGE form. Use the target reader
  ;; for escaping/case, with reader evaluation disabled during editor inference.
  (with-point ((p point))
    (loop
      (let ((line (line-string p)))
        (when (ppcre:scan "^\\\\s*\\\\(\\\\s*(?:cl:|common-lisp:)?in-package\\\\s" (string-downcase line))
          (let ((form (handler-case
                          (let ((*read-eval* nil) (*package* (find-package "CL-USER")))
                            (read-from-string line nil nil))
                        (error () nil))))
            (when (and (consp form) (eq (car form) 'cl:in-package)
                       (consp (cdr form)) (null (cddr form)))
              (let ((name (second form)))
                (when (and (consp name) (eq (car name) 'quote)
                           (consp (cdr name)) (null (cddr name)))
                  (setf name (second name)))
                (typecase name (string (return name)) (symbol (return (symbol-name name)))))))))
      (unless (line-offset p -1)
        (return (when (equal "asd" (buffer-pathname-type (point-buffer point))) "ASDF-USER"))))))`);
    return source;
  });
  patch('repl', source => {
    source = form(source, '(defun listener-eval (string)', '(defun listener-eval (string) (local-listener-eval string))');
    source = form(source, '(define-command lisp-repl-interrupt ()', '(define-command lisp-repl-interrupt () () (local-interrupt))');
    source = form(source, '(defun repl-compute-context-menu-items ()', '(defun repl-compute-context-menu-items () nil)');
    source = replaceChecked(source, "(setf (variable-value 'completion-spec) 'repl-completion)",
                            "(setf (variable-value 'completion-spec) nil)", 'local REPL completion');
    source = form(source, '(defparameter *welcome-text*', `(defparameter *welcome-text*
  ";; MIRICL local Lisp session in the browser.
;; C-c C-c cancels local requests. Interactive input and the debugger UI
;; are unavailable; desktop process and socket operations are unavailable.

")`);
    return source;
  });
  patch('ext/eval', source => {
    source = form(source, '(defun eval-region (start end)', '(defun eval-region (start end) (local-eval-region start end))');
    source = form(source, '(defun compute-context-menu-items ()', `(defun compute-context-menu-items ()
  (list (lem/context-menu:make-item :label "Clear eval results"
         :callback (lambda (&rest args) (declare (ignore args)) (lisp-eval-clear)))))`);
    return source;
  });
  patch('ext/macroexpand', source => {
    // Select the genuine package and one-step command, without loading the
    // macrostep/source replacement closure. The provider owns async display.
    const selected = [form(source, '(defpackage :lem-lisp-mode/macroexpand'),
      form(source, '(in-package :lem-lisp-mode/macroexpand)'),
      form(form(source, '(defun macroexpand-internal (expander)'),
        '(defun macroexpand-internal (expander)',
        '(defun macroexpand-internal (expander)\n  (lem-lisp-mode/internal::local-macroexpand-1 expander))'),
      form(source, '(define-command lisp-macroexpand ()')];
    for (const [anchor, operation] of [
      ['(define-command lisp-macroexpand-in-place ()', 'in-place macroexpansion'],
      ['(define-command lisp-macroexpand-all ()', 'full or recursive macroexpansion'],
      ['(define-command lisp-macrostep-expand ()', 'lexical macrostep']]) {
      const original = form(source, anchor);
      selected.push(form(original, anchor, `${anchor} ()\n  (lem-lisp-mode/connection:unsupported-operation "${operation}"))`));
    }
    selected.push('(define-key *lisp-mode-keymap* "C-c C-m" \'lisp-macroexpand)');
    return ';;;; Selected genuine LEM one-step command; owned local display.\n' + selected.join('\n\n') + '\n';
  });
}
