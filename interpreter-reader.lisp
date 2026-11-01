;;;; Use the host's list/string/number syntax while resolving symbols in WASM.
(in-package #:miricl)

(defvar *guest-backquote-depth* 0)

(defun guest-reader-token (interpreter stream first &key uninterned)
  (let ((raw (make-string-output-stream)) (name (make-string-output-stream))
        (colons nil) (bars nil) (escape nil) (escaped nil) (length 0))
    (labels ((consume (char)
               (write-char char raw)
               (cond (escape (write-char char name) (incf length) (setf escape nil escaped t))
                     ((char= char #\\) (setf escape t escaped t))
                     ((char= char #\|) (setf bars (not bars) escaped t))
                     (t (when (and (not bars) (char= char #\:)) (push length colons))
                        (write-char (if bars char (char-upcase char)) name) (incf length))))
             (delimiterp (char)
               (or (find char '(#\Space #\Tab #\Newline #\Return #\Page))
                   (multiple-value-bind (function nonterminating) (get-macro-character char *readtable*)
                     (and function (not nonterminating))))))
      (consume first)
      (loop for char = (peek-char nil stream nil nil t)
            while (and char (or bars escape (not (delimiterp char))))
            do (consume (read-char stream t nil t))))
    (when (or bars escape) (error 'end-of-file :stream stream))
    (when *read-suppress* (return-from guest-reader-token nil))
    (let ((raw (get-output-stream-string raw)) (name (get-output-stream-string name)))
      (when (and (not uninterned) (not escaped) (null colons)
                 (or (digit-char-p (char raw 0))
                     (and (> (length raw) 1) (find (char raw 0) "+-")
                          (or (digit-char-p (char raw 1)) (char= (char raw 1) #\.)))))
        (let ((*readtable* (copy-readtable nil)))
          (let ((number (read-from-string raw)))
            (when (numberp number) (return-from guest-reader-token number)))))
      (when uninterned
        (when colons (error "Package markers are not allowed after #: ."))
        (return-from guest-reader-token (guest-request interpreter "resolve-symbol" (list name nil) 2)))
      (setf colons (nreverse colons))
      (cond
        ((null colons) (guest-request interpreter "resolve-symbol" (list name nil) 0))
        ((and (= (length colons) 1) (zerop (first colons)))
         (guest-request interpreter "resolve-symbol" (list (subseq name 1) "KEYWORD") 0))
        ((or (and (= (length colons) 1) (plusp (first colons)))
             (and (= (length colons) 2) (plusp (first colons)) (= (second colons) (1+ (first colons)))))
         (guest-request interpreter "resolve-symbol"
                        (list (subseq name (+ (first colons) (length colons))) (subseq name 0 (first colons)))
                        (if (= (length colons) 1) 1 0)))
        (t (error 'interpreter-package-error :code 12 :message "Invalid package markers in symbol token."))))))

(defun make-interpreter-readtable (interpreter)
  (let ((table (copy-readtable nil)) (standard (copy-readtable nil)))
    (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!$%&*+-/<=>?@[]^_{}~:|\\" do
      (set-macro-character char (lambda (stream first) (values (guest-reader-token interpreter stream first))) t table))
    ;; CCL's character dispatch reader scans with the current syntax table.
    ;; Our symbol-token macros (including backslash) must not affect #\ names.
    (set-dispatch-macro-character #\# #\\
      (lambda (stream char argument)
        (let ((*readtable* standard))
          (funcall (get-dispatch-macro-character #\# #\\ standard) stream char argument))) table)
    (set-dispatch-macro-character #\# #\:
      (lambda (stream char argument)
        (declare (ignore char))
        (when argument (error "#: does not accept a numeric argument."))
        (peek-char t stream t nil t)
        (values (guest-reader-token interpreter stream (read-char stream t nil t) :uninterned t))) table)
    ;; Keep backquote as guest syntax rather than host-specific reader objects.
    (set-macro-character #\`
      (lambda (stream char)
        (declare (ignore char))
        (let ((*guest-backquote-depth* (1+ *guest-backquote-depth*)))
          (list 'quasiquote (read stream t nil t)))) nil table)
    (set-macro-character #\,
      (lambda (stream char)
        (declare (ignore char))
        (unless (or *read-suppress* (plusp *guest-backquote-depth*))
          (error "Comma outside backquote."))
        (let* ((next (peek-char nil stream nil nil t))
               (operator (cond ((eql next #\@) (read-char stream) 'unquote-splicing)
                               ((eql next #\.) (read-char stream) 'unquote-nsplicing)
                               (t 'unquote)))
               (*guest-backquote-depth* (1- *guest-backquote-depth*)))
          (list operator (read stream t nil t)))) nil table)
    table))

(defun read-form (interpreter stream &optional (eof-error-p t) eof-value)
  "Read a CL form whose symbol identities and package prefixes are resolved in WASM."
  (unless (interpreter-context interpreter) (error "Interpreter is closed."))
  (let ((*readtable* (or (interpreter-readtable interpreter)
                        (setf (interpreter-readtable interpreter) (make-interpreter-readtable interpreter))))
        (*read-eval* nil)
        (*guest-backquote-depth* 0))
    (read stream eof-error-p eof-value)))

(defun evaluate-string (interpreter source)
  "Read and evaluate SOURCE one form at a time, returning the last form's values."
  (with-input-from-string (input source)
    (let ((eof (gensym)) (values nil))
      (loop for form = (read-form interpreter input nil eof) until (eq form eof)
            do (setf values (multiple-value-list (evaluate interpreter form))))
      (values-list values))))

(defun load-source (interpreter pathname)
  "Read and evaluate a source file in the guest, honoring each IN-PACKAGE form."
  (with-open-file (input pathname)
    (let ((eof (gensym)) (values nil))
      (loop for form = (read-form interpreter input nil eof) until (eq form eof)
            do (setf values (multiple-value-list (evaluate interpreter form))))
      (values-list values))))
