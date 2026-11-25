(defvar *ansi-wasi-loaded-p* nil)
(progn
  (setq miricl-compiler:*compile-evaluations* nil
        *ansi-wasi-loaded-p* (load "/ansi/init.lsp"))
  (values))

;; COPY-TREE.2 checks the entire upstream universe. Use the upstream
;; identity checks with an explicit work list so this assertion helper
;; works on the default Wasmtime stack.
(defun cl-test::check-cons-copy (x y)
  (let ((pending (list (cons x y))))
    (loop while pending do
      (let* ((pair (pop pending))
             (original (car pair))
             (copy (cdr pair)))
        (if (consp original)
            (progn
              (unless (and (consp copy) (not (cl-test::eqt original copy)))
                (return-from cl-test::check-cons-copy nil))
              (push (cons (cdr original) (cdr copy)) pending)
              (push (cons (car original) (car copy)) pending))
            (unless (cl-test::eqt original copy)
              (return-from cl-test::check-cons-copy nil)))))
    t))

(let ((*package* (find-package "CL-TEST"))
      (*default-pathname-defaults* rt::*sandbox-path*)
      (entries (cdr rt::*entries*)))
  (assert (and *ansi-wasi-loaded-p* *package* *default-pathname-defaults* entries))
  (let ((passed (loop for entry in entries
                      count (progn
                              (format t "~%ANSI-WASI-TEST ~S~%" (rt::name entry))
                              (rt::do-entry entry)))))
    (format t "~%ANSI-WASI-RESULT ~D ~D~%" passed (length entries)))
  (values))
:quit
