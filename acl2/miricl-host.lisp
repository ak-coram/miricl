;;;; Browser-compatible raw host services for the sequential ACL2 port.
(in-package "ACL2")

(defmacro without-interrupts (&body forms)
  `(miricl-threads:without-interrupts ,@forms))

(defmacro with-interrupts (&body forms)
  `(miricl-threads:with-interrupts ,@forms))

(defun system-call+ (string arguments)
  "Return the failure status/output pair when external processes are unavailable."
  (declare (ignore string arguments))
  ;; Match upstream's error path: a nonzero integer status and an empty string.
  ;; In particular, the optional Git revision query must not launch a process.
  (values 1 ""))
