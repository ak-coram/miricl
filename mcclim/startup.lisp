(in-package #:mcclim-browser)

(defvar *demo-process* nil)
(defvar *demo-frame* nil)
(defun start-demo ()
  (when (and *demo-process* (miricl-threads:thread-alive-p *demo-process*))
    (when *demo-frame* (clim:raise-frame *demo-frame*))
    (return-from start-demo (values)))
  (setq *demo-frame* (clim-demo:demodemo :activate nil))
  (setq *demo-process*
    (clim-sys:make-process
      (lambda ()
        (handler-bind ((error (lambda (condition)
                        (js:invoke (js:global "console") "error" (princ-to-string condition)))))
          (clim:run-frame-top-level *demo-frame*)))
      :name "McCLIM demonstrations"))
  (values))
