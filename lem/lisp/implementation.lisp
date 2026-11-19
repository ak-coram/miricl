(defpackage #:lem-lisp-mode/implementation
  (:use #:cl)
  (:export #:default-command #:set-default-command #:get-usable-commands))
(in-package #:lem-lisp-mode/implementation)
(defun default-command () nil)
(defun get-usable-commands () nil)
(defun set-default-command (command)
  (declare (ignore command))
  (lem:editor-error "Launching desktop Lisp processes is unavailable in the browser."))
