;;;; miricl-test.lisp

(defpackage #:miricl-test
  (:use #:cl #:fiveam))
(in-package #:miricl-test)

(def-suite :miricl)
(in-suite :miricl)

(test dummy
  (is (eql 1 1)))
