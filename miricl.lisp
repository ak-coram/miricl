;;;; miricl.lisp

(in-package #:miricl)

(named-readtables:in-readtable :miricl-wat)

(defparameter *example*
  (wt:wat-to-wasm
   (format nil "~A"
           '#@(module
               (func $add (param $x i32) (param $y i32) (result i32)
                     (i32.add (local.get $x) (local.get $y)))))))
