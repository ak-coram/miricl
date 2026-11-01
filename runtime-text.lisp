;;;; Assemble the guest UTF-8 reader and printer.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-section text-runtime-wat ()
  (text-codec-wat)
  (text-number-wat)
  (text-labels-wat)
  (text-reader-wat)
  (text-arrays-wat)
  (text-printer-wat))
