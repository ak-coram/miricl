;; Keep the named-readtables runtime without its optional PAX documentation.
;; This edits only the guest overlay; Quicklisp's checkout stays untouched.
(let* ((file "/quicklisp/named-readtables/named-readtables.asd")
       (source (uiop:read-file-string file)))
  (unless (search "; MIRICL named-readtables runtime" source)
    (flet ((replace-fragment (before after)
             (let ((position (search before source)))
               (unless position
                 (error "The named-readtables system definition changed; update its MIRICL port (~S)." before))
               (setq source (concatenate 'string (subseq source 0 position) after
                              (subseq source (+ position (length before))))))))
      (replace-fragment ":depends-on (\"mgl-pax-bootstrap\")"
                        ":depends-on (#-miricl \"mgl-pax-bootstrap\")")
      (replace-fragment "(:file \"doc\")" "#-miricl (:file \"doc\")"))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "; MIRICL named-readtables runtime" out)
      (write-string source out))))
