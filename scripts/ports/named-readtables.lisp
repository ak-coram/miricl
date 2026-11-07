#+miricl
(defun %make-readtable-iterator (readtable)
  (let ((characters (miricl:readtable-macro-characters readtable)))
    (lambda ()
      (when characters
        (let* ((character (pop characters))
               (function (get-macro-character character readtable)))
          (multiple-value-bind (dispatchp entries)
              (handler-case
                  (values t (mapcar (lambda (subchar)
                                      (cons subchar (get-dispatch-macro-character character subchar readtable)))
                                    (miricl:readtable-macro-characters readtable character)))
                (error () (values nil nil)))
            (values t character function dispatchp entries)))))))
