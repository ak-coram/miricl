;;;; Basic shared-backend checks; load the genuine backend and Gray files first.
;;;; Run through the async green-thread driver for the lock transfer check.
(defpackage #:lem-browser-ide-stream-tests
  (:use #:cl)
  (:export #:check-backend))
(in-package #:lem-browser-ide-stream-tests)

(defun check-lock-transfer (lock)
  ;; A recursive owner can reacquire even if an unwind leaked ownership.
  ;; A different thread's nonblocking acquisition detects that leak safely.
  (assert
   (miricl-threads:join-thread
    (miricl-threads:make-thread
     (lambda ()
       (when (miricl-threads:acquire-recursive-lock lock :wait nil)
         (miricl-threads:release-recursive-lock lock)
         t))
     :name "IDE lock transfer check"))))

(defun check-backend (backend-package gray-package)
  (flet ((backend (name)
           (or (find-symbol name backend-package)
               (error "Missing backend interface ~A in ~A" name backend-package))))
    (let* ((make-lock (backend "MAKE-LOCK"))
           (with-lock (backend "CALL-WITH-LOCK-HELD"))
           (make-input (backend "MAKE-INPUT-STREAM"))
           (make-output (backend "MAKE-OUTPUT-STREAM"))
           (lock (funcall make-lock :name "IDE stream check"))
           (condition (make-condition 'simple-error :format-control "Callback failure")))
      (assert (eq (find-package (funcall (backend "GRAY-PACKAGE-NAME")))
                  (find-package :miricl-gray)))
      (dolist (name '("FUNDAMENTAL-CHARACTER-INPUT-STREAM"
                      "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
                      "STREAM-READ-CHAR" "STREAM-WRITE-CHAR"))
        (assert (eq (find-symbol name gray-package)
                    (find-symbol name :miricl-gray))))
      (dolist (name '("GRAY-PACKAGE-NAME" "MAKE-LOCK" "CALL-WITH-LOCK-HELD"))
        (let ((symbol (backend name)))
          (assert (get symbol (find-symbol "IMPLEMENTATION" backend-package)))
          (assert (not (member symbol
                               (symbol-value (backend "*UNIMPLEMENTED-INTERFACES*")))))))
      (assert (miricl-threads:recursive-lock-p lock))
      (assert (equal (multiple-value-list
                      (funcall with-lock lock
                               (lambda ()
                                 (funcall with-lock lock (lambda () (values :one :two))))))
                     '(:one :two)))
      (assert (eq condition
                  (handler-case
                      (funcall with-lock lock (lambda () (error condition)))
                    (error (caught) caught))))
      (assert (eq :released (funcall with-lock lock (lambda () :released))))
      (check-lock-transfer lock)
      (let* ((chunks nil)
             (stream (funcall make-output (lambda (text) (push text chunks))))
             (text (copy-seq "xBCy")))
        (assert (char= #\A (write-char #\A stream)))
        (assert (eq text (write-string text stream :start 1 :end 3)))
        (format stream "|~A|" 42)
        (assert (fresh-line stream))
        (assert (not (fresh-line stream)))
        (finish-output stream)
        (assert (string= (format nil "ABC|42|~%")
                         (apply #'concatenate 'string (reverse chunks))))
        (assert (eql 0 (miricl-gray:stream-line-column stream)))
        (close stream)
        (assert (not (open-stream-p stream))))
      ;; A failed flush keeps the buffered text available and releases its lock.
      (let* ((fail t) (chunks nil)
             (stream (funcall make-output
                              (lambda (text)
                                (when fail (setf fail nil) (error condition))
                                (push text chunks)))))
        (write-string "pending" stream)
        (assert (eq condition (handler-case (finish-output stream)
                                (error (caught) caught))))
        (finish-output stream)
        (assert (equal chunks '("pending")))
        (close stream))
      (let* ((chunks (list (format nil "abc~%rest") "")) (calls 0)
             (stream (funcall make-input
                              (lambda ()
                                (incf calls)
                                (copy-seq (or (pop chunks) ""))))))
        (assert (not (listen stream)))
        (assert (null (read-char-no-hang stream nil :eof)))
        (assert (zerop calls))
        (assert (char= #\a (read-char stream)))
        (unread-char #\a stream)
        (assert (char= #\a (peek-char nil stream)))
        (assert (listen stream))
        (assert (char= #\a (read-char-no-hang stream)))
        (unread-char #\a stream)
        (assert (equal (multiple-value-list (read-line stream)) '("abc" nil)))
        (multiple-value-bind (line missing-newline) (read-line stream)
          (assert (string= line "rest"))
          (assert missing-newline))
        (assert (eq :eof (read-char stream nil :eof)))
        (multiple-value-bind (line missing-newline) (read-line stream nil :eof)
          (assert (eq line :eof))
          (assert missing-newline))
        (close stream))
      (let* ((chunks (list "(+ 20 22)" ""))
             (stream (funcall make-input
                              (lambda () (copy-seq (or (pop chunks) ""))))))
        (assert (equal (read stream) '(+ 20 22)))
        (close stream))
      t)))
