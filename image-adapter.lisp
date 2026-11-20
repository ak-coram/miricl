;;;; Host distribution packaging; compiler and internal bootstrap images stay bare.
(in-package #:miricl)

(defvar *image-javascript-adapter-cache* nil)

(defun image-javascript-adapter-bytes ()
  ;; Content identity, rather than timestamps, also catches rapid edits and
  ;; replacement of source files while a persistent host compiler remains live.
  (let* ((names '("scripts/build-adapter.mjs" "scripts/miricl.mjs"
                  "scripts/dev-repl.mjs" "scripts/dev-filesystem.mjs"))
         (sources (mapcar (lambda (name)
                            (uiop:read-file-string (asdf:system-relative-pathname :miricl name)
                                                  :external-format :utf-8)) names)))
    (unless (equal sources (first *image-javascript-adapter-cache*))
      (uiop:with-temporary-file (:stream stream :pathname file :type "gz"
                                        :element-type '(unsigned-byte 8))
        (close stream)
        ;; Node's built-in gzip is packaging tooling, not a compiler pass or a
        ;; shipped dependency. Compression keeps the complete adapter compact.
        (uiop:run-program
          (list (or (uiop:getenv "NODE") "node")
                (namestring (asdf:system-relative-pathname :miricl "scripts/build-adapter.mjs"))
                "--embedded" (namestring file))
          :output *standard-output* :error-output *error-output*)
        (with-open-file (input file :element-type '(unsigned-byte 8))
          (let ((bytes (make-array (file-length input) :element-type '(unsigned-byte 8))))
            (read-sequence bytes input)
            (unless (and (> (length bytes) 2) (= (aref bytes 0) #x1f) (= (aref bytes 1) #x8b))
              (error "Adapter packager did not produce gzip bytes: ~A" file))
            (setf *image-javascript-adapter-cache* (list sources bytes))))))
    (second *image-javascript-adapter-cache*)))

(defun image-with-javascript-adapter (bytes)
  "Append the matching gzip adapter to a freshly built core runtime image."
  (unless (and (typep bytes '(vector (unsigned-byte 8))) (>= (length bytes) 8)
               (equalp (subseq bytes 0 8) #(0 97 115 109 1 0 0 0)))
    (error "Expected core Wasm bytes before embedding the JavaScript adapter."))
  (let ((position 8) (limit (length bytes)))
    (labels ((word (end)
               (let ((value 0))
                 (loop for shift from 0 below 35 by 7 do
                   (when (>= position end) (error "Truncated image section length at ~D." position))
                   (let ((byte (aref bytes position)))
                     (incf position)
                     (when (and (= shift 28) (> byte 15)) (error "Image section length exceeds u32."))
                     (incf value (ash (logand byte 127) shift))
                     (when (< byte 128) (return-from word value))))
                 (error "Invalid image section length."))))
      (loop while (< position limit) do
        (let* ((kind (aref bytes position))
               (length (progn (incf position) (word limit)))
               (end (+ position length)))
          (when (> end limit) (error "Image section exceeds its container at ~D." position))
          (when (zerop kind)
            (let ((length (word end)) (name "miricl.adapter.gz"))
              (when (> length (- end position)) (error "Truncated image custom section name."))
              (when (and (= length (length name))
                         (loop for char across name for index from position
                               always (= (char-code char) (aref bytes index))))
                (error "Image already has an embedded JavaScript adapter."))))
          (setf position end)))))
  (wasm-with-custom-section bytes "miricl.adapter.gz" (image-javascript-adapter-bytes)))
