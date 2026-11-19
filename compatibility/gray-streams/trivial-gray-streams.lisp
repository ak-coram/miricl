;;;; Thin signature adapters for the real portable Trivial Gray Streams system.
(in-package #:trivial-gray-streams)
(defmethod miricl-gray:stream-read-sequence
    ((stream fundamental-input-stream) sequence start end &key)
  (or-fallback (stream-read-sequence stream sequence start end)))
(defmethod miricl-gray:stream-write-sequence
    ((stream fundamental-output-stream) sequence start end &key)
  (or-fallback (stream-write-sequence stream sequence start end)))
(defmethod miricl-gray:stream-file-position
    ((stream fundamental-stream) &optional (position nil suppliedp))
  (if suppliedp (setf (stream-file-position stream) position)
      (stream-file-position stream)))
