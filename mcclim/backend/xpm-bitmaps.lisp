;;;; Keep upstream's portable XPM parser alongside the browser codecs.
(in-package #:clim-internals)
(define-bitmap-file-reader :xpm (pathname) (xpm-parse-file pathname))
(define-bitmap-file-reader :pixmap (pathname) (xpm-parse-file pathname))
(define-bitmap-file-reader :pixmap-3 (pathname) (xpm-parse-file pathname))
