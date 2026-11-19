;;;; MIRICL currently targets little-endian Wasm32.
(in-package #:cl-user)

(pushnew :little-endian *features*)
(pushnew :32-bit *features*)
