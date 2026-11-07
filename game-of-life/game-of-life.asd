(asdf:defsystem "game-of-life"
  :description "A small, functional Game of Life using the FSet 2 API."
  :depends-on ((:version "fset" "2.0.0"))
  :serial t
  :components ((:file "package") (:file "life") (:file "browser"))
  :build-operation "monolithic-compile-bundle-op"
  :build-pathname #P"build/game-of-life.wasm")
