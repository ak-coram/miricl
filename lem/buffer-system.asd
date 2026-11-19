;;; Selected upstream text-buffer subsystem; this is not LEM/CORE.
;;; Paths resolve beside the generated /lem/lem-browser.asd.
(defsystem "lem-browser/buffer"
  :description "LEM text buffers without the editor frontend or file services"
  :version "2.3.0"
  :depends-on ("uiop" "alexandria" "iterate" "trivia" "cl-ppcre"
               "babel" "str" "log4cl")
  :serial t
  :components ((:file "src/common/ring")
               (:file "src/common/hooks")
               (:file "src/common/var")
               (:file "src/common/utils")
               (:file "src/common/character/icon")
               (:file "src/common/character/eastasian")
               (:file "src/common/character/string-width-utils")
               (:file "src/common/character/package")
               (:file "src/buffer/errors")
               (:file "src/buffer/file-utils")
               (:file "src/buffer/line")
               (:file "src/buffer/buffer-list-manager")
               (:file "src/buffer/syntax-table")
               (:file "src/buffer/interrupt")
               (:file "src/buffer/package")
               (:file "src/buffer/internal/var")
               (:file "src/buffer/internal/editor-variables")
               (:file "src/buffer/internal/buffer")
               (:file "src/buffer/internal/point")
               (:file "src/buffer/internal/edit")
               (:file "src/buffer/internal/mark")
               (:file "src/buffer/internal/undo")
               (:file "src/buffer/internal/buffer-insert")
               (:file "src/buffer/internal/basic")))
