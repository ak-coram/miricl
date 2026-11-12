(in-package #:mcclim-browser)

(defun install-window-drag (port sheet mirror handle resizep)
  (let ((start nil))
    (js:invoke handle "addEventListener" "pointerdown"
      (js:callback
        (lambda (event)
          (when (= (js-field event "button") 0)
            (js:invoke event "preventDefault")
            (js:invoke handle "setPointerCapture" (js-field event "pointerId"))
            (setf start (list (js-field event "clientX") (js-field event "clientY")
                             (mirror-x mirror) (mirror-y mirror)
                             (mirror-width mirror) (mirror-height mirror)))
            (enqueue-browser-event port (list :focus sheet))))))
    (js:invoke handle "addEventListener" "pointermove"
      (js:callback
        (lambda (event)
          (when start
            (destructuring-bind (sx sy x y width height) start
              (let ((dx (- (js-field event "clientX") sx))
                    (dy (- (js-field event "clientY") sy)))
                (if resizep
                    (setf width (max 80 (+ width dx)) height (max 40 (+ height dy)))
                    (setf x (max 0 (+ x dx))
                          y (max (mirror-decoration-height mirror) (+ y dy))))
                (enqueue-browser-event port (list :configure sheet x y width height))))))))
    (dolist (type '("pointerup" "pointercancel" "lostpointercapture"))
      (js:invoke handle "addEventListener" type
        (js:callback (lambda (event) (declare (ignore event)) (setf start nil)))))))

(defun install-window-controls (port sheet mirror)
  (let* ((container (mirror-element mirror))
         (header (element "div"))
         (title (element "span" header))
         (close (element "button" header))
         (resize (element "div" container)))
    (setf (mirror-decoration-height mirror) 24 (mirror-title mirror) title
          (js:property header "className") "mcclim-titlebar"
          (js:property title "textContent") (princ-to-string (clime:sheet-pretty-name sheet))
          (js:property close "textContent") "×"
          (js:property close "type") "button"
          (js:property close "ariaLabel") "Close window"
          (js:property resize "className") "mcclim-resize-handle")
    (js:invoke container "insertBefore" header (or (mirror-front-canvas mirror) (mirror-canvas mirror)))
    (css header "height" "24px")
    (css header "box-sizing" "border-box")
    (css header "display" "flex")
    (css header "align-items" "center")
    (css header "justify-content" "space-between")
    (css header "padding" "0 4px 0 8px")
    (css header "background" "#29294d")
    (css header "color" "#ffffea")
    (css header "font" "bold 13px monospace")
    (css header "cursor" "move")
    (css header "user-select" "none")
    (css header "touch-action" "none")
    (css close "font" "bold 18px monospace")
    (css close "line-height" "18px")
    (css close "cursor" "pointer")
    (css resize "position" "absolute")
    (css resize "right" "0") (css resize "bottom" "0")
    (css resize "width" "14px") (css resize "height" "14px")
    (css resize "cursor" "nwse-resize")
    (css resize "touch-action" "none")
    (css resize "background" "linear-gradient(135deg, transparent 65%, #666 66%, transparent 75%, #666 76%, transparent 85%, #666 86%)")
    (js:invoke close "addEventListener" "pointerdown"
      (js:callback (lambda (event) (js:invoke event "stopPropagation"))))
    (js:invoke close "addEventListener" "click"
      (js:callback (lambda (event)
        (js:invoke event "stopPropagation")
        (enqueue-browser-event port (list :close sheet)))))
    (install-window-drag port sheet mirror header nil)
    (install-window-drag port sheet mirror resize t)))

(defmethod adopt-frame :after ((manager browser-frame-manager) (frame application-frame))
  (let* ((sheet (frame-top-level-sheet frame)) (mirror (sheet-direct-mirror sheet)))
    (when (and mirror (plusp (mirror-decoration-height mirror))
               (null (slot-value frame 'climi::geometry-left))
               (null (slot-value frame 'climi::geometry-top)))
      (let ((offset (* 24 (mod (browser-stack (port manager)) 8))))
        (move-sheet sheet (+ 16 offset) (+ 40 offset))))))
