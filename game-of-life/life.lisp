(in-package :life)

;; Values are immutable by convention. Every edit and generation returns a new
;; world, sharing unchanged structure through FSet 2's persistent collections.
(defstruct (world (:constructor %world (width height cells generation)))
  width height cells generation)

(defun empty-world (&optional (width 24) (height 16))
  (%world width height (fset2:empty-set) 0))

(defun cell-index (world x y)
  (+ (mod x (world-width world)) (* (mod y (world-height world)) (world-width world))))

(defun paint (world x y &optional (alive t))
  (%world (world-width world) (world-height world)
          (funcall (if alive #'fset2:with #'fset2:less)
                   (world-cells world) (cell-index world x y))
          (world-generation world)))

(defun neighbors (world cell)
  (let ((x (mod cell (world-width world))) (y (floor cell (world-width world))))
    (loop for dy from -1 to 1 append
      (loop for dx from -1 to 1 unless (and (zerop dx) (zerop dy))
        collect (cell-index world (+ x dx) (+ y dy))))))

(defun next-generation (world)
  (let* ((cells (world-cells world))
         (candidates
           (fset2:reduce (lambda (set cell)
                           (reduce #'fset2:with (neighbors world cell)
                                   :initial-value (fset2:with set cell)))
                         cells :initial-value (fset2:empty-set)))
         (next (fset2:filter
                 (lambda (cell)
                   (let ((count (count-if (lambda (neighbor) (fset2:contains? cells neighbor))
                                          (neighbors world cell))))
                     (or (= count 3) (and (= count 2) (fset2:contains? cells cell)))))
                 candidates)))
    (%world (world-width world) (world-height world) next (1+ (world-generation world)))))

(defun seed (&optional (pattern :glider) (world (empty-world)))
  (let ((points (ecase pattern
                  (:glider '((1 0) (2 1) (0 2) (1 2) (2 2)))
                  (:blinker '((0 1) (1 1) (2 1)))
                  (:r-pentomino '((1 0) (2 0) (0 1) (1 1) (1 2)))))
        (x (- (floor (world-width world) 2) 1))
        (y (- (floor (world-height world) 2) 1)))
    (reduce (lambda (board point) (paint board (+ x (first point)) (+ y (second point))))
            points :initial-value (empty-world (world-width world) (world-height world)))))

(defun ascii-grid (&optional (world *world*))
  (with-output-to-string (out)
    (dotimes (y (world-height world))
      (dotimes (x (world-width world))
        (write-char (if (fset2:contains? (world-cells world) (cell-index world x y)) #\# #\.) out))
      (terpri out))))

(defun print-grid (&optional (world *world*) (stream *standard-output*))
  (write-string (ascii-grid world) stream)
  (values))

(defmethod print-object ((world world) stream)
  (format stream "#<Life ~Dx~D, generation ~D, ~D alive~%~A>"
          (world-width world) (world-height world) (world-generation world)
          (fset2:size (world-cells world)) (ascii-grid world)))

(defvar *world* (seed))
