;;;; Target array type facts. Callbacks expose target expansion/metadata;
;;;; host SUBTYPEP and host array representations are never consulted.
(in-package #:miricl-runtime)
(export '(array-type-query-value))

(defun array-type-query-value (a b state metadata)
  "STATE is #(mode flags rank environment), owned by the raw boundary.
Modes 0/2 describe a type/intersection, 1 normalizes a shape, 3 proves a subtype.
Descriptor result is #(flags rank shape); subtype result is 0/1/2 for unknown/
true/false. Shapes share valid input spines unless intersection requires merging."
  (declare (type simple-vector state) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) car cdr cons consp rplacd eq eql symbolp integerp
                   = < > <= >= + - * 1+ logand logior ash zerop make-array truncate))
  (values
   (block result
     (let ((mode (svref state 0)) (environment (svref state 3))
           (type a) (name nil) (arguments nil) (class nil) (element nil) (dimensions nil)
           (flags 0) (rank -1) (shape nil) (vector-mode 0) (count 0) (length-value nil)
           (stack nil) (frame nil) (scan nil) (axis nil)
           (product 1) (overflow nil) (wild nil) (size 0)
           (left nil) (right nil) (head nil) (tail nil) (cell nil) (x nil) (y nil)
           (af 0) (ar -1) (as nil) (phase 0) (parent-rank -1))
       (declare (type (integer 0 3) mode)
                (type (integer 0 255) flags af)
                (type (integer -1 #x7fffffff) rank ar parent-rank)
                (type (integer 0 2) vector-mode)
                (type (integer 0 #x1ffffffe) product size count)
                (type (integer 0 1) phase))
       (tagbody
          (if (= mode 1)
              (progn (setq flags (svref state 1) rank (svref state 2) shape a)
                     (go normalize-shape)))
        parse
          ;; Validate every spine read by target expansion and revalidate the
          ;; expansion result. Opaque MEMBER/EQL values are not traversed.
          (if (consp type)
              (if (funcall metadata 3 type nil) nil (funcall metadata 4 type 3)))
          (setq type (funcall metadata 0 type environment))
          (if (consp type)
              (if (funcall metadata 3 type nil) nil (funcall metadata 4 type 3)))
          (setq flags 0 rank -1 shape nil vector-mode 0 arguments nil)
          (if type nil (progn (setq flags 32) (go descriptor)))
          (if (consp type)
              (progn
                (setq name (car type) arguments (cdr type))
                (if (eq name 'and)
                    (progn
                      (setq scan stack)
                      (tagbody active-path
                         (if scan nil (go path-checked))
                         (if (eq type (svref (the simple-vector (car scan)) 0))
                             (funcall metadata 4 type 3))
                         (setq scan (cdr scan)) (go active-path)
                        path-checked)
                      (setq frame (make-array 8))
                      (setf (svref (the simple-vector frame) 0) type)
                      (setf (svref (the simple-vector frame) 1) arguments)
                      (setf (svref (the simple-vector frame) 2) 143)
                      (setf (svref (the simple-vector frame) 3) -1)
                      (setf (svref (the simple-vector frame) 4) nil)
                      (setf (svref (the simple-vector frame) 5) nil)
                      (setf (svref (the simple-vector frame) 6) nil)
                      (setq stack (cons frame stack))
                      (go next-conjunct))))
              (progn
                (setq name type class (funcall metadata 2 type nil))
                (if class (setq name (car class)))))
          (case name
            ((t) (setq flags 64) (go descriptor))
            (array (setq flags 143))
            (simple-array (setq flags 159))
            (vector (setq flags 143 vector-mode 1))
            (simple-vector (setq flags 17 vector-mode 2))
            (string (setq flags 10 vector-mode 2))
            (simple-string (setq flags 26 vector-mode 2))
            (base-string (setq flags 2 vector-mode 2))
            (simple-base-string (setq flags 18 vector-mode 2))
            (bit-vector (setq flags 4 vector-mode 2))
            (simple-bit-vector (setq flags 20 vector-mode 2)))
          (if (= flags 0) (go descriptor))
          (if (= vector-mode 0) nil (setq rank 1))
          (setq length-value (funcall metadata 3 arguments nil))
          (if length-value nil (funcall metadata 4 type 3))
          (setq count length-value)
          (if (> count (if (= vector-mode 2) 1 2)) (funcall metadata 4 type 1))
          (if (= vector-mode 2)
              (setq shape arguments)
              (progn
                (if arguments
                    (progn
                      (setq element (car arguments))
                      (if (eq element '*) nil
                          (progn
                            (if (consp element)
                                (if (funcall metadata 3 element nil) nil (funcall metadata 4 element 3)))
                            (setq element (funcall metadata 0 element environment))
                            (if (consp element)
                                (if (funcall metadata 3 element nil) nil (funcall metadata 4 element 3)))
                            (setq flags (logior (logand flags 16)
                                               (let ((kind (the (integer 0 4) (funcall metadata 1 element environment))))
                                                 (ash 1 (if (< kind 4) kind (+ kind 3))))))))))
                (if (= vector-mode 1)
                    (setq shape (cdr arguments))
                    (if (cdr arguments)
                        (progn
                          (setq dimensions (car (cdr arguments)))
                          (if (eq dimensions '*) nil
                              (if (integerp dimensions)
                                  (if (< dimensions 0) (funcall metadata 4 type 2)
                                      (if (>= dimensions 256)
                                          (progn (setq flags 32) (go descriptor))
                                          (setq rank dimensions)))
                                  (setq shape dimensions rank 0))))))))
        normalize-shape
          (if (>= rank 256) (progn (setq flags 32 shape nil) (go descriptor)))
          (if shape
              (progn
                (setq length-value (funcall metadata 3 shape nil))
                (if length-value nil (funcall metadata 4 shape 3))
                (setq count length-value)
                (if (>= count 256) (progn (setq flags 32 rank -1 shape nil) (go descriptor)))
                (setq scan shape count 0 product 1 overflow nil wild nil)
                (tagbody axis
                   (if scan nil (go shape-done))
                   (setq axis (car scan))
                   (if (eq axis '*)
                       (setq wild t)
                       (progn
                         (if (integerp axis) nil (funcall metadata 4 shape 2))
                         (if (< axis 0) (funcall metadata 4 shape 2))
                         (if (>= axis #x1fffffff)
                             (progn (setq flags 32 rank -1 shape nil) (go descriptor)))
                         (setq size axis)
                         (if (= size 0)
                             (setq product 0 overflow nil)
                             (if overflow nil
                                 (if (> product (truncate #x1ffffffe size))
                                     (setq overflow t)
                                     (setq product (* product size)))))))
                   (setq count (the (integer 0 #x1ffffffe) (1+ count)) scan (cdr scan))
                   (go axis)
                  shape-done)
                (setq rank count)
                (if overflow
                    (if wild nil (progn (setq flags 32 shape nil) (go descriptor))))))
        descriptor
          (if stack (go merge-conjunct))
          (go finish)
        next-conjunct
          (setq frame (car stack) scan (svref (the simple-vector frame) 1))
          (if scan
              (progn
                ;; A DEFTYPE expansion may mutate a previously validated spine.
                (if (funcall metadata 3 scan nil) nil (funcall metadata 4 scan 3))
                (setf (svref (the simple-vector frame) 1) (cdr scan))
                (setq type (car scan))
                (go parse)))
          (setq flags (svref (the simple-vector frame) 2)
                rank (svref (the simple-vector frame) 3)
                shape (svref (the simple-vector frame) 4))
          (if (svref (the simple-vector frame) 5)
              (setq flags 0 rank -1 shape nil)
              (if (svref (the simple-vector frame) 6) nil (setq flags 64 rank -1 shape nil)))
          (setq stack (cdr stack))
          (if (= flags 0) (go descriptor))
          (if (= flags 64) (go descriptor))
          (go normalize-shape)
        merge-conjunct
          (setq frame (car stack))
          (if (= flags 32)
              (progn
                (setq stack (cdr stack) rank -1 shape nil)
                (go descriptor)))
          (if (= flags 0)
              (progn (setf (svref (the simple-vector frame) 5) t) (go next-conjunct)))
          (if (= flags 64) (go next-conjunct))
          (setf (svref (the simple-vector frame) 6) t)
          (setq flags (logior (logand (logand (the (integer 0 255) (svref (the simple-vector frame) 2)) flags) 143)
                             (logand (logior (the (integer 0 255) (svref (the simple-vector frame) 2)) flags) 16)))
          (if (= (logand flags 143) 0) (go empty-intersection))
          (setq parent-rank (svref (the simple-vector frame) 3))
          (if (>= parent-rank 0)
              (if (>= rank 0) (if (= parent-rank rank) nil (go empty-intersection)))
              (setf (svref (the simple-vector frame) 3) rank))
          (setq left (svref (the simple-vector frame) 4))
          (if left
              (if shape
                  (progn
                    (setq right shape head nil tail nil)
                    (tagbody merge-axis
                       (if left nil (go merged))
                       (setq x (car left) y (car right))
                       (if (eq x '*) (setq x y)
                           (if (eq y '*) nil (if (eql x y) nil (go empty-intersection))))
                       (setq cell (cons x nil))
                       (if tail (rplacd (the cons tail) cell) (setq head cell))
                       (setq tail cell left (cdr left) right (cdr right))
                       (go merge-axis)
                      merged)
                    (setf (svref (the simple-vector frame) 4) head)))
              (setf (svref (the simple-vector frame) 4) shape))
          (setf (svref (the simple-vector frame) 2) flags)
          (go next-conjunct)
        empty-intersection
          (setq stack (cdr stack) flags 32 rank -1 shape nil)
          (go descriptor)
        finish
          (if (= mode 3)
              (progn
                (if (= phase 0)
                    (progn
                      (if (= flags 32) (return-from result 1))
                      (setq af flags ar rank as shape phase 1 type b)
                      (go parse)))
                (if (= flags 64) (return-from result 1))
                (if (= af 64) (if (= flags 0) nil (return-from result 2)))
                (if (= af 0) nil
                    (if (= flags 0) nil
                        (progn
                          (if (= flags 32) (return-from result 2))
                          (if (= (logand (logand af 143) flags) (logand af 143)) nil (return-from result 2))
                          (if (= (logand flags 16) 0) nil
                              (if (= (logand af 16) 0) (return-from result 2)))
                          (if (>= rank 0) (if (= ar rank) nil (return-from result 2)))
                          (tagbody subtype-axis
                             (if shape nil (go subtype-done))
                             (setq axis (car shape))
                             (if (eq axis '*) nil
                                 (if as (if (eql (car as) axis) nil (return-from result 2))
                                     (return-from result 2)))
                             (setq as (cdr as) shape (cdr shape))
                             (go subtype-axis)
                            subtype-done)
                          (return-from result 1))))
                (if (= af 0) nil
                    (progn
                      (setq name b class (funcall metadata 2 b nil))
                      (if class (setq name (car class)))
                      (if (eq name t) (return-from result 1))
                      (if (eq name 'atom) (return-from result (if (= af 64) 2 1)))
                      (if (eq name 'sequence) (return-from result (if (= ar 1) 1 2)))
                      (if class (if (cdr class) (return-from result 2)))
                      (if (consp name) (setq name (car name)))
                      (case name
                        ((boolean keyword standard-char base-char extended-char bit signed-byte unsigned-byte
                          compiled-function integer mod rational ratio float real single-float double-float short-float long-float
                          complex cons function eql member) (return-from result 2)))))
                (if (= flags 0) nil
                    (progn (setq class (funcall metadata 2 a nil))
                           (if class (if (cdr class) (return-from result 2)))))
                (return-from result 0)))
          (let ((answer (make-array 3)))
            (declare (type simple-vector answer))
            (setf (svref answer 0) flags)
            (setf (svref answer 1) rank)
            (setf (svref answer 2) shape)
            (return-from result answer)))))))

(in-package #:miricl-runtime)
(export '(check-type-control-value))

(defun check-type-control-value (type metadata)
  "Validate finite type-control spines without expanding types or running predicates.
MEMBER/EQL operands and arguments of opaque derived types are ordinary data;
they may themselves be circular. Return the original type without copying it."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons consp eq rplacd =))
  (values
   (let ((pending (cons (cons type nil) nil)) (active nil)
         (task nil) (node nil) (name nil) (arguments nil) (children nil) (scan nil))
     (tagbody next
        (if pending nil (go done))
        (setq task (car pending) pending (cdr pending) node (car task))
        (if (cdr task)
            (progn (setq active (cdr active)) (go next)))
        (if (consp node) nil (go next))
        (if (funcall metadata 3 node nil) nil (funcall metadata 4 node 3))
        (setq scan active)
        (tagbody path
           (if scan nil (go checked))
           (if (eq node (car scan)) (funcall metadata 4 node 3))
           (setq scan (cdr scan)) (go path)
          checked)
        (setq active (cons node active) pending (cons (cons node t) pending)
              name (car node) arguments (cdr node) children nil)
        (case name
          ((and or not cons values complex) (setq children arguments))
          ((array simple-array vector)
           (if arguments
               (progn
                 (setq children (cons (car arguments) nil))
                 (if (eq name 'vector) nil
                     (if (cdr arguments)
                         (let ((dimensions (car (cdr arguments))))
                           (if (consp dimensions)
                               (if (funcall metadata 3 dimensions nil) nil
                                   (funcall metadata 4 dimensions 3))))))))))
        (tagbody child
           (if children nil (go queued))
           (setq pending (cons (cons (car children) nil) pending)
                 children (cdr children))
           (go child)
          queued)
        (go next)
       done)
     type)))
