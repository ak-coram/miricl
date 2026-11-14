;;;; Target real intervals above the checked numeric representation substrate.
(in-package #:miricl-runtime)
(export '(real-type-interval-value float-interval-bound-value real-subtype-query-value))

(defun real-type-interval-value (type coverage metadata)
  "Return #(lower upper open-flags representation-mask). NIL bounds are unbounded.
Mask bits 1/2/4 are rational/single/double; 8 empty, 16 integer, 32 ratio.
METADATA 0 returns target class metadata, 1 advances an IEEE float, 2 tests membership."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr consp symbolp eq = > realp logior))
  (values
   (block interval
     (let ((name type) (arguments nil) (lower nil) (upper nil) (flags 0) (mask 0)
           (class nil) (count nil) (bound nil) (position 0))
       (declare (type integer flags mask position))
       (if (consp type) (setq name (car type) arguments (cdr type))
           (if (symbolp type) nil
               (progn (setq class (funcall metadata 0 type nil))
                      (if class (setq name (car class))))))
       (case name
         ((nil) (setq mask 8))
         (rational (setq mask 1)) (real (setq mask 7)) (float (setq mask 6))
         ((short-float single-float) (setq mask 2))
         ((double-float long-float) (setq mask 4))
         ((integer fixnum bignum bit signed-byte unsigned-byte mod) (if coverage (setq mask 16)))
         (ratio (if coverage (setq mask 32)))
         (complex (if coverage (return-from interval (vector nil nil 0 8))))
         ((number atom t) (if coverage (setq mask 7))))
       (if (= mask 0) (return-from interval (vector nil nil 0 0)))
       ;; The boundary checks the whole control graph; check this mutable spine
       ;; again before traversing arguments or singleton bound wrappers.
       (setq count (funcall metadata 3 arguments nil))
       (if count nil (funcall metadata 4 type 3))
       (if (> count 2) (funcall metadata 4 type 1))
       (tagbody next
          (if arguments nil (go done))
          (setq bound (car arguments))
          (if (eq bound '*) (setq bound nil)
              (progn
                (if (consp bound)
                    (progn
                      (setq count (funcall metadata 3 bound nil))
                      (if count nil (funcall metadata 4 bound 3))
                      (if (= count 1) nil (funcall metadata 4 bound 1))
                      (setq bound (car bound) flags (logior flags (if (= position 0) 1 2)))))
                (if (realp bound) nil (error 'type-error :datum bound :expected-type 'real))))
          (if (= position 0) (setq lower bound) (setq upper bound))
          (setq position 1 arguments (cdr arguments)) (go next)
        done)
       (vector lower upper flags mask)))))

(defun float-interval-bound-value (bound state metadata)
  "STATE is #(exclusive format lower); format is 3 single or 4 double.
Return the extreme finite member; integer +/-1 denotes an out-of-range edge."
  (declare (type simple-vector state) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline svref = < > float -))
  (values
   (let* ((exclusive (svref state 0)) (kind (svref state 1)) (lower (svref state 2))
          (maximum (funcall metadata 5 kind nil))
          (value maximum))
     (if bound
         (progn
           (setq value (if (> bound maximum) maximum
                           (if (< bound (- maximum)) (- maximum) (float bound maximum))))
           (if (if lower (< value bound) (> value bound))
               (setq value (funcall metadata 1 value lower))
               (if (if exclusive (= value bound))
                   (setq value (funcall metadata 1 value lower))))
           value)
         (if lower (- maximum) maximum)))))

(defun real-subtype-query-value (a b metadata)
  "Return 0 unknown / 1 subtype / 2 not subtype. Inputs have checked type controls.
Shared parse and float-rounding continuations bound the sealed worker's size."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline svref car cdr consp symbolp eq realp = < > <= logand logior ash - 1+ 1-
                   integerp rational ceiling floor float))
  (values
   (block query
     (let ((type a) (coverage nil) (name nil) (arguments nil) (class nil) (count nil)
           (lower nil) (upper nil) (open-flags 0) (representation-mask 0) (position 0)
           (lo nil) (hi nil) (flags 0) (mask 0) (blo nil) (bhi nil) (bflags 0) (bmask 0)
           (integer-low nil) (integer-high nil) (kind 3) (bit 0)
           (low nil) (high nil) (target-low nil)
           (bound nil) (exclusive nil) (bound-lower nil) (maximum nil) (value nil) (continuation 0))
       (declare (type integer open-flags representation-mask position flags mask bflags bmask kind bit continuation))
       (tagbody
        parse
          (setq name type arguments nil lower nil upper nil open-flags 0 representation-mask 0 position 0)
          (if (consp type) (setq name (car type) arguments (cdr type))
              (if (symbolp type) nil
                  (progn (setq class (funcall metadata 0 type nil))
                         (if class (setq name (car class))))))
          (case name
            ((nil) (setq representation-mask 8))
            (rational (setq representation-mask 1)) (real (setq representation-mask 7)) (float (setq representation-mask 6))
            ((short-float single-float) (setq representation-mask 2))
            ((double-float long-float) (setq representation-mask 4))
            ((integer fixnum bignum bit signed-byte unsigned-byte mod) (if coverage (setq representation-mask 16)))
            (ratio (if coverage (setq representation-mask 32)))
            (complex (if coverage (progn (setq representation-mask 8) (go descriptor))))
            ((number atom t) (if coverage (setq representation-mask 7))))
          (if (= representation-mask 0) (return-from query 0))
          (setq count (funcall metadata 3 arguments nil))
          (if count nil (funcall metadata 4 type 3))
          (if (> count 2) (funcall metadata 4 type 1))
        parse-bound
          (if arguments nil (go descriptor))
          (setq bound (car arguments))
          (if (eq bound '*) (setq bound nil)
              (progn
                (if (consp bound)
                    (progn
                      (setq count (funcall metadata 3 bound nil))
                      (if count nil (funcall metadata 4 bound 3))
                      (if (= count 1) nil (funcall metadata 4 bound 1))
                      (setq bound (car bound) open-flags (logior open-flags (if (= position 0) 1 2)))))
                (if (realp bound) nil (error 'type-error :datum bound :expected-type 'real))))
          (if (= position 0) (setq lower bound) (setq upper bound))
          (setq position 1 arguments (cdr arguments)) (go parse-bound)
        descriptor
          (if coverage
              (setq blo lower bhi upper bflags open-flags bmask representation-mask)
              (progn (setq lo lower hi upper flags open-flags mask representation-mask type b coverage t) (go parse)))
          (if (= (logand mask 1) 0) (go rational-done))
          (if lo (if hi (if (if (> lo hi) t (if (= lo hi) (> flags 0))) (go rational-done))))
          (if (> (logand bmask 16) 0)
              (progn
                (if (if lo (if hi (= flags 0))) nil (return-from query 2))
                (if (= lo hi) nil (return-from query 2))
                (if (funcall metadata 2 (rational lo) b) nil (return-from query 2))
                (go rational-done)))
          (if (> (logand bmask 32) 0)
              (progn
                (if (if lo hi) nil (return-from query 2))
                (setq integer-low (rational lo) integer-high (rational hi))
                (setq integer-low (if (integerp integer-low)
                                      (if (> (logand flags 1) 0) (1+ integer-low) integer-low)
                                      (ceiling integer-low))
                      integer-high (if (integerp integer-high)
                                       (if (> (logand flags 2) 0) (1- integer-high) integer-high)
                                       (floor integer-high)))
                (if (> integer-low integer-high) nil (return-from query 2))
                (go rational-done)))
          (if (= (logand bmask 1) 0) (return-from query 2))
          (if blo
              (progn
                (if lo nil (return-from query 2))
                (if (< lo blo) (return-from query 2))
                (if (= lo blo) (if (> (logand bflags 1) 0) (if (= (logand flags 1) 0) (return-from query 2))))))
          (if bhi
              (progn
                (if hi nil (return-from query 2))
                (if (> hi bhi) (return-from query 2))
                (if (= hi bhi) (if (> (logand bflags 2) 0) (if (= (logand flags 2) 0) (return-from query 2))))))
        rational-done
        formats
          (setq bit (ash 1 (- kind 2)))
          (if (= (logand mask bit) 0) (go next-format))
          (setq bound lo exclusive (> (logand flags 1) 0) bound-lower t continuation 0) (go round-float)
        source-low
          (setq low value bound hi exclusive (> (logand flags 2) 0) bound-lower nil continuation 1) (go round-float)
        source-high
          (setq high value)
          ;; Only a lower edge above maximum or an upper edge below minimum
          ;; can be a sentinel. Both establish an empty source format.
          (if (if (integerp low) t (integerp high)) (go next-format))
          (if (<= low high) nil (go next-format))
          (if (= (logand bmask bit) 0) (return-from query 2))
          (setq bound blo exclusive (> (logand bflags 1) 0) bound-lower t continuation 2) (go round-float)
        target-low
          (setq target-low value)
          (if (integerp target-low) (return-from query 2))
          (if (< low target-low) (return-from query 2))
          (setq bound bhi exclusive (> (logand bflags 2) 0) bound-lower nil continuation 3) (go round-float)
        target-high
          (if (integerp value) (return-from query 2))
          (if (> high value) (return-from query 2))
        next-format
          (setq kind (1+ kind)) (if (<= kind 4) (go formats))
          (return-from query 1)
        round-float
          (setq maximum (funcall metadata 5 kind nil))
          (if bound
              (progn
                (setq value (if (> bound maximum) maximum
                                (if (< bound (- maximum)) (- maximum) (float bound maximum))))
                (if (if bound-lower (< value bound) (> value bound))
                    (setq value (funcall metadata 1 value bound-lower))
                    (if (if exclusive (= value bound)) (setq value (funcall metadata 1 value bound-lower)))))
              (setq value (if bound-lower (- maximum) maximum)))
          (case continuation
            (0 (go source-low)) (1 (go source-high)) (2 (go target-low)) (3 (go target-high))))))))
