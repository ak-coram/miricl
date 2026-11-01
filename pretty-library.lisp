;;;; Logical-block syntax and protocol; document layout runs in WASM.
(defparameter %pretty-depth 0)

(defmacro with-open-stream ((var stream) &body body)
  (let ((declarations nil) (abort (gensym)))
    (%%while (and (consp (car body)) (eq (caar body) 'declare))
      (push (pop body) declarations))
    (cons 'let (cons (list (list var stream) (list abort t))
      (append (reverse declarations)
        (list (list 'unwind-protect
          (list 'multiple-value-prog1 (cons 'progn body) (list 'setq abort nil))
          (list 'close var :abort abort))))))))

(defun %pretty-stream (stream)
  (cond ((null stream) *standard-output*) ((eq stream t) *terminal-io*) (t stream)))
(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (let ((item (gensym)) (target (gensym)) (typep (gensym)) (identityp (gensym)))
    (cons 'let* (cons (list (list item object) (list target (list '%pretty-stream stream))
                          (list typep type) (list identityp identity))
      (append
        (list (list 'when '*print-readably* (list 'error (list 'quote 'print-not-readable) :object item))
              (list 'write-string "#<" target)
              (list 'when typep (list 'princ (list 'type-of item) target) (list 'write-char #\Space target)))
        body
        (list (list 'when identityp
                (if body (list 'write-char #\Space target)
                  (list 'unless typep (list 'write-char #\Space target)))
                (list 'format target "{~X}" (list '%%object-identity item)))
              (list 'write-char #\> target) nil))))))
(defun pprint-newline (kind &optional stream)
  (let ((code (position kind '(:linear :fill :miser :mandatory))))
    (unless code (error 'type-error :datum kind :expected-type '(member :linear :fill :miser :mandatory)))
    (when *print-pretty* (%%pretty-event (%pretty-stream stream) (1+ code) 0 0))) nil)
(defun pprint-indent (relative-to n &optional stream)
  (unless (member relative-to '(:block :current)) (error 'error))
  (unless (realp n) (error 'type-error :datum n :expected-type 'real))
  (when *print-pretty*
    (%%pretty-event (%pretty-stream stream) (if (eq relative-to :block) 5 6) (truncate n) 0)) nil)
(defun pprint-tab (kind colnum colinc &optional stream)
  (let ((code (position kind '(:line :section :line-relative :section-relative))))
    (unless code (error 'error))
    (unless (typep colnum '(integer 0)) (error 'type-error :datum colnum :expected-type '(integer 0)))
    (unless (typep colinc '(integer 0)) (error 'type-error :datum colinc :expected-type '(integer 0)))
    (when *print-pretty* (%%pretty-event (%pretty-stream stream) (+ 7 code) colnum colinc))) nil)

(defmacro pprint-pop () (error 'program-error))
(defmacro pprint-exit-if-list-exhausted () (error 'program-error))
(defmacro pprint-logical-block
    ((stream-symbol object &key (prefix "" prefixp) (per-line-prefix "" perlinep) (suffix "")) &body body)
  (when (and prefixp perlinep) (error 'error))
  (unless (symbolp stream-symbol) (error 'program-error))
  (let* ((stream (cond ((null stream-symbol) '*standard-output*)
                       ((eq stream-symbol t) '*terminal-io*) (t stream-symbol)))
         (target (gensym)) (item (gensym)) (pre (gensym)) (suf (gensym))
         (destination (gensym)) (state (gensym)) (exit (gensym)) (value (gensym)) (done (gensym))
         (pop-form (list 'multiple-value-bind (list value done) (list '%%pretty-next state stream)
                     (list 'if done (list 'return-from exit nil) value)))
         (exit-form (list 'if (list 'null (list 'car state)) (list 'return-from exit nil) nil))
         (inside (list 'unwind-protect
                   (list 'block exit
                     (cons 'macrolet (cons
                       (list (list 'pprint-pop nil (list 'quote pop-form))
                             (list 'pprint-exit-if-list-exhausted nil (list 'quote exit-form))) body)))
                   (list '%%pretty-finish stream)))
         (callback (list 'lambda (list destination)
           (list 'cond
             (list (list 'and '(not *print-readably*) '*print-level* '(>= %pretty-depth *print-level*))
               (list 'write-string "#" destination))
             (list (list 'not (list 'listp item)) (list 'write item :stream destination))
             (list (list '%%pretty-reference item destination 0) nil)
             (list t (list 'let*
               (list (list stream (list '%%pretty-start destination pre suf (not (not perlinep))))
                     (list state (list 'list item 0 item)) (list '%pretty-depth '(1+ %pretty-depth))) inside))))))
    (list 'let* (list (list target (list '%pretty-stream stream)) (list item object)
                     (list pre (if perlinep per-line-prefix prefix)) (list suf suffix))
      (list 'unless (list 'stringp pre) (list 'error (list 'quote 'type-error) :datum pre :expected-type (list 'quote 'string)))
      (list 'unless (list 'stringp suf) (list 'error (list 'quote 'type-error) :datum suf :expected-type (list 'quote 'string)))
      (list '%%pretty-call (list 'function callback) target) nil)))

(defun %format-logical (stream object body prefix suffix perline)
  (if perline
    (pprint-logical-block (stream object :per-line-prefix prefix :suffix suffix)
      (%%format-logical-body stream body object))
    (pprint-logical-block (stream object :prefix prefix :suffix suffix)
      (%%format-logical-body stream body object))))

(defparameter %english-cardinals '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
  "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))
(defparameter %english-ordinals '("zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
  "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))
(defparameter %english-tens '("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))
(defparameter %english-tenth-ordinals '("" "" "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))
(defparameter %english-scales '("" "thousand" "million" "billion" "trillion" "quadrillion" "quintillion"
  "sextillion" "septillion" "octillion" "nonillion" "decillion" "undecillion" "duodecillion" "tredecillion"
  "quattuordecillion" "quindecillion" "sexdecillion" "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))
(defun %format-small-english (n ordinal stream)
  (cond ((< n 20) (write-string (nth n (if ordinal %english-ordinals %english-cardinals)) stream))
        ((< n 100)
         (multiple-value-bind (tens ones) (floor n 10)
           (write-string (nth tens (if (and ordinal (zerop ones)) %english-tenth-ordinals %english-tens)) stream)
           (unless (zerop ones) (write-char #\- stream) (%format-small-english ones ordinal stream))))
        (t (multiple-value-bind (hundreds rest) (floor n 100)
             (%format-small-english hundreds nil stream)
             (write-string (if (and ordinal (zerop rest)) " hundredth" " hundred") stream)
             (unless (zerop rest) (write-char #\Space stream) (%format-small-english rest ordinal stream))))))
(defun %format-english-number (number ordinal)
  (unless (integerp number) (error 'type-error :datum number :expected-type 'integer))
  (with-output-to-string (stream)
    (when (minusp number) (write-string "negative " stream) (setq number (- number)))
    (if (zerop number) (%format-small-english 0 ordinal stream)
      (let ((groups nil) (index 0))
        (%%while (plusp number)
          (multiple-value-bind (quotient remainder) (floor number 1000)
            (unless (zerop remainder) (push (cons remainder index) groups))
            (setq number quotient)) (incf index))
        (%%while groups
          (let* ((group (pop groups)) (last (not groups)) (scale (cdr group)))
            (%format-small-english (car group) (and ordinal last (zerop scale)) stream)
            (when (plusp scale)
              (if (< scale (length %english-scales))
                (progn (write-char #\Space stream) (write-string (nth scale %english-scales) stream))
                ;; Repeated thousands retain an English representation for
                ;; integers beyond the conventional short-scale vocabulary.
                (dotimes (i scale) (declare (ignore i)) (write-string " thousand" stream)))
              (when (and ordinal last) (write-string "th" stream)))
            (when groups (write-char #\Space stream))))))))
