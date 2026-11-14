;;;; Character semantics, including target scripts and nonadjacent duplicates.
(in-package #:miricl-test)
(in-suite :miricl)

;; The target's decimal repertoire is pinned to Unicode 15.1, whereas CCL's
;; DIGIT-CHAR-P recognizes only ASCII. Keep this slow reference independent of
;; the runtime's packed ranges and binary search. These are the zero characters
;; of all ten-character Nd runs in UnicodeData.txt (decimal value, field 6).
;; https://www.unicode.org/Public/15.1.0/ucd/UnicodeData.txt
(defparameter *reference-decimal-zero-codes*
  '(#x30 #x660 #x6f0 #x7c0 #x966 #x9e6 #xa66 #xae6 #xb66 #xbe6 #xc66 #xce6
    #xd66 #xde6 #xe50 #xed0 #xf20 #x1040 #x1090 #x17e0 #x1810 #x1946 #x19d0
    #x1a80 #x1a90 #x1b50 #x1bb0 #x1c40 #x1c50 #xa620 #xa8d0 #xa900 #xa9d0
    #xa9f0 #xaa50 #xabf0 #xff10 #x104a0 #x10d30 #x11066 #x110f0 #x11136
    #x111d0 #x112f0 #x11450 #x114d0 #x11650 #x116c0 #x11730 #x118e0 #x11950
    #x11c50 #x11d50 #x11da0 #x11f50 #x16a60 #x16ac0 #x16b50 #x1d7ce
    #x1d7d8 #x1d7e2 #x1d7ec #x1d7f6 #x1e140 #x1e2f0 #x1e4f0 #x1e950 #x1fbf0))

(defun reference-target-digit-char-p (character &optional (radix 10))
  (let* ((code (char-code character))
         (weight (or (loop for zero in *reference-decimal-zero-codes*
                           when (<= zero code (+ zero 9)) return (- code zero))
                     (cond ((char<= #\A character #\Z) (+ 10 (- code (char-code #\A))))
                           ((char<= #\a character #\z) (+ 10 (- code (char-code #\a))))))))
    (and weight (< weight radix) weight)))

(defun reference-target-alphanumericp (character)
  ;; Alphabetic scripts are still generated from this host's ALPHA-CHAR-P.
  (or (alpha-char-p character) (not (null (reference-target-digit-char-p character)))))

(defun reference-target-case (character direction)
  ;; Host case maps supply the target's repertoire, restricted to reciprocal
  ;; pairs for Common Lisp case operations. Do not query production helpers.
  (let* ((up-p (eq direction :up))
         (mapped (if up-p (char-upcase character) (char-downcase character))))
    (if (char= character (if up-p (char-downcase mapped) (char-upcase mapped)))
        mapped character)))

(defun reference-target-string-capitalize (text &key (start 0) end)
  ;; Determine each word start from the original bounded substring, without
  ;; using either the host's STRING-CAPITALIZE or the runtime's state machine.
  (let* ((source (string text)) (result (copy-seq source)))
    (loop for index from start below (or end (length result))
          for character = (char source index)
          when (reference-target-alphanumericp character) do
            (setf (char result index)
                  (reference-target-case character
                    (if (or (= index start)
                            (not (reference-target-alphanumericp (char source (1- index)))))
                        :up :down))))
    result))

(test native-character-upcase
  (m:with-interpreter (vm)
    (let ((characters (coerce (loop for code below (min char-code-limit #x110000)
                                   for char = (code-char code)
                                   when (and char (char/= char (char-upcase char))) collect char) 'string)))
      (is (equal (map 'string #'m::ansi-character-upcase characters) (m:evaluate vm `(string-upcase ,characters))))
      (is (equal (map 'string #'m::ansi-character-upcase characters) (m:evaluate vm
        `(coerce (mapcar #'char-upcase (coerce ,characters 'list)) 'string)))))
    (loop for code below 128 for char = (code-char code) do
      (is (eql (char-upcase char) (m:evaluate vm `(char-upcase ,char)))))
    (loop for start from 0 to 4 do
      (loop for end from start to 4 do
        (let ((form `(string-upcase "aBλ🙂" :start ,start :end ,end)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (dolist (form '((string-upcase 'abc) (string-upcase #\a) (string-upcase nil)
                    (string-upcase "abc" :start 0 :start 2 :end nil :end 0)
                    (string-upcase "abc" :unknown t :allow-other-keys t)
                    (let ((s (copy-seq "abc"))) (list (string-upcase s :start 1) s))
                    (let ((s (copy-seq "abc"))) (list (eq s (nstring-upcase s :end 2)) s))
                    (multiple-value-list (char-upcase #\a))
                    (multiple-value-list (string-upcase "abc"))
                    (multiple-value-list (nstring-upcase (copy-seq "abc")))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (source '("(char-upcase 1)" "(string-upcase 1)" "(nstring-upcase 'abc)"
                      "(string-upcase \"abc\" :start nil)"
                      "(string-upcase \"abc\" :start 2 :end 1)"
                      "(string-upcase \"abc\" :end 1000000000000000000000)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-character-downcase
  (m:with-interpreter (vm)
    (let ((characters (coerce (loop for code below (min char-code-limit #x110000)
                                   for char = (code-char code)
                                   when (and char (char/= char (char-downcase char))) collect char) 'string)))
      (is (equal (map 'string #'m::ansi-character-downcase characters) (m:evaluate vm `(string-downcase ,characters))))
      (is (equal (map 'string #'m::ansi-character-downcase characters) (m:evaluate vm
        `(coerce (mapcar #'char-downcase (coerce ,characters 'list)) 'string)))))
    (loop for code below 128 for char = (code-char code) do
      (is (eql (char-downcase char) (m:evaluate vm `(char-downcase ,char)))))
    (loop for start from 0 to 4 do
      (loop for end from start to 4 do
        (let ((form `(string-downcase "aBΛ🙂" :start ,start :end ,end)))
          (is (equal (eval form) (m:evaluate vm form))))))
    (dolist (form '((string-downcase 'ABC) (string-downcase #\A) (string-downcase nil)
                    (string-downcase "ABC" :start 0 :start 2 :end nil :end 0)
                    (string-downcase "ABC" :unknown t :allow-other-keys t)
                    (let ((s (copy-seq "ABC"))) (list (string-downcase s :start 1) s))
                    (let ((s (copy-seq "ABC"))) (list (eq s (nstring-downcase s :end 2)) s))
                    (multiple-value-list (char-downcase #\A))
                    (multiple-value-list (string-downcase "ABC"))
                    (multiple-value-list (nstring-downcase (copy-seq "ABC")))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (source '("(char-downcase 1)" "(string-downcase 1)" "(nstring-downcase 'ABC)"
                      "(string-downcase \"ABC\" :start nil)"
                      "(string-downcase \"ABC\" :start 2 :end 1)"
                      "(string-downcase \"ABC\" :end 1000000000000000000000)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-character-comparisons
  (m:with-interpreter (vm)
    (dolist (operator '(char= char/= char< char> char<= char>= char-equal char-not-equal
                        char-lessp char-greaterp char-not-greaterp char-not-lessp))
      (dolist (characters '((#\a) (#\a #\a) (#\a #\b) (#\b #\a) (#\a #\b #\a)
                            (#\a #\b #\c) (#\c #\b #\a) (#\a #\a #\b)
                            (#\a #\b #\A) (#\É #\é) (#\λ #\Λ) (#\𐐀 #\𐐨)
                            (#\a #\A #\b) (#\🙂 #\🙂)))
        (let ((form `(multiple-value-list (,operator ,@characters))))
          (is (equal (eval form) (m:evaluate vm form)) "Character mismatch for ~S" form)))
      (is (equal '(t 3) (m:evaluate vm
        `(let ((i 0))
           (list (equal (multiple-value-list
                          (,operator (progn (incf i) #\a) (progn (incf i) #\a)
                                     (progn (incf i) #\a)))
                        ',(multiple-value-list (funcall operator #\a #\a #\a))) i)))))
      (is (eq t (m:evaluate-string vm (format nil
        "(handler-case (~A) (program-error () t))" operator))))
      (dolist (arguments '("1" "#\\a #\\b nil" "#\\a #\\a 1" "#\\a \"a\""))
        (is (eq t (m:evaluate-string vm (format nil
          "(handler-case (~A ~A) (type-error (c)
            (not (typep (type-error-datum c) (type-error-expected-type c)))))"
          operator arguments))))))))

(test native-character-names
  (m:with-interpreter (vm)
    (dolist (entry '((32 . "Space") (10 . "Newline") (9 . "Tab") (12 . "Page")
                     (127 . "Rubout") (13 . "Return") (8 . "Backspace")))
      (is (equal (cdr entry) (m:evaluate vm `(char-name (code-char ,(car entry))))))
      (dolist (name (list (cdr entry) (string-upcase (cdr entry)) (string-downcase (cdr entry))))
        (is (eql (code-char (car entry)) (m:evaluate vm `(name-char ,name))))))
    (is (eq t (m:evaluate vm
      '(loop for code below 256 for c = (code-char code)
         always (or (graphic-char-p c)
                    (let ((name (char-name c)))
                      (and (stringp name) (eql c (name-char name))
                           (eql c (read-from-string (format nil "#\\~A" name))))))))))
    (is (eq t (m:evaluate vm
      '(loop for code below 256 for c = (code-char code)
         always (eql c (read-from-string (format nil "~S" c)))))))
    (dolist (form '((name-char 'space) (name-char :newline) (name-char "Linefeed")
                    (name-char "not-a-character-name")
                    (multiple-value-list (char-name #\Space))
                    (multiple-value-list (name-char "Tab"))))
      (is (equal (eval form) (m:evaluate vm form))))
    ;; Graphic characters need not have names; the guest does not name A.
    (is (null (m:evaluate vm '(name-char #\A))))
    (is (null (m:evaluate vm '(name-char ""))))
    (is (equal "Space" (m:evaluate-string vm
      "(let ((name (char-name #\\Space))) (setf (char name 0) #\\x) (char-name #\\Space))")))
    (dolist (source '("(char-name)" "(char-name #\\a nil)" "(name-char)" "(name-char \"Tab\" nil)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(char-name 1)" "(name-char 1)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-string-capitalization
  (m:with-interpreter (vm)
    (dolist (text '("" "abc" "ABC" "don't" "elm 13c arthur;fig don't"
                    "aB-ÉΛ Жx🙂AB" "中文aB ١A 2B"))
      (loop for start from 0 to (length text) do
        (loop for end from start to (length text) do
          (let ((form `(string-capitalize ,text :start ,start :end ,end)))
            (is (equal (reference-target-string-capitalize text :start start :end end)
                       (m:evaluate vm form)))))))
    ;; Digits start and continue words even on hosts which do not recognize
    ;; these target scripts. Punctuation still delimits words; START resets one.
    (dolist (entry '(("١A-١A" "١a-١a") ("A١B" "A١b") ("१A １B 𝟘C" "१a １b 𝟘c")))
      (is (equal (second entry) (reference-target-string-capitalize (first entry))))
      (is (equal (second entry) (m:evaluate vm `(string-capitalize ,(first entry)))))
      (is (equal (second entry) (m:evaluate vm `(nstring-capitalize (copy-seq ,(first entry)))))))
    (is (equal "١A" (m:evaluate vm '(string-capitalize "١a" :start 1))))
    (dolist (form '((string-capitalize 'kludgy-hash-search) (string-capitalize #\a)
                    (string-capitalize nil)
                    (string-capitalize "ABC" :start 0 :start 2 :end nil :end 0)
                    (string-capitalize "ABC" :unknown t :allow-other-keys t)
                    (let ((s (copy-seq "aBC DEf"))) (list (string-capitalize s :start 1) s))
                    (let ((s (copy-seq "aBC DEf"))) (list (eq s (nstring-capitalize s :end 5)) s))
                    (multiple-value-list (string-capitalize "ABC"))
                    (multiple-value-list (nstring-capitalize (copy-seq "ABC")))))
      (is (equal (eval form) (m:evaluate vm form))))
    (dolist (source '("(string-capitalize 1)" "(nstring-capitalize 'ABC)"
                      "(string-capitalize \"ABC\" :start nil)"
                      "(string-capitalize \"ABC\" :start 2 :end 1)"
                      "(string-capitalize \"ABC\" :end 1000000000000000000000)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-character-properties
  (m:with-interpreter (vm)
    (let* ((codes (append
             (loop for code below (min char-code-limit #x110000)
                   for char = (code-char code)
                   when (and char (or (< code 256) (char/= char (char-upcase char))
                                      (char/= char (char-downcase char)))) collect code)
             (loop for boundary in miricl::*character-alpha-ranges*
                   append (list (1- boundary) boundary (1+ boundary)))
             (loop for zero in *reference-decimal-zero-codes*
                   append (loop for code from (1- zero) to (+ zero 10) collect code))
             '(55295 57344 65535 65536 128578 1114111)))
           (characters (map 'string #'code-char (sort (remove-duplicates codes) #'<))))
      (dolist (predicate '(upper-case-p lower-case-p both-case-p alpha-char-p alphanumericp graphic-char-p))
        (is (equal (coerce (mapcar (lambda (c) (if
                           (case predicate
                             (upper-case-p (and (char= c (reference-target-case c :up))
                                                (char/= c (reference-target-case c :down))))
                             (lower-case-p (and (char= c (reference-target-case c :down))
                                                (char/= c (reference-target-case c :up))))
                             (both-case-p (char/= (reference-target-case c :up) (reference-target-case c :down)))
                             (alphanumericp (reference-target-alphanumericp c))
                             (otherwise (funcall predicate c))) #\1 #\0))
                                  (coerce characters 'list)) 'string)
          (m:evaluate vm `(coerce (mapcar (lambda (c) (if (,predicate c) #\1 #\0))
                                        (coerce ,characters 'list)) 'string))))
        (is (equal '(1) (m:evaluate vm `(multiple-value-list (length (multiple-value-list (,predicate #\a)))))))
        (dolist (arguments '(nil (1) (nil) (#\a #\b)))
          (is (eq t (m:evaluate vm
            `(handler-case (,predicate ,@arguments) (,(if (member (length arguments) '(0 2))
               'program-error 'type-error) () t))))))))))

(test reciprocal-and-unicode-case-mappings
  (m:with-interpreter (vm)
    (let ((characters (coerce (loop for code below (min char-code-limit #x110000)
                          for char = (code-char code)
                          when (and char (or (char/= char (char-upcase char))
                                             (char/= char (char-downcase char)))) collect char) 'string)))
      (is (equal (string-upcase characters) (m:evaluate vm `(m::unicode-string-upcase ,characters))))
      (is (equal (string-downcase characters) (m:evaluate vm `(m::unicode-string-downcase ,characters))))
      (is (equal (string-upcase characters) (m:evaluate vm
        `(map 'string #'m::unicode-char-upcase ,characters))))
      (is (equal (string-downcase characters) (m:evaluate vm
        `(map 'string #'m::unicode-char-downcase ,characters))))
      (is (eq t (m:evaluate vm `(loop for c across ,characters always
        (let ((up (char-upcase c)) (down (char-downcase c)))
          (and (eql down (char-downcase up)) (eql up (char-upcase down))
               (or (lower-case-p c) (eql c up)) (or (upper-case-p c) (eql c down))
               (eq (not (null (both-case-p c)))
                   (not (null (or (upper-case-p c) (lower-case-p c))))))))))))))

(test native-digit-characters
  (m:with-interpreter (vm)
    (let ((characters (coerce
            (loop for code below (min char-code-limit #x110000)
                  for char = (code-char code)
                  when (and char (or (< code 128) (digit-char-p char 36)
                                    (reference-target-digit-char-p char 36))) collect char) 'string))
          (weights (append (loop for i below 40 collect i) (list (expt 2 100)))))
      (loop for radix from 2 to 36 do
        (is (equal (mapcar (lambda (c) (reference-target-digit-char-p c radix)) (coerce characters 'list))
          (m:evaluate vm `(mapcar (lambda (c) (digit-char-p c ,radix)) (coerce ,characters 'list)))))
        (is (equal (mapcar (lambda (n) (digit-char n radix)) weights)
          (m:evaluate vm `(mapcar (lambda (n) (digit-char n ,radix)) ',weights))))))
    (dolist (form '((digit-char 0) (digit-char 10) (digit-char 12 16)
                    (digit-char-p #\a) (digit-char-p #\a 11)
                    (multiple-value-list (digit-char 3))
                    (multiple-value-list (digit-char-p #\0))
                    (multiple-value-list (standard-char-p #\Newline))
                    (let ((i 0)) (list (digit-char (incf i) (progn (incf i) 10)) i))))
      (is (equal (eval form) (m:evaluate vm form))))
    (let ((chars (loop for i below 128 collect (code-char i))))
      (is (equal (mapcar (lambda (c) (not (null (standard-char-p c)))) chars)
        (m:evaluate vm `(mapcar (lambda (c) (not (null (standard-char-p c)))) ',chars)))))
    (dolist (source '("(digit-char)" "(digit-char 0 10 nil)" "(digit-char-p)"
                      "(digit-char-p #\\a 10 nil)" "(standard-char-p)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (program-error () t))")))))
    (dolist (source '("(digit-char -1)" "(digit-char 1/2)" "(digit-char 1 1)"
                      "(digit-char-p #\\a 37)" "(digit-char-p #\\a 100000000000000000000000000)"
                      "(digit-char-p #\\a nil)" "(digit-char-p 3)" "(standard-char-p nil)"))
      (is (eq t (m:evaluate-string vm (concatenate 'string
        "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))))

(test native-character-codes
  (m:with-interpreter (vm)
    (dolist (code '(0 9 10 32 65 127 128 255 955 55295 57344 65535 65536 128578 1114111))
      (let ((form `(list (characterp (code-char ,code))
                         (char-code (code-char ,code)) (char-int (code-char ,code)))))
        (is (equal (eval form) (m:evaluate vm form)))))
    (is (equal '(nil nil nil nil) (m:evaluate vm
      '(list (code-char 55296) (code-char 57343) (characterp "a") (characterp nil)))))
    (is (= 1114112 (m:evaluate vm 'char-code-limit)))
    (is (equal '(65 1 1 1) (m:evaluate vm
      '(let ((i 0))
         (list (char-code (code-char (progn (incf i) 65))) i
               (length (multiple-value-list (code-char 65)))
               (length (multiple-value-list (characterp 0))))))))
    (dolist (source '("(code-char -1)" "(code-char 1114112)" "(code-char 1.0)"
                      "(code-char 1000000000000000000000)" "(char-code 65)" "(char-int nil)"))
      (is (eq t (m:evaluate-string vm
        (concatenate 'string "(handler-case " source " (type-error (c)
          (not (typep (type-error-datum c) (type-error-expected-type c)))))")))))
    (dolist (operator '(code-char char-code char-int characterp))
      (dolist (arguments '("" "nil nil"))
        (is (eq t (m:evaluate-string vm (format nil
          "(handler-case (~A ~A) (program-error () t))" operator arguments))))))))
