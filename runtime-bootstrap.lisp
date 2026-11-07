;;;; Bootstrap identities shared by the guest and its transport.
(in-package #:miricl)

;; Shared with evaluator shortcuts that recognize captured native expanders.
(defconstant +clos-primitive-base+ 2147483500)

(defparameter *extension-symbols*
  '(register-source-file return-values quasiquote unquote unquote-splicing unquote-nsplicing *editor-function*
    unicode-char-upcase unicode-char-downcase unicode-string-upcase unicode-string-downcase
    unicode-string-capitalize unicode-nstring-upcase unicode-nstring-downcase unicode-nstring-capitalize))

;; IDs are part of the bootstrap wire protocol, not host function addresses.
(defparameter *runtime-symbols*
  '(nil t quote if progn let let* setq defun function lambda the locally declare
    + - * / = < > <= >= /= 1+ 1- abs numerator denominator complex realpart imagpart
    float typep zerop plusp minusp not numberp integerp rationalp realp complexp floatp
    cons car cdr list funcall expt gcd
    type integer rational real number single-float double-float short-float long-float
    fixnum bignum signed-byte unsigned-byte and or ratio
    &optional &rest &key &allow-other-keys &aux &body &whole &environment
    defpackage in-package make-package find-package list-all-packages rename-package delete-package
    package-name package-nicknames package-use-list package-used-by-list package-shadowing-symbols
    intern find-symbol unintern export unexport import shadow shadowing-import use-package unuse-package
    symbol-name symbol-package make-symbol find-all-symbols symbolp keywordp packagep string=
    eq eql equal values multiple-value-list multiple-value-bind nth-value *package*
    :internal :external :inherited :nicknames :use :shadow :shadowing-import-from
    :import-from :export :intern :size :documentation
    symbol package string base-string simple-string boolean null character
    do-symbols do-external-symbols do-all-symbols
    copy-symbol gensym gentemp symbol-function symbol-plist symbol-value get remprop
    boundp makunbound set fboundp fmakunbound fdefinition functionp special-operator-p
    keyword *gensym-counter* setf defvar defparameter special
    condition warning style-warning serious-condition error simple-condition simple-warning simple-error
    type-error simple-type-error program-error control-error cell-error unbound-variable undefined-function
    arithmetic-error division-by-zero floating-point-invalid-operation floating-point-overflow
    floating-point-underflow floating-point-inexact package-error parse-error reader-error stream-error
    end-of-file file-error storage-condition print-not-readable unbound-slot
    define-condition make-condition signal warn cerror invoke-debugger break
    simple-condition-format-control simple-condition-format-arguments type-error-datum type-error-expected-type
    cell-error-name arithmetic-error-operation arithmetic-error-operands package-error-package
    stream-error-stream file-error-pathname print-not-readable-object unbound-slot-instance
    handler-bind handler-case ignore-errors restart-bind restart-case with-simple-restart with-condition-restarts
    compute-restarts find-restart invoke-restart invoke-restart-interactively restart-name restart
    abort continue muffle-warning store-value use-value *debugger-hook* *break-on-signals*
    unwind-protect block return-from return catch throw assert check-type
    :format-control :format-arguments :datum :expected-type :name :operation :operands :package :stream
    :pathname :object :instance :initarg :initform :reader :accessor :report :default-initargs
    :report-function :interactive-function :test-function :interactive :test :no-error
    format write-string :writer :allocation :class :type
    loop loop-finish append nconc reverse rplaca rplacd length elt char endp))

;; Public object-system identities. MOP extensions live in MIRICL-MOP.
(defparameter *mop-symbols*
  '(ensure-class ensure-class-using-class class-direct-superclasses class-direct-subclasses
    class-precedence-list class-direct-slots class-slots class-finalized-p finalize-inheritance
    compute-class-precedence-list compute-slots compute-effective-slot-definition
    direct-slot-definition-class effective-slot-definition-class validate-superclass
    slot-definition slot-definition-name slot-definition-allocation slot-definition-initargs
    slot-definition-initform slot-definition-initfunction slot-definition-type
    slot-definition-readers slot-definition-writers slot-definition-location
    direct-slot-definition effective-slot-definition standard-slot-definition
    standard-direct-slot-definition standard-effective-slot-definition
    generic-function-name generic-function-lambda-list generic-function-methods
    generic-function-argument-precedence-order generic-function-method-class
    generic-function-method-combination method-function method-generic-function method-specializers
    method-lambda-list method-qualifiers specializer eql-specializer eql-specializer-object
    intern-eql-specializer specializer-direct-methods specializer-direct-generic-functions
    compute-discriminating-function compute-effective-method compute-applicable-methods-using-classes
    slot-value-using-class slot-boundp-using-class slot-makunbound-using-class
    standard-instance-access funcallable-standard-instance-access
    funcallable-standard-class funcallable-standard-object set-funcallable-instance-function
    forward-referenced-class add-direct-subclass remove-direct-subclass
    add-direct-method remove-direct-method ensure-generic-function-using-class))
(defparameter *clos-operators*
  '(defclass defgeneric defmethod defstruct with-slots with-accessors define-method-combination))
(setf *runtime-symbols*
  (append *runtime-symbols*
    '(defclass defgeneric defmethod defstruct with-slots with-accessors define-method-combination
      class-of class-name find-class make-instance allocate-instance initialize-instance
      reinitialize-instance shared-initialize change-class update-instance-for-redefined-class
      update-instance-for-different-class slot-value slot-boundp slot-exists-p slot-makunbound
      slot-missing slot-unbound make-instances-obsolete
      standard-object structure-object class standard-class built-in-class structure-class
      generic-function standard-generic-function method standard-method method-combination standard
      ensure-generic-function add-method remove-method find-method compute-applicable-methods
      call-next-method next-method-p no-next-method no-applicable-method call-method make-method
      type-of subtypep copy-structure documentation apply sequence array vector max min print-object
      :before :after :around :metaclass :direct-superclasses :direct-slots :direct-default-initargs
      :lambda-list :specializers :qualifiers :function :generic-function-class :method-class
      :argument-precedence-order :method-combination :initfunction :initargs :readers :writers
      :allocation :instance :constructor :predicate :copier :conc-name :include :read-only
      :named :initial-offset :print-function :print-object :most-specific-first :most-specific-last)
    (remove-duplicates *mop-symbols*)))
(setf *runtime-symbols* (remove-duplicates *runtime-symbols* :from-end t))

;; Development facilities are appended to preserve the existing builtin ranges.
(defparameter *development-functions*
  '(eval macro-function macroexpand-1 macroexpand load
    register-source-file read-from-string make-hash-table gethash remhash clrhash
    hash-table-count hash-table-p sxhash))
(defparameter *development-operators*
  '(defmacro macrolet flet labels destructuring-bind eval-when defconstant declaim))
(setf *runtime-symbols*
  (append *runtime-symbols*
    (remove-if (lambda (x) (member x *runtime-symbols*))
      (append *development-functions* *development-operators*
        '(cond when unless null consp atom listp stringp list* first second third fourth fifth
          rest caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
          cadddr cddddr copy-list copy-tree member assoc acons nth nthcdr last butlast
          mapcar mapc mapcan every some notany notevery complement identity constantly
          getf push pop incf decf dolist dotimes case ecase multiple-value-prog1
          multiple-value-call prog1 prog2 psetq do do* return-values
          *features* *load-pathname* *load-truename* *load-verbose* *load-print*
          *default-pathname-defaults* *read-eval* *read-suppress* *standard-output*
          *error-output* *trace-output* *compile-verbose* *compile-print*
          *print-circle* *print-readably* *print-length* *print-level*
          *print-pretty* *print-escape* *print-gensym* *print-case*
          *macroexpand-hook* optimize ftype inline notinline ignorable ignore dynamic-extent
          quasiquote unquote unquote-splicing unquote-nsplicing
          :execute :compile-toplevel :load-toplevel :if-does-not-exist :external-format
          :allow-other-keys :test :key :test-not :initial-value :start :end :from-end
          :count :rehash-size :rehash-threshold hash-table list-length nbutlast
          most-positive-fixnum most-negative-fixnum tailp)))))
;; Source loading uses portable pathnames in the mounted filesystem.
(setf *runtime-symbols* (append *runtime-symbols*
  '(pathname pathnamep namestring file-namestring directory-namestring
    pathname-name pathname-type pathname-directory make-pathname merge-pathnames
    probe-file truename streamp finish-output force-output terpri fresh-line
    prin1 princ print values-list nreverse count count-if count-if-not
    maphash copy-seq equalp get-setf-expansion nreconc revappend copy-alist
    make-list pairlis ldiff maplist mapl stream
    member-if member-if-not assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
    oddp evenp arrayp vectorp array-rank array-dimensions array-dimension
    array-total-size aref row-major-aref schar
    adjoin subsetp intersection set-difference union
    *compile-file-pathname* *compile-file-truename* compile-file-pathname directory
    string-equal :start1 :end1 :start2 :end2)))

(defparameter *string-comparison-functions*
  '(string= string/= string< string> string<= string>=
    string-equal string-not-equal string-lessp string-greaterp
    string-not-greaterp string-not-lessp))
(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (name) (member name *runtime-symbols*)) *string-comparison-functions*)))

(defparameter *character-comparison-functions*
  '(char= char/= char< char> char<= char>= char-equal char-not-equal
    char-lessp char-greaterp char-not-greaterp char-not-lessp))
(defparameter *character-functions*
  (append '(code-char char-code char-int characterp) *character-comparison-functions*))
(setf *runtime-symbols* (append *runtime-symbols* *character-functions*
  '(char-code-limit proclaim simple-base-string simple-array base-char standard-char extended-char satisfies
    make-string coerce concatenate subseq :initial-element :element-type array-dimension-limit
    char-upcase string-upcase nstring-upcase char-downcase string-downcase nstring-downcase
    digit-char digit-char-p standard-char-p upper-case-p lower-case-p both-case-p
    alpha-char-p alphanumericp graphic-char-p string-capitalize nstring-capitalize char-name name-char
    remove remove-if remove-if-not delete delete-if delete-if-not
    find find-if find-if-not position position-if position-if-not reduce)))

(defparameter *list-accessors*
  '(first second third fourth fifth rest
    caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    sixth seventh eighth ninth tenth))
(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (name) (member name *runtime-symbols*)) *list-accessors*)))
(defparameter *integer-bit-functions*
  '(ash integer-length logcount logbitp logtest lognot boole
    logand logior logxor logeqv lognand lognor logandc1 logandc2 logorc1 logorc2
    byte byte-size byte-position ldb ldb-test mask-field dpb deposit-field))
(setf *runtime-symbols* (append *runtime-symbols* *integer-bit-functions*
  '(boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2 boole-and boole-ior
    boole-xor boole-eqv boole-nand boole-nor boole-andc1 boole-andc2 boole-orc1 boole-orc2)))
(setf *runtime-symbols* (append *runtime-symbols* '(remove-duplicates delete-duplicates)))
;; The compact native accessor decoder uses these contiguous ID ranges.
(dolist (group '((consp atom listp stringp) (first second third fourth fifth) (caar cadr cdar cddr)
                 (caaar caadr cadar caddr cdaar cdadr cddar cdddr)
                 (cadddr cddddr)
                 (caaaar caaadr caadar caaddr cadaar cadadr caddar
                  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar)
                 (sixth seventh eighth ninth tenth)))
  (assert (loop for name in group for id from (position (car group) *runtime-symbols*)
                always (= id (position name *runtime-symbols*)))))

;; Builtins have real function objects, independent of their mutable symbol cells.
(defparameter *native-sequence-functions*
  '(count count-if count-if-not remove remove-if remove-if-not delete delete-if delete-if-not
    find find-if find-if-not position position-if position-if-not))
(dolist (group (list (subseq *native-sequence-functions* 0 3) (nthcdr 3 *native-sequence-functions*)))
  (assert (loop for name in group for offset from (position (car group) *runtime-symbols*)
                always (= offset (position name *runtime-symbols*)))))

(setf *runtime-symbols* (append *runtime-symbols*
  '(nunion nintersection nset-difference set-exclusive-or nset-exclusive-or)))
(setf *runtime-symbols* (append *runtime-symbols* '(simple-vector simple-vector-p svref make-sequence fill replace map)))

(defparameter *runtime-functions*
  '(+ - * / = < > <= >= /= 1+ 1- abs numerator denominator complex realpart imagpart
    float typep zerop plusp minusp not numberp integerp rationalp realp complexp floatp
    cons car cdr list funcall expt gcd rational
    make-package find-package list-all-packages rename-package delete-package
    package-name package-nicknames package-use-list package-used-by-list package-shadowing-symbols
    intern find-symbol unintern export unexport import shadow shadowing-import use-package unuse-package
    symbol-name symbol-package make-symbol find-all-symbols symbolp keywordp packagep string=
    eq eql equal values
    copy-symbol gensym gentemp symbol-function symbol-plist symbol-value get remprop
    boundp makunbound set fboundp fmakunbound fdefinition functionp special-operator-p
    make-condition signal error warn cerror invoke-debugger break
    simple-condition-format-control simple-condition-format-arguments type-error-datum type-error-expected-type
    cell-error-name arithmetic-error-operation arithmetic-error-operands package-error-package
    stream-error-stream file-error-pathname print-not-readable-object unbound-slot-instance
    compute-restarts find-restart invoke-restart invoke-restart-interactively restart-name
    abort continue muffle-warning store-value use-value format write-string
    append nconc reverse rplaca rplacd length elt char endp))
(setf *runtime-functions* (append *runtime-functions* *development-functions*))
(setf *runtime-functions* (union *runtime-functions* *string-comparison-functions*))
(setf *runtime-functions* (append *runtime-functions* *character-functions*))
(setf *runtime-functions* (append *runtime-functions* *native-sequence-functions*))
(setf *runtime-functions* (append *runtime-functions* *list-accessors*))
(setf *runtime-functions* (append *runtime-functions* *integer-bit-functions*))
(setf *runtime-functions* (append *runtime-functions* '(remove-duplicates delete-duplicates)))
(setf *runtime-symbols* (append *runtime-symbols*
  '(make-array adjust-array vector-push vector-pop vector-push-extend array-element-type adjustable-array-p array-has-fill-pointer-p array-displacement
    fill-pointer array-in-bounds-p array-row-major-index array-rank-limit array-total-size-limit
    call-arguments-limit lambda-parameters-limit :initial-contents :adjustable :fill-pointer :displaced-to :displaced-index-offset
    bit-vector simple-bit-vector bit-vector-p simple-bit-vector-p bit sbit upgraded-array-element-type simple-string-p tagbody go prog prog*
    readtable readtablep copy-readtable readtable-case *readtable* *read-base* *read-default-float-format*
    :upcase :downcase :preserve :invert :capitalize
    :array :base :case :circle :escape :gensym :length :level :lines :miser-width
    :pprint-dispatch :pretty :radix :readably :right-margin
    copy-pprint-dispatch pprint-dispatch set-pprint-dispatch with-standard-io-syntax
    *standard-input* *terminal-io* read read-preserving-whitespace :preserve-whitespace make-string-input-stream read-char read-char-no-hang peek-char unread-char read-line listen clear-input interactive-stream-p with-input-from-string
    read-sequence write-sequence write-line constantp make-string-output-stream get-output-stream-string with-output-to-string
    close open-stream-p input-stream-p output-stream-p stream-element-type string-stream clear-output write-char :abort
    write write-to-string prin1-to-string princ-to-string pprint
    *print-array* *print-base* *print-lines* *print-miser-width* *print-pprint-dispatch* *print-radix* *print-right-margin*)))
(defparameter *bit-array-functions*
  '((bit-and . 8) (bit-andc1 . 2) (bit-andc2 . 4) (bit-eqv . 9) (bit-ior . 14)
    (bit-nand . 7) (bit-nor . 1) (bit-orc1 . 11) (bit-orc2 . 13) (bit-xor . 6) (bit-not . 3)))
(setf *runtime-symbols* (append *runtime-symbols* (mapcar #'car *bit-array-functions*)))
(setf *runtime-functions* (append *runtime-functions* (mapcar #'car *bit-array-functions*)))
(setf *runtime-functions* (append *runtime-functions* '(readtablep copy-readtable readtable-case)))
(setf *runtime-functions* (append *runtime-functions* '(pprint-fill pprint-linear pprint-tabular copy-pprint-dispatch pprint-dispatch set-pprint-dispatch write write-to-string constantp write-char read-sequence write-sequence write-line
    make-string-input-stream read-char read-char-no-hang peek-char unread-char read-line listen clear-input read read-preserving-whitespace)))
(setf *runtime-functions* (append *runtime-functions* '(make-array adjust-array vector-push vector-pop vector-push-extend vector svref arrayp vectorp simple-vector-p simple-string-p bit-vector-p simple-bit-vector-p bit sbit upgraded-array-element-type array-element-type adjustable-array-p array-has-fill-pointer-p array-displacement fill-pointer array-in-bounds-p array-row-major-index aref row-major-aref schar array-rank array-dimensions array-dimension array-total-size)))
(setf *runtime-functions* (append *runtime-functions* '(null consp atom listp stringp identity string copy-seq subseq concatenate)))
(setf *runtime-functions* (append *runtime-functions*
  '(char-upcase char-downcase coerce maphash digit-char digit-char-p standard-char-p
    upper-case-p lower-case-p both-case-p alpha-char-p alphanumericp graphic-char-p char-name name-char oddp evenp equalp max min reduce)))

(defparameter *runtime-special-operators*
  '(quote if progn let let* setq function the locally unwind-protect block return-from catch throw tagbody go
    multiple-value-call multiple-value-prog1))
(setf *runtime-special-operators* (append *runtime-special-operators* '(flet labels macrolet eval-when)))

(defparameter *runtime-macros*
  '(lambda defun defpackage in-package multiple-value-list multiple-value-bind nth-value
    do-symbols do-external-symbols do-all-symbols setf defvar defparameter
    define-condition handler-bind handler-case ignore-errors restart-bind restart-case
    with-simple-restart with-condition-restarts return assert check-type loop loop-finish))
(setf *runtime-macros* (append *runtime-macros* '(defmacro destructuring-bind defconstant declaim and or)))

(setf *runtime-macros* (append *runtime-macros* *clos-operators*))

(defun runtime-symbol-id (symbol)
  (or (position symbol *runtime-symbols*) (error "Unknown bootstrap symbol ~S." symbol)))

(defparameter *runtime-object-tags*
  '((:null . 0) (:integer . 1) (:ratio . 2) (:single-float . 3)
    (:double-float . 4) (:complex . 5) (:symbol . 6) (:cons . 7)
    (:function . 8) (:binding . 9) (:string . 10) (:cell . 11)
    (:package . 12) (:package-entry . 13) (:values . 14) (:character . 16)
    (:condition . 17) (:condition-class . 18) (:restart . 19)
    (:handler . 20) (:control . 21) (:report-stream . 22) (:instance . 23)
    (:pathname . 28) (:hash-table . 29) (:vector . 34) (:array . 35) (:bit-vector . 36) (:readtable . 37) (:pprint-table . 38) (:input-stream . 39) (:two-way-stream . 40) (:synonym-stream . 41) (:random-state . 42) (:broadcast-stream . 43) (:file-stream . 44) (:concatenated-stream . 45) (:echo-stream . 46)))

(defun runtime-object-tag (name)
  (or (cdr (assoc name *runtime-object-tags*))
      (error "Unknown guest object kind ~S." name)))

(assert (every (lambda (entry) (typep (cdr entry) '(unsigned-byte 6))) *runtime-object-tags*))

(defparameter *runtime-error-names*
  #(:trap :division-by-zero :expected-number :unbound-variable
    :undefined-function :wrong-arity :declared-type :malformed-form
    :unsupported :malformed-type :allocation :invalid-float
    :invalid-package :package-conflict :expected-designator
    :reader-syntax :incomplete-input :terminal-io :condition :control-error :empty-vector :not-adjustable))

(defun runtime-error-code (name)
  (or (position name *runtime-error-names*)
      (error "Unknown guest error ~S." name)))

(defparameter *guest-errors*
  #("WASM execution trapped." "Division by zero." "Expected a numeric value of the required type."
    "Unbound variable." "Undefined function." "Wrong number of arguments."
    "Value does not satisfy its declared type." "Malformed form."
    "This operation or result is not supported yet." "Unsupported or malformed type specifier."
    "Guest memory allocation failed." "Floating-point overflow or invalid result."
    "Package or external symbol does not exist, or the package operation is invalid."
    "Package name or symbol conflict." "Expected a string, symbol, or package designator of the required type."
    "Malformed or unsupported reader syntax." "Incomplete input." "Terminal I/O failed."
    "Unhandled condition." "No matching active control target or restart."
    "Cannot pop a vector with a zero fill pointer." "Cannot extend an array that is not adjustable."))

;; Direct superclasses; guest classes and instances are independent of host CLOS.
(defparameter *runtime-condition-classes*
  '((condition) (warning condition) (style-warning warning) (serious-condition condition)
    (error serious-condition) (simple-condition condition)
    (simple-warning simple-condition warning) (simple-error simple-condition error)
    (type-error error) (simple-type-error simple-condition type-error)
    (program-error error) (control-error error) (cell-error error)
    (unbound-variable cell-error) (undefined-function cell-error)
    (arithmetic-error error) (division-by-zero arithmetic-error)
    (floating-point-invalid-operation arithmetic-error) (floating-point-overflow arithmetic-error)
    (floating-point-underflow arithmetic-error) (floating-point-inexact arithmetic-error)
    (package-error error) (parse-error error) (stream-error error)
    (reader-error parse-error stream-error) (end-of-file stream-error)
    (file-error error) (storage-condition serious-condition) (print-not-readable error)
    (unbound-slot cell-error)))

(defparameter *constructed-stream-functions*
  '(make-two-way-stream two-way-stream-input-stream two-way-stream-output-stream
    make-synonym-stream synonym-stream-symbol streamp open-stream-p input-stream-p
    output-stream-p stream-element-type interactive-stream-p))
(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (name) (member name *runtime-symbols*))
    (append '(two-way-stream synonym-stream *query-io* *debug-io*) *constructed-stream-functions*))))
(setf *runtime-functions* (append *runtime-functions* *constructed-stream-functions*))

(defparameter *float-property-functions*
  '(decode-float scale-float float-radix float-sign float-digits float-precision integer-decode-float))
(setf *runtime-symbols* (append *runtime-symbols* *float-property-functions*))
(setf *runtime-functions* (append *runtime-functions* *float-property-functions*))

(setf *runtime-symbols* (append *runtime-symbols*
  '(most-positive-short-float most-negative-short-float
    least-positive-short-float least-negative-short-float
    least-positive-normalized-short-float least-negative-normalized-short-float
    short-float-epsilon short-float-negative-epsilon
    most-positive-single-float most-negative-single-float
    least-positive-single-float least-negative-single-float
    least-positive-normalized-single-float least-negative-normalized-single-float
    single-float-epsilon single-float-negative-epsilon
    most-positive-double-float most-negative-double-float
    least-positive-double-float least-negative-double-float
    least-positive-normalized-double-float least-negative-normalized-double-float
    double-float-epsilon double-float-negative-epsilon
    most-positive-long-float most-negative-long-float
    least-positive-long-float least-negative-long-float
    least-positive-normalized-long-float least-negative-normalized-long-float
    long-float-epsilon long-float-negative-epsilon)))

(setf *runtime-symbols* (append *runtime-symbols* '(deftype)))

(setf *runtime-symbols* (append *runtime-symbols* '(mod)))

(defparameter *sequence-substitution-functions*
  '(substitute substitute-if substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not))
(setf *runtime-symbols* (append *runtime-symbols* *sequence-substitution-functions*))
(setf *runtime-functions* (append *runtime-functions* *sequence-substitution-functions*))

(setf *runtime-symbols* (append *runtime-symbols* '(mismatch search)))
(setf *runtime-functions* (append *runtime-functions* '(mismatch search)))

(setf *runtime-symbols* (append *runtime-symbols* '(map-into)))
(setf *runtime-functions* (append *runtime-functions* '(map-into)))

(setf *runtime-symbols* (append *runtime-symbols* '(otherwise)))

(defparameter *tree-functions*
  '(subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis tree-equal))
(setf *runtime-symbols* (append *runtime-symbols* *tree-functions*))
(setf *runtime-functions* (append *runtime-functions* *tree-functions*))

(setf *runtime-symbols* (append *runtime-symbols* '(sort stable-sort merge)))
(setf *runtime-functions* (append *runtime-functions* '(sort stable-sort merge)))

(defparameter *rounding-functions* '(floor ceiling truncate round ffloor fceiling ftruncate fround mod rem))
(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (name) (member name *runtime-symbols*)) *rounding-functions*)))
(setf *runtime-functions* (append *runtime-functions* *rounding-functions*))

(setf *runtime-symbols* (append *runtime-symbols* '(random make-random-state random-state-p random-state *random-state*)))
(setf *runtime-functions* (append *runtime-functions* '(random make-random-state random-state-p)))

(setf *runtime-symbols* (append *runtime-symbols*
  '(string-trim string-left-trim string-right-trim parse-integer :junk-allowed)))
(setf *runtime-functions* (append *runtime-functions*
  '(string-trim string-left-trim string-right-trim parse-integer)))

(setf *runtime-symbols* (append *runtime-symbols* '(lcm isqrt conjugate)))
(setf *runtime-functions* (append *runtime-functions* '(lcm isqrt conjugate)))

(setf *runtime-symbols* (append *runtime-symbols* '(typecase etypecase ccase ctypecase)))

(setf *runtime-symbols* (append *runtime-symbols* '(get-properties mapcon)))
(setf *runtime-functions* (append *runtime-functions* '(get-properties)))

(setf *runtime-symbols* (append *runtime-symbols* '(sqrt signum)))
(setf *runtime-functions* (append *runtime-functions* '(sqrt signum)))

(setf *runtime-symbols* (append *runtime-symbols* '(psetf rotatef shiftf multiple-value-setq)))
(setf *runtime-symbols* (append *runtime-symbols* '(remf)))
(setf *runtime-functions* (append *runtime-functions* '(getf)))
(setf *runtime-symbols* (append *runtime-symbols* '(pushnew)))

(setf *runtime-functions* (append *runtime-functions*
  '(adjoin subsetp intersection nintersection set-difference nset-difference union nunion set-exclusive-or nset-exclusive-or)))

(setf *runtime-symbols* (append *runtime-symbols* '(progv)))
(setf *development-operators* (append *development-operators* '(progv)))
(setf *runtime-special-operators* (append *runtime-special-operators* '(progv)))

(setf *runtime-symbols* (append *runtime-symbols* '(symbol-macrolet define-symbol-macro)))
(setf *development-operators* (append *development-operators* '(symbol-macrolet)))
(setf *runtime-special-operators* (append *runtime-special-operators* '(symbol-macrolet)))
(setf *runtime-macros* (append *runtime-macros* '(define-symbol-macro)))

(setf *runtime-symbols* (append *runtime-symbols* '(apropos apropos-list)))
(setf *runtime-functions* (append *runtime-functions* '(apropos-list)))

(setf *runtime-symbols* (append *runtime-symbols* '(define-compiler-macro compiler-macro-function compiler-macro)))
(setf *runtime-functions* (append *runtime-functions* '(compiler-macro-function)))
(setf *runtime-macros* (append *runtime-macros* '(define-compiler-macro)))

(setf *runtime-symbols* (append *runtime-symbols* '(pi)))

(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (name) (member name *runtime-symbols*))
    '(hash-table-test hash-table-size hash-table-rehash-size hash-table-rehash-threshold :rehash-size :rehash-threshold))))
(setf *runtime-functions* (append *runtime-functions*
  '(hash-table-test hash-table-size hash-table-rehash-size hash-table-rehash-threshold)))

(setf *runtime-symbols* (append *runtime-symbols* '(with-hash-table-iterator with-compilation-unit)))
(setf *runtime-macros* (append *runtime-macros* '(with-hash-table-iterator with-compilation-unit)))

(setf *runtime-symbols* (append *runtime-symbols* '(compiled-function compiled-function-p)))
(setf *runtime-functions* (append *runtime-functions* '(compiled-function-p)))
(setf *runtime-functions* (append *runtime-functions* '(constantly)))
(setf *runtime-functions* (append *runtime-functions* '(complement)))
(setf *runtime-functions* (append *runtime-functions* '(values-list)))
(setf *runtime-functions* (append *runtime-functions* '(apply)))
(setf *runtime-symbols* (append *runtime-symbols* '(function-lambda-expression)))
(setf *runtime-functions* (append *runtime-functions* '(function-lambda-expression)))
(setf *runtime-symbols* (append *runtime-symbols* '(*modules* provide require)))
(setf *runtime-symbols* (append *runtime-symbols* '(with-package-iterator)))
(setf *runtime-macros* (append *runtime-macros* '(with-package-iterator)))
(setf *runtime-symbols* (append *runtime-symbols* '(** *** ++ +++ // ///)))
(setf *runtime-symbols* (append *runtime-symbols* '(describe describe-object)))
(setf *runtime-symbols* (append *runtime-symbols*
  '(lisp-implementation-type lisp-implementation-version machine-type machine-version
    machine-instance software-type software-version short-site-name long-site-name)))
(setf *runtime-symbols* (append *runtime-symbols* '(make-load-form make-load-form-saving-slots)))
(setf *runtime-symbols* (append *runtime-symbols* '(formatter)))
(setf *runtime-macros* (append *runtime-macros* '(formatter)))
(setf *runtime-symbols* (append *runtime-symbols*
  '(pprint-logical-block pprint-pop pprint-exit-if-list-exhausted pprint-newline pprint-indent
    pprint-tab pprint-fill pprint-linear pprint-tabular :linear :fill :miser :mandatory :block :current
    :line :section :line-relative :section-relative :prefix :suffix :per-line-prefix)))
(setf *runtime-symbols* (remove-duplicates *runtime-symbols* :from-end t))
(setf *runtime-macros* (append *runtime-macros*
  '(pprint-logical-block pprint-pop pprint-exit-if-list-exhausted)))
(setf *runtime-symbols* (append *runtime-symbols* '(with-open-stream)))
(setf *runtime-macros* (append *runtime-macros* '(with-open-stream)))
(setf *runtime-symbols* (append *runtime-symbols* '(print-unreadable-object)))
(setf *runtime-macros* (append *runtime-macros* '(print-unreadable-object)))

(setf *runtime-symbols* (append *runtime-symbols* '(set-syntax-from-char)))
(setf *runtime-functions* (append *runtime-functions* '(set-syntax-from-char)))
(setf *runtime-symbols* (append *runtime-symbols* '(get-macro-character set-macro-character read-delimited-list)))
(setf *runtime-functions* (append *runtime-functions* '(get-macro-character set-macro-character read-delimited-list)))
(setf *runtime-symbols* (append *runtime-symbols* '(parse-namestring make-dispatch-macro-character get-dispatch-macro-character set-dispatch-macro-character)))
(setf *runtime-functions* (append *runtime-functions* '(make-dispatch-macro-character get-dispatch-macro-character set-dispatch-macro-character)))
(setf *runtime-symbols* (append *runtime-symbols* '(compile structure variable)))
(setf *runtime-symbols* (append *runtime-symbols* '(load-time-value)))
(setf *runtime-special-operators* (append *runtime-special-operators* '(load-time-value)))
(setf *runtime-symbols* (append *runtime-symbols* '(defsetf define-setf-expander define-modify-macro)))
(setf *runtime-macros* (append *runtime-macros* '(defsetf define-setf-expander define-modify-macro)))
(setf *runtime-symbols* (append *runtime-symbols* '(multiple-values-limit)))
(setf *runtime-symbols* (append *runtime-symbols* '(upgraded-complex-part-type)))
(setf *runtime-functions* (append *runtime-functions* '(upgraded-complex-part-type)))
(setf *runtime-symbols* (append *runtime-symbols* '(rationalize)))
(setf *runtime-functions* (append *runtime-functions* '(rationalize)))
(setf *runtime-functions* (append *runtime-functions* '(subtypep)))
(setf *runtime-functions* (append *runtime-functions* '(log phase atan)))
(setf *runtime-functions* (append *runtime-functions* '(sin cos tan cis exp sinh cosh tanh)))
(setf *runtime-functions* (append *runtime-functions* '(asin acos asinh acosh atanh)))
(setf *runtime-functions* (append *runtime-functions*
  '(make-concatenated-stream concatenated-stream-streams make-echo-stream echo-stream-input-stream echo-stream-output-stream
    make-broadcast-stream broadcast-stream-streams file-length file-position file-string-length stream-external-format read-byte write-byte)))
(setf *runtime-functions* (append *runtime-functions* '(fresh-line terpri)))
(setf *runtime-functions* (append *runtime-functions* '(class-of)))
(setf *runtime-symbols* (append *runtime-symbols* '(:default)))

;; A standard name belongs to COMMON-LISP even before its definition is
;; implemented. Preserve all existing IDs and make the added tail deterministic.
(defparameter *standard-common-lisp-symbols*
  (sort (loop for symbol being the external-symbols of :common-lisp collect symbol)
        #'string< :key #'symbol-name))
(setf *runtime-symbols* (append *runtime-symbols*
  (remove-if (lambda (symbol) (member symbol *runtime-symbols*)) *standard-common-lisp-symbols*)))
;; METHOD-QUALIFIERS is also specified by ANSI. Keep its existing identity,
;; but initialize its home in CL rather than treating it as a MOP extension.
(setf *mop-symbols* (set-difference *mop-symbols* *standard-common-lisp-symbols*))
(setf *runtime-symbols* (append *runtime-symbols* '(*editor-function*)))
(setf *runtime-symbols* (append *runtime-symbols*
  '(unicode-char-upcase unicode-char-downcase unicode-string-upcase unicode-string-downcase
    unicode-string-capitalize unicode-nstring-upcase unicode-nstring-downcase unicode-nstring-capitalize)))
(setf *runtime-functions* (append *runtime-functions* '(unicode-char-upcase unicode-char-downcase)))

;; Sparse readtable enumeration for implementation portability libraries.
(push 'readtable-macro-characters *extension-symbols*)
(setf *runtime-symbols* (append *runtime-symbols* '(readtable-macro-characters)))
(push 'readtable-macro-characters *runtime-functions*)
