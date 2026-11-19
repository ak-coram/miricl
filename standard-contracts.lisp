;;;; Target ANSI function identities, signatures and conservative semantic facts.
(in-package :miricl-compiler)

;; Authority: CLHS alphabetical symbol index and the Function, Accessor and
;; Standard Generic Function entries it references:
;; https://www.lispworks.com/documentation/HyperSpec/Front/X_AllSym.htm
;; This is an explicit language inventory, independent of host FBOUNDP, host
;; function types, target dispatch support and COMMON-LISP package membership.
;; Macros, special operators, types and variables are not function definitions.
;; A descriptor is an opaque list of shared fact plists, newest facts first.
;; Missing facts mean UNKNOWN, never false or permission to remove a call.
(defparameter *standard-function-contracts*
  (let ((table (make-hash-table :test 'eq)))
    (dolist
        (group
         '(((:kind :function)
            * + - / /= 1+ 1- < <= = > >=
            abort abs acons acos acosh adjoin adjust-array adjustable-array-p
            alpha-char-p alphanumericp append apply apropos apropos-list
            arithmetic-error-operands arithmetic-error-operation array-dimension
            array-dimensions array-displacement array-element-type
            array-has-fill-pointer-p array-in-bounds-p array-rank
            array-row-major-index array-total-size arrayp ash asin asinh
            assoc assoc-if assoc-if-not atan atanh atom
            bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor bit-not
            bit-orc1 bit-orc2 bit-vector-p bit-xor boole both-case-p boundp break
            broadcast-stream-streams butlast byte byte-position byte-size
            ceiling cell-error-name cerror char-code char-downcase char-equal
            char-greaterp char-int char-lessp char-name char-not-equal
            char-not-greaterp char-not-lessp char-upcase char/= char< char<=
            char= char> char>= character characterp cis class-of
            clear-input clear-output close clrhash code-char coerce compile
            compile-file compile-file-pathname compiled-function-p complement
            complex complexp compute-restarts concatenate
            concatenated-stream-streams conjugate cons consp constantly constantp
            continue copy-alist copy-list copy-pprint-dispatch copy-readtable
            copy-seq copy-structure copy-symbol copy-tree cos cosh
            count count-if count-if-not decode-float decode-universal-time
            delete delete-duplicates delete-file delete-if delete-if-not
            delete-package denominator deposit-field describe digit-char
            digit-char-p directory directory-namestring disassemble dpb dribble
            echo-stream-input-stream echo-stream-output-stream ed
            encode-universal-time endp enough-namestring ensure-directories-exist
            ensure-generic-function eq eql equal equalp error eval evenp every
            exp export expt fboundp fceiling ffloor file-author file-error-pathname
            file-length file-namestring file-position file-string-length
            file-write-date fill find find-all-symbols find-if find-if-not
            find-package find-restart find-symbol finish-output float float-digits
            float-precision float-radix float-sign floatp floor fmakunbound
            force-output format fresh-line fround ftruncate funcall
            function-lambda-expression functionp gcd gensym gentemp
            get-decoded-time get-dispatch-macro-character get-internal-real-time
            get-internal-run-time get-macro-character get-output-stream-string
            get-properties get-setf-expansion get-universal-time getf
            graphic-char-p hash-table-count hash-table-p hash-table-rehash-size
            hash-table-rehash-threshold hash-table-size hash-table-test
            host-namestring identity imagpart import input-stream-p inspect
            integer-decode-float integer-length integerp interactive-stream-p
            intern intersection invalid-method-error invoke-debugger invoke-restart
            invoke-restart-interactively isqrt keywordp last lcm ldb ldb-test ldiff
            length lisp-implementation-type lisp-implementation-version list list*
            list-all-packages list-length listen listp load
            load-logical-pathname-translations log logand logandc1 logandc2
            logbitp logcount logeqv logical-pathname logior lognand lognor lognot
            logorc1 logorc2 logtest logxor long-site-name lower-case-p
            machine-instance machine-type machine-version macroexpand macroexpand-1
            make-array make-broadcast-stream make-concatenated-stream make-condition
            make-dispatch-macro-character make-echo-stream make-hash-table make-list
            make-load-form-saving-slots make-package make-pathname make-random-state
            make-sequence make-string make-string-input-stream make-string-output-stream
            make-symbol make-synonym-stream make-two-way-stream makunbound
            map map-into mapc mapcan mapcar mapcon maphash mapl maplist mask-field
            max member member-if member-if-not merge merge-pathnames
            method-combination-error min minusp mismatch mod
            muffle-warning name-char namestring nbutlast nconc nintersection not
            notany notevery nreconc nreverse nset-difference nset-exclusive-or
            nstring-capitalize nstring-downcase nstring-upcase nsublis nsubst
            nsubst-if nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not
            nthcdr null numberp numerator nunion oddp open open-stream-p
            output-stream-p package-error-package package-name package-nicknames
            package-shadowing-symbols package-use-list package-used-by-list
            packagep pairlis parse-integer parse-namestring pathname pathname-device
            pathname-directory pathname-host pathname-match-p pathname-name
            pathname-type pathname-version pathnamep peek-char phase plusp position
            position-if position-if-not pprint pprint-dispatch pprint-fill pprint-indent
            pprint-linear pprint-newline pprint-tab pprint-tabular prin1 prin1-to-string
            princ princ-to-string print print-not-readable-object probe-file proclaim
            provide random random-state-p rassoc rassoc-if rassoc-if-not rational
            rationalize rationalp read read-byte read-char read-char-no-hang
            read-delimited-list read-from-string read-line read-preserving-whitespace
            read-sequence readtablep realp realpart reduce rem remhash remove
            remove-duplicates remove-if remove-if-not remprop rename-file rename-package
            replace require restart-name revappend reverse room round rplaca rplacd
            scale-float search set set-difference set-dispatch-macro-character
            set-exclusive-or set-macro-character set-pprint-dispatch set-syntax-from-char
            shadow shadowing-import short-site-name signal signum simple-bit-vector-p
            simple-condition-format-arguments simple-condition-format-control
            simple-string-p simple-vector-p sin sinh sleep slot-boundp slot-exists-p
            slot-makunbound software-type software-version some sort special-operator-p
            sqrt stable-sort standard-char-p store-value stream-element-type
            stream-error-stream stream-external-format streamp string string-capitalize
            string-downcase string-equal string-greaterp string-left-trim string-lessp
            string-not-equal string-not-greaterp string-not-lessp string-right-trim
            string-trim string-upcase string/= string< string<= string= string> string>=
            stringp sublis subsetp subst subst-if subst-if-not substitute substitute-if
            substitute-if-not subtypep sxhash symbol-name symbol-package symbolp
            synonym-stream-symbol tailp tan tanh terpri translate-logical-pathname
            translate-pathname tree-equal truename truncate two-way-stream-input-stream
            two-way-stream-output-stream type-error-datum type-error-expected-type
            type-of typep unbound-slot-instance unexport unintern union unread-char
            unuse-package upgraded-array-element-type upgraded-complex-part-type
            upper-case-p use-package use-value user-homedir-pathname values values-list
            vector vector-pop vector-push vector-push-extend vectorp warn wild-pathname-p
            write write-byte write-char write-line write-sequence write-string
            write-to-string y-or-n-p yes-or-no-p zerop)
           ;; CLHS Accessor entries, and Figure 5-7. SETF-able place syntax
           ;; does not require a function named (SETF accessor); see below.
           ((:kind :accessor :setf-place t)
            aref bit caaaar caaadr caaar caadar caaddr caadr caar
            cadaar cadadr cadar caddar cadddr caddr cadr car
            cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar
            cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
            char compiler-macro-function eighth elt fdefinition fifth fill-pointer
            find-class first fourth get gethash logical-pathname-translations
            macro-function ninth nth readtable-case rest row-major-aref sbit schar
            second seventh sixth slot-value subseq svref symbol-function symbol-plist
            symbol-value tenth third)
           ;; Standard generic functions retain all permitted user methods.
           ((:kind :generic-function :callbacks t)
            add-method allocate-instance change-class class-name compute-applicable-methods
            describe-object documentation find-method function-keywords
            initialize-instance make-instance make-instances-obsolete make-load-form
            method-qualifiers
            no-applicable-method no-next-method print-object reinitialize-instance
            remove-method shared-initialize slot-missing slot-unbound
            update-instance-for-different-class update-instance-for-redefined-class)
           ;; These names describe lexically supplied method functions, not
           ;; fixed global runtime entries. Global FBOUNDP is unspecified.
           ((:kind :local-function :callbacks t)
            call-next-method next-method-p)))
      (let ((descriptor (list (car group))))
        (dolist (name (cdr group))
          (setf (gethash name table) descriptor))))
    ;; Audited partial facts. Facts in a group are shared, with one cons per
    ;; participating name. Keep unknown operations in the inventory even when
    ;; none of these facts applies. In particular there is no default count,
    ;; purity, checked entry, unchecked worker, arity or keyword convention.
    (dolist
        (group
         '(;; CLHS unary type predicate entries: ARRAYP, ATOM, BIT-VECTOR-P,
           ;; CHARACTERP, COMPILED-FUNCTION-P, COMPLEXP, CONSP, FLOATP,
           ;; FUNCTIONP, HASH-TABLE-P, INTEGERP, KEYWORDP, LISTP, NUMBERP,
           ;; PACKAGEP, PATHNAMEP, RANDOM-STATE-P, RATIONALP, READTABLEP,
           ;; REALP, SIMPLE-BIT-VECTOR-P, SIMPLE-STRING-P, SIMPLE-VECTOR-P,
           ;; STREAMP, STRINGP, SYMBOLP, VECTORP; also f_not.htm, f_null.htm.
           ;; A generalized boolean is T as a *type*, not CL:BOOLEAN. Its
           ;; non-NIL value is not specified to be the canonical symbol T.
           ((:result-count 1 :arity (1 . 1) :reads nil :writes nil
             :callbacks nil :may-signal nil :allocation nil :truth-result t)
            arrayp atom bit-vector-p characterp compiled-function-p complexp consp
            floatp functionp hash-table-p integerp keywordp listp null not numberp
            packagep pathnamep random-state-p rationalp readtablep realp
            simple-bit-vector-p simple-string-p simple-vector-p stringp
            symbolp vectorp)
           ((:result-type boolean) null not)
           ((:predicate-type array) arrayp)
           ((:predicate-type atom) atom)
           ((:predicate-type bit-vector) bit-vector-p)
           ((:predicate-type character) characterp)
           ((:predicate-type compiled-function) compiled-function-p)
           ((:predicate-type complex) complexp)
           ((:predicate-type cons) consp)
           ((:predicate-type float) floatp)
           ((:predicate-type function) functionp)
           ((:predicate-type hash-table) hash-table-p)
           ((:predicate-type integer) integerp)
           ((:predicate-type keyword) keywordp)
           ;; A symbol can be uninterned from KEYWORD. KEYWORDP therefore
           ;; observes package state even though its type predicate is unary.
           ((:reads t) keywordp)
           ((:predicate-type list) listp)
           ((:predicate-type null) null not)
           ((:predicate-type number) numberp)
           ((:predicate-type package) packagep)
           ((:predicate-type pathname) pathnamep)
           ((:predicate-type random-state) random-state-p)
           ((:predicate-type rational) rationalp)
           ((:predicate-type readtable) readtablep)
           ((:predicate-type real) realp)
           ((:predicate-type simple-bit-vector) simple-bit-vector-p)
           ((:predicate-type simple-string) simple-string-p)
           ((:predicate-type simple-vector) simple-vector-p)
           ((:result-count 1 :arity (1 . 1) :truth-result t
             :reads t :writes t :callbacks t :may-signal t) streamp)
           ((:predicate-type stream) streamp)
           ((:predicate-type string) stringp)
           ((:predicate-type symbol) symbolp)
           ((:predicate-type vector) vectorp)
           ;; f_eq.htm, f_eql.htm. These have generalized boolean results too.
           ((:result-count 1 :arity (2 . 2) :reads nil :writes nil
             :callbacks nil :may-signal nil :allocation nil :truth-result t)
            eq eql)
           ;; f_typep.htm and f_subtpp.htm: SATISFIES/DEFTYPE are not pure
           ;; compiler queries. Record count independently of callback facts.
           ((:result-count 1 :arity (2 . 3) :truth-result t) typep)
           ((:result-count 2 :arity (2 . 3)) subtypep)
           ;; f_gethas.htm: lookup reads mutable state, and secondary presence
           ;; is a generalized boolean. No inference of a canonical T here.
           ((:result-count 2 :arity (2 . 3) :reads t :writes nil)
            gethash)
           ;; f_length.htm, f_intege.htm, f_ar_ran.htm. No host limits.
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :callbacks nil :writes nil :may-signal t)
            length integer-length array-rank)
           ((:reads t) length)
           ((:reads nil) integer-length array-rank)
           ;; f_char_.htm, f_bt_sb.htm.
           ((:result-count 1 :result-type character :arity (2 . 2)
             :reads t :writes nil :callbacks nil :may-signal t)
            char schar)
           ((:result-count 1 :result-type bit :arity (1 . nil)
             :reads t :writes nil :callbacks nil :may-signal t)
            bit sbit)
           ;; f_cons.htm: fresh outer object with operand references inside.
           ((:result-count 1 :result-type cons :arity (2 . 2) :reads nil
             :writes nil :callbacks nil :allocation :fresh :sharing :operands)
            cons)
           ;; f_abs.htm: complex arguments have real magnitude; do not assume
           ;; integer input is nonnegative or complex rationals remain rational.
           ((:result-count 1 :result-type real :arity (1 . 1) :reads nil
             :writes nil :callbacks nil :may-signal t)
            abs)
           ;; f_identi.htm preserves EQL value, not necessarily EQ identity
           ;; for numbers and characters.
           ((:result-count 1 :result-argument 0 :arity (1 . 1) :reads nil
             :writes nil :callbacks nil :may-signal nil :sharing :argument)
            identity)
           ;; f_revers.htm. Both return one sequence; only NREVERSE may write
           ;; to argument storage; REVERSE has fresh storage for non-NIL results.
           ((:result-count 1 :result-type sequence :arity (1 . 1)
             :reads t :callbacks nil :may-signal t :sharing :elements)
            reverse nreverse)
           ((:writes nil :allocation :fresh-unless-null) reverse)
           ((:writes t) nreverse)
           ;; f_symb_2.htm: string need not be simple or freshly allocated;
           ;; ANSI makes subsequent modification undefined.
           ((:result-count 1 :result-type string :arity (1 . 1)
             :reads nil :writes nil :callbacks nil :may-signal t
             :sharing :symbol-name)
            symbol-name)
           ;; f_symb_4.htm: the actual mutable property list, not a copy.
           ((:result-count 1 :result-type list :arity (1 . 1)
             :reads t :writes nil :callbacks nil :sharing :symbol-plist
             :may-signal t)
            symbol-plist)
           ;; f_intern.htm, f_find_s.htm: no false purity from the "Side
           ;; Effects: None" headings; INTERN can create a package binding.
           ((:result-count 2 :result-type symbol :arity (1 . 2) :reads t)
            intern find-symbol)
           ((:writes t) intern)
           ((:writes nil) find-symbol)
           ;; f_floorc.htm, f_dec_fl.htm, f_cmp.htm and f_cmp_fi.htm.
           ((:result-count 2 :result-type integer :arity (1 . 2))
            floor ceiling truncate round)
           ((:result-count 2 :result-type float :arity (1 . 2))
            ffloor fceiling ftruncate fround)
           ((:result-count 3 :result-type float :arity (1 . 1)) decode-float)
           ((:result-count 3 :result-type unsigned-byte :arity (1 . 1))
            integer-decode-float)
           ((:result-count 1 :result-type float :arity (2 . 2)) scale-float)
           ((:result-count 1 :result-type float :arity (1 . 2)) float-sign)
           ((:result-count 1 :result-type integer :arity (1 . 1)) float-radix)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1))
            float-digits float-precision)
           ((:result-count 3 :arity (1 . 2)) compile)
           ((:result-count 3 :result-type (or pathname null)) compile-file)
           ;; f_values.htm: count depends on actual argument count. VALUES-
           ;; LIST and forwarding operations retain unknown result counts.
           ((:kind :accessor :setf-place t :result-count :argument-count
             :arity (0 . nil) :reads nil :writes nil :callbacks nil)
            values)
           ((:setf-place t) class-name documentation getf ldb mask-field)))
      (dolist (name (cdr group))
        (setf (gethash name table) (cons (car group) (gethash name table)))))
    ;; Signatures and successful results are independent of lowering and of
    ;; effects. :ARGUMENT-TYPES describes positional arguments (including
    ;; optional ones); :REST-TYPE describes remaining positional arguments.
    ;; :KEYWORDS lists declared keyword names; every &KEY call still has the
    ;; standard :ALLOW-OTHER-KEYS convention. It gives no permission to omit
    ;; argument/bounds/keyword validation. :REQUIREMENTS
    ;; records structural and relational restrictions which are not CL types.
    ;; :ARITY is a total argument-count envelope; :KEYWORDS additionally
    ;; requires the usual well-formed keyword pairs after positional arguments.
    ;; :KEYWORD-ARGUMENTS T distinguishes an empty &KEY list from no &KEY;
    ;; :ALLOW-OTHER-KEYS T describes an explicit &ALLOW-OTHER-KEYS contract.
    ;; An initarg list still has structural/validity requirements even when
    ;; its lambda list has &ALLOW-OTHER-KEYS. Generic argument metadata describes
    ;; the generic lambda list, never a proof of the selected effective method.
    ;; :ALLOCATION :FRESH-UNLESS-NULL excludes NIL's shared empty-list identity.
    ;; These descriptors never expand a type or invoke a user function.
    (dolist
        (group
         '(;; Numbers: CLHS 12.2 dictionary, f_pl.htm through f_floatp.htm.
           ;; In particular, numeric results need not fit a target fixnum,
           ;; and MAX/MIN need not preserve an argument's representation.
           ((:result-count 1 :result-type number :arity (0 . nil)
             :keywords nil :rest-type number :reads nil :writes nil
             :callbacks nil :may-signal t) + *)
           ((:result-count 1 :result-type number :arity (1 . nil)
             :keywords nil :rest-type number :reads nil :writes nil
             :callbacks nil :may-signal t) - /)
           ((:result-count 1 :result-type number :arity (1 . 1)
             :keywords nil :argument-types (number) :reads nil :writes nil
             :callbacks nil :may-signal t)
            1+ 1- conjugate signum exp sqrt sin cos tan sinh cosh tanh
            asin acos asinh acosh atanh)
           ((:result-count 1 :result-type number :arity (1 . 2)
             :keywords nil :argument-types (number number) :reads nil
             :writes nil :callbacks nil :may-signal t) log atan)
           ((:requirements (:second-argument-real :two-argument-first-real)) atan)
           ((:result-count 1 :result-type number :arity (2 . 2)
             :keywords nil :argument-types (number number) :reads nil
             :writes nil :callbacks nil :may-signal t) expt)
           ((:result-count 1 :result-type number :arity (1 . 2)
             :keywords nil :argument-types (real real) :reads nil
             :writes nil :callbacks nil :may-signal t) complex)
           ((:result-count 1 :result-type complex :arity (1 . 1)
             :keywords nil :argument-types (real) :reads nil :writes nil
             :callbacks nil :may-signal t) cis)
           ((:result-count 1 :result-type real :arity (1 . 1)
             :keywords nil :argument-types (number) :reads nil :writes nil
             :callbacks nil :may-signal t) realpart imagpart phase abs)
           ((:result-count 1 :result-type real :arity (1 . nil)
             :keywords nil :rest-type real :reads nil :writes nil
             :callbacks nil :may-signal t) max min)
           ((:result-count 1 :result-type real :arity (2 . 2)
             :keywords nil :argument-types (real real) :reads nil :writes nil
             :callbacks nil :may-signal t) mod rem)
           ((:result-count 1 :arity (1 . nil) :keywords nil
             :rest-type number :truth-result t :reads nil :writes nil
             :callbacks nil :may-signal t) = /=)
           ((:result-count 1 :arity (1 . nil) :keywords nil
             :rest-type real :truth-result t :reads nil :writes nil
             :callbacks nil :may-signal t) < <= > >=)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :argument-types (number) :truth-result t :reads nil :writes nil
             :callbacks nil :may-signal t) zerop)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :argument-types (real) :truth-result t :reads nil :writes nil
             :callbacks nil :may-signal t) plusp minusp)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :argument-types (integer) :truth-result t :reads nil :writes nil
             :callbacks nil :may-signal t) evenp oddp)
           ((:result-count 1 :result-type unsigned-byte :arity (0 . nil)
             :keywords nil :rest-type integer :reads nil :writes nil
             :callbacks nil :may-signal t) gcd lcm)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (unsigned-byte) :reads nil
             :writes nil :callbacks nil :may-signal t) isqrt)
           ((:result-count 1 :result-type integer :arity (0 . nil)
             :keywords nil :rest-type integer :reads nil :writes nil
             :callbacks nil :may-signal t) logand logior logxor logeqv)
           ((:result-count 1 :result-type integer :arity (2 . 2)
             :keywords nil :argument-types (integer integer) :reads nil
             :writes nil :callbacks nil :may-signal t)
            ash logandc1 logandc2 lognand lognor logorc1 logorc2)
           ((:result-count 1 :result-type integer :arity (1 . 1)
             :keywords nil :argument-types (integer) :reads nil :writes nil
             :callbacks nil :may-signal t) lognot)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (integer) :reads nil :writes nil
             :callbacks nil :may-signal t) logcount integer-length)
           ((:result-count 1 :result-type integer :arity (3 . 3)
             :keywords nil :argument-types (integer integer integer)
             :requirements (:boole-operation) :reads nil :writes nil
             :callbacks nil :may-signal t) boole)
           ((:result-count 1 :arity (2 . 2) :keywords nil :truth-result t
             :argument-types (unsigned-byte integer) :reads nil :writes nil
             :callbacks nil :may-signal t) logbitp)
           ((:result-count 1 :arity (2 . 2) :keywords nil :truth-result t
             :argument-types (integer integer) :reads nil :writes nil
             :callbacks nil :may-signal t) logtest)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (unsigned-byte unsigned-byte) :reads nil
             :writes nil :callbacks nil :may-signal t) byte)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :requirements (:byte-specifier) :reads nil
             :writes nil :callbacks nil :may-signal t) byte-size byte-position)
           ((:result-count 1 :result-type unsigned-byte :arity (2 . 2)
             :keywords nil :argument-types (t integer)
             :requirements (:first-byte-specifier) :reads nil :writes nil
             :callbacks nil :may-signal t) ldb mask-field)
           ((:result-count 1 :arity (2 . 2) :keywords nil :truth-result t
             :argument-types (t integer) :requirements (:first-byte-specifier)
             :reads nil :writes nil :callbacks nil :may-signal t) ldb-test)
           ((:result-count 1 :result-type integer :arity (3 . 3)
             :keywords nil :argument-types (integer t integer)
             :requirements (:second-byte-specifier) :reads nil :writes nil
             :callbacks nil :may-signal t) dpb deposit-field)
           ((:result-count 1 :result-type rational :arity (1 . 1)
             :keywords nil :argument-types (real) :reads nil :writes nil
             :callbacks nil :may-signal t) rational rationalize)
           ((:result-count 1 :result-type integer :arity (1 . 1)
             :keywords nil :argument-types (rational) :reads nil :writes nil
             :callbacks nil :may-signal t) numerator)
           ((:result-count 1 :result-type (integer 1 *) :arity (1 . 1)
             :keywords nil :argument-types (rational) :reads nil :writes nil
             :callbacks nil :may-signal t) denominator)
           ((:result-count 1 :result-type float :arity (1 . 2)
             :keywords nil :argument-types (real float) :reads nil :writes nil
             :callbacks nil :may-signal t) float)
           ((:argument-types (real real) :keywords nil :reads nil :writes nil
             :callbacks nil :may-signal t)
            floor ceiling truncate round ffloor fceiling ftruncate fround)
           ((:argument-types (float) :keywords nil :reads nil :writes nil
             :callbacks nil :may-signal t)
            decode-float integer-decode-float float-radix float-digits float-precision)
           ((:argument-types (float integer) :keywords nil :reads nil :writes nil
             :callbacks nil :may-signal t) scale-float)
           ((:argument-types (float float) :keywords nil :reads nil :writes nil
             :callbacks nil :may-signal t) float-sign)
           ;; Conses: CLHS f_list_.htm, f_append.htm, f_nconc.htm, f_nth.htm,
           ;; f_nthcdr.htm, f_last.htm, f_butlas.htm, f_cp_*.htm, f_mapc_.htm.
           ;; APPEND/NCONC/LIST*/NTHCDR/LAST can return arbitrary dotted tails.
           ((:result-count 1 :result-type list :arity (0 . nil)
             :keywords nil :rest-type t :reads nil :writes nil :callbacks nil
             :allocation :fresh-unless-null :sharing :elements) list)
           ((:result-count 1 :arity (1 . nil) :keywords nil :rest-type t
             :reads nil :writes nil :callbacks nil :sharing :last-argument)
            list*)
           ((:result-count 1 :arity (0 . nil) :keywords nil :rest-type t
             :requirements (:proper-lists-except-last) :reads t
             :callbacks nil :sharing :last-argument :may-signal t) append nconc)
           ((:writes nil) append)
           ((:writes t) nconc)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (unsigned-byte list) :reads t :writes nil
             :callbacks nil :may-signal t :sharing :argument-tail) nth nthcdr)
           ((:result-count 1 :arity (1 . 2) :keywords nil
             :argument-types (list unsigned-byte) :reads t :writes nil
             :callbacks nil :may-signal t :sharing :argument-tail
             :requirements (:first-proper-or-dotted-list)) last)
           ((:result-count 1 :result-type list :arity (1 . 2) :keywords nil
             :argument-types (list unsigned-byte) :reads t :callbacks nil
             :may-signal t :requirements (:first-proper-or-dotted-list))
            butlast nbutlast)
           ((:writes nil :allocation :fresh-unless-null :sharing :elements) butlast)
           ((:writes t :sharing :argument-tail) nbutlast)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :argument-types (list) :reads t :writes nil :callbacks nil
             :may-signal t :allocation :fresh-unless-null) copy-list copy-alist)
           ((:requirements (:first-proper-or-dotted-list)
             :sharing :elements-and-dotted-tail) copy-list)
           ((:requirements (:first-association-list) :sharing :association-contents)
            copy-alist)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (t)
             :requirements (:acyclic-cons-tree) :reads t :writes nil
             :callbacks nil :sharing :non-cons-leaves) copy-tree)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (1 . 1)
             :keywords nil :argument-types (list) :reads t :writes nil
             :callbacks nil :may-signal t
             :requirements (:first-proper-or-circular-list)) list-length)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (list)
             :truth-result t :reads nil :writes nil :callbacks nil :may-signal t
             :allocation nil) endp)
           ;; Audited operation domains, independent of dispatch/polling and
           ;; stronger than ordinary argument signatures. Integer comparison
           ;; needs no floating contagion or floating exception assumption.
           ((:non-signaling-arguments (list)) car first cdr rest endp)
           ((:non-signaling-arguments (integer integer)) < > <= >= = /=)
           ((:result-count 1 :result-type cons :result-argument 0 :arity (2 . 2)
             :keywords nil :argument-types (cons t) :writes t :callbacks nil
             :may-signal t :sharing :argument) rplaca rplacd)
           ((:result-count 1 :result-type list :arity (2 . nil) :keywords nil
             :argument-types ((or function symbol)) :rest-type list
             :requirements (:proper-list-rest :callback-matching-list-count)
             :reads t :callbacks t :may-signal t)
            mapc mapcar mapcan mapl maplist mapcon)
           ((:result-argument 1 :sharing :argument) mapc mapl)
           ;; Callback effects are not suppressed by non-destructive operation
           ;; names. In particular no :WRITES NIL for these search/copy APIs.
           ;; CLHS f_member.htm, f_assocc.htm, f_rassoc.htm, f_unionc.htm,
           ;; f_isec_.htm, f_set_di.htm, f_set_ex.htm, f_subset.htm, f_adjoin.htm.
           ((:result-count 1 :result-type list :arity (2 . nil) :reads t
             :keywords (:key :test :test-not) :callbacks t :may-signal t)
            member assoc rassoc adjoin union nunion intersection nintersection
            set-difference nset-difference set-exclusive-or nset-exclusive-or)
           ((:result-count 1 :result-type list :arity (2 . nil) :reads t
             :keywords (:key) :callbacks t :may-signal t)
            member-if member-if-not assoc-if assoc-if-not rassoc-if rassoc-if-not)
           ((:sharing :argument-tail :argument-types (t list))
            member member-if member-if-not)
           ((:sharing :association-entry :argument-types (t list)
             :requirements (:second-association-list))
            assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not)
           ((:requirements (:proper-list-operands) :argument-types (list list)
             :sharing :input-structure)
            union nunion intersection nintersection set-difference nset-difference
            set-exclusive-or nset-exclusive-or)
           ((:result-count 1 :arity (2 . nil) :truth-result t
             :argument-types (list list) :requirements (:proper-list-operands)
             :keywords (:key :test :test-not) :reads t :callbacks t :may-signal t)
            subsetp)
           ((:result-count 1 :arity (2 . nil) :truth-result t
             :argument-types (t t) :keywords (:test :test-not)
             :reads t :callbacks t :may-signal t) tree-equal)
           ((:result-count 1 :arity (2 . nil) :keywords (:key :test :test-not)
             :argument-types (list t) :reads t :callbacks t :sharing :input-structure
             :may-signal t) sublis nsublis)
           ((:result-count 1 :arity (3 . nil) :keywords (:key :test :test-not)
             :argument-types (t t t) :reads t :callbacks t :sharing :input-structure
             :may-signal t) subst nsubst)
           ((:result-count 1 :arity (3 . nil) :keywords (:key)
             :argument-types (t (or function symbol) t) :reads t :callbacks t
             :sharing :input-structure :may-signal t)
            subst-if subst-if-not nsubst-if nsubst-if-not)
           ((:result-count 1 :result-type cons :arity (3 . 3) :keywords nil
             :argument-types (t t list) :reads nil :writes nil :callbacks nil
             :sharing :operands) acons)
           ((:result-count 1 :result-type list :arity (2 . 3) :keywords nil
             :argument-types (list list list) :requirements (:equal-length-proper-lists)
             :reads t :writes nil :callbacks nil :sharing :operands :may-signal t)
            pairlis)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (list t) :reads t :callbacks nil
             :sharing :second-argument :may-signal t) revappend nreconc)
           ((:writes nil) revappend)
           ((:writes t) nreconc)
           ((:result-count 1 :result-type list :arity (2 . 2) :keywords nil
             :argument-types (list t) :reads t :writes nil :callbacks nil
             :sharing :elements-and-dotted-tail :may-signal t) ldiff)
           ((:result-count 1 :arity (2 . 2) :keywords nil :truth-result t
             :argument-types (t list) :reads t :writes nil :callbacks nil
             :may-signal t) tailp)
           ;; Sequences: CLHS 17.3 dictionary. REDUCE's empty/no-initial case
           ;; returns whatever the callback does; no fixed count is asserted.
           ((:result-count 1 :result-type sequence :arity (1 . 1)
             :keywords nil :argument-types (sequence) :requirements (:proper-sequence)
             :reads t :writes nil :callbacks nil :allocation :fresh-unless-null
             :sharing :elements :may-signal t) copy-seq)
           ((:result-count 1 :result-type sequence :arity (2 . 3)
             :keywords nil :argument-types (sequence unsigned-byte (or unsigned-byte null))
             :requirements (:valid-sequence-bounds) :reads t :writes nil
             :callbacks nil :allocation :fresh-unless-null :sharing :elements :may-signal t)
            subseq)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (sequence unsigned-byte) :requirements (:valid-element-index)
             :reads t :writes nil :callbacks nil :sharing :element :may-signal t) elt)
           ((:result-count 1 :result-type sequence :arity (1 . nil)
             :keywords nil :argument-types (t) :rest-type sequence
             :requirements (:sequence-result-type :proper-sequence-rest)
             :reads t :sharing :elements :may-signal t) concatenate)
           ((:result-count 1 :result-type sequence :arity (2 . nil)
             :keywords (:initial-element) :argument-types (t unsigned-byte)
             :requirements (:sequence-result-type) :sharing :initial-element
             :may-signal t) make-sequence)
           ((:result-count 1 :result-type sequence :arity (3 . nil)
             :keywords nil :argument-types (t (or function symbol))
             :rest-type sequence :requirements (:sequence-or-null-result-type
               :proper-sequence-rest :callback-matching-sequence-count)
             :reads t :callbacks t :sharing :callback-results :may-signal t) map)
           ((:result-count 1 :result-type sequence :result-argument 0 :arity (2 . nil)
             :keywords nil :argument-types (sequence (or function symbol))
             :rest-type sequence :reads t :writes t :callbacks t
             :sharing :argument :may-signal t) map-into)
           ((:result-count 1 :arity (2 . nil) :keywords nil
             :argument-types ((or function symbol)) :rest-type sequence
             :requirements (:proper-sequence-rest :callback-matching-sequence-count)
             :reads t :callbacks t :truth-result t :may-signal t)
            every some notany notevery)
           ((:arity (2 . nil) :argument-types ((or function symbol) sequence)
             :keywords (:key :from-end :start :end :initial-value)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :may-signal t) reduce)
           ((:result-count 1 :result-type sequence :result-argument 0 :arity (2 . nil)
             :argument-types (sequence t) :keywords (:start :end)
             :requirements (:valid-sequence-bounds :storable-element)
             :reads t :writes t :callbacks nil :sharing :argument :may-signal t) fill)
           ((:result-count 1 :result-type sequence :result-argument 0 :arity (2 . nil)
             :argument-types (sequence sequence) :keywords (:start1 :end1 :start2 :end2)
             :requirements (:valid-sequence-bounds :storable-elements)
             :reads t :writes t :callbacks nil :sharing :argument :may-signal t) replace)
           ((:result-count 1 :result-type sequence :arity (2 . nil)
             :argument-types (sequence (or function symbol)) :keywords (:key)
             :reads t :writes t :callbacks t :sharing :input-structure
             :may-signal t) sort stable-sort)
           ((:result-count 1 :result-type sequence :arity (4 . nil)
             :argument-types (t sequence sequence (or function symbol)) :keywords (:key)
             :requirements (:sequence-result-type) :reads t :writes t :callbacks t
             :sharing :input-structure :may-signal t) merge)
           ((:result-count 1 :arity (2 . nil) :argument-types (t sequence)
             :keywords (:from-end :start :end :key :test :test-not)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :may-signal t) count find position)
           ((:result-count 1 :arity (2 . nil)
             :argument-types ((or function symbol) sequence)
             :keywords (:from-end :start :end :key)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :may-signal t) count-if count-if-not find-if find-if-not
            position-if position-if-not)
           ((:result-type unsigned-byte) count count-if count-if-not)
           ((:sharing :element) find find-if find-if-not)
           ((:result-type (or unsigned-byte null)) position position-if position-if-not)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (2 . nil)
             :argument-types (sequence sequence)
             :keywords (:from-end :test :test-not :key :start1 :end1 :start2 :end2)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :may-signal t) search mismatch)
           ((:result-count 1 :result-type sequence :arity (2 . nil)
             :argument-types (t sequence)
             :keywords (:from-end :test :test-not :start :end :count :key)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :sharing :input-structure :may-signal t) remove delete)
           ((:result-count 1 :result-type sequence :arity (2 . nil)
             :argument-types ((or function symbol) sequence)
             :keywords (:from-end :start :end :count :key)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :sharing :input-structure :may-signal t)
            remove-if remove-if-not delete-if delete-if-not)
           ((:result-count 1 :result-type sequence :arity (3 . nil)
             :argument-types (t t sequence)
             :keywords (:from-end :test :test-not :start :end :count :key)
             :requirements (:valid-sequence-bounds :storable-element) :reads t
             :callbacks t :sharing :input-structure :may-signal t) substitute nsubstitute)
           ((:result-count 1 :result-type sequence :arity (3 . nil)
             :argument-types (t (or function symbol) sequence)
             :keywords (:from-end :start :end :count :key)
             :requirements (:valid-sequence-bounds :storable-element) :reads t
             :callbacks t :sharing :input-structure :may-signal t)
            substitute-if substitute-if-not nsubstitute-if nsubstitute-if-not)
           ((:result-count 1 :result-type sequence :arity (1 . nil)
             :argument-types (sequence)
             :keywords (:from-end :test :test-not :start :end :key)
             :requirements (:valid-sequence-bounds) :reads t :callbacks t
             :sharing :input-structure :may-signal t) remove-duplicates delete-duplicates)
           ;; Strings/characters: f_stgeq_.htm, f_stg_up.htm, f_stg_tr.htm,
           ;; f_char_*.htm, f_digit_.htm, f_digi_1.htm, f_name_c.htm.
           ;; Ordering string predicates return an index, which can be zero.
           ((:result-count 1 :arity (2 . nil) :argument-types (t t)
             :keywords (:start1 :end1 :start2 :end2)
             :requirements (:string-designators :valid-string-bounds)
             :reads t :writes nil :callbacks nil :may-signal t :truth-result t)
            string= string/= string< string<= string> string>= string-equal
            string-not-equal string-lessp string-greaterp string-not-greaterp string-not-lessp)
           ((:result-type (or unsigned-byte null)) string/= string< string<= string>
            string>= string-not-equal string-lessp string-greaterp
            string-not-greaterp string-not-lessp)
           ((:result-count 1 :result-type string :arity (1 . nil)
             :keywords (:start :end) :argument-types (t)
             :requirements (:string-designator :valid-string-bounds)
             :reads t :writes nil :callbacks nil :may-signal t)
            string-upcase string-downcase string-capitalize)
           ((:result-count 1 :result-type string :result-argument 0 :arity (1 . nil)
             :keywords (:start :end) :argument-types (string)
             :requirements (:valid-string-bounds) :reads t :writes t
             :callbacks nil :sharing :argument :may-signal t)
            nstring-upcase nstring-downcase nstring-capitalize)
           ((:result-count 1 :result-type string :arity (2 . 2)
             :keywords nil :argument-types (sequence t)
             :requirements (:character-bag :second-string-designator)
             :reads t :writes nil :callbacks nil :sharing :possible-input-storage
             :may-signal t) string-trim string-left-trim string-right-trim)
           ((:result-count 1 :result-type string :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:string-designator)
             :reads nil :writes nil :callbacks nil :sharing :possible-input-storage
             :may-signal t) string)
           ((:result-count 1 :result-type character :arity (1 . 1)
             :keywords nil :argument-types (character) :reads nil :writes nil
             :callbacks nil :may-signal t) char-upcase char-downcase)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (character) :reads nil :writes nil
             :callbacks nil :may-signal t) char-code char-int)
           ((:result-count 1 :arity (1 . nil) :keywords nil :rest-type character
             :reads nil :writes nil :callbacks nil :truth-result t :may-signal t)
            char= char/= char< char<= char> char>= char-equal char-not-equal
            char-lessp char-greaterp char-not-greaterp char-not-lessp)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (character)
             :reads nil :writes nil :callbacks nil :truth-result t :may-signal t)
            alpha-char-p alphanumericp both-case-p graphic-char-p lower-case-p
            upper-case-p standard-char-p)
           ((:result-count 1 :result-type (or character null) :arity (1 . 2)
             :keywords nil :argument-types (unsigned-byte (integer 2 36))
             :reads nil :writes nil :callbacks nil :may-signal t) digit-char)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (1 . 2)
             :keywords nil :argument-types (character (integer 2 36))
             :reads nil :writes nil :callbacks nil :truth-result t :may-signal t)
            digit-char-p)
           ((:result-count 1 :result-type (or character null) :arity (1 . 1)
             :keywords nil :argument-types (unsigned-byte)
             :requirements (:character-code-bounds) :reads nil :writes nil
             :callbacks nil :may-signal t) code-char)
           ((:result-count 1 :result-type (or character null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:string-designator)
             :reads t :writes nil :callbacks nil :may-signal t) name-char)
           ((:result-count 1 :result-type (or string null) :arity (1 . 1)
             :keywords nil :argument-types (character) :reads nil :writes nil
             :callbacks nil :may-signal t) char-name)
           ;; Arrays: CLHS 15.2 dictionary. Dimensions/fill pointers are
           ;; mutable, unlike rank. AREF uses array bounds, not LENGTH.
           ((:result-count 1 :arity (1 . nil) :keywords nil
             :argument-types (array) :rest-type unsigned-byte
             :requirements (:subscripts-match-rank :valid-array-subscripts)
             :reads t :writes nil :callbacks nil :may-signal t) aref)
           ((:argument-types ((array bit)) :rest-type unsigned-byte :keywords nil
             :requirements (:subscripts-match-rank :valid-array-subscripts)) bit)
           ((:argument-types ((simple-array bit)) :rest-type unsigned-byte :keywords nil
             :requirements (:subscripts-match-rank :valid-array-subscripts)) sbit)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (array unsigned-byte) :requirements (:valid-array-row-major-index)
             :reads t :writes nil :callbacks nil :may-signal t) row-major-aref)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (simple-vector unsigned-byte) :requirements (:valid-array-index)
             :reads t :writes nil :callbacks nil :may-signal t) svref)
           ((:result-count 1 :result-type unsigned-byte :arity (2 . 2)
             :keywords nil :argument-types (array unsigned-byte)
             :requirements (:valid-array-axis) :reads t :writes nil
             :callbacks nil :may-signal t) array-dimension)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (array) :reads t :writes nil
             :callbacks nil :may-signal t) array-total-size)
           ((:argument-types (array) :keywords nil) array-rank)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :argument-types (array) :reads t :writes nil :callbacks nil
             :may-signal t) array-dimensions)
           ((:result-count 2 :result-type (or array null) :arity (1 . 1)
             :keywords nil :argument-types (array) :reads t :writes nil
             :callbacks nil :may-signal t :sharing :displaced-array) array-displacement)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (array)
             :reads t :writes nil :callbacks nil :may-signal t) array-element-type)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (array)
             :reads nil :writes nil :callbacks nil :truth-result t :may-signal t)
            adjustable-array-p array-has-fill-pointer-p)
           ((:result-count 1 :arity (1 . nil) :keywords nil
             :argument-types (array) :rest-type integer
             :requirements (:subscripts-match-rank) :truth-result t
             :reads t :writes nil :callbacks nil :may-signal t) array-in-bounds-p)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . nil)
             :keywords nil :argument-types (array) :rest-type unsigned-byte
             :requirements (:subscripts-match-rank :valid-array-subscripts)
             :reads t :writes nil :callbacks nil :may-signal t) array-row-major-index)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (vector) :requirements (:has-fill-pointer)
             :reads t :writes nil :callbacks nil :may-signal t) fill-pointer)
           ((:result-count 1 :result-type array :arity (1 . nil)
             :argument-types (t)
             :keywords (:element-type :initial-element :initial-contents :adjustable
               :fill-pointer :displaced-to :displaced-index-offset)
             :requirements (:valid-array-dimensions :consistent-array-initialization
               :valid-array-displacement) :sharing :possible-displaced-storage
             :may-signal t) make-array)
           ((:result-count 1 :result-type array :arity (2 . nil)
             :argument-types (array t)
             :keywords (:element-type :initial-element :initial-contents
               :fill-pointer :displaced-to :displaced-index-offset)
             :requirements (:same-array-rank :valid-array-dimensions
               :consistent-array-initialization :valid-array-displacement)
             :reads t :writes t :sharing :possible-input-storage :may-signal t)
            adjust-array)
           ((:result-count 1 :result-type simple-vector :arity (0 . nil)
             :keywords nil :rest-type t :reads nil :writes nil :callbacks nil
             :allocation :fresh :sharing :elements) vector)
           ((:result-count 1 :result-type list :arity (1 . nil)
             :argument-types (unsigned-byte) :keywords (:initial-element)
             :reads nil :writes nil :callbacks nil :sharing :initial-element
             :may-signal t) make-list)
           ((:result-count 1 :result-type simple-string :arity (1 . nil)
             :argument-types (unsigned-byte) :keywords (:initial-element :element-type)
             :requirements (:valid-array-dimension :character-element-type)
             :allocation :fresh :may-signal t) make-string)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :argument-types (vector) :requirements (:nonempty-fill-pointer)
             :reads t :writes t :callbacks nil :sharing :element :may-signal t)
            vector-pop)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (2 . 2)
             :keywords nil :argument-types (t vector)
             :requirements (:has-fill-pointer :storable-element)
             :reads t :writes t :callbacks nil :may-signal t) vector-push)
           ((:result-count 1 :result-type unsigned-byte :arity (2 . 3)
             :keywords nil :argument-types (t vector (integer 1 *))
             :requirements (:has-fill-pointer :storable-element)
             :reads t :writes t :callbacks nil :may-signal t) vector-push-extend)
           ((:result-count 1 :result-type (array bit) :arity (2 . 3)
             :keywords nil :argument-types ((array bit) (array bit) (or (array bit) boolean))
             :requirements (:matching-array-dimensions) :reads t :writes t
             :callbacks nil :sharing :possible-input-storage :may-signal t)
            bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor
            bit-orc1 bit-orc2 bit-xor)
           ((:result-count 1 :result-type (array bit) :arity (1 . 2)
             :keywords nil :argument-types ((array bit) (or (array bit) boolean))
             :requirements (:matching-array-dimensions) :reads t :writes t
             :callbacks nil :sharing :possible-input-storage :may-signal t) bit-not)
           ;; Hash tables: CLHS 18.2. Extended tests may invoke callbacks;
           ;; neither table identity nor a constant table value proves purity.
           ((:result-count 1 :result-type hash-table :arity (0 . nil)
             :keywords (:test :size :rehash-size :rehash-threshold)
             :requirements (:hash-test-designator :valid-rehash-parameters)
             :allocation :fresh :may-signal t) make-hash-table)
           ((:argument-types (t hash-table t) :keywords nil :may-signal t) gethash)
           ((:result-count 1 :arity (2 . 2) :keywords nil
             :argument-types (t hash-table) :reads t :writes t
             :truth-result t :may-signal t) remhash)
           ((:result-count 1 :result-type hash-table :result-argument 0 :arity (1 . 1)
             :keywords nil :argument-types (hash-table) :reads t :writes t
             :callbacks nil :sharing :argument :may-signal t) clrhash)
           ((:result-count 1 :result-type null :arity (2 . 2) :keywords nil
             :argument-types ((or function symbol) hash-table) :reads t :callbacks t
             :may-signal t) maphash)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (hash-table) :reads t :writes nil
             :callbacks nil :may-signal t) hash-table-count hash-table-size)
           ((:result-count 1 :result-type real :arity (1 . 1)
             :keywords nil :argument-types (hash-table) :reads nil :writes nil
             :callbacks nil :may-signal t) hash-table-rehash-size hash-table-rehash-threshold)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :argument-types (hash-table) :reads nil :writes nil :callbacks nil
             :may-signal t) hash-table-test)
           ((:result-count 1 :result-type unsigned-byte :arity (1 . 1)
             :keywords nil :argument-types (t) :reads t :writes nil
             :callbacks nil) sxhash)
           ;; Symbols/packages: CLHS 10.2 and 11.2 dictionaries. Package
           ;; deletion permits NIL names; package/symbol access is mutable.
           ((:argument-types (symbol) :keywords nil) symbol-name symbol-plist)
           ((:result-count 1 :result-type (or package null) :arity (1 . 1)
             :keywords nil :argument-types (symbol) :reads t :writes nil
             :callbacks nil :may-signal t) symbol-package)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (symbol)
             :reads t :writes nil :callbacks nil :may-signal t) symbol-value symbol-function)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (symbol)
             :reads t :writes nil :callbacks nil :may-signal t :truth-result t) boundp)
           ((:result-count 1 :result-type symbol :arity (1 . 1) :keywords nil
             :argument-types (symbol) :writes t :callbacks nil :result-argument 0
             :sharing :argument :may-signal t) makunbound)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (symbol t)
             :writes t :callbacks nil :result-argument 1 :sharing :argument
             :may-signal t) set)
           ((:result-count 1 :result-type symbol :arity (1 . 1) :keywords nil
             :argument-types (string) :reads t :writes nil :callbacks nil
             :allocation :fresh :may-signal t) make-symbol)
           ((:result-count 1 :result-type symbol :arity (1 . 2) :keywords nil
             :argument-types (symbol t) :reads t :writes nil :callbacks nil
             :allocation :fresh :sharing :possible-plist-contents :may-signal t) copy-symbol)
           ((:result-count 1 :result-type symbol :arity (0 . 1) :keywords nil
             :argument-types ((or string unsigned-byte)) :reads t :writes t
             :callbacks nil :allocation :fresh :may-signal t) gensym)
           ((:result-count 1 :result-type symbol :arity (0 . 2) :keywords nil
             :argument-types (string t) :requirements (:second-package-designator)
             :reads t :writes t :callbacks nil :may-signal t) gentemp)
           ((:argument-types (string t) :keywords nil
             :requirements (:second-package-designator)) intern find-symbol)
           ((:result-count 1 :arity (2 . 3) :keywords nil
             :argument-types (symbol t t) :reads t :writes nil :callbacks nil
             :sharing :property-value-or-default :may-signal t) get)
           ((:result-count 1 :arity (2 . 3) :keywords nil :argument-types (list t t)
             :requirements (:proper-property-list) :reads t :writes nil :callbacks nil
             :sharing :property-value-or-default :may-signal t) getf)
           ((:result-count 3 :arity (2 . 2) :keywords nil :argument-types (list list)
             :requirements (:first-property-list :second-proper-list)
             :reads t :writes nil :callbacks nil :sharing :property-list-tail
             :may-signal t) get-properties)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (symbol t)
             :reads t :writes t :callbacks nil :truth-result t :may-signal t) remprop)
           ((:result-count 1 :result-type (or package null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:package-or-string-designator)
             :reads t :writes nil :callbacks nil :may-signal t) find-package)
           ((:result-count 1 :result-type (or string null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:package-designator)
             :reads t :writes nil :callbacks nil :may-signal t) package-name)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:package-designator)
             :reads t :writes nil :callbacks nil :may-signal t)
            package-nicknames package-shadowing-symbols package-use-list package-used-by-list)
           ((:result-count 1 :result-type list :arity (0 . 0) :keywords nil
             :reads t :writes nil :callbacks nil) list-all-packages)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:string-designator)
             :reads t :writes nil :callbacks nil :may-signal t) find-all-symbols)
           ((:result-count 1 :result-type package :arity (1 . nil)
             :argument-types (t) :keywords (:nicknames :use)
             :requirements (:string-designator :package-nickname-and-use-lists)
             :reads t :writes t :callbacks nil :may-signal t) make-package)
           ((:result-count 1 :result-type package :arity (2 . 3) :keywords nil
             :argument-types (t t list) :requirements (:package-designators
               :proper-string-designator-nickname-list)
             :reads t :writes t :callbacks nil :may-signal t) rename-package)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (t)
             :requirements (:package-designator) :reads t :writes t
             :callbacks nil :truth-result t :may-signal t) delete-package)
           ;; Function binding queries do not promise FUNCTION results for
           ;; macros/special operators (f_fdefin.htm, f_symb_1.htm).
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :requirements (:function-name) :reads t :writes nil :callbacks nil
             :may-signal t) fdefinition)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :requirements (:function-name) :reads t :writes nil :callbacks nil
             :truth-result t :may-signal t) fboundp)
           ((:result-count 1 :arity (1 . 1) :keywords nil
             :requirements (:function-name) :reads t :writes t :callbacks nil
             :result-argument 0 :sharing :argument :may-signal t) fmakunbound)
           ((:result-count 1 :result-type (or function null) :arity (1 . 2)
             :keywords nil :requirements (:function-name :optional-environment)
             :reads t :writes nil :callbacks nil :may-signal t)
            macro-function compiler-macro-function)
           ((:argument-types (symbol t) :requirements (:optional-environment)) macro-function)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (symbol)
             :reads nil :writes nil :callbacks nil :truth-result t :may-signal t)
            special-operator-p)
           ((:result-count 3 :arity (1 . 1) :keywords nil :argument-types (function)
             :reads t :writes nil :callbacks nil :may-signal t) function-lambda-expression)
           ((:result-count 2 :arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:optional-environment) :reads t :callbacks t
             :may-signal t) macroexpand macroexpand-1)
           ((:result-count 5 :result-type list :arity (1 . 2) :keywords nil
             :argument-types (t t) :requirements (:place :optional-environment)
             :reads t :callbacks t :may-signal t) get-setf-expansion)
           ((:result-count 1 :arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:form :optional-environment) :reads t :callbacks t
             :truth-result t :may-signal t) constantp)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (t t)
             :requirements (:second-type-specifier) :may-signal t) coerce)
           ((:result-count 1 :result-type pathname :arity (1 . nil)
             :argument-types (t) :keywords (:output-file) :allow-other-keys t
             :requirements (:pathname-designators) :reads t :may-signal t)
            compile-file-pathname)
           ;; Remaining foundational operations: f_apply.htm, f_funcal.htm,
           ;; f_vals_l.htm, f_eval.htm, f_comple.htm, f_cons_1.htm, f_ch.htm,
           ;; f_equal.htm, f_equalp.htm and the type dictionary. Forwarded
           ;; values and implementation-dependent returns have no fixed count.
           ((:arity (2 . nil) :keywords nil :argument-types ((or function symbol))
             :rest-type t :requirements (:spreadable-argument-list)
             :reads t :callbacks t :may-signal t) apply)
           ((:arity (1 . nil) :keywords nil :argument-types ((or function symbol))
             :rest-type t :reads t :callbacks t :may-signal t) funcall)
           ((:arity (1 . 1) :keywords nil :argument-types (list)
             :requirements (:proper-list) :reads t :writes nil :callbacks nil
             :sharing :elements :may-signal t) values-list)
           ((:arity (1 . 1) :keywords nil :argument-types (t)
             :requirements (:form) :reads t :callbacks t :may-signal t) eval)
           ((:result-count 1 :result-type function :arity (1 . 1) :keywords nil
             :argument-types (function) :reads nil :writes nil :callbacks nil
             :may-signal t :sharing :captured-function) complement)
           ((:result-count 1 :result-type function :arity (1 . 1) :keywords nil
             :argument-types (t) :reads nil :writes nil :callbacks nil
             :sharing :captured-value) constantly)
           ((:result-count 1 :result-type character :arity (1 . 1) :keywords nil
             :argument-types ((or character symbol string))
             :requirements (:character-designator) :reads t :writes nil
             :callbacks nil :may-signal t) character)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (t t)
             :truth-result t :reads t :writes nil :callbacks nil :may-signal t)
            equal equalp)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (t)
             :reads t :writes nil :callbacks nil) type-of)
           ((:result-count 1 :arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:type-specifier :optional-environment) :reads t
             :may-signal t) upgraded-array-element-type upgraded-complex-part-type)
           ((:argument-types (t t t) :keywords nil
             :requirements (:second-type-specifier :optional-environment)) typep)
           ((:argument-types (t t t) :keywords nil
             :requirements (:type-specifiers :optional-environment)) subtypep)
           ((:argument-types (t) :keywords nil) identity)
           ((:argument-types (t t) :keywords nil) eq eql cons)
           ((:argument-types (t) :keywords nil)
            arrayp atom bit-vector-p characterp compiled-function-p complexp consp
            floatp functionp hash-table-p integerp keywordp listp null not numberp
            packagep pathnamep random-state-p rationalp readtablep realp
            simple-bit-vector-p simple-string-p simple-vector-p streamp stringp
            symbolp vectorp)
           ((:argument-types (sequence) :keywords nil
             :requirements (:proper-sequence)) length reverse nreverse)
           ((:argument-types (string unsigned-byte) :keywords nil) char)
           ((:argument-types (simple-string unsigned-byte) :keywords nil) schar)
           ((:keywords nil :rest-type t) values)
           ((:argument-types (t list)) adjoin)
           ((:argument-types (t)) byte-position byte-size fboundp fmakunbound fdefinition)
           ((:argument-types (t t)) compiler-macro-function)
           ((:argument-types (t t) :keywords nil
             :requirements (:function-name-or-null :lambda-expression-or-function-or-null))
            compile)
           ((:result-count 1 :result-type structure-object :arity (1 . 1)
             :keywords nil :argument-types (structure-object) :reads t :writes nil
             :callbacks nil :allocation :fresh :sharing :slot-contents :may-signal t)
            copy-structure)
           ((:result-count 1 :result-type random-state :arity (0 . 1) :keywords nil
             :argument-types ((or random-state boolean)) :reads t :writes nil
             :callbacks nil :allocation :fresh :may-signal t) make-random-state)
           ((:result-count 1 :result-type (or unsigned-byte float) :arity (1 . 2)
             :keywords nil :argument-types ((or (integer 1) (float (0))) random-state)
             :reads t :writes t :callbacks nil :may-signal t) random)
           ((:result-count 2 :result-type (or integer null) :arity (1 . nil)
             :keywords (:start :end :radix :junk-allowed) :argument-types (string)
             :requirements (:valid-string-bounds :radix-2-to-36) :reads t
             :writes nil :callbacks nil :may-signal t) parse-integer)
           ;; Package mutation: f_export.htm, f_import.htm, f_shadow.htm,
           ;; f_shdw_i.htm, f_unexpo.htm, f_uninte.htm, f_use_pk.htm, f_unuse_.htm.
           ((:result-count 1 :result-type (eql t) :arity (1 . 2) :keywords nil
             :argument-types ((or symbol list) t)
             :requirements (:symbol-list-designator :second-package-designator)
             :reads t :writes t :may-signal t) export import shadowing-import unexport)
           ((:result-count 1 :result-type (eql t) :arity (1 . 2) :keywords nil
             :argument-types (t t)
             :requirements (:string-designator-list-designator :second-package-designator)
             :reads t :writes t :may-signal t) shadow)
           ((:result-count 1 :arity (1 . 2) :keywords nil :argument-types (symbol t)
             :requirements (:second-package-designator) :truth-result t
             :reads t :writes t :may-signal t) unintern)
           ((:result-count 1 :result-type (eql t) :arity (1 . 2) :keywords nil
             :argument-types (t t)
             :requirements (:package-designator-list-designator :second-package-designator)
             :reads t :writes t :may-signal t) use-package unuse-package)
           ((:requirements (:package-designator-list-designator
               :second-package-designator :no-keyword-package)) use-package)
           ;; Streams: CLHS 21.2 dictionary. Stream designators include T/NIL;
           ;; binary/sequence operations require actual streams. Synonym and
           ;; composite streams observe mutable component state. IO operations
           ;; may signal and change stream state even when no value is wanted.
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (stream)
             :truth-result t :reads t :writes t :may-signal t)
            input-stream-p output-stream-p interactive-stream-p)
           ((:result-count nil :arity (1 . 1) :keywords nil :argument-types (stream)
             :reads t :writes t :callbacks t :may-signal t) open-stream-p)
           ((:result-count nil :arity (1 . 1) :keywords nil :argument-types (stream)
             :reads t :writes t :callbacks t :may-signal t) stream-element-type)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (file-stream)
             :reads t :writes nil :may-signal t) stream-external-format)
           ((:result-count 1 :arity (1 . 3) :keywords nil
             :argument-types (stream t t) :requirements (:binary-input-stream)
             :reads t :writes t :may-signal t) read-byte)
           ((:result-count 1 :result-type integer :result-argument 0 :arity (2 . 2)
             :keywords nil :argument-types (integer stream)
             :requirements (:binary-output-stream :storable-stream-element)
             :reads t :writes t :may-signal t) write-byte)
           ((:result-count 1 :arity (0 . 4) :keywords nil
             :argument-types ((or stream boolean) t t t)
             :requirements (:character-input-stream-designator)
             :reads t :writes t :may-signal t) read-char read-char-no-hang)
           ((:result-count 1 :arity (0 . 5) :keywords nil
             :argument-types ((or character boolean) (or stream boolean) t t t)
             :requirements (:character-input-stream-designator)
             :reads t :writes t :may-signal t) peek-char)
           ((:result-count 2 :arity (0 . 4) :keywords nil
             :argument-types ((or stream boolean) t t t)
             :requirements (:character-input-stream-designator)
             :reads t :writes t :may-signal t) read-line)
           ((:result-count 1 :result-type null :arity (1 . 2) :keywords nil
             :argument-types (character (or stream boolean))
             :requirements (:character-input-stream-designator :last-read-character
               :no-intervening-unread-char-or-file-position)
             :reads t :writes t :may-signal t) unread-char)
           ((:result-count 1 :result-type character :result-argument 0 :arity (1 . 2)
             :keywords nil :argument-types (character (or stream boolean))
             :requirements (:character-output-stream-designator)
             :reads t :writes t :may-signal t) write-char)
           ((:result-count 1 :result-type string :result-argument 0 :arity (1 . nil)
             :argument-types (string (or stream boolean)) :keywords (:start :end)
             :requirements (:character-output-stream-designator :valid-string-bounds)
             :reads t :writes t :may-signal t) write-string write-line)
           ((:result-count 1 :arity (2 . nil) :argument-types (sequence stream)
             :keywords (:start :end) :requirements (:valid-sequence-bounds
               :compatible-stream-sequence-element-types) :reads t :writes t
             :may-signal t) read-sequence write-sequence)
           ((:result-type unsigned-byte) read-sequence)
           ((:result-type sequence :result-argument 0 :sharing :argument) write-sequence)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (1 . 1)
             :keywords nil :argument-types (stream) :requirements (:file-associated-stream)
             :reads t :may-signal t) file-length)
           ((:result-count 1 :arity (1 . 2) :keywords nil
             :argument-types (stream (or unsigned-byte (member :start :end)))
             :reads t :writes t :may-signal t) file-position)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (2 . 2)
             :keywords nil :argument-types (file-stream (or string character))
             :requirements (:character-output-file-stream) :reads t :may-signal t)
            file-string-length)
           ((:result-count 1 :result-type (or file-stream null) :arity (1 . nil)
             :argument-types (t) :keywords (:direction :element-type :if-exists
               :if-does-not-exist :external-format)
             :requirements (:pathname-designator :open-keyword-values)
             :reads t :writes t :may-signal t) open)
           ((:result-count nil :arity (1 . nil) :argument-types (stream)
             :keywords (:abort) :reads t :writes t :callbacks t :may-signal t) close)
           ((:result-count 1 :arity (0 . 1) :keywords nil
             :argument-types ((or stream boolean))
             :requirements (:character-input-stream-designator)
             :truth-result t :reads t :may-signal t) listen)
           ((:result-count 1 :result-type null :arity (0 . 1) :keywords nil
             :argument-types ((or stream boolean)) :reads t :writes t :may-signal t)
            clear-input clear-output finish-output force-output terpri)
           ((:requirements (:input-stream-designator)) clear-input)
           ((:requirements (:output-stream-designator)) clear-output finish-output force-output)
           ((:requirements (:character-output-stream-designator)) terpri)
           ((:result-count 1 :arity (0 . 1) :keywords nil
             :argument-types ((or stream boolean)) :truth-result t
             :requirements (:character-output-stream-designator)
             :reads t :writes t :may-signal t) fresh-line)
           ((:result-count 1 :arity (0 . nil) :keywords nil
             :argument-types ((or string function)) :rest-type t
             :requirements (:format-control-and-arguments) :truth-result t
             :reads t :writes t :callbacks t :may-signal t) y-or-n-p yes-or-no-p)
           ((:result-count 1 :result-type synonym-stream :arity (1 . 1)
             :keywords nil :argument-types (symbol) :requirements (:dynamic-variable-name)
             :sharing :symbol :may-signal t) make-synonym-stream)
           ((:result-count 1 :result-type symbol :arity (1 . 1) :keywords nil
             :argument-types (synonym-stream) :reads t :writes nil
             :sharing :stream-symbol :may-signal t) synonym-stream-symbol)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :reads t :writes nil :sharing :component-streams :may-signal t)
            broadcast-stream-streams concatenated-stream-streams)
           ((:argument-types (broadcast-stream)) broadcast-stream-streams)
           ((:argument-types (concatenated-stream)) concatenated-stream-streams)
           ((:result-count 1 :arity (0 . nil) :keywords nil :rest-type stream
             :sharing :component-streams :may-signal t)
            make-broadcast-stream make-concatenated-stream)
           ((:result-type broadcast-stream :requirements (:output-stream-rest)) make-broadcast-stream)
           ((:result-type concatenated-stream :requirements (:input-stream-rest))
            make-concatenated-stream)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (stream stream)
             :requirements (:first-input-stream :second-output-stream)
             :sharing :component-streams :may-signal t) make-two-way-stream make-echo-stream)
           ((:result-type two-way-stream) make-two-way-stream)
           ((:result-type echo-stream) make-echo-stream)
           ((:result-count 1 :result-type stream :arity (1 . 1) :keywords nil
             :reads t :writes nil :sharing :component-stream :may-signal t)
            two-way-stream-input-stream two-way-stream-output-stream
            echo-stream-input-stream echo-stream-output-stream)
           ((:argument-types (two-way-stream)) two-way-stream-input-stream two-way-stream-output-stream)
           ((:argument-types (echo-stream)) echo-stream-input-stream echo-stream-output-stream)
           ((:result-count 1 :result-type string :arity (1 . 1) :keywords nil
             :argument-types (stream) :requirements (:string-output-stream)
             :reads t :writes t :may-signal t) get-output-stream-string)
           ((:result-count 1 :result-type string-stream :arity (1 . 3) :keywords nil
             :argument-types (string unsigned-byte (or unsigned-byte null))
             :requirements (:valid-string-bounds) :sharing :string-storage
             :may-signal t) make-string-input-stream)
           ((:result-count 1 :result-type string-stream :arity (0 . nil)
             :keywords (:element-type) :requirements (:character-subtype-element-type)
             :may-signal t) make-string-output-stream)
           ;; Reader: CLHS 23.2. Reader macros and #. can execute arbitrary
           ;; code. Fixed outer return counts do not imply callback purity.
           ((:result-count 1 :arity (0 . 4) :keywords nil
             :argument-types ((or stream boolean) t t t)
             :requirements (:character-input-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) read read-preserving-whitespace)
           ((:result-count 1 :result-type list :arity (1 . 3) :keywords nil
             :argument-types (character (or stream boolean) t)
             :requirements (:character-input-stream-designator :non-whitespace-delimiter)
             :reads t :writes t :callbacks t :may-signal t) read-delimited-list)
           ((:result-count 2 :arity (1 . nil) :argument-types (string t t)
             :keywords (:start :end :preserve-whitespace)
             :requirements (:valid-string-bounds) :reads t :callbacks t :may-signal t)
            read-from-string)
           ((:result-count 1 :result-type readtable :arity (0 . 2) :keywords nil
             :argument-types ((or readtable null) (or readtable null))
             :reads t :writes t :callbacks nil :sharing :possible-destination
             :may-signal t) copy-readtable)
           ((:result-count 1 :result-type (member :upcase :downcase :preserve :invert)
             :arity (1 . 1) :keywords nil :argument-types (readtable)
             :reads t :writes nil :callbacks nil :may-signal t) readtable-case)
           ((:result-count 1 :result-type (eql t) :arity (1 . 3) :keywords nil
             :argument-types (character t readtable) :reads t :writes t
             :callbacks nil :may-signal t) make-dispatch-macro-character)
           ((:result-count 1 :result-type (or function symbol null) :arity (2 . 3)
             :keywords nil :argument-types (character character (or readtable null))
             :requirements (:dispatch-macro-character) :reads t :writes nil
             :callbacks nil :may-signal t) get-dispatch-macro-character)
           ((:result-count 1 :result-type (eql t) :arity (3 . 4) :keywords nil
             :argument-types (character character (or function symbol) (or readtable null))
             :requirements (:dispatch-character-and-nondecimal-subcharacter)
             :reads t :writes t :callbacks nil :may-signal t) set-dispatch-macro-character)
           ((:result-count 2 :result-type (or function symbol null) :arity (1 . 2)
             :keywords nil :argument-types (character (or readtable null))
             :reads t :writes nil :callbacks nil :may-signal t) get-macro-character)
           ((:result-count 1 :result-type (eql t) :arity (2 . 4) :keywords nil
             :argument-types (character (or function symbol) t (or readtable null))
             :reads t :writes t :callbacks nil :may-signal t) set-macro-character)
           ((:result-count 1 :result-type (eql t) :arity (2 . 4) :keywords nil
             :argument-types (character character readtable (or readtable null))
             :reads t :writes t :callbacks nil :may-signal t) set-syntax-from-char)
           ;; Printer: CLHS 22.4. A pprint-dispatch table is not a named ANSI
           ;; type. Printing invokes user PRINT-OBJECT/dispatch/FORMAT code.
           ((:result-count 1 :arity (0 . 1) :keywords nil :argument-types (t)
             :requirements (:pprint-dispatch-table-or-null) :reads t
             :allocation :fresh :may-signal t) copy-pprint-dispatch)
           ((:result-count 2 :result-type (or function symbol) :arity (1 . 2)
             :keywords nil :argument-types (t t)
             :requirements (:second-pprint-dispatch-table-or-null)
             :reads t :callbacks t :may-signal t) pprint-dispatch)
           ((:result-count 1 :result-type null :arity (2 . 4) :keywords nil
             :argument-types ((or stream boolean) t t t)
             :requirements (:character-output-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) pprint-fill pprint-linear)
           ((:result-count 1 :result-type null :arity (2 . 5) :keywords nil
             :argument-types ((or stream boolean) t t t unsigned-byte)
             :requirements (:character-output-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) pprint-tabular)
           ((:result-count 1 :result-type null :arity (2 . 3) :keywords nil
             :argument-types ((member :block :current) real (or stream boolean))
             :requirements (:output-stream-designator)
             :reads t :writes t :may-signal t) pprint-indent)
           ((:result-count 1 :result-type null :arity (1 . 2) :keywords nil
             :argument-types ((member :linear :fill :miser :mandatory) (or stream boolean))
             :requirements (:output-stream-designator)
             :reads t :writes t :may-signal t) pprint-newline)
           ((:result-count 1 :result-type null :arity (3 . 4) :keywords nil
             :argument-types ((member :line :section :line-relative :section-relative)
               unsigned-byte unsigned-byte (or stream boolean))
             :requirements (:output-stream-designator)
             :reads t :writes t :may-signal t) pprint-tab)
           ((:result-count 1 :result-type null :arity (2 . 4) :keywords nil
             :argument-types (t t real t)
             :requirements (:type-specifier :function-name-function-or-null
               :fourth-pprint-dispatch-table) :reads t :writes t :may-signal t)
            set-pprint-dispatch)
           ((:result-count 1 :result-argument 0 :arity (1 . nil) :argument-types (t)
             :keywords (:array :base :case :circle :escape :gensym :length :level
               :lines :miser-width :pprint-dispatch :pretty :radix :readably
               :right-margin :stream) :requirements (:printer-keyword-values)
             :reads t :writes t :callbacks t :may-signal t) write)
           ((:result-count 1 :result-argument 0 :arity (1 . 2) :keywords nil
             :argument-types (t (or stream boolean))
             :requirements (:character-output-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) prin1 princ print)
           ((:result-count 0 :result-type null :arity (1 . 2) :keywords nil
             :argument-types (t (or stream boolean))
             :requirements (:character-output-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) pprint)
           ((:result-count 1 :result-type string :arity (1 . nil) :argument-types (t)
             :keywords (:array :base :case :circle :escape :gensym :length :level
               :lines :miser-width :pprint-dispatch :pretty :radix :readably
               :right-margin) :requirements (:printer-keyword-values)
             :reads t :callbacks t :may-signal t) write-to-string)
           ((:result-count 1 :result-type string :arity (1 . 1) :keywords nil
             :argument-types (t) :reads t :callbacks t :may-signal t)
            prin1-to-string princ-to-string)
           ((:result-count 1 :result-type (or string null) :arity (2 . nil)
             :keywords nil :argument-types ((or boolean stream string) (or string function))
             :rest-type t :requirements (:format-destination :format-control-and-arguments)
             :reads t :writes t :callbacks t :may-signal t) format)
           ;; Pathnames/files: CLHS 19.4 and 20.2. Pathname component domains
           ;; include implementation-defined hosts/devices; do not narrow those
           ;; to host strings or presume fresh, unshared component storage.
           ((:result-count 1 :result-type pathname :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:pathname-designator)
             :reads t :may-signal t) pathname truename)
           ((:result-count 1 :result-type logical-pathname :arity (1 . 1) :keywords nil
             :argument-types ((or logical-pathname string stream))
             :requirements (:logical-pathname-designator) :reads t :may-signal t)
            logical-pathname)
           ((:result-count 1 :result-type pathname :arity (0 . nil)
             :keywords (:host :device :directory :name :type :version :defaults :case)
             :requirements (:valid-pathname-components :pathname-defaults :pathname-case)
             :reads t :may-signal t) make-pathname)
           ((:result-count 1 :arity (1 . nil) :argument-types (t) :keywords (:case)
             :requirements (:pathname-designator :pathname-case)
             :reads t :writes nil :may-signal t)
            pathname-host pathname-device pathname-directory pathname-name pathname-type)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (t)
             :requirements (:pathname-designator) :reads t :writes nil :may-signal t)
            pathname-version)
           ((:result-count 1 :result-type list :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:logical-host-designator)
             :reads t :writes nil :sharing :translation-list :may-signal t)
            logical-pathname-translations)
           ((:result-count 1 :arity (1 . 1) :keywords nil :argument-types (string)
             :truth-result t :reads t :writes t :may-signal t) load-logical-pathname-translations)
           ((:result-count 1 :result-type (or string null) :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:pathname-designator)
             :reads t :writes nil :may-signal t)
            namestring file-namestring directory-namestring host-namestring)
           ((:result-count 1 :result-type (or string null) :arity (1 . 2) :keywords nil
             :argument-types (t t) :requirements (:pathname-designators)
             :reads t :writes nil :may-signal t) enough-namestring)
           ((:result-count 2 :result-type (or pathname null) :arity (1 . nil)
             :argument-types ((or string pathname stream) t t)
             :keywords (:start :end :junk-allowed) :requirements (:valid-pathname-host
               :third-pathname-designator :valid-namestring-bounds :file-associated-stream)
             :reads t :may-signal t) parse-namestring)
           ((:result-count 1 :arity (1 . 2) :keywords nil
             :argument-types (t (member :host :device :directory :name :type :version nil))
             :requirements (:pathname-designator) :truth-result t
             :reads t :writes nil :may-signal t) wild-pathname-p)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (t t)
             :requirements (:pathname-designators :second-wild-pathname)
             :truth-result t :reads t :writes nil :may-signal t) pathname-match-p)
           ;; These ANSI lambda lists have an empty &KEY section, permitting
           ;; implementation extension keywords and the :ALLOW-OTHER-KEYS rule.
           ((:result-count 1 :result-type pathname :arity (1 . nil)
             :argument-types (t) :keywords nil :keyword-arguments t
             :requirements (:pathname-designator) :reads t :may-signal t)
            translate-logical-pathname)
           ((:result-count 1 :result-type pathname :arity (3 . nil)
             :argument-types (t t t) :keywords nil :keyword-arguments t
             :requirements (:pathname-designators :compatible-wildcard-components)
             :reads t :may-signal t) translate-pathname)
           ((:result-count 1 :result-type pathname :arity (1 . 3) :keywords nil
             :argument-types (t t t) :requirements (:pathname-designators :valid-default-version)
             :reads t :writes nil :may-signal t) merge-pathnames)
           ((:result-count 1 :result-type list :arity (1 . nil) :argument-types (t)
             :keywords nil :keyword-arguments t :requirements (:pathname-designator)
             :reads t :may-signal t) directory)
           ((:result-count 1 :result-type (or pathname null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:pathname-designator)
             :reads t :may-signal t) probe-file)
           ((:result-count 2 :result-argument 0 :arity (1 . nil) :argument-types (t)
             :keywords (:verbose) :requirements (:pathname-designator)
             :reads t :writes t :may-signal t) ensure-directories-exist)
           ((:result-count 1 :result-type (or string null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:pathname-designator)
             :reads t :may-signal t) file-author)
           ((:result-count 1 :result-type (or unsigned-byte null) :arity (1 . 1)
             :keywords nil :argument-types (t) :requirements (:pathname-designator)
             :reads t :may-signal t) file-write-date)
           ((:result-count 3 :result-type pathname :arity (2 . 2) :keywords nil
             :argument-types (t (or pathname string))
             :requirements (:pathname-designators) :reads t :writes t :may-signal t)
            rename-file)
           ((:result-count 1 :result-type (eql t) :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:pathname-designator)
             :reads t :writes t :may-signal t) delete-file)
           ;; System construction: CLHS 24.2 and f_procla.htm. Source loading,
           ;; compilation hooks and implementation declarations can run code.
           ((:arity (1 . nil) :argument-types (t)
             :keywords (:output-file :verbose :print :external-format)
             :requirements (:pathname-designator) :reads t :writes t :callbacks t
             :may-signal t) compile-file)
           ((:result-count 1 :arity (1 . nil) :argument-types (t)
             :keywords (:verbose :print :if-does-not-exist :external-format)
             :requirements (:stream-or-pathname-designator) :truth-result t
             :reads t :writes t :callbacks t :may-signal t) load)
           ((:arity (1 . 1) :keywords nil :argument-types (t)
             :requirements (:string-designator) :reads t :writes t :may-signal t) provide)
           ((:arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:string-designator :optional-pathname-list-designator)
             :reads t :writes t :callbacks t :may-signal t) require)
           ((:arity (1 . 1) :keywords nil :argument-types (list)
             :requirements (:declaration-specifier) :reads t :writes t
             :may-signal t) proclaim)
           ;; Environment/time: CLHS 25.2. The universal-time epoch and
           ;; decoded-time ranges are target ANSI rules, not host limits.
           ((:result-count 9 :result-type (integer 0 59) :arity (1 . 2)
             :keywords nil :argument-types (unsigned-byte (rational -24 24))
             :reads t :may-signal t) decode-universal-time)
           ((:result-count 1 :result-type unsigned-byte :arity (6 . 7) :keywords nil
             :argument-types ((integer 0 59) (integer 0 59) (integer 0 23)
               (integer 1 31) (integer 1 12) unsigned-byte (rational -24 24))
             :requirements (:valid-calendar-date) :reads t :may-signal t) encode-universal-time)
           ((:result-count 1 :result-type unsigned-byte :arity (0 . 0) :keywords nil
             :reads t :writes nil :callbacks nil) get-universal-time
            get-internal-real-time get-internal-run-time)
           ((:result-count 9 :result-type (integer 0 59) :arity (0 . 0) :keywords nil
             :reads t :writes nil :callbacks nil) get-decoded-time)
           ((:result-count 1 :result-type null :arity (1 . 1) :keywords nil
             :argument-types ((real 0)) :may-signal t) sleep)
           ((:arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:string-designator :optional-package-designator-or-null)
             :reads t :may-signal t) apropos apropos-list)
           ((:result-count 0 :result-type null :writes t) apropos)
           ((:result-count 1 :result-type list :writes nil) apropos-list)
           ((:result-count 0 :result-type null :arity (1 . 2) :keywords nil
             :argument-types (t (or stream boolean)) :requirements (:output-stream-designator)
             :reads t :writes t :callbacks t :may-signal t) describe)
           ((:result-count 1 :result-type null :arity (1 . 1) :keywords nil
             :argument-types (t) :requirements (:extended-function-designator-or-lambda-expression)
             :reads t :writes t :may-signal t) disassemble)
           ((:arity (0 . 1) :keywords nil
             :argument-types ((member t nil :default)) :reads t :writes t
             :may-signal t) room)
           ((:arity (0 . 1) :keywords nil :argument-types (t)
             :requirements (:null-pathname-string-or-function-name)
             :reads t :writes t :callbacks t :may-signal t) ed)
           ((:arity (1 . 1) :keywords nil :argument-types (t)
             :reads t :writes t :callbacks t :may-signal t) inspect)
           ((:arity (0 . 1) :keywords nil :argument-types (t)
             :requirements (:pathname-designator) :reads t :writes t :may-signal t) dribble)
           ((:result-count 1 :result-type (or string null) :arity (0 . 0)
             :keywords nil :reads t :writes nil :callbacks nil)
            lisp-implementation-type lisp-implementation-version short-site-name long-site-name
            machine-instance machine-type machine-version software-type software-version)
           ((:result-count 1 :result-type (or pathname null) :arity (0 . 1)
             :keywords nil :argument-types ((or string list (member :unspecific)))
             :requirements (:host-designator) :reads t :may-signal t) user-homedir-pathname)
           ;; Conditions/restarts: CLHS 9.2 and condition accessor entries in
           ;; their own dictionaries. A condition accessor can expose shared
           ;; mutable data. Signaling/handler control remains distinct from IO.
           ((:result-count 1 :arity (1 . 1) :keywords nil :reads t :writes nil
             :callbacks nil :sharing :condition-slot :may-signal t)
            arithmetic-error-operands arithmetic-error-operation cell-error-name
            file-error-pathname package-error-package simple-condition-format-arguments
            simple-condition-format-control stream-error-stream type-error-datum
            type-error-expected-type print-not-readable-object unbound-slot-instance)
           ((:argument-types (arithmetic-error)) arithmetic-error-operands arithmetic-error-operation)
           ((:result-type list) arithmetic-error-operands simple-condition-format-arguments)
           ((:result-type (or function symbol)) arithmetic-error-operation)
           ((:argument-types (cell-error)) cell-error-name)
           ((:argument-types (file-error)) file-error-pathname)
           ((:argument-types (package-error)) package-error-package)
           ((:argument-types (simple-condition)) simple-condition-format-arguments
            simple-condition-format-control)
           ((:result-type (or string function)) simple-condition-format-control)
           ((:argument-types (stream-error) :result-type stream) stream-error-stream)
           ((:argument-types (type-error)) type-error-datum type-error-expected-type)
           ((:argument-types (print-not-readable)) print-not-readable-object)
           ((:argument-types (unbound-slot)) unbound-slot-instance)
           ((:arity (1 . nil) :keywords nil :argument-types (t) :rest-type t
             :requirements (:condition-designator-and-arguments)
             :reads t :callbacks t :may-signal t) error signal warn)
           ((:normal-return nil) error invoke-debugger)
           ((:result-count 1 :result-type null) signal warn)
           ((:result-count 1 :result-type null :arity (2 . nil) :keywords nil
             :argument-types ((or string function) t) :rest-type t
             :requirements (:format-control-and-condition-designator-arguments)
             :reads t :callbacks t :may-signal t) cerror)
           ((:arity (2 . nil) :keywords nil :argument-types (method (or string function))
             :rest-type t :requirements (:format-control-and-arguments)
             :reads t :callbacks t :may-signal t) invalid-method-error)
           ((:arity (1 . nil) :keywords nil :argument-types ((or string function))
             :rest-type t :requirements (:format-control-and-arguments)
             :reads t :callbacks t :may-signal t) method-combination-error)
           ((:arity (1 . 1) :keywords nil :argument-types (condition)
             :reads t :callbacks t :may-signal t) invoke-debugger)
           ((:result-count 1 :result-type null :arity (0 . nil) :keywords nil
             :argument-types ((or string function)) :rest-type t
             :requirements (:format-control-and-arguments)
             :reads t :callbacks t :may-signal t) break)
           ((:result-count 1 :result-type condition :arity (1 . nil) :keywords nil
             :argument-types (t) :rest-type t
             :requirements (:condition-subtype-specifier :initialization-argument-list)
             :reads t :callbacks t :may-signal t) make-condition)
           ((:result-count 1 :result-type list :arity (0 . 1) :keywords nil
             :argument-types ((or condition null)) :reads t :callbacks t
             :may-signal t) compute-restarts)
           ((:result-count 1 :result-type (or restart null) :arity (1 . 2) :keywords nil
             :argument-types ((or restart symbol) (or condition null))
             :requirements (:non-null-restart-identifier) :reads t :callbacks t
             :may-signal t) find-restart)
           ((:arity (1 . nil) :keywords nil :argument-types ((or restart symbol))
             :rest-type t :requirements (:restart-designator)
             :reads t :callbacks t :may-signal t) invoke-restart)
           ((:arity (1 . 1) :keywords nil :argument-types ((or restart symbol))
             :requirements (:restart-designator) :reads t :callbacks t
             :may-signal t) invoke-restart-interactively)
           ((:result-count 1 :result-type symbol :arity (1 . 1) :keywords nil
             :argument-types (restart) :reads t :writes nil :callbacks nil
             :may-signal t) restart-name)
           ((:arity (0 . 1) :keywords nil :argument-types ((or condition null))
             :reads t :callbacks t :may-signal t) abort continue muffle-warning)
           ;; No-restart CONTINUE/STORE-VALUE/USE-VALUE return NIL; when a
           ;; restart is present they can forward its values (f_abortc.htm).
           ((:arity (1 . 2) :keywords nil
             :argument-types (t (or condition null)) :reads t :callbacks t
             :may-signal t) store-value use-value)
           ;; Objects: CLHS 7.7. Retain method-sensitive result counts as
           ;; unknown even where the standard primary methods return one value.
           ;; No generic lambda-list fact proves a fixed effective method.
           ((:result-count 1 :result-type class :arity (1 . 1) :keywords nil
             :argument-types (t) :reads t :writes nil :callbacks nil) class-of)
           ((:result-count 1 :result-type (or class null) :arity (1 . 3) :keywords nil
             :argument-types (symbol t t) :requirements (:optional-environment)
             :reads t :writes nil :may-signal t) find-class)
           ((:result-count 1 :arity (2 . 2) :keywords nil :argument-types (t symbol)
             :reads t :callbacks t :may-signal t) slot-value slot-boundp slot-exists-p slot-makunbound)
           ((:truth-result t) slot-boundp slot-exists-p)
           ((:result-argument 0 :sharing :argument :writes t) slot-makunbound)
           ((:result-count 2 :arity (1 . nil) :argument-types (t)
             :keywords (:slot-names :environment)
             :requirements (:optional-slot-name-list :optional-environment)
             :reads t :callbacks t :may-signal t) make-load-form-saving-slots)
           ((:result-count 1 :result-type generic-function :arity (1 . nil)
             :argument-types (t) :keywords (:argument-precedence-order :declare :documentation
               :environment :generic-function-class :lambda-list :method-class :method-combination)
             :requirements (:function-name :generic-function-options)
             :reads t :writes t :callbacks t :may-signal t) ensure-generic-function)
           ((:arity (1 . 1) :keywords nil :argument-types (method)) function-keywords method-qualifiers)
           ((:arity (1 . nil) :argument-types (class) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:initialization-argument-list)) allocate-instance)
           ((:arity (1 . nil) :argument-types (t) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:initialization-argument-list)) initialize-instance reinitialize-instance)
           ((:arity (1 . nil) :argument-types ((or class symbol)) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:class-designator :initialization-argument-list)) make-instance)
           ((:arity (2 . nil) :argument-types (t (or list (eql t))) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:slot-name-list-or-t :initialization-argument-list)) shared-initialize)
           ((:arity (2 . nil) :argument-types (t (or class symbol)) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:class-designator :initialization-argument-list)) change-class)
           ((:arity (2 . nil) :argument-types (t t) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:initialization-argument-list)) update-instance-for-different-class)
           ((:arity (4 . nil) :argument-types (t list list list) :keywords nil
             :keyword-arguments t :allow-other-keys t
             :requirements (:slot-name-lists :property-list :initialization-argument-list))
            update-instance-for-redefined-class)
           ((:arity (1 . 1) :keywords nil :argument-types (class)) class-name)
           ((:arity (2 . 2) :keywords nil :argument-types (generic-function list)
             :requirements (:proper-argument-list)) compute-applicable-methods)
           ((:arity (2 . 2) :keywords nil :argument-types (t stream)) describe-object print-object)
           ((:arity (2 . 2) :keywords nil :argument-types (t symbol)) documentation)
           ((:arity (3 . 4) :keywords nil :argument-types (generic-function list list t)
             :requirements (:proper-qualifier-list :specializer-list)) find-method)
           ((:arity (1 . 1) :keywords nil :argument-types ((or class symbol))
             :requirements (:class-designator)) make-instances-obsolete)
           ((:arity (1 . 2) :keywords nil :argument-types (t t)
             :requirements (:optional-environment)) make-load-form)
           ((:arity (1 . nil) :keywords nil :argument-types (generic-function)
             :rest-type t) no-applicable-method)
           ((:arity (2 . nil) :keywords nil :argument-types (generic-function method)
             :rest-type t) no-next-method)
           ((:arity (2 . 2) :keywords nil :argument-types (generic-function method)) add-method remove-method)
           ((:arity (4 . 5) :keywords nil :argument-types (class t symbol
               (member setf slot-boundp slot-makunbound slot-value) t)) slot-missing)
           ((:arity (3 . 3) :keywords nil :argument-types (class t symbol)) slot-unbound)
           ((:arity (0 . nil) :keywords nil :rest-type t) call-next-method)
           ((:result-count 1 :arity (0 . 0) :keywords nil :truth-result t) next-method-p)))
      (dolist (name (cdr group))
        (setf (gethash name table) (cons (car group) (gethash name table)))))
    ;; f_car_c.htm, f_firstc.htm and f_rest.htm. Each intermediate CAR/CDR
    ;; must still be checked unless independently proved safe. CDR can return
    ;; any object, including the dotted tail of a list, so no LIST result type.
    (dolist (group '(((:car) car first) ((:cdr) cdr rest)
                     ((:car :car) caar) ((:cdr :car) cadr second)
                     ((:car :cdr) cdar) ((:cdr :cdr) cddr)
                     ((:car :car :car) caaar) ((:cdr :car :car) caadr)
                     ((:car :cdr :car) cadar) ((:cdr :cdr :car) caddr third)
                     ((:car :car :cdr) cdaar) ((:cdr :car :cdr) cdadr)
                     ((:car :cdr :cdr) cddar) ((:cdr :cdr :cdr) cdddr)
                     ((:car :car :car :car) caaaar)
                     ((:cdr :car :car :car) caaadr)
                     ((:car :cdr :car :car) caadar)
                     ((:cdr :cdr :car :car) caaddr)
                     ((:car :car :cdr :car) cadaar)
                     ((:cdr :car :cdr :car) cadadr)
                     ((:car :cdr :cdr :car) caddar)
                     ((:cdr :cdr :cdr :car) cadddr fourth)
                     ((:car :car :car :cdr) cdaaar)
                     ((:cdr :car :car :cdr) cdaadr)
                     ((:car :cdr :car :cdr) cdadar)
                     ((:cdr :cdr :car :cdr) cdaddr)
                     ((:car :car :cdr :cdr) cddaar)
                     ((:cdr :car :cdr :cdr) cddadr)
                     ((:car :cdr :cdr :cdr) cdddar)
                     ((:cdr :cdr :cdr :cdr) cddddr)
                     ((:cdr :cdr :cdr :cdr :car) fifth)
                     ((:cdr :cdr :cdr :cdr :cdr :car) sixth)
                     ((:cdr :cdr :cdr :cdr :cdr :cdr :car) seventh)
                     ((:cdr :cdr :cdr :cdr :cdr :cdr :cdr :car) eighth)
                     ((:cdr :cdr :cdr :cdr :cdr :cdr :cdr :cdr :car) ninth)
                     ((:cdr :cdr :cdr :cdr :cdr :cdr :cdr :cdr :cdr :car) tenth)))
      (let ((facts (list :list-path (car group) :result-count 1 :arity '(1 . 1)
                         :keywords nil :argument-types '(list)
                         :requirements '(:valid-intermediate-list-path)
                         :reads t :writes nil :callbacks nil :may-signal t)))
        (dolist (name (cdr group))
          (setf (gethash name table) (cons facts (gethash name table))))))
    table))

;; Only these two SETF *function names* are explicitly standardized (CLHS
;; f_docume.htm, f_class_.htm and f_class_1.htm). Other standardized places
;; may use SETF expanders instead. Neither host nor MIRICL setter availability
;; upgrades a place to an ANSI function name.
;; https://www.lispworks.com/documentation/HyperSpec/Issues/iss305_w.htm
(defparameter *standard-setf-function-contracts*
  '((class-name (:kind :generic-function :callbacks t :setf-function t
                :arity (2 . 2) :keywords nil :argument-types (symbol class)))
    (documentation (:kind :generic-function :callbacks t :setf-function t
                    :arity (3 . 3) :keywords nil :argument-types (string t symbol)))))

(defun standard-function-contract (name)
  "Opaque target ANSI descriptor or NIL. Malformed names give no proof.
The two-cons SETF recognition is bounded even for dotted or cyclic input."
  (cond ((symbolp name) (gethash name *standard-function-contracts*))
        ((and (consp name) (eq (car name) 'setf)
              (consp (cdr name)) (null (cddr name)) (symbolp (cadr name)))
         (cdr (assoc (cadr name) *standard-setf-function-contracts* :test #'eq)))))

(defun standard-function-name-p (name)
  (not (null (standard-function-contract name))))

(defun standard-function-property (name property)
  "Return fact, knownp. NIL,T is a known false fact; NIL,NIL is unknown.
Facts describe successful ANSI calls, not arity/type-check removal or a
checked native entry. Inventory membership alone licenses neither."
  (dolist (facts (standard-function-contract name) (values nil nil))
    (loop for tail on facts by #'cddr
          when (eq (car tail) property)
            do (return-from standard-function-property (values (cadr tail) t)))))

(defun standard-function-result-type (name &optional argument-types)
  (if (and (eql (standard-function-property name :result-argument) 0)
           (consp argument-types) (null (cdr argument-types)))
      (car argument-types)
      (multiple-value-bind (type knownp) (standard-function-property name :result-type)
        (if knownp type t))))

(defun standard-function-result-count (name &optional argument-count)
  "Exact successful result count, or NIL; unknown never means one value."
  (let ((count (standard-function-property name :result-count)))
    (if (eq count :argument-count)
        (and (integerp argument-count) (>= argument-count 0) argument-count)
        count)))

(defun standard-function-predicate-type (name)
  (standard-function-property name :predicate-type))

(defun standard-function-list-path (name)
  "CAR/CDR operations in evaluation order, or NIL when no path is known."
  (standard-function-property name :list-path))
