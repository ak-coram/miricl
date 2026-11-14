// These forms run unchanged in Node and in the browser's Lisp environment.
export const compilerCases = [
  ['inline cons tag checks', `(let ((function (miricl-compiler:compile-native
      '(lambda (object) (declare (optimize (safety 3)) (inline consp))
         (values (consp object) (if (consp object) :cons :other))))))
    (and (equal (multiple-value-list (funcall function '(1 . 2))) '(t :cons))
         (every (lambda (object)
                  (equal (multiple-value-list (funcall function object)) '(nil :other)))
                (list nil 17 (expt 2 100) 1/2 #\\A "cons" #(1 2) #'car (make-hash-table)))))`],
  ['checked inline list access', `(let ((function (miricl-compiler:compile-native
      '(lambda (object) (declare (optimize (safety 3)) (inline car cdr))
         (values (car object) (cdr object))))))
    (and (equal (multiple-value-list (funcall function nil)) '(nil nil))
         (equal (multiple-value-list (funcall function '(1 . 2))) '(1 2))
         (let ((object (list :cycle))) (setf (cdr object) object)
           (multiple-value-bind (first rest) (funcall function object)
             (and (eq first :cycle) (eq rest object))))
         (handler-case (progn (funcall function 42) nil)
           (type-error (condition) (= (type-error-datum condition) 42)))
         (equal (multiple-value-list (funcall function '(3 . 4))) '(3 4))))`],
  ['literal membership conditions', `(let ((function (miricl-compiler:compile-native
      '(lambda (key) (let ((events nil))
         (values (if (member (progn (push :key events) (values key :extra)) '(nil 1 :target #\\A))
                     :yes :no) (reverse events))))))
      (empty (miricl-compiler:compile-native '(lambda () (let ((calls 0))
        (values (if (member (incf calls) '()) :bad :empty) calls)))))
      (local (miricl-compiler:compile-native '(lambda ()
        (flet ((member (item list) (declare (ignore item list)) :local))
          (if (member :missing '(:target)) :local :bad)))))
      (keywords (miricl-compiler:compile-native '(lambda ()
        (if (member 2 '((1 . :one) (2 . :two)) :key #'car) :key :bad)))))
    (and (equal (multiple-value-list (funcall function nil)) '(:yes (:key)))
         (equal (multiple-value-list (funcall function #\\A)) '(:yes (:key)))
         (equal (multiple-value-list (funcall function 1.0)) '(:no (:key)))
         (equal (multiple-value-list (funcall function :target)) '(:yes (:key)))
         (equal (multiple-value-list (funcall function :missing)) '(:no (:key)))
         (equal (multiple-value-list (funcall empty)) '(:empty 1))
         (eq (funcall local) :local) (eq (funcall keywords) :key)
         (let ((form 'x))
           (dotimes (i 200)
             (setq form (list 'if (list 'member form '(quote (1 2))) '(values 1 :deep) 0)))
           (let ((deep (miricl-compiler:compile-native (list 'lambda '(x) form))))
             (and (equal (multiple-value-list (funcall deep 1)) '(1 :deep))
                  (= (funcall deep 9) 0))))))`],
  ['CASE match context and values', `(let ((function (miricl-compiler:compile-native
      '(lambda (key) (let ((evaluations 0))
         (values (case (progn (incf evaluations) key)
                   (nil :unreachable) ((nil) :nil) (1 :integer) (1 :shadowed)
                   (1.0f0 :single) (1.0d0 :double) (#\\A :character)
                   ((t) :true) ((otherwise) :symbol) ((2 3) :group) (4)
                   (otherwise :default)) evaluations)))))
      (counts (miricl-compiler:compile-native
        '(lambda (key) (case key (0 (values)) (1 (values 17 23))))))
      (strict (miricl-compiler:compile-native
        '(lambda (key) (ecase key (1 (values 17 23)) ((2 3) :group)))))
      (member-tail (miricl-compiler:compile-native '(lambda (key) (member key '(1 2 3))))))
    (and (equal (mapcar (lambda (key) (multiple-value-list (funcall function key)))
                        '(nil 1 1.0f0 1.0d0 #\\A t otherwise 2 3 4 :missing))
                '((:nil 1) (:integer 1) (:single 1) (:double 1) (:character 1)
                  (:true 1) (:symbol 1) (:group 1) (:group 1) (nil 1) (:default 1)))
         (null (multiple-value-list (funcall counts 0)))
         (equal (multiple-value-list (funcall counts 1)) '(17 23))
         (equal (multiple-value-list (funcall counts 9)) '(nil))
         (equal (multiple-value-list (funcall strict 1)) '(17 23))
         (handler-case (progn (funcall strict :missing) nil)
           (type-error (condition)
             (and (eq (type-error-datum condition) :missing)
                  (typep 1 (type-error-expected-type condition))
                  (typep 3 (type-error-expected-type condition))
                  (not (typep :missing (type-error-expected-type condition))))))
         (eq (funcall strict 2) :group)
         (equal (funcall member-tail 2) '(2 3))
         (eq (cdr (funcall member-tail 1)) (funcall member-tail 2))))`],
  ['deep compiler expression trees', `(let ((form '(values x :deep)))
    (dotimes (i 1000)
      (setq form (list 'let '((y x)) (list 'if 'y
        (list 'locally '(declare (optimize (debug 1))) (list 'progn form)) :absent))))
    (let ((function (miricl-compiler:compile-native (list 'lambda '(x) form))))
      (and (equal (multiple-value-list (funcall function 42)) '(42 :deep))
           (eq (funcall function nil) :absent))))`],
  ['general tail calls', `(funcall (miricl-compiler:compile-native '(lambda ()
      (labels ((even-step (n function)
                 (if (zerop n)
                     (let ((*compiler-tail-special* t))
                       (declare (special *compiler-tail-special*))
                       (values *compiler-tail-special* :done))
                     (funcall function (1- n) #'even-step)))
               (odd-step (n function)
                 (if (zerop n)
                     (block finished (funcall (lambda () (return-from finished (values nil :done)))))
                     (apply function (1- n) (list #'odd-step)))))
        (equal (multiple-value-list (even-step 50000 #'odd-step)) '(t :done))))))`],
  ['integer tier boundaries', `(let ((function (miricl-compiler:compile-native
      '(lambda (a b) (list (+ a 1) (- b 1) (- b) (* a a)
                           (ash b -63) (ldb (byte 64 0) b)))))
      (typed (miricl-compiler:compile-native
        '(lambda (a b) (values (+ (the (signed-byte 63) a) (the (signed-byte 63) b)))))))
    (and (equal (funcall function 1073741823 -9223372036854775808)
           '(1073741824 -9223372036854775809 9223372036854775808
             1152921502459363329 -1 9223372036854775808))
      (= (funcall typed 4611686018427387903 4611686018427387903) 9223372036854775806)
      (= most-positive-fixnum 9223372036854775807)
      (typep 9223372036854775807 'fixnum) (typep 9223372036854775808 'bignum)))`],
  ['subnormal exponential formatting', `(let* ((source
      '(lambda (x)
         (let ((*read-default-float-format* (type-of x)))
           (and (string= (format nil "~E" x) (prin1-to-string x))
                (string= (with-output-to-string (stream)
                           (funcall (formatter "~E") stream x))
                         (prin1-to-string x))))))
      (interpreted (eval source)) (compiled (miricl-compiler:compile-native source)))
    (and (compiled-function-p compiled)
      (loop for least in (list least-positive-single-float least-positive-double-float)
        always (loop for n from 1 to 64 always
          (loop for sign in '(1 -1) for x = (* n sign least)
            always (and (funcall interpreted x) (funcall compiled x)))))
      (equal (list (format nil "~,1E" least-positive-single-float)
                   (format nil "~,,,2E" least-positive-single-float))
             '("1.4e-45" "10.0e-46"))))`],
  ['arithmetic and branches', `(progn
    (defparameter *compiler-test-function* (miricl-compiler:compile-native
      '(lambda (x) (if x (+ x 2) 0))))
    (and (compiled-function-p *compiler-test-function*)
         (= (funcall *compiler-test-function* 40) 42)
         (= (funcall *compiler-test-function* nil) 0)))`],
  ['shared mutable captures', `(let ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () (incf x)) (lambda () x))))))
    (multiple-value-bind (increment read) (funcall factory 41)
      (and (= (funcall increment) 42) (= (funcall read) 42)
           (= (funcall increment) 43) (= (funcall read) 43))))`],
  ['unassigned transitive captures', `(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (lambda () (lambda () x)))))
      (object (list 1)) (middle (funcall factory object)) (inner (funcall middle)))
    (setf (car object) 42)
    (and (eq (funcall inner) object) (= (car (funcall inner)) 42)))`],
  ['zero and multiple values', `(let ((function (miricl-compiler:compile-native
      '(lambda (flag) (multiple-value-bind (a b c) (if flag (values 1 2) (values))
                       (values a b c))))))
    (and (equal (multiple-value-list (funcall function t)) '(1 2 nil))
         (equal (multiple-value-list (funcall function nil)) '(nil nil nil))))`],
  ['dynamic bindings', `(progn (defvar *compiler-test-special* :outside)
    (let ((function (miricl-compiler:compile-native
        '(lambda (*compiler-test-special*)
           (values *compiler-test-special* (symbol-value '*compiler-test-special*))))))
      (and (equal (multiple-value-list (funcall function :inside)) '(:inside :inside))
           (eq *compiler-test-special* :outside))))`],
  ['loops and keyword arguments', `(let ((function (miricl-compiler:compile-native
      '(lambda (n &key (initial 0))
         (let ((sum initial)) (dotimes (i n sum) (incf sum i)))))))
    (= (funcall function 10 :initial 7) 52))`],
  ['keyword parameter scope and rest identity', `(progn
    (defvar *keyword-parameter-special* :outside)
    (let ((special (miricl-compiler:compile-native
            '(lambda (*keyword-parameter-special* &rest tail
                      &key (value *keyword-parameter-special* supplied))
               (declare (special *keyword-parameter-special*))
               (values value supplied tail *keyword-parameter-special*))))
          (shadow (miricl-compiler:compile-native
            '(lambda (x &key (y x) &aux (x :aux))
               (declare (type symbol x)) (list y x))))
          (typed (miricl-compiler:compile-native
            '(lambda (x &key (y (1+ x)))
               (declare (type (unsigned-byte 8) x)) (list y x)))))
      (and (equal (multiple-value-list (funcall special 7)) '(7 nil nil 7))
        (equal (multiple-value-list (funcall special 9 :value 11 :value 13))
               '(11 t (:value 11 :value 13) 9))
        (eq *keyword-parameter-special* :outside)
        (equal (funcall shadow 42) '(42 :aux))
        (equal (funcall typed 255) '(256 255))
        (handler-case (progn (funcall special) nil) (program-error () t))
        (handler-case (progn (funcall special 7 :unknown t) nil) (program-error () t))
        (equal (multiple-value-list (funcall special 17)) '(17 nil nil 17))
        (eq *keyword-parameter-special* :outside))))`],
  ['global redefinition', `(progn
    (defun compiler-test-callee (x) (+ x 1))
    (let ((caller (miricl-compiler:compile-native '(lambda (x) (compiler-test-callee x)))))
      (defun compiler-test-callee (x) (+ x 2))
      (= (funcall caller 40) 42)))`],
  ['symbol iteration', `(let ((package (make-package "MIRICL-NATIVE-SYMBOLS" :use nil)))
    (unwind-protect
      (let* ((internal (intern "INTERNAL" package)) (external (intern "EXTERNAL" package))
             (function (miricl-compiler:compile-native
               '(lambda (package)
                  (let ((evaluations 0) (seen nil) (external nil) (all nil))
                    (multiple-value-bind (final count)
                        (do-symbols (symbol (progn (incf evaluations) package)
                                            (values symbol (length seen)))
                          (push symbol seen) (setq symbol :changed))
                      (do-external-symbols (symbol package) (push symbol external))
                      (do-all-symbols (symbol)
                        (when (eq (symbol-package symbol) package) (push symbol all)))
                      (values evaluations final count seen external all)))))))
        (export external package)
        (multiple-value-bind (evaluations final count seen exported all) (funcall function package)
          (and (= evaluations 1) (null final) (= count 2)
               (null (set-exclusive-or seen (list internal external)))
               (equal exported (list external))
               (null (set-exclusive-or all (list internal external))))))
      (delete-package package)))`],
  ['literal cycles and identity', `(let* ((literal (list 1))
      (source (list 'lambda nil (list 'quote literal))))
    (setf (cdr literal) literal)
    (let ((function (miricl-compiler:compile-native source)))
      (and (eq (funcall function) literal) (eq (cdr (funcall function)) literal))))`],
  ['native condition propagation', `(let ((function (miricl-compiler:compile-native '(lambda () (/ 1 0)))))
    (handler-case (progn (funcall function) nil) (division-by-zero () t)))`],
  ['native catches and throws', `(let ((function (miricl-compiler:compile-native
      '(lambda (tag flag) (catch tag (if flag (throw tag (values 1 2)) (values)))))))
    (let ((tag (list :tag)))
      (and (equal (multiple-value-list (funcall function tag t)) '(1 2))
           (null (multiple-value-list (funcall function tag nil))))))`],
  ['native handlers and no-error values', `(let ((function (miricl-compiler:compile-native
      '(lambda (x) (handler-case (if x (/ 1 0) (values 20 22))
                    (division-by-zero (c) (typep c 'division-by-zero))
                    (:no-error (a b) (+ a b)))))))
    (and (eq (funcall function t) t) (= (funcall function nil) 42)))`],
  ['native cleanup on transfers', `(let ((function (miricl-compiler:compile-native
      '(lambda (override) (let ((cleaned nil))
         (values (catch 'done (unwind-protect (throw 'done 1)
                               (setq cleaned t) (if override (throw 'done 2)))) cleaned))))))
    (and (equal (multiple-value-list (funcall function nil)) '(1 t))
         (equal (multiple-value-list (funcall function t)) '(2 t))))`],
  ['native signaling handler', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((seen nil))
         (handler-bind ((simple-condition (lambda (condition) (setq seen (typep condition 'simple-condition)))))
           (signal 'simple-condition)) seen)))))
    (funcall function))`],
  ['native type dispatch', `(let ((function (miricl-compiler:compile-native
      '(lambda (x) (typecase x ((integer 0 10) (values x :small))
                              (string (values (length x) :text)) (t (values)))))))
    (and (equal (multiple-value-list (funcall function 7)) '(7 :small))
         (equal (multiple-value-list (funcall function "abc")) '(3 :text))
         (null (multiple-value-list (funcall function 20)))))`],
  ['native arithmetic loop limits', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((up 0) (down 0))
         (loop for i fixnum from 1 to 5 by 2 do (incf up i))
         (loop for i fixnum downfrom 5 above 0 by 2 do (incf down i))
         (values up down))))))
    (equal (multiple-value-list (funcall function)) '(9 9)))`],
  ['native assertion continuation', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((ready nil))
         (handler-bind ((error (lambda (condition) (declare (ignore condition))
                                 (setq ready t) (invoke-restart 'continue))))
           (assert ready)) ready)))))
    (funcall function))`],
  ['nonlocal lexical block values and shadowing', `(let ((function (miricl-compiler:compile-native
      '(lambda (many)
         (block done
           (flet ((exit () (return-from done (if many (values 17 23) (values)))))
             (block done (mapcar (lambda (x) (declare (ignore x)) (exit)) '(1 2)))
             :unreachable))))))
    (and (equal (multiple-value-list (funcall function t)) '(17 23))
         (null (multiple-value-list (funcall function nil)))))`],
  ['nonlocal block primary value', `(let ((function (miricl-compiler:compile-native
      '(lambda () (list (block done
                         (funcall (lambda () (return-from done (values 17 23)))))
                       (block done (funcall (lambda () (return-from done (values))))))))))
    (equal (funcall function) '(17 nil)))`],
  ['nonlocal GO keeps all tags live through cleanup', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((n 0) (events nil))
         (tagbody again
           (incf n)
           (unwind-protect
             (when (< n 3) (funcall (lambda () (go again))))
             (push n events))
           (funcall (lambda () (go 17)))
           (setq n :bad)
          17)
         (and (= n 3) (equal (reverse events) '(1 2 3))))))))
    (funcall function))`],
  ['nonlocal GO replaces a pending error', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((escaped nil))
         (tagbody (unwind-protect (error "replaced") (go done)) done (setq escaped t))
         escaped)))))
    (funcall function))`],
  ['local exit expires nested captured block', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((exit nil))
         (block outer (block inner
           (setq exit (lambda () (return-from inner 17)))
           (return-from outer 23)))
         (handler-case (progn (funcall exit) nil) (control-error () t)))))))
    (funcall function))`],
  ['nonlocal block unwinding and replacement exit', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((x :outer) (seen nil)) (declare (special x))
         (list (block done (let ((x :inner)) (declare (special x))
                  (unwind-protect (return-from done (values 17 23))
                    (setq seen x) (return-from done (values 31 37)))))
               seen x))))))
    (equal (funcall function) '(31 :inner :outer)))`],
  ['native assertion skips places on success', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((calls 0) (data (vector 1)))
         (and (null (assert t ((aref data (progn (incf calls) 0)))
                           (progn (incf calls) "unused")))
              (= calls 0)))))))
    (funcall function))`],
  ['native assertion place correction', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((data (vector 0 0)) (calls 0))
         (handler-bind ((error (lambda (condition) (continue condition))))
           (assert (= (+ (aref data 0) (aref data 1)) 5)
             ((values (aref data (progn (incf calls) 0)) (aref data 1)))))
         (and (= calls 1) (equalp data #(2 3))))))))
    (let ((*query-io* (make-two-way-stream
            (make-string-input-stream (format nil "y~%(values 2 3)~%"))
            (make-string-output-stream))))
      (funcall function)))`],
  ['restart case associates and unwinds native clauses', `(let ((function (miricl-compiler:compile-native
      '(lambda ()
         (let ((other (make-condition 'simple-error)))
           (handler-bind ((error (lambda (condition)
             (assert (find-restart 'use-values condition))
             (assert (null (find-restart 'use-values other)))
             (invoke-restart 'use-values 17 23))))
             (restart-case (error "Restart me")
               (use-values (a b) (values a b (null (find-restart 'use-values)))))))))))
    (equal (multiple-value-list (funcall function)) '(17 23 t)))`],
  ['restart binding options and interactive native callback', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((prefix "Use") (n 0))
         (restart-bind ((supply (lambda (a b) (values (+ a b) n))
                           :report-function (progn (incf n) (lambda (s) (write-string prefix s)))
                           :interactive-function (lambda () '(17 23))))
           (list (format nil "~A" (find-restart 'supply))
                 (multiple-value-list (invoke-restart-interactively 'supply)))))))))
    (equal (funcall function) '("Use" (40 1))))`],
  ['simple restart preserves normal and restarted values', `(let ((function (miricl-compiler:compile-native
      '(lambda (abortp)
         (with-simple-restart (stop "Stop ~D" 17)
           (if abortp (invoke-restart 'stop) (values 23 31)))))))
    (and (equal (multiple-value-list (funcall function nil)) '(23 31))
         (equal (multiple-value-list (funcall function t)) '(nil t))))`],
  ['local function initializers use enclosing block scope', `(let ((function (miricl-compiler:compile-native
      '(lambda ()
         (list (block f (flet ((f (&key (x (return-from f :keyword))) x)) (f) :bad))
               (block g (labels ((g (&aux (x (return-from g :aux))) x)) (g) :bad)))))))
    (equal (funcall function) '(:keyword :aux)))`],
  ['terminated value expressions preserve GO and cleanup', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((result nil))
         (tagbody
           (unwind-protect (prog1 (progn (go done)) (error "Unreachable"))
             (push :cleaned result))
           (error "Unreachable")
           done)
         (values result (typecase 42 (t :default) (integer :unreachable))))))))
    (equal (multiple-value-list (funcall function)) '((:cleaned) :default)))`],
  ['correctable places preserve selection and values', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((data (vector :bad)) (selected 0))
         (handler-bind ((type-error (lambda (c) (store-value :ok c))))
           (let ((result (multiple-value-list
                   (ccase (aref data (progn (incf selected) 0)) (:ok (values 17 23))))))
             (and (= selected 1) (eq (aref data 0) :ok) (equal result '(17 23))))))))))
    (funcall function))`],
  ['native check-type retries and evaluates description once', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((value :bad) (attempts 0) (described 0))
         (handler-bind ((type-error (lambda (c) (incf attempts)
                                      (store-value (if (= attempts 1) :still-bad 42) c))))
           (check-type value integer (progn (incf described) "an integer")))
         (and (= value 42) (= attempts 2) (= described 1)))))))
    (funcall function))`],
  ['native PROGV preserves lexical bindings and restores unbound variables', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((x :lexical))
         (and (equal (progv '(x missing-progv-value) '(23)
                       (list x (symbol-value 'x) (boundp 'missing-progv-value)))
                     '(:lexical 23 nil))
              (not (boundp 'missing-progv-value))))))))
    (funcall function))`],
  ['native condition initializers and reports capture lexical state', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((n 17) (initialized 0))
         (define-condition compiler-native-condition (error)
           ((a :initform (progn (incf initialized) n) :reader compiler-native-condition-a)
            (b :initarg :b))
           (:default-initargs :b (1+ n))
           (:report (lambda (condition stream)
                      (format stream "~D/~D" (compiler-native-condition-a condition) (slot-value condition 'b)))))
         (setq n 23)
         (let* ((object (make-condition 'compiler-native-condition))
                (slot (find 'a (mop:class-slots (find-class 'compiler-native-condition))
                            :key #'mop:slot-definition-name)))
           (and (= initialized 1) (= (compiler-native-condition-a object) 23)
                (= (slot-value object 'b) 24) (string= (format nil "~A" object) "23/24")
                (compiled-function-p (mop:slot-definition-initfunction slot)))))))))
    (funcall function))`],
  ['no-error handler scope', `(let ((function (miricl-compiler:compile-native
      '(lambda () (handler-case
          (handler-case 42 (simple-error () :wrong)
            (:no-error (x) (declare (ignore x)) (error "Outside handler scope")))
          (simple-error () :outer))))))
    (eq (funcall function) :outer))`],
  ['cleanup preserves the original condition', `(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((cleaned nil))
         (handler-case (unwind-protect (/ 1 0) (setq cleaned t) (values 9 8))
           (division-by-zero () (values cleaned :original))))))))
    (equal (multiple-value-list (funcall function)) '(t :original)))`],
];

// Runtime/browser smoke coverage checks the native ABI. The complete set runs
// once in FILE-CODE-SHARING through a fresh FASL; ANSI owns broad conformance.
const smokeNames = new Set([
  'literal membership conditions',
  'CASE match context and values',
  'deep compiler expression trees',
  'general tail calls',
  'integer tier boundaries',
  'arithmetic and branches', 'shared mutable captures', 'unassigned transitive captures',
  'zero and multiple values', 'dynamic bindings', 'literal cycles and identity',
  'native condition propagation', 'native cleanup on transfers',
]);
export const compilerSmokeCases = compilerCases.filter(([name]) => smokeNames.has(name));
