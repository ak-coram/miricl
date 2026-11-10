const replace = (source, before, after) => {
  if (source.split(before).length !== 2) throw new Error(`Coalton host port anchor changed: ${before.slice(0, 90)}`);
  return source.replace(before, after);
};
export function portCoaltonSource(name, bytes) {
  let source = bytes.toString('utf8');
  if (name === '/quicklisp/trivial-features/trivial-features.asd')
    source = replace(source, '#-(or sbcl clisp', '#-(or miricl sbcl clisp');
  if (name === '/quicklisp/trivial-garbage/trivial-garbage.asd')
    source = replace(source, '#-(or cmu scl', '#-(or miricl cmu scl');
  if (name === '/quicklisp/trivial-garbage/trivial-garbage.lisp')
    source += `
#+miricl (progn
  (defun gc (&key full verbose)
    (declare (ignore full verbose))
    ;; WasmGC collection is controlled by the browser/runtime.
    nil)
  (defun make-weak-pointer (object)
    (declare (ignore object)) (error "WasmGC weak pointers are unavailable."))
  (defun weak-pointer-p (object) (declare (ignore object)) nil)
  (defun weak-pointer-value (object)
    (declare (ignore object)) (error "WasmGC weak pointers are unavailable."))
  (defun finalize (object function)
    (declare (ignore object function)) (error "WasmGC finalizers are unavailable."))
  (defun cancel-finalization (object)
    (declare (ignore object)) (error "WasmGC finalizers are unavailable.")))
`;
  if (name === '/coalton/coalton-compiler.asd')
    source = replace(source, '"trivial-gray-streams")', '#-miricl "trivial-gray-streams")');
  if (name === '/coalton/library/classes.ct') {
    source = replace(source, '#-(or sbcl allegro ccl dotcl)', '#-(or miricl sbcl allegro ccl dotcl)');
    source = replace(source, '#+sbcl', '#+miricl\n  (repr :native (cl:and cl:fixnum cl:unsigned-byte))\n\n  #+sbcl');
  }
  if (name === '/coalton/library/hash.ct')
    source = replace(source, '#+(or ccl dotcl)', '#+(or miricl ccl dotcl)');
  if (name === '/coalton/library/math/num-defining-macros.lisp')
    source = replace(source, '#-sbcl (cl:1+ (cl:floor (cl:log cl:most-positive-fixnum 2)))',
      '#+miricl (cl:1+ (cl:integer-length cl:most-positive-fixnum))\n    #-(or sbcl miricl) (cl:1+ (cl:floor (cl:log cl:most-positive-fixnum 2)))');
  if (name === '/coalton/src/source.lisp') {
    const start = source.indexOf('(defclass char-position-stream '), end = source.indexOf('(defun utf-8-char-width ', start);
    if (start < 0 || end < 0) throw new Error('Coalton character-position stream port anchor changed');
    source = source.slice(0, start) + '#+miricl (deftype char-position-stream () \'string-stream)\n'
      + '#-miricl (progn\n' + source.slice(start, end) + ')\n' + source.slice(end);
    const methodStart = source.indexOf('(defmethod source-stream ((self source-file))');
    const methodEnd = source.indexOf('(defclass source-string ', methodStart);
    if (methodStart < 0 || methodEnd < 0) throw new Error('Coalton file source port anchor changed');
    source = source.slice(0, methodStart) + `#+miricl
(defmethod source-stream ((self source-file))
  ;; Native string streams count characters, including Unicode, rather than
  ;; the byte offsets of the underlying file. No Gray wrapper is necessary.
  (let ((stream (make-string-input-stream
                 (uiop:read-file-string (input-name self)
                   :external-format (source-external-format)))))
    (when (plusp (file-offset self)) (file-position stream (file-offset self)))
    stream))
#-miricl (progn
` + source.slice(methodStart, methodEnd) + ')\n' + source.slice(methodEnd);
  }
  if (name === '/quicklisp/named-readtables/src/cruft.lisp')
    source += '\n' + namedReadtablesPort;
  if (name === '/quicklisp/named-readtables/named-readtables.asd') {
    source = replace(source, ':depends-on ("mgl-pax-bootstrap")', ':depends-on (#-miricl "mgl-pax-bootstrap")');
    source = replace(source, '(:file "doc")', '#-miricl (:file "doc")');
  }
  if (name === '/quicklisp/float-features/float-features.lisp') source = floatFeaturesPort;
  if (name === '/quicklisp/float-features/nan.lisp')
    source = '(in-package :float-features)\n' + ['short', 'single', 'double', 'long'].map(format =>
      `(define-symbol-macro ${format}-float-nan (bits-${['short', 'single'].includes(format) ? 'single-float #x7fc00000' : 'double-float #x7ff8000000000000'}))`).join('\n');
  if (name === '/quicklisp/float-features/infinity.lisp')
    source = '(in-package :float-features)\n' + ['short', 'single', 'double', 'long'].flatMap(format =>
      ['positive', 'negative'].map(sign => {
        const single = ['short', 'single'].includes(format);
        const bits = single ? (sign === 'positive' ? '#x7f800000' : '#xff800000')
          : (sign === 'positive' ? '#x7ff0000000000000' : '#xfff0000000000000');
        return `(define-symbol-macro ${format}-float-${sign}-infinity (bits-${single ? 'single' : 'double'}-float ${bits}))`;
      })).join('\n');
  return Buffer.from(source);
}
let namedReadtablesPort = '';
export function setNamedReadtablesPort(source) {namedReadtablesPort = source;}
let floatFeaturesPort = '';
export function setFloatFeaturesPort(source) {floatFeaturesPort = source;}
export const closerMopPort = `(assert (eq (find-package :closer-mop) (find-package :miricl-mop)))`;
