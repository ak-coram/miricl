import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  check(`(progn
    (defparameter *codec-records*
      (coerce (loop for i below 600 collect (if (< i 300) (list 5 i (mod (1+ i) 600) (mod i 4))
        (list 5 i (mod (1+ i) 600) 0 "é𝄞" :keyword t nil -2147483647 2147483647 #()))) 'vector))
    (setq *codec-records* (concatenate 'vector *codec-records*
      '#((11 0 1 (-1 0 127 128 16383 16384 2147483646)) (11 0 0 nil))))
    (with-open-file (s "/native.wasm" :direction :output :element-type '(unsigned-byte 8))
      (dolist (b '(0 97 115 109 1 0 0 0)) (write-byte b s))
      (miricl-compiler::write-indexed-unit s nil *codec-records*
        :metadata '(:miricl-ir "source é𝄞" nil)))
    t)`);
  const bytes = await vm.readFile('/native.wasm');
  assert.ok(WebAssembly.validate(bytes));
  const payload = new Uint8Array(WebAssembly.Module.customSections(await WebAssembly.compile(bytes), 'miricl.fasl')[0]);
  assert.deepEqual([...payload.subarray(0, 4)], [77, 73, 82, 70]);
  const directory = new DataView(payload.buffer, payload.byteOffset, payload.byteLength);
  assert.equal(directory.getUint32(4, true), 7, 'Metadata, dense-record and byte-string feature bits');
  const entries = Array.from({length: directory.getUint32(12, true)}, (_, i) => {
    const at = 20 + 16 * i;
    return {at, kind: directory.getUint32(at, true), key: directory.getUint32(at + 4, true),
      offset: directory.getUint32(at + 8, true), size: directory.getUint32(at + 12, true)};
  });
  assert.equal(entries.filter(x => x.kind === 3).length, 3);
  const firstBlock = entries.find(x => x.kind === 3);
  assert.deepEqual([...payload.subarray(firstBlock.offset, firstBlock.offset + 8)], [10, 0, 128, 2, 6, 1, 0, 2],
    'Record blocks encode list runs, adaptive edges and packed instruction flags.');
  check(`(with-open-file (s "/native.wasm" :element-type '(unsigned-byte 8))
    (file-position s 26)
    (let ((payload (miricl-compiler::read-unit-payload s (file-length s) :metadata t)))
      (and (equalp (second payload) *codec-records*)
           (equal (third payload) '(:miricl-ir "source é𝄞" nil)))))`);
  check(`(with-open-file (s "/native.wasm" :element-type '(unsigned-byte 8))
    (file-position s 26)
    (multiple-value-bind (payload count)
        (miricl-compiler::read-unit-payload s (file-length s) :metadata t :records nil)
      (and (= count (length *codec-records*)) (null (second payload))
           (equal (third payload) '(:miricl-ir "source é𝄞" nil)))))`);
  // Exercise the portable host algorithm independently of the native codec.
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn
        (setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (s "/native.wasm" :element-type '(unsigned-byte 8))
          (file-position s 26)
          (unless (equalp (second (miricl-compiler::read-unit-payload s (file-length s) :metadata t))
                          *codec-records*) (error "Portable decoder mismatch.")))
        (with-open-file (s "/portable.wasm" :direction :output :element-type '(unsigned-byte 8))
          (dolist (b '(0 97 115 109 1 0 0 0)) (write-byte b s))
          (miricl-compiler::write-indexed-unit s nil *codec-records*
            :metadata '(:miricl-ir "source é𝄞" nil))) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/portable.wasm'), bytes, 'Native and portable encoders must agree byte for byte.');
  check(`(progn
    (defparameter *spines* (coerce (loop for i below 700 collect
      (list 5 i (mod (1+ i) 700) (if (zerop i) 2 0))) 'vector))
    (with-open-file (out "/spines.wasm" :direction :output :element-type '(unsigned-byte 8))
      (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
      (miricl-compiler::write-indexed-unit out nil *spines*))
    (with-open-file (in "/spines.wasm" :element-type '(unsigned-byte 8))
      (file-position in 26)
      (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *spines*)))`);
  const spines = await vm.readFile('/spines.wasm');
  assert.ok(spines.length < 1000, `700 records should use implicit CDRs across three independent blocks: ${spines.length}`);
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn
        (setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (in "/spines.wasm" :element-type '(unsigned-byte 8))
          (file-position in 26)
          (unless (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *spines*)
            (error "Portable list run mismatch.")))
        (with-open-file (out "/spines-portable.wasm" :direction :output :element-type '(unsigned-byte 8))
          (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
          (miricl-compiler::write-indexed-unit out nil *spines*)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/spines-portable.wasm'), spines);
  check(`(progn
    (defparameter *latin-prefix-values* (vector
      (concatenate 'string "HEAD" (make-string 512 :initial-element (code-char 200)))
      (concatenate 'string "HEAD" (make-string 512 :initial-element (code-char 201)))
      (concatenate 'string (make-string 500 :initial-element (code-char 200)) "A")
      (concatenate 'string (make-string 500 :initial-element (code-char 200)) "B")
      (concatenate 'string "abc" (string (code-char 200)))
      (concatenate 'string "abc" (string (code-char 201)))))
    (with-open-file (out "/latin-prefix.bin" :direction :output :element-type '(unsigned-byte 8))
      (miricl-compiler::write-binary-chunk *latin-prefix-values* out))
    (with-open-file (in "/latin-prefix.bin" :element-type '(unsigned-byte 8))
      (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *latin-prefix-values*)))`);
  const latinPrefix = await vm.readFile('/latin-prefix.bin');
  assert.ok(latinPrefix.length < 1600,
    `Short shared prefixes must not force large Latin-1 suffixes through UTF-8: ${latinPrefix.length}`);
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn (setf (symbol-function 'miricl-compiler::compiler-primitive)
               (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (in "/latin-prefix.bin" :element-type '(unsigned-byte 8))
          (unless (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *latin-prefix-values*)
            (error "Portable Latin-1 prefix mismatch.")))
        (with-open-file (out "/latin-prefix-portable.bin" :direction :output :element-type '(unsigned-byte 8))
          (miricl-compiler::write-binary-chunk *latin-prefix-values* out)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/latin-prefix-portable.bin'), latinPrefix);
  check(`(progn
    (defparameter *latin-boundary-values* (coerce (append
      (loop for i below 140 collect (format nil "dictionary-~D" i))
      (loop for n in '(63 64 65 127 128 129) append (list
        (concatenate 'string "encoding-" (make-string n :initial-element (code-char 200)))
        (concatenate 'string "encoding-" (make-string n :initial-element (code-char 201)))))
      (list (concatenate 'string "ABCD" (string (code-char 200)))
            (concatenate 'string "ABCD" (string (code-char 201))))) 'vector))
    (with-open-file (out "/latin-boundaries.bin" :direction :output :element-type '(unsigned-byte 8))
      (miricl-compiler::write-binary-chunk *latin-boundary-values* out))
    (with-open-file (in "/latin-boundaries.bin" :element-type '(unsigned-byte 8))
      (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *latin-boundary-values*)))`);
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn (setf (symbol-function 'miricl-compiler::compiler-primitive)
               (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (out "/latin-boundaries-portable.bin" :direction :output :element-type '(unsigned-byte 8))
          (miricl-compiler::write-binary-chunk *latin-boundary-values* out)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/latin-boundaries-portable.bin'), await vm.readFile('/latin-boundaries.bin'),
    'Encoding costs agree across dictionary, prefix and suffix ULEB boundaries.');
  check(`(progn
    (defparameter *zero-tail* '#((5 0 1 1) (5 0 0 0)))
    (defparameter *prefix-values* (vector
      (concatenate 'string "prefixλ𝄞" (string (code-char 0)) "first")
      (concatenate 'string "prefixλ𝄞" (string (code-char 0)) "second")
      "shared-symbol-prefix-alpha" "shared-symbol-prefix-beta" "shared-symbol-prefix-beta" ""
      (concatenate 'string (make-string 260 :initial-element (code-char 955)) "first")
      (concatenate 'string (make-string 260 :initial-element (code-char 955)) "second")))
    (with-open-file (out "/zero-tail.bin" :direction :output :element-type '(unsigned-byte 8))
      (miricl-compiler::write-binary-chunk *zero-tail* out 0))
    (with-open-file (out "/prefix.bin" :direction :output :element-type '(unsigned-byte 8))
      (miricl-compiler::write-binary-chunk *prefix-values* out))
    (with-open-file (in "/zero-tail.bin" :element-type '(unsigned-byte 8))
      (unless (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *zero-tail*) (error "Zero-tail mismatch.")))
    (with-open-file (in "/prefix.bin" :element-type '(unsigned-byte 8))
      (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *prefix-values*)))`);
  assert.deepEqual([...await vm.readFile('/zero-tail.bin')], [10, 0, 2, 26, 0, 0]);
  const prefix = await vm.readFile('/prefix.bin');
  const front = 4 + new TextEncoder().encode('prefixλ𝄞\0first').length;
  assert.equal(prefix[front], 9, 'Repeated Unicode prefixes use character counts and UTF-8 suffixes.');
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn (setf (symbol-function 'miricl-compiler::compiler-primitive)
               (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (in "/prefix.bin" :element-type '(unsigned-byte 8))
          (unless (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *prefix-values*)
            (error "Portable prefix mismatch.")))
        (with-open-file (out "/prefix-portable.bin" :direction :output :element-type '(unsigned-byte 8))
          (miricl-compiler::write-binary-chunk *prefix-values* out)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/prefix-portable.bin'), prefix);
  // Native code is stored as byte-valued strings. Cover every octet, NULs,
  // dictionary reuse and UTF-8 suffixes following a byte-encoded prefix.
  check(`(progn
    (defparameter *byte-string* (map 'string #'code-char
      (loop for i below 256 collect i)))
    (defparameter *byte-values* (vector *byte-string* (copy-seq *byte-string*)
      (concatenate 'string *byte-string* "suffix")
      (concatenate 'string *byte-string* "suffixλ𝄞")))
    (with-open-file (out "/byte-strings.bin" :direction :output :element-type '(unsigned-byte 8))
      (miricl-compiler::write-binary-chunk *byte-values* out))
    (with-open-file (in "/byte-strings.bin" :element-type '(unsigned-byte 8))
      (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *byte-values*)))`);
  const byteStrings = await vm.readFile('/byte-strings.bin');
  assert.deepEqual([...byteStrings.subarray(0, 5)], [8, 4, 11, 128, 2]);
  assert.deepEqual([...byteStrings.subarray(5, 261)], Array.from({length: 256}, (_, i) => i));
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn
        (setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (in "/byte-strings.bin" :element-type '(unsigned-byte 8))
          (unless (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) *byte-values*)
            (error "Portable byte string mismatch.")))
        (with-open-file (out "/byte-strings-portable.bin" :direction :output :element-type '(unsigned-byte 8))
          (miricl-compiler::write-binary-chunk *byte-values* out)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/byte-strings-portable.bin'), byteStrings);
  vm.mountFile('/truncated-byte-string.bin', Uint8Array.of(11, 2, 0));
  for (const portable of [false, true]) {
    check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
      (unwind-protect
        (progn ${portable ? `(setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (&rest args) (declare (ignore args)) (values nil nil)))` : ''}
          (handler-case
            (with-open-file (in "/truncated-byte-string.bin" :element-type '(unsigned-byte 8))
              (miricl-compiler::read-binary-chunk in 0 (file-length in)) nil)
            (error () t)))
        (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  }
  check(`(progn
    (defparameter *packets* (make-array 0 :adjustable t :fill-pointer 0))
    (loop for run from 1 to 16 do
      (dotimes (flags 3)
        (dotimes (mask (if (<= run 5) (ash 1 run) 1))
          (let ((start (length *packets*)))
            (dotimes (j run)
              (vector-push-extend (list 5 (if (logbitp j mask) (+ start j) 0)
                (if (= j (1- run)) 0 (+ start j 1)) (if (zerop j) flags 0)) *packets*))))))
    (with-open-file (out "/packets.wasm" :direction :output :element-type '(unsigned-byte 8))
      (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
      (miricl-compiler::write-indexed-unit out nil *packets*))
    (with-open-file (in "/packets.wasm" :element-type '(unsigned-byte 8))
      (file-position in 26)
      (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *packets*)))`);
  check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
    (unwind-protect
      (progn (setf (symbol-function 'miricl-compiler::compiler-primitive)
               (lambda (&rest args) (declare (ignore args)) (values nil nil)))
        (with-open-file (in "/packets.wasm" :element-type '(unsigned-byte 8))
          (file-position in 26)
          (unless (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *packets*)
            (error "Portable packet mismatch.")))
        (with-open-file (out "/packets-portable.wasm" :direction :output :element-type '(unsigned-byte 8))
          (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
          (miricl-compiler::write-indexed-unit out nil *packets*)) t)
      (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  assert.deepEqual(await vm.readFile('/packets-portable.wasm'), await vm.readFile('/packets.wasm'));
  // Symbol domains must reset at block boundaries and distinguish private
  // namespaces; packed references must also reconstruct backward CAR edges.
  check(`(progn
    (defparameter *domains* (coerce (loop for i below 600 collect
      (list 4 (format nil "SYMBOL-~D" i) (if (< (mod i 100) 50) "PACKAGE" nil)
        (floor i 75))) 'vector))
    (with-open-file (out "/domains.wasm" :direction :output :element-type '(unsigned-byte 8))
      (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
      (miricl-compiler::write-indexed-unit out nil *domains*))
    (with-open-file (in "/domains.wasm" :element-type '(unsigned-byte 8))
      (file-position in 26)
      (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *domains*)))`);
  for (const portable of [false, true]) {
    vm.mountFile('/bad-domain.bin', Uint8Array.of(10, 0, 1, 12, 3, 1, 65));
    check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive))
                 (backward '#((5 88 129 0) (5 89 130 0) (5 90 131 0) (5 91 132 0) (5 92 0 0))))
      (unwind-protect
        (progn ${portable ? `(setf (symbol-function 'miricl-compiler::compiler-primitive)
          (lambda (&rest args) (declare (ignore args)) (values nil nil)))` : ''}
          (with-open-file (in "/domains.wasm" :element-type '(unsigned-byte 8))
            (file-position in 26)
            (unless (equalp (second (miricl-compiler::read-unit-payload in (file-length in))) *domains*)
              (error "Symbol domain mismatch.")))
          (with-open-file (out "/domains-${portable}.wasm" :direction :output :element-type '(unsigned-byte 8))
            (dolist (byte '(0 97 115 109 1 0 0 0)) (write-byte byte out))
            (miricl-compiler::write-indexed-unit out nil *domains*))
          (with-open-file (out "/backward-${portable}.bin" :direction :output :element-type '(unsigned-byte 8))
            (miricl-compiler::write-binary-chunk backward out 128))
          (with-open-file (in "/backward-${portable}.bin" :element-type '(unsigned-byte 8))
            (unless (equalp (miricl-compiler::read-binary-chunk in 0 (file-length in)) backward)
              (error "Backward packet mismatch.")))
          (handler-case (with-open-file (in "/bad-domain.bin" :element-type '(unsigned-byte 8))
            (miricl-compiler::read-binary-chunk in 0 (file-length in)) nil) (error () t)))
        (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  }
  assert.deepEqual(await vm.readFile('/domains-true.wasm'), await vm.readFile('/domains.wasm'));
  assert.deepEqual(await vm.readFile('/backward-false.bin'), await vm.readFile('/backward-true.bin'));
  for (const offset of [1, 2, 3]) {
    const bad = prefix.slice(); bad[front + offset] = 127;
    vm.mountFile('/bad-prefix.bin', bad);
    for (const portable of [false, true])
      check(`(let ((native (symbol-function 'miricl-compiler::compiler-primitive)))
        (unwind-protect
          (progn ${portable ? `(setf (symbol-function 'miricl-compiler::compiler-primitive)
            (lambda (&rest args) (declare (ignore args)) (values nil nil)))` : ''}
            (handler-case (with-open-file (in "/bad-prefix.bin" :element-type '(unsigned-byte 8))
              (miricl-compiler::read-binary-chunk in 0 (file-length in)) nil) (error () t)))
          (setf (symbol-function 'miricl-compiler::compiler-primitive) native)))`);
  }
  const reject = async (name, mutate, metadata = false) => {
    const broken = bytes.slice();
    mutate(new DataView(broken.buffer, broken.byteOffset + 26), broken.subarray(26));
    vm.mountFile(`/${name}.wasm`, broken);
    check(`(handler-case
      (with-open-file (s "/${name}.wasm" :element-type '(unsigned-byte 8))
        (file-position s 26)
        (miricl-compiler::read-unit-payload s (file-length s) :metadata ${metadata ? 't' : 'nil'}) nil)
      (error () t))`);
  };
  await reject('large-count', data => data.setUint32(8, 0x7fffffff, true));
  await reject('overlap', data => data.setUint32(entries[1].at + 8, entries[0].offset, true));
  await reject('out-of-range', data => data.setUint32(entries[0].at + 8, 0xffffffff, true));
  await reject('duplicate-block', data => data.setUint32(entries.filter(x => x.kind === 3)[1].at + 4, 0, true));
  await reject('pool-reference', (data, content) => {content[entries[0].offset] = 4;});
  await reject('metadata', (data, content) => {content[entries.find(x => x.kind === 2).offset] = 255;}, true);
  await reject('empty-run', (data, content) => {content[firstBlock.offset + 5] = 0;});
  await reject('overlong-run', (data, content) => {
    content[firstBlock.offset + 5] = 255; content[firstBlock.offset + 6] = 2;
  });
  await reject('negative-edge', (data, content) => {content[firstBlock.offset + 6] = 3;});
  await reject('record-opcode', (data, content) => {content[firstBlock.offset + 4] = 255;});
  // Analysis chunks are not decoded by a normal LOAD.
  check('(load "/metadata.wasm")');

  vm.mountFile('/another.lisp', '(defparameter *another-loaded* 42)');
  check('(pathnamep (compile-file "/another.lisp" :verbose nil))');
  const another = await vm.readFile('/another.wasm');
  vm.mountFile('/bundle.wasm', Buffer.concat([bytes, another.subarray(8)]));
  check('(and (load "/bundle.wasm") (= *another-loaded* 42))');
  await reject('magic', (data, content) => {content[0] = 0;});
  console.log('Indexed FASL: native/portable codec agreement, independent blocks, Unicode, malformed directories, skipped metadata and multi-unit bundles pass.');
} finally {vm.close();}
