// Apply explicit MIRICL host cases to guest copies of the pinned sources.
// The submodule remains unchanged, including when a probe fails.
import {iterativeInvariantRisk} from './invariant-risk.mjs';
import {iterativeFnnames, iterativeExecFnnames} from './fnnames.mjs';
import {iterativeAdjacentDuplicates} from './list-duplicates.mjs';
import {iterativeGuardHolders, iterativeGuardHolderLists} from './guard-holders.mjs';
import {iterativeLevelNumbers} from './level-numbers.mjs';
import {iterativeOneify} from './oneify.mjs';
import {createHash} from 'node:crypto';
export const acl2Adaptations = [
  ':UNIX for the POSIX-style guest filesystem',
  'SYSTEM-CALL+: unavailable external processes return status 1 and empty output',
  'WITHOUT-INTERRUPTS/WITH-INTERRUPTS: preserve and restore scheduler interrupt masks',
  'GC$: advisory request; collection is controlled automatically by the WasmGC host',
  'ALL-FNNAMES1-INVARIANT-RISK: use an explicit work list in raw Lisp; retain the recursive logical definition',
  'ALL-FNNAMES1: use an explicit work list in raw Lisp; retain the recursive logical definition',
  'ALL-FNNAMES1-EXEC: use an explicit work list for execution-only function names',
  'SHARP-ATSIGN-ALIST: build the printer index iteratively in raw Lisp',
  'REMOVE-ADJACENT-DUPLICATES/EQ: scan list runs iteratively in raw Lisp; retain the recursive logical definitions',
  'REMOVE-GUARD-HOLDERS1/LST/LST-LST: traverse terms and list spines with heap continuations in raw Lisp; retain the recursive logical definitions',
  'MAX-LEVEL-NO/LST and GET-LEVEL-NO: scan function levels with a work list in raw Lisp; retain the recursive logical definitions',
  'ONEIFY/LST/FLET-BINDINGS: traverse ordinary expressions and lists with heap continuations in raw Lisp',
  'Raw-code coverage: register the iterative variants in ACL2’s program- and logic-mode lists',
  'EV-FNCALL-W-GUARD1-CACHE-INIT: reuse its composite constant when compiling an already loaded source',
  'OUR-STANDARD-IO: preserve the stream constant when compiling an already loaded source',
  'INITIALIZE-STATE-GLOBALS: identify the host as :MIRICL',
  'GETENV$-RAW: explicit guest environment with the pinned ACL2 revision in ACL2_SNAPSHOT_INFO',
  'ACL2-CHARACTERS: transcode its Latin-1 fixture to the UTF-8 guest stream encoding, preserving all 256 characters',
  'COMPILE-ACL2: use the compile-on-load path because MIRICL compiles evaluations to native Wasm',
];

const acl2RawCode = {
  program: ['sharp-atsign-alist', 'remove-guard-holders1-lst-lst', 'max-level-no',
    'max-level-no-lst', 'get-level-no', 'remove-adjacent-duplicates-eq',
    'all-fnnames1-exec', 'all-fnnames1-invariant-risk'],
  logic: ['all-fnnames1', 'remove-guard-holders1', 'remove-guard-holders1-lst', 'remove-adjacent-duplicates'],
};

function replaceOnce(source, anchor, replacement, name) {
  if (source.split(anchor).length !== 2) throw new Error(`ACL2 ${name} port anchor changed`);
  return source.replace(anchor, replacement);
}

function replaceRawFunction(source, startText, endText, hash, replacement, name) {
  const start = source.indexOf(startText), end = source.indexOf(endText, start);
  if (start < 0 || end < 0) throw new Error(`ACL2 ${name} port anchor changed`);
  const original = source.slice(start, end);
  if (createHash('sha256').update(original).digest('hex') !== hash)
    throw new Error(`ACL2 ${name} source changed; review the iterative counterpart`);
  return replaceOnce(source, original,
    '#+(and miricl (not acl2-loop-only))\n' + replacement + '\n\n'
    + '#-(and miricl (not acl2-loop-only))\n' + original, name);
}

export function portAcl2Source(name, bytes, {revision} = {}) {
  if (name === 'acl2-characters') {
    if (bytes.length !== 256 || bytes.some((value, index) => value !== index))
      throw new Error('ACL2 character fixture changed');
    return Buffer.from(bytes.toString('latin1'), 'utf8');
  }
  let source = bytes.toString('utf8');
  if (name === 'acl2-init.lisp') {
    // The unknown-host body contains an empty OUR-IGNORE-ERRORS form. Exclude
    // that whole definition at read time and install the explicit host service.
    const anchor = '(defun system-call+ (string arguments)';
    source = replaceOnce(source, anchor,
      '#+miricl (load "miricl-host.lisp")\n#-miricl\n' + anchor, 'SYSTEM-CALL+');
  } else if (name === 'acl2-fns.lisp') {
    if (!/^[a-f0-9]{40}$/.test(revision ?? '')) throw new Error('Expected the pinned ACL2 revision');
    const anchor = '(defun getenv$-raw (string)';
    source = replaceOnce(source, anchor,
      `#+miricl\n(defparameter *miricl-environment*\n  '(("ACL2_SNAPSHOT_INFO" . "Pinned ACL2 source ${revision}")))\n\n${anchor}`,
      'MIRICL environment');
    const value = '(let* ((val\n          #+cmu';
    source = replaceOnce(source, value,
      '(let* ((val\n          #+miricl\n          (cdr (assoc string *miricl-environment* :test #\'equal))\n          #+cmu',
      'GETENV$-RAW');
  } else if (name === 'axioms.lisp') {
    for (const [mode, first] of [['program', 'relieve-hyp-synp'], ['logic', 'mod-expt']]) {
      const anchor = `  '(${first} `;
      source = replaceOnce(source, anchor, `  '(#+miricl ${acl2RawCode[mode].join('\n    #+miricl ')}\n    ${first} `,
        `${mode}-mode raw-code coverage`);
    }
    source = replaceRawFunction(source, '(defun sharp-atsign-alist (i acc)',
      '\n; Essay on the Implementation of Time$',
      '770e8c31deab449856ae0a1411b1773acd3945dbc405c5820ff49ed657e89a51',
      `(defun sharp-atsign-alist (i acc)
  (loop until (zp i) do
    (setq acc (acons i (make-sharp-atsign i) acc) i (1- i))
    finally (return acc)))`, 'printer index traversal');
    source = replaceOnce(source,
      '(defconstant *ev-fncall-w-guard1-cache-init*\n  (cons 0 (make-list *ev-fncall-w-guard1-cache-size*)))',
      `(defconstant *ev-fncall-w-guard1-cache-init*
  #+miricl
  (if (boundp '*ev-fncall-w-guard1-cache-init*)
      (symbol-value '*ev-fncall-w-guard1-cache-init*)
    (cons 0 (make-list *ev-fncall-w-guard1-cache-size*)))
  #-miricl
  (cons 0 (make-list *ev-fncall-w-guard1-cache-size*)))`, 'cache constant identity');
    source = replaceOnce(source, '(defun-one-output gc$-fn (args)\n',
      '(defun-one-output gc$-fn (args)\n  #+miricl (declare (ignore args))\n', 'GC$ arguments');
    source = replaceOnce(source,
      '#-(or allegro gcl clisp cmu sbcl ccl lispworks)\n  (illegal \'gc$',
      '#-(or allegro gcl clisp cmu sbcl ccl lispworks miricl)\n  (illegal \'gc$', 'automatic WasmGC');
  } else if (name === 'translate.lisp') {
    source = replaceRawFunction(source, '(mutual-recursion\n\n(defun remove-guard-holders1 (',
      '\n(defun remove-guard-holders-weak (',
      '801e5b9c07f98a777cb93e66471e0fc2228807ce5ca36314936830b7e501d27a',
      iterativeGuardHolders, 'guard-holder term traversal');
    source = replaceRawFunction(source, '(defun remove-guard-holders1-lst-lst (',
      '\n(defun remove-guard-holders-weak-lst-lst (',
      'afec8e3ca2c8c14773d0e642ad13e1f78a106e3e9e9aea4b439f001ac34d6f52',
      iterativeGuardHolderLists, 'guard-holder clause traversal');
    const start = source.indexOf('(defun all-fnnames1 (flg x acc)');
    const end = source.indexOf('\n(defmacro all-fnnames (', start);
    if (start < 0 || end < 0) throw new Error('ACL2 function-name traversal port anchor changed');
    const original = source.slice(start, end);
    if (createHash('sha256').update(original).digest('hex')
        !== '0df1c49bed5402c729d95c21d86d0968b6a221588ef2998d49762fa1c20300c7')
      throw new Error('ACL2 function-name traversal source changed; review the iterative counterpart');
    source = replaceOnce(source, original,
      '#+(and miricl (not acl2-loop-only))\n' + iterativeFnnames + '\n\n'
      + '#-(and miricl (not acl2-loop-only))\n' + original, 'function-name traversal');
  } else if (name === 'defuns.lisp') {
    source = replaceRawFunction(source, '(defun all-fnnames1-exec (flg x acc)',
      '\n(defmacro all-fnnames-exec (',
      'fb61eaa516b72eaa0189e2f04a6add55c5e7755bf35069aac998768b17fb863d',
      iterativeExecFnnames, 'execution function-name traversal');
    const start = source.indexOf('(defun all-fnnames1-invariant-risk (flg x i wrld acc)');
    const end = source.indexOf('\n(defun put-invariant-risk (', start);
    if (start < 0 || end < 0) throw new Error('ACL2 invariant-risk traversal port anchor changed');
    const original = source.slice(start, end);
    // A whole-function replacement must be reviewed when upstream changes its
    // behavior, even if the surrounding DEFUN names have stayed the same.
    if (createHash('sha256').update(original).digest('hex')
        !== '01937e8ceeb1d146ece6323ef4734f8191bdb72097ba75287b465e172685315e')
      throw new Error('ACL2 invariant-risk traversal source changed; review the iterative counterpart');
    source = replaceOnce(source, original,
      '#+(and miricl (not acl2-loop-only))\n' + iterativeInvariantRisk + '\n\n'
      + '#-(and miricl (not acl2-loop-only))\n' + original, 'invariant-risk traversal');
  } else if (name === 'simplify.lisp') {
    source = replaceRawFunction(source, '(mutual-recursion\n\n(defun max-level-no (',
      '\n(mutual-recursion\n\n(defun sort-fcds1-rating1',
      'd85e2c73a1cba74da43876acae401a03cc2f0a8d98ad5417cc4165753972a507',
      iterativeLevelNumbers, 'function-level traversal');
  } else if (name === 'induct.lisp') {
    source = replaceRawFunction(source, '(defun remove-adjacent-duplicates (x)',
      '\n(defun remove-adjacent-duplicates-eq (x)',
      '9a3f988d95ff50e6dab325057495363f71ad86ad8dbfae2b91a225ad2dbcfed6',
      iterativeAdjacentDuplicates('remove-adjacent-duplicates', 'equal'), 'adjacent duplicate traversal');
    source = replaceRawFunction(source, '(defun remove-adjacent-duplicates-eq (x)',
      '\n#+non-standard-analysis\n(defun non-standard-induction-vars',
      'ccacc80b010fe61a0394d869328076fba74609fa24cbbd5e1e54b231d0936138',
      iterativeAdjacentDuplicates('remove-adjacent-duplicates-eq', 'eq'), 'adjacent EQ duplicate traversal');
  } else if (name === 'interface-raw.lisp') {
    const start = '(mutual-recursion\n\n(defun-one-output oneify-flet-bindings (';
    const end = '\n(defun-one-output select-stobj';
    const original = source.slice(source.indexOf(start), source.indexOf(end, source.indexOf(start)));
    source = replaceRawFunction(source, start, end,
      '2044819c5e3986051c3638c9deff0ca6f5ddabcc54fe71a918ed2e9c40ae4eec',
      iterativeOneify(original), 'executable counterpart traversal');
    source = replaceOnce(source,
      '  (make-two-way-stream *standard-input* *standard-output*))\n\n(defun ld-acl2-customization',
      `  #+miricl
  (if (boundp '*our-standard-io*) (symbol-value '*our-standard-io*)
    (make-two-way-stream *standard-input* *standard-output*))
  #-miricl
  (make-two-way-stream *standard-input* *standard-output*))

(defun ld-acl2-customization`, 'standard I/O constant identity');
  } else if (name === 'acl2.lisp') {
    const anchor = '#+lispworks :lispworks\n           #-(or gcl ccl sbcl allegro clisp cmu lispworks)';
    source = replaceOnce(source, anchor,
      '#+lispworks :lispworks\n           #+miricl :miricl\n           #-(or gcl ccl sbcl allegro clisp cmu lispworks miricl)',
      'INITIALIZE-STATE-GLOBALS');
    const compilation = '#+(or sbcl ccl) ; these compile on-the-fly\n  t\n  #-(or sbcl ccl)\n  nil)';
    source = replaceOnce(source, compilation,
      '#+(or sbcl ccl miricl) ; these compile on-the-fly\n  t\n  #-(or sbcl ccl miricl)\n  nil)',
      'compile-on-load');
  } else return bytes;
  return source;
}
