// Build-time only: compile the compiler with itself and translate the small
// instruction vocabulary emitted by our backend into the runtime's WAT.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
import {collectCompilationStatistics} from './compilation-statistics.mjs';

const [bootstrap, output] = process.argv.slice(2);
if (!bootstrap || !output) throw new Error('Usage: node prepare-compiler-image.mjs BOOTSTRAP OUTPUT-PREFIX');
const vm = await Miricl.instantiate(await fs.readFile(bootstrap));
try {
  const bootstrapStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'bootstrap native functions'}, () => vm.evaluate(`(in-package :miricl-compiler)
    (setq *compiler-bootstrapping* t)
    (defparameter *image-sources* (mapcar (lambda (name)
      (list name (compiler-source-policy (function-lambda-expression (symbol-function name))))) *native-compiler-seeds*))
    (multiple-value-bind (installed unsupported) (compile-compiler)
      (unless (and (= (length installed) (length *image-sources*)) (null unsupported))
        (error "Compiler bootstrap failed: ~S" unsupported)))
    (values)`, {eof: true}));
  const interpretedBootstrapMs = performance.now() - bootstrapStarted;
  const moduleStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'sealed compiler module'}, () => vm.evaluate(`
    (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
        '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
      (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
      (let ((names nil) (codes nil) (roots nil)
            (dump (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))))
        ;; This second compilation uses the already self-compiled frontend and emitter.
        (dolist (entry *image-sources*)
          (push (list (and (symbol-package (first entry)) (package-name (symbol-package (first entry))))
                      (symbol-name (first entry))) names))
        (let ((unit (compile-module-to-hir
                      (mapcar (lambda (entry) (list* 'defun (first entry) (cdr (second entry)))) *image-sources*)
                      :inline nil)))
          (multiple-value-bind (code literals) (emit-native-module unit)
            ;; One factory shares the module's literals and closes over its
            ;; internal function bindings. Preserve only its literal graph;
            ;; runtime startup creates and installs the native entry points.
            (let ((entries (funcall (compiler-primitive 17 code literals nil))))
              ;; Validate and instantiate the output without installing another
              ;; compiler generation. Keep compiling the fixed original corpus.
              (unless (every (lambda (entry) (compiled-function-p (cdr entry))) entries)
                (error "Module compiler output contains an uncompiled entry")))
            (unless (= 42 (funcall (compile-native '(lambda (x) (+ x 2))) 40))
              (error "Module-compiled compiler failed its compilation check"))
            (push (coerce code 'list) codes)
            (push (funcall dump (list :module literals)) roots)))
        (with-open-file (out "/compiler-image.lisp" :direction :output :if-exists :supersede)
          (let ((*print-readably* t) (*print-pretty* nil) (*print-circle* t) (*print-base* 10) (*print-radix* nil))
            (write (list (nreverse names) (nreverse codes) (nreverse roots)
              (coerce (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")) 'list)) :stream out)))))
    (values)`, {eof: true}));
  const nativeModuleAndManifestMs = performance.now() - moduleStarted;
  console.log(`Compiler phase times: ${JSON.stringify({interpretedBootstrapMs, nativeModuleAndManifestMs,
    statisticsEnabled: Boolean(process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY)})}`);
  const image = await vm.readFile('/compiler-image.lisp');
  // A JSON copy of the code vectors avoids implementing a Lisp reader in JS.
  const codeOutput = vm.evaluate(`(progn
    (with-open-file (in "/compiler-image.lisp")
      (let ((codes (second (read in))))
        (write-string "[")
        (loop for code in codes for first = t then nil do
          (unless first (write-string ",")) (write-string "[")
          (loop for byte in code for first = t then nil do
            (unless first (write-string ",")) (princ byte)) (write-string "]"))
        (write-string "]"))) (values))`, {eof: true}).output;
  const codes = JSON.parse(codeOutput);
  const declarations = [], entries = [], callCounts = new Map();
  const wat = codes.map((code, index) => nativeModuleToWat(Uint8Array.from(code), index, declarations, entries, callCounts)).join('\n');
  const functions = [...new Set([...entries, ...declarations])];
  await fs.writeFile(`${output}.wat`, `${wat}\n(table $compiler-entries (export "core-compiler-functions") ${functions.length} ${functions.length} funcref)\n(elem (table $compiler-entries) (i32.const 0) func ${functions.join(' ')})\n`);
  await fs.writeFile(`${output}.lisp`, image);
  console.log(`Self-compiled compiler: ${codes.length} module, ${declarations.length} bodies, ${codes.reduce((n, code) => n + code.length, 0)} module bytes before linking.`);
  console.log(`Compiler typed call sites: ${JSON.stringify(Object.fromEntries([...callCounts]
    .filter(([name]) => /^(?:native-vector-(?:ref|set)|native-car-list|native-cdr-list|native-(?:add|sub|mul)-int64)/u.test(name))))}`);
} finally {vm.close();}

function nativeModuleToWat(bytes, moduleIndex, declarations, entries, callCounts) {
  assert.ok(WebAssembly.validate(bytes), `Invalid compiler module ${moduleIndex}`);
  let position = 8;
  const byte = () => {assert.ok(position < bytes.length, 'Truncated native module'); return bytes[position++];};
  const unsigned = () => {
    let value = 0, shift = 0, next;
    do {next = byte(); value += (next & 127) * 2 ** shift; shift += 7; assert.ok(shift <= 35);} while (next & 128);
    return value;
  };
  const signed = () => {
    let value = 0, shift = 0, next;
    do {next = byte(); value |= (next & 127) << shift; shift += 7; assert.ok(shift <= 35);} while (next & 128);
    return shift < 32 && (next & 64) ? value | (-1 << shift) : value;
  };
  const name = () => {const size = unsigned(); const value = new TextDecoder().decode(bytes.subarray(position, position + size)); position += size; return value;};
  const imports = [], bodies = [];
  const functionName = index => index < imports.length ? `$${imports[index]}` : `$compiler-${moduleIndex}-${index - imports.length}`;
  const valueType = () => {
    const type = byte();
    if (type === 0x7f) return 'i32';
    if (type === 0x7e) return 'i64';
    if (type === 0x6f) return 'externref';
    throw new Error(`Unexpected native local type ${type}`);
  };
  const blockType = () => {
    const type = byte();
    if (type === 0x40) return '';
    if (type === 0x7f) return ' (result i32)';
    if (type === 0x6f) return ' (result externref)';
    throw new Error(`Unexpected native block type ${type}`);
  };
  while (position < bytes.length) {
    const kind = byte(), size = unsigned(), end = position + size;
    if (kind === 2) {
      const count = unsigned();
      for (let i = 0; i < count; i++) {
        assert.equal(name(), 'miricl.runtime');
        const field = name(); assert.match(field, /^native-[a-z0-9-]+$/);
        assert.equal(byte(), 0); unsigned(); imports.push(field);
      }
    } else if (kind === 7) {
      assert.equal(unsigned(), 1); assert.equal(name(), 'entry'); assert.equal(byte(), 0);
      entries.push(functionName(unsigned()));
    } else if (kind === 10) {
      const count = unsigned();
      for (let i = 0; i < count; i++) {
        const size = unsigned(), bodyEnd = position + size, locals = [], instructions = [];
        const localGroups = unsigned();
        for (let group = 0; group < localGroups; group++) {
          const count = unsigned(), type = valueType();
          if (count) locals.push(`(local ${Array(count).fill(type).join(' ')})`);
        }
        while (position < bodyEnd) {
          const op = byte();
          switch (op) {
            case 0x00: instructions.push('unreachable'); break;
            case 0x02: instructions.push(`block${blockType()}`); break;
            case 0x03: instructions.push(`loop${blockType()}`); break;
            case 0x04: instructions.push(`if${blockType()}`); break;
            case 0x05: instructions.push('else'); break;
            case 0x0b: if (position < bodyEnd) instructions.push('end'); break;
            case 0x0c: instructions.push(`br ${unsigned()}`); break;
            case 0x0d: instructions.push(`br_if ${unsigned()}`); break;
            case 0x0e: {
              const count = unsigned(), targets = [];
              for (let j = 0; j <= count; j++) targets.push(unsigned());
              instructions.push(`br_table ${targets.join(' ')}`); break;
            }
            case 0x0f: instructions.push('return'); break;
            case 0x10: {
              const index = unsigned();
              if (index < imports.length) callCounts.set(imports[index], (callCounts.get(imports[index]) ?? 0) + 1);
              instructions.push(`call ${functionName(index)}`); break;
            }
            case 0x12: instructions.push(`return_call ${functionName(unsigned())}`); break;
            case 0x1a: instructions.push('drop'); break;
            case 0xd3: instructions.push('ref.eq'); break;
            case 0x20: instructions.push(`local.get ${unsigned()}`); break;
            case 0x21: instructions.push(`local.set ${unsigned()}`); break;
            case 0x22: instructions.push(`local.tee ${unsigned()}`); break;
            case 0x41: instructions.push(`i32.const ${signed()}`); break;
            case 0x42: instructions.push(`i64.const ${signed()}`); break;
            case 0x45: instructions.push('i32.eqz'); break;
            case 0x46: instructions.push('i32.eq'); break;
            case 0x48: instructions.push('i32.lt_s'); break;
            case 0x4a: instructions.push('i32.gt_s'); break;
            case 0x4c: instructions.push('i32.le_s'); break;
            case 0x4e: instructions.push('i32.ge_s'); break;
            case 0x50: instructions.push('i64.eqz'); break;
            case 0x51: instructions.push('i64.eq'); break;
            case 0x52: instructions.push('i64.ne'); break;
            case 0x53: instructions.push('i64.lt_s'); break;
            case 0x55: instructions.push('i64.gt_s'); break;
            case 0x57: instructions.push('i64.le_s'); break;
            case 0x59: instructions.push('i64.ge_s'); break;
            case 0x71: instructions.push('i32.and'); break;
            case 0x7c: instructions.push('i64.add'); break;
            case 0x7d: instructions.push('i64.sub'); break;
            case 0x7e: instructions.push('i64.mul'); break;
            case 0x80: instructions.push('i64.div_u'); break;
            case 0x82: instructions.push('i64.rem_u'); break;
            case 0x83: instructions.push('i64.and'); break;
            case 0x84: instructions.push('i64.or'); break;
            case 0x85: instructions.push('i64.xor'); break;
            case 0x86: instructions.push('i64.shl'); break;
            case 0x87: instructions.push('i64.shr_s'); break;
            case 0xa7: instructions.push('i32.wrap_i64'); break;
            case 0xac: instructions.push('i64.extend_i32_s'); break;
            case 0xd0: assert.equal(byte(), 0x6f); instructions.push('ref.null extern'); break;
            case 0xd1: instructions.push('ref.is_null'); break;
            case 0xd2: instructions.push(`ref.func ${functionName(unsigned())}`); break;
            case 0xfb: {
              const gc = unsigned();
              if (gc === 2 || gc === 5) {
                assert.equal(unsigned(), 1);
                instructions.push(`struct.${gc === 5 ? 'set' : 'get'} $obj ${unsigned()}`);
              }
              else if (gc === 0x17) {
                const type = unsigned();
                assert.ok(type === 1 || type === 0x6d, `Unsupported nullable native heap type ${type}`);
                instructions.push(`ref.cast (ref null ${type === 1 ? '$obj' : 'eq'})`);
              }
              else if (gc === 0x14) {assert.equal(unsigned(), 1); instructions.push('ref.test (ref $obj)');}
              else if (gc === 0x16) {
                const type = unsigned();
                assert.ok(type === 1 || type === 0x6c, `Unsupported native heap type ${type}`);
                instructions.push(`ref.cast (ref ${type === 1 ? '$obj' : 'i31'})`);
              }
              else if (gc === 0x1a) instructions.push('any.convert_extern');
              else if (gc === 0x1b) instructions.push('extern.convert_any');
              else if (gc === 0x1c) instructions.push('ref.i31');
              else if (gc === 0x1d) instructions.push('i31.get_s');
              else throw new Error(`Unsupported native GC instruction ${gc}`);
              break;
            }
            default: throw new Error(`Unsupported native instruction ${op.toString(16)}`);
          }
        }
        assert.equal(position, bodyEnd);
        const function_ = `$compiler-${moduleIndex}-${i}`;
        declarations.push(function_);
        bodies.push(`(func ${function_} (type $native-entry) (param externref externref) (result externref)\n${locals.join('\n')}\n${instructions.join('\n')}\n)`);
      }
    }
    position = end;
  }
  return bodies.join('\n');
}
