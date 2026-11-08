// Build-time only: compile the compiler with itself and translate the small
// instruction vocabulary emitted by our backend into the runtime's WAT.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';

const [bootstrap, output] = process.argv.slice(2);
if (!bootstrap || !output) throw new Error('Usage: node prepare-compiler-image.mjs BOOTSTRAP OUTPUT-PREFIX');
const vm = await Miricl.instantiate(await fs.readFile(bootstrap));
try {
  vm.evaluate(`(in-package :miricl-compiler)
    (defparameter *image-sources* (mapcar (lambda (name)
      (list name (function-lambda-expression (symbol-function name)))) *native-compiler-seeds*))
    (multiple-value-bind (installed unsupported) (compile-compiler)
      (unless (and (= (length installed) (length *image-sources*)) (null unsupported))
        (error "Compiler bootstrap failed: ~S" unsupported)))
    (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
        '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
      (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
      (let ((names nil) (codes nil) (roots nil)
            (dump (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))))
        ;; This second compilation uses the already self-compiled frontend and emitter.
        (dolist (entry *image-sources*)
          (multiple-value-bind (code literals) (emit-native-module (compile-to-hir (second entry) :name (first entry)))
            (push (list (and (symbol-package (first entry)) (package-name (symbol-package (first entry))))
                        (symbol-name (first entry))) names)
            (push (coerce code 'list) codes)
            (push (funcall dump (list (first entry) literals)) roots)))
        (with-open-file (out "/compiler-image.lisp" :direction :output :if-exists :supersede)
          (let ((*print-readably* t) (*print-pretty* nil) (*print-circle* t) (*print-base* 10) (*print-radix* nil))
            (write (list (nreverse names) (nreverse codes) (nreverse roots)
              (coerce (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")) 'list)) :stream out)))))
    (values)`, {eof: true});
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
  const declarations = [], entries = [];
  const wat = codes.map((code, index) => nativeModuleToWat(Uint8Array.from(code), index, declarations, entries)).join('\n');
  await fs.writeFile(`${output}.wat`, `${wat}\n(elem declare func ${declarations.join(' ')})\n(table $compiler-entries ${entries.length} ${entries.length} funcref)\n(elem (table $compiler-entries) (i32.const 0) func ${entries.join(' ')})\n`);
  await fs.writeFile(`${output}.lisp`, image);
  console.log(`Self-compiled compiler: ${codes.length} functions, ${codes.reduce((n, code) => n + code.length, 0)} module bytes before linking.`);
} finally {vm.close();}

function nativeModuleToWat(bytes, moduleIndex, declarations, entries) {
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
    if (type === 0x6f) return 'externref';
    throw new Error(`Unexpected native local type ${type}`);
  };
  const blockType = () => {
    const type = byte();
    if (type === 0x40) return '';
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
            case 0x10: instructions.push(`call ${functionName(unsigned())}`); break;
            case 0x1a: instructions.push('drop'); break;
            case 0x20: instructions.push(`local.get ${unsigned()}`); break;
            case 0x21: instructions.push(`local.set ${unsigned()}`); break;
            case 0x41: instructions.push(`i32.const ${signed()}`); break;
            case 0x45: instructions.push('i32.eqz'); break;
            case 0x46: instructions.push('i32.eq'); break;
            case 0x71: instructions.push('i32.and'); break;
            case 0xd0: assert.equal(byte(), 0x6f); instructions.push('ref.null extern'); break;
            case 0xd1: instructions.push('ref.is_null'); break;
            case 0xd2: instructions.push(`ref.func ${functionName(unsigned())}`); break;
            case 0xfb: {
              const gc = unsigned();
              if (gc === 2) {assert.equal(unsigned(), 1); instructions.push(`struct.get $obj ${unsigned()}`);}
              else if (gc === 0x17) {assert.equal(unsigned(), 1); instructions.push('ref.cast (ref null $obj)');}
              else if (gc === 0x1a) instructions.push('any.convert_extern');
              else if (gc === 0x1b) instructions.push('extern.convert_any');
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
