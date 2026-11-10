// Final bootstrap setup reads raw sources using ACL2's current package.
import assert from 'node:assert/strict';
import {loadAcl2TestCore} from './acl2-test-core.mjs';
const {vm} = await loadAcl2TestCore(process.argv[2] ?? 'build/acl2/runtime.wasm',
  process.argv[3] ?? 'build/acl2/acl2-native.wasm');
try {
  const result = await vm.evaluateAsync(`
    (in-package :acl2)
    (let ((ht (make-hash-table :test 'eq)))
      (note-fns-in-file "hons-raw.lisp" ht)
      (note-fns-in-file "memoize-raw.lisp" ht)
      (assert (gethash 'memoize-fn ht))
      (assert (gethash 'initialize-never-memoize-ht ht))
      (assert (null (gethash (intern "DEFG" "CL-USER") ht)))
      t)`, {eof: true});
  assert.ok(result.output.endsWith('T\n'));
  console.log('ACL2 final bootstrap source scan: raw hons/memoization sources and ACL2 symbol identities pass.');
} finally {vm.close();}
