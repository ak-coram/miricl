import {Miricl} from '../scripts/miricl.mjs';

// Backend suites explicitly compile their test bodies. Interpret their driver
// forms consistently with either image, independently of its default policy.
// Startup, evaluation-policy and cold-bootstrap suites use Miricl directly.
export async function createCompilerTestVM(bytes, options) {
  const vm = await Miricl.instantiate(bytes, options);
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)', {eof: true});
  return vm;
}
