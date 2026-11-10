// Cold self-compilation exercises the interpreter before native kernels exist.
// node scripts/benchmark-compiler-bootstrap.mjs OUTPUT IMAGE [IMAGE ...] [--profile]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import inspector from 'node:inspector';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';

const args = process.argv.slice(2), profileEnabled = args.includes('--profile');
const [output, ...paths] = args.filter(arg => arg !== '--profile');
if (!output || !paths.length) throw new Error('Expected OUTPUT IMAGE [IMAGE ...] [--profile]');
await fs.mkdir(output, {recursive: true});
const variants = await Promise.all(paths.map(async file => ({file, image: await fs.readFile(file), samples: []})));
const session = new inspector.Session();
const post = (method, params = {}) => new Promise((resolve, reject) =>
  session.post(method, params, (error, value) => error ? reject(error) : resolve(value)));
if (profileEnabled) {session.connect(); await post('Profiler.enable');}
try {
  for (let round = 0; round < (profileEnabled ? 1 : 3); round++) {
    for (const variant of round % 2 ? variants.toReversed() : variants) {
      const started = performance.now(), vm = await Miricl.instantiate(variant.image);
      const instantiateMs = performance.now() - started;
      try {
        assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, ':COLD\n');
        if (profileEnabled) await post('Profiler.start');
        const cpu = process.cpuUsage(), before = performance.now();
        assert.equal(vm.evaluate('(miricl-compiler::ensure-compiler)').output, 'T\n');
        const bootstrapMs = performance.now() - before, elapsed = process.cpuUsage(cpu);
        if (profileEnabled) {
          const {profile} = await post('Profiler.stop');
          await fs.writeFile(path.join(output, `${variants.indexOf(variant)}.cpuprofile`), JSON.stringify(profile));
          const frames = new Map(), nodes = new Map(profile.nodes.map(node => [node.id, node]));
          profile.samples.forEach((id, index) => {
            const frame = nodes.get(id).callFrame, key = JSON.stringify(frame);
            const item = frames.get(key) ?? {...frame, milliseconds: 0};
            item.milliseconds += profile.timeDeltas[index] / 1000; frames.set(key, item);
          });
          variant.hotFunctions = [...frames.values()].sort((a, b) => b.milliseconds - a.milliseconds).slice(0, 40);
        }
        assert.equal(vm.evaluate(`(and (eq miricl-compiler::*compiler-state* :ready)
          (every (lambda (name) (compiled-function-p (symbol-function name)))
            miricl-compiler::*native-compiler-seeds*)
          (= 42 (funcall (miricl-compiler:compile-native '(lambda (x) (+ x 2))) 40)))`).output, 'T\n');
        vm.evaluate(`(multiple-value-bind (bytes literals) (miricl-compiler:emit-native-module
            (miricl-compiler:compile-to-hir '(lambda (x) (if x (+ x 2) 0))))
          (declare (ignore literals))
          (with-open-file (out "/result.wasm" :direction :output :element-type '(unsigned-byte 8))
            (write-sequence bytes out))) (values)`);
        const sha256 = createHash('sha256').update(await vm.readFile('/result.wasm')).digest('hex');
        if (variants[0].resultSha256) assert.equal(sha256, variants[0].resultSha256, 'Compiled output differs');
        variant.resultSha256 = sha256;
        variant.samples.push({round, instantiateMs, bootstrapMs, cpuMs: (elapsed.user + elapsed.system) / 1000});
        console.log(`${variant.file}: cold compiler bootstrap ${Math.round(bootstrapMs)} ms`);
      } finally {vm.close();}
    }
  }
} finally {if (profileEnabled) session.disconnect();}
const median = values => values.toSorted((a, b) => a - b)[Math.floor(values.length / 2)];
for (const variant of variants) {
  variant.imageBytes = variant.image.length; delete variant.image;
  variant.medianBootstrapMs = median(variant.samples.map(sample => sample.bootstrapMs));
}
await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(variants, null, 2) + '\n');
