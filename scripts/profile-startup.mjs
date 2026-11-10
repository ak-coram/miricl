// Profile a real FASL/native bundle load without changing the runtime image.
// node scripts/profile-startup.mjs IMAGE BUNDLE OUTPUT-DIRECTORY [SAMPLE-MICROSECONDS]
import fs from 'node:fs/promises';
import path from 'node:path';
import inspector from 'node:inspector';
import {PerformanceObserver} from 'node:perf_hooks';
import {Miricl} from './miricl.mjs';

const [imagePath, bundlePath, output, interval = '10000'] = process.argv.slice(2);
if (!imagePath || !bundlePath || !output) throw new Error('Expected IMAGE BUNDLE OUTPUT-DIRECTORY');
const sampleMicroseconds = Number(interval);
if (!Number.isInteger(sampleMicroseconds) || sampleMicroseconds < 1000)
  throw new Error('Expected a sampling interval of at least 1000 microseconds');
await fs.mkdir(output, {recursive: true});
const [image, bundle] = await Promise.all([fs.readFile(imagePath), fs.readFile(bundlePath)]);
let at = 8;
const units = new Map();
const uleb = () => {let value = 0, shift = 0, byte;
  do {byte = bundle[at++]; value += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128);
  return value;};
while (at < bundle.length) {
  const kind = bundle[at++], size = uleb(), end = at + size;
  if (kind === 0) {
    const length = uleb(), name = bundle.subarray(at, at + length).toString(); at += length;
    if (name === 'miricl.fasl' && bundle.subarray(at, at + 4).toString() === 'MIRF')
      units.set(`${at}:${end}`, {index: units.size, bytes: size,
        records: bundle.readUInt32LE(at + 8)});
  }
  at = end;
}
const session = new inspector.Session(); session.connect();
const post = (method, params = {}) => new Promise((resolve, reject) =>
  session.post(method, params, (error, result) => error ? reject(error) : resolve(result)));
const report = {image: imagePath, bundle: bundlePath, imageBytes: image.length,
  bundleBytes: bundle.length, sampleMicroseconds, memoryIncludesProfiler: true,
  units: [], nativeModules: [], gcMilliseconds: 0};
let loadStart = Infinity, loadEnd = Infinity;
const observer = new PerformanceObserver(list => {
  for (const entry of list.getEntries())
    if (entry.startTime >= loadStart && entry.startTime <= loadEnd) report.gcMilliseconds += entry.duration;
});
observer.observe({entryTypes: ['gc']});
const active = [];
let vm, profile;
const start = performance.now();
try {
  vm = await Miricl.instantiate(image, {imports: {startupProfile: {event(kind, first, end) {
    if (kind === 0) active.push({start: performance.now(), first, end,
      ...units.get(`${first}:${end}`)});
    else {
      const entry = active.pop();
      report.units.push({...entry, milliseconds: performance.now() - entry.start});
    }
  }}}});
  report.instantiateMilliseconds = performance.now() - start;
  const mounted = performance.now(); vm.mountFile('/profile.wasm', bundle);
  report.mountMilliseconds = performance.now() - mounted;
  await vm.evaluateAsync(`
    (js:define-import startup-profile-event ("startupProfile" "event") (:i32 :i32 :i32) :void)
    (let* ((name (miricl-compiler::compiler-primitive 10 1 "%LOAD-COMPILED-UNIT"))
           (original (symbol-function name)))
      (setf (symbol-function name) (lambda (stream end print)
        (let ((first (file-position stream)))
          (startup-profile-event 0 first end)
          (unwind-protect (funcall original stream end print)
            (startup-profile-event 1 first end))))))
    (values)`, {eof: true});
  const Module = WebAssembly.Module, Instance = WebAssembly.Instance;
  let current;
  WebAssembly.Module = class extends Module {
    constructor(bytes, ...options) {
      const began = performance.now(); super(bytes, ...options);
      current = {bytes: bytes.byteLength, compileMilliseconds: performance.now() - began};
      report.nativeModules.push(current);
    }
  };
  WebAssembly.Instance = class extends Instance {
    constructor(module, imports) {
      const began = performance.now(); super(module, imports);
      if (current) current.instantiateMilliseconds = performance.now() - began;
    }
  };
  try {
    await post('Profiler.enable'); await post('Profiler.setSamplingInterval', {interval: sampleMicroseconds});
    await post('Profiler.start');
    const before = process.cpuUsage(), began = performance.now(); loadStart = began;
    await vm.evaluateAsync('(load "/profile.wasm") (values)', {eof: true});
    report.loadMilliseconds = performance.now() - began;
    loadEnd = performance.now();
    const cpu = process.cpuUsage(before);
    report.cpuMilliseconds = (cpu.user + cpu.system) / 1000;
    ({profile} = await post('Profiler.stop'));
  } finally {WebAssembly.Module = Module; WebAssembly.Instance = Instance;}
  const nodes = new Map(profile.nodes.map(node => [node.id, node]));
  const samples = new Map();
  profile.samples.forEach((id, i) => samples.set(id, (samples.get(id) ?? 0) + profile.timeDeltas[i]));
  const frames = new Map();
  for (const [id, microseconds] of samples) {
    const frame = nodes.get(id).callFrame;
    const key = JSON.stringify([frame.functionName, frame.url, frame.lineNumber, frame.columnNumber]);
    const entry = frames.get(key) ?? {...frame, milliseconds: 0};
    entry.milliseconds += microseconds / 1000; frames.set(key, entry);
  }
  report.hotFunctions = [...frames.values()].sort((a, b) => b.milliseconds - a.milliseconds).slice(0, 40);
  report.nativeModuleMilliseconds = report.nativeModules.reduce((n, entry) =>
    n + entry.compileMilliseconds + (entry.instantiateMilliseconds ?? 0), 0);
  report.peakRSSBytes = process.resourceUsage().maxRSS * 1024;
  report.memory = process.memoryUsage();
  await fs.writeFile(path.join(output, 'startup.cpuprofile'), JSON.stringify(profile));
  await new Promise(resolve => setImmediate(resolve));
  await fs.writeFile(path.join(output, 'startup.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(`Startup: ${Math.round(report.loadMilliseconds)} ms; ` +
    `${report.units.length} units; native module compilation/instantiation ${Math.round(report.nativeModuleMilliseconds)} ms; ` +
    `peak RSS ${Math.round(report.peakRSSBytes / 1048576)} MiB`);
} finally {observer.disconnect(); session.disconnect(); vm?.close();}
