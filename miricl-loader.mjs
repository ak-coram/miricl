// Generic host launcher. The image carries its matching, self-contained adapter.
const coreHeader = [0,97,115,109,1,0,0,0];
const componentHeader = [0,97,115,109,13,0,1,0];
const headerIs = (bytes, header) => bytes.length >= 8 && header.every((value, i) => bytes[i] === value);

async function imageBytes(source) {
  if (ArrayBuffer.isView(source)) return new Uint8Array(source.buffer, source.byteOffset, source.byteLength).slice();
  if (source instanceof ArrayBuffer) return new Uint8Array(source.slice(0));
  if (source instanceof Blob || source instanceof Response) return new Uint8Array(await source.arrayBuffer());
  if (typeof source !== 'string' && !(source instanceof URL))
    throw new TypeError('Expected image bytes, a Blob, a Response, a pathname or a URL.');
  const name = String(source);
  if (globalThis.process?.versions?.node && !/^(?:https?|data|blob):/u.test(name)) {
    const {readFile} = await import('node:fs/promises');
    return new Uint8Array(await readFile(name.startsWith('file:') ? new URL(name) : name));
  }
  const response = await fetch(source);
  if (!response.ok) throw new Error(`Cannot load image: ${response.status} ${response.statusText}`);
  return new Uint8Array(await response.arrayBuffer());
}

async function gunzip(bytes) {
  return new Uint8Array(await new Response(new Blob([bytes]).stream()
    .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer());
}

export function extractComponentCore(bytes, section, source = 'Wasm component') {
  if (!headerIs(bytes, componentHeader)) throw new Error(`Expected a Wasm component in ${source}.`);
  let position = 8, core;
  const decoder = new TextDecoder('utf-8', {fatal: true, ignoreBOM: true});
  const word = limit => {
    let value = 0;
    for (let shift = 0; shift < 35; shift += 7) {
      if (position >= limit) throw new Error('Truncated image section length.');
      const byte = bytes[position++];
      if (shift === 28 && byte > 15) throw new Error('Image section length exceeds u32.');
      value += (byte & 127) * 2 ** shift;
      if (byte < 128) return value;
    }
    throw new Error('Invalid image section length.');
  };
  while (position < bytes.length) {
    const kind = bytes[position++], length = word(bytes.length), end = position + length;
    if (end > bytes.length) throw new Error('Image section exceeds its container.');
    if (kind === 0) {
      const size = word(end);
      if (size > end - position) throw new Error('Truncated image custom section name.');
      const name = decoder.decode(bytes.subarray(position, position + size));
      position += size;
      if (name === section) {
        if (core) throw new Error(`Duplicate ${section} sections in ${source}.`);
        core = bytes.subarray(position, end);
      }
    }
    position = end;
  }
  if (!core) throw new Error(`Missing ${section} section in ${source}.`);
  if (!headerIs(core, coreHeader)) throw new Error(`Expected a core Wasm image in the ${section} section of ${source}.`);
  return core;
}

export async function getMiriclAdapter(source) {
  let bytes = await imageBytes(source);
  if (bytes[0] === 0x1f && bytes[1] === 0x8b) bytes = await gunzip(bytes);
  if (headerIs(bytes, componentHeader)) bytes = extractComponentCore(bytes, 'miricl.node-compiler');
  if (!headerIs(bytes, coreHeader)) throw new Error('Expected a core Wasm image.');
  const module = await WebAssembly.compile(bytes);
  const sections = WebAssembly.Module.customSections(module, 'miricl.adapter.gz');
  if (sections.length !== 1) throw new Error('Expected one embedded MIRICL JavaScript adapter.');
  const sourceBytes = await gunzip(new Uint8Array(sections[0]));
  const adapter = new TextDecoder('utf-8', {fatal: true}).decode(sourceBytes);
  const exports = await import('data:text/javascript,' + encodeURIComponent(adapter));
  if (typeof exports.Miricl?.instantiate !== 'function') throw new Error('Embedded adapter does not export Miricl.');
  return {image: bytes, adapter: exports};
}

export async function loadMiricl(source, options) {
  const {image, adapter} = await getMiriclAdapter(source);
  // Pass the actual bytes. An unrelated caller-supplied compiled Module must
  // never stand in for the executable whose metadata the adapter validates.
  const interpreter = await adapter.Miricl.instantiate(image, options);
  Object.defineProperty(interpreter, 'adapter', {value: adapter});
  return interpreter;
}
