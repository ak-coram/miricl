// Compact guest filesystem: UTF-8 paths and raw bytes, with no base64/JSON copy.
const magic = new TextEncoder().encode('MIRICLFS');
export function encodeFilesystem(files) {
  const encoder = new TextEncoder();
  const entries = files.map(([name, data]) => [encoder.encode(name), new Uint8Array(data)]);
  const bytes = new Uint8Array(12 + entries.reduce((size, [name, data]) => size + 8 + name.length + data.length, 0));
  bytes.set(magic);
  const view = new DataView(bytes.buffer); view.setUint32(8, entries.length, true);
  let offset = 12;
  for (const [name, data] of entries) {
    view.setUint32(offset, name.length, true); view.setUint32(offset + 4, data.length, true); offset += 8;
    bytes.set(name, offset); offset += name.length; bytes.set(data, offset); offset += data.length;
  }
  return bytes;
}
export function decodeFilesystem(source) {
  const bytes = source instanceof Uint8Array ? source : new Uint8Array(source);
  if (bytes.length < 12 || magic.some((byte, i) => bytes[i] !== byte)) throw new Error('Invalid guest filesystem archive.');
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const count = view.getUint32(8, true), decoder = new TextDecoder('utf-8', {fatal: true}), files = [], names = new Set();
  let offset = 12;
  for (let i = 0; i < count; i++) {
    if (offset + 8 > bytes.length) throw new Error('Truncated guest filesystem archive.');
    const nameLength = view.getUint32(offset, true), dataLength = view.getUint32(offset + 4, true); offset += 8;
    if (offset + nameLength + dataLength > bytes.length) throw new Error('Truncated guest filesystem entry.');
    const name = decoder.decode(bytes.subarray(offset, offset + nameLength)); offset += nameLength;
    if (!name.startsWith('/') || names.has(name)) throw new Error('Invalid or duplicate guest filesystem path.');
    names.add(name); files.push([name, bytes.subarray(offset, offset + dataLength)]); offset += dataLength;
  }
  if (offset !== bytes.length) throw new Error('Unexpected guest filesystem data.');
  return files;
}
