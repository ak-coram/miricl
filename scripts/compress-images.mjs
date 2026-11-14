// Package local build images for static hosting without Content-Encoding.
import fs from 'node:fs/promises';
import {gzipSync} from 'node:zlib';
if (!process.argv[2]) throw new Error('Usage: node compress-images.mjs IMAGE ...');
for (const file of process.argv.slice(2))
  await fs.writeFile(`${file}.gz`, gzipSync(await fs.readFile(file), {level: 9}));
