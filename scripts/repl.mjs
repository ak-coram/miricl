#!/usr/bin/env node
import fs from 'node:fs/promises';
import readline from 'node:readline';
import {Miricl} from './miricl.mjs';

const interpreter = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'interpreter.wasm'));
const terminal = Boolean(process.stdin.isTTY && process.stdout.isTTY);
const input = readline.createInterface({input: process.stdin, output: process.stdout, terminal});
let pending = '';
let quit = false;
function prompt() {
  if (terminal) {
    input.setPrompt(pending ? '...> ' : 'miricl> ');
    input.prompt();
  }
}
function report(error) {
  if (error.output) process.stdout.write(error.output);
  process.stderr.write(`Error: ${error.message}\n`);
}
input.on('SIGINT', () => {
  if (pending) {pending = ''; process.stdout.write('\n'); prompt();}
  else input.close();
});
prompt();
for await (const line of input) {
  pending += `${line}\n`;
  try {
    const result = interpreter.evaluate(pending);
    if (result.output) process.stdout.write(result.output);
    if (!result.incomplete) pending = '';
    if (result.quit) {quit = true; input.close(); break;}
  } catch (error) {
    report(error);
    pending = '';
  }
  prompt();
}
if (pending && !quit) {
  try {process.stdout.write(interpreter.evaluate(pending, {eof: true}).output);}
  catch (error) {report(error); process.exitCode = 1;}
}
