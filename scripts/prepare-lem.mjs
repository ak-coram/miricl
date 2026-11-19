#!/usr/bin/env node
import {prepareLemSources} from '../lem/files.mjs';

const args = process.argv.slice(2);
if (args.some(arg => arg !== '--sources-only') || new Set(args).size !== args.length) {
  console.error('Usage: node scripts/prepare-lem.mjs [--sources-only]');
  process.exitCode = 2;
} else {
  try {
    const prepared = await prepareLemSources();
    console.log(`Prepared ${prepared.files.size} source files without changing the checkouts.`);
    for (const blocker of prepared.blockers) console.log(blocker);
    if (prepared.blockers.length && !args.includes('--sources-only')) process.exitCode = 1;
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
