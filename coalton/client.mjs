// Configure the shared session UI before starting the Coalton REPL.
import {config} from './examples.mjs';
import {createReplDebugger} from './repl-debugger.mjs';
config.createDebugger = createReplDebugger;
await import('./app.mjs');
