// Tooling uses canonical runtime helper names independently of their wire ABI.
export const isRuntimeImport = entry => entry.module === 'm';
export const runtimeImportName = entry => `native-${entry.name}`;
