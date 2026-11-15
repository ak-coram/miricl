import {runtimeImportName} from '../../scripts/native-imports.mjs';
import {nativeImageModules} from '../../scripts/image-composition.mjs';

// Both lookup helpers resolve the current function definition at runtime. Literal lookup
// fusion changes the helper name without changing that redefinition contract.
export const usesDynamicFunctionLookup = names => names.some(name =>
  name === 'native-function-reference' || name === 'native-literal-function');
// Optimization assertions concern helper operations, not their serialized ABI.
export const nativeModuleImports = module => WebAssembly.Module.imports(module)
  .map(entry => ({...entry, name: runtimeImportName(entry)}));

export const nativeBundleImports = bytes => nativeImageModules(bytes).modules
  .flatMap(module => module.imports);

