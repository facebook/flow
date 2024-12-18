const moduleRef = 'm#esm';
declare const extractedModuleInfo: typeof moduleRef extends $Flow$ModuleRef<infer T> ? T : empty;
extractedModuleInfo as $Flow$EsmModuleMarkerWrapperInModuleRef<mixed>; // ok
extractedModuleInfo as {+default: string}; // ok
extractedModuleInfo as string; // error;
extractedModuleInfo as empty; // error;
