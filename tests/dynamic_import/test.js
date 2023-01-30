async function esm() {
  const esmodule = await import('./esmodule');
  (esmodule.pi: number); // ok
  (esmodule.pi: string); // error
  esmodule.default; // error

  const esmodulePromise = import('./esmodule');
  (esmodulePromise.pi: string); // error
}

async function cjs() {
  const cjsmodule = await import('./cjsmodule');
  (cjsmodule.pi: number); // ok
  (cjsmodule.default: {pi: number});
  (cjsmodule.pi: string); // error
  (cjsmodule.default: number); // error
}
