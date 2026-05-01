async function esm() {
  const esmodule = await import('./esmodule');
  esmodule.pi as number; // ok
  esmodule.pi as string; // error
  esmodule.default; // error

  const esmodulePromise = import('./esmodule');
  esmodulePromise.pi as string; // error
}

async function cjs() {
  const cjsmodule = await import('./cjsmodule');
  cjsmodule.pi as number; // ok
  cjsmodule.default as {pi: number};
  cjsmodule.pi as string; // error
  cjsmodule.default as number; // error
}
