// A function declaration is bound over the whole file it is written in, so it
// shadows a call written above it just as it shadows one written below, and
// neither call mints.

const early = Symbol('x');
early as number; // OK: the hoisted function's return type
early as symbol; // ERROR: nothing minted a symbol here

function Symbol(description?: string): number {
  return 1;
}

export const exported = Symbol('x'); // ERROR: cannot build a typed interface
