// The registry lookup is shadowed the same way the bare call is. What a local
// `Symbol` names is what `Symbol.for` reads, so the call is an ordinary member
// call and mints nothing, in the checker and in the signature alike.

const Symbol = {
  for(key: string): number {
    return 1;
  },
};

const shadowed = Symbol.for('x');
shadowed as number; // OK: the local object's method
shadowed as symbol; // ERROR: nothing minted a symbol here

export const exported = Symbol.for('x'); // ERROR: cannot build a typed interface
