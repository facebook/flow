// Only the global constructor mints. A binding of that name in the file
// shadows it, and then the call is an ordinary call, read the same way by the
// checker and by the signature.

function Symbol(description?: string): number {
  return 1;
}

const shadowed = Symbol('x');
shadowed as number; // OK: the local function's return type
shadowed as symbol; // ERROR: nothing minted a symbol here

export const exported = Symbol('x'); // ERROR: cannot build a typed interface
