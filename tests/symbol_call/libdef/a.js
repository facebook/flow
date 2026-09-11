// What the call is tested against is the binding, not the type behind it: the
// callee has to be a global rather than something the file binds, and any
// global named `Symbol` counts, whichever library declared it. So a project
// that writes its own `Symbol` still mints, and the return type it wrote is not
// what the binding holds. TypeScript resolves the callee to the one declaration
// it ships and does not read a redeclared `Symbol` this way.

const k = Symbol('x');
k as number; // ERROR: minted, so not the declared return type
k as symbol; // OK

const o = {[k]: 1};
o[k] as number; // OK
