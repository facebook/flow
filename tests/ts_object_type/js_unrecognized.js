// In .js files, `object` is not a recognized builtin -- it is only available
// in .ts/.d.ts files, so the name resolves like any other unknown identifier.
// This locks in the `.ts`-extension scoping.

type T = object; // ERROR -- cannot-resolve-name
