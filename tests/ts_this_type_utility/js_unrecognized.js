// In .js files, `ThisType` is not a recognized builtin -- the name resolves
// like any other unknown identifier. Locks in the `.ts`-extension scoping.

type T = ThisType<number>; // ERROR -- cannot-resolve-name
