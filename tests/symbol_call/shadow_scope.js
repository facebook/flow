// The binding is what shadows, so where the binding sits is what decides. A
// block-scoped one does not reach a call written outside its block, and that
// call mints as it would with no shadow in the file at all. The checker and the
// signature read the scope the same way.

{
  const Symbol = () => 1;
  const inner = Symbol();
  inner as number; // OK: the block's arrow
  inner as symbol; // ERROR: nothing minted a symbol here
}

const outer = Symbol();
const fromOuter = {[outer]: 1};
fromOuter[outer] as number; // OK: the block's binding does not reach here

// A signature that could not name the symbol would report on this export, the
// way it does in `shadow.js`, so nothing reported here is the signature reading
// the same scope the checker did.
export const exported = Symbol();
