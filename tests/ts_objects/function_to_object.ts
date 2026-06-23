// Pure TS: function value flowing into object-shaped targets.
// In .js the `FunT -> Exact ObjT` case errors with `incompatible-exact`.
// In .ts the upper-side Exact extra-prop check is skipped, so casting a
// function to an exact object type is accepted. The `Indexed` target is
// intentionally not relaxed -- a callable function is not an indexed map.

declare function f(): void;

// Exact target (`{}` is exact under exact_by_default): accepted in .ts.
f satisfies {}; // OK

// Exact target with required props the function does not have: must still
// error -- the relaxation only suppresses the exact/inexact mismatch, not
// missing-prop checks.
f satisfies {x: number}; // ERROR: missing `x` on function

// Indexed target: still an error in .ts -- the relaxation does not cover
// indexed signatures.
f satisfies {[k: string]: number}; // ERROR: function vs indexed object
