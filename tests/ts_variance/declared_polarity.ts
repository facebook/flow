// In .ts files, EPolarityMismatch is fully suppressed. TS's
// own variance inference treats method-syntax params as bivariant,
// so this canonical shape is accepted by tsc --strict. Flow
// matches that here.
//
// Known divergence: TS still rejects arrow-property and call-sig
// shapes (TS2636) because those go through the strictFunctionTypes
// path. Flow does not flag them either since EPolarityMismatch is
// gated entirely in .ts. Flow being more permissive than TS in
// these niche cases is acceptable; Flow blocking valid TS code
// is not.

interface Box<out T> {
  set(val: T): void; // OK in .ts: matches tsc behavior
}
