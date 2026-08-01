//@flow

// Calling a generic class value *without* `new` bails out of implicit
// instantiation with an empty inferred type-argument list even though the class
// declares tparams. Pinning must not assume the two line up (regression:
// index-out-of-bounds). `new C(3)` would not exercise this: it goes down the
// ctor path, which does produce a type argument for `T`.
class C<T> {}
// expected error below, but should not panic
C(3); // ERROR: `C` has no call signature; pinning must not panic on the empty type-arg list
