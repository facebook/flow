// @flow

// Poly-to-poly subtyping must substitute each side's bounds against the full
// fresh-generic map, so a bound that forward-references a sibling tparam lines
// up on both sides (subtyping_kit.rs "do substitution all at once"). Before
// this change the left bound was compared raw, so the sibling reference would
// not match the fresh generic created for the right side.

declare var polyForward: <A extends B, B>(a: A, b: B) => A;

const sameShape: <A extends B, B>(a: A, b: B) => A = polyForward; // OK: identical forward-referencing bounds

// alpha-renamed but structurally identical; bounds still reference the sibling,
// just under different names.
const alphaRenamed: <X extends Y, Y>(a: X, b: Y) => X = polyForward; // OK: alpha-renamed but structurally identical

declare var polyBounded: <A extends B, B extends string>(a: A, b: B) => A;

const tighterTarget: <A extends B, B extends string>(a: A, b: B) => A =
  polyForward; // OK: source works for any B, target only needs `B extends string`

const looserTarget: <A extends B, B>(a: A, b: B) => A = polyBounded; // ERROR: source only accepts `B extends string`, target promises any B
