// When an inexact source flows into a generic constraint that Flow models
// as exact (e.g. `T extends { a: number }`), the same `Exact`-target
// extra-prop relaxation should suppress the resulting `incompatible-exact`
// error in .ts files.

type Constrained<T extends {a: number}> = T;

interface RGB {
  a: number;
  b: number;
  c: number;
}

declare const rgb: RGB;

// `RGB` is an interface (modeled as InstanceT) with extra props `b` and
// `c`. The constraint `{a: number}` is checked as an exact-object upper.
// In .js this would error with "incompatible-exact"; in .ts it is
// accepted because the upper-side Exact extra-prop check is skipped.
rgb as Constrained<RGB>; // OK

// Negative: missing the required `a` is still an error -- only the
// extra-prop direction is relaxed.
interface NoA {
  b: number;
}
declare const noA: NoA;
noA as Constrained<NoA>; // ERROR: `a` missing in NoA
