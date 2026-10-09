// A partial written the TypeScript way, without variance or a default.
interface VarianceMerged<T extends string | number> {
  get(): T;
}

// The adopted `out` is not checked against this declaration's members, as in
// TypeScript's libs.
interface VarianceUnchecked<T> {
  set(x: T): void;
}
