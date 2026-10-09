interface VarianceMerged<out T extends string | number = string | number> {
  readonly value: T;
}

interface VarianceUnchecked<out T> {
  readonly value: T;
}
