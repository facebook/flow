type O = {a: number, b: string};

// Bare `as` -> ERROR unsupported-syntax
type Id = {[K in keyof O as K]: O[K]}; // ERROR

// `as` and `-readonly` errors fire in one pass
type CombinedAsMinusReadonly = {
  -readonly [K in keyof O as K]-?: O[K],
}; // ERROR x2 (as, -readonly)
