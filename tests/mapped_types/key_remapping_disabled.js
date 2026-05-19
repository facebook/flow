type O = {a: number, b: string};

// Bare `as` -> ERROR unsupported-syntax
type Id = {[K in keyof O as K]: O[K]}; // ERROR

// `as` combined with `-?` and `-readonly` -> all gate-off errors fire in one pass
type CombinedAsMinusReadonlyMinusOptional = {
  -readonly [K in keyof O as K]-?: O[K],
}; // ERROR x3 (as, -readonly, -?)
