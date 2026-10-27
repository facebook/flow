// Identifier and optional string-literal keys are computed properties, not indexers.
type T = { [TREND_SERIES_ALL]: number }; // OK
type U = { ['data-x']?: string }; // OK

// Numeric and qualified keys retain their value-expression ASTs.
type Keys = { [42]: number; [-1]?: string; [Keys.value]: boolean }; // OK

// Computed methods retain their method and optional flags.
type Methods = { [KEY](): void; [OPTIONAL_KEY]?(x: number): void }; // OK

// Interfaces and ambient class bodies use the same computed-property grammar.
interface I {
  [KEY]: number; // OK
  ['data-x']?: string; // OK
}
declare class C {
  readonly [KEY]: number; // OK
  static ['data-x']?: string; // OK
}

// Named index signatures and mapped types remain distinct from computed properties.
type Indexer = { [key: string]: number }; // OK
type Mapped<T> = { [K in keyof T]: T[K] }; // OK
