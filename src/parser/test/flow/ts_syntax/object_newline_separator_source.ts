// Type literals allow newline-separated optional properties, methods, and index signatures.
type T = {
  count: number // OK
  ['data-x']?: string // OK
  read(): number // OK
  [key: string]: unknown // OK
}

// Interfaces and ambient classes allow the same member separators.
interface I {
  readonly count: number // OK
  read(): number // OK
}
declare class C {
  count: number // OK
  read(): number // OK
}

// Explicit commas and semicolons can be mixed with newline separators.
type Mixed = {
  a: number,
  b: string;
  c?: boolean // OK
  d: number // OK
}

// A newline inside a block comment also separates type members.
type Comment = { a: number /*
*/ b: string }; // OK
