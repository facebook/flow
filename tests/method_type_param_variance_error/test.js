// @flow
// Variance modifiers on function/method type parameters have no semantic
// meaning — the type-param is universally quantified at each call site,
// not definition-site, so there is no enclosing-type subtyping relationship
// for the variance marker to govern. Flow rejects all spellings (+T, -T,
// in T, out T) regardless of any experimental flag.

// Function type annotations
type F = <out T>(x: T) => void; // ERROR
type G = <in T>() => T; // ERROR

// Class method type parameters
class C {
  foo<out T>(x: T): void {} // ERROR
  bar<in T>(): T {
    throw 0;
  } // ERROR
}

// Interface method type parameters
interface I {
  foo<out T>(x: T): void; // ERROR
  bar<in T>(): T; // ERROR
}
