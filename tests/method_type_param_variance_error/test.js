// @flow
// Variance modifiers on function/method type parameters have no semantic
// meaning — the type-param is universally quantified at each call site,
// not definition-site, so there is no enclosing-type subtyping relationship
// for the variance marker to govern. Flow rejects all spellings (+T, -T,
// in T, out T) regardless of any experimental flag.

// Function type annotations
type F = <+T>(x: T) => void; // ERROR
type G = <-T>() => T; // ERROR

// Class method type parameters
class C {
  foo<+T>(x: T): void {} // ERROR
  bar<-T>(): T {
    throw 0;
  } // ERROR
}

// Interface method type parameters
// TODO: should ERROR like the class-method case above, but currently accepted
// silently because mk_method_func_sig in type_annotation.ml does not call
// error_on_unsupported_variance_annotation. Will be fixed in a follow-up diff.
interface I {
  foo<+T>(x: T): void; // OK (TODO: should ERROR)
  bar<-T>(): T; // OK (TODO: should ERROR)
}
