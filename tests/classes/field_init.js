/* @flow */

/* Field initializers without annotations are checked for every instantiation of
 * any type parameters (including `this`). This can cause problems if the field
 * initializers internally references the type param. It will see the various
 * instantiations and likely cause errors downstream.
 *
 * The simple solution to this is to add an annotation to the field. For
 * functions, you can fully annotate the parameters/return type inline instead
 * of adding a separate, redundant function type annotation on the field */

// OK, `f` is fully annotated, so we extract a `T => T` type annotation
class C1<T> {
  f = (x: T): T => x;
  m(x: T): T { return this.f(x) }
}

// Error: `f` is not fully annotated
class C2<T> {
  f = (x: T) => x;
  m(x: T): T { return this.f(x) } // error: incompatible instantiation
}

// OK: Can still provide separate annotation
class C3<T> {
  f: T => T = (x: T) => x;
  m(x: T): T { return this.f(x) }
}
