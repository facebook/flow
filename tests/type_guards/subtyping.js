class A {}
class B {}
class C extends A {}

function non_matching_prams(f: (x: mixed, y: mixed) => x is number): (y: mixed, x: mixed) => x is number {
  return f; // error 'x' is not in the same position as 'y'
}

function non_type_guard_to_type_guard(f: (x: mixed) => boolean): (x: mixed) => x is number {
  return f; // error non type predicate
}

function pred_based_to_type_guard(): (x: mixed) => x is number {
  declare function f(x: mixed): boolean %checks(typeof x === "number");
  return f; // error pred based to type guard based
}

function type_guard_to_pred_based(g: (x: mixed) => x is number): void {
  declare function f(x: mixed): boolean %checks(typeof x === "number");
  (g: typeof f); // error type guard based to pred based
}

// Subtyping

function type_guard_subtyping_ok_1(f: (x: mixed) => x is A): (x: mixed) => x is A {
  return f; // okay
}

function type_guard_subtyping_ok_2(f: (x: mixed) => x is C): (x: mixed) => x is A {
  return f; // okay
}

function type_guard_subtyping_ok_3(f: <A>(x: mixed) => x is Array<A>): <B>(x: mixed) => x is $ReadOnlyArray<B> {
  return f; // okay
}

function type_guard_subtyping_error_1(f: (x: mixed) => x is A): (x: mixed) => x is B {
  return f; // error A ~> B
}

function type_guard_subtyping_error_2(f: (x: mixed) => x is A): (x: mixed) => x is C {
  return f; // error A ~> C
}

function type_guard_subtyping_error_3(f: <A>(x: mixed) => x is $ReadOnlyArray<A>): <B>(x: mixed) => x is Array<B> {
  return f; // error
}

// Unification

function type_guard_unif_ok_1(f: Array<(x: mixed) => x is A>): Array<(x: mixed) => x is A> {
  return f; // okay
}

function type_guard_unif_error_1(f: Array<(x: mixed) => x is C>): Array<(x: mixed) => x is A> {
  return f; // error C ~ A
}

function type_guard_unif_error_2(f: Array<(x: mixed) => x is A>): Array<(x: mixed) => x is B> {
  return f; // errors A ~> B, B ~> A
}

function type_guard_unif_error_3(f: Array<(x: mixed) => x is A>): Array<(x: mixed) => x is C> {
  return f; // error A ~> C
}
