class A {}
class B {}
class C extends A {}

function non_matching_prams(f: (x: unknown, y: unknown) => x is number): (y: unknown, x: unknown) => x is number {
  return f; // error 'x' is not in the same position as 'y'
}

function non_type_guard_to_type_guard(f: (x: unknown) => boolean): (x: unknown) => x is number {
  return f; // error non type predicate
}

// Subtyping

function type_guard_subtyping_ok_1(f: (x: unknown) => x is A): (x: unknown) => x is A {
  return f; // okay
}

function type_guard_subtyping_error_1(f: (x: unknown) => x is A): (x: unknown) => x is B {
  return f; // error A ~> B
}

function type_guard_subtyping_error_2(f: (x: unknown) => x is A): (x: unknown) => x is C {
  return f; // error A ~> C
}

function type_guard_subtyping_error_3(f: <A>(x: unknown) => x is ReadonlyArray<A>): <B>(x: unknown) => x is Array<B> {
  return f; // error
}

function type_guard_subtyping_error_4(f: (x: unknown) => x is C): (x: unknown) => x is A {
  return f; // error C <~> A
}

function type_guard_subtyping_error_5(f: <A>(x: unknown) => x is Array<A>): <B>(x: unknown) => x is ReadonlyArray<B> {
  return f; // error Array<A> <~> ReadonlyArray<B>
}

function type_guard_subtyping_one_sided_ok_1(f: (x: unknown) => implies x is A): (x: unknown) => implies x is A {
  return f; // okay
}

function type_guard_subtyping_one_sided_ok_2(f: (x: unknown) => x is A): (x: unknown) => implies x is A {
  return f; // okay
}

function type_guard_subtyping_one_sided_ok_3(f: (x: unknown) => x is C): (x: unknown) => implies x is A {
  return f; // okay due to "implies" on the RHS
}

function type_guard_subtyping_one_sided_ok_4(f: (x: unknown) => implies x is C): (x: unknown) => implies x is A {
  return f; // okay due to "implies" on the RHS
}

function type_guard_subtyping_one_sided_error(f: (x: unknown) => implies x is A): (x: unknown) => x is A {
  return f; // error
}

// Unification

function type_guard_unif_ok_1(f: Array<(x: unknown) => x is A>): Array<(x: unknown) => x is A> {
  return f; // okay
}

function type_guard_unif_error_1(f: Array<(x: unknown) => x is C>): Array<(x: unknown) => x is A> {
  return f; // error C ~ A
}

function type_guard_unif_error_2(f: Array<(x: unknown) => x is A>): Array<(x: unknown) => x is B> {
  return f; // errors A ~> B, B ~> A
}

function type_guard_unif_error_3(f: Array<(x: unknown) => x is A>): Array<(x: unknown) => x is C> {
  return f; // error A ~> C
}

function type_guard_unif_one_sided_ok(f: Array<(x: unknown) => implies x is A>): Array<(x: unknown) => implies x is A> {
  return f; // okay
}

function type_guard_unif_one_sided_error(f: Array<(x: unknown) => x is A>): Array<(x: unknown) => implies x is A> {
  return f; // error
}
