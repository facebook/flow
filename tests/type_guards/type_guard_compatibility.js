class A {}
class B {}
class C extends A {}

//
// Decls
//

function okay_1(x: A): x is C { // okay
  return x instanceof C;
}

declare function okay_poly_1<X>(x: Array<X>): x is Array<X>; // okay

function error_1(x: A): x is B { // error B ~> A
  return x instanceof B;
}

declare function error_poly_1<X>(x: Array<X>): x is Array<unknown>; // error unknown ~> X

//
// Type aliases
//

type F_okay_1 = (x: A) => x is C; // okay

type F_error_1 = (x: A) => x is B; // error B ~> A

type F_error_poly_1 = <X>(x: Array<X>) => x is Array<unknown>; // error unknown ~> X
type F_error_poly_2<X> = (x: Array<X>) => x is Array<unknown>; // error unknown ~> X
