// A union member that is an instance of a class unrelated to the guard's class is dropped.

class A { a: number = 1; }
class B { b: string = ''; }
class C extends A {}
class D {}

declare function isA(x: unknown): x is A;

function union_member(x: A | B) {
  if (isA(x)) {
    x as A; // okay
    x as B; // error A ~> B
  } else {
    x as B; // okay
  }
}

function subclass(x: A | B) {
  declare function isC(x: unknown): x is C;
  if (isC(x)) {
    x as C; // okay
  }
}

function superclass(x: C | B) {
  if (isA(x)) {
    x as C; // okay
  }
}

function with_primitive(x: A | B | number) {
  if (isA(x)) {
    x as A; // okay
  }
}

function all_unrelated(x: A | B) {
  declare function isD(x: unknown): x is D;
  if (isD(x)) {
    x as D; // okay
    x as empty; // error (D & A) | (D & B) ~> empty
  }
}

function single(x: A) {
  declare function isB(x: unknown): x is B;
  if (isB(x)) {
    x as A; // okay
    x as B; // okay
  }
}

function interface_guard(x: A | B) {
  declare interface HasA { a: number }
  declare function isHasA(x: unknown): x is HasA;
  if (isHasA(x)) {
    x as A; // error HasA & B ~> A, an instance of B may have `a`
  }
}

function declared_classes() {
  declare class Base {}
  declare class Left extends Base {}
  declare class Right extends Base {}
  declare const x: Left | Right;
  declare function isLeft(x: unknown): x is Left;
  if (isLeft(x)) {
    x as Left; // okay
    x as Right; // error Left ~> Right
  }
}

function union_guard_partly_unrelated(x: A | B) {
  declare function isAorD(x: unknown): x is A | D;
  if (isAorD(x)) {
    x as A; // okay
    x as B; // error A ~> B
  } else {
    x as B; // okay
  }
}

function union_guard_subclass(x: A | B) {
  declare function isCorD(x: unknown): x is C | D;
  if (isCorD(x)) {
    x as C; // okay
  }
}

function union_guard_all_unrelated(x: A | B) {
  declare class E {}
  declare function isDorE(x: unknown): x is D | E;
  if (isDorE(x)) {
    x as D | E; // okay
    x as empty; // error (D & A) | (E & A) | (D & B) | (E & B) ~> empty
  }
}

function interface_extends_class(x: A | B) {
  declare interface I extends A {}
  class B3 extends B implements I {
    a: number = 1;
  }
  new B3() as I; // okay, so a subclass of `B` can be an `I`
  declare function isI(x: unknown): x is I;
  if (isI(x)) {
    x as A; // okay
    x as empty; // error A ~> empty, I & B ~> empty
  }
}
