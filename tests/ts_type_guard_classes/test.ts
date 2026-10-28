// Classes in TypeScript files are structural, so instances of unrelated classes may overlap.

class B {
  x: number = 1;
  b: string = '';
}
class C {
  c: string = '';
}
class C2 extends C {
  x: number = 1;
}
class D {
  d: string = '';
}
class L {
  x: number = 1;
}

new C2() as L; // okay
new C2() as C; // okay

declare function isL(x: unknown): x is L;

function f(x: B | C) {
  if (isL(x)) {
    // TypeScript narrows `x` to `B` and accepts this
    x as B; // error L & C ~> B, `new C2()` reaches here
  }
}
f(new C2());

function g(x: C | D) {
  if (isL(x)) {
    x as never; // error L & C ~> never, L & D ~> never
  }
}
g(new C2());
