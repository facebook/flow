abstract class A {
  abstract m(): void;
}

class C extends A {
  m(): void {}
}

A as new () => A; // ERROR: abstract into non-abstract ctor slot

const c2 = A as abstract new () => A; // OK
const c3 = C as abstract new () => A; // OK

const c4 = C as new () => A; // OK
const a4 = new c4() as A; // OK
a4.m(); // OK

new c2() as A; // ERROR: c2 is abstract
new c3() as A; // ERROR: c3 is abstract
