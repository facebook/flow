/* @flow */

class A {
  abstract m(): void;
}

class B extends A {
  m() {}
}

let X: AbstractClass<A> = B;
let x1 = new X; //ng
let Y: AbstractClass<B> = B;
let y1 = new Y;

function concretize(C: AbstractClass<A>): Class<A> {
  return class K extends C {
    m() {}
  }
}

concretize(A);
concretize(B);
