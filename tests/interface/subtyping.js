//@flow

interface I {
  [string]: number;
  constructor(): void;
}

class C {
  x: number;
}

new C() as I;

declare class C2 {
  [string]: number;
  x: number;
}

new C2() as I;

class A {
  x: number;
}

class B extends A {
  y: number;
}

interface J {
  x: number;
  y: number;
}

new A() as J; // error
new B() as J;

class D {
  static x: number;
}

interface K {
  x: number;
}

new D() as K; // error
D as K;

class E {
  static x: number;
}

class F extends E {
  static y: number;
}

interface L {
  x: number;
  y: number;
}

E as L; // error
F as L;

class G {}

interface M {
  z: number;
}

interface N extends M {}

new G() as M; // error
new G() as N; // error
