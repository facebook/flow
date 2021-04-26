//@flow

interface I {
  [string] : number;
  constructor() : void
}

class C {
  x : number
}

(new C : I);


declare class C2 {
  [string] : number;
  x : number
}

(new C2 : I);

class A {
    x : number
}

class B extends A {
    y : number
}

interface J {
    x : number;
    y : number
}

(new A : J); // error
(new B : J);


class D {
    static x : number
}

interface K {
    x : number
}

(new D : K); // error
(D : K);


class E {
  static x : number
}

class F extends E {
  static y : number
}

interface L {
    x : number;
    y : number
}

(E : L); // error
(F : L);

class G {
}

interface M {
  z : number
}

interface N extends M {
}

(new G : M); // error
(new G : N); // error
