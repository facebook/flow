/* @flow */

type AI = class {
  aMethod1(): number;
  static asMethod1(): string;
};

class A {
  constructor() {};
  aMethod1(): number {
    return 13;
  }
  aMethod2(): string {
    return "from A's nonstatic";
  }
  static asMethod1(): string {
    return "from A's static";
  }
  static asMethod2(): number {
    return 7;
  }
}

type BI = class {
  bMethod1(): number;
  static bsMethod1(): number;
};

class B {
  constructor() {};
  bMethod1(): number {
    return 7;
  };
  bMethod2(): string {
    return "a string";
  };
  static bsMethod1(): number {
    return 123;
  };
  static bsMethod2(): string {
    return "another string";
  }
}

class C extends B {
  a(): number {
    return 22;
  }
  static c(): string {
    return "c";
  }
}

class D extends C {
  aMethod1(): number {
    return 98;
  }
  static asMethod1(): string {
    return "asMethod on D";
  }
}

type EI<T> = class {
  eMethod1(): T;
  static esMethod1(): Class<T>;
}

class E {
  eMethod1() {
    return new B();
  }
  eMethod2() {
    return 5;
  }
  static esMethod1() {
    return B;
  }
  static esMethod2() {
    return "e static";
  }
}

class F {
  eMethod1() {
    return new D();
  }
  eMethod2() {
    return "f method";
  }
  static esMethod1() {
    return D;
  }
  static esMethod2() {
    return 1;
  }
}

function b1(ell: BI): number {
  return ell.bMethod1();
}

function b2(Ell: Class<B>): number {
  return Ell.bsMethod1();
}

function b3(Ell: Class<BI>): number {
  return Ell.bsMethod1();
}

function ab1(m: AI&BI): number {
  return m.aMethod1() + m.bMethod1();
}

function ab2(M: Class<AI>&Class<BI>): string {
  return M.asMethod1() + M.bsMethod1();
}

function e1(N: Class<EI<B>>): string { // Shifted TypeApp
  var Bp = N.esMethod1();
  return Bp.bsMethod1() + Bp.bsMethod2();
}

function e2(n: EI<B>): string {
  var b = n.eMethod1();
  return b.bMethod1() + b.bMethod2();
}

type GI<T> = class Id<U> { // class<U> currently rejected by parser, right?
  gMethod1(): U;
  static gsMethod1(): U;
};

var b: BI = new B();
var x1 = b1(b);
var x2 = b2(C);
var x3 = b3(C);
var d = new D();
var y1 = ab1(d);
var y2 = ab2(D);
var e: EI<B> = new F();
//var f: EI<D> = new E(); // OK error: E returns B's, which may not have all of the methods of D's
var z1 = e.eMethod1();
var z2 = e.eMethod1(b,d); // Too many arguments => okay by Flow.
var Z3 = e.constructor.esMethod1();
var Z4 = e.constructor.esNonmethod(b,d); // NG: Type checks despite bad name.
var Z5 = E.esMethod1();
var Z6 = F.esMethod1();
var z7 = e1(E);
var z8 = e1(F);
var z9 = e2(e);
/*
 * Operator `Interface<.>` would be handy for stripping an interface from a
 * class and for shifting interfaces (Class<SomeInterface> errors).
 */
