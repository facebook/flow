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
  /*
   * Operator `Interface<.> would be handy for stripping an interface from a
   * class and for shifting interfaces (Class<SomeInterface> errors).
   */
  return M.asMethod1() + M.asMethod2();
}

var b = new B();
var x1 = b1(b);
var x2 = b2(C);
var x3 = b3(C);
var d = new D();
var y1 = ab1(d);
var y2 = ab2(D);
