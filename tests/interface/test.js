/* @flow */

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

function b1(ell: BI): number {
  return ell.bMethod1();
}

function b2(Ell: Class<B>): number {
  return Ell.bsMethod1();
}

function b3(Ell: Class<BI>): number {
  return Ell.bsMethod1();
}

var b = new B();
var y1 = b1(b);
var y2 = b2(C);
var y2 = b3(C);
