/* @flow */

class K {
  a: number;
  constructor(a: number) {
    this.a = a;
  }
  static create() {
    return new K(3);
  }
}

class L {
  b: string;
  constructor(b: string) {
    this.b = b;
  }
  static create2() {
    return new L("a string");
  }
}

function newC<T: K | L>(i: T): T {
  if (i instanceof K) {
    return new i.constructor(5);
  } else {
    return new i.constructor("some string");
  }
}

function staticC<T: K | L>(i: T): T {
  if (i instanceof K) {
    return i.constructor.create();
  } else {
    return i.constructor.create2();
  }
}

var k = new K(1);
var kp = newC(k);
var kpp = staticC(k);

var ell = new L("asdf");
var ellp = newC(ell);
var ellpp = staticC(ell);
