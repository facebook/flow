//@flow

var e = new Array(10);

function f1<X>(x: X, escape: boolean, n: number): X | number {
  if (escape) {
    e[n] = x;
  }
  return e[n];
}

var c = class e {
  f<X>(x: X, escape: boolean): X | Class<this> {
    if (escape) {
      e = x;
    }
    return e;
  }
};

var a = Object.create(null, {x: {value: 42}});

function f2<X>(x: X, escape: boolean): X | number {
  if (escape) {
    a.x = x;
  }
  return a.x;
}

var a2 = Object.keys({x: 42});

function f3<X>(x: X, escape: boolean, n: number): X | string {
  if (escape) {
    a2[n] = x;
  }
  return a2[n];
}

function f4(escape: boolean, n: number): void {
  function g<X>(x: X): X | ((boolean, number) => void) {
    if (escape) {
      g = x;
    }
    return g;
  }
}

class C {}

function f5<X>(x: X, escape: boolean): X | Class<C> {
  if (escape) {
    C = x;
  }
  return C;
}

class D {
  f<X>(x: X, escape: boolean): X {
    if (escape) {
      D = x;
    }
    return D;
  }
}

var u = (<X>(x: X, escape: boolean): X => {
  if (escape) {
    u = x;
  }
  return u;
})(42, true);

var a3 = {};

function f6<X>(x: X, escape: boolean): X | void {
  if (escape) {
    a3.x = x;
  }
  return a3.x;
}

var a4 = () => 42;

function f7<X>(x: X, escape: boolean): X | void {
  if (escape) {
    a4.x = x;
  }
  return a4.x;
}

var a5 = Object.create(null);

function f8<X>(x: X, escape: boolean): X | void {
  if (escape) {
    a5.x = x;
  }
  return a5.x;
}

var a6 = {};
a6.a7 = 42;
function f9<Y>(y: Y, escape): ?Y {
  var {a7, ...a8} = a6;
  a8.c = y; // this should be ok
  function g<X>(x: X): ?X {
    if (escape) {
      a8.b = x;
    }
    return a8.b;
  }
  return a8.c;
}

var wwo;

class A<X> {}

function f<X>() {
  wwo = new A<X>();
}

var ooo;

class B<X> {
  s: X;
  f() {
    ooo = this.s;
  }
}
