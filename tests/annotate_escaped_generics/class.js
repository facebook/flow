//@flow

var x = 42;

class A<X> {
  s: X;
  f() {
    var y = 42;
    x = this.s;
    y = this.s;
  }
}

var y = 22;

class B<X> {
  s: X;
  f() {
    if (this.s) {
      y = this.s;
    }
    this.s = y;
  }
}

var z = 42;

class C<X> {
  s;
  f<Y>(y: Y, x: X) {
    this.s = y;
    if (y) {
      this.s = x;
    }
    var a = 42;
    var captured_this = this;
    function h<Z>(z: Z) {
      captured_this.s = z;
      a = z;
    }
    <W>(w: W) => {
      this.s = w;
      a = w;
    };
    z = this.s;
  }
}

class ObjectUtils {
  static stableCopy<T: mixed>(value: T): T {
    return ObjectUtils.stableFilteredCopy(value);
  }

  static stableFilteredCopy<T: mixed>(value: T): T {
    const stable = {};
    return (stable: any);
  }
}

type D1 = {p: *};

class C1<Y> {
  x: {p: Y};
  foo(): D1 {
    return this.x;
  }
}


class O<T> {
  a = this;
  b;
  f(t: T) {
    const x = this;
    this.b = t;
    return t;
  }
}
