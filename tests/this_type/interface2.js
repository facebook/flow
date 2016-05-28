interface I {
  static deref(): this
}

type T = {
  x: string,
  TheClass: Class<C>
}

class C {
  constructor(n: number) {}
  fn(this: this & I, t: T): this {
    return this.constructor.deref();
  }
}

class D extends C {
  static deref(): this {
    return new this("qwer");
  }
}

var c1 = new C(5); // OK
var c2 = c1.fn({x:"asdf",TheClass:C}); // NG: missing `deref`
var d1 = new D(6); // OK
var d2 = D.deref(); // OK

class X {
  fn(this: this & I): this {
    return new this.constructor();
  }
}

class Y extends X {
  static deref() {
    return new this();
  }
}

var x1 = new X(); // OK
var x2 = x1.fn(); // NG: missing `deref`
var y1 = new Y(); // OK
var y2 = Y.deref(); // OK
