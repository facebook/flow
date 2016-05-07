class Generic<X> {
  clone(): Generic<X> { return this; }
}

class Implicit<X> { arg: X; val: X; }
class ImplicitNumber extends Implicit { arg: number; }

(new ImplicitNumber().val: string) // error: number ~> string

class A {}

class B<T> extends A {
  x: T;
  constructor(x: T) {
    super();
    this.x = x;
  }
  method(): this {
    return new this.constructor(this.x);
  }
  static staticMethod(x: T): this {
    return new this(x);
  }
}

class C extends B<number> {
/* Demo bugs in calls on `super`
  method() {
    var t = super.method();
    t.x += 5;
    return t;
  }
  static staticMethod(x) {
    var t = super.staticMethod(x);
    t.x += 6;
    return t;
  }
*/
}

var a = new A();
var b1 = new B(1);
var b2 = new B("a string");
var b3 = new B(null);
var c1: C = new C(2);
var c2 = new C("another string"); // NG
var c3 = new C(null); // NG

c1 = b1.method(); // NG
c1 = b2.method(); // NG
c1 = b3.method(); // NG
