type I = {
  anotherMethod(): number;
};

type J = {
  anotherStaticMethod(): number;
};

class A {
  x: number;
  static x: number;
  method(this: this & I): number {
    return this.anotherMethod();
  }
  static staticMethod(this: J & Class<this>): number {
    return this.anotherStaticMethod();
  }
}
A.x = 1

var a = new A();
var n1: number = a.method(); // NG
var n2: number = A.staticMethod(); // NG

class B extends A {
  anotherMethod(): string {
    // NG: Incompatible with the `this` constraint on `method`.
    return "another method";
  }
  static anotherStaticMethod(): string {
    // NG: Incompatible with the `this` constraint on `staticMethod`.
    return "another static method";
  }
}

var b = new B();
var s1: string = b.anotherMethod();
var n3: number = b.method(); // NG
var s2: string = B.anotherStaticMethod();

// False negative.  This is an error, but the errors include a possibly
// unnecessary error, i.e. `J & Class<this>` incompatible with
// `class B extends A`.  This traces back to the InstanceT ~> ObjT flow: found
// fields get `rec_unify`ed instead of `rec_flow`ed (I suspect that this has to
// do with the dynamism of objects under flow).
var s3: number = B.staticMethod(); // NG

class C extends A {
  anotherMethod() {
    return 5;
  }
  static anotherStaticMethod() {
    return 2;
  }
}

var c = new C();
var n3: number = c.method();

// False negative.
var n4: number = C.staticMethod(); // OK

class K {
  method(this: I): number {
    return this.anotherMethod();
  }
  methodCaller(): number {
    return this.method(); // NG
  }
}

var k = new K();
var n5: number = k.method(); // NG
var n6: number = k.methodCaller();

var ell = {
  anotherMethod(): number {
    return 3;
  },
  fn: K.prototype.method
};

var n7: number = ell.fn();

class Issue1369<T> {
  _x: T;
  getXMultBy(this: Issue1369<number>, y: number): number {
    return this._x * y;
  }
}

var s = new Issue1369();
s._x = 5;
s.getXMultBy(3);

var t = new Issue1369();
t._x = "a string";
t.getXMultBy(4); // NG
