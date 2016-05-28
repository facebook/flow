interface I {
  anotherMethod(): number;
  static anotherStaticMethod(): number;
}

class A {
  x: number;
  static x: number;
  method(this: this & I): number {
    return this.x + this.anotherMethod();
  }
  static staticMethod(this: Class<this> & Class<I>): number {
    return this.x + this.anotherStaticMethod();
  }
}
A.x = 1

var a = new A();                   // OK
var n1: number = a.method();       // NG
var n2: number = A.staticMethod(); // NG

class B extends A {
  anotherMethod(): string {
    // NGish: Incompatible with the `this` constraint on `method`.
    return "another method";
  }
  static anotherStaticMethod(): string {
    // NGish: Incompatible with the `this` constraint on `staticMethod`.
    return "another static method";
  }
}

var b = new B();
var s1: string = b.anotherMethod(); // OK: Breaks a type requirement on
                                    // `method`'s `this` pseudo-param, but
                                    // that's okay on a `B` instance.
var n3: number = b.method(); // NG: The return type on `anotherMethod` of `B`
                             // breaks the constraint on `method`'s `this`
                             // pseudo-param.
var s2: string = B.anotherStaticMethod(); // OK: Analogous to `s1`.

// False positive.  Flow ignores the following error, unless I comment out `n3`
// above. If I move the `s3` above `n3`, then `s3` errors and the `n3` error is
// ignored. Is this a bug or a feature?  If bug, it traces to an `Ops.peek`
// call in `typecheck_error`, where the call yields the prior function call's
// loc.
var s3: number = B.staticMethod(); // NG: The return type on
                                   // `anotherStaticMethod` of `B` breaks the
                                   // constraint on `staticMethod`'s `this`
                                   // pseudo-param.

class C extends A {
  anotherMethod() {
    return 5;
  }
  static anotherStaticMethod() {
    return 2;
  }
}

var c = new C();
var n3: number = c.method();       // OK
var n4: number = C.staticMethod(); // OK

class K {
  method(this: I): number {
    return this.anotherMethod(); // OK
  }
  methodCaller(): number {
    return this.method(); // NG: The implicit pseudo-param of `methodCaller`,
                          // i.e. `this`, doesn't satisfy the `I` interface.
  }
}

var k = new K();             // OK
var n5: number = k.method(); // NG: `k` doesn't satisfy the requirements of `I`
var n6: number = k.methodCaller();

var ell = {
  fn: K.prototype.method
};
var n7: number = ell.fn(); // NG

var fn = K.prototype.method;
var n8: number = fn(); // NG
