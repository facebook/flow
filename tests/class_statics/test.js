class A {
  static x: number;
  static y: string;
  static foo(x: number) { }
  static bar(y: string) { }
}
A.qux = function(x: string) { } // error

class B extends A {
  static x: string; // error
  static foo(x: string) { } // error
  static main() {
    B.x = 0; // error
    B.x = "";
    B.foo(0); // error
    B.foo("");
    B.y = 0; // error
    B.bar(0); // error
    B.qux(0); // error
  }
  static create(): A {
    return new this();
  }

  static badCreate(): number {
    return new this(); // error B ~> number
  }
}

class C<X> {
  static x: X;
  static bar(x: X) { }
  static create(): C<any> {
    return new this();
  }
}

class D extends C<string> {
  static main() {
    D.foo(0); // error

    D.bar(0);
  }
}

var d: C<any> = D.create();
(new A: typeof A);
(B: typeof A);

class E {
  static x: number;
  static foo(): string {
    this.bar(); // error
    return this.x; // error
  }
}

class N {
  static x: number;
  static f(): string {
    return "";
  }
}

(N.x: number); // OK
N.x = 1; // OK
(N.x: empty); // ERROR
N.xxx = () => {}; // ERROR

class M extends N{
  static g(): string {
    return "";
  }
}
(M.f(): number); // ERROR
(M.g(): number); // ERROR

// note: above classdefs are sufficiently annotated to export
module.exports = {
  A: A, B: B, C: C, D: D, E: E
}
