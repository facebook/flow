class A {
  static x: number;
  static y: string;
  static foo(x: number) { }
  static bar(y: string) { }
}
A.qux = function(x: string) { } // error?

class B extends A {
  static x: string; // error?
  static foo(x: string) { } // error?
  static main() {
    B.x = 0; // error
    B.x = "";
    B.foo(0); // error
    B.foo("");
    B.y = 0; // error
    B.bar(0); // error
    B.qux(0); // error
  }
}

class C<X> {
  static x: X;
  static bar(x: X) { }
}

class D extends C<string> {
  static main() {
    D.foo(0); // error?

    D.bar(0);
  }
}

(new A: typeof A);
(B: typeof A);

class E {
  static x: number;
  static foo(): string {
    this.bar(); // error
    return this.x; // error
  }
}