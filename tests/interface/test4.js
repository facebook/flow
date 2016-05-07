interface I {
  fn(): number;
  static gn(): number;
}

class A {}
var a = new A();
var p: A & I = a; // NG
var P: Class<A> & Class<I> = A; // NG

class B extends A {
  fn(): number {
    return 1;
  }
  gn(): number {
    return 2;
  }
}
var b = new B();
var q: A & I = b; // OK
var Q: Class<A> & Class<I> = B; // OK
