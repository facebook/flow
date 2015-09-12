class C {
  C() { }
  foo() { }
  static bar() { }
  qux() { this.constructor.x; }
}
C.x;
(new C).foo.x;
C.bar.x;
