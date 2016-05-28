class A {
  method(this: Class<this>) {}
  static staticMethod(this: this) {}
}

var a = new A();
a.method(); // NG

// False positive.
A.staticMethod(); // NG
