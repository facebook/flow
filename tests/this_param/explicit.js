class A {
  method(this: Class<this>) {}
  static staticMethod(this: this) {}
}

var a = new A();
a.method(); // NG

// False positive.  Most likely a problem with the "knot" of `fix_this_class`.
A.staticMethod(); // NG
