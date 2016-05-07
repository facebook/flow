//@flow

class A {
  method(this: Class<this>) {}
  static staticMethod(this: this) {}
}

var a = new A(); // NG
a.method();
A.staticMethod(); // NG
