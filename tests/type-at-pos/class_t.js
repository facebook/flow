// @flow

// class_t (t=Ty.This)
class A {
  static foo() {
    this.bar()  // HERE
  }
  static bar() {}
}

// class_t (t=Ty.Any)
var Any: any;
var a: Class<Any<*>>;

// class_t (t=Ty.Top)
var b: Class<mixed>;

// class_t (t=Ty.Bot)
var c: Class<*>;

class C1 {}
class C2 {}

// class_t (t=Ty.Inter)
var i: Class<C1 & C2>;

// class_t (t=Ty.Union)
var u: Class<C1 | C2>;
