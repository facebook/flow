// @flow
class A {
  method(): this {
    return new this.constructor();
  }
  static staticMethod(): this {
    return new this();
  }
}

class B extends A {
/* Demo bugs in calls on `super`
  method() {
    return super.method();
  }
  static staticMethod(x) {
    return super.staticMethod(x);
  }
*/
}
