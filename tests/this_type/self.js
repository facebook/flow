class A {
  foo() { return this; } // escaped this
  bar(): this { return new A().foo(); }
  qux(): this { return this.bar(); } // OK (don't cascade errors)
}
