// @flow

class A {
  foo;
  declare bar;

  aMethod(x) {
    this.foo = new A();
    this.bar = new A();
    return x + 1;
  }

  arrow = x => x ? 0 : 1;
}

const a = new A();
a.aMethod(2);
a.arrow(true);
