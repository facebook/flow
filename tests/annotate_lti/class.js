// @flow

class A {

  aMethod(x) {
    return x + 1;
  }

  arrow = x => x ? 0 : 1;
}

const a = new A();
a.aMethod(2);
a.arrow(true);


