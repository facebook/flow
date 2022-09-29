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

class B {
  prop1 = 42; // ok
  prop2 = 10 + 30; // annot
  prop3; // annot
  prop4 = (x: number): number => 42; // ok
  prop5 = (x: number) => 42; // annot return
  prop6 = function (x: number) { return 42 } // annot return

  method1() { } // ok
  method2() { return 42; } // annot return
  constructor() { return 10; } // ok (but separately, we should error on returns in constructors no?)

  #priv1 = 10 + 20; // annot
  #priv2; // annot
}

function sanity_check_that_we_dont_annotate_non_method_functions() {
  return 42;
}
