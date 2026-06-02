abstract class A {
  abstract m(x: string | number): void;
}

class B extends A {
  m(x: string): void {} // ERROR: narrowing parameter type
}

class C extends A {
  m(x: string | number): void {}
}
new C().m("hi"); // OK

abstract class P {
  abstract m(): this;
}

class Q extends P {
  m(): this {
    return this;
  }
}
new Q().m().m(); // OK
