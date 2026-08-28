abstract class A {
  abstract m(): void;
}

class B extends A {
  m(): void {}
}

const b = new B(); // OK
const f = b.m; // OK: extracting a method preserves its receiver type
