class E1 {}
class E2 {}

declare class A {
  static Error: Class<E1>;
}

declare class B1 extends A {
  static Error: Class<E2>; //ng
}

declare class B2 inherits A {
  static Error: Class<E2>; //ok
}
