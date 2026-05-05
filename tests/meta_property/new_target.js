function Foo() {
  new.target as unknown; // OK
  new.target as boolean; // Error

  if (!new.target) {} // OK
}

class A {
  constructor() {
    new.target as unknown; // OK
    new.target as boolean; // Error
  }
}
