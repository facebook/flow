function Foo() {
  new.target as mixed; // OK
  new.target as boolean; // Error

  if (!new.target) {} // OK
}

class A {
  constructor() {
    new.target as mixed; // OK
    new.target as boolean; // Error
  }
}
