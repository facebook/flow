// @flow

function Foo() {
  (new.target: mixed); // OK
  (new.target: boolean); // Error

  if (!new.target) {} // OK
}

class A {
  constructor() {
    (new.target: mixed); // OK
    (new.target: boolean); // Error
  }
}
