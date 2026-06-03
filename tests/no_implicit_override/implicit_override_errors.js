// Under `no_implicit_override`, a subclass member that shadows an
// inherited member without the `override` modifier is rejected.

class Base {
  greet(): string {
    return "hi";
  }
  name: string = "base";
}

class Sub extends Base {
  greet(): string { // ERROR: implicit override of `Base.greet` needs `override`
    return "yo";
  }
  name: string = "sub"; // ERROR: implicit override of `Base.name` needs `override`
}
