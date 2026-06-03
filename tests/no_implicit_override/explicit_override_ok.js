// Same shape as `implicit_override_errors.js` but every shadowing member
// carries the `override` modifier — clean under NIO.

class Base {
  greet(): string {
    return "hi";
  }
  name: string = "base";
}

class Sub extends Base {
  override greet(): string { // OK
    return "yo";
  }
  override name: string = "sub"; // OK
}

new Sub().greet() as string; // OK
new Sub().name as string; // OK
