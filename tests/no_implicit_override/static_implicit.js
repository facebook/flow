// NIO enforcement applies to the static side independently from the
// instance side.

class Base {
  static s(): void {}
  static count: number = 0;
}

class Sub extends Base {
  static s(): void {} // ERROR: implicit override of `Base.s` needs `override`
  static count: number = 1; // ERROR: implicit override of `Base.count` needs `override`
}

class Good extends Base {
  static override s(): void {} // OK
  static override count: number = 2; // OK
}
