abstract class Foo { // ERROR
  abstract getName(): string; // ERROR

  concreteMethod(): void {}
}

declare abstract class Bar { // ERROR
  abstract getName(): string; // ERROR

  meth(): void;
}
