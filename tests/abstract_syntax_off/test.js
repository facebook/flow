// With `experimental.abstract_classes` OFF, every `abstract` class/member
// must continue to fire [unsupported-syntax].

abstract class Foo { // ERROR
  abstract getName(): string; // ERROR

  concreteMethod(): void {}
}

declare abstract class Bar { // ERROR
  abstract getName(): string; // ERROR

  meth(): void;
}

declare abstract class AbstractComputed { // ERROR
  abstract [Symbol.iterator](): Iterator<any>;
}
