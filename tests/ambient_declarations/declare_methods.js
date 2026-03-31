// Test declare methods in classes (currently unsupported)
class MyClass {
  method(x: number): void; // ERROR: Declare method in classes not supported
  static staticMethod(): string; // ERROR: Declare method in classes not supported
}

export {MyClass};
