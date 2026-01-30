// Constructor without return type
declare class A {
  constructor(); // ERROR
}

// Method without return type
declare class B {
  method(); // ERROR
}

// Multiple methods without return type
declare class C {
  constructor(); // ERROR
  method(); // ERROR
  anotherMethod(x: number); // ERROR
}

// Method with explicit return type
declare class D {
  method(): string; // OK
  constructor(): void; // OK
}
