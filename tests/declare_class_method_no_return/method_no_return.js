// Constructor without return type
declare class A {
  constructor(); // OK: with tslib_syntax=true
}

// Method without return type
declare class B {
  method(); // ERROR
}

// Multiple methods without return type
declare class C {
  constructor(); // OK: with tslib_syntax=true
  method(); // ERROR
  anotherMethod(x: number); // ERROR
}

// Method with explicit return type
declare class D {
  method(): string; // OK
  constructor(): void; // OK
}

// Setters — pass even in `.js` because setter defaulting is unconditional
declare class S1 {
  set foo(v: number); // OK
}
declare class S2 {
  get foo(): number; // OK
  set foo(v: number); // OK
}
declare class S3<T> {
  set foo(v: T); // OK
}
declare class S4<T, U> {
  set foo(v: T); // OK
  set bar(v: U); // OK
}
declare class S5 {
  set foo(v: number): void; // OK — explicit `: void` still accepted
}

declare class S6 {
  get foo(); // ERROR — getter without return type still errors
}
