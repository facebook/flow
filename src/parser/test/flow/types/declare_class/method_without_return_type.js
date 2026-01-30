// Constructor without return type
declare class A {
  constructor(x: number);
}

// Method without return type
declare class B {
  method();
}

// Method with return type should still work
declare class C {
  constructor(x: number): void;
  method(): string;
}
