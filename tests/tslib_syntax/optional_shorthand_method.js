type T = {
  foo?(): void; // OK
  bar?(): string; // OK
  baz?(prop: string): boolean; // OK
};

declare var t: T;
// Calling an optional method requires a check
if (t.foo) {
  t.foo(); // OK
}

// Optional shorthand method in interface
interface I {
  f?(): void; // OK
}

// Optional shorthand method in declare class
declare class C {
  m?(): void; // OK
}
