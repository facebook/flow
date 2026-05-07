interface I {
  set foo(x: string, y: string): void
}

declare const o: I;
o.foo = 1;
// ^
