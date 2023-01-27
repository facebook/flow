interface I {
  set foo(x: string, y: string): void
}

declare var o: I;
o.foo = 1;
// ^
