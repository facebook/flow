interface I {
  set foo(): void
}

declare const o: I;
o.foo = 1;
// ^
