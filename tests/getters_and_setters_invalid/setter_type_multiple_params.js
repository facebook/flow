type O = {
  set foo(x: string, y: string): void
}

declare var o: O;
o.foo = 1;
// ^
