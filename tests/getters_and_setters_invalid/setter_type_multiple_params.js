type O = {
  set foo(x: string, y: string): void
}

declare const o: O;
o.foo = 1;
// ^
