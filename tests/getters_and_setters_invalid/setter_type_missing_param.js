type O = {
  set foo(): void
}

declare const o: O;
o.foo = 1;
// ^
