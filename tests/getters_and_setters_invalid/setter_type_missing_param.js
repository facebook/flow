type O = {
  set foo(): void
}

declare var o: O;
o.foo = 1;
// ^
