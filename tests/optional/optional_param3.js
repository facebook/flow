function foo(x?: number) {}
foo(undefined); // ok

function bar(x: string = "bar"): string {
  return x;
}
bar(undefined); // ok
