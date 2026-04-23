function Foo(items: ?MyIterable<number>) {
  MyIterable(items || []).size;
}
