// Tests that you can't have duplicate parameter names. In its own file because
// it is a parse error!
component Foo(
  a: number,
  destructured as {a}: {a: number}, // ERROR
) {}
