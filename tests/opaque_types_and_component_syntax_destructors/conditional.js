component Foo() {
  return null;
}
component Bar(x: React.Element<typeof Foo>) {
  const y: number = x && 3; // OK
  return null;
}
