component Foo() {
  return null;
}
component Bar(x: React.MixedElement) {
  const y: number = x && 3; // OK
  return null;
}
