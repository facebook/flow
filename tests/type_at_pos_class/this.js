class Foo2 {
  bar(
    baz: $ReadOnly<this>,
  ): $ReadOnly<this> {
    baz.bar(baz);
    return this
  }
}
