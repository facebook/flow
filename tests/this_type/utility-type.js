class Foo1 {
  bar(
    baz: Readonly<{readonly prop: this}>, // ok due to unsoundness
  ): Readonly<{prop: this}> { // ok
    baz.prop.bar(baz);
    return {prop: this}
  }
}

class Foo2 {
  bar(
    baz: Readonly<this>,
  ): Readonly<this> { // ok
    baz.bar(baz); // OK: this is a direct method call, so `baz` remains the receiver
    return this
  }
}
