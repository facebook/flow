class Foo1 {
  bar(
    baz: Readonly<{+prop: this}>, // ok due to unsoundness
  ): Readonly<{prop: this}> { // ok
    baz.prop.bar(baz);
    return {prop: this}
  }
}

class Foo2 {
  bar(
    baz: Readonly<this>, // ok due to unsoundness
  ): Readonly<this> { // ok
    baz.bar(baz);
    return this
  }
}
