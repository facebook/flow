class Foo1 {
  bar(
    baz: $ReadOnly<{+prop: this}>, // ok due to unsoundness
  ): $ReadOnly<{prop: this}> { // ok
    baz.prop.bar(baz);
    return {prop: this}
  }
}

class Foo2 {
  bar(
    baz: $ReadOnly<this>, // ok due to unsoundness
  ): $ReadOnly<this> { // ok
    baz.bar(baz);
    return this
  }
}
