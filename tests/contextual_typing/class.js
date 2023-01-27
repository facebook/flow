// @flow

class Foo {
  returnThis(): this { // ok, no spurious illegal-this error
    // $FlowExpectedError[missing-local-annot]
    // $FlowExpectedError[incompatible-return]
    return _ => {};
  }
}
