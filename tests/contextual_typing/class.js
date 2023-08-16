// @flow

class Foo {
  #privateName: Array<string>;

  returnThis(): this { // ok, no spurious illegal-this error
    // $FlowExpectedError[missing-local-annot]
    // $FlowExpectedError[incompatible-return]
    return _ => {};
  }

  assignPrivateName() {
    this.#privateName = []; // ok
  }
}
