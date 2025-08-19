// @flow

class Foo {
  #privateName: Array<string>;

  returnThis(): this { // ok, no spurious illegal-this error
    // $FlowExpectedError[missing-local-annot]
    // $FlowExpectedError[incompatible-type]
    return _ => {};
  }

  assignPrivateName() {
    this.#privateName = []; // ok
  }
}
