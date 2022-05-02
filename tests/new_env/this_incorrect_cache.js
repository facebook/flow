// @flow

// This is a regression test for incorrect new-env cache on this/super.

interface This { p: number }

function foo(this: This): boolean {
  return (
    this.p === 1 ||
    this.p === 2
  );
}

class Bar {
  q: ?Object;

  baz(o: Object): void {
    if (!this.q) {
      // There was a bug that make new-env read the cached type `This` from the `foo` function above.
      this.q = o;
    }
  }
}
