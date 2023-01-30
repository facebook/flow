/**
 * @flow
 */

class Foo {
  _method(): string {
    return 'this is private';
  }
}

class Bar extends Foo {
  test() {
    (this._method(): string); // error
  }
}

export class Bar1 { _a = (p: mixed): number => 42; } // no annot on _a required
export class Bar2 { _a(p: mixed) {} }
