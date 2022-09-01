// @flow

function test1() {
  declare function f<T>(f: (string) => T): T;
  // We should be able to infer that c is string.
  const c = f((a) => a);
  (c: string);
  const d = f(function (a) { return a });
  (d: string);

  let f2: (string) => string = (a) => (a: string); // ok
  f2 = (b) => (b: string); // ok
}

function test2() {
  declare class Foo {
    bar<T>(f: (string) => T): T
  }
  declare var foo: Foo;

  const c = foo.bar((a) => a);
  (c: string);
  const d = foo?.bar((a) => a);
  (d: string);

  class Bar {
    #baz<T>(f: (string) => T): T {
      return f("");
    }

    test() {
      const c = this.#baz((a) => a);
      (c: string);
    }
  }
}
