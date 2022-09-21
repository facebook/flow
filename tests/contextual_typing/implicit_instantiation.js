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

  const reasonTest: (string) => mixed = (a) => (a: empty); // ok
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

function test3() {
  declare function magic<T>(): T;
  declare class A<T> {}

  const magicResult: string = magic(); // OK
  const map: A<string> = new A(); // OK
}

function test4() {
  declare function foo<T>(x: T): Array<T>;
  declare var n: number;
  const x: Array<?number> = foo(n); // OK

  declare function bar<T>(x?: T): Array<T>;
  const y: Array<number> = bar(); // OK
}
