// @flow

type T = {
  foo: number
}

type F = (this: T) => void

const f: F = function () {
  (this.foo: number); // ok
  (this.foo: string); // error
}

type F1 = (this: T, foo: number) => void

const f1: F1 = function (foo) {
  (this: string); // error
  (foo: number); // ok
  (foo: string); // error
}

declare function f2(this: T): void;

f2(); // error
f2.bind(null) // error
f2.call({foo: 1}) // ok
f2.apply({foo: 1}, []) // ok
