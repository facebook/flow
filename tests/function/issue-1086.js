// @flow

declare class MyArray<T> {
  // TODO: support methods
  // this one is actually works better if implemented with $Call
  +sort:
    & ((this: MyArray<number>, compareFn: (a: T, b: T) => number) => Array<T>)
    & ((compareFn?: (a: T, b: T) => number) => Array<T>);
}

declare var foo: MyArray<number>;
foo.sort(); // should error, nothing happens

declare var bar: MyArray<string>;
bar.sort(); // ok
