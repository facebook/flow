/* @flow */

type NestedArray<T> = Array<T | NestedArray<T>>;

function flatten<T>(arrArg: NestedArray<T>) {
  let arr = arrArg;
  while (true) {
    arr = Array.prototype.concat.apply([], arr); // error: apply cannot rebind the method receiver
  }
}
