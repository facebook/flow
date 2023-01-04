// @flow

function foo<T: number | string>(x: T): number {
  if (typeof x === 'number') {
    return x
  } else {
    return 1;
  }
}
