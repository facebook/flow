// @flow

function foo(x: Array<number>): [number, ?number] {
  return x; // OK. This is unsound, but no more so than [x[0], x[1]]
}
