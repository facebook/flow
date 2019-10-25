// @flow

function f(x: any) {
  x();
  const y = x();
}
