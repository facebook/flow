// @flow

type nums = 1 | 2
type bigints = 1n | 2n

function f(x: nums): bigints {
  return x; // fast path error
}

function g(x: bigints): nums {
  return x; // fast path error
}
