// @flow

function foo() {
  return {
    '-': 1,
    '': 2,
    '\u{1F60A}': 3,
    [1]: 4,
  };
}

export const foo_ = foo();

declare var b: {
  '-': 1,
  '': 2,
  '\u{1F60A}': 3,
  [1]: 4,
};
function bar() {
  return b;
}

export const bar_ = bar();
