// @flow

type T = [1, b: 2, c?: 3];
//   ^

type S = [0, ...T, d?: 4];
//   ^
