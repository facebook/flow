// @flow

type O = {a: number};

type T = O['a'];
//   ^

type M = ?O;

type S = M?.['a'];
//   ^
