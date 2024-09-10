// @flow

type T = [1, b: 2, c?: 3];
//   ^?

type S = [0, ...T, d?: 4];
//   ^?

type Tup = [a: number, string, boolean];
type R = $ReadOnly<Tup>;
//   ^?

type P = Partial<Tup>;
//   ^?

type Q = Partial<R>;
//   ^?

type M = [1, 2, 3];
type ROM = $ReadOnly<M>;
//   ^?

type Spread = [1, 2, ...[3, 4]];
//   ^?
