// @flow

type O = {a: number};

type T = O['a'];
//   ^

type M = ?O;

type S = M?.['a'];
//   ^

// An optional access keeps the default walk: the maybe-typed object has no
// expandable member to frame.
type P = M?.['a'];
//         ^

// A string-literal index names a member of the object type.
type U = O['a'];
//         ^
