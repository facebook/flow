// @flow

const F = require('./library');

enum E {A, B}
//   ^

const a = E.A;
//        ^

const b = F.A;
//        ^
