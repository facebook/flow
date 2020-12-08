// @flow

enum E {A, B}
//   ^

const x = E.A;
//        ^

const y = E.A;
//          ^

const z: E = E.A;
//       ^

type T = Class<E>;
//   ^

import type {F} from './export.js';
//           ^
declare var f: F;
//          ^
declare var f2: F;
//              ^

import type G from './export.js';
//          ^
declare var g: G;
//          ^
declare var g2: G;
//              ^

import H from './export.js';
//     ^
declare var h: H;
//          ^
declare var h2: H;
//              ^

import type {FType} from './export.js';
//           ^
declare var i: FType;
//          ^
declare var i2: FType;
//              ^
