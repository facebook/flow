// @flow

enum E {A, B}
//   ^

const x = E.A;
//        ^

const y = E.A;
//          ^

type T = Class<E>;
//   ^

import type {F} from './export.js';
declare var f: F;
//          ^

import type G from './export.js';
declare var g: G;
//          ^

