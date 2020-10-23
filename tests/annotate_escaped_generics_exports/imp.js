//@flow

import {f} from './exp';

var v = [];

const res = f();

function h<X: typeof res>(x: X) {
  v[0] = x;
}
