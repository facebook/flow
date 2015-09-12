// @flow
var A = require ('./A');
import type * as B from './B';

class C extends A {
  b: B;
}

module.exports = C;
