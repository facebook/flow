// @flow

import type {Bar} from './references';

function takesBar(y: Bar): void {
  y.baz;
}
