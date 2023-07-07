// @flow

import {a,b} from './identifiers-def';
import {a as c} from './identifiers-def';

a.toPrecision();
b();
c.toExponential();

{
  const {a: d} = require('./identifiers-def');
  d.toExponential();
  const {a} = require('./identifiers-def');
  a.toExponential();
}
