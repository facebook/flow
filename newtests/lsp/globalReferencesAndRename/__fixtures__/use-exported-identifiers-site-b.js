// @flow

const A = require('./identifiers-def');

A.a;
const B = {C:{D:A}};
B.C.D.a;

import * as C from './identifiers-def';
C.a;
const D = {E:{F:C}};
D.E.F.a;
