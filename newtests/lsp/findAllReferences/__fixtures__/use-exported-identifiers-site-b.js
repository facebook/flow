// @flow

const A = require('./identifiers-def');

A.a;
const B = {C:{D:A}};
B.C.D.a;
